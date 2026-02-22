# =====================================================================
# ARCHIVO: plumber.R (VERSION FINAL CON INCERTIDUMBRE)
# =====================================================================
library(plumber)
library(sp)
library(gstat)
library(fields)

# 1. CARGAMOS EL WORKSPACE
load("geoid_models.RData")

#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

# --- FUNCIONES AUXILIARES ---
crear_matriz_diseno <- function(x, y, grado, cx = 0, cy = 0) {
  xc <- x - cx; yc <- y - cy
  if (grado == 1) return(cbind(1, xc, yc))
  if (grado == 2) return(cbind(1, xc, yc, xc^2, yc^2, xc * yc))
  if (grado == 3) return(cbind(1, xc, yc, xc^2, yc^2, xc * yc, xc^3, yc^3, (xc^2) * yc, xc * (yc^2)))
  stop("Grado no soportado.")
}
calc_dist <- function(x1, y1, x2, y2) sqrt((x1-x2)^2 + (y1-y2)^2)

# --- EL CEREBRO MATEMÁTICO (RETORNA LISTA: Z y ERROR) ---
calcular_z_con_error <- function(x_num, y_num, modelo) {
  
  # --- MÉTODO 1: KRIGING ---
  if(modelo == "1"){
    punto <- data.frame(x = x_num, y = y_num)
    coordinates(punto) <- ~ x + y
    proj4string(punto) <- proj4string(datos_sp)
    pred <- krige(z ~ 1, datos_sp, punto, model = vg_fit, 
                  nmin = VECINOS_MIN, nmax = VECINOS_MAX, maxdist = RADIO_MANUAL, debug.level = 0)
    return(list(z = pred$var1.pred, err = sqrt(pred$var1.var)))
  } 
  
  # --- MÉTODO 2: LSC / MCC ---
  else if(modelo == "2"){
    dists <- sqrt((datos_lsc$x - x_num)^2 + (datos_lsc$y - y_num)^2)
    cand  <- which(dists <= RADIO_MANUAL)
    
    # Calculamos cuántos parámetros tiene el polinomio según el grado ganador
    F_temp <- crear_matriz_diseno(1, 1, mejor_grado, cx_global, cy_global)
    u_p <- ncol(F_temp) 
    min_v <- max(VECINOS_MIN, u_p + 1)
    
    if(length(cand) >= min_v){
      max_v <- max(VECINOS_MAX, u_p * 3)
      sel <- if(length(cand) > max_v) cand[order(dists[cand])[1:max_v]] else cand
      vec <- datos_lsc[sel, ]; k <- nrow(vec)
      
      Cxx <- matrix(0, k, k); Csx <- numeric(k)
      cov_f <- function(h) modelos_lista[[m_nom]](h, m_C0, m_a)
      
      for(r in 1:k){
        Csx[r] <- cov_f(calc_dist(vec$x[r], vec$y[r], x_num, y_num))
        for(cc in 1:k){
          val <- cov_f(calc_dist(vec$x[r], vec$y[r], vec$x[cc], vec$y[cc]))
          if(r == cc) val <- val + vec$err[r]^2
          Cxx[r, cc] <- val
        }
      }
      
      if(det(Cxx) < 1e-15) diag(Cxx) <- diag(Cxx) + 1e-7
      
      # --- AQUÍ ESTABA EL ERROR: USAR NOMBRES CONSISTENTES ---
      F_mat <- crear_matriz_diseno(vec$x, vec$y, mejor_grado, cx_global, cy_global)
      f0    <- crear_matriz_diseno(x_num, y_num, mejor_grado, cx_global, cy_global)
      
      # Construcción de la matriz aumentada [Cxx F_mat; F_mat' 0]
      K_top <- cbind(Cxx, F_mat)
      K_bot <- cbind(t(F_mat), matrix(0, u_p, u_p))
      K_aug <- rbind(K_top, K_bot)
      
      aug_v <- c(Csx, as.numeric(f0))
      sol   <- solve(K_aug, aug_v)
      
      z_final <- sum(sol[1:k] * vec$z)
      var_final <- max(0, m_C0 - sum(aug_v * sol))
      
      return(list(z = z_final, err = sqrt(var_final)))
    } else {
      return(list(z = NA, err = NA))
    }
  }
  
  # --- MÉTODO 3: MÍNIMA CURVATURA ---
  else if(modelo == "3"){
    z_mc <- predict(tps_final, x = matrix(c(x_num, y_num), 1, 2))[1]
    err_mc <- tryCatch({
      predict(tps_final, x = matrix(c(x_num, y_num), 1, 2), se.fit = TRUE)$se.fit
    }, error = function(e) NA)
    return(list(z = z_mc, err = err_mc))
  }
  
  return(list(z = NA, err = NA))
}

#* @apiTitle Calculadora Geoidal API Completa

#* 1. Calcular para UN punto
#* @param x Coordenada Este
#* @param y Coordenada Norte
#* @param modelo Nombre del modelo (1, 2, 3)
#* @get /predecir
function(x, y, modelo) {
  res <- calcular_z_con_error(as.numeric(x), as.numeric(y), modelo)
  if (is.na(res$z)) return(list(error = "Fuera de rango"))
  
  return(list(
    X = as.numeric(x),
    Y = as.numeric(y),
    Modelo = modelo,
    Z_Ondulacion = round(res$z, 4),
    Error_Incertidumbre = round(res$err, 4)
  ))
}

#* 2. Calcular para LOTE (Subida de Archivo)
#* @param modelo Nombre del modelo (1, 2, 3)
#* @post /predecir_lote
function(req, res, modelo) {
  puntos <- req$body 
  z_vals <- numeric(nrow(puntos))
  e_vals <- numeric(nrow(puntos))
  
  for(i in 1:nrow(puntos)){
    calc <- calcular_z_con_error(as.numeric(puntos$x[i]), as.numeric(puntos$y[i]), modelo)
    z_vals[i] <- calc$z
    e_vals[i] <- calc$err
  }
  
  puntos$z_ondulacion <- round(z_vals, 4)
  puntos$incertidumbre <- round(e_vals, 4)
  return(puntos)
}
