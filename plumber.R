# ==========================================
# ARCHIVO: plumber.R (Actualizado para TXT)
# ==========================================
library(plumber)
library(sp)
library(gstat)
library(fields)

load("modelo_geoidal.RData")

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

# --- FUNCIÓN AUXILIAR (El cerebro matemático) ---
calcular_z <- function(x_num, y_num, modelo) {
  if(modelo == "1"){
    punto <- data.frame(x = x_num, y = y_num)
    coordinates(punto) <- ~ x + y
    pred <- krige(z ~ 1, datos_sp, punto, model = vg_fit, nmin = 4, nmax = 8, maxdist = 5300, debug.level = 0)
    return(pred$var1.pred)
  } else if(modelo == "2"){
    sigma_ruido <- 0.03
    dists <- sqrt((datos$x - x_num)^2 + (datos$y - y_num)^2)
    cand <- which(dists <= 5300) 
    if(length(cand) >= 4){
      sel <- if(length(cand)>8) cand[order(dists[cand])[1:8]] else cand
      vec <- datos[sel,]; k <- nrow(vec)
      Cxx <- matrix(0, k, k); Csx <- numeric(k)
      for(r in 1:k){
        Csx[r] <- cov_func(calc_dist(vec$x[r], vec$y[r], x_num, y_num))
        for(c in 1:k){ 
          val_cov <- cov_func(calc_dist(vec$x[r], vec$y[r], vec$x[c], vec$y[c]))
          if(r==c) val_cov <- val_cov + m_C0 + sigma_ruido^2
          Cxx[r,c] <- val_cov 
        }
      }
      if(det(Cxx) < 1e-15) diag(Cxx) <- diag(Cxx) + 1e-7
      F_mat <- cbind(1, vec$x, vec$y); f0 <- c(1, x_num, y_num)
      K_aug <- rbind(cbind(Cxx, F_mat), cbind(t(F_mat), matrix(0, 3, 3)))
      w <- solve(K_aug, c(Csx, f0))[1:k]
      return(sum(w * vec$z))
    } else {
      return(NA)
    }
  } else if(modelo == "3"){
    return(predict(tps_final, x = cbind(x_num, y_num))[1])
  }
  return(NA)
}

#* @apiTitle Calculadora Geoidal API Completa

#* 1. Calcular Ondulación para UN punto
#* @param x Coordenada Este
#* @param y Coordenada Norte
#* @param modelo Nombre del modelo (1, 2, 3)
#* @get /predecir
function(x, y, modelo) {
  x_num <- as.numeric(x)
  y_num <- as.numeric(y)
  resultado <- calcular_z(x_num, y_num, modelo)
  if (is.na(resultado)) resultado <- "Error: Punto muy lejano o modelo inválido"
  return(list(X = x_num, Y = y_num, Modelo = modelo, Z_Ondulacion = resultado))
}

#* 2. Calcular Ondulación para MÚLTIPLES puntos (Subida de TXT)
#* @param modelo Nombre del modelo (1, 2, 3)
#* @post /predecir_lote
function(req, res, modelo) {
  # Extraer la lista de puntos enviada desde la web
  puntos <- req$body 
  resultados <- numeric(nrow(puntos))
  
  # Calcular cada punto
  for(i in 1:nrow(puntos)){
    resultados[i] <- calcular_z(as.numeric(puntos$x[i]), as.numeric(puntos$y[i]), modelo)
  }
  
  # Pegar la respuesta y enviarla de vuelta
  puntos$z_ondulacion <- resultados
  return(puntos)
}