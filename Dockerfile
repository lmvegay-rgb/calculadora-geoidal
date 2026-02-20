# 1. Usamos una imagen de R que ya tiene Plumber instalado
FROM rstudio/plumber:latest

# 2. Instalamos las librerías de mapas y mates que usas
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
libgdal-dev libgeos-dev libproj-dev \
&& install2.r --error sp gstat fields

# 3. Copiamos tus archivos al servidor de Render
COPY modelo_geoidal.RData /app/modelo_geoidal.RData
COPY plumber.R /app/plumber.R

# 4. Abrimos el puerto para internet
EXPOSE 8080

# 5. Encendemos la API
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('/app/plumber.R'); pr$run(host = '0.0.0.0', port = 8080)"]