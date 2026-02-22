# 1. Usamos una imagen de R optimizada para Plumber
FROM rstudio/plumber:latest

# 2. Instalamos dependencias del sistema para paquetes geográficos
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    && install2.r --error sp gstat fields

# 3. Creamos el directorio de la app y copiamos los archivos
WORKDIR /app
# Asegúrate de que estos nombres coincidan con tus archivos en GitHub
COPY geoid_models.RData /app/geoid_models.RData
COPY plumber.R /app/plumber.R

# 4. Exponemos el puerto 8080 (el que Render usa por defecto)
EXPOSE 8080

# 5. Comando para ejecutar la API
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('/app/plumber.R'); pr$run(host = '0.0.0.0', port = 8080)"]
