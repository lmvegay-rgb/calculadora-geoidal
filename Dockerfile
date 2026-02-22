FROM rstudio/plumber:latest

RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    libgdal-dev libgeos-dev libproj-dev libudunits2-dev \
    && install2.r --error sp gstat fields

# Copiamos usando el nombre geoid_models.RData
COPY geoid_models.RData /app/geoid_models.RData
COPY plumber.R /app/plumber.R

EXPOSE 8080

ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('/app/plumber.R'); pr$run(host = '0.0.0.0', port = 8080)"]
