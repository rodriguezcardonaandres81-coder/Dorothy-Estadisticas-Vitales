# Dockerfile — Dorothy Shiny App
FROM rocker/shiny:4.3.1

# Dependencias del sistema para sf y otros paquetes espaciales
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Copiar archivos de la app
COPY packages.R /packages.R
COPY app.R /srv/shiny-server/app/app.R

# Instalar paquetes de R
RUN Rscript /packages.R

# Configurar Shiny Server para escuchar en el puerto de Render
RUN echo "run_as shiny;\n\
server {\n\
  listen 3838;\n\
  location / {\n\
    site_dir /srv/shiny-server/app;\n\
    log_dir /var/log/shiny-server;\n\
    directory_index off;\n\
  }\n\
}" > /etc/shiny-server/shiny-server.conf

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
