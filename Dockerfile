FROM rocker/shiny:4.3.1

# Dependencias del sistema
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
    locales \
    && locale-gen es_CO.UTF-8 \
    && rm -rf /var/lib/apt/lists/*

ENV LANG=es_CO.UTF-8
ENV LC_ALL=es_CO.UTF-8

COPY packages.R /packages.R
COPY app.R /srv/shiny-server/app/app.R

RUN Rscript /packages.R

RUN Rscript -e "\
cat('=== Verificando paquetes ===\n'); \
pkgs <- c('shiny','shinydashboard','shinyWidgets','shinyjs','readxl', \
'dplyr','janitor','lubridate','stringr','ggplot2','plotly', \
'DT','leaflet','sf','writexl','tidyr','zip','openxlsx','htmltools'); \
for(p in pkgs){ if(requireNamespace(p,quietly=TRUE)) cat('OK:',p,'\n') else cat('FALTA:',p,'\n') }; \
cat('=== Verificando sintaxis ===\n'); \
tryCatch(parse('/srv/shiny-server/app/app.R'), error=function(e){ cat('ERROR:',conditionMessage(e),'\n'); quit(status=1) }); \
cat('Sintaxis OK\n') \
"

RUN printf 'run_as shiny;\nserver {\n  listen 3838;\n  location / {\n    site_dir /srv/shiny-server/app;\n    log_dir /var/log/shiny-server;\n    directory_index off;\n    sanitize_errors off;\n  }\n}\n' > /etc/shiny-server/shiny-server.conf

RUN printf '#!/bin/bash\necho "=== Test app.R ==="\nRscript -e "suppressPackageStartupMessages(source(\"/srv/shiny-server/app/app.R\"))" 2>&1 | head -80 || true\necho "=== Iniciando Shiny Server ==="\nexec /usr/bin/shiny-server\n' > /start.sh && chmod +x /start.sh

EXPOSE 3838
CMD ["/start.sh"]

