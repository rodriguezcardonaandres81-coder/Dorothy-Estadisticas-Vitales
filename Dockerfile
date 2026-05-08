FROM rocker/shiny:4.3.1

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
    libv8-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libjq-dev \
    cmake \
    && locale-gen es_CO.UTF-8 \
    && rm -rf /var/lib/apt/lists/*

ENV LANG=es_CO.UTF-8
ENV LC_ALL=es_CO.UTF-8

# Instalar paquetes por grupos - si uno falla se ve exactamente cuál es
RUN Rscript -e "install.packages('leaflet',      repos='https://cran.rstudio.com/', dependencies=TRUE)"
RUN Rscript -e "install.packages('sf',           repos='https://cran.rstudio.com/', dependencies=TRUE)"
RUN Rscript -e "install.packages('plotly',       repos='https://cran.rstudio.com/', dependencies=TRUE)"
RUN Rscript -e "install.packages('shiny',        repos='https://cran.rstudio.com/', dependencies=TRUE)"
RUN Rscript -e "install.packages('shinydashboard', repos='https://cran.rstudio.com/')"
RUN Rscript -e "install.packages('shinyWidgets', repos='https://cran.rstudio.com/')"
RUN Rscript -e "install.packages('shinyjs',      repos='https://cran.rstudio.com/')"
RUN Rscript -e "install.packages('DT',           repos='https://cran.rstudio.com/')"
RUN Rscript -e "install.packages(c('readxl','dplyr','janitor','lubridate','stringr','ggplot2','writexl','tidyr','zip','openxlsx','htmltools'), repos='https://cran.rstudio.com/')"

COPY app.R /srv/shiny-server/app/app.R

RUN printf 'run_as shiny;\nserver {\n  listen 3838;\n  location / {\n    site_dir /srv/shiny-server/app;\n    log_dir /var/log/shiny-server;\n    directory_index off;\n    sanitize_errors off;\n  }\n}\n' > /etc/shiny-server/shiny-server.conf

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]



 
