FROM rocker/geospatial:4.3.1

# rocker/geospatial ya incluye precompilados:
# sf, terra, raster, leaflet, ggplot2, tidyverse, shiny y más

RUN Rscript -e "install.packages(c('shinydashboard','shinyWidgets','shinyjs','plotly','DT','writexl','openxlsx','zip'), repos='https://cran.rstudio.com/', quiet=TRUE)"

COPY app.R /srv/shiny-server/app/app.R

RUN printf 'run_as shiny;\nserver {\n  listen 3838;\n  location / {\n    site_dir /srv/shiny-server/app;\n    log_dir /var/log/shiny-server;\n    directory_index off;\n    sanitize_errors off;\n  }\n}\n' > /etc/shiny-server/shiny-server.conf

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
