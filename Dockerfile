FROM rocker/geospatial:4.3.1

RUN apt-get update && apt-get install -y gdebi-core wget && \
    wget -q https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.22.1017-amd64.deb && \
    gdebi -n shiny-server-1.5.22.1017-amd64.deb && \
    rm shiny-server-1.5.22.1017-amd64.deb && \
    rm -rf /var/lib/apt/lists/*

RUN Rscript -e "install.packages(c('shinydashboard','shinyWidgets','shinyjs','plotly','DT','writexl','openxlsx','zip','janitor','readxl'), repos='https://cran.rstudio.com/', quiet=TRUE)"

COPY app.R /srv/shiny-server/app/app.R

RUN mkdir -p /etc/shiny-server && \
    printf 'run_as shiny;\nserver {\n  listen 3838;\n  location / {\n    site_dir /srv/shiny-server/app;\n    log_dir /var/log/shiny-server;\n    directory_index off;\n    sanitize_errors off;\n  }\n}\n' > /etc/shiny-server/shiny-server.conf

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
