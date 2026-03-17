FROM rocker/shiny:4.4.2

RUN apt-get update && apt-get install -y \
  --no-install-recommends \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev \
  curl \
  libsodium-dev \
  libxml2-dev \
  libicu-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

ENV _R_SHLIB_STRIP_=true
ENV SHINY_LOG_STDERR=1

RUN install2.r --error --skipinstalled \
  shiny \
  shinyjs \
  shinyWidgets \
  jsonlite \
  ggplot2 \
  htmltools \
  drc \
  DT \
  httr \
  agricolae \
  broom \
  readxl \
  openxlsx \
  purrr \
  png \
  RColorBrewer \
  remotes \
  xml2 \
  xlsx \
  openssl \
  ggpmisc \
  jose \
  R6 \
  cowplot \
  car \
  equatiomatic \
  quarto \
  emmeans \
  callr \
  permuco

# Create writable directories as root and assign them to shiny
RUN mkdir -p /home/shiny/results \
    /srv/shiny-server/OpenStats \
 && chown -R shiny:shiny /home/shiny \
 && chown -R shiny:shiny /srv/shiny-server

# Copy package sources
COPY ./MTT/ /tmp/MTT
COPY ./comeln/ /tmp/comeln
COPY ./OpenStats/ /tmp/OpenStats

# Install local packages
RUN R CMD INSTALL /tmp/MTT && \
    R CMD INSTALL /tmp/comeln && \
    R CMD INSTALL /tmp/OpenStats && \
    rm -rf /tmp/MTT /tmp/comeln /tmp/OpenStats

# Copy Shiny app entrypoint for Shiny Server
COPY ./Start_Server_App.R /srv/shiny-server/OpenStats/app.R

# Optional: custom Shiny Server config
RUN printf '%s\n' \
  'run_as shiny;' \
  'server {' \
  '  listen 3838;' \
  '  location / {' \
  '    app_dir /srv/shiny-server/OpenStats;' \
  '    log_dir /var/log/shiny-server;' \
  '    directory_index on;' \
  '  }' \
  '}' > /etc/shiny-server/shiny-server.conf

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
