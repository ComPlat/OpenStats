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
  jsonlite \
  emmeans

USER shiny
COPY ./OpenStats/R /home/myapp
RUN mkdir /home/shiny/results

COPY ./MTT/ /home/MTT
COPY ./comeln/ /home/comeln
COPY ./OpenStats/ /home/OpenStats

USER root
RUN bash -c "cd /home/MTT; R CMD INSTALL . && cd /home/comeln; R CMD INSTALL . && cd /home/OpenStats; R CMD INSTALL ."

EXPOSE 4001
COPY ./Start_Server_App.R /srv/shiny-server/app.R
COPY ./run.sh /home

USER shiny
ENV SHINY_LOG_STDERR=1

# CMD ["/bin/bash", "-c", "/home/run.sh"]
CMD ["Rscript", "/srv/shiny-server/app.R", "3838"]

# Testing ELN: workshop.chemotion.scc.kit.edu
# OpenStats running on: https://openstats.chemserv.scc.kit.edu/
