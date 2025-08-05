#!/bin/bash

#/usr/bin/shiny-server
sudo -u shiny /usr/bin/shiny-server

#cd /srv/shiny-server

#Rscript -e 'shiny::runApp(".", port = 3838)'

Rscript /srv/shiny-server/app.R
