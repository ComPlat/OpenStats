#!/bin/bash

# Kill leftover Java processes from previous ShinyProxy runs
# requires sudo
pkill -f shinyproxy 2>/dev/null

# Optional: Create network if not already created
docker network inspect sp-net >/dev/null 2>&1 || docker network create sp-net
# Optional: create docker network (for an internal network)
# docker network create sp-net

# Start app
docker run -it --rm -p 8080:8080 \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v $PWD/application.yml:/opt/shinyproxy/application.yml \
  --network sp-net \
  --user root \
  openanalytics/shinyproxy

# example call: http://localhost:8080/app/biostats/?token=XYZ
# Maybe url has to be adapted in ELN?
