#!/bin/bash

docker build -f Dockerfile.opendoe -t konradkraemer/opendoe:latest .
# docker build --no-cache -f Dockerfile.opendoe -t konradkraemer/opendoe:latest .

docker push konradkraemer/opendoe:latest
