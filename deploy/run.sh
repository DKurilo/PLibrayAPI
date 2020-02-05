#!/bin/bash

# Generate self signed certificate
# openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -days 365

# Copy certificate into docker container
# docker cp ./key.pem dkurilo/library-api:/var/build/key.pem
# docker cp ./cert.pem dkurilo/library-api:/var/build/cert.pem

# Run container
docker run -t -i -e POSTGRES_CONN=$POSTGRES_CONN --name LibraryAPI dkurilo/library-api
