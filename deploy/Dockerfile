FROM ubuntu:18.04
MAINTAINER Dima Kurilo <dkurilo@gmail.com>

RUN mkdir -p /var/build

COPY ./bin/library-api-exe /var/build/library-api-exe
COPY ./cert.pem /var/build/cert.pem
COPy ./key.pem /var/build/key.pem

WORKDIR /var/build

EXPOSE 8080

CMD ["/var/build/library-api-exe"]
