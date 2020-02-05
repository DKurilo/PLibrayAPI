FROM ubuntu:18.04
MAINTAINER Dima Kurilo <dkurilo@gmail.com>

RUN mkdir -p /var/build/db
RUN apt install postgresql

COPY ./bin/library-api-exe /var/build/library-api-exe
COPY ./cert.pem /var/build/cert.pem
COPY ./key.pem /var/build/key.pem
COPY ../db/init.sql /var/build/db/init.sql
COPY ../db/dump.sql /var/build/db/dump.sql

RUN psql -f /var/build/db/init.sql postgres
RUN psql -f /var/build/db/dump.sql library

WORKDIR /var/build

EXPOSE 8080

CMD ["/var/build/library-api-exe"]
