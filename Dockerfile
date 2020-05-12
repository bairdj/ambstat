FROM rocker/shiny:3.6.3
COPY install.R /opt/install.R
RUN apt-get update && apt-get install -y libssl-dev curl && r /opt/install.R
COPY app /srv/shiny-server/ambstat
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
EXPOSE 80