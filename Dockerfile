FROM rocker/r-ver:latest

EXPOSE 8080

RUN apt-get update && apt-get install -y \
  libcurl4-openssl-dev \
  libicu-dev \
  libssl-dev \
  make \
  pandoc pandoc-citeproc

COPY ./shiny_app/deps.R /tmp/deps.R

RUN Rscript /tmp/deps.R && rm -rf /tmp/*

# Copy app into `shiny_app` directory
COPY shiny_app /srv/shiny-server/shiny_app

CMD ["Rscript","-e","shiny::runApp(appDir='/srv/shiny-server/shiny_app',port=8080,launch.browser=FALSE,host='0.0.0.0')"]
