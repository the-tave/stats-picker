# Start your image with a rocker base image
## Think of this as a recipe for building the image.
## This will be its own isolated world where we need to install everything we want to use (esp. R and Shiny)
FROM rocker/shiny:latest

# Install a bunch of Linux dependencies to make sure all the R packages install and run properly

RUN apt-get update && apt-get install -y --no-install-recommends \
    cmake \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libnlopt-dev \
    libpng-dev \
    libssl-dev \
    libtiff5-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

COPY install_packages.R .

RUN Rscript --verbose install_packages.R


##### More robust option for running several shiny apps later on
#COPY app /srv/shiny-server/

#RUN chown -R shiny:shiny /srv/shiny-server

#EXPOSE 3838

#CMD ["/usr/bin/shiny-server"]

###### Simple option for just one app:
## Copy everything in the app folder into the Docker image
COPY ./app .
EXPOSE 3838
## Start the app using shiny runApp command
CMD ["R", "-e", "shiny::runApp(appDir = '.', host = '0.0.0.0', port = 3838)"]
