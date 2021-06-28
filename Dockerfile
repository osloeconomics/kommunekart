# build the Docker image from the base image 'openanalytics/r-base'
# this is an Ubuntu 16.04 LTS with a recent R version.
# this image is available on Docker hub at https://hub.docker.com/r/openanalytics/r-base/
FROM openanalytics/r-base
# add the maintainer of this Docker image (this should be you in this case)
LABEL maintainer "Eivind Moe Hammersmark <emh@osloeconomics.no>"
# system libraries of general use
#RUN apt-get update && apt-get install -y \
#sudo \
#pandoc \
#pandoc-citeproc \
#libcurl4-gnutls-dev \
#libcairo2-dev \
#libxt-dev \
#libssl-dev \
#libssh2-1-dev \
#libssl1.0.0

# system library dependency for the euler app
# the R package Rmpfr requires the system library libmpfr-dev to be available
# note that if you don't need to use the Rmpfr package, you can delete this line
RUN apt-get update && apt-get install -y \
libgdal-dev \
libudunits2-dev \
libgeos-dev \
libproj-dev \
iputils-ping \
libfontconfig1-dev

# install basic shiny functionality to R
RUN R -e "install.packages(c('shiny', 'sf', 'dplyr', 'readxl', 'leaflet'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('readr', 'haven', 'waiter', 'ggplot2', 'ggmap'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('shinycssloaders', 'classInt', 'shinyjs'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('RColorBrewer', 'leafpop', 'htmltools'), repos='https://cloud.r-project.org/')"

# copy the example euler app (with the ui.R and server.R files)
# onto the image in folder /root/euler
RUN mkdir /root/kommunekart
COPY kommunekart-app /root/kommunekart

# copy the Rprofile.site set up file to the image.
# this make sure your Shiny app will run on the port expected by
# ShinyProxy and also ensures that one will be able to connect to
# the Shiny app from the outside world
COPY kommunekart-app/Rprofile.site /usr/lib/R/etc/
# instruct Docker to expose port 3838 to the outside world
# (otherwise it will not be possible to connect to the Shiny application)
EXPOSE 3838
# finally, instruct how to launch the Shiny app when the container is started
CMD ["R", "-e", "shiny::runApp('/root/kommunekart')"]
