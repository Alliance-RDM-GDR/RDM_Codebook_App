# Use the official R Shiny image as the base
FROM rocker/shiny:latest

# Install required system libraries
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libtiff5-dev \
    libjpeg-dev \
    libpng-dev \
    libxt-dev \
    libglpk-dev \
    pandoc \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c(\
  'shiny', 'shinyjs', 'shinythemes', 'rhandsontable', \
  'readxl', 'DT', 'shinyBS', 'shiny.i18n'\
), repos = 'https://cloud.r-project.org')"

# Copy your app files into the image
COPY . /srv/shiny-server/

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Expose port
EXPOSE 3838

# Run the Shiny server
CMD ["/usr/bin/shiny-server"]
