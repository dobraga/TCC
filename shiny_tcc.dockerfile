FROM rocker/shiny

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libudunits2-dev \
    libgdal-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    clang \
    && install2.r --error \
    --deps TRUE \
    sf \
    ranger \
    leaflet \
    flexdashboard \
    dplyr

COPY ./scripts/dash.rmd /srv/shiny-server/scripts/
ADD output/ /srv/shiny-server/output/

EXPOSE 3838
CMD ["R", "-e", "rmarkdown::run('/srv/shiny-server/scripts/dash.rmd', shiny_args = list(port = 3838, host = '0.0.0.0'))"]
