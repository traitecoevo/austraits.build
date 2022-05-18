FROM rocker/verse:4.1.2
LABEL maintainer="Daniel Falster"
LABEL email="daniel.falster@unsw.edu.au"

# Install major libraries
RUN    apt-get update \
    && apt-get install -y --no-install-recommends \
        zip \
        unzip

# ---------------------------------------------

ENV NB_USER rstudio
ENV NB_UID 1000

# And set ENV for R! It doesn't read from the environment...
RUN echo "PATH=${PATH}" >> /usr/local/lib/R/etc/Renviron
RUN echo "export PATH=${PATH}" >> ${HOME}/.profile

# The `rsession` binary that is called by nbrsessionproxy to start R doesn't seem to start
# without this being explicitly set
ENV LD_LIBRARY_PATH /usr/local/lib/R/lib

ENV HOME /home/${NB_USER}
WORKDIR ${HOME}

# ---------------------------------------------

# Add custom installations here

## Install packages based on DESCRIPTION file in repository. 

## Copies your description file into the Docker Container, specifying dependencies

USER root
COPY ./DESCRIPTION ${HOME}
# The above line adds only the description file for the project

RUN chown -R ${NB_USER} ${HOME}

RUN R --quiet -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-05-01/')); devtools::install_deps(upgrade = 'never')"

RUN echo "options(remake.verbose.noop=FALSE)" >> /usr/local/lib/R/etc/Rprofile.site

