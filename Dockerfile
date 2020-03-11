FROM rocker/verse:3.6.1
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
RUN chown -R ${NB_USER} ${HOME}

# ---------------------------------------------

# Install extra packages not already available in verse

# Set default source for new packages, using the MRAN snapshots of CRAN

RUN echo "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2019-08-26/'))" > /usr/local/lib/R/etc/Rprofile.site

#RUN R --quiet -e "install.packages(c('ggbeeswarm', 'gridExtra', 'kableExtra', 'leaflet', 'pacman', 'rcrossref', 'RefManageR', 'Taxonstand', 'whisker'));"

# Install remake

#RUN R --quiet -e "remotes::install_github('richfitz/remake', dependencies=TRUE);"

# Set behaviour of remake 

RUN echo "options(remake.verbose.noop=FALSE)" >> /usr/local/lib/R/etc/Rprofile.site

