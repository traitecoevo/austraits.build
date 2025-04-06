# Pre-built Dev Container Image for R. More info: https://github.com/rocker-org/devcontainer-images/pkgs/container/devcontainer%2Ftidyverse
ARG VARIANT="4.4"
FROM ghcr.io/rocker-org/devcontainer/tidyverse:${VARIANT}

# RUN install2.r --error --skipinstalled -n -1 \
#     && rm -rf /tmp/downloaded_packages \
#     && R -q -e 'remotes::install_github("https://github.com/traitecoevo/traits.build@v1.0.1")' \
#     && R -q -e 'install.packages("vscDebugger", repos = "https://manuelhentschel.r-universe.dev")'

# Install Python packages
RUN python3 -m pip --disable-pip-version-check --no-cache-dir install \
        radian \
        language-server-protocol \
    && python3 -m pip cache purge
