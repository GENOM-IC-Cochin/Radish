FROM rocker/r-ver:4.0.5
LABEL maintainer="Paul Etheimer <paul.etheimer@inserm.fr>"
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libharfbuzz-dev \
    perl \
# for ragg & svglite
    libpng-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libtiff-dev \
    libfribidi-dev \
    libjpeg-dev \
# for colourpicker
    pandoc \
# for stringr
    libicu-dev \
    && rm -rf /var/lib/apt/lists/*
# Install renv
ENV RENV_VERSION 0.14.0
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
RUN addgroup --system app \
    && adduser --system --ingroup app app
WORKDIR /home/app
COPY . .
RUN chown app:app -R /home/app
USER app
RUN R -e 'renv::restore()'
RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/home/app')"]
