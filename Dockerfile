FROM rocker/r-ver:4.2.3
LABEL maintainer="Paul Etheimer <paul.etheimer@inserm.fr>"
RUN  apt update && \
    DEBIAN_FRONTEND=noninteractive apt install --yes \
    gcc \
    g++ \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    pandoc \
    libharfbuzz-dev \
# for ragg & svglite
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libjpeg-dev \
    libpng-dev \
    libtiff-dev \
# for colourpicker
    zlib1g-dev \
# for devtools
    libcurl4-openssl-dev \
    git \
    libgit2-dev \
# for DESeq2
    libxml2-dev \
# for stringr
    libicu-dev && \
    apt autoremove --yes && \
    apt clean && \
    rm -rf /var/lib/apt/lists/*

# Install renv
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN groupadd --system app \
    && useradd --system --gid app app
WORKDIR /home/app
COPY . .
RUN chown app:app -R /home/app
USER app
RUN R -e 'renv::restore()'
EXPOSE 3838
CMD ["R", "-e", "pkgload::load_all('.');run_radish_app(options = list(port = 3838, host = '0.0.0.0'))"]
