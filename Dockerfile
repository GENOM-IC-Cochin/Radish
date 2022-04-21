FROM fedora:35
LABEL maintainer="Paul Etheimer <paul.etheimer@inserm.fr>"
RUN dnf -y upgrade && dnf install -y \
    gcc \
    gcc-c++ \
    R-core \
    R-devel \
    gnutls-devel \
    cairo-devel \
    libXt-devel \
    openssl-devel \
    harfbuzz-devel \
# for ragg & svglite
    fontconfig-devel \
    freetype-devel \
    fribidi-devel \
    libjpeg-turbo-devel \
    libpng-devel \
    libtiff-devel \
# for colourpicker
    zlib-devel \
# for devtools
    libcurl-devel \
    libgit2-devel \
# for DESeq2
    libxml2-devel \
# for stringr
    libicu-devel \
    && dnf clean all

# Install renv
ENV RENV_VERSION 0.15.4
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
RUN groupadd --system app \
    && adduser --system --gid app app
WORKDIR /home/app
COPY . .
RUN chown app:app -R /home/app
USER app
RUN R -e 'renv::restore()'
EXPOSE 3838
CMD ["R", "-e", "pkgload::load_all('.');ShareApp(options = list(port = 3838, host = '0.0.0.0'))"]
