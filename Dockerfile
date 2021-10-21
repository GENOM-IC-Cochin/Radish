FROM rocker/rbase:4.0.5
LABEL maintainer="<paul.etheimer@inserm.fr>"
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
    lipjpeg-dev \
# for colourpicker
    pandoc \
# for stringr
    libicu-dev \
    && rm -rf /var/lib/apt/lists/*
RUN addgroup --system app \
    && adduser --system --ingroup app app
WORKDIR /home/app
COPY . .
RUN chown app:app -R /home/app
USER app
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/home/app')"]
