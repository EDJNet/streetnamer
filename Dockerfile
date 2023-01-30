FROM rocker/shiny:4.2.2
RUN apt-get update -y && apt-get install -y  make pandoc libicu-dev  make zlib1g-dev pandoc libpng-dev libjpeg-dev libicu-dev  libssl-dev libcurl4-openssl-dev  make zlib1g-dev pandoc libpng-dev libjpeg-dev libssl-dev libicu-dev libcurl4-openssl-dev  make  make zlib1g-dev  git libssl-dev libcurl4-openssl-dev  libcurl4-openssl-dev libssl-dev  zlib1g-dev  git libgit2-dev libssl-dev libcurl4-openssl-dev  git  git libxml2-dev make pandoc libgit2-dev libssl-dev libicu-dev libcurl4-openssl-dev zlib1g-dev  libjpeg-dev  pandoc libicu-dev  libpng-dev libjpeg-dev  make pandoc libpng-dev libicu-dev libgdal-dev gdal-bin libgeos-dev libproj-dev libsqlite3-dev  libssl-dev  libxml2-dev libproj-dev libssl-dev libicu-dev libcurl4-openssl-dev  libpng-dev  libproj-dev  libgdal-dev gdal-bin libgeos-dev libproj-dev libsqlite3-dev  libxml2-dev make pandoc libicu-dev  libxml2-dev libssl-dev libicu-dev libcurl4-openssl-dev  libicu-dev  libgdal-dev gdal-bin libgeos-dev libproj-dev libsqlite3-dev libudunits2-dev libssl-dev  make libsodium-dev zlib1g-dev  libsodium-dev  git make zlib1g-dev pandoc libpng-dev libjpeg-dev libgit2-dev libssl-dev libicu-dev libcurl4-openssl-dev  libudunits2-dev  git make libgit2-dev libssl-dev libcurl4-openssl-dev  libxml2-dev 

RUN apt-get update && apt-get install -y  gdal-bin git-core libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libicu-dev libpng-dev libproj-dev libsodium-dev libssl-dev libudunits2-dev libxml2-dev make pandoc pandoc-citeproc unixodbc-dev zlib1g-dev
RUN apt-get install -y unixodbc unixodbc-dev libsqliteodbc odbc-postgresql --install-suggests

RUN rm -rf /var/lib/apt/lists/*

# ODBC driver
ADD https://dev.mysql.com/get/Downloads/Connector-ODBC/8.0/mysql-connector-odbc-8.0.31-linux-glibc2.27-x86-64bit.tar.gz .
RUN tar -C . -xzvf mysql-connector-odbc-8.0.31-linux-glibc2.27-x86-64bit.tar.gz
RUN cp -r mysql-connector-odbc-8.0.31-linux-glibc2.27-x86-64bit/bin/* /usr/local/bin
RUN cp -r mysql-connector-odbc-8.0.31-linux-glibc2.27-x86-64bit/lib/* /usr/local/lib
RUN myodbc-installer -a -d -n "MySQL ODBC 8.0 Driver" -t "Driver=/usr/local/lib/libmyodbc8w.so"


RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = TRUE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages(c("renv","remotes"))'
COPY renv.lock.prod renv.lock
RUN R -e 'renv::restore()'

COPY streetnamer_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never")'
RUN rm /app.tar.gz
EXPOSE 3838