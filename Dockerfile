FROM rocker/shiny:4.1.3
RUN apt-get update && apt-get install -y  gdal-bin git-core libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libicu-dev libpng-dev libproj-dev libsodium-dev libssl-dev libudunits2-dev libxml2-dev make pandoc pandoc-citeproc unixodbc-dev zlib1g-dev
RUN apt-get install -y unixodbc unixodbc-dev libsqliteodbc odbc-postgresql --install-suggests
RUN rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.1.8")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.6.2")'
RUN Rscript -e 'remotes::install_version("stringi",upgrade="never", version = "1.7.8")'
RUN Rscript -e 'remotes::install_version("fs",upgrade="never", version = "1.5.2")'
RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.4.1")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.3")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.3")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.3")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.10")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.40")'
RUN Rscript -e 'remotes::install_version("usethis",upgrade="never", version = "2.1.6")'
RUN Rscript -e 'remotes::install_version("RSQLite",upgrade="never", version = "2.2.18")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.2.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.5")'
RUN Rscript -e 'remotes::install_version("odbc",upgrade="never", version = "1.3.3")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.17")'
RUN Rscript -e 'remotes::install_version("tictoc",upgrade="never", version = "1.1")'
RUN Rscript -e 'remotes::install_version("dbplyr",upgrade="never", version = "2.2.1")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.1.1")'
RUN Rscript -e 'remotes::install_version("cicerone",upgrade="never", version = "1.0.4")'
RUN Rscript -e 'remotes::install_version("shinyauthr",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.7.4")'
RUN Rscript -e 'remotes::install_version("countrycode",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("waiter",upgrade="never", version = "0.2.5")'
RUN Rscript -e 'remotes::install_version("tidywikidatar",upgrade="never", version = "0.5.5")'
RUN Rscript -e 'remotes::install_version("sf",upgrade="never", version = "1.0-8")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.5")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.26")'
RUN Rscript -e 'remotes::install_github("giocomai/latlon2map@6d2e029f8144472dfad35d5fc6fba1dbe9d00373")'

# ODBC driver
ADD https://dev.mysql.com/get/Downloads/Connector-ODBC/8.0/mysql-connector-odbc-8.0.31-linux-glibc2.27-x86-64bit.tar.gz .
RUN tar -C . -xzvf mysql-connector-odbc-8.0.31-linux-glibc2.27-x86-64bit.tar.gz
RUN cp -r mysql-connector-odbc-8.0.31-linux-glibc2.27-x86-64bit/bin/* /usr/local/bin
RUN cp -r mysql-connector-odbc-8.0.31-linux-glibc2.27-x86-64bit/lib/* /usr/local/lib
RUN myodbc-installer -a -d -n "MySQL ODBC 8.0 Driver" -t "Driver=/usr/local/lib/libmyodbc8w.so"


RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 3838