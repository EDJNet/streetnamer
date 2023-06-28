FROM streetnamer_base
COPY renv.lock.prod renv.lock
RUN R -e 'renv::restore()'

RUN Rscript -e 'remotes::install_github("giocomai/latlon2map", upgrade="never", ref = "5f6bc95")'

COPY streetnamer_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never")'
RUN rm /app.tar.gz
EXPOSE 3838
