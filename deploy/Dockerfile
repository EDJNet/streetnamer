FROM streetnamer_base
COPY renv.lock.prod renv.lock
RUN R -e 'renv::restore()'
RUN Rscript -e 'remotes::install_github("giocomai/latlon2map", upgrade="never", ref = "f346600bd770d95659faec09a5fd98059db9258f")'
RUN Rscript -e 'remotes::install_github("EDJNet/tidywikidatar", upgrade="never", ref = "0a2cf61fc7b34627e1cd2f5d1c50084cacffcf6e")'

COPY streetnamer_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never")'
RUN rm /app.tar.gz
EXPOSE 3838
