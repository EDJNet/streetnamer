FROM streetnamer_base
COPY renv.lock.prod renv.lock
RUN R -e 'renv::restore()'

RUN Rscript -e 'remotes::install_github("giocomai/latlon2map", upgrade="never", ref = "2a2f0acbf68c7e00ef662b9058ace811a2e40379")'

COPY streetnamer_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never")'
RUN rm /app.tar.gz
EXPOSE 3838
