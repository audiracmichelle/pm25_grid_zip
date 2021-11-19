FROM rocker/geospatial:3.6.3

RUN install2.r --error \
    --deps TRUE \
    argparse

WORKDIR /home/rstudio/kitematic
