# pm25_grid_zip

```
docker build -t geospatial_argparse .
docker tag  geospatial_argparse audiracmichelle/geospatial_argparse:<tag>
docker run -v $(pwd):/home/rstudio/kitematic/ -it audiracmichelle/geospatial_argparse /bin/sh
```

```
Rscript --vanilla jobs/extract_pm25.R -r 2013.tif -n 60 -c 60 -o "test/" 
```

x <- "Rscript --vanilla jobs/extract_pm25.R -r 2013.tif -n 60 -c "
writeLines(paste0(x, 1:60), "jobs")
