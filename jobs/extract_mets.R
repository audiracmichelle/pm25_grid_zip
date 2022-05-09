library(tidyverse)
library(sp)
library(raster)
library(argparse)

parser <- ArgumentParser()
parser$add_argument("-i", "--index", default=1,
                    help="Year month index", type="integer")
args = parser$parse_args()

# args=list()
# args$index = 2

# read mets objects
mets <- read_rds("data/input/mets.rds")

## download zipcode polygons
crs_wgs84 = "+proj=longlat +datum=WGS84"
zip_sp = shapefile("data/input/local/gz_2010_us_860_00_500k/gz_2010_us_860_00_500k.shp")
zip_sp = spTransform(zip_sp, CRSobj=CRS(crs_wgs84))

## crosswalk
rast = projectRaster(mets[[args$index]], crs=crs_wgs84)
zip_sp$zip_concentrations = 0

# iterating over zip codes
pbar = progress::progress_bar$new(total=nrow(zip_sp))
for (i in 1:nrow(zip_sp)) {
  zip_i = zip_sp[i, ]
  rast_i = try(crop(rast, extent(zip_i)), silent = T)
  if(class(rast_i) != "try-error"){
    rast_i = rasterToPolygons(rast_i)
    if(!is.null(rast_i)) {
      zip_sp$zip_concentrations[i] = over(zip_i, rast_i, fn = mean)[[1]]
    } else zip_sp$zip_concentrations[i] = NA
  } else zip_sp$zip_concentrations[i] = NA
  
  pbar$tick()
}

savefile = paste0(
  "data/output/local/mets_",
  args$index,
  ".rds"
)

write_rds(zip_sp, savefile)
