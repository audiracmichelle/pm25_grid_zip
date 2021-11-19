library(tidyverse)
library(stringr)
library(sp)
library(raster)
library(argparse)

parser <- ArgumentParser()
parser$add_argument("-r", "--raster_file", default=NULL,
                    help="File with raster", type="character")
# will run zip codes from (c - 1)*chunk_n to c*chunk
parser$add_argument("-c", "--chunk", default=1,
                    help="Chunk of zips to run", type="integer")
parser$add_argument("-n", "--chunk_n", default=NULL,
                    help="Total number of chunks of zips", type="integer")
parser$add_argument("-d", "--dir", 
                    default="data/input/local/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-annual-geotiff/Annual-geotiff/",
                    help="Directory with rasters", type="character")
parser$add_argument("-o", "--output_dir", 
                    default="test/",
                    help="Output directory", type="character")
args = parser$parse_args()

# args=list()
# args$raster_file = "2013.tif"
# args$dir = "data/input/local/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-annual-geotiff/Annual-geotiff/"
# args$chunk_n = 1000
# args$chunk = 3
# args$output_dir = "test/"

# read desired locations
load("data/input/local/zip_easternUS.RData")
xmin = min(zip$Longitude.zip)
xmax = max(zip$Longitude.zip)
ymin = min(zip$Latitude.zip)
ymax = max(zip$Latitude.zip)
extent = c(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)

crs_wgs84 = "+proj=longlat +datum=WGS84"

if (file.exists("data/input/local/gz_2010_us_860_00_500k_sp.rds")) {
  zip_sp = read_rds("data/input/local/gz_2010_us_860_00_500k_sp.rds")
} else {
  zip_sp = shapefile("data/input/local/gz_2010_us_860_00_500k/gz_2010_us_860_00_500k.shp")
  zip_sp = zip_sp[zip_sp$ZCTA5 %in% zip$zip, ]
  zip_sp = spTransform(zip_sp, CRSobj=CRS(crs_wgs84))
  write_rds(zip_sp, "data/input/local/gz_2010_us_860_00_500k_sp.rds")
}

if (!is.null(args$raster_file)) {
  files = paste0(args$dir, args$raster_file)
} else {
  files = list.files(args$dir, full.names = TRUE)
}

N = nrow(zip_sp)

# subset zipcodes
if (is.null(args$chunk_n)) {
  range = seq(1,N)
} else{
  chunk_seq = round(seq(0,N,length.out=(args$chunk_n + 1)))
  range = seq(chunk_seq[args$chunk] + 1, chunk_seq[args$chunk+1])
}
zip_sp = zip_sp[range, ]

for (f in files) {
  print(paste("Extracting", f))
  
  rast = raster(f)
  rast = projectRaster(rast, crs=crs_wgs84)
  zip_sp$zip_concentrations = 0
  
  # iterating over zip codes
  pbar = progress::progress_bar$new(total=nrow(zip_sp))
  for (i in 1:nrow(zip_sp)) {
    zip_i = zip_sp[i, ]
    rast_i = crop(rast, extent(zip_i))
    rast_i = rasterToPolygons(rast_i)
    zip_sp$zip_concentrations[i] = over(zip_i, rast_i, fn = mean)[[1]]
    pbar$tick()
  }
  
  f_ = str_split(f, "/", simplify=TRUE)
  f_ = f_[length(f_)]
  savefile = paste0(
    "data/output/local/",
    args$output_dir,
    gsub(".tif", "_", f_),
    sprintf("%03d", args$chunk),
    ".rds"
  )
  
  print(paste("Saving", savefile))
  write_rds(zip_sp, savefile)
}
