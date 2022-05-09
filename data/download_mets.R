# download_mets
library(tidyverse)
library(lubridate)
library(raster)
library(ncdf4)
library(rnaturalearth)

## -------------------------------------------------------------------- ##
##     define functions
## -------------------------------------------------------------------- ##
# download the necessary met files, 20th century reanalysis
downloader.fn <- function( filename,
                           dataset = c( '20thC_ReanV2c', 'ncep.reanalysis.derived', 'NARR')){
  if( length( dataset) > 1)
    dataset <- dataset[1]
  #fileloc <- file.path('~', 'Dropbox', 'Harvard', 'RFMeval_Local', 'Comparisons_Intermodel', 'Global_meteorology', dataset)
  fileloc <- file.path('~', 'tmp', dataset)
  
  # create directory to store in
  dir.create( fileloc, 
              recursive = T, 
              showWarnings = F)
  
  # name variable, filenames
  varname_NOAA <- gsub( "\\..*", "", filename)
  file_NOAA <- file.path( fileloc, filename)
  
  # define URL
  if( dataset == '20thC_ReanV2c'){
    # https://www.esrl.noaa.gov/psd/data/gridded/data.20thC_ReanV2c.monolevel.mm.html
    url_NOAA <- paste0( "ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV2c/Monthlies/gaussian/monolevel/", filename)
  } else if( dataset == 'ncep.reanalysis.derived'){
    url_NOAA <- paste0( "ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.derived/surface/", filename)
  }else if( dataset == 'NARR'){
    url_NOAA <- paste0( "ftp://ftp.cdc.noaa.gov/Datasets/NARR/Monthlies/monolevel/", filename)
  }
  
  if( !file.exists( file_NOAA))
    download.file( url = url_NOAA,
                   destfile = file_NOAA)
  
  hpbl_rasterin <- brick( x = file_NOAA,
                         varname = varname_NOAA)
  
  return( hpbl_rasterin)
  
}

#' Read netcdf file and extract date
#'
#' \code{subset_nc_date}  takes as input a netcdf file location, date, and variable name
#' and outputs the data as a raster layer.
#'
#' @param hpbl_file netcdf file path
#' @param varname variables name
#' @param vardate variable date
#' @return raster layer of input file, variable, and date

subset_nc_date <- function( hpbl_file = NULL,
                            hpbl_brick = NULL,
                            varname = NULL,
                            vardate){
  
  if( (is.null( hpbl_file)  & is.null( hpbl_brick)) |
      (!is.null( hpbl_file) & !is.null( hpbl_brick)))
    stop( "Uh oh! Please define EITHER hpbl_file OR hpbl_brick")
  
  Sys.setenv(TZ='UTC')
  
  if( !is.null( hpbl_file))
    rasterin <- rotate( brick( hpbl_file, varname = varname ))
  if( !is.null( hpbl_brick))
    rasterin <- hpbl_brick
  
  #get time vector to select layers
  dates <- names( rasterin)
  dates <- gsub( 'X', '', dates)
  dates <- gsub( '\\.', '-', dates)
  
  # Get first day of the month for vardate
  vardate_month <- as.Date( paste( year( vardate),
                                   month( vardate),
                                   '01',
                                   sep = '-'))
  
  #select layer
  layer <- which( dates == vardate_month)
  if( length( layer) == 0)
    stop( "Cannot match the dates of PBL raster file. Did you set the time zone to UTC before reading it in? (Sys.setenv(TZ='UTC'))")
  
  rastersub <- raster::subset(rasterin, subset = layer)
  
  return(rastersub)
}

# download the necessary met files, 20th century reanalysis
extract_year.fn <- function( raster.in = list.met[[1]],
                             year.in = NULL,
                             month.in = NULL,
                             dataset = c( '20thC_ReanV2c', 'ncep.reanalysis.derived', 'NARR'),
                             return.t = 'annual'){
  
  # default to 20th cent reanalysis
  if( length( dataset) > 1){
    dataset <- dataset[1]
    print( paste( 'No dataset specified, defaulting to', dataset))
  }
  
  # name months 1:12 for extracting from raster
  if( return.t == 'annual'){
    if( is.null( year.in))
      stop( 'return.t = "annual" requires year.in to be defined')
    names.months <- paste0( year.in, '-',
                            formatC( 1:12, width = 2, flag = '0'), '-',
                            '01')
  } else if( return.t == 'monthly'){
    if( is.null( month.in))
      stop( 'return.t = "monthly" requires month.in to be defined')
    names.months <- month.in
  } else
    stop( 'return.t must be either "annual" or "monthly"')
  
  # extract monthly dates using function from hyspdisp
  raster.sub <- subset_nc_date(  hpbl_brick = raster.in,
                                 vardate = names.months)
  
  #NARR dataset requires rotating
  # if( dataset != 'NARR')
  #   raster.sub <- rotate( raster.sub)
  
  # take annual mean
  if( return.t == 'annual'){
    raster.sub.mean <- stackApply( raster.sub, indices = rep( 1, 12), fun = mean)
  } else if( return.t == 'monthly'){
    raster.sub.mean <- raster.sub
  } else
    stop( 'return.t must be either "annual" or "monthly"')
  
  return( raster.sub.mean)
}

# trim data over US, combine into data.table, create spatial object
# X can be either year.in or month.in
usa.functioner <- function( X = 2005,
                            list.met,
                            dataset = c( '20thC_ReanV2c', 'ncep.reanalysis.derived', 'NARR'),
                            return.usa.sub = F,
                            return.t = 'annual', 
                            return.type = c('raster','data.table', 'sp')){
  crs.usa <- crs( "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")
  
  # download USA polygon from rnaturalearth
  usa <- ne_countries(scale = 110, type = "countries", country = "United States of America", geounit = NULL, sovereignty = NULL,
                      returnclass = c("sp"))
  usa.sub <- disaggregate(usa)[6,]
  usa.sub <- spTransform(usa.sub, CRSobj = crs.usa)
  
  if(return.usa.sub){
    usa.sub.p <- spTransform(usa.sub, CRSobj = crs.usa)
    usa.sub.sf <- data.table(st_as_sf( usa.sub.p))
    return(usa.sub.sf)
  }
  
  # extract year
  if(return.t == 'annual'){
    mets <- brick(lapply(list.met,
                         extract_year.fn,
                         year.in = X,
                         dataset = dataset,
                         return.t = return.t))
  } else if(return.t == 'monthly'){
    mets <- brick(lapply(list.met,
                         extract_year.fn,
                         month.in = X,
                         dataset = dataset,
                         return.t = return.t))
  } else
    stop('return.t must be either "annual" or "monthly"')
  
  # calculate windspeed
  # calculate meteorology wind angle (0 is north wind)
  # http://weatherclasses.com/uploads/3/6/2/3/36231461/computing_wind_direction_and_speed_from_u_and_v.pdf
  if( 'uwnd' %in% names( list.met) & 'vwnd' %in% names( list.met)){
    mets$wspd <- sqrt( mets$uwnd ^ 2 + mets$vwnd ^ 2)
    mets$phi <- atan2( mets$uwnd, mets$vwnd) * 180 / pi + 180
  }
  
  # reproject
  mets.usa <- projectRaster( mets, crs = crs.usa)
  
  # crop to USA
  mets.usa <- crop( mask(mets.usa, usa.sub), usa.sub)
  
  if(return.type[1] == "raster"){
    
    return(mets.usa)
    
  }else if(return.type[1] == "sp"){
    
    # convert rasters to sf - differences
    mets.usa.sp <- rasterToPolygons(mets.usa)
    return(mets.usa.sp)
    
  }else if(return.type[1] == "data.table"){
    
    # convert rasters to sf - annual
    mets.usa.sf <- data.table(st_as_sf(mets.usa.sp))[, time := X]
    # merge with coordinates - annual
    coords <- st_coordinates(st_centroid(mets.usa.sf$geometry))
    mets.usa.sf <- cbind(mets.usa.sf, coords)
    # return sf object
    return( mets.usa.sf)
    
  }
}


## -------------------------------------------------------------------- ##
##     Download the layers
## -------------------------------------------------------------------- ##

#define the layer names, do the actual downloading
Sys.setenv(TZ='UTC')
layer.names <- c("air.2m.mon.mean.nc",
                 "rhum.2m.mon.mean.nc",
                 "apcp.mon.mean.nc",
                 "vwnd.10m.mon.mean.nc",
                 "uwnd.10m.mon.mean.nc")
names( layer.names) <- c( "temp", "rhum", "prec", "vwnd", "uwnd")

# do the data downloading
list.met <- lapply(layer.names,
                   downloader.fn,
                   dataset = 'NARR')

## -------------------------------------------------------------------- ##
##     xtract monthly means
## -------------------------------------------------------------------- ##
mets <- lapply(seq.Date(as.Date('2004-01-01'), as.Date('2006-12-31'), by = 'month'),
               usa.functioner,
               list.met,
               dataset = 'NARR',
               return.t = 'monthly')

write_rds(mets, "./input/mets.rds")
