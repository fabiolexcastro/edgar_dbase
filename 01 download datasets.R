
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, rgeos, gtools, stringr, glue, geodata, rnaturalearthdata, rnaturalearth)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# World vector data -------------------------------------------------------
wrld <- ne_countries(returnclass = 'sf', scale = 50)
wrld <- vect(wrld)

# To download -------------------------------------------------------------

## URL and output directory
root <- 'https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/CO2bio/TOTALS/emi_nc/v8.0_FT2022_GHG_CO2bio_'

## Function to use
down.data <- function(yr){
  
  ### To download
  cat('To download ', yr, '\n')
  urlw <- glue('{root}{yr}_TOTALS_emi_nc.zip')
  download.file(url = urlw, path = './zip', destfile = glue('./zip/{basename(urlw)}'))
  
  ### To unzip
  unzip(glue('./zip/{basename(urlw)}'))
  
  ### To copy the raster
  file.rename(from = gsub('_nc.zip', '.nc', basename(urlw)), to = glue('./tif/c02/{gsub("_nc.zip", ".nc", basename(urlw))}'))
  cat('Finish!\n')
  
}

## To apply the function
map(1970:2022, down.data)

# To create just one raster and extract by mask for the world -------------
fles <- dir_ls('./tif/c02', regexp = '.nc$')
rstr <- rast(fles)
rstr <- terra::crop(rstr, wrld)
rstr <- terra::mask(rstr, wrld)

year <- basename(fles) %>% str_split(., pattern = '_') %>% map_chr(5)
names(rstr) <- glue('emi_ton_{year}')
terra::writeRaster(x = rstr, filename = glue('./tif/c02/emi_tons_timeseries.tif'), overwrite = TRUE)






