

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, scales, magick, ggspatial, RColorBrewer, classInt, rgeos, exactextractr, gtools, stringr, glue, geodata, rnaturalearthdata, rnaturalearth)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Functions ---------------------------------------------------------------
theme_for_the_win <- function(){
  theme_void() +
    theme(
      legend.position = "top",
      legend.title = element_text(
        size = 9, color = "grey20"
      ),
      legend.text = element_text(
        size = 9, color = "grey20"
      ),
      plot.margin = unit(
        c(
          t = 1, r = 0, # Add 1
          b = 0, l = 0 
        ), "lines"
      )
    )
}

# World vector data -------------------------------------------------------
wrld <- ne_countries(returnclass = 'sf', scale = 50)

# Raster data -------------------------------------------------------------
rstr <- terra::rast('./tif/c02/emi_tons_timeseries.tif')

# To calculate the zonal  -------------------------------------------------
znal <- exact_extract(x = rstr, y = wrld, fun = 'sum')
znal <- as_tibble(znal)
znal <- mutate(znal, iso = wrld$iso_a3)
znal <- dplyr::select(znal, iso, everything())

# All the values as a vector ----------------------------------------------
vles <- round(as.numeric(unlist(as.vector(znal[,2:ncol(znal)]))), 0)
brks <- classInt::classIntervals(var = vles, n = 5, style = 'fisher')
brks <- brks$brks
brks <- round(brks, -2.5)
clss <- tibble(class = 1:5, min = brks[1:5], max = brks[2:6], interval = glue('{min}-{max}'))

# To join the table with the shapefile ------------------------------------
shpf <- inner_join(wrld, znal, by = c('iso_a3' = 'iso'))

# To make the map  --------------------------------------------------------

## Function to use -----------
mke.map <- function(yr){
  
  cat('>>> Process: ', yr, '\n')
  shp <- dplyr::select(shpf, iso_a3, name, glue('sum.emi_ton_{yr}'), geometry)
  colnames(shp)[3] <- 'value'
  vls <- pull(shp, value)
  
  # To find the interval for the values
  shp <- mutate(shp, class = findInterval(x = vls, vec = brks, all.inside = TRUE))
  shp <- inner_join(shp, clss, by = 'class')
  shp <- mutate(shp, interval = factor(interval, levels = clss$interval))
  
  # To make the map
  gmp <- ggplot() + 
    geom_sf(data = shp, aes(fill = interval)) + 
    scale_fill_manual(values = brewer.pal(n = 5, name = 'YlOrRd')) + 
    coord_sf() + 
    theme_minimal() + 
    labs(fill = 'GEI (Ton)', 
         caption = 'Adaptado de EDGAR (Emissions Database for Global Atmospheric Research)') + 
    ggtitle(label = glue('Emisiones totales de GEI por país - {yr}')) +
    theme(
      legend.position = 'bottom', 
      text = element_text(family = 'Segoe UI'), 
      plot.title = element_text(size = 16, face = 'bold', hjust = 0.5)
    ) +
    guides(fill = guide_legend( 
      direction = 'horizontal',
      keyheight = unit(3.5, units = "mm"),
      keywidth = unit(35, units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) 
  gmp
  
  ggsave(plot = gmp, filename = glue('./png/maps/gei_{yr}.jpg'), units = 'in', width = 9, height = 6.5, dpi = 300)
  
  # Finish!
  cat('Done!\n')
  
}

## To make the maps ----------
map(1970:2022, mke.map)

# To make the GIF from the maps -------------------------------------------

## To compile the GIF
imgs <- dir_ls('./png/maps')
imgs <- map(imgs, image_read)
jnds <- image_join(imgs)
anmt <- magick::image_animate(jnds, fps = 10)

dir_create('./gif')

## To write the GIF
image_write(image = anmt, path = './gif/gei_10fps.gif')


