# library(rgbif)
library(leaflet)
library(tidyverse)
library(sf)
library(terra)

# get species data --------------------------------------------------------
# my species for modelling
species <- c(
  "Scirpophaga excerptalis",
  "Sesamia grisescens",
  "Chilo auricilius",
  "Chilo infuscatellus",
  "Eumetopina flavipes",
  "Matsumuratettix hiroglyphicus",
  "Yamatotettix flavovittatus",
  "Perkinsiella saccharicida",
  "Perkinsiella thompsoni"
)

get_species <- function(x, genus = FALSE){
  out <- x %>%
    str_split(" ") %>%
    unlist()
  if(genus){
    return(out[1])
  } else{
    out[2]
  }
}


splist <- list()
for(i in seq_along(species)){

  gbif_data <- geodata::sp_occurrence(genus = get_species(species[i], TRUE),
                                      species = get_species(species[i], FALSE))

  # take a look at the downloaded data:
  # gbif_data %>% View()

  if(is.null(gbif_data)) next

  sp_coords <- gbif_data %>%
    dplyr::select(lon, lat, status = occurrenceStatus,
                  country, species, genus, family) %>%
    drop_na(lon, lat)

  if(nrow(sp_coords) < 2) next

  sp_points <- st_as_sf(sp_coords, coords = c("lon", "lat"))
  # sp_points

  splist[[i]] <- sp_points
}

# check the number of species
unlist(map(splist, nrow))

# combine all species data
sp_all <- splist %>%
  do.call(bind_rows, .) %>%
  st_set_crs(4326)

head(sp_all)
nrow(sp_all)


species_palette <- colorFactor(palette = viridis::inferno(length(unique(sp_all$species))),
                               domain = unique(sp_all$species))
leaflet() %>%
  addTiles() %>%
  # addMarkers(data = sp_points)
  addCircleMarkers(
    data = sp_all,
    radius = 6,
    stroke = FALSE,
    label = ~species,
    color = ~species_palette(species),
    fillOpacity = 0.4
  )




# raster data -------------------------------------------------------------
# download bias layer
bias_layer <- geodata::travel_time(path = "data/travel_layer.tif",
                                   size = 1,
                                   up = TRUE)
plot(bias_layer)

# download bioclim layers
bioclim <- geodata::worldclim_global(var = "bio",
                                     res = 0.5,
                                     path = "data/bioclim.tif")
plot(bioclim)


plot(bioclim[[1]])
plot(sp_points$geometry, add = TRUE)

leaflet() %>%
  addTiles() %>%
  addRasterImage(raster::raster(clim[[1]])) %>%
  addMarkers(data = sp_points)

r <- clim[[1]] %>%
  terra::aggregate(fact = 5) %>%
  raster::raster()

library(mapview)

mapview(r)


# Crop raster layers ------------------------------------------------------
fls <- list.files("data/bioclim.tif/wc2.1_30s/",
                  pattern = ".tif$",
                  full.names = TRUE)
bioclim <- terra::rast(fls)
plot(bioclim)

bg_mask <- st_read("data/background_mask.gpkg")
plot(st_geometry(bg_mask))

# mask raster layers one-by-one
for(i in 15:nlyr(bioclim)){
  masked <- terra::mask(bioclim[[i]], vect(bg_mask))
  terra::writeRaster(masked, paste0("data/bg_layers/", names(bioclim)[i], ".tif"))
  print(i)
}


# KDE for background sampling ---------------------------------------------
# loading required libraries
library(spatialEco)
library(terra)
library(disdat)
library(dismo)
library(sf)

# read TGB data
tgbs <- st_read("data/tgbs.gpkg")
# read a raster mask for the region
rs <- terra::rast("data/bg_layers/wc2.1_30s_bio_1.tif")

plot(rs)
plot(st_geometry(tgbs), add = TRUE, pch = 16, cex = 0.3)

# remove duplicated points in raster cells
samplecellID <- terra::cellFromXY(rs, st_coordinates(tgbs)) 
dup <- duplicated(samplecellID)
tgbsp <- tgbs[!dup, ]

nrow(tgbs)
nrow(tgbsp)
# st_write(tgbsp, "data/tgbs_reduced.gpkg")

tgb_kde <- spatialEco::sp.kde(x = sf::as_Spatial(tgbsp),
                              bw = 5, # degree
                              newdata = raster::raster(rs),
                              standardize = TRUE,
                              scale.factor = 10000)
plot(tgb_kde)


# create background data with accessibility bias --------------------------
# read species data
sp_all <- st_read("data/species_data.gpkg")

# accessibility map
access <- terra::rast("data/accessibility_masked.tif")
plot(access)
setMinMax(access)

# standardize the map
rmin <- terra::minmax(access)[1]
rmax <- terra::minmax(access)[2]
access_std <- (rmax - access) / (rmax - rmin)
plot(access_std)

# calculate the distance map
distr <- terra::distance(access, vect(sp_all))
plot

# create sparate bg records for each species
bg_df <- data.frame()
for(i in seq_along(unique(sp_all$species))){
  
  kde_samples <- dismo::randomPoints(tgb_kde, 
                                     n = 10000, 
                                     prob = TRUE)
  bg_df <- kde_samples %>% 
    as.data.frame() %>%
    mutate(species = unique(sp_all$species)[i]) %>% 
    bind_rows(bg_df)
  
}

nrow(bg_df)








