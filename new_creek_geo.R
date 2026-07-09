##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                          BREAKING UP NEW CREEK GEO                       ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load necessary packages
library(tidyverse)
library(sf)
library(here)

# read in new creek geometry
new_creek_geo <- read_sf(here("data", "new_creek.geojson")) 

# transform new_creek geo to leaflet projection
new_creek_geo <- st_transform(new_creek_geo, crs = '+proj=longlat +datum=WGS84')

new_creek_samples <- data.frame(
  # sampling site IDs and lat/long
  id = c("New 0.5", "New 1", "New 1.5", "New 2", "New 3"),
  lng = c(-73.3187, -73.3169, -73.3151, -73.3138, -73.3128),
  lat = c(41.11657, 41.11884, 41.12076, 41.12334, 41.13064),
  bact = c(20, 98, 624, 201, 108)) %>% 
  
  st_as_sf(coords = c("lng", "lat"), crs = st_crs(new_creek_geo))

#                  Segmentize New Creek Geo                   ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create one single linestring feature from geometry
new_creek_seg <- st_segmentize(new_creek_geo, dfMaxLength = 50)

## break into individual 2-point segments
# creates coordinates from the linestring we created
coords <- st_coordinates(new_creek_seg)[, 1:2]

# takes all the coordinates and makes them into their own, little linestrings (line between each two coords)
segs <- lapply(1:(nrow(coords) - 1), function(i) st_linestring(coords[i:(i+1), ]))

# create an sf object from those little linestrings, using the same CRS a our original geo
segs <- st_sf(geometry = st_sfc(segs, crs = st_crs(new_creek_geo)))

# calculate midpoint of each tiny linestring (stored as a matrix, NOT an sf object yet)
mid_coords <- (coords[-nrow(coords), ] + coords[-1, ]) / 2

# use the midpoints, and create sf objects from them (using st_point)
mids <- st_sf(geometry = st_sfc(lapply(1:nrow(mid_coords), function(i)
  st_point(mid_coords[i, ])), crs = st_crs(new_creek_geo)))

# find the nearest sample to the midpoints
nearest <- st_nearest_feature(mids, new_creek_samples)

# add a column for bacteria concentration based on the index of the nearest sample site
segs$conc <- new_creek_samples$bact[nearest]


#                        Map in Leaflet                       ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create color palette from the concentrations
pal <- colorNumeric(palette = "viridis", domain = segs$conc)

# map in leaflet
leaflet(segs) %>%
  addTiles() %>%
  addPolylines(color = ~pal(conc), weight = 4, opacity = 1) %>%
  addLegend(pal = pal, values = ~conc, title = "Bacteria conc.")