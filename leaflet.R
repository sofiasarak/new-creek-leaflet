library(tidyverse)
library(leaflet)
library(leaftime) # for time functionality
library(sf)
library(here)
library(viridis) # color palette

# read in new creek geometry
new_creek_geo <- read_sf(here("data", "new_creek.geojson")) 

# transform new_creek geo to leaflet projection
new_creek_geo <- st_transform(new_creek_geo, crs = '+proj=longlat +datum=WGS84')

#........................CREATE DATA FRAME.......................

# create dataframe of New Creek sampling sites
new_creek <- data.frame(
  
  # sampling site IDs and lat/long
  id = rep(c("New 0.5", "New 1", "New 1.5", "New 2", "New 3"), each = 5), # 5 = number of dates
  lng = rep(c(-73.3187, -73.3169, -73.3151, -73.3138, -73.3128), each = 5),
  lat = rep(c(41.11657, 41.11884, 41.12076, 41.12334, 41.13064), each = 5),
  
  # sampling dates (included as strings and not dateTime objects to avoid big gaps in slider)
  date = rep(c("2025-05-12", "2025-06-05", "2025-06-09", "2025-06-23", "2025-07-24"), 5), # 5 = number of locations
  
  # bacteria levels
  bact = c(20, 10, 109, 20, 723,       # New 0.5
           98, 272, 7701, 317, 8146,   # New 1
           624, 677, 857, 5794, 6488,  # New 1.5
           201, 461, 1126, 980, NA,    # New 2
           108, 272, 472, 1986, 2318)  # New 3
)

#.................CONVERT DATA TO LEAFLET FORMAT.................

# match dates to ID numbers (for time slider)
date_lookup <- new_creek%>%
  distinct(date) %>%
  mutate(time_index = row_number()) # Gives May 11 = 1, June 4 = 2

# match time slider index to data, and create start and end time for timeline
prepared_data <- new_creek %>%
  left_join(date_lookup, by = "date") %>%
  mutate(
    # Timeline steps through integer blocks instead of true calendar ranges
    start = time_index,
    end   = time_index + 1,
    point_color = pal(bact) # Assumes your pal object is defined
  )

# Convert mapped index data into the timeline GeoJSON structure
geojson_features <- geojsonio::geojson_json(prepared_data, lat = "lat", lon = "lng")


# define continuous color palette
pal <- colorNumeric(palette = "viridis", domain = new_creek$bact)


#                        Map in Leaflet                       ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
leaflet() %>%
  addTiles() %>%
  
  # set default zoom to New 1.5 sampling site
  setView(lng = -73.3151, lat = 41.12076, zoom = 13.4) %>% 
  
  # add new creek polygon
  addPolygons(data = new_creek_geo, color = "darkblue", stroke = 1) %>%  
  
  addTimeline(
    data = geojson_features,
    # Map the custom colors based on row data
    timelineOpts = timelineOptions(
      styleOptions = htmlwidgets::JS(
        "function(data) {
           return {
             radius: 10,
             fillColor: data.properties.point_color,
             color: data.properties.point_color,
             fillOpacity: 0.8,
             weight: 2
           };
         }"
      )
    ),
    # Force the slider tool to translate the integers 1, 2... back into "May 11", "June 4"
    sliderOpts = sliderOptions(
      step = 1,
      formatOutput = htmlwidgets::JS(
        sprintf(
          "function(date) {
       var labels = %s;
       var idx = Math.round(date) - 1;
       return '<strong>Date: </strong>' + labels[idx];
     }",
          jsonlite::toJSON(date_lookup$date)
        )
      )
    )) %>% 
  # RESTORED LEGEND: Ensure this uses your original palette logic
  addLegend(
    pal = pal, 
    values = new_creek$bact, 
    title = "Bacteria Levels", 
    position = "bottomright"
  )

#............................OLD MAP.............................

leaflet() %>%
  addTiles() %>%
  
  # set default zoom to Noroton 3 sampling site
  setView(lng = -73.51443, lat = 41.0953, zoom = 13) %>% 
  
  # add Noroton river geometry
  addPolygons(data = noroton_river, color = "darkblue", stroke = 1) %>%  
  
  addPolygons(data = new_creek, color = "darkblue", stroke = 1) %>%  
  
  # add Noroton sampling site markers
  addCircleMarkers(data = noroton_sites,
                   popup = ~site,
                   color = ~pal_num(bact),
                   fillColor = ~pal_num(bact),
                   fillOpacity = 0.9,
                   stroke = FALSE) %>%  
  
  # add legend
  addLegend(
    data = noroton_sites,
    pal = pal_num, 
    values = ~bact, 
    title = "Bacteria\nConcentration")

