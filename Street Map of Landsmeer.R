## Mapping exercise of the streets and waterways of the city of Landsmeer, Netherlands.

# Load all necessary packages -----
library(tidyverse)
library(osmdata)
library(sf)


# Data exploration ---------

# Display all features that OpenStreetMap saves
available_features()

# Interest for this map is on: "highway", "natural" and "waterway" features

# Print  cooordinates of our city (nwrite the city and the country as a string)
getbb("Landsmeer Nederland")
#  These will help framing the final plot



# Data extraction ------------

# Store the streets in the coordinates of the city in an object
big_streets <- getbb("Landsmeer Nederland")%>%
    opq()%>%
    add_osm_feature(key = "highway", 
                    value = c("motorway", "primary", 
                              "secondary", "tertiary")) %>%
    osmdata_sf()


# Extract small streets from city coordinates
small_streets <- getbb("Landsmeer Netherlands")%>%
    opq()%>%
    add_osm_feature(key = "highway", 
                    value = c("residential", "living_street",
                              "unclassified",
                              "service", "footway")) %>%
    osmdata_sf()


# Extract river waterways from city coordinates
river <- getbb("Landsmeer Netherlands") %>%
    opq()%>%
    add_osm_feature(key = "waterway", value = c("river","canal","ditch","riverbank")) %>%
    osmdata_sf()

water <- getbb("Landsmeer Netherlands") %>%
    opq()%>%
    add_osm_feature(key = "natural", value = "water") %>%
    osmdata_sf()



# Plot all OSM elements together ----------

ggplot() +
    geom_sf(data = river$osm_lines,
            inherit.aes = FALSE,
            color = "#ffbe7f",
            size = .2,
            alpha = .5) +
    geom_sf(data = water$osm_multipolygons,
            inherit.aes = FALSE,
            color = "#6baed6",fill = "#6baed6",
            size = .2,
            alpha = .5) +
    geom_sf(data = big_streets$osm_lines,
            inherit.aes = FALSE,
            color = "#7fc0ff",
            size = .4,
            alpha = .8) +
    geom_sf(data = small_streets$osm_lines,
            inherit.aes = FALSE,
            color = "#ffbe7f",
            size = .2,
            alpha = .6) +
    theme_void() +
    coord_sf(xlim = c(4.89 , 4.96),
             ylim = c(52.41, 52.48773),
             expand = FALSE) +
    theme(
        plot.background = element_rect(fill = "#282828")
    )

    # theme(plot.background = element_rect(fill = "#282828"),
    #       axis.title.x = element_text(size = 20, family = "Helvetica", 
    #                                 #face="bold", 
    #                                 hjust=.5, color="#ffbe7f") ) +
    # labs(xlab = "LANDSMEER") 


# Export map ------------

ggsave("map.png", width = 6, height = 6)
