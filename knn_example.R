# Oscar the grouch playground finder?

# Oscar needs to find a new trashcan to live in, and he wants to find one
# near a nice park. What is the nearest park to each trash can

# Identify the nearest neighbor playground (sf object) from each trash can

library(tidyverse)
library(sf)
library(FNN)

# Load in Philadelphia wastebaskets

trashCans <- st_read("https://opendata.arcgis.com/datasets/5cf8e32c2b66433fabba15639f256006_0.geojson") %>%
  st_transform(4326)

# Load in Philadelphia playgrounds

playGrounds <- st_read("https://opendata.arcgis.com/datasets/899c807e205244278b3f39421be8489c_0.geojson") %>%
  st_transform(4326)

# Turn each data frame into a matrix

trashCansXY <- st_coordinates(trashCans %>% 
                             select(geometry)) %>% 
  as.matrix()

# Turn the trash cans in to centroids... and then into a matrix

playGroundsXY <- st_coordinates(playGrounds %>% 
                              st_as_sf(., coords = c("lon", "lat"), crs = 4326) %>%
                              select(geometry)) %>% 
  as.matrix()

# Create a crosswalk for parcel objectid in the sf and parcel objectid in the matrix (they are slightly different)

playGround_and_matrixXY <- cbind(playGroundsXY %>% 
                                as.data.frame() %>% 
                                cbind(., playGround_id=seq_len(nrow(.))), 
                              playGrounds %>% 
                                as.data.frame() %>%
                                select(-geometry))

# Do the same for the crimes

trashCans_and_matrixXY <- cbind(trashCansXY %>% 
                               as.data.frame() %>% 
                               cbind(., trashCan_id=seq_len(nrow(.))), #bind the matrix to a column id
                             trashCans %>%
                               as.data.frame() %>%
                               select(-geometry))

# Now run the get.knnx function for the 1 nearest neighbor and return a data frame of crimes
# alongside the nearest parcel's objectID and block_face_info

test <- get.knnx(playGroundsXY, trashCansXY, 1)$nn.index %>%
  as.data.frame() %>%  # Here we just have a list of nearest playgrounds in sequence
  cbind(., trashCan_id = seq_len(nrow(.))) %>%  # The row number is actually the trashCan row number
  left_join(., trashCans_and_matrixXY, 
            by = c("trashCan_id" = "trashCan_id")) %>% 
  rename(playGround_id = V1) %>%
  left_join(., playGround_and_matrixsXY %>% 
              rename(playGroundX = X,
                     playGroundY = Y), 
            by = c("playGround_id" = "playGround_id"))

# Did it work? It seems so.

ggplot()+
  geom_point(data = test %>%
               filter(PARK_NAME %in% c("Cedar Park", "48th and Woodland Playground")),
             aes(x = X, y = Y))+
  geom_sf(data = playGrounds %>%
            filter(PARK_NAME %in% c("Cedar Park", "48th and Woodland Playground")),
          color = "red")
