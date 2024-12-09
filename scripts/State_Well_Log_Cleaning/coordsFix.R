library(sf)
library(tidyverse)

files <- list.files(here("data/Clean_Well_Logs/"),
                    pattern = ".tsv$",full.names = TRUE)

states <- st_read("D:/data/Census/cb_2021_us_all_500k.gdb",layer = "cb_2021_us_state_500k")%>%
  filter(!NAME %in% c("Alaska","Hawaii","American Samoa","Commonwealth of the Northern Mariana Islands",
                      "United States Virgin Islands","Puerto Rico","Guam"))%>%
  st_cast("MULTILINESTRING")


# Florida
fl <- vroom("D:/Github/Groundwater_Depth/data/Clean_Well_Logs/Florida.tsv")

flC <- fl%>%
  filter(Longitude < -75 & Latitude < 32)%>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)

flW <- fl%>%
  filter(Longitude >= -75 & Latitude >= 32)%>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 2236,remove = FALSE)%>%
  st_transform(4269)

flB <- states%>%
  filter(NAME == "Florida")%>%
  st_transform(4326)

ggplot(flC)+
  geom_sf()+
  geom_sf(data = flB)

# download data
s3 <- paws::s3()
s3$list_objects(bucket = "my_bucket")

s3$put(Bucket = "nwfwatersgs")
