library(tidyverse)
library(vroom)
library(sf)
library(here)

files <- list.files("D:/data/wells/Hawaii", pattern = ".txt", full.names = TRUE)

dt <- vroom(files)

sf <- dt%>%
  st_as_sf(coords = c("LongDegree","LatDegree"), crs = 4326)

st_write(sf, "D:/data/wells/Hawaii/All_Wells.shp")
