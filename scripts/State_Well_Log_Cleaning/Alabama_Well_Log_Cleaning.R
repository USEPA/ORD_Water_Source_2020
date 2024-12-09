library(tidyverse)
library(vroom)
library(sf)
library(here)

wl <- vroom(here("data/RawLogs/Alabama/WaterLevel.csv"))

wells <- vroom(here("data/RawLogs/Alabama/Wells.csv"))

join <- wells%>%
  left_join(wl, by = "FacilityID")

wellsSf <- wells%>%
  drop_na(Longitude27)%>%
  st_as_sf(coords = c("Longitude27","Latitude27"), crs = 4267)%>%
  st_transform(4326)%>%
  left_join(wl, by = "FacilityID")%>%
  select(FacilityID,MeasurementDate,WaterLevel,WaterLevelElevation,PumpingWaterLevel,
         PumpingRate)

coords <- as.data.frame(st_coordinates(wellsSf))

wellsSf$Latitude_WGS84 <- coords$Y
wellsSf$Longitude_WGS84 <- coords$X

colnames(wellsSf) <- c("ID","Date","Level","Level_Elv","Pump_Lvl",
                       "Pump_Rate","geometry","Lat_WGS84","Lon_WGS84")

st_write(wellsSf, here("data/RawLogs/Alabama/WaterLevelJoin.shp"),append = FALSE)

missingLoc <- wl%>%
  filter(!FacilityID %in% wells$FacilityID)
