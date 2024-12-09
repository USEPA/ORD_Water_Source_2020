library(tidyverse)
library(lubridate)
library(sf)
library(here)
library(vroom)

template <- data.frame(State = character(),
                       Well_ID = character(),
                       Dpth_to_Water = double(),
                       Dpth_Unit = character(),
                       Dpth_Date = character(),
                       Pump_Rate = double(),
                       Pump_Rate_Unit = character(),
                       Dpth_to_Bedrock = double(),
                       Dpth_to_Bedrock_Unit = character(),
                       Install_Date = character(),
                       Well_Type = character(),
                       Well_Depth = double(),
                       Well_Depth_Unit = character(),
                       Latitude = double(),
                       Longitude = double(),
                       XY_Source = character())


tn <- st_read(here("data/RawLogs/TennesseeCoordFix.shp"))%>%
  select(WELL_NUMBR,CMPLTN_DAT,CMPLTN_TOT,CMPLTN_STA,WELL_USE)%>%
  mutate(CMPLTN_DAT = as.character(lubridate::ymd(substr(CMPLTN_DAT,1,10))),
         WELL_NUMBR = as.character(WELL_NUMBR))

coords <- as.data.frame(st_coordinates(tn))

tnDf <- tn%>%
  st_drop_geometry()

tnDf$Longitude <- coords$X
tnDf$Latitude <- coords$Y


tnFilt <- tnDf%>%
  filter(Longitude > -91 & Longitude < -81 & Latitude > 34 & Latitude < 37)

colnames(tnDf) <- c("Well_ID","Install_Date","Well_Depth","Dpth_to_Water","Well_Type","Longitude","Latitude")

tnReady <- template%>%
  bind_rows(tnDf)%>%
  mutate(State = "TN",
         Dpth_Unit = "ft",
         Well_Depth_Unit = "ft")

vroom_write(tnReady, here("data/Clean_Well_Logs/Tennessee.tsv"), delim = "\t")
