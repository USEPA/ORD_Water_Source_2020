library(tidyverse)
library(sf)
library(here)
library(vroom)

nh <- st_read(here("data/RawLogs/Water_Well_Inventory.shp"))

template <- read_rds(here("data/well_template.rds"))

wl <- nh%>%
  filter(!is.na(SWL))%>%
  st_drop_geometry()%>%
  select(WRB_,SWL,DMEAS,YTQ,BDKD,DCOMP,USE_,TOTD,LATITUDE,LONGITUDE)
colnames(wl) <- c("Well_ID","Dpth_to_Water","Dpth_Date","Pump_Rate","Dpth_to_Bedrock",
                  "Install_Date","Well_Type","Well_Depth","Latitude","Longitude")

nhClean <- wl%>%
  mutate(State = "NH",
         Dpth_Unit = "ft",
         Pump_Rate_Unit = "GPM",
         Dpth_to_Bedrock_Unit = "ft",
         Well_Depth_Unit = "ft",
         Dpth_to_Water = as.numeric(Dpth_to_Water),
         Pump_Rate = as.numeric(Pump_Rate),
         Dpth_to_Bedrock = as.numeric(Dpth_to_Bedrock),
         Well_Depth = as.numeric(Well_Depth),
         Install_Date = as.character(Install_Date))

nhDone <- template%>%
  bind_rows(nhClean)

vroom_write(nhDone, here("data/New_Hampshire.tsv"), delim = "\t")
