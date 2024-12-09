library(tidyverse)
library(sf)
library(here)



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

wells <- st_read(here("data/RawLogs/Illinois/IL_Wells.gdb"), layer = "Wells_UTM16")%>%
  st_drop_geometry()%>%
  select(latitude,longitude,td,welltype,statlevl,pumpgpm,cdate,location_s)%>%
  mutate(State = "IL",
         Well_ID = paste0(State,seq(1,n())),
         Dpth_to_Water = statlevl,
         Dpth_Unit = "ft",
         Dpth_Date = as.character(lubridate::ymd(paste0(substr(cdate,1,4),"/",substr(cdate,5,6),"/",substr(cdate,7,8)))),
         Pump_Rate = pumpgpm,
         Pump_Rate_Unit = "GPM",
         Install_Date = Dpth_Date,
         Well_Type = welltype,
         Well_Depth = td,
         Well_Depth_Unit = "ft",
         Latitude = latitude,
         Longitude = longitude,
         XY_Source = location_s)%>%
  select(State,Well_ID,Dpth_to_Water,Dpth_Unit,Dpth_Date,Pump_Rate,Pump_Rate_Unit,Install_Date,
         Well_Type,Well_Depth,Well_Depth_Unit,Latitude,Longitude,XY_Source)

ilDone <- template%>%
  bind_rows(wells)

vroom::vroom_write(ilDone, here("data/Clean_Well_Logs/Illinois.tsv"), delim = "\t")
