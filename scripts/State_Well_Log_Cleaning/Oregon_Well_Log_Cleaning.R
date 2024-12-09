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

df <- st_read(here("data/RawLogs/Oregon_05032022.gdb"), layer = "well_report_pt")%>%
  st_drop_geometry()%>%
  mutate(State = "Oregon",
         Well_ID = paste0("OR_",wl_id),
         Dpth_to_Water = depth_first_water,
         Dpth_Unit = "feet",
         Dpth_Date = lubridate::date(complete_date),
         Pump_Rate = yield_gpm,
         Pump_Rate_Unit = "GPM",
         Dpth_to_Bedrock = NA,
         Dpth_to_Bedrock_Unit = NA,
         Install_Date = lubridate::date(complete_date),
         Well_Type = primary_use,
         Well_Depth = completed_depth,
         Well_Depth_Unit = "feet",
         Latitude = latitude_dec,
         Longitude = longitude_dec,
         XY_Source = coordinate_source)%>%
  select(colnames(template))

vroom_write(df, here("data/Clean_Well_Logs/Oregon_clean.tsv"), delim = "\t")
