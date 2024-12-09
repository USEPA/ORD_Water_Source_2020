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

scFiles <- list.files(here("data/RawLogs/SouthCarolina"), full.names = TRUE)

sc <- vroom(scFiles)%>%
  mutate(State = "SC",
         Well_ID = WELL_ID,
         Dpth_to_Water = WL_FT,
         Dpth_Unit = "feet",
         Dpth_Date = lubridate::mdy(paste0("01/01/",WL_YR)),
         Pump_Rate = YIELD,
         Pump_Rate_Unit = "GPM",
         Dpth_to_Bedrock = NA,
         Dpth_to_Bedrock_Unit = NA,
         Install_Date = lubridate::mdy(paste0("01/",DRILL_MO,"/",DRILL_YR)),
         Well_Type = WELL_USE,
         Well_Depth = DEPTH_C,
         Well_Depth_Unit = "feet",
         Latitude = Lat_DD_NAD83,
         Longitude = Lon_DD_NAD83,
         XY_Source = NA)%>%
  select(colnames(template))%>%
    distinct()

vroom_write(sc, here("data/Clean_Well_Logs/SouthCarolina_clean.tsv"), delim = "\t")
