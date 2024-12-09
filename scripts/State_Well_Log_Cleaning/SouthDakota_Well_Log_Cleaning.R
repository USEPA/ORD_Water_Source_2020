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

sdFiles <- list.files(here("data/RawLogs/SouthDakota"), pattern = ".csv",
                      full.names = TRUE)

sd <- vroom(sdFiles)%>%
  mutate(State = "South Dakota",
         Well_ID = paste0("SD_",GUID),
         Dpth_to_Water = ifelse(staticType == "depth",staticValue,NA),
         Dpth_Unit = "feet",
         Dpth_Date = mdy(WellCompDate),
         Pump_Rate = NA,
         Pump_Rate_Unit = NA,
         Dpth_to_Bedrock = NA,
         Dpth_to_Bedrock_Unit = NA,
         Install_Date = mdy(WellCompDate),
         Well_Type = WellType,
         Well_Depth = Depth,
         Well_Depth_Unit = "feet",
         Latitude = Lat,
         Longitude = Long,
         XY_Source = NA)%>%
  select(colnames(template))

vroom_write(sd,here("data/Clean_Well_Logs/SouthDakota_clean.tsv"), delim = "\t")
