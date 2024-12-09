library(tidyverse)
library(vroom)
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
                       Longtude = double(),
                       XY_Source = character())

files <- list.files("data/Well Logs/Downloaded/", pattern = "Arkansas",full.names = TRUE)

wells <- vroom(files)%>%
  distinct()%>%
  mutate(State = "Arkansas",
         Well_ID = paste0("AR_",`Well ID`),
         Dpth_to_Water = `Water Level`,
         Dpth_Unit = "feet",
         Dpth_Date = lubridate::mdy(`Date Well Completed`),
         Pump_Rate = Yield,
         Pump_Rate_Unit = "GPM",
         Dpth_to_Bedrock = NA,
         Dpth_to_Bedrock_Unit = NA,
         Install_Date = Dpth_Date,
         Well_Type = `Use Type`,
         Well_Depth = `Well Depth`,
         Well_Depth_Unit = "feet",
         Latitude = Latitude,
         Longitude = Longitude,
         XY_Source = NA)%>%
  select(State, Well_ID,Dpth_to_Water,Dpth_Unit,
         Dpth_Date,Pump_Rate,Pump_Rate_Unit,Dpth_to_Bedrock,
         Dpth_to_Bedrock_Unit,Install_Date,Well_Type,
         Well_Depth,Well_Depth_Unit,Latitude,Longitude,
         XY_Source)

vroom_write(wells, "data/Well Logs/Clean/Arkansas_clean.tsv", delim = "\t")
