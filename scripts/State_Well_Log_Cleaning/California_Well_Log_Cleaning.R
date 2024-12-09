library(tidyverse)
library(vroom)
library(here)
library(sf)

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

ca <- vroom(here("data/RawLogs/California_05032022.tsv"))%>%
  mutate(State = "California",
         Well_ID = paste0("CA_",WCRNUMBER),
         Dpth_to_Water = STATICWATERLEVEL,
         Dpth_Unit = "feet",
         Dpth_Date = lubridate::mdy(DATEWORKENDED),
         Pump_Rate = WELLYIELD,
         Pump_Rate_Unit = WELLYIELDUNITOFMEASURE,
         Dpth_to_Bedrock = NA,
         Dpth_to_Bedrock_Unit = NA,
         Install_Date = DATEWORKENDED,
         Well_Type = PLANNEDUSEFORMERUSE,
         Well_Depth = TOTALCOMPLETEDDEPTH,
         Well_Depth_Unit = "feet",
         Latitude = DECIMALLATITUDE,
         Longitude = DECIMALLONGITUDE,
         XY_Source = LLACCURACY)%>%
  select(State, Well_ID,Dpth_to_Water,Dpth_Unit,
         Dpth_Date,Pump_Rate,Pump_Rate_Unit,Dpth_to_Bedrock,
         Dpth_to_Bedrock_Unit,Install_Date,Well_Type,
         Well_Depth,Well_Depth_Unit,Latitude,Longitude,
         XY_Source)

vroom_write(ca, here("data/Clean_Well_Logs/California_clean.tsv"))
