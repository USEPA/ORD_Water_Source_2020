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

md <- vroom(here("data/RawLogs/Maryland.csv"))%>%
  select(STATE,Addr_type,X,Y,LAT_DEC_DEG,LON_DEC_DEG,PERMIT,COMPLETION_DATE,EST_GPM_PRODUCED,APPROX_DEPTH,TOTAL_DEPTH,Use_for_water_sim,
         PUMPING_RATE,LEVEL_BEFORE,LEVEL_DURING,ABANDONED,ABANDON_DATE,REPLACEMENT)%>%
  mutate(LAT_DEC_DEG = ifelse(is.na(LAT_DEC_DEG),Y,LAT_DEC_DEG),
         LON_DEC_DEG = ifelse(is.na(LON_DEC_DEG),X,LON_DEC_DEG),
         Install_Date = as.character(lubridate::mdy_hms(COMPLETION_DATE)),
         XY_Source = ifelse(is.na(Addr_type),"State",paste0("GCode-",Addr_type)),
         Pump_Rate = ifelse(is.na(PUMPING_RATE),EST_GPM_PRODUCED,PUMPING_RATE))%>%
  select(STATE,PERMIT,LEVEL_BEFORE,Pump_Rate,Install_Date,Use_for_water_sim,TOTAL_DEPTH,LAT_DEC_DEG,LON_DEC_DEG,XY_Source)

colnames(md) <- c("State","Well_ID","Dpth_to_Water","Pump_Rate","Install_Date","Well_Type","Well_Depth","Latitude","Longitude","XY_Source")

mdFormat <- md%>%
  mutate(State = "MD",
         Dpth_Unit = "ft",
         Pump_Rate_Unit = "GPM",
         Well_Depth_Unit = "ft")

# Save output
mdReady <- template%>%
  bind_rows(mdFormat)

vroom_write(mdReady, here("data/Clean_Well_Logs/Maryland.tsv"), delim = "\t")
