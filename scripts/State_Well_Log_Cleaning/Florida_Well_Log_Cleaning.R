library(tidyverse)
library(sf)
library(vroom)
library(here)
library(readxl)

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

# Saint Johns
sj <- vroom(here("data/RawLogs/Florida/StJohns.csv"))%>%
  select(stn_id, well_use, well_cur_dpth,date_drilled,static_water_level, lat_no_dd,long_no_dd)
colnames(sj) <- c("Well_ID","Well_Type","Well_Depth","Install_Date","Dpth_to_Water","Latitude","Longitude")

sjReady <- sj%>%
  mutate(Install_Date = as.character(lubridate::ymd(substr(Install_Date,1,10))),
         Well_ID = as.character(Well_ID))

# Southwest ( No level data)
sw <- vroom(here("data/RawLogs/Florida/southwest.csv"))

# Suwannee
# Correct coordinates
suCoords <- vroom(here("data/RawLogs/Florida/Suwannee_Coords_Correct.csv"),
                  col_types = c("Location_Address2" = "c","Delineated__" = "c",
                                "Range" = "c","Legacy__"="c"))%>%
  select(Permit__,Y,X,Completion_Date, Total_Depth,Well_Use,Static_Water_Level__ft_)%>%
  mutate(Completion_Date = as.character(lubridate::parse_date_time(Completion_Date,c("mdy HM","mdy HMS"))))
colnames(suCoords) <- c("Well_ID","Latitude","Longitude","Install_Date","Well_Depth","Well_Type","Dpth_to_Water")

# Northwest
nw <- vroom(here("data/RawLogs/Florida/Northwest.csv"),
            col_types = c("StDir" = "c","Field22" = "c"))%>%
  select(OID_,Addr_type,X,Y,TOTAL_WELL_DEPTH,STATIC_WATER_LEVEL)%>%
  mutate(XY_Source = paste0("GCode-",Addr_type),
         Well_ID = as.character(OID_))%>%
  select(-c(OID_,Addr_type))

colnames(nw) <- c("Longitude","Latitude","Well_Depth","Dpth_to_Water","XY_Source","Well_ID")

# South
sf <- vroom(here("Data/RawLogs/Florida/DBHYDRO_Wells_and_Boreholes.csv"))



flOut <- template%>%
  bind_rows(sjReady,suCoords,nw)%>%
  mutate(State = "FL",
         Dpth_Unit = "ft")

vroom_write(flOut, here("data/Clean_Well_Logs/Florida.tsv"), delim = "\t")
