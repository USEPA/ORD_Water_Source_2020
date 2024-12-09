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

wa <- vroom(here("data/RawLogs/Washington.csv"))%>%
  select(WELL_LOG_I,WELL_COMP_,WELL_DEPTH,WELLLOG_ST,WELLLOG_GP,NAD83LATIT,NAD83LONGI)%>%
  mutate(Install_Date = as.character(lubridate::mdy_hms(WELL_COMP_)),
         WELL_LOG_I = as.character(WELL_LOG_I))%>%
  select(-WELL_COMP_)

colnames(wa) <- c("Well_ID","Well_Depth","Dpth_to_Water","Pump_Rate","Latitude","Longitude","Install_Date")

waReady <- template%>%
  bind_rows(wa)%>%
  mutate(State = "WA",
         Dpth_Unit = "ft",
         Well_Depth_Unit = "ft",
         Dpth_Date = Install_Date,
         Pump_Rate_Unit = "GPM")

vroom_write(waReady, here("data/Clean_Well_Logs/Washington2.tsv"), delim = "\t")
