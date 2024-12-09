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
write_rds(template,here("data/well_template.rds"))

# Oklahoma
ok <- vroom(here("data/RawLogs/Oklahoma.csv"))%>%
  select(WELL_ID,FIRST_WTR,CONST_DATE,APPROX_YLD,USE_CLASS,
         TOTAL_DPTH,LATITUDE,LONGITUDE,LL_METHOD)%>%
  mutate(Install_Date = CONST_DATE)
colnames(ok) <- c("Well_ID","Dpth_to_Water","Dpth_Date","Pump_Rate",
                  "Well_Type","Well_Depth","Latitude","Longitude","XY_Source","Install_Date")

ok2 <- vroom(here("data/RawLogs/OK_Sites_Final.txt"))%>%
  mutate(State = "Oklahoma",Dpth_Unit = "feet")%>%
  select(State,Site_ID,Date_Time,WLevel,Dpth_Unit,OWRB_OWRB_SITE_LATITUDE,OWRB_OWRB_SITE_LONGITUDE)

colnames(ok2) <- c("State","Well_ID","Dpth_Date","Dpth_to_Water","Dpth_Unit","Latitude","Longitude")

okCombine <- bind_rows(ok,ok2)

okAdd <- okCombine%>%
  mutate(State = "Oklahoma",
         Dpth_Unit = "ft",
         Pump_Rate_Unit = "GPM",
         Well_Depth_Unit = "ft",
         Well_ID = as.character(Well_ID))

# Attach to template
okDone <- bind_rows(template,okAdd)

vroom_write(okDone, here("data/Clean_Well_Logs/Oklahoma.tsv"), delim = "\t")
