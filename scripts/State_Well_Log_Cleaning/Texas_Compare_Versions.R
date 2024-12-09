library(tidyverse)
library(vroom)
library(here)

txNew <- vroom("data/Clean_Well_Logs/Texas.tsv")%>%
  drop_na(Dpth_to_Water)

newFilt <- txNew%>%
  filter(!is.na(Dpth_to_Water))%>%
  filter(!is.na(Dpth_Date) | !is.na(Install_Date))

# Import well info from Texas GWDB
txGWlocs <- vroom(here("data/RawLogs/Texas/GWDBDownload/WellMain.txt"),
                  col_types = c(PluggingReportTrackingNumber = "i",
                                OtherWellNumber = "c",
                                PreviousStateWellNumber = "c"))%>%
  select(StateWellNumber,LatitudeDD,LongitudeDD,CoordinateSource,WellDepth,
         DrillingEndDate,WellUse)

# Import groundwater levels from Texas GWDB
txGW <- vroom(list.files(
  here("data/RawLogs/Texas/GWDBDownload/"),
  pattern = "WaterLevels",
  full.names = TRUE),
  col_types = c(Remarks = "c",Comments = "c"))%>%
  select(StateWellNumber,Status,MeasurementDate,DepthFromLSD)%>%
  filter(!Status == "No Measurement")

# Join measurements to wells
txJoin <- txGWlocs%>%
  left_join(txGW)%>%
  filter(!is.na(DepthFromLSD) & Status == "Publishable")
colnames(txJoin) <- c("Well_ID","Latitude","Longitude","XY_Source","Well_Depth",
                      "Install_Date","Well_Type","Status","Dpth_Date","Dpth_to_Water")


