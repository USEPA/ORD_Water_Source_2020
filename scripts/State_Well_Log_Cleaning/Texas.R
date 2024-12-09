library(tidyverse)
library(sf)
library(vroom)
library(here)

template <- read_rds(here("data/well_template.rds"))

# Import well info
wd <- vroom(here("data/RawLogs/SDRDownload/WellData.txt"),
            col_types = c(SurfaceCompletionOtherDesc = "c",
                          DrillerAddress2 = "c",
                          ProposedUseOtherDesc = "c",
                          OwnerAddress2 = "c",
                          DrillerCountry = "c",
                          TypeOfWorkOtherDesc = "c",
                          PWSNo = "c",
                          OwnerCountry = "c",
                          OriginalWellReportTrackingNumber = "c",
                          OwnerOtherCountry = "c",
                          DrillerOtherCountry = "c"
                          ))%>%
  select(WellReportTrackingNumber,CoordDDLat,CoordDDLong,
         ProposedUse,DrillingEndDate)

# Well Depth
td <- vroom(here("data/RawLogs/SDRDownload/WellBoreHole.txt"))%>%
  select(WellReportTrackingNumber,BottomDepth)

# Import water levels
levels <- vroom(here("data/RawLogs/SDRDownload/WellLevels.txt"))%>%
  select(WellReportTrackingNumber,Measurement,MeasurementDate)

# Import yield
yield <- vroom(here("data/RawLogs/SDRDownload/WellTest.txt"),
               col_types = c(TestTypeOtherDesc = "c",
                             Drawdown = "c"))%>%
  select(WellReportTrackingNumber, Yield)

merge <- wd%>%
  left_join(levels)%>%
  left_join(yield)%>%
  left_join(td)%>%
  filter(!is.na(Measurement))

format <- merge%>%
  mutate(State = "TX",
         MeasurementDate = ifelse(is.na(MeasurementDate),as.character(DrillingEndDate),as.character(MeasurementDate)),
         WellReportTrackingNumber = as.character(WellReportTrackingNumber),
         DrillingEndDate = as.character(DrillingEndDate),
         Yield = as.numeric(Yield))
colnames(format) <- c("Well_ID","Latitude","Longitude","Well_Type","Install_Date",
                      "Dpth_to_Water","Dpth_Date","Pump_Rate","Well_Depth","State" )

txOut <- template%>%
  bind_rows(format)%>%
  mutate(Dpth_Unit = "ft",
         Pump_Rate_Unit = "GPM",
         Well_Depth_Unit = "ft")%>%
  filter(!is.na(Dpth_to_Water))

# Add data from Texas GWDB
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
  filter(!is.na(DepthFromLSD) & Status == "Publishable")%>%
  select(!Status)
colnames(txJoin) <- c("Well_ID","Latitude","Longitude","XY_Source","Well_Depth",
                      "Install_Date","Well_Type","Dpth_Date","Dpth_to_Water")

txJoinFormat <- txJoin%>%
  mutate(State = "TX",
         Install_Date = as.character(Install_Date),
         Dpth_Date = as.character(Dpth_Date),
         Dpth_Unit = "ft",
         Well_Depth_Unit = "ft")

txAll <- txOut%>%
  bind_rows(txJoinFormat)
vroom_write(txAll, here("data/Clean_Well_Logs/Texas.tsv"), delim = "\t")
