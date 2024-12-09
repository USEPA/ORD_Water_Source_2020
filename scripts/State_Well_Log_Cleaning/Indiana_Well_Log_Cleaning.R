library(tidyverse)
library(vroom)
library(here)
library(sf)

logFiles <- list.files(here("data/RawLogs/Indiana/Logs/"),
                       pattern = "logs", full.names = TRUE)

logs <- vroom(logFiles, delim = "\t")

recFiles <- list.files(here("data/RawLogs/Indiana/Records/"),
                       pattern = "records", full.names = TRUE)

recs <- vroom(recFiles[1])%>%
  select(RefNum,County1,Completed,Twp1,Rng1,Sec1,Quar1,Quar2,Quar3,
         LotNum,FeetNS,FeetSN,FeetEW,FeetWE,"UTM-X","UTM-Y",
         Depth,Static,PumpRate,WellUse,DateComplete,BedRockD)%>%
  mutate(UID = paste0(County1,Twp1,Rng1,Sec1))

for(n in 2:length(recFiles)){
  new <- vroom(recFiles[n])%>%
    select(RefNum,County1,Completed,Twp1,Rng1,Sec1,Quar1,Quar2,Quar3,
           LotNum,FeetNS,FeetSN,FeetEW,FeetWE,"UTM-X","UTM-Y",
           Depth,Static,PumpRate,WellUse,DateComplete,BedRockD)%>%
    mutate(UID = paste0(County1,Twp1,Rng1,Sec1))
  recs <- rbind(recs,new)
}

# convert from UTM
utm <- recs%>%
  filter(!is.na(`UTM-X`)&!is.na(`UTM-Y`))%>%
  st_as_sf(coords = c('UTM-X','UTM-Y'), crs = 26916)%>%
  st_transform(4269)

NADcoords <- as.data.frame(st_coordinates(utm))

utm$Latitude <- NADcoords$Y
utm$Longitude <- NADcoords$X

NAD <- utm%>%
  st_drop_geometry()%>%
  select(RefNum,Completed,Latitude,Longitude,
         Depth,Static,PumpRate,WellUse,DateComplete,BedRockD)%>%
  mutate(XY_Source = "Coords")

# Use PLSS for the rest of the wells

# Import PLSS coords for quarter / quarter-quarter / quarter-quarter-quarter sections

q <- st_read(here("data/PLSS/IndianaQ.gpkg"))%>%
  st_centroid()
qCoords <- as.data.frame(st_coordinates(q))

q$Latitude <- qCoords$Y
q$Longitude <- qCoords$X

qDf <- q%>%
  st_drop_geometry()

qqLayers <- data.frame(layer = st_layers(here("data/PLSS/Indiana.gpkg"))$name)%>%
  filter(substr(layer,1,3)=="QQ_")

qq <- st_read(here("data/PLSS/Indiana.gpkg"), layer = qqLayers$layer[1])%>%
  st_centroid()
qqCoords <- as.data.frame(st_coordinates(qq))
qq$Latitude <- qqCoords$Y
qq$Longitude <- qqCoords$X

qqDf <- qq%>%
  st_drop_geometry()

for(n in 2:nrow(qqLayers)){
  qqNext <- st_read(here("data/PLSS/Indiana.gpkg"), layer = qqLayers$layer[n])%>%
    st_centroid()
  qqNextCoords <- as.data.frame(st_coordinates(qqNext))
  qqNext$Latitude <- qqNextCoords$Y
  qqNext$Longitude <- qqNextCoords$X
  
  qqNextDf <- qqNext%>%
    st_drop_geometry()
  
  qqDf <- rbind(qqDf,qqNextDf)
}

# QQQ sections
qqqLayers <- data.frame(layer = st_layers(here("data/PLSS/Indiana.gpkg"))$name)%>%
  filter(substr(layer,1,3)=="QQQ")

qqq <- st_read(here("data/PLSS/Indiana.gpkg"), layer = qqqLayers$layer[1])%>%
  st_centroid()
qqqCoords <- as.data.frame(st_coordinates(qqq))
qqq$Latitude <- qqqCoords$Y
qqq$Longitude <- qqqCoords$X

qqqDf <- qqq%>%
  st_drop_geometry()

for(n in 2:nrow(qqqLayers)){
  qqqNext <- st_read(here("data/PLSS/Indiana.gpkg"), layer = qqqLayers$layer[n])%>%
    st_centroid()
  qqqNextCoords <- as.data.frame(st_coordinates(qqqNext))
  qqqNext$Latitude <- qqqNextCoords$Y
  qqqNext$Longitude <- qqqNextCoords$X
  
  qqqNextDf <- qqqNext%>%
    st_drop_geometry()
  
  qqqDf <- rbind(qqqDf,qqqNextDf)
}

allQ <- qqqDf%>%
  bind_rows(qqDf,qDf)%>%
  mutate(qJoin = paste0(County,"_",Township,"_",Range,"_",Section,"_",QSec,"_",QQSec,"_",QQQSec))





#subset wells that need plss locations
wellsPlss <- recs%>%
  filter(is.na(`UTM-X`) | is.na(`UTM-Y`))%>%
  drop_na(Twp1,Rng1,Sec1)%>%
  mutate(County1 = tolower(County1),
         County1 = str_replace(County1,". ",""))


# Fix township and range
## Import plss polygons
plssShp <- st_read("D:/data/PLSS/IN_CadNSDI_V2.gdb", layer = "PLSSIntersected")%>%
  select(TWNSHPNO,TWNSHPDIR,RANGENO,RANGEDIR)%>%
  st_make_valid()

## Import counties
cnty <- st_read("D:/data/Census/cb_2021_us_all_500k.gdb",layer = "cb_2021_us_county_500k")%>%
  filter(STATE_NAME == "Indiana")%>%
  select(NAME)

# Intersect plss with counties
plssIntrsct <- st_intersection(plssShp,cnty)

# count number of unique township directions by county
ut <- plssIntrsct%>%
  st_drop_geometry()%>%
  mutate(NAME = tolower(NAME),
         NAME = str_replace(NAME,". ",""))%>%
  select(NAME,TWNSHPDIR)%>%
  distinct()

# Return counties with only one possible township direction
utTbl <- as.data.frame(table(ut$NAME))%>%
  filter(Freq == 1)%>%
  left_join(ut, by = c("Var1" = "NAME"))

# count number of unique range directions by county
ur <- plssIntrsct%>%
  st_drop_geometry()%>%
  mutate(NAME = tolower(NAME),
         NAME = str_replace(NAME,". ",""))%>%
  select(NAME,RANGEDIR)%>%
  distinct()

# Return counties with only one possible range direction
urTbl <- as.data.frame(table(ur$NAME))%>%
  filter(Freq == 1)%>%
  left_join(ur, by = c("Var1" = "NAME"))

# Fix incorrect townships
twnRight <- wellsPlss%>%
  filter(substr(Twp1,nchar(Twp1),nchar(Twp1))%in%LETTERS)
twnWrong <- wellsPlss%>%
  filter(!substr(Twp1,nchar(Twp1),nchar(Twp1))%in%LETTERS)%>%
  left_join(utTbl, by = c("County1" = "Var1"))%>%
  mutate(Twp1 = paste0(Twp1,TWNSHPDIR))%>%
  drop_na(TWNSHPDIR)

twnCrct <- twnRight%>%
  bind_rows(twnWrong)

# Fix incorrect ranges
rngRight <- twnCrct%>%
  filter(substr(Rng1,nchar(Rng1),nchar(Rng1))%in%LETTERS)

rngWrong <- twnCrct%>%
  filter(!substr(Rng1,nchar(Rng1),nchar(Rng1))%in%LETTERS)%>%
  left_join(urTbl, by = c("County1" = "Var1"))%>%
  mutate(Rng1 = paste0(Rng1,RANGEDIR))%>%
  drop_na(RANGEDIR)

rngCrct <- rngRight%>%
  bind_rows(rngWrong)


# Create join ID and join coordinates
coordFix <- rngCrct%>%
  mutate(qJoin = paste0(County1,"_",Twp1,"_",Rng1,"_",Sec1,"_",Quar3,"_",Quar2,"_",Quar1))%>%
  left_join(allQ)%>%
  drop_na(Latitude,Longitude)%>%
  select(RefNum,Completed,Latitude,Longitude,
         Depth,Static,PumpRate,WellUse,DateComplete,BedRockD)%>%
  mutate(XY_Source = "PLSS")

# Merge data
allData <- NAD%>%
  bind_rows(coordFix)%>%
  select(!DateComplete)

colnames(allData) <- c("Well_ID","Install_Date","Latitude","Longitude","Well_Depth","Dpth_to_Water","Pump_Rate",
                       "Well_Type","Dpth_to_Bedrock","XY_Source")
# Write to Template
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

inReady <- template%>%
  bind_rows(allData)%>%
  mutate(State = "IN",
         Dpth_Unit = "ft",
         Pump_Rate_Unit = "GPM",
         Dpth_to_Bedrock_Unit = "ft",
         Well_Depth_Unit = "ft")

vroom_write(inReady, here("data/Clean_Well_Logs/Indiana.tsv"), delim = "\t")
