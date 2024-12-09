library(tidyverse)
library(vroom)
library(readxl)
library(here)

template <- read_rds(here("data/well_template.rds"))

# Check csv file names
csvFiles <- list.files(here("data/Clean_Well_Logs/csv/"),
                       pattern = ".csv$",full.names = TRUE)

csvNames <- list.files(here("data/Clean_Well_Logs/csv/"),
                       pattern = ".csv$",full.names = FALSE)

# Data frame of all column names
names <- data.frame(ColNum = seq(1,30,1))

for(n in 1:length(csvFiles)){
  names2 <- data.frame(fileName = colnames(vroom(csvFiles[n])))
  
  names[1:nrow(names2),n+1] <- names2[,1]
  colnames(names)[n+1] <- csvNames[n]
}

# Alabama, Georgia & Idaho need columns fixed
## Alabama
alFix <- vroom(here("data/Clean_Well_Logs/csv/Alabama.csv"))%>%
  select(!c("Facility Name","Address1","Address2","City","County","Aquifer"))
colnames(alFix) <- colnames(template)
vroom_write(alFix, here("data/Clean_Well_Logs/Alabama.tsv"), delim = "\t")
## Idaho
idFix <- vroom(here("data/Clean_Well_Logs/csv/Idaho.csv"))%>%
  select(!c('Address1','City','County','Removal Date'))
colnames(idFix) <- c("Well_ID","Dpth_to_Water","Dpth_Unit","Dpth_Date",
                     "Install_Date","Latitude","Longitude","Well_Type","Well_Depth")
idClean <- idFix%>%
  mutate(State = "Idaho",
         Dpth_Unit = "ft",
         Well_ID = as.character(Well_ID))%>%
  drop_na(Dpth_to_Water)
idDone <- template%>%
  bind_rows(idClean)
vroom_write(idDone, here("data/Clean_Well_Logs/Idaho.tsv"), delim = "\t")
## Georgia
gaFix <- vroom(here("data/Clean_Well_Logs/csv/Georgia.csv"))%>%
  select('Well ID','Depth to Water','Measurement Date','Install Date',
         Latitude,Longitude,'Well Type','Total Depth')
colnames(gaFix) <- c("Well_ID","Dpth_to_Water","Dpth_Date",
                     "Install_Date","Latitude","Longitude","Well_Type","Well_Depth")
gaAdd <- gaFix%>%
  mutate(State = "Georgia",
         Dpth_Unit = "ft",
         Install_Date = paste0(substr(as.character(Install_Date),1,4),
                               "/",substr(as.character(Install_Date),5,6),
                               "/",substr(as.character(Install_Date),7,8)))
gaDone <- template%>%
  bind_rows(gaAdd)
vroom_write(gaDone, here("data/Clean_Well_Logs/Georgia.tsv"), delim = "\t")