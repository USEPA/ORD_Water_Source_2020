library(tidyverse)
library(vroom)

fips <- select(tidycensus::fips_codes,state_code,state)%>%
  distinct()

# Predicted classes
predclass.files <- data.frame(path = list.files("Water_USe/outputs/data/reclassify_na/",full.names = TRUE),
                              file = list.files("Water_USe/outputs/data/reclassify_na/",full.names = FALSE))%>%
  mutate(state = substr(file,1,2))

predClass <- vroom(predclass.files$path)%>%
  mutate(GISJOIN_BG = substr(GISJOIN,1,15),
         state_code = substr(GISJOIN,2,3))%>%
  left_join(fips)%>%
  select(!`...1`)%>%
  distinct()

dups <- predClass%>%
  group_by(GISJOIN)%>%
  summarise(nRows = n())%>%
  filter(nRows > 1)

temp <- predClass%>%
  filter(GISJOIN %in% check$GISJOIN)

check <- predClass%>%
  filter(is.na(Intake_mi) & HU_2020>0)

vroom_write(predClass,"Water_Use/outputs/data/US_Blks_Reclass_NEW.csv",delim = ",")

predClass <- vroom("Water_Use/outputs/data/US_Blks_Reclass_NEW.csv")

check <- predClass%>%
  filter(GISJOIN == "G48014500008004026")

pub2priv <- predClass%>%
  filter(HU_1990 > 0 & PctPub90 == 100 & Prob_Pub < 0.5)

pub2priv <- predClass%>%
  filter(HU_1990 > 0 & PctPub90 == 100 & Prob_Pub < 0.5)
