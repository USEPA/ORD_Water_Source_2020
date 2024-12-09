# This script performs final validations on the output data

library(tidyverse)
library(vroom)
library(here)
# Join GEOID and GISJOIN
nhgis <- vroom("D:/data/nhgis/tables/Blocks/nhgis0307_ds248_2020_block.csv")%>%
  select(GISJOIN,GEOID)%>%
  mutate(GEOID = substr(GEOID,10,24))

## Load Well Estimates
est <- vroom(here("outputs/Final_Classification.csv"))%>%
  left_join(nhgis, by = c("GEOID_Blk"="GEOID"))


# Load well to block joins
blk.files <- list.files("data/Well Logs/Census_Blocks/", full.names = TRUE)
blk.joins <- vroom(blk.files)

blk.agg <- blk.joins%>%
  select(GISJOIN,Well_ID,Install_Date)%>%
  group_by(Well_ID)%>%
  mutate(Install_Date = max(Install_Date))%>%
  ungroup()%>%
  distinct()%>%
  group_by(GISJOIN)%>%
  summarise(nWells = n())

compare <- est%>%
  left_join(blk.agg)

df <- compare%>%
  mutate(nWells = replace_na(nWells,0),
         HU_2020 = replace_na(HU_2020,0))%>%
  group_by(State,Final_Class)%>%
  summarise(Wells = sum(nWells),
            HU = sum(HU_2020))%>%
  pivot_wider(names_from = Final_Class, values_from = Wells)%>%
  mutate(Pct_Private = 100*(Private/(Private+Public)))


# VALIDATION 1: Blocks against training states
# Load Training data
train <- vroom()

# Validation 2: Well Logs Against Classified Blocks
well.joins <- list.files("Analysis/Accurate_Well_Blocks", full.names = TRUE)
wells.a <- vroom(well.joins)%>%
  filter(Accurate == TRUE)
  group_by(GISJOIN)%>%
  summarise(Wells = n())

sf <- wells.a%>%
  st_as_sf(coords = c("X","Y"), crs = st_crs(check))
  
temp <- st_transform(sf, st_crs(4326))

st_write(temp,"D:/temp/Wells_A.shp")
class <- vroom("outputs/Final_Classification.csv")




## Count wells by block, then join to classified data for selected states
compare <- class%>%
  filter(State %in% c("Arkansas","California","Kansas","Texas"))%>%
  left_join(nhgis, by = c("GEOID_Blk"="GEOID"))%>%
  left_join(wells.a)%>%
  filter(Wells>0)%>%
  group_by(State,Final_Class)%>%
  summarise(Wells = sum(Wells))%>%
  pivot_wider(names_from = Final_Class, values_from = Wells)

# Check Arkansas
ar <- st_read("D:/data/nhgis/boundaries/Blocks/2020/AR_block_2020.shp")%>%
  select(GISJOIN)%>%
  left_join(nhgis)%>%
  st_transform(4326)%>%
  st_make_valid()%>%
  left_join(class,c("GEOID"="GEOID_Blk"))%>%
  drop_na(Final_Class)%>%
  group_by(Final_Class)%>%
  summarise()

st_write(ar,"D:/temp/AR.shp")

census <- vroom("D:/data/nhgis/tables/Blocks/nhgis0307_ds248_2020_block.csv")%>%
  select(GISJOIN,U7B001)

ar.temp <- vroom("D:/Github/ORD_SAB_Model/Output_Data/Archive/Block_Join/AR.csv")%>%
  filter(PWSID_12 == "AR0000513")%>%
  left_join(census)
