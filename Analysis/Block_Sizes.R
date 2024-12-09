library(sf)
library(dplyr)
library(vroom)

tract.info <- vroom("D:/data/nhgis/tables/Tracts/nhgis0345_ds120_1990_tract.csv")%>%
  select(GISJOIN,ET1001)%>%
  setNames(c("GISJOIN","Population"))

tracts <- st_read("D:/data/nhgis/boundaries/Tracts/US_tract_1990.shp")%>%
  select(GISJOIN)%>%
  left_join(tract.info)%>%
  filter(Population>0)%>%
  st_transform(st_crs(5070))%>%
  mutate(Area_Km = as.numeric(st_area(.))/1000000)

bg.info <- vroom("D:/data/nhgis/tables/Block_Groups/nhgis0294_ds123_1990_blck_grp.csv")%>%
  select(GISJOIN,E2X001)%>%
  setNames(c("GISJOIN","Population"))

bgs <- st_read("D:/data/nhgis/boundaries/Block_Groups/US_blck_grp_1990.shp")%>%
  select(GISJOIN)%>%
  left_join(bg.info)%>%
  filter(Population>0)%>%
  st_transform(st_crs(5070))%>%
  mutate(Area_Km = as.numeric(st_area(.))/1000000)

blk.info <- vroom("D:/data/nhgis/tables/Blocks/nhgis0312_ds120_1990_block.csv")%>%
  select(GISJOIN,ET1001)%>%
  setNames(c("GISJOIN","Population"))
blk.files <- list.files("D:/data/nhgis/boundaries/Blocks/1990", recursive = TRUE, pattern = ".shp$", full.names = TRUE)
blk.areas <- data.frame()

for(n in 1:length(blk.files)){
  sf <- st_read(blk.files[n])%>%
    select(GISJOIN)%>%
    left_join(blk.info)%>%
    filter(Population>0)%>%
    st_transform(st_crs(5070))%>%
    mutate(Area_Km = as.numeric(st_area(.))/1000000)%>%
    st_drop_geometry()
  
  blk.areas <- rbind(blk.areas,sf)
  
}
