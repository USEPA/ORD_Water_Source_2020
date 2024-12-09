library(vroom)
library(tidyverse)

est <- vroom("outputs/Well_Estimates_2020_Blks.csv")

df <- vroom("D:/data/nhgis/tables/Blocks/nhgis0307_ds248_2020_block.csv")%>%
  mutate(GEOID_S = substr(GEOID,10,nchar(GEOID)))%>%
  select(GEOID,GEOID_S,U7B001,U7G001)%>%
  left_join(est, by = c("GEOID_S"="GEOID_Blk"))


check <- df%>%
  filter(U7B001 >0 & U7G001 >0 & is.na(State))

check2 <- check%>%
  filter(is.na(State))
