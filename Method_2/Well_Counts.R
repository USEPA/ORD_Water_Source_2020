library(tidyverse)
library(sf)
library(vroom)
library(plotly)

# 1990 counts in 2020 blocks
df.90 <- vroom("D:/Github/ORD_Water_Supply/Water_Use/outputs/data/Blocks_2020.csv")


# Test for North Carolina

# Look at Percent well use in 1990 vs. 2020

nc <- df.90%>%
  filter(STATE == "North Carolina")%>%
  mutate(HU_1990 = replace_na(HU_1990,0),
         Public_S_90 = replace_na(Public_S_90,0),
         Wells_90 = HU_1990 - Public_S_90,
         Pct_90 = 1-(Public_S_90-HU_1990),
         Pct_20 = Wells_90/HU_2020,
         Pct_Dif = Pct_90 - Pct_20)

ggplot(nc)+
  geom_point(aes(x = Pct_90, y = Pct_20))

plot_ly(nc)%>%
  add_markers(x = ~Pct_90, y = ~Pct_20)

# Save spatial file to look at

sf <- st_read("D:/data/nhgis/boundaries/Blocks/2020/NC_block_2020.shp")%>%
  select(GISJOIN)%>%
  left_join(nc)%>%
  filter(HU_2020 > 0)

st_write(sf, "D:/temp/NC_Blocks.shp", append = FALSE)
