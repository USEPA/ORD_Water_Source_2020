library(dplyr)
library(vroom)

blocks <- vroom("outputs/Well_Estimates_2020_Blocks.csv")

blockGroups <- blocks%>%
  group_by(GEOID_BlockGroup)%>% # group by block group
  summarise(Population = sum(Population,na.rm=TRUE), # Sum population of the block group
            HousingUnits = sum(Housing_Units,na.rm=TRUE), # Sum housing units of the block group
            Wells = sum(Est_Wells_2020,na.rm=TRUE))%>% # Sum wells of the block group
  mutate(Pop_HU = Population/HousingUnits, # Calculate population per housing unit
         Pop_Wells = Wells * Pop_HU, # Calculate population using wells
         Pct_Wells = Pop_Wells/Population) # Calculate percentage of population using wells
