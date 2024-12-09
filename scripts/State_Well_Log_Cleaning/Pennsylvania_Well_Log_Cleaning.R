library(tidyverse)
library(vroom)
library(here)
library(sf)

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
                       Longtude = double(),
                       XY_Source = character())

pa <- vroom(here("data/RawLogs/Pennsylvania_12_22_2021.csv"))%>%
  mutate(State = "Pennsylvania",
         Well_ID = paste0("PA_",PAWellID),
         Dpth_to_Water = `StaticWaterLevel(ft)`,
         Dpth_Unit = "feet",
         Dpth_Date = DateDrilled,
         Pump_Rate = `WellYield(gpm)`,
         Pump_Rate_Unit = "GPM",
         Dpth_to_Bedrock = `DepthToBedrock(ft)`,
         Dpth_to_Bedrock_Unit = "feet",
         Install_Date = DateDrilled,
         Well_Type = WaterUse,
         Well_Depth = `WellDepth(ft)`,
         Well_Depth_Unit = "feet",
         Latitude = LatitudeDD,
         Longitude = LongitudeDD,
         XY_Source = NA)%>%
  select(State, Well_ID,Dpth_to_Water,Dpth_Unit,
         Dpth_Date,Pump_Rate,Pump_Rate_Unit,Dpth_to_Bedrock,
         Dpth_to_Bedrock_Unit,Install_Date,Well_Type,
         Well_Depth,Well_Depth_Unit,Latitude,Longitude,
         XY_Source)

vroom_write(pa, here("data/Clean_Well_Logs/Pennsylvania_clean.tsv"), delim = "\t")

# Map Wells with Depth measurements
paFilt <- pa%>%
  filter(Dpth_to_Water>0 & Dpth_to_Water < 300 &
           Longitude > -80 & Longitude < -74 &
           Latitude > 38 & Latitude < 43)%>%
  drop_na(Longitude,Latitude)%>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)


ggplot(paFilt)+
  geom_sf(aes(color = Dpth_to_Water))


# There is in issue in Pennsylvanis where:
  # A: Many records have well depth == to static water level (This is probably to fill in NA data)
  # B: Some wells show a water level that is higher after a pumping test which is suspicious.
ggplot()+
  geom_point(aes(x = paRaw$`StaticWaterLevel(ft)`,
                 y = paRaw$`WaterLevelAfterYieldTest(ft)`),
             alpha = .2)+
  xlim(0,250)+
  ylim(0,250)+
  labs(title = "Depth to Water [feet]",
       x = "Static Water Level",
       y = "Water Level After Test")

ggplot()+
  geom_histogram(aes(x = paRaw$`StaticWaterLevel(ft)`))+
  xlim(0,250)

ggplot()+
  geom_histogram(aes(x = paRaw$`WaterLevelAfterYieldTest(ft)`))+
  xlim(0,250)
