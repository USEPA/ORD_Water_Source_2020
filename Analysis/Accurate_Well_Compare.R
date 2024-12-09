library(tidyverse)
library(sf)
library(vroom)
library(here)

# We provide classifications for whether wells are domestic and whether their location is accurate
# Load the join codes for domestic wells
dc <- vroom(here("data/Well Logs/State_Domestic_Codes.csv"))%>%
  select(Well_Type,Category)

# Load location accuracy classifications
xy.cat <- read.csv(here("data/XY_Source_class.csv"))%>%
  mutate(state_xy = paste0(State,"_",XY_Source))%>%
  select(state_xy,Accurate)

# Load well data
wells <- vroom("data/Well Logs/Clean/All_State_Records.csv",
               col_types = c("Install_Date" = "c", "Dpth_Date" = "c"))%>%
  mutate(Install_Date = lubridate::parse_date_time(Install_Date, orders = c("ymd","mdy")),
         Dpth_Date = lubridate::parse_date_time(Dpth_Date, orders = c("ymd","mdy")),
         Well_Type = tolower(Well_Type),
         Install_Date = if_else(is.na(Install_Date)==TRUE,Dpth_Date,Install_Date),
         State_Type = paste0(State,"_",Well_Type),
         state_xy = paste0(State,"_",XY_Source),
         Well_Type = ifelse(State == "AR",paste0("AR_",Well_Type),Well_Type))%>%
  left_join(dc, by = c("State_Type"="Well_Type"))%>%
  left_join(xy.cat, by = c("state_xy"))%>%
  mutate(Accurate = ifelse(State == "AR",TRUE,Accurate))


# Filter to domestic wells with installation dates between 1990 and 2020
wells.d <- wells%>%
  filter(Category == "Domestic" | State == "NJ" | State == "WA")%>%
  filter(Install_Date > lubridate::mdy("12/31/2015") & Install_Date < lubridate::mdy("01/01/2020"))%>%
  #filter(State %in% c("AR","CA","KS","TX"))%>%
  select(State,Well_ID,Latitude,Longitude,Accurate,Install_Date)%>%
  group_by(Latitude,Longitude)%>%
  summarise(State = State[1],
            Well_ID = Well_ID[1],
            Accurate = ifelse(TRUE %in% Accurate,TRUE,FALSE),
            Install_Date = max(Install_Date))%>%
  ungroup()%>%
  distinct()

sf <- wells.d%>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4269)%>%
  st_transform(4326)

st_write(sf, "D:/temp/Wells.shp")

cws <- st_read("D:/Github/ORD_SAB_Model/Output_Data/Archive/Final_052024/Final_052024.gdb", layer = "Final")%>%
  filter(Symbology_Field == "STATE")%>%
  filter(substr(PWSID_12,1,2) %in% c("AR","AZ","CA","KS","MA","NH","NJ","PA"))%>%
  st_transform(4326)


intersections <- data.frame()
for(st in c("AR","AZ","CA","KS","MA","NH","NJ","PA")){
  print(paste0("Starting ",st," @ ",Sys.time()))
  sf.sub <- sf%>%
    filter(State == st)
  
  cws.sub <- cws%>%
    filter(substr(PWSID_12,1,2)==st)%>%
    st_make_valid()%>%
    summarise()%>%
    st_make_valid()
  
  tictoc::tic()
  intrsct <- st_intersection(sf.sub,cws.sub)
  tictoc::toc()
  
  intersections <- rbind(intersections,intrsct)
}


in.cws <- intersections%>%
  st_drop_geometry()%>%
  group_by(State)%>%
  summarise(WellsInCWS = n())

out.cws <- sf%>%
  st_drop_geometry()%>%
  filter(!Well_ID %in% intersections$Well_ID)%>%
  group_by(State)%>%
  summarise(nOut = n())


compare <- in.cws%>%
  left_join(out.cws)%>%
  mutate(Pct_In = 100*(WellsInCWS / (WellsInCWS+nOut)))

year <- wells.d%>%
  mutate(Year = lubridate::year(Install_Date))

ggplot(year)+
  geom_histogram(aes(x = Year))

# Filter to accurately located domestic wells
wells.a <- wells.d%>%
  filter(Accurate == TRUE)

# Save Accurate Wells
vroom_write(wells.a,"Analysis/Accurate_Wells.csv",delim = ",", append = FALSE)


# The first step is to join accurate wells to 2020 census blocks, then add the total number of drilled wells to the number reported in 1990
# Wells that do not successfully join to a block will be added back to the pool of uncertain wells
# Load 2020 Census Blocks
block.files <- data.frame(path = list.files("D:/data/nhgis/boundaries/blocks/2020/", full.names = TRUE, pattern = ".shp$"),
                          file = list.files("D:/data/nhgis/boundaries/blocks/2020/", full.names = FALSE, pattern = ".shp$"))%>%
  separate(file, into = "State", sep = "_", extra = "drop")%>%
  filter(State %in% wells.a$State)

for(n in 1:nrow(block.files)){
  # Load spatial file for blocks and join the cross walked data from script 01
  blks.st <- st_read(block.files$path[n])%>%
    select(GISJOIN)
  
  # Filter wells to the matching state of the census blocks
  st.wells <- wells.d%>%
    filter(State == block.files$State[n])%>%
    st_as_sf(coords = c("Longitude","Latitude"), crs = 4269)%>%
    st_transform(st_crs(blks.st))
  
  # Intersect accurate wells for the state with blocks
  blk.intrsct <- st_intersection(st.wells, blks.st)
  
  coords <- as.data.frame(st_coordinates(blk.intrsct))
  
  blk.df <- blk.intrsct%>%
    st_drop_geometry()%>%
    cbind(coords)
  
  # export dataset of accurate well IDs and their corresponding Census block
  vroom_write(blk.df, paste0("Analysis/Accurate_Well_Blocks/",block.files$State[n],"_Acc_Wells_blocks.csv"),delim = ",")
  
  print(paste0("Completed ",block.files$State[n]," --- ",Sys.time()))
  
}
