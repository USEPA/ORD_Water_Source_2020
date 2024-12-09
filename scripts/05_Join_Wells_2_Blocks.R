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
  filter(Category == "Domestic"&
           # State %in% c("AZ","CA","CO","DE","IA","IL",
           #              "KS","KY","LA","ME","MI","MN",
           #              "MO","MT","NE","NH","NV","OH",
           #              "OK","OR","PA","TN","VT","WI")&
           Install_Date > lubridate::mdy("12/31/1989") & Install_Date < lubridate::mdy("01/01/2021"))


# The first step is to join accurate wells to 2020 census blocks, then add the total number of drilled wells to the number reported in 1990
# Wells that do not successfully join to a block will be added back to the pool of uncertain wells
# Load 2020 Census Blocks
block.files <- data.frame(path = list.files("D:/data/nhgis/boundaries/blocks/2020/", full.names = TRUE, pattern = ".shp$"),
                         file = list.files("D:/data/nhgis/boundaries/blocks/2020/", full.names = FALSE, pattern = ".shp$"))%>%
  separate(file, into = "State", sep = "_", extra = "drop")%>%
  filter(State %in% wells.d$State)

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
  blk.intrsct <- st_intersection(st.wells, blks.st)%>%
    st_drop_geometry()
  
  # export dataset of accurate well IDs and their corresponding Census block
  vroom_write(blk.intrsct, paste0("data/Well Logs/Census_Blocks/",block.files$State[n],"_Domestic_Wells_blocks.csv"),delim = ",")
  
  print(paste0("Completed ",block.files$State[n]," --- ",Sys.time()))
  
}
