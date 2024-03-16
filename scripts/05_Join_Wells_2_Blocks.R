library(tidyverse)
library(sf)
library(vroom)
library(here)

# We provide classifications for whether wells are domestic and whether their location is accurate
# Load the join codes for domestic wells
dc <- vroom(here("data/State_Domestic_Codes.csv"))%>%
  select(Well_Type,Category)

# Load location accuracy classifications
xy.cat <- read.csv(here("data/XY_Source_class.csv"))%>%
  mutate(state_xy = paste0(State,"_",XY_Source))%>%
  select(state_xy,Accurate)

# Load well data
wells <- vroom("D:/Github/Groundwater_Depth/data/All_State_Records.csv",
               col_types = c("Install_Date" = "c", "Dpth_Date" = "c"))%>%
  mutate(Install_Date = lubridate::parse_date_time(Install_Date, orders = c("ymd","mdy")),
         Dpth_Date = lubridate::parse_date_time(Dpth_Date, orders = c("ymd","mdy")),
         Well_Type = tolower(Well_Type),
         Install_Date = if_else(is.na(Install_Date)==TRUE,Dpth_Date,Install_Date),
         State_Type = paste0(State,"_",Well_Type),
         state_xy = paste0(State,"_",XY_Source))%>%
  left_join(dc, by = c("State_Type"="Well_Type"))%>%
  left_join(xy.cat, by = c("state_xy"))


# Filter to domestic wells in our identified states with installation dates between 1990 and 2020
wells.d <- wells%>%
  filter(Category == "Domestic"&
           # State %in% c("AZ","CA","CO","DE","IA","IL",
           #              "KS","KY","LA","ME","MI","MN",
           #              "MO","MT","NE","NH","NV","OH",
           #              "OK","OR","PA","TN","VT","WI")&
           Install_Date > lubridate::mdy("12/31/1989") & Install_Date < lubridate::mdy("01/01/2021"))


# Create two data sets
## Data set 1: Accurate wells to be joined to blocks
## Data set 2: Accuracy unknown wells to be joined to block groups

accurate <- wells.d%>%
  filter(Accurate==TRUE)
uncertain <- wells.d%>%
  filter(Accurate == FALSE)

# The first step is to join accurate wells to 2020 census blocks, then add the total number of drilled wells to the number reported in 1990
# Wells that do not successfully join to a block will be added back to the pool of uncertain wells
# Load 2020 Census Blocks
block.files <- data.frame(path = list.files("D:/data/nhgis/boundaries/blocks/2020/", full.names = TRUE, pattern = ".shp$"),
                         file = list.files("D:/data/nhgis/boundaries/blocks/2020/", full.names = FALSE, pattern = ".shp$"))%>%
  separate(file, into = "State", sep = "_", extra = "drop")%>%
  filter(State %in% accurate$State)

block.data <- vroom(here("Water_Use/outputs/data/Blocks_2020.csv"))

blk.join <- data.frame()
wells.nojoy <- data.frame()
for(n in 1:nrow(block.files)){
  # Load spatial file for blocks and join the cross walked data from script 01
  blks.st <- st_read(block.files$path[n])%>%
    select(GISJOIN)%>%
    left_join(block.data)%>%
    filter(HU_2020 > 0)
  
  # Filter accurate wells to the matching state of the census blocks
  st.a.wells <- accurate%>%
    filter(State == block.files$State[n])%>%
    st_as_sf(coords = c("Longitude","Latitude"), crs = 4269)%>%
    st_transform(st_crs(blks.st))
  
  # Intersect accurate wells for the state with blocks
  blk.a.intrsct <- st_intersection(st.a.wells, blks.st)
  
  # export dataset of accurate well IDs and their corresponding Census block
  acc.sjoin <- blk.a.intrsct%>%
    st_drop_geometry()
  vroom_write(acc.sjoin, paste0("Water_use/outputs/data/accurate_wells_by_block/",block.files$State[n],"_accWells_blocks.csv"),delim = ",")
  
  # Wells that did not join
  blk.a.miss <- st.a.wells%>%
    st_drop_geometry()%>%
    filter(!Well_ID %in% blk.a.intrsct$Well_ID)
  
  wells.nojoy <- rbind(wells.nojoy,blk.a.miss)
  print(paste0(nrow(blk.a.miss)," wells did not overlap a bloack in ",block.files$State[n]))
  
  # Count how many wells were drilled in each block
  b.drill.count <-  as.data.frame(table(blk.a.intrsct$GISJOIN, dnn = list("GISJOIN")), responseName = "AccDrillCount")
  
  # join the count of accurate drilled wells to the state census blocks and output for use with next step
  blk.update <- blks.st%>%
    st_drop_geometry()%>%
    left_join(b.drill.count)%>%
    mutate(AccDrillCount = replace_na(AccDrillCount,0))
  
  blk.join <- rbind(blk.join,blk.update)
  print(paste0("Completed ",block.files$State[n]," --- ",Sys.time()))
  
}

# The second step is to join the uncertain wells to block groups and cascade them into blocks, using the updated estimate as a limiter.

# Add wells that did not join to a block into the pool of uncertain wells
u.wells <- accurate%>%
  filter(Well_ID %in% wells.nojoy$Well_ID)%>%
  rbind(uncertain)

# load state abbreviations
fips <- select(tidycensus::fips_codes,state,state_code,state_name)%>%
  distinct()

# Load block groups (spatial) and add state abbreviations
bg.sf <- st_read("D:/data/nhgis/boundaries/Block_Groups/US_blck_grp_2020.shp")%>%
  left_join(fips, by=c("STATEFP"="state_code"))%>%
  filter(state %in% uncertain$State | state_name %in% blk.join$STATE)%>%
  select(GISJOIN,state,state_name)

# Load 2020 block data
blk.data.2020 <- vroom(here("Water_Use/outputs/data/Blocks_2020.csv"))%>%
  left_join(fips, by = c("STATE" = "state_name"))

# Select columns for accurate wells to be joined
blk.acc.sel <- blk.join%>%
  select(GISJOIN,AccDrillCount)

for(st in unique(bg.sf$state)){
  print(paste0("Beginning ",st," --- ",Sys.time()))
  # Filter to the uncertain wells for the state
  u.wells.st <- u.wells%>%
    filter(State == st)%>%
    st_as_sf(coords = c("Longitude","Latitude"), crs = 4269)%>%
    st_transform(st_crs(bg.sf))
  
  # Filter block groups to current state
  st.bg.20 <- bg.sf%>%
    filter(state == st)
  
  # Intersect wells with block groups
  bg.intrsct <- st_intersection(u.wells.st,st.bg.20)
  
  # Count how many wells were drilled in each block
  bg.drill.count <-  as.data.frame(table(bg.intrsct$GISJOIN, dnn = list("GISJOIN")), responseName = "UncDrillCount")
  
  # Filter block data for state, create block group join and join block group data to prepare for cascade
  st.blks.20 <- blk.data.2020%>%
    filter(state == st)%>%
    mutate(GISJOIN_BG = substr(GISJOIN,1,15))%>%
    left_join(bg.drill.count, by = c("GISJOIN_BG"="GISJOIN"))%>%
    mutate(UncDrillCount = replace_na(UncDrillCount,0),
           HU_Km_B = HU_2020/(AREALAND/1000000))
    left_join(blk.acc.sel, by = "GISJOIN")%>%
    mutate(AccDrillCount = replace_na(AccDrillCount,0),
           HU_Km_B = HU_2020/(AREALAND/1000000))
  
  # Calculate weighted estimates of public water users
  ## Percent of housing unit density within block group multiplied by the total public users
  # Recalculate BG housing units and BG Public water based on aggregate of block Housing Units
  weight <- st.blks.20%>%
    filter(HU_2020 > 0)%>%
    group_by(GISJOIN_BG)%>%
    mutate(HU_BG = sum(HU_2020, na.rm = TRUE),
           Wells_90_BG = (sum(HU_1990,na.rm=TRUE) - sum(Pub_WS_1990,na.rm=TRUE)),
           Wells_Added = UncDrillCount + sum(AccDrillCount,na.rm=TRUE),
           pctWell_90 = (sum(HU_1990,na.rm=TRUE) - sum(Pub_WS_1990,na.rm=TRUE))/sum(HU_1990,na.rm=TRUE),
           hu_loss = sum(HU_1990,na.rm=TRUE)-sum(HU_2020,na.rm = TRUE),
           hu_loss = ifelse(hu_loss <0,0,hu_loss),
           HU_Avail = HU_2020 - AccDrillCount,
           Wells_Est_BG = round(Wells_90_BG + Wells_Added - (pctWell_90 * hu_loss)),
           Well_Dnsty_Weight = (HU_Km_B / sum(HU_Km_B))*Wells_Est_BG)%>%
    ungroup()%>%
    select(GISJOIN,HU_1990,Pub_WS_1990,HU_2020,HU_Km_B,AccDrillCount,GISJOIN_BG,Wells_90_BG,HU_BG,HU_Avail,
           Wells_Est_BG,Well_Dnsty_Weight)
  
  # Cascade surplus estimates into the next most dense block
  # Create template to write to
  weighted.out <- weight%>%
    replace(!is.na(.),NA)
  
  # Order blocks by housing unit density and allocate Public water users
  # To keep accurate wells in the blocks they belong, we substract them from housing units before allocating, then add them back after the cascade
  i <- 0
  rowCount <- 0
  nBGs <- length(unique(weight$GISJOIN_BG))
  pb <- txtProgressBar(min = 0, max = length(unique(weight$GISJOIN_BG)), style = 3)
  for (bg in unique(weight$GISJOIN_BG)){
    order <- weight%>%
      filter(GISJOIN_BG == bg)%>%
      arrange(HU_Km_B)%>%
      mutate(Wells_Est_BG = replace_na(Wells_Est_BG,0),
             Well_Dnsty_Weight = replace_na(Well_Dnsty_Weight,0))
    nrows <- nrow(order)
    if(nrows>1 & sum(order$Well_Dnsty_Weight>0)){
      # Public water users to be allocated
      pwu <- order$Wells_Est_BG[1]
      for(n in 1:(nrow(order))){
        # Add up to the number of housing units in a block
        if(pwu > order$HU_Avail[n]){
          order$Well_Dnsty_Weight[n] <- order$HU_Avail[n]
        } else(order$Well_Dnsty_Weight[n] <- pwu)
        pwu <- pwu - order$Well_Dnsty_Weight[n]
      }
    }
    # Overwrite with new data
    rowStart <- rowCount+1
    rowEnd <- rowCount+nrows
    weighted.out[rowStart:rowEnd,] <- order
    rowCount <- rowCount + nrows
    i <- i+1
    setTxtProgressBar(pb,i)
  }
  
  wells.acc.add <- weighted.out%>%
    mutate(Est_Wells_2020 = Well_Dnsty_Weight + AccDrillCount)
  
  vroom_write(wells.acc.add,paste0("Water_Use/Outputs/data/Drilled_Wells_Allocated/",st,"_2020_blocks.csv"))
}








