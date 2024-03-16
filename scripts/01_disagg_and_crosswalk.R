library(tidyverse)
library(vroom)
library(sf)
library(here)

# We allocate the number of public water users to the most dense blocks
# The inverse is the number of self-suppliers but we also need to track the
# Number of wells vs. self - suppliers. Perhaps also drilled vs dug wells.

# Load 100% Housing unit data for block groups
hu.100 <- vroom("D:/data/nhgis/tables/Block_Groups/nhgis0293_ds120_1990_blck_grp.csv")%>%
  select(GISJOIN,ESA001)

# Import data for 1990 Census Block Groups
bg.90 <- vroom(here("data/Census_Tables/US_Blk_Grps_1990.csv"))%>%
  left_join(hu.100,by = c("GISJOIN_BG"="GISJOIN"))%>%
  mutate(Pct_Wells_S = (Drill_WS+Dug_WS)/(Public_WS+Dug_WS+Drill_WS+Other_WS),
         Pct_Public_S = (Public_WS)/(Public_WS+Dug_WS+Drill_WS+Other_WS),
         Pct_Other_S = (Other_WS)/(Public_WS+Dug_WS+Drill_WS+Other_WS),
         Pct_Sewer_D = Public_WD/(Public_WD+Septic_WD+Other_WD),
         Pct_Septic_D = Septic_WD/(Public_WD+Septic_WD+Other_WD),
         Pct_Other_D = Other_WD/(Public_WD+Septic_WD+Other_WD))%>%
  select(GISJOIN_BG,Area_Km,
         Pct_Wells_S,Pct_Public_S,Pct_Other_S,
         Pct_Sewer_D,Pct_Septic_D,Pct_Other_D)%>%
  filter(!is.na(Pct_Public_S))

# Load data for 1990 blocks
blks.90 <- vroom(here("data/Census_Tables/US_Blks_1990.csv"))

# check <- blks.90%>%
#   filter(!GISJOIN_BG %in% bg.90$GISJOIN_BG)


# Calculate weighted estimates of public water users
## Percent of housing unit density within block group multiplied by the total public users
# Recalculate BG housing units and BG Public water based on aggregate of block Housing Units
## Repeat for septic
## We need to make sure the estimated wells+public+other is equal to housing units.
weight <- blks.90%>%
  left_join(bg.90, by = "GISJOIN_BG")%>%
  group_by(GISJOIN_BG)%>%
  mutate(HU_BG = sum(HU_B, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(Public_S_BG = round(HU_BG * Pct_Public_S),
         Sewer_D_BG = round(HU_BG * Pct_Sewer_D))%>%
  select(GISJOIN_B,HU_B,HU_Km_B,Area_Km_B,
         GISJOIN_BG,HU_BG,
         Pct_Wells_S,Pct_Public_S,Pct_Other_S,
         Pct_Sewer_D,Pct_Septic_D,Pct_Other_D,
         Public_S_BG,Sewer_D_BG)%>%
  mutate(Public_S_B = 0)%>%
  filter(!is.na(Pct_Public_S))

vroom_write(weight,"Water_Use/outputs/data/Blocks_1990_SOW_HU.csv", delim = ",")
#weight <- vroom("Water_Use/outputs/data/Blocks_1990_SOW_HU.csv")
# Cascade surplus estimates into the next most dense block

# Order blocks by housing unit density and allocate Public water users
## MAKE PARALLEL
library(doParallel)
library(foreach)

# Create Cluster
cores <- detectCores()-1
cl <- makeCluster(cores)
registerDoParallel(cl)

# Sample for testing
# s <- sample(seq(1,length(unique(weight$GISJOIN_BG))),10)
# s.bg <- unique(weight$GISJOIN_BG)[s]
# weight <- weight%>%
#   filter(GISJOIN_BG %in% s.bg)

start <- Sys.time()

weight.blks <- foreach(bg = unique(weight$GISJOIN_BG),
        .combine=rbind,
        .packages = c("tidyverse")) %dopar%{
          order <- weight%>%
            filter(GISJOIN_BG == bg)%>%
            arrange(desc(HU_Km_B))
          
          nrows <- nrow(order)
          
          if(order$Pct_Public_S[1] == 1){
            order$Public_S_B <- order$HU_B
          }else({
            pwu <- order$Public_S_BG[1]
            
            for(n in 1:(nrow(order))){
              # Add up to the number of housing units in a block
              if(pwu > order$HU_B[n]){
                order$Public_S_B[n] <- order$HU_B[n]
              } else(order$Public_S_B[n] <- pwu)
              pwu <- pwu - order$Public_S_B[n]
            }
            
          })
          order
        }

end <- Sys.time()
vroom_write(weight.blks,"Water_Use/outputs/data/blocks_90_weight.csv", delim = ",")

weight.blks <- vroom("Water_Use/outputs/data/blocks_90_weight.csv")
# Order blocks by housing unit density and allocate Public sewer users
## NOTE: I ran these separately in the event there are blocks with sewer users and no water users or vice versa
weight.sewer <- blks.90%>%
  left_join(bg.90, by = "GISJOIN_BG")%>%
  group_by(GISJOIN_BG)%>%
  mutate(HU_BG = sum(HU_B, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(Public_S_BG = round(HU_BG * Pct_Public_S),
         Sewer_D_BG = round(HU_BG * Pct_Sewer_D))%>%
  select(GISJOIN_B,HU_B,HU_Km_B,Area_Km_B,
         GISJOIN_BG,HU_BG,
         Pct_Wells_S,Pct_Public_S,Pct_Other_S,
         Pct_Sewer_D,Pct_Septic_D,Pct_Other_D,
         Public_S_BG,Sewer_D_BG)%>%
  mutate(Sewer_D_B = 0)%>%
  filter(!is.na(Pct_Sewer_D))

start <- Sys.time()

weight.sewer.blks <- foreach(bg = unique(weight.sewer$GISJOIN_BG),
                       .combine=rbind,
                       .packages = c("tidyverse")) %dopar%{
                         order <- weight.sewer%>%
                           filter(GISJOIN_BG == bg)%>%
                           arrange(desc(HU_Km_B))
                         
                         nrows <- nrow(order)
                         
                         if(order$Pct_Sewer_D[1] == 1){
                           order$Sewer_D_B <- order$HU_B
                         }else({
                           psu <- order$Sewer_D_BG[1]
                           
                           for(n in 1:(nrow(order))){
                             # Add up to the number of housing units in a block
                             if(psu > order$HU_B[n]){
                               order$Sewer_D_B[n] <- order$HU_B[n]
                             } else(order$Sewer_D_B[n] <- psu)
                             psu <- psu - order$Sewer_D_B[n]
                           }
                           
                         })
                         order
                       }

end <- Sys.time()



sewer.sel <- weight.sewer.blks%>%
  select(GISJOIN_B,Sewer_D_B)


vroom_write(sewer.sel,"Water_Use/outputs/data/blocks_90_weight_sewer.csv", delim = ",")
sewer.sel <- vroom("Water_Use/outputs/data/blocks_90_weight_sewer.csv")

cascade <- weight.blks%>%
  left_join(sewer.sel)

# Crosswalk from 1990 to 2010

# Calculate % non-public sources/disposal that are wells/septic compared to 'other'
pct.other <- vroom(here("data/Census_Tables/US_Blk_Grps_1990.csv"))%>%
  mutate(Pct_Wells_Other_S = (Drill_WS+Dug_WS)/(Drill_WS+Dug_WS+Other_WS),
         Pct_Septic_Other_D = Septic_WD/(Septic_WD+Other_WD))%>%
  select(GISJOIN_BG,Pct_Wells_Other_S,Pct_Septic_Other_D)

cw.90.10 <- vroom("D:/data/nhgis/crosswalks/Blks_2_Blks/nhgis_blk1990_blk2010_gj.csv")

data.90 <- cascade%>%
  left_join(cw.90.10, by = c("GISJOIN_B" = "GJOIN1990"))%>%
  left_join(pct.other)%>%
  mutate(HU_1990 = HU_B * WEIGHT,
         Public_S_90 = Public_S_B * WEIGHT,
         Sewer_D_90 = Sewer_D_B * WEIGHT)%>%
  select(GJOIN2010,HU_1990,Public_S_90,Sewer_D_90,
         Pct_Wells_Other_S,Pct_Septic_Other_D)%>%
  group_by(GJOIN2010)%>%
  mutate(HU_1990 = sum(HU_1990, na.rm = TRUE),
         Public_S_90 = sum(Public_S_90, na.rm = TRUE),
         Sewer_D_90 = sum(Sewer_D_90, na.rm = TRUE),
         Pct_Wells_Other_S = mean(Pct_Wells_Other_S,na.rm = TRUE),
         Pct_Septic_Other_D = mean(Pct_Septic_Other_D,na.rm = TRUE))%>%
  ungroup()%>%
  distinct()

# Crosswalk 2000 housing units to 2010
cw.00.10 <- vroom("D:/data/nhgis/crosswalks/Blks_2_Blks/nhgis_blk2000_blk2010_gj.csv")

hu.2000 <- vroom("D:/data/NHGIS/tables/blocks/nhgis0321_ds147_2000_block.csv")%>%
  select(GISJOIN,FV5001)%>%
  left_join(cw.00.10, by = c("GISJOIN" = "GJOIN2000"))%>%
  mutate(HU_2000 = FV5001 * WEIGHT)%>%
  select(GJOIN2010,HU_2000)%>%
  group_by(GJOIN2010)%>%
  mutate(HU_2000 = sum(HU_2000, na.rm = TRUE))%>%
  ungroup()%>%
  select(GJOIN2010,HU_2000)%>%
  distinct()


# Join 1990 & 2000 crosswalked data to 2010 data
hu.2010 <- vroom("D:/data/NHGIS/tables/blocks/nhgis0321_ds172_2010_block.csv")%>%
  select(GISJOIN,IFC001)%>%
  left_join(data.90, by = c("GISJOIN" = "GJOIN2010"))%>%
  left_join(hu.2000, by = c("GISJOIN" = "GJOIN2010"))
colnames(hu.2010)[2] <- "HU_2010"


# Crosswalk everything to 2020
cw.10.20 <- vroom("D:/data/nhgis/crosswalks/Blks_2_Blks/nhgis_blk2010_blk2020_gj.csv")

df.2020 <- vroom("D:/data/nhgis/tables/Blocks/nhgis0307_ds248_2020_block.csv")%>%
  select(GISJOIN,YEAR,STATE,COUNTY,AREALAND,AREAWATR,U7B001,U7G001)%>%
  left_join(cw.10.20, by = c("GISJOIN" = "GJOIN2020"))%>%
  left_join(hu.2010, by = c("GJOIN2010" = "GISJOIN"))%>%
  mutate(Public_S_90 = Public_S_90 * WEIGHT,
         Sewer_D_90 = Sewer_D_90 * WEIGHT,
         HU_1990 = HU_1990 * WEIGHT,
         HU_2000 = HU_2000 * WEIGHT,
         HU_2010 = HU_2010 * WEIGHT)%>%
  group_by(GISJOIN)%>%
  mutate(Public_S_90 = sum(Public_S_90,na.rm = TRUE),
         Sewer_D_90 = sum(Sewer_D_90,na.rm = TRUE),
         HU_1990 = sum(HU_1990, na.rm = TRUE),
         HU_2000 = sum(HU_2000, na.rm = TRUE),
         HU_2010 = sum(HU_2010, na.rm = TRUE),
         Pct_Wells_Other_S = mean(Pct_Wells_Other_S,na.rm = TRUE),
         Pct_Septic_Other_D = mean(Pct_Septic_Other_D,na.rm = TRUE))%>%
  select(GISJOIN,YEAR,STATE,COUNTY,
         HU_1990,HU_2000,HU_2010,U7G001,U7B001,AREALAND,AREAWATR,
         Public_S_90,Sewer_D_90,Pct_Wells_Other_S,Pct_Septic_Other_D)%>%
  distinct()

colnames(df.2020)[8:9] <- c("HU_2020","Pop_2020")

# Save file
vroom_write(df.2020,here("Water_Use/outputs/data/Blocks_2020.csv"), delim = ",")

df.2020 <- vroom(here("Water_Use/outputs/data/Blocks_2020.csv"))

check3 <- df.2020%>%
  filter(GISJOIN == "G36008904907004079")
