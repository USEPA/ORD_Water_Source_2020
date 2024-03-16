library(dplyr)
library(tidyr)
library(sf)
library(doParallel)
library(foreach)
library(vroom)
library(here)

# This script removes islands, which we define as blocks that are public,
# completely surrounded by private or blocks that are private, completely surrounded by public.
# This seems to happen most in urban areas where you may have a park with one or two housing units on the edges.

# Create Cluster
cores <- detectCores()-1
cl <- makeCluster(cores)
registerDoParallel(cl)

print(paste0("Cluster created with ",cores," cores ... Beginning to Reclassify ...",Sys.time()))

# Set initial count of islands found
nIslands <- 0

# List state datasets from decision tree
files <- data.frame(file = list.files("Water_Use/outputs/DT_Output_shp/", pattern = ".shp$"),
                    path = list.files("Water_Use/outputs/DT_Output_shp/", pattern = ".shp$", full.names = TRUE))%>%
  mutate(state = substr(file,1,2))%>%
  filter(state %in% c("AZ","AR"))

us.counties <- st_read("D:/data/Census/cb_2021_us_all_500k.gdb",layer = "cb_2021_us_county_500k")




# Iterate through states
for(i in 1:nrow(files)){
  print(paste0("Starting ",files$state[i]," --- ",Sys.time()))
  sf <- st_read(files$path[i])%>%
    mutate(HU_2020 = replace_na(HU_2020,0),
           Pred_Class = ifelse(HU_2020 == 0,NA,Pred_Class),
           ST_CNTY = paste0(substr(GISJOIN,2,3),substr(GISJOIN,5,7)))%>%
    st_transform(4269)%>%
    st_make_valid()
  
  st.counties <- us.counties%>%
    filter(STUSPS == files$state[i])
  
  # Iterate through counties
  pb <- txtProgressBar(min = 0, max = nrow(st.counties), style = 3)
  for(j in 1:nrow(st.counties)){
    #print(paste0("Starting ",st.counties$NAMELSAD[j]," --- ",Sys.time()))
    cnty.blks <- sf%>%
      filter(ST_CNTY == st.counties$GEOID[j])
    #print(paste0("(",nrow(cnty.blks)," blocks)"))
    
    # filter potential islands to blocks that have been classified already
    islands <- cnty.blks%>%
      mutate(wasIsland = NA)%>%
      filter(!is.na(Pred_Class))
    
    islands.out <- foreach(n = 1:nrow(islands),
                           .combine=rbind,
                           .packages = c("tidyverse","sf")) %dopar%{
                             
                             block <- islands[n,]
                             
                             # Find neighbors
                             neighbors <- islands%>%
                               filter(st_touches(.,block, sparse = FALSE))
                             
                             # Are nighbors all of one class?
                             nClasses <- length(unique(neighbors$Pred_Class))
                             
                             # If all neighbors are of one class, check to see if it matches the block
                             if(nClasses == 1){
                               match <- block$Pred_Class == neighbors$Pred_Class[1]
                               
                               # If the neighbors are of the opposite class, flip the block class and flag the block as an island
                               
                               if(match == FALSE){
                                 block$Pred_Class <- neighbors$Pred_Class[1]
                                 block$wasIsland <- TRUE
                                 
                                 nIslands <- nIslands+1
                                 print(paste0("Reclassified ",nIslands," islands..."))
                               }else(block$wasIsland <- FALSE)
                               
                             }else(block$wasIsland <- FALSE)
                             return(block)
                           }
    #print(paste0("Completed Reclassification ... Merging... ",Sys.time()))
    # Combine reclassified data with NA data
    islands.reclass <- cnty.blks%>%
      mutate(wasIsland = NA)%>%
      filter(is.na(Pred_Class))%>%
      rbind(islands.out)
    
    #print(paste0("Saving CSV... ",Sys.time()))
    # Save output csv
    df <- islands.reclass%>%
      st_drop_geometry()
    vroom_write(df,paste0("Water_Use/outputs/data/reclassify_islands/",
                          files$state[i],"_",st.counties$COUNTYFP[j],"_Islands_Reclass.csv"), delim = ",")
    
    
    setTxtProgressBar(pb,j)
    
  }
}

check.files <- list.files("Water_Use/outputs/data/reclassify_islands/",pattern = "NE_", full.names = TRUE)
check <- vroom(check.files)%>%
  distinct()
length(unique(check$GISJOIN))

dups <- check%>%
  group_by(GISJOIN)%>%
  summarise(nRows = n())%>%
  filter(nRows>1)

check2 <- check%>%
  filter(GISJOIN=="G31000109662002013")

print(paste0("Script Complete... ",Sys.time()))