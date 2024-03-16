library(tidyverse)
library(sf)
library(doParallel)
library(foreach)
library(vroom)

# Skip files that have been run already
files.done <- data.frame(file = list.files("Water_Use/outputs/data/reclassify_na/"))%>%
  mutate(ST_CO = substr(file,1,6))

# List files from island reclassification
files.all <- data.frame(name = list.files("Water_Use/outputs/data/reclassify_islands/"),
                    path = list.files("Water_Use/outputs/data/reclassify_islands/", full.names = TRUE))%>%
  mutate(state = substr(name,1,2),
         ST_CO = substr(name,1,6))%>%
  filter(!ST_CO %in% files.done$ST_CO)

fl.files <- files.all%>%
  filter(state == "FL")

fl <- vroom(fl.files$path)

fl.temp <- fl%>%
  filter(GISJOIN == "G12004309900000002")


# List spatial files
sp.files <- data.frame(file = list.files("Water_Use/outputs/DT_Output_shp", pattern = ".shp$"),
                       path = list.files("Water_Use/outputs/DT_Output_shp", pattern = ".shp$", full.names = TRUE))%>%
  mutate(state = substr(file,1,2))%>%
  filter(state %in% c("AK","HI"))


# Create Cluster
cores <- detectCores()-1
cl <- makeCluster(cores)
registerDoParallel(cl)

print(paste0("Cluster created with ",cores," cores ... Beginning to Reclassify ...",Sys.time()))

state.list <- data.frame()

# Loop through counties
## replace classifications with the island remove results
for(st in unique(files.all$state)){
  files <- files.all%>%
    filter(state == st)
  for(i in 1:nrow(files)){
    df <- vroom(files$path[i])%>%
      distinct()%>%
      select(GISJOIN,Pred_Class,wasIsland)
    
    sf.file <- sp.files%>%
      filter(state == files$state[i])
    
    # Write selected file to a list
    state.list <- rbind(state.list,data.frame(file = sf.file$state))
    
    # If the state is the same, don't read in new file (time saving)
    
    if(i ==1){
      sf.state <- st_read(sf.file$path)
    }
    
    if(i>1){
      if(!state.list$file[nrow(state.list)]==state.list$file[(nrow(state.list)-1)]){
        sf.state <- st_read(sf.file$path)
      }
    }
    
    islands.reclass <- sf.state%>%
      filter(GISJOIN %in% df$GISJOIN)%>%
      select(!Pred_Class)%>%
      left_join(df)%>%
      mutate(TouchesClass = NA,
             PctPubAdjct = NA)
    
    reclass <- islands.reclass%>%
      filter(is.na(Pred_Class))
    
    if(nrow(reclass)>0){
      # Dummy values for start and finish number of blocks that need to be reclassified
      n.reclass <- 2
      n.still.na <- 1
      # Run a while loop that runs until no more blocks are reclassified
      n.loops <- 0
      while (!n.reclass == n.still.na) {
        na.touch <- reclass%>%
          filter(is.na(TouchesClass))
        n.reclass <- nrow(na.touch)
        
        if(n.reclass>0){
          print(paste0("Starting Loop: ",n.loops+1," @ ",Sys.time()," ",n.reclass, " remaining to be classified"))
          
          reclass.out <- foreach(n = 1:nrow(reclass),
                                 .combine=rbind,
                                 .packages = c("tidyverse","sf")) %dopar%{
                                   # Subset a single block
                                   block <- reclass[n,]
                                   
                                   # In the first loop we check for touches against classified blocks
                                   if(is.na(block$TouchesClass) & n.loops == 0){
                                     # Find the blocks it touches that have been classified
                                     blks.touch <- islands.reclass%>%
                                       filter(st_touches(.,block,sparse = FALSE))%>%
                                       filter(!is.na(Pred_Class))
                                     
                                     # If there are blocks that touch, write TRUE to the reclass data and calculate % public
                                     if(nrow(blks.touch)>0){
                                       # Touches = TRUE
                                       block$TouchesClass <- TRUE
                                       
                                       # Count of Adjacent Public
                                       adj.pub <- blks.touch%>%
                                         filter(Pred_Class == "Public")
                                       
                                       # Calculate percent of adjacent blocks that are public
                                       block$PctPubAdjct <- round(100*(nrow(adj.pub)/nrow(blks.touch)),1)
                                       
                                       # Reclassify based on percent public
                                       block$Pred_Class <- ifelse(block$PctPubAdjct>=50,"Public","Private")
                                     }
                                   }
                                   
                                   # In loops after the first loop we check for touches against reclass blocks
                                   if(is.na(block$TouchesClass) & n.loops > 0){
                                     # Find the blocks it touches that have been classified
                                     blks.touch <- reclass%>%
                                       filter(st_touches(.,block,sparse = FALSE))%>%
                                       filter(!is.na(Pred_Class))
                                     
                                     # If there are blocks that touch, write TRUE to the reclass data and calculate % public
                                     if(nrow(blks.touch)>0){
                                       # Touches = TRUE
                                       block$TouchesClass <- TRUE
                                       
                                       # Count of Adjacent Public
                                       adj.pub <- blks.touch%>%
                                         filter(Pred_Class == "Public")
                                       
                                       # Calculate percent of adjacent blocks that are public
                                       block$PctPubAdjct <- round(100*(nrow(adj.pub)/nrow(blks.touch)),1)
                                       
                                       # Reclassify based on percent public
                                       block$Pred_Class <- ifelse(block$PctPubAdjct>=50,"Public","Private")
                                     }
                                   }
                                   return(block)
                                 }
          
          still.na <- reclass.out%>%
            filter(is.na(TouchesClass))
          
          reclass <- reclass.out
          
          n.still.na <- nrow(still.na)
          
          n.loops <- n.loops+1
          print(paste0("Completed ",n.loops," loops and reclassified ",n.reclass - n.still.na,
                       " blocks --- ",Sys.time()))
        }
        
        
      }
      
      # Combine data sets and add a flag if we reclassified
      reclass.all <- islands.reclass%>%
        filter(!is.na(Pred_Class))%>%
        rbind(reclass)%>%
        mutate(NA_Reclass = ifelse(HU_2020 == 0 & !is.na(Pred_Class),TRUE,FALSE))
      
      df.out <- reclass.all%>%
        st_drop_geometry()
      write.csv(df.out,paste0("Water_Use/outputs/data/reclassify_na/",substr(files$name[i],1,6),"_Reclassify_NA.csv"))
      
      print(paste0("Completed ", substr(files$name[i],1,6)," --- ",Sys.time()))
      #st_write(reclass.all,paste0("/work/GRDVULN/water/outputs/reclass_na/",substr(files$name[i],1,6),"_Reclass_NA.shp"), append = FALSE)
    }else({
      df.out <- islands.reclass%>%
        st_drop_geometry()%>%
        mutate(NA_Reclass = FALSE)
      
      write.csv(df.out,paste0("Water_Use/outputs/data/reclassify_na/",substr(files$name[i],1,6),"_Reclassify_NA.csv"))
      print(paste0("Completed ", substr(files$name[i],1,6)," --- ",Sys.time()))
    })
  }
}

stopCluster(cl)







