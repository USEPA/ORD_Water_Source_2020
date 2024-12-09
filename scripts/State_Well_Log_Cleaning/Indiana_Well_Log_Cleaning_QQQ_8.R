library(tidyverse)
library(vroom)
library(sf)
library(foreach)
library(doParallel)
library(here)
# Import and merge QQ sections

layers <- data.frame(layer = st_layers(here("data/PLSS/Indiana.gpkg"))$name)%>%
  filter(substr(layer,1,3)=="QQ_")
qq <- st_read(here("data/PLSS/Indiana.gpkg"),layer = layers$layer[1])

for(n in 2:nrow(layers)){
  qqNext <- st_read(here("data/PLSS/Indiana.gpkg"),layer = layers$layer[n])
  qq <- rbind(qq,qqNext)
}


# Subset to Quarter/Quarter sections where wells exist
wells <- vroom(here("data/PLSS/Indiana_Wells_All.tsv"))%>%
  filter(!is.na(Quar2))%>%
  mutate(QID = paste0(County1,"_",Twp1,"_",Rng1,"_",Quar3,"_",Quar2))

qqsub <- qq%>%
  mutate(QID = paste0(County,"_",Township,"_",Range,"_",QSec,"_",QQSec))%>%
  filter(QID %in% wells$QID)

qq <- qqsub



qq <- qq[308786:nrow(qq),]

n_cores <- detectCores() - 1  
cl <- makeCluster(n_cores)  
registerDoParallel(cl)  
start <- Sys.time()
print(paste0("Starting to break down polygons at ",Sys.time()," with ",n_cores, " cores"))
qqq <- foreach(n=1:nrow(qq), .combine='rbind', .packages = c("tidyverse","sf")) %dopar% {
  row <- qq[n,]
  qqqGrid <- st_as_sf(st_make_grid(row, n = c(2,2)))%>%
    mutate(County = row$County,
           Township = row$Township,
           Range = row$Range,
           Section = row$Section,
           QSec = row$QSec,
           QQSec = row$QQSec,
           QQQSec = c("SW","SE","NW","NE"))
}
end <- Sys.time()

print(paste0("Completed in: ",round(as.numeric(difftime(end,start,units = "mins")),2)," minutes"))
st_write(qqq,"/work/GRDVULN/github/ORD_Groundwater_Depth/data/Indiana/Indiana.gpkg",layer = "QQQ_Sections_8", append = FALSE)