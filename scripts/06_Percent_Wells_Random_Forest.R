library(tidyverse)
library(tidymodels)
library(vroom)


# Load classified blocks and add block group ID, join data on well estimates from 25 states for 2020 &
# filter to block groups with both public and private
fips <- select(tidycensus::fips_codes,state_code,state)%>%
  distinct()

# Add centroids
cntrds <- vroom("Water_Use/misc/Block_Centroids.csv")

# RW estimates
rw.files <- list.files("Water_USe/outputs/data/Drilled_Wells_Allocated/", full.names = TRUE)
rw.estimates <- vroom(rw.files)%>%
  mutate(state_code = substr(GISJOIN,2,3))%>%
  left_join(fips, by = "state_code")%>%
  select(GISJOIN,GISJOIN_BG,state,Est_Wells_2020)

# Predicted classes
predClass <- vroom("Water_Use/outputs/data/US_Blks_Reclass_NEW.csv")%>%
  left_join(cntrds)

# Identify block groups with public and private classes
class.distinct <- predClass%>%
  filter(state %in% rw.estimates$state)%>%
  select(GISJOIN_BG,Pred_Class)%>%
  filter(Pred_Class %in% c("Private","Public"))%>%
  distinct()%>%
  group_by(GISJOIN_BG)%>%
  summarise(n = n())%>%
  filter(n == 2)

# Filter blocks to block groups that have multiple classes and are classified as public
# And states where we are confident about well records
split.class <- predClass%>%
  filter(GISJOIN_BG %in% class.distinct$GISJOIN_BG & Pred_Class == "Public")%>%
  left_join(rw.estimates)%>%
  mutate(Pct_Wells = 100*(Est_Wells_2020/HU_2020),
         Pct_Wells = ifelse(Pct_Wells >100,100,Pct_Wells))%>%
  drop_na(Pct_Wells)

set.seed(123)
sampleSize <- sample(nrow(split.class),250000,replace = FALSE)

split.class <- split.class[sampleSize,]

set.seed(123)
split <- initial_split(split.class, prop = 0.7, strata = Pct_Wells)

train <- training(split)
test <- testing(split)

mtry.vals <- seq(2,9,2)
tree.vals <- seq(500,5500,1000)

vals <- expand.grid(mtry.vals,tree.vals)
colnames(vals) <- c("mtry","trees")

performance <- data.frame()

for(n in 1:nrow(vals)){
  set.seed(123)
  rf_mod <- rand_forest(mtry = vals$mtry[n], trees = vals$trees[n], mode = "regression")%>%
    set_engine("ranger", importance = "impurity")%>%
    fit(Pct_Wells ~ HU_1990+HU_2020+Intake_mi+Pct_Imprv+
          HU_Km_90+HU_Km_20+Area_Km+PctSwr90+PctPub90+Pct_HU_Chg+X+Y, data = train)
  
  pred <- rf_mod%>%
    predict(test)%>%
    bind_cols(test)
  
  acc <- pred%>%
    metrics(truth = Pct_Wells, estimate = .pred)
  
  newRow <- data.frame(mtry = vals$mtry[n], trees = vals$trees[n],rmse = acc$.estimate[1],
                       rsq = acc$.estimate[2], mae = acc$.estimate[3])
  
  performance <- rbind(performance,newRow)
  
  print(paste0("Completed model with mtry = ",vals$mtry[n],", & ",vals$trees[n]," trees. Returned: R2= ",round(newRow$rsq,3)," --- rmse= ",round(newRow$rmse,3)," --- ",Sys.time()))
  
}


# Final model
set.seed(123)
rf_mod <- rand_forest(mtry = 8, trees = 2500, mode = "regression")%>%
  set_engine("ranger", importance = "impurity")%>%
  fit(Pct_Wells ~ HU_1990+HU_2020+Intake_mi+Pct_Imprv+
        HU_Km_90+HU_Km_20+Area_Km+PctSwr90+PctPub90+Pct_HU_Chg, data = train)

# Started at 7:20 PM
Sys.time()

pred <- rf_mod%>%
  predict(test)%>%
  bind_cols(test)

acc <- pred%>%
  metrics(truth = Pct_Wells, estimate = .pred)

# Plot sample
pred.samp <- sample(seq(1,nrow(pred)),25000,replace = FALSE)
samp.df <- pred[pred.samp,]

ggplot(samp.df)+
  geom_point(aes(x = Pct_Wells, y = .pred))

# Save model
write_rds(rf_mod,"Water_Use/outputs/data/PctWells_RF.rds")

rf_mod <- read_rds("Water_Use/outputs/data/PctWells_RF.rds")

# Predict the % wells for blocks that are classified public and are within block groups with both public and private classifications
# in the 25 states where we did not have comprehensive well data. For the 25 states we have good well data, use the RW method.


# For every block predicted public within a split block group, use RF model to predict number of wells

# Fix intake distance in Arizona


two.class <- predClass%>%
  select(GISJOIN_BG,Pred_Class)%>%
  filter(Pred_Class %in% c("Private","Public"))%>%
  distinct()%>%
  group_by(GISJOIN_BG)%>%
  summarise(n = n())%>%
  filter(n == 2)

# Filter blocks to block groups that have multiple classes
split.public <- predClass%>%
  filter(GISJOIN_BG %in% two.class$GISJOIN_BG & Pred_Class == "Public" & HU_2020>0)

# Predict percent wells in blocks
pred.split <- rf_mod%>%
  predict(split.public)%>%
  bind_cols(split.public)

# Calculate number of Wells per block
# Private = 100%
# Public & in split block group = RW method or predicted

# Public blocks in non-RW states within split block groups
pred.count <- pred.split%>%
  mutate(RF_Wells_2020 = round((.pred/100)*HU_2020),
         type = "RF_Predicted")%>%
  select(GISJOIN,RF_Wells_2020)

rw.count <- rw.estimates%>%
  mutate(RW_Wells_2020 = Est_Wells_2020)%>%
  select(GISJOIN,RW_Wells_2020)

all.counts <- predClass%>%
  left_join(pred.count)%>%
  left_join(rw.count)%>%
  mutate(RW_Wells_2020 = ifelse(RW_Wells_2020>HU_2020,HU_2020,RW_Wells_2020),
         Est_Wells_2020 = ifelse(state %in% rw.estimates$state & GISJOIN %in% split.public$GISJOIN,RW_Wells_2020,
                                 ifelse(GISJOIN %in% split.public$GISJOIN,RF_Wells_2020,
                                        ifelse(Pred_Class == "Public",0,HU_2020))),
         method = ifelse(state %in% rw.estimates$state & GISJOIN %in% split.public$GISJOIN,"RW",
                         ifelse(GISJOIN %in% split.public$GISJOIN,"RF",
                                ifelse(Pred_Class == "Public","100% Public","100% Private"))),
         HU_Loss = ifelse(HU_2020 < HU_1990,HU_1990-HU_2020,0),
         NHU_Wells_2020 = round((((100-PctPub90)/100))*HU_2020-(HU_Loss*((100-PctPub90)/100))),
         NHU_Wells_2020 = ifelse(NHU_Wells_2020<0,0,NHU_Wells_2020))


vroom_write(all.counts, "Water_Use/outputs/Well_Estimates_2020_Blocks.csv")
#all.counts <- vroom("Water_Use/outputs/Well_Estimates_2020_Blocks.csv")

populated <- all.counts%>%
  filter(HU_2020>0)

rf <- all.counts%>%
  filter(method == "RF")

vroom_write(rf,"Water_Use/outputs/RF_preds.csv",delim = ",")

# 2010 estimate: 23 million / 131.7 million
# 2020 estimate: 25.5 million / 140.8

library(sf)
# Write to spatial blocks
block.files <- data.frame(path = list.files("D:/data/nhgis/boundaries/Blocks/2020/",full.names = TRUE, pattern = ".shp$"),
                          file = list.files("D:/data/nhgis/boundaries/Blocks/2020/",full.names = FALSE, pattern = ".shp$"))%>%
  mutate(state = substr(file,1,2))

for(n in 1:nrow(block.files)){
  shp <- st_read(block.files$path[n])%>%
    select(GISJOIN)%>%
    left_join(all.counts)%>%
    drop_na(Est_Wells_2020)%>%
    select(GISJOIN,State,HU_1990,HU_2020,Tree_Cat,PctPub90,RF_Wells_2020,NHU_Wells_2020,RW_Wells_2020,Est_Wells_2020,method)
    
  
  
  st_write(shp,paste0("Water_Use/outputs/data/est_wells_blocks_shp/",block.files$state[n],".shp"),append = FALSE)
}


# Aggregate and write to block groups

subset <- all.counts%>%
  filter(GISJOIN %in% split.public$GISJOIN & state %in% rw.estimates$state)

rw.nhu <- lm(RW_Wells_2020~NHU_Wells_2020, data = subset)
summary(rw.nhu)

ggplot(subset)+
  geom_point(aes(x = RW_Wells_2020, y = NHU_Wells_2020))


wisconsin <- subset%>%
  filter(State == "Wisconsin")

ggplot(wisconsin)+
  geom_point(aes(x = RW_Wells_2020, y = NHU_Wells_2020,color = "NHU"), color = "blue")+
  geom_point(aes(x = RW_Wells_2020, y = RF_Wells_2020), color = "green")

rw.nhu.wi <- lm(RW_Wells_2020~NHU_Wells_2020, data = wisconsin)
summary(rw.nhu.wi)


rw.rf.wi <- lm(RW_Wells_2020~RF_Wells_2020, data = wisconsin)
summary(rw.rf.wi)

ggplot(subset)+
  geom_point(aes(x = RW_Wells_2020, y = RF_Wells_2020))
