library(tidymodels)
library(sf)
library(here)
library(vroom)
library(rpart.plot)
library(vip)

# import crosswalked data
cw <- vroom("D:/Github/ORD_Water_Supply/Water_Use/outputs/data/Blocks_2020.csv")



# Import distance & Imperviousness for all states
df <- vroom("D:/Github/ORD_Water_Supply/sensitive/Blocks_2020_DT_BUILD.csv")%>%
  mutate(Water_Type = as.factor(Water_Type))%>%
  select(GISJOIN,Water_Type,impervious,Intake_miles)%>%
  left_join(cw)%>%
  mutate(Area_Km = AREALAND/1000000,
         HU_Km_90 = HU_1990/Area_Km,
         HU_Km_20 = HU_2020/Area_Km,
         Pct_Public_S_90 = Public_S_90/HU_1990,
         Pct_Sewer_D_90 = Sewer_D_90/HU_1990,
         Pct_HU_Change = 100*(HU_2020=HU_1990)/HU_1990)


# Sampling training data from all 3 states
performance.3 <- data.frame()

# split data for training & testing, making sure to keep a similar ratio of unconfined:confined
set.seed(123)
split <- initial_split(df, prop = .75, strata = Water_Type)

train <- training(split)
#vroom_write(train,here("Water_Use/inputs/training_data.csv"),delim = ",")
test <- testing(split)
i <- 0
importance <- data.frame()
for(splits in seq(3,6)){
  # Fit a model
  tree_spec <- decision_tree(min_n = 30,
                             tree_depth = splits,
                             cost_complexity = 0.0000001)%>%
    set_engine("rpart")%>%
    set_mode("classification")
  
  set.seed(123)
  model <- tree_spec%>%
    fit(formula = Water_Type ~ Intake_miles+impervious+
          HU_Km_90+HU_Km_20+Area_Km+Pct_Public_S_90, data = train)
  
  # stable model
  #fit(formula = Water_Type ~ HU_1990+HU_2020+Intake_miles+impervious+
  #HU_Km_90+HU_Km_20+Area_Km+Pct_Public_S_90+Pct_Sewer_D_90+Pct_HU_Change, data = train)
  
  imp.vals <- data.frame(value = model$fit$variable.importance)%>%
    mutate(nSplits = splits,
           var = rownames(.))
  
  importance <- rbind(importance,imp.vals)
  
  # Predict the testing data
  predictions <-  predict(model, test, "class")
  
  predictions_combined <- predictions%>%
    cbind(test)
  
  # Confusion Matrix
  conf_matrix <- conf_mat(predictions_combined,
                          .pred_class,
                          Water_Type)
  
  # Measure Accuracy
  accuracy <- accuracy(predictions_combined,
                       truth = Water_Type,
                       estimate = .pred_class)
  accNum <- round(100*(accuracy$.estimate),2)
  
  # Accuracy of Public Water
  privateAcc <- round(100*(conf_matrix$table[1] / (conf_matrix$table[1] + conf_matrix$table[2])),2)
  
  publicAcc <- round(100*(conf_matrix$table[4] / (conf_matrix$table[3] + conf_matrix$table[4])),2)
  
  # Output Performance
  perf <- data.frame(Accuracy = accNum,
                     Public_Acc = publicAcc,
                     Private_Acc = privateAcc,
                     nSplits = splits)
  
  performance.3 <- rbind(performance.3,perf)
  i <- i+1
  print(paste0("Completed ",i," models: ",Sys.time()))
}

write.csv(performance.3, "Water_Use/outputs/data/Tree_Training_performance.csv")


# Try out a neural net
library(neuralnet)


# Density stats
d.stats <- cw%>%
  mutate(HU_Km_90 = HU_1990/(AREALAND/1000000),
         HU_Km_20 = HU_2020/(AREALAND/1000000),
         Type_90 = ifelse(Public_S_90 < HU_1990,"Private","Public"))%>%
  filter(HU_Km_90<1000000)

pub.90 <- d.stats%>%
  filter(Type_90 == "Public")

priv.90 <- d.stats%>%
  filter(Type_90 == "Private")

ggplot(d.stats)+
  geom_boxplot(aes(x = Type_90, y = HU_Km_90))+
  ylim(0,1000000)

# Rescale hu 1990
hukm.90 <- df%>%
  filter(HU_Km_90 < Inf)
hukm.90.sd <- sd(hukm.90$HU_Km_90)

ggplot(hukm.90)+
  geom_histogram(aes(x = HU_Km_90),binwidth = 500)+
  xlim(0,5000)

# Intake distance
ggplot(df)+
  geom_histogram(aes(x = Intake_miles),binwidth = 1,color="black",fill="white")+
  coord_cartesian(xlim=c(0,20))+
  geom_vline(xintercept = 4.923264, color = "red")

# Area
summary(df$Area_Km)

df.rescale <- df%>%
  mutate(Intake_miles = ifelse(Intake_miles>20,20,Intake_miles),
         Intake_miles = rescale(Intake_miles,c(0,1)),
         impervious = impervious/100,
         HU_Km_90 = ifelse(HU_Km_90>500,1,HU_Km_90),
         HU_Km_90 = rescale(HU_Km_90,c(0,1)),
         HU_Km_20 = ifelse(HU_Km_20>500,1,HU_Km_20),
         HU_Km_20 = rescale(HU_Km_20,c(0,1)),
         Area_Km = ifelse(Area_Km > 12.82,12.82,Area_Km), # 2*sd+mean
         Area_Km = rescale(Area_Km,c(0,1)))%>%
  drop_na()

set.seed(123)
split <- initial_split(df.rescale, prop = .05, strata = Water_Type)
train <- training(split)
test <- testing(split)

model = neuralnet(
  Water_Type~Intake_miles+impervious+HU_Km_90+HU_Km_20+Area_Km,
  data=train,
  stepmax = 1e+08,
  hidden=c(3,3,3,3),
  linear.output = FALSE
)

end <- Sys.time()

# Final Model
tree_spec <- decision_tree(min_n = 30,
                           tree_depth = 4,
                           cost_complexity = 0.0000001)%>%
  set_engine("rpart")%>%
  set_mode("classification")

set.seed(123)
model <- tree_spec%>%
  fit(formula = Water_Type ~ Intake_miles+impervious+
        HU_Km_90+HU_Km_20+Area_Km+Pct_Public_S_90+Pct_Sewer_D_90+
        HU_1990+Pct_HU_Change, data = train)

# SAVE MODEL
#saveRDS(model,"Water_USE/outputs/Final_Decision_Tree.RDS")
model <- readRDS("Water_USE/outputs/Final_Decision_Tree.RDS")

# Predict the testing data
predictions <-  predict(model, test, "class")

predictions_combined <- predictions%>%
  cbind(test)

accuracy <- accuracy(predictions_combined,
                     truth = Water_Type,
                     estimate = .pred_class)
accNum <- round(100*(accuracy$.estimate),2)

# Save Variable Importance
imp <- as.data.frame(model$fit$variable.importance)%>%
  mutate(variable = rownames(.))

colnames(imp) <- c("Importance","Variable")
rownames(imp) <- seq(1,nrow(imp))
imp.sel <- imp%>%
  select(Variable,Importance)%>%
  arrange(-Importance)%>%
  mutate(used = if_else(Variable %in% c("impervious","PctPublic_90","Intake_miles",
                                        "pct_HU_Change","HU_km_20","HU_1990","km2",
                                        "PctSewer_90"),"Yes","No"))
write.csv(imp,"Water_Use/outputs/data/Final_Model_Importance.csv")

ggplot(imp.sel)+
  geom_col(aes(x = reorder(Variable,Importance), y = Importance, fill = used))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.1, hjust=1.2))+
  scale_fill_manual(values = c("Yes" = "#40a333","No" = "grey50"))+
  labs(title = "Final Model Variable Importance", x = "", fill = "In Tree")+
  coord_flip()

## Final Model applied to the entire U.S.

## Load metrics for intake distance and imperviousness
intake.dist <- vroom(here("Water_Use/inputs/data/Intake_Distance.csv"))
colnames(intake.dist) <- c("GISJOIN","Intake_miles","NEAR_PWSID")

blks <- vroom("Water_Use/outputs/data/Blocks_2020.csv")%>%
  select(GISJOIN,STATE,AREALAND,HU_1990,HU_2020,Pub_WS_1990)
colnames(blks) <- c("GISJOIN","STATE","ALAND20","HU_1990","HU_2020","Pub_WS_1990")

imp.files <- list.files("Water_Use/inputs/data/imperviousness", full.names = TRUE)
imperv <- vroom(imp.files)

us.blks <- vroom("sensitive/Blocks_2020_DT_PREDICT_fix.csv")
new <- vroom("Water_Use/outputs/data/Blocks_2020.csv")

us.blks <- vroom("Water_Use/outputs/data/Blocks_2020.csv")%>%
  filter(HU_2020 > 0)%>%
  left_join(intake.dist, by = "GISJOIN")%>%
  left_join(imperv, by = "GISJOIN")%>%
  mutate(HU_Change = HU_2020 - HU_1990,
         HU_km_90 = HU_1990/(AREALAND/1000000),
         HU_km_20 = HU_2020/(AREALAND/1000000),
         km2 = AREALAND/1000000,
         PctPublic_90 = 100*(Pub_WS_1990/HU_1990),
         PctSewer_90 = 100*(Pub_WD_1990/HU_1990),
         pct_HU_Change = 100*((HU_2020 - HU_1990)/HU_1990),
         HU_1990 = replace_na(HU_1990,0),
         HU_2020 = replace_na(HU_2020,0),
         impervious = replace_na(impervious,0),
         HU_Change = replace_na(HU_Change,0),
         HU_km_90 = replace_na(HU_km_90,0),
         HU_km_20 = replace_na(HU_km_20,0),
         PctPublic_90 = replace_na(PctPublic_90,0),
         PctSewer_90 = replace_na(PctSewer_90,0))

# Apply Model
final.predictions <-  predict(model, us.blks, "class")
final.prob <-  predict(model, us.blks, "prob")

df.pred <- us.blks%>%
  cbind(final.predictions)%>%
  cbind(final.prob)%>%
  mutate(Tree_Cat = ifelse(impervious <9.2 & PctPublic_90<38&km2>=0.25&impervious<3.7,"A",
                           ifelse(impervious <9.2 & PctPublic_90<38&km2>=0.25&impervious>=3.7&pct_HU_Change<465,"B",
                                  ifelse(impervious <9.2 & PctPublic_90<38&km2>=0.25&impervious>=3.7&pct_HU_Change>=465,"C",
                                         ifelse(impervious <9.2 & PctPublic_90<38&km2<0.25&HU_1990>=0.32,"D",
                                                ifelse(impervious <9.2 & PctPublic_90<38&km2<0.25&HU_1990<0.32&HU_km_20<47,"E",
                                                       ifelse(impervious <9.2 & PctPublic_90<38&km2<0.25&HU_1990<0.32&HU_km_20>=47,"F",
                                                              ifelse(impervious <9.2 & PctPublic_90>=38&HU_km_20<23&km2>=1.1,"G",
                                                                     ifelse(impervious <9.2 & PctPublic_90>=38&HU_km_20<23&km2<1.1&PctSewer_90>=12,"H",
                                                                            ifelse(impervious <9.2 & PctPublic_90>=38&HU_km_20<23&km2<1.1&PctSewer_90<12,"I",
                                                                                   ifelse(impervious <9.2 & PctPublic_90>=38&HU_km_20>=23,"J",
                                                                                          ifelse(impervious>=9.2&PctPublic_90<71&impervious<29&pct_HU_Change<201&PctPublic_90<6.1,"K",
                                                                                                 ifelse(impervious>=9.2&PctPublic_90<71&impervious<29&pct_HU_Change<201&PctPublic_90>=6.1,"L",
                                                                                                        ifelse(impervious>=9.2&PctPublic_90<71&impervious<29&pct_HU_Change>=201,"M",
                                                                                                               ifelse(impervious>=9.2&PctPublic_90<71&impervious>=29&impervious<44,"N",
                                                                                                                      ifelse(impervious>=9.2&PctPublic_90<71&impervious>=29&impervious>=44&Intake_miles>=10,"O",
                                                                                                                             ifelse(impervious>=9.2&PctPublic_90<71&impervious>=29&impervious>=44&Intake_miles<10,"P",
                                                                                                                                    ifelse(impervious>=9.2&PctPublic_90>=71&Intake_miles>=10&HU_km_20<603,"Q",
                                                                                                                                           ifelse(impervious>=9.2&PctPublic_90>=71&Intake_miles>=10&HU_km_20>=603&HU_1990>=16,"R",
                                                                                                                                                  ifelse(impervious>=9.2&PctPublic_90>=71&Intake_miles>=10&HU_km_20>=603&HU_1990<16,"S",
                                                                                                                                                         ifelse(impervious>=9.2&PctPublic_90>=71&Intake_miles<10,"T",NA)))))))))))))))))))))
# Clean it up
states <- us.blks%>%
  select(GISJOIN,STATE)

clean <- df.pred%>%
  left_join(states)%>%
  select(GISJOIN,STATE,HU_1990,HU_2020,Intake_miles,impervious,HU_km_90,HU_km_20,
         km2,PctSewer_90,PctPublic_90,pct_HU_Change,.pred_class,.pred_Private,.pred_Public,Tree_Cat)

colnames(clean) <- c("GISJOIN","State","HU_1990","HU_2020","Intake_mi","Pct_Imprv",
                     "HU_Km_90","HU_Km_20","Area_Km","PctSwr90","PctPub90","Pct_HU_Chg",
                     "Pred_Class","Prob_Prvt","Prob_Pub","Tree_Cat")


# Save File
vroom_write(clean, "Water_Use/outputs/data/AZ_AR_DT_Predicted.csv", delim = ",")


# Create shapefiles
shp.files <- data.frame(path = list.files("D:/data/nhgis/boundaries/Blocks/2020", full.names = TRUE, pattern = ".shp$"),
                        file = list.files("D:/data/nhgis/boundaries/Blocks/2020", full.names = FALSE, pattern = ".shp$"))%>%
  mutate(State = substr(file,1,2))%>%
  filter(State %in% c("AZ","AR"))
for(n in 1:nrow(shp.files)){
  shp <- st_read(shp.files$path[n])%>%
    select(GISJOIN)%>%
    left_join(clean)%>%
    filter(!is.na(Pred_Class))
  
  st_write(shp,paste0("Water_Use/outputs/DT_Output_shp/",shp.files$State[n],"_classified_blocks.shp"),append = FALSE)
}
