library(tidyverse)
library(sf)
library(vroom)
library(here)

# Create mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Join the percent of self-suppliers using wells in 1990
df.ss <- vroom(here("Water_Use/outputs/data/Blocks_2020.csv"))%>%
  mutate(Non_Wells = round((HU_1990 - Public_S_90) * (1-Pct_Wells_Other_S)))%>%
  select(GISJOIN,Non_Wells,Pct_Wells_Other_S)

# Load final estimates for blocks and summarize
df <- vroom("Water_Use/outputs/Well_Estimates_2020_Blocks.csv")%>%
  mutate(Wells_1990 = round(((100-PctPub90)/100)*HU_1990),
         Est_Wells_2020_CRCT = ifelse(Wells_1990 == 0 & HU_1990 >0.5,0,Est_Wells_2020),
         Flag = ifelse(Wells_1990 == 0 & HU_1990 >0.5,"100% Public 1990",NA))%>%
  left_join(df.ss)

check2 <- df%>%
  filter(substr(GISJOIN,1,15)=="G34000509822003")

df.bg <- df%>%
  mutate(Tree_Cat = replace_na(Tree_Cat,"NA"))%>%
  group_by(GISJOIN_BG)%>%
  summarise(State = State[1],
            HU_1990_BG = round(sum(HU_1990,na.rm = TRUE)),
            HU_2020_BG = sum(HU_2020,na.rm = TRUE),
            Area_Km_BG = sum(Area_Km,na.rm = TRUE),
            Prob_Pub_BG = mean(Prob_Pub,na.rm = TRUE),
            Tree_Cat_BG = getmode(Tree_Cat),
            NHU_2020_BG = sum(NHU_Wells_2020,na.rm = TRUE),
            RW_2020_BG = sum(RW_Wells_2020,na.rm = TRUE),
            Wells_2020_BG = sum(Est_Wells_2020_CRCT, na.rm = TRUE),
            Wells_1990_BG = sum(Wells_1990,na.rm=TRUE),
            Non_Wells_90_BG = sum(Non_Wells,na.rm=TRUE))%>%
  ungroup()%>%
  mutate(Non_Wells_90_BG = ifelse(Non_Wells_90_BG>Wells_1990_BG,Wells_1990_BG,Non_Wells_90_BG),
         Pct_Change_BG = round(100*((Wells_2020_BG-Wells_1990_BG)/Wells_1990_BG),1),
         Tot_Change_BG = Wells_2020_BG-Wells_1990_BG,
         Pct_Non_Wells_90_BG = round(100*(Non_Wells_90_BG/Wells_1990_BG)),
         Pct_Non_Wells_90_BG = ifelse(Wells_1990_BG >0,Pct_Non_Wells_90_BG,NA))

# Load population
pop <- vroom("D:/data/nhgis/tables/Block_Groups/2020/nhgis_ds248_2020_blck_grp.csv")%>%
  select(GISJOIN,U7B001,U7G002)
colnames(pop) <- c("GISJOIN","Pop_2020","HU_2020_O")
# Load block groups
bg <- st_read("D:/data/nhgis/boundaries/Block_Groups/US_blck_grp_2020.shp")%>%
  select(GISJOIN,GEOID,ALAND)%>%
  left_join(df.bg, by = c("GISJOIN"="GISJOIN_BG"))%>%
  left_join(pop)%>%
  mutate(Pct_SS = round(100*(Wells_2020_BG/HU_2020_BG),1),
         Pop_Served = round(Pop_2020*(Pct_SS/100)),
         Prob_Pub = round(Prob_Pub_BG,3),
         Area_Km = round(ALAND/1000000,3),
         Well_Density_1990 = round(Wells_1990_BG/Area_Km,3),
         Well_Density_2020 = round(Wells_2020_BG/Area_Km,3))%>%
  select(GISJOIN,GEOID,State,Area_Km,HU_1990_BG,Wells_1990_BG,Well_Density_1990,Prob_Pub_BG,
         Tree_Cat_BG,HU_2020_BG,HU_2020_O,Pop_2020,NHU_2020_BG,RW_2020_BG,Wells_2020_BG,Well_Density_2020,
         Pct_SS,Pop_Served,Pct_Change_BG,Tot_Change_BG,Pct_Non_Wells_90_BG)%>%
  st_transform(4269)%>%
  st_make_valid()

st_write(bg,"Water_Use/outputs/Final_Estimates/spatial.gpkg",layer = "Block_Groups_2020", append = FALSE)


# Finalize block dataset

# Load block population
blk.pop <- vroom("D:/data/nhgis/tables/Blocks/nhgis0307_ds248_2020_block.csv")%>%
  select(GISJOIN,GEOID,U7B001,U7G002)
colnames(blk.pop) <- c("GISJOIN","GEOID","Pop_2020","HU_2020_O")


# TABLE ONLY
blks.df.clean <- df%>%
  left_join(blk.pop)%>%
  #filter(Wells_1990 >0 | Est_Wells_2020>0)%>%
  mutate(Pct_SS = round(100*(Est_Wells_2020/HU_2020),1),
         Pop_Served = round(Pop_2020*(Pct_SS/100)),
         Prob_Pub = round(Prob_Pub,3),
         Well_Density_1990 = round(Wells_1990/Area_Km,3),
         Well_Density_2020 = round(Est_Wells_2020_CRCT/Area_Km,3),
         Pct_Change = round(100*((Est_Wells_2020_CRCT-Wells_1990)/Wells_1990),1),
         Tot_Change = Est_Wells_2020_CRCT-Wells_1990)%>%
  left_join(df.ss)%>%
  mutate(Pct_Non_Wells_90 = round(100*(1-Pct_Wells_Other_S),2))%>%
  select(GISJOIN,GEOID,State,Area_Km,HU_1990,Wells_1990,Well_Density_1990,Prob_Pub,Tree_Cat,HU_2020,HU_2020_O,
         Pop_2020,NHU_Wells_2020,RW_Wells_2020,Est_Wells_2020,Est_Wells_2020_CRCT,Flag,Well_Density_2020,
         Pct_SS,Pop_Served,Pct_Change,Tot_Change,Pct_Non_Wells_90)

vroom_write(blks.df.clean, "Water_Use/outputs/Block_2020_Estimates_Final.csv",delim = ",")

# Iterate through the blocks state by state, run a 10m simplification
block.files <- data.frame(path = list.files("D:/data/nhgis/boundaries/Blocks/2020/",full.names = TRUE, pattern = ".shp$"),
                          file = list.files("D:/data/nhgis/boundaries/Blocks/2020/",full.names = FALSE, pattern = ".shp$"))%>%
  mutate(state = substr(file,1,2))

pb <- txtProgressBar(min=0,max = 51,initial = 0, style = 3)

for(n in 2:nrow(block.files)){
  blks.clean <- st_read(block.files$path[n],quiet = TRUE)%>%
    select(GISJOIN)%>%
    left_join(blks.df.clean, by = "GISJOIN")%>%
    st_simplify(dTolerance = 10)%>%
    st_transform(4269)%>%
    st_make_valid()
  
  st.name <- blks.clean%>%
    st_drop_geometry()%>%
    filter(!is.na(State))%>%
    select(State)%>%
    distinct()%>%
    as.character()
  
  blks.clean.all <- blks.clean%>%
    mutate(State = replace_na(State,st.name),
           HU_1990 = replace_na(HU_1990,0),
           HU_1990 = round(HU_1990),
           Wells_1990 = replace_na(Wells_1990,0),
           Well_Density_1990 = replace_na(Well_Density_1990),
           HU_2020 = replace_na(HU_2020,0),
           HU_2020_O = replace_na(HU_2020_O,0),
           Pop_2020 = replace_na(Pop_2020,0),
           NHU_Wells_2020 = replace_na(NHU_Wells_2020,0),
           RW_Wells_2020 = replace_na(RW_Wells_2020,0),
           Est_Wells_2020 = replace_na(Est_Wells_2020,0),
           Est_Wells_2020_CRCT = replace_na(Est_Wells_2020_CRCT,0),
           Well_Density_2020 = replace_na(Well_Density_2020,0),
           Pop_Served = replace_na(Pop_Served,0))
  
  st_write(blks.clean.all,paste0("Water_Use/outputs/Final_Estimates/Block_Shapefiles/",st.name,"_Blocks_2020.shp"),append = FALSE)
  
  setTxtProgressBar(pb,n)
}



# TEMP
blks.df.clean <- vroom("Water_Use/outputs/Block_2020_Estimates_Final.csv")
