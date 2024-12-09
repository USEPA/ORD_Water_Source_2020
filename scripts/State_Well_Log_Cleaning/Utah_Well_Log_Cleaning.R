library(tidyverse)
library(vroom)
library(rvest)
library(here)
library(sf)
library(foreach)
library(doParallel)

# Load Utah well logs
ut <- vroom(here("data/RawLogs/Utah_Well_Logs.csv"))
# Iterate through well logs to scrape data

n_cores <- detectCores() - 1  
cl <- makeCluster(n_cores)  
registerDoParallel(cl)  
start <- Sys.time()
utData <- foreach(n=1:nrow(ut),.combine = 'rbind', .packages = c("tidyverse","rvest")) %dopar% {
  site <- read_html(ut$LINK[n])
  
  
  # This returns table elements
  tbls <- site%>%
    html_elements("table")%>%
    html_text()
  
  # Work through the tables to collect data
  
  ## Table 1
  # Convert to table
  df1 <- data.frame(table = as.character(tbls[1]))%>%
    separate(table,into = LETTERS, sep = "\r\n")
  
  # Create new data frame
  row1 <- data.frame(matrix(ncol = 5,nrow = 1))
  colnames(row1) <- str_squish(df1[1,1:5])
  row1[1,1:5] <- str_squish(df1[1,6:10])
  
  ## Table 2
  # Convert to table
  df2 <- data.frame(table = as.character(tbls[2]))%>%
    separate(table,into = LETTERS, sep = "\r\n")
  
  # Create new data frame
  row2 <- data.frame(matrix(ncol = 8,nrow = 1))
  colnames(row2) <- str_squish(df2[1,1:8])
  row2[1,1:8] <- str_squish(df2[1,9:16])
  
  
  # Table 3 is just comments
  
  ## Table 4
  # Convert to table
  df4 <- data.frame(table = as.character(tbls[4]))%>%
    separate(table,into = LETTERS, sep = "\r\n")
  
  # Create new data frame
  row4 <- data.frame(matrix(ncol = 7,nrow = 1))
  colnames(row4) <- str_squish(df4[1,1:7])
  row4[1,1:7] <- str_squish(df4[1,8:14])
  
  # Write data to utah dataset
  newRow <- data.frame(WIN = ut$WIN[n],
                       Activity = row1[1,4],
                       Activity_Date =  row1[1,5],
                       Well_Depth = row2[1,5],
                       Well_Intake_Depth = row2[1,7],
                       Dpth_to_Water = row4[1,5],
                       Dpth_Date = row4[1,3])
}

end <- Sys.time()

# rbind
perf <- data.frame(n = c(100,250,500,1000),
                   time = c(19.7,27.96,50.795, 98.835))


ggplot(perf, aes(x = n, y = time))+
  geom_line()



utJoin <- ut%>%
  left_join(utData)%>%
  select(!WRCHEX)%>%
  distinct()


vroom_write(utJoin, here("data/RawLogs/Utah_Appended.csv"), delim = ",")
utJoin <- vroom(here("data/RawLogs/Utah_Appended.csv"))


template <- data.frame(State = character(),
                       Well_ID = character(),
                       Dpth_to_Water = double(),
                       Dpth_Unit = character(),
                       Dpth_Date = character(),
                       Pump_Rate = double(),
                       Pump_Rate_Unit = character(),
                       Dpth_to_Bedrock = double(),
                       Dpth_to_Bedrock_Unit = character(),
                       Install_Date = character(),
                       Well_Type = character(),
                       Well_Depth = double(),
                       Well_Depth_Unit = character(),
                       Latitude = double(),
                       Longitude = double(),
                       XY_Source = character())


utSf <- utJoin%>%
  st_as_sf(coords = c("LON","LAT"), crs = 4269)


utClean <- utJoin%>%
  mutate(Install_Date = ifelse(Activity == "New" | Activity == "Replace", Activity_Date,NA))%>%
  select(WIN,Dpth_to_Water,Dpth_Date,Well_Depth,Install_Date,LAT,LON)%>%
  mutate(Dpth_to_Water = as.numeric(Dpth_to_Water),
         Well_Depth = as.numeric(Well_Depth),
         Dpth_Date = as.character(lubridate::mdy(Dpth_Date)),
         Install_Date = as.character(lubridate::mdy(Install_Date)),
         State = "UT",
         Dpth_Unit = "ft",
         Well_Depth_Unit = "ft",
         WIN = as.character(WIN))

colnames(utClean) <- c("Well_ID","Dpth_to_Water","Dpth_Date","Well_Depth","Install_Date","Latitude","Longitude","State","Dpth_Unit","Well_Depth_Unit")


utReady <- template%>%
  bind_rows(utClean)

vroom_write(utReady, here("Data/Clean_Well_Logs/Utah_Clean1.tsv"), delim = "\t")




### BELOW CODE IS FOR TESTING ###
# s2 <- read_html("https://waterrights.utah.gov/wellinfo/welldrilling/wlbrowse.asp?WIN=428348")
# 
# s2%>%table <- url%>%
#   html_nodes(".DescriptionCell a")%>%
#   html_text()
# 
# simple %>%
#   html_nodes("td")%>%
#   html_text()
# 
# vals <- simple %>%
#   html_nodes("td")%>%
#   html_text()
# 
# vals[53]
# 
# id <- vals[23]
# wellDepth <- vals[26]
# wtrDate <- vals[51]
# wtrDepth <- vals[53]
# 
# newRow <- data.frame(Well_ID = vals[23],
#                      Well_Depth = vals[26],
#                      Dpth_to_Water = vals[53],
#                      Dpth_Date = vals[51])
# 
# tbl <- s2%>%
#   html_element("pageBanner")%>%
#   html_text()
# 
# 
# cols <- s2%>%
#   html_nodes(".columnHeader")%>%
#   html_text()%>%
#   as.character()
# 
# df <- as.data.frame(matrix(ncol = 29))
# colnames(df) <- cols
# 
# vals <- s2%>%
#   html_nodes(".whitebar")
# html_text()
# 
# # This returns the table names
# s2%>%
#   html_nodes(" .pageBanner")%>%
#   html_text()  
# 
# # This returns 8 empty values
# s2%>%
#   html_nodes(" .formspacer")%>%
#   html_text() 
