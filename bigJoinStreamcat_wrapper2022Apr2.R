# clear workspace
rm(list=ls())

# 1. Load packages --------------------------------------------------------
library(janitor)
library(lubridate)
#library(readxl)
library(plyr)
library(tidyverse)
#library(reshape2)
#library(stringr)
library(data.table)
library(readtext)
#library(sf)
#library(nhdplusTools)

#  2. Set up the columns --------------------------------------------------

# 3. Read in relevant data files ------------------------------------------
# read in variable list with descriptions
#variables <- read.csv('./variablelist-quickreference.csv')
# NLCD2016_variables <- variables %>% filter(Data.Location == "All tables" | grepl("NLCD2016_", variables$Data.Location))

# read in data files for Region 12
#filepaths <- list.files(path = './data/', pattern = 'NLCD', recursive = TRUE)

region<-"12"
#region<-"11"
#region<-"10L"
#region<-"10U"
#region<-"08"
#region<-"07"

#regions<-c("01","02","03","04","05")z
#regions<-c("03N","03S","03W")
#regions<-c("04","05")
#regions<-c("06","07")
#regions<-c("08","09")
#regions<-c("10L","10U")
regions<-c("11","12")
#regions<-c("13","14")
#regions<-c("15","16")
#regions<-c("17","18")
#regions<-c("20","21","22")


for(j in 1:length(regions)){

region<-regions[j]

patternset<-paste("Region",region,'.zip$',sep="")
outfile<-paste0("", "combinedStreamcatRegion",region,"_", Sys.Date(), ".csv")

filepaths <- list.files(path = './EPAtables/', pattern = patternset, recursive = TRUE)
#filepaths<-filepaths[-grep("MTBS|EPA_FRS|FirePerimeters|ForestLoss|ICI_IWI|CanalDensity|GeoChemPhys4|wdrw|WaterInput|sw_flux|NRSA_Predicted|ImperviousSurfacesHiSlope|ImperviousSurfacesMidSlope|HiSlope_R|NonAgIntro",filepaths)]
filepaths<-filepaths[-grep("MTBS|EPA_FRS|FirePerimeters|ForestLoss|ICI_IWI|CanalDensity|GeoChemPhys4|wdrw|WaterInput|sw_flux|NRSA_Predicted",filepaths)]

#filepaths <- list.files(path = './data/', pattern = '.zip$', recursive = TRUE)
#filenames <- gsub('Region12.csv', '', filepaths)
#filenames <- gsub('^NLCD/', '', filenames)
filenames<-filepaths
filepaths <- paste0('./EPAtables/', filepaths)

#commands<-paste('unzip -cq', filenames)

#fread(cmd = 'unzip -cq myfile.zip')

#mydata <- lapply(filepaths, fread(function(x) {
#  commands$x
#}))

mydata <- lapply(filepaths, readtext)
names(mydata) <- filenames



# 4. Data table cleaning --------------------------------------------------
# Ignore common table columns on all files which contain them except the first
commonvariables <- filenames[-grep('RipBuf100_$', filenames)]
commonvariables <- commonvariables[-grep('HiSlope_$', commonvariables)]
commonvariables <- commonvariables[-grep('MidSlope_$', commonvariables)]
commonvariables <- commonvariables[-1] # remove the first list item so that the columns aren't completely deleted
mydata[commonvariables] <- map(mydata[commonvariables], ~ (.x %>% select(-c(CatAreaSqKm, WsAreaSqKm, CatPctFull, WsPctFull))))

# RipBuf100 tables
commonvariables <- filenames[grep('RipBuf100_$', filenames)]
commonvariables <- commonvariables[-1] # remove the first list item so that the columns aren't completely deleted
mydata[commonvariables] <- map(mydata[commonvariables], ~ (.x %>% select(-c(CatAreaSqKmRp100, WsAreaSqKmRp100, CatPctFullRp100, WsPctFullRp100))))

# HiSlope tables
# CatAreaSqKmSlp20, WSPctFullSlp20, CatAreaSqKmSlp10, WSPctFullSlp10, CatPctFull, WsPctFull
commonvariables <- filenames[grep('HiSlope_$', filenames)]
commonvariables <- commonvariables[-2] # for some reason NLCD2001 Hi Slope had the same metrics as all the other mid slopes, overrride for now
mydata[commonvariables] <- map(mydata[commonvariables], ~ (.x %>% select(-c(CatAreaSqKmSlp20, WsAreaSqKmSlp20, CatPctFullSlp20, WsPctFullSlp20))))

# MidSlope tables
commonvariables <- filenames[grep('MidSlope_$', filenames)]
commonvariables <- commonvariables[-1]
mydata[commonvariables] <- map(mydata[commonvariables], ~ (.x %>% select(-c(CatAreaSqKmSlp10, WsAreaSqKmSlp10, CatPctFullSlp10, WsPctFullSlp10))))

# # Pivot Longer
# mylongdata <- lapply(names(mydata), function(X, mydata) {
#   ans <- mydata[[X]] %>% pivot_longer(cols = c(2:ncol(mydata[[X]])), names_to = "Metric", values_to = "Value") ; ans},
#                             mydata = mydata)

# 5. Smart List Variable Names -------------------------------------------
# Get a list of variable names and locations:
variableLocations <- lapply(names(mydata), function(X, mydata) {ans <- data.frame(location = X,
                                                                                  variable = names(mydata[[X]])); ans},
                            mydata = mydata)

# Concatenate list items into a data frame
variableLocations.df <- data.frame(location = character(), variable = character())
for(i in 1:length(variableLocations)){
  variableLocationsi <- data.frame(location = variableLocations[[i]]$location,
                                   variable = variableLocations[[i]]$variable)
  variableLocations.df <- rbind(variableLocations.df, variableLocationsi)
}
rm(variableLocations, variableLocationsi)

# Modify All tables Rows
variableLocations.df <- variableLocations.df %>% filter(variable != "COMID") %>% 
  add_row(variable = "COMID", location = "All tables") %>% # mutate All tables rows
  mutate(location = replace(location, variable == "CatAreaSqKm" | variable == "WsAreaSqKm" |
                              variable == "CatPctFull" | variable == "WsPctFull", "All tables")) %>%
  mutate(location = replace(location, variable == "CatAreaSqKmRp100" | variable == "WsAreaSqKmRp100" |
                              variable == "CatPctFullRp100" | variable == "WsPctFullRp100", "All RipBuf100 tables")) %>%
  mutate(location = replace(location, variable == "CatAreaSqKmSlp20" | variable == "WsAreaSqKmSlp20" |
                            variable == "CatPctFullSlp20" | variable == "WsPctFullSlp20", "All HiSlope tables")) %>%
  mutate(location = replace(location, variable == "CatAreaSqKmSlp10" | variable == "WsAreaSqKmSlp10" |
                            variable == "CatPctFullSlp10" | variable == "WsPctFullSlp10", "All MidSlope tables"))
# Add identifier table for smart list
variableLocations.df$identifier <- paste0(variableLocations.df$variable, " - ", variableLocations.df$location)
variableLocations.df$identifier <- gsub('_$', '', variableLocations.df$identifier)


# 6. Export ---------------------------------------------------------------
# combine into one data frame
# merge the first two to create bigdata

#for(i in 1:48){
for(i in 1:length(mydata)){
  datai<-mydata[[i]] %>% as.data.frame()
  datai<- datai %>% rename(COMID=text) %>% select(-doc_id)
  datai<-datai %>% arrange(COMID)
  if(i==1){
    data_wide<-datai
    next()}
  if(i>1){
    test<-datai$COMID-data_wide$COMID
    print(mean(test))
    
#    which_drop<-grep("CatAreaSqKm|WsAreaSqKm|CatPctFull|WsPctFull",names(datai))
#    if(length(which_drop)>0){
#    datai<-datai[,-which_drop] 
#    }
    datai<-datai %>% arrange(COMID)
    datai<-datai %>% select(-COMID)
    
#    datai<-datai %>% select(-CatAreaSqKm,-WsAreaSqKm,-CatPctFull,-WsPctFull) %>% arrange(COMID)
    data_wide<-cbind(data_wide,datai)
  }
  
}

bigdata<-data_wide[,-which(duplicated(names(data_wide))==TRUE)]

write.csv(bigdata, file = outfile)

#write.csv(bigdata, file = paste0("", "combinedStreamcatRegion12_", Sys.Date(), ".csv"))
write.csv(variableLocations.df, file = paste0("", "variableLocations_", Sys.Date(), ".csv"))

}
