# Set Global Packages and Variables
library(plyr)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(data.table)
library(DT)

#library(janitor)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(leaflet.extras)

library(sf)
library(nhdplusTools)

#library(RCurl)
library(curl)
library(stringr)
library(downloader)

url<- "https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/HydroRegions/"
h<- new_handle(dirlistonly=TRUE)
con<-curl(url, "r", h)
tbl<-read.table(con,stringsAsFactors=TRUE,fill=TRUE)
close(con)


reads<-tbl[[1]] %>% as.character()
reads<-reads[grep(".zip",reads)]

starts<-rep(7,length(reads))
ends<-3+str_locate(reads,".zip")[,1]

files<-substr(reads,starts,ends)

url0<-"https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/HydroRegions/"

for (i in 1:10){
#for (i in 1:length(files)){
  urli<-paste(url0,files[i],sep="")  
  tmp<-tempfile(tmpdir=getwd(),fileext=".zip")

  dest<-paste("data/",files[i],sep="")
  
  download(urli, dest=dest, mode="wb") 
  
}

