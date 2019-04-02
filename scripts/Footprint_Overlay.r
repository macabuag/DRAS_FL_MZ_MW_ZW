# Title: Footprint_Overlay.r
# By: Josh Macabuag (macabuag@gmail.com)
# Date: 02-Apr-2019
# Description: This script imports hazard and exposure layers, calculates an overlay ,
#              and estimates losses.
# Naming Convention: 
#              variables: lowerCamelCase
#              datasets: UpperCamelCase

## 1.0 SET-UP ################################################################################################################
## Timer ##
time <- list()
time$start <- Sys.time()

## General I/O Files ##
iFile <- list()
iFile$GISFolder <- file.path("..", "GIS") #location of the GIS folder
iFile$ZWE_admin2_losses <- file.path(iFile$GISFolder, "generated layers", 
                                      "ZWE Results", "zwe_adm2_zimstat_ocha_itos_20180911-FInal-losses.gpkg")

oFile <- list()

## User Inputs ##


## Create Output Folder ##
oFile$folder$R <- paste0(gsub("[[:punct:]]", " ", Sys.time()), "-Overlay")  #create a unique folder with the run date


## Packages ##
if(!require("data.table")) {
  install.packages("data.table")
  library(data.table)
}
if(!require("bit64")) {
  install.packages("bit64")
  library(bit64)
} #to read type 'integer64'

if(!require("stringr")){
  install.packages("stringr")
  library(stringr)
}

if(!require("rgdal")){
  install.packages("rgdal")
  library(rgdal)
}


## 2.0 Read ShapeFiles -----------------------------------------------------------------

## Losses ##
layers <- ogrListLayers(dsn = file.path(iFile$ZWE_admin2_losses))


ZWE_admin2_losses <- readOGR(dsn = file.path(iFile$ZWE_admin2_losses), layer = layers[1]) 
summary(ZWE_admin2_losses)
