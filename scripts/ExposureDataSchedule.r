# Title: Exposure Data Schedule
# By: Josh Macabuag (macabuag@gmail.com)
# Date: 17-Mar-2019
# Description: This script creates a table of all files within a folder vs a series of criteria
#              and populates those criteria which can be evaluated automatically.
#              The remaining criteria are to be completed manually.
# Naming Convention: 
#              variables: lowerCamelCase
#              datasets: UpperCamelCase

## 1.0 SET-UP ################################################################################################################
  ## Timer ##
time <- list()
time$start <- Sys.time()

  ## General Input Files ##
iFile <- list()
iFile$ExpFolder <- file.path("..", "..", "..", "Mozambique Ph2") #location of the files to be summarized
iFile$IgnoreFolder <- c("") #folders NOT to include in the schedule

oFile <- list()
oFile$filename <- "ExposureDataSchedule_MZ"
oFile$folder$spreadsheet <- file.path("..","Spreadsheets", "File Summary") #output location (same as summary spreadsheet location)

  ## User Inputs ##
machineReadable_list <- list()
machineReadable_list$yes <- c("txt", "README", "mdb", "xlsx", "xls", "xlsm", "csv",
                         "cpg", "dbf", "prj", "sbn", "sbx", "shp", "shx", "qpj", "lpk", "pitem", 
                         "html", "lyr", "json")

machineReadable_list$no <- c("pdf",  "PDF", "zip", "7z", "png", "avi", "wmv", "mpg", "sd",
                        "gif", "jpg", "doc")

shapeFile <- list()
shapeFile$extension <- c("cpg","dbf", "prj", "sbn", "sbx", "shp", "shx", "qpj","lpk", "pitem")
shapeFile$omit <- c("cpg","dbf", "prj", "sbn", "sbx", "shx")

colHeaders <- c("Geometry Type", "Spatial Extent", "Spatial Resolution",
                "Exposure Disaggregation", "Building Typology Distribution", "Asset Value Determination",
                "Non-Building Infrastructure Model", "Disaggregation GDP Model",
                "Relevant Analysis Steps", 
                "Social and institutional impacts","Service delivery impacts","Economic recovery",
                "Occupancy Coverage", "Typology Data", "Structural Condition Data",
                "Completeness_Coverage", "Age of Dataset", "Time Coverage", "Data Source", "Uncertainties", "Reliability",
                "Additional Notes")

  ## Create Output Files ##
oFile$folder$R <- paste0(gsub("[[:punct:]]", " ", Sys.time()), "-ExpDat")  #create a unique folder with the run date

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


## 2.0 Read Folder -----------------------------------------------
#ToDo: deal with spanish characters (UTC-8)
fullPath <- list.files(iFile$ExpFolder, recursive = T, include.dirs=F)

  ## Omit Specified Folders and Files ##
fullPath <- fullPath[!word(fullPath, sep = "/", start = 1) %in% iFile$IgnoreFolder] #ignore specified folders
#fullPath <- fullPath[!word(fullPath, sep = "\\.", start = -1) %in% shapeFile$omit] #ignore specified file extensions


  ## Extract File Info ##
fileName <- word(fullPath, sep = "/", start = -1)

headFolder <- word(fullPath, sep = "/", start = 1)  
headFolder[grepl(pattern = "\\.", x = headFolder)] <- NA

subFolder <- word(fullPath, sep = "/", start = 2)
subFolder[grepl(pattern = "\\.", x = subFolder)] <- NA

fileSize <- file.size(file.path(iFile$ExpFolder, fullPath))

extension <- word(fileName, sep="\\.", start = -1)


  ## Collate as Table ##
DataSchedule <- data.table(fileName, headFolder, subFolder, 
                           fileSize_byte=fileSize, extension, fullPath)
rm(fileName, headFolder, subFolder, fileSize, fullPath, extension)


DataSchedule$machineReadable <- "NA"
DataSchedule[extension %in% machineReadable_list$yes]$machineReadable <- "yes"
#ToDo: see if files within zip folder are machine readable
#ToDo: highlight zipped folders with shapefiles NOT in the root folder (if un sub folders within the zipped folder then they won't be read by QGIS)
DataSchedule[extension %in% machineReadable_list$no]$machineReadable <- "no"

#DataSchedule[,ShapeFile:="no"]
DataSchedule[extension %in% shapeFile$extension, shapeFile:="yes"]


DataSchedule[,(colHeaders):=NA]

  ## Arrange in Final Format ##
View(DataSchedule)
a <- DataSchedule[,.(FileName=fileName, Checked=NA, `Key Source`=NA, `Document Type`=NA, Description=NA,
                     HeadFolder=headFolder, SubFolder=subFolder, FileSize_byte=fileSize_byte,
                     FullPath=fullPath, Extension=extension, 
                     MachineReadable=machineReadable, MachineReadable_manual=NA,
                     ShapeFile=shapeFile)]
a[,(colHeaders):=NA]

DataSchedule <- a ; rm(a)


## 3.0 Write Output ---------------------------------------------------
dir.create(path = "outputs")
dir.create(path =  file.path("outputs", oFile$folder$R))
fwrite(DataSchedule, file = file.path("outputs", oFile$folder$R, 
                                      paste0(oFile$filename, ".csv")))
fwrite(DataSchedule, file = file.path(oFile$folder$spreadsheet,
                                      paste0(oFile$filename, ".csv")))


## 4.0 Tidy Up --------------------------------------------------------
time$total <- Sys.time() - time$start
time