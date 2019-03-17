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
#iFile$ExpFolder <- file.path("..", "..", "..", "Mozambique Ph2") #location of the files to be summarized
iFile$ExpFolder <- file.path("..", "..", "..", "Malawi Ph2") #location of the files to be summarized
iFile$IgnoreFolder <- c("") #folders NOT to include in the schedule

oFile <- list()
oFile$filename <- "ExposureDataSchedule_MW"
oFile$folder$spreadsheet <- file.path("..","Spreadsheets", "File Summary") #output location (same as summary spreadsheet location)

  ## User Inputs ##
shapeFile <- list()
shapeFile$extension <- c("cpg","dbf", "prj", "sbn", "sbx", "shp", "shx", "qpj","lpk", "pitem",
                         "gpkg", "kmz", "kml", "gdb")
shapeFile$omit <- c("cpg","dbf", "prj", "sbn", "sbx", "shx")


machineReadable_list <- list()
machineReadable_list$yes <- c(shapeFile$extension, "txt", "README", "mdb", "xlsx", "xls", "xlsm", "csv",
                              "html", "lyr", "json")

machineReadable_list$no <- c("pdf",  "PDF", "zip", "7z", "png", "avi", "wmv", "mpg", "sd",
                             "gif", "jpg", "doc")


colHeaders <- c("Geometry Type", "Spatial Extent", "Spatial Resolution",
                "Exposure Disaggregation", "Building Typology Distribution", "Asset Value Determination",
                "Non-Building Infrastructure Model", "Disaggregation GDP Model",
                "Relevant Analysis Steps", 
                "Social and institutional impacts","Service delivery impacts","Economic recovery",
                "Occupancy Coverage", "Typology Data", "Structural Condition Data",
                "Completeness_Coverage", "Age of Dataset", "Time Coverage", "Data Source", "Uncertainties", "Reliability",
                "Additional Notes") #NOTE there are more column headings defined at the end of section 2

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
#ToDo: deal with spanish characters (UTF-8)
fullPath <- list.files(iFile$ExpFolder, recursive = T, include.dirs=F)

  ## Omit Specified Folders and Files ##
fullPath <- fullPath[!word(fullPath, sep = "/", start = 1) %in% iFile$IgnoreFolder] #ignore specified folders
#fullPath <- fullPath[!word(fullPath, sep = "\\.", start = -1) %in% shapeFile$omit] #ignore specified file extensions


  ## Extract File Info ##
fileName <- word(fullPath, sep = "/", start = -1)

headFolder <- word(fullPath, sep = "/", start = 1)  
headFolder[grepl(pattern = "\\.", x = headFolder)] <- NA #word returns '.' if no headFolders. Replace this with NA

subFolder <- word(fullPath, sep = "/", start = 2)
subFolder[grepl(pattern = "\\.", x = subFolder)] <- NA

fileSize <- file.size(file.path(iFile$ExpFolder, fullPath))

extension <- word(fileName, sep="\\.", start = -1)


  ## Collate as Table ##
DataSchedule <- data.table(fileName, headFolder, subFolder, 
                           fileSize_byte=fileSize, extension, fullPath)
#rm(fileName, headFolder, subFolder, fileSize, fullPath, extension)


#DataSchedule$machineReadable <- "NA"
DataSchedule[extension %in% machineReadable_list$yes, machineReadable:="yes"]
DataSchedule[extension %in% machineReadable_list$no, machineReadable:= "no"]

DataSchedule[extension %in% shapeFile$extension, shapeFile:="yes"]

#ToDo: highlight zipped folders with shapefiles NOT in the root folder (if un sub folders within the zipped folder then they won't be read by QGIS)

zipped_contains <- function(zipFile, yesList, noList=NA) {
  a <- unzip(zipfile = zipFile, list = T) #list of all files in zip folder
  b <- word(string = a$Name, sep = "\\.", start = -1) #all file extensions
  
  c <- b %in% yesList
  if(any(c)) return("yes")
} #return "yes" if ANY files extensions within the zipped folder are on the yesList
# zipped_contains(file.path(iFile$ExpFolder, "Malawi-RiskProfile.zip"), machineReadable_list$yes)
DataSchedule[extension == "zip"]$machineReadable <- 
  sapply(X = file.path(iFile$ExpFolder,DataSchedule[extension == "zip"]$fullPath),
         FUN = zipped_contains, yesList=machineReadable_list$yes) #for each zipped folder check if it contains a machinereadable file

DataSchedule[extension == "zip"]$shapeFile <- 
  sapply(X = file.path(iFile$ExpFolder,DataSchedule[extension == "zip"]$fullPath),
         FUN = zipped_contains, yesList=shapeFile$extension) #for each zipped folder check if it contains a GIS file

DataSchedule[,(colHeaders):=NA] #initialize all of the additional user-defined headings

  ## Arrange in Final Format ##
View(DataSchedule)
a <- DataSchedule[,.(FileName=fileName, Checked=NA, `Key Source`=NA, `Document Type`=NA, Description=NA,
                     HeadFolder=headFolder,`HeadFolder Description`=NA,
                     SubFolder=subFolder, `SubFolder Description`=NA,
                     FileSize_byte=fileSize_byte, FullPath=fullPath, Extension=extension, 
                     MachineReadable=machineReadable, ShapeFile=shapeFile)]
a[,(colHeaders):=NA]

#ToDo: Set Document Type to Shapefile for shapefiles (also do if any relevant files within zipped folder)
#ToDo: Set Document Type to Spreadsheet for .xls, .xlsx, .xlsm, .csv (also do if any relevant files within zipped folder)


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