# system.time({

#-----------------------------------------------------------------------------#
# vec_building_turnover.R

# Type: R function script

# AUTHOR:
# Tyler Huntington, 2017

# PROJECT:
# Project: California Building Turnover and Energy Use Study
# Principal Investigator: Hanna Breunig
# Lawrence Berkeley National Lab Sustainable Energy Group


# PURPOSE:
# A spatially explicit simulation model for quantifying
# current and future energy demands of all buildings except single-family homes
# on land parcels in the state of California.

# RETURNS:
# none

# OUTPUTS:

# 1. An excel workbook with summary tables detailing the types and statuses
# of CA building
# stock in the years 2016, 2020, and 2050 for the c

# 2. Three shapefiles for each CA county whose polygons represent:
# 2016 building stock for that county
# 2020 building stock for that county
# 2050 building stock for that county

# The attribute table of each shapefile contains parcel type, status and
# energy use intensity data specific to the corresponding year.

#-----------------------------------------------------------------------------#

# ########## TEMP: set wd for local testing ##########
# 
# # a basic function to get the filepath of the current script
# csf <- function() {
#   # adapted from http://stackoverflow.com/a/32016824/2292993
#   cmdArgs = commandArgs(trailingOnly = FALSE)
#   needle = "--file="
#   match = grep(needle, cmdArgs)
#   if (length(match) > 0) {
#     # Rscript via command line
#     return(normalizePath(sub(needle, "", cmdArgs[match])))
#   } else {
#     ls_vars = ls(sys.frames()[[1]])
#     if ("fileName" %in% ls_vars) {
#       # Source'd via RStudio
#       return(normalizePath(sys.frames()[[1]]$fileName))
#     } else {
#       if (!is.null(sys.frames()[[1]]$ofile)) {
#         # Source'd via R console
#         return(normalizePath(sys.frames()[[1]]$ofile))
#       } else {
#         # RStudio Run Selection
#         # http://stackoverflow.com/a/35842176/2292993
#         return(normalizePath(rstudioapi::getActiveDocumentContext()$path))
#       }
#     }
#   }
# }
# 
# this.dir <- dirname(csf())
# setwd(this.dir)
# rm(list=ls())
# 
# ########## TEMP ##########

#-----------------------------------------------------------------------------#


# ########### INSTALL REQUIRED LIBRARIES ############
# packages <- c("sp", "raster", "rgeos",
#               "geosphere", "doParallel", "iterators",
#               "foreach", "rgdal", "plyr","openxlsx", "doSNOW", "snow")
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())))
# }

########### LOAD REQUIRED LIBRARIES ############
require(raster)
require(sp)
require(rgeos)
require(doParallel)
require(iterators)
require(foreach)
require(rgdal)
require(plyr)
require(snow)
require(doSNOW)
require(openxlsx)

############ SOURCE CORE FUNCTIONS ############ 
source("ST_bt_functions.R")

############ LOAD & CLEAN SUPPLEMENTARY DATA ############

# set name of data file with sheets containing required tables and input data
input.datafile <- "../supp_data/Steps_BuildingStockTurnover_7_6.xlsx"

LoadSuppData(input.datafile)

########## INITIALIZE SPREADSHEET FOR SUMMARY OUTPUTS ##########

output.filename <-  "subtype_summary_tables"
output.filepath <- CreateOutputWorkbook(output.filename)

############ LOAD & PROCESS INPUT SHAPEFILES IN PARALLEL ############

# # set up processing que from shapefiles in input_shapefiles dir
# dir.files <- list.files("../input_shapefiles/")
# in.shapes <- dir.files[CheckExt(dir.files)]

# TEMP: load from binary
dir.files <- list.files("../output_data/binary_shapefiles/")
in.shapes <- substr(dir.files, 1, nchar(dir.files) - 4)


# KEEP FOR FINAL DRAFT!
# initialize parallel backend
no.cores <- (detectCores() - 1)
cl <- makeCluster(no.cores, type = "SOCK", outfile="")
registerDoSNOW(cl)

# iterate over shapefiles in 'input_shapefiles' directory
output.metalist <- 
  foreach(shapefile = in.shapes,
          .packages = c("plyr", "rgdal", "sp", "raster",  "rgeos", "openxlsx"),
          .export = c(ls())) %dopar% {
            
            try(ProcessCountyParcels(shapefile))
          }

# cancel parallel backend
stopCluster(cl)

# write output to excel spreadsheet
ExportSummaryTables(output.metalist, output.filename, output.filepath)
  

# # stop system timer 
# })




