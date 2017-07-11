# TODO:

# change column class types appropriately in data cleaning section
# remonve as.character(), as.numeric() calls later on in script
# make a function to write out row to sheet, with zero-row handling

# add fx header for WriteOutput()

# remove parcel.df  param description from function headers
# remove parcel.df arguments from function calls 

# address "ifBtoL is null or 0, use state average for that usetype"

# NOTES: 
# - in the ConvertLot20 function, does this handle ranking within the 
  # Condo/MF case properly?

# - question about formatting of tabular data in 2020Rebuild sheet of output

# - question about Deactivated2020 sheet of output df
# - if fs diff is negative should vacated building statuses be changed to 
  # deactivated or vacant?

# -791 - should lotarea (and BArea) fields be changes with conversions?

# -when assigning EUI vintages, how should 2020 building be assigned?
  # - is there supposed to be a condition for these in the EUIVint tab?

# three letter county codes?

# statewide btl ratios table?




#-----------------------------------------------------------------------------#
# TLPointsInRange_fun.R

# Type: R function script

# AUTHOR:
# Tyler Huntington, 2017
# Lawrence Berkeley National Lab Sustainable Energy Group
# Project: California Building Turnover Study
# Principal Investigator: Hanna Breunig

# PURPOSE:
# A function to perform a network analysis to determine which counties are 
# within a specified range (either drive time or drive distance) of a focal 
# point using the TIGER/Line US primary and secondary road network.

# PARAMETERS:
# hubs.data - object of class SpatialPointsDataFrame with the points to use as
# "hubs," or start points of the network analysis. 
# E.g. biorefineries. 
# nodes.data - object of class SpatialPointsDataFrame containing the 
# destination points to be used in the network analsysis. 
# E.g. county centroids.
# constraint - a charater (either "time" or "distance") indicating type of 
# constraint to set for determining whether points are in range of hubs. 
# max.dist - numeric inidicating the drive distance (in miles) to use as an 
# upper bound for determining which points are within range of hubs.   
# max.time - numeric inidicating the drive time (in hours) to use as an 
# upper bound for determining which points are within range of hubs. 

# RETURNS:
# list of lists in which first element is RID and second is
# character vector of county names within specified range of refinery

#-----------------------------------------------------------------------------#

# # TEMP: set wd for local testing
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

# for gc compute
# setwd()

############ LOAD LIBRARIES############ 
# packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos",
#               "spatialEco", "geosphere", "doParallel", "iterators",
#               "foreach", "rgdal", "plyr")
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())))  
# }
# 
# library(ggplot2)
# library(ggmap)
# library(raster)
# library(sp)
# library(rgeos)
# library(spatialEco)
# library(geosphere)
# library(doParallel)
# library(iterators)
# library(foreach)
# library(rgdal)
# library(xlsx)
# library(plyr)

require(ggplot2)
require(ggmap)
require(raster)
require(sp)
require(rgeos)
require(spatialEco)
require(geosphere)
require(doParallel)
require(iterators)
require(foreach)
require(rgdal)
require(xlsx)
require(plyr)



############ DEFINE FUNCTIONS ############  
CalcEUIVint16 <- function(yr) {
  if ((yr < 1964) & (yr != 0)) {
    return ("EUI64")
  } else if ((yr < 1979) & (yr > 1963)) {
    return ("EUI75")
  } else if ((yr < 1984) & (yr > 1978)) {
    return ("EUI79")
  } else if ((yr < 1992) & (yr > 1983)) {
    return("EUI84")
  } else if ((yr < 1998) & (yr > 1991)) {
    return ("EUI92")
  } else if ((yr < 2001) & (yr > 1997)) {
    return("EUI98")
  } else if ((yr < 2005) & (yr > 2000)) { 
    return ("EUI01")
  } else if ((yr < 2008) & (yr > 2004)) {
    return ("EUI05")
  } else if ((yr < 2013) & (yr > 2007)) {
    return("EUI05")
  } else {
    return ("EUI13")
  }
}

# assign EUIVint codes
CalcEUIVint20 <- function(yr) {
  if ((yr < 1964) & (yr != 0)) {
    return ("EUI13")
  } else if ((yr < 1979) & (yr > 1963)) {
    return ("EUI13")
  } else if ((yr < 1984) & (yr > 1978)) {
    return ("EUI79")
  } else if ((yr < 1992) & (yr > 1983)) {
    return("EUI84")
  } else if ((yr < 1998) & (yr > 1991)) {
    return ("EUI92")
  } else if ((yr < 2001) & (yr > 1997)) {
    return("EUI98")
  } else if ((yr < 2005) & (yr > 2000)) { 
    return ("EUI01")
  } else if ((yr < 2008) & (yr > 2004)) {
    return ("EUI05")
  } else if ((yr < 2013) & (yr > 2007)) {
    return("EU08")
  } else {
    return ("EUI13")
  }
}

# assign EUIVint codes
CalcEUIVint50 <- function(yr) {
  if ((yr < 1964) & (yr != 0)) {
    return ("EUI13")
  } else if ((yr < 1979) & (yr > 1963)) {
    return ("EUI13")
  } else if ((yr < 1984) & (yr > 1978)) {
    return ("EUI13")
  } else if ((yr < 1992) & (yr > 1983)) {
    return("EUI13")
  } else if ((yr < 1998) & (yr > 1991)) {
    return ("EUI13")
  } else if ((yr < 2001) & (yr > 1997)) {
    return("EUI13")
  } else if ((yr < 2005) & (yr > 2000)) { 
    return ("EUI01")
  } else if ((yr < 2008) & (yr > 2004)) {
    return ("EUI05")
  } else if ((yr < 2013) & (yr > 2007)) {
    return("EUI08") 
  } else if (yr == 2050) {
    return ("EUI35")
  } else {
    return ("EUI13")
  }
}

# define function for turning a county's summary stats into a 1 row df
MakeRow <- function(summary.data, county.name, col.name, NAs.to.zeros = T) {
  
  rownames(summary.data) <- summary.data$TYPE
  row.df <- subset(summary.data, select = col.name)
  row.df <- data.frame(t(row.df))
  ncols <- ncol(row.df)
  row.df$County <- county.name
  row.df <- cbind("County" = row.df$County, row.df[1:ncols])
  
  # change NA vals to zeros
  if (NAs.to.zeros) {
    for (c in 1:ncol(row.df)) {
      if (is.na(row.df[1, c])) {
        row.df[1, c] <- 0
      }
    }
  }
  return(row.df)
}

MakeZeroRow <- function(column.names, county.name) {
  
  df <- data.frame(column.names)
  df$vals = 0
  rownames(df) <- df$column.names
  df <- subset(df, select = c("vals"))
  row.df <- data.frame(t(df))
  ncols <- ncol(row.df)
  row.df$County <- county.name
  row.df <- cbind("County" = row.df$County, row.df[1:ncols])
  
  return(row.df)
}

AssignEUIVint <- function(parcel.df, stock.year) {
  
  yr <- substr(stock.year, 3, 4)
  col.name <- "EUIVint"
  
  # determin which EUIVint calculator function to call
  fun.name <- paste0("CalcEUIVint", yr)
  
  # init vintage column in parcel data
  parcel.df[,col.name] <- NA
  
  # iterate over parcels, assigning EUIVint codes
  for (r in 1:nrow(parcel.df)) {
    par <- parcel.df[r,]
    print(sprintf("Assigning EUI Vintage to parcel with APN: %s", par$APN))
    yr <- par$Yrfinal
    vint <- get(fun.name)(yr)
    par[,col.name] <- vint
    parcel.df[r,] <- par
  }
  return(parcel.df)
}

WriteShapefile <- function(parcel.df, county.code, year, 
                           parcel.sp=cnt.raw.sp) {
  
  yr <- substr(year, 3, 4)
  
  # assign EUI vint codes
  parcel.df <- AssignEUIVint(parcel.df, year)
  
  # join EUIs to parcels
  eui.table <- paste0("eui.", yr)
  
  parcel.df$Join <- paste0(parcel.df$cz, 
                           parcel.df$Typef, 
                           parcel.df$EUIVint)
  
  parcel.df <- merge(parcel.df, get(eui.table), by = "Join", all.x = T)
  
  # set contents of BArea field to match year
  if (year == 2016) {
    barea.col <- "BArea"
  } else if (year == 2020) {
    barea.col <- "NewArea"
  } else if (year == 2050) {
    barea.col <- "FNArea.50"
  }
  
  parcel.df$BArea <- parcel.df[,barea.col]
  
  # subset for fields to include in shapefile attribute table
  drop <- c("NewArea", "FNArea.50")
  cols <- names(parcel.df)
  fields <- cols[!(cols %in% drop)]
  parcel.df <- subset(parcel.df, select = fields)
  
  # set up spatial object for export as .shp
  rownames(parcel.df) <- parcel.df$APN
  shp <- SpatialPolygonsDataFrame(parcel.sp, parcel.df)
  
  # set output file name and paths
  layer.name <- paste0(county.code, year)
  path <- paste0(layer.name, ".shp")
  
  # write out shapefile
  writeOGR(shp, dsn=path, layer=layer.name, 
           driver = "ESRI Shapefile", overwrite_layer = T)
}

# define function for writing row-wise county data to output tables
WriteOutput <- function(data.row, output.filename, sheet.name) {
  
  # check if the target sheet is blank
  readsheet.df <- read.xlsx(output.filename, sheetName=sheet.name)
  
  if (is.null(readsheet.df)) {
    
    # write to sheet, including column names
    output.wb <- loadWorkbook(output.filename)
    wb.sheets <- getSheets(output.wb)
    write.sheet <- wb.sheets[[sheet.name]]
    addDataFrame(data.row, write.sheet, row.names=F, col.names=T)
    saveWorkbook(output.wb, output.filename)
    
  } else {
    # determine next empty row for writing to
    write.row <- nrow(readsheet.df) + 2
    
    # write to sheet, excluding column names
    output.wb <- loadWorkbook(output.filename)
    wb.sheets <- getSheets(output.wb)
    write.sheet <- wb.sheets[[sheet.name]]
    addDataFrame(data.row, write.sheet, startRow=write.row, 
                 row.names=F, col.names=F)
    saveWorkbook(output.wb, output.filename)
  }
}


############ SOURCE REQUIRED FUNCTIONS ######


############ SET GLOBAL VARS ######
aea.crs <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0
               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 
               +units=m +no_defs")

wgs84.crs <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

## Init output excel file for exporting intermediate tables
output.wb <- createWorkbook()
output.filename <- "building_turnover_output_tables.xlsx"

# init sheets for 2020
sheet1 <- createSheet(output.wb, "2016Stock")
sheet2 <- createSheet(output.wb, "2020Projected")
sheet3 <- createSheet(output.wb, "2020Remain")
sheet4 <- createSheet(output.wb, "2020Demo")
sheet5 <- createSheet(output.wb, "2020LotsOpen")
sheet6 <- createSheet(output.wb, "2020Diff")
sheet7 <- createSheet(output.wb, "2020DemoMult")
sheet8 <- createSheet(output.wb, "2020Rebuild")
sheet9 <- createSheet(output.wb, "2020LotsFilled")
sheet10 <- createSheet(output.wb, "2020VacantFilled")
sheet11 <- createSheet(output.wb, "2020Deactivated")
sheet12 <- createSheet(output.wb, "2016BtoL")
sheet13 <- createSheet(output.wb, "2020UnmetFloorspace")
sheet14 <- createSheet(output.wb, "2020Stock")

# init sheets for 2050
sheet15 <- createSheet(output.wb, "2050Projected")
sheet16 <- createSheet(output.wb, "2050Remain")
sheet17 <- createSheet(output.wb, "2050Demo")
sheet18 <- createSheet(output.wb, "2050LotsOpen")
sheet19 <- createSheet(output.wb, "2050Diff")
sheet20 <- createSheet(output.wb, "2050DemoMult")
sheet21 <- createSheet(output.wb, "2050Rebuild")
sheet22 <- createSheet(output.wb, "2050LotsFilled")
sheet23 <- createSheet(output.wb, "2050VacantFilled")
sheet24 <- createSheet(output.wb, "2050Deactivated")
sheet25 <- createSheet(output.wb, "2020BtoL")
sheet26 <- createSheet(output.wb, "2050UnmetFloorspace")
sheet26 <- createSheet(output.wb, "2050Stock")

saveWorkbook(output.wb, output.filename)

# init list of dfs for outputting building stock data in 2016, 2020 and 2050
output.stock.df.lst <- vector("list", 3)


# ############ LOAD DATA############
# 
# # set name of data file with sheets containing required tables and input data
# input.datafile <- "../data/Steps_BuildingStockTurnover_7_6_TH.xlsx"
# 
# # load cutoff yrs table
# cutoff.yrs <- read.xlsx(file = input.datafile,
#                        sheetIndex = 2)
# 
# ## Load multiplier tables
# # 2020
# g.mult.20 <- read.xlsx(file = input.datafile,
#                        sheetName = "2020growthmult", colClasses = "character")
# 
# # 2050
# g.mult.50 <- read.xlsx(file = input.datafile,
#                        sheetName = "2050growthmult", colClasses = "character")
# 
# ## Load EUI tables
# #2016
# eui.16 <- read.xlsx(file = input.datafile,
#                        sheetName = "EUI2016", colClasses = "character")
# # 2020
# eui.20 <- read.xlsx(file = input.datafile,
#                        sheetName = "EUI2020", colClasses = "character")
# # 2050
# eui.50 <- read.xlsx(file = input.datafile,
#                        sheetName = "EUI2050", colClasses = "character")
# 
# # load county codes
# county.codes.df <- read.xlsx(file = input.datafile, sheetName = "CountyCodes",
#                              colClasses = "character")
# 
# ## load county parcel data
# 
# # TODO:
# # automatically load all shapfiles in dir
# # extract county name from County col of attribute table
# 

# # load Alameda county parcel data
# in.shape.layer <- "ALA2016"
# in.shape.path <- paste0("../data/", in.shape.layer, ".shp")
# cnt.final <- readOGR(dsn = in.shape.path, layer = in.shape.layer)
# cnt.final.df <- cnt.final@data
# crs <- crs(cnt.final)
# 
# # get this county's three letter code
# cnt.code <- substr(in.shape.layer, 1, 3)


############ CLEAN DATA ######

# get county name
cnt.name <- unique(as.character(cnt.final.df$County))


# TEMP: test a subset of the parcel data
percent.data <- 0.5
pars.to.test <- sample(1:115465, (115465*(percent.data/100.0)))

# # TEMP: test all parcels of a particular usetype
# pars.to.test <- which(cnt.final$TYPE == "Food Sale")

# TEMP: total rows in parcel data: 115465

## County codes data
# set col classes to character
county.codes.df$county.name <- as.character(county.codes.df$county.name)
county.codes.df$county.code <- as.character(county.codes.df$county.code)

# extract list of county names
ca.cnties <- county.codes.df$county.name

## Cutoff yrs table
# change col class to character
cutoff.yrs$TYPE <- as.character(cutoff.yrs$TYPE)

# change type names for consistency
cutoff.yrs$TYPE[cutoff.yrs$TYPE == "Food Sale"] <- "Food.Sale"
cutoff.yrs$TYPE[cutoff.yrs$TYPE == "Food Srv"] <- "Food.Srv"
cutoff.yrs$TYPE[cutoff.yrs$TYPE == "Comm Misc"] <- "Comm.Misc"
cutoff.yrs$TYPE[cutoff.yrs$TYPE == "Heath"] <- "Health"

# change col names
names(cutoff.yrs) <- c("TYPE", "Cut20", "Cut50")

## Multiplier tables
# change class of county col to class to character
g.mult.20$County <- as.character(g.mult.20$County)
g.mult.50$County <- as.character(g.mult.50$County)

# elim whitespace chars from county names
g.mult.20$County <- trimws(g.mult.20$County)
g.mult.50$County <- trimws(g.mult.50$County)

# change class of numeric cols to numeric
for (col in 2:ncol(g.mult.20)) { 
  g.mult.20[, col] <- as.numeric(g.mult.20[, col])
}

for (col in 2:ncol(g.mult.50)) { 
  g.mult.50[, col] <- as.numeric(g.mult.50[, col])
}

# change building type names in multiplier tables for consistency
type.names <- names(g.mult.20)

# EUI tables
eui.16$Join <- as.character(paste(eui.16$Join))
eui.20$Join <- as.character(paste(eui.20$Join))
eui.50$Join <- as.character(paste(eui.50$Join))

## Parcel data
# subset for original parcel data + fields added during preprocessing steps
cnt.raw <- cnt.final[, c(1:32, 37)]


# TEMP: subset for first 1000 parcels for faster testing
cnt.raw <- cnt.raw[pars.to.test,]

# extract attribute data from sp parcel data
cnt.raw.df <- cnt.raw@data

# change FIDs to APN codes
cnt.raw <- spChFIDs(cnt.raw, as.character(cnt.raw$APN))

# extract and spatialize polygons data part from cnt.raw
cnt.raw.polys <- cnt.raw@polygons
cnt.raw.sp <- SpatialPolygons(cnt.raw.polys, proj4string = crs)

# change column classes
cnt.raw.df$TYPE <- as.character(cnt.raw.df$TYPE)
cnt.raw.df$Yrfinal <- as.character(cnt.raw.df$Yrfinal)
cnt.raw.df$LOTAREA <- as.numeric(cnt.raw.df$LOTAREA)
cnt.raw.df$APN <- as.character(paste(cnt.raw.df$APN))
cnt.raw.df$Typef <- as.character(cnt.raw.df$Typef)

# change building type names in parcel data for consistency
cnt.raw.df$TYPE[cnt.raw.df$TYPE == "Food Srv"] <- "Food.Srv"
cnt.raw.df$TYPE[cnt.raw.df$TYPE == "Food Sale"] <- "Food.Sale"
cnt.raw.df$TYPE[cnt.raw.df$TYPE == "Comm Misc"] <- "Comm.Misc"
cnt.raw.df$TYPE[cnt.raw.df$TYPE == "college"] <- "College"




## Step 1. determine 2016 building stock by building type
cnt.sum.16 <- ddply(cnt.raw.df, .(TYPE), .drop = F,
                    summarise, SUM.FS.16 = sum(BArea))

# add 2016 stock df to output.stock.dfs
output.stock.df.lst[[1]] <- cnt.raw.df

# write output shapefile for 2016 building stock
WriteShapefile(cnt.raw.df, cnt.code, 2016)

# organize particular county's data for export
cnt.row <- MakeRow(cnt.sum.16, cnt.name, "SUM.FS.16")

# write to output file
WriteOutput(cnt.row, output.filename, sheet.name = "2016Stock")

## Step 2. project 2020 building stock
# init projected floorspace df
proj.df <- data.frame(TYPE = as.character(unique(cnt.raw.df$TYPE)), 
                      stringsAsFactors = F)

# susbet growth multipliers for particular county
g.mult.20.cnt <- g.mult.20[g.mult.20$County == cnt.name, ]

# transpose and organize multiplier dataframe 
g.mult.20.cnt <- data.frame(t(g.mult.20.cnt))
g.mult.20.cnt$TYPE <- rownames(g.mult.20.cnt)
g.mult.20.cnt <- g.mult.20.cnt[2:nrow(g.mult.20.cnt), ]
names(g.mult.20.cnt) <- c("MULT.20", "TYPE")
rownames(g.mult.20.cnt) <- seq(1, nrow(g.mult.20.cnt))

# merge multipliers to parcel data
cnt.sum.16 <- merge(cnt.sum.16, g.mult.20.cnt, by = "TYPE", all.x = T)
cnt.sum.16$SUM.FS.16 <- as.numeric(cnt.sum.16$SUM.FS.16)
cnt.sum.16$MULT.20 <- as.numeric(as.character(cnt.sum.16$MULT.20))

# multiply 2016 stocks by 2020 multipliers
cnt.sum.16$PROJ.FS.20 <- NA
cnt.sum.16$PROJ.FS.20 <- cnt.sum.16$SUM.FS.16 * cnt.sum.16$MULT.20
PROJ.FS.20.col <- cnt.sum.16[,c("TYPE", "PROJ.FS.20")]

# merge 2020 projections with projections dataframe
proj.df <- merge(proj.df, PROJ.FS.20.col, by="TYPE", all.x=T)


# organize particular county's data for export
cnt.row <- MakeRow(cnt.sum.16, cnt.name, "PROJ.FS.20")

# write to output file
WriteOutput(cnt.row, output.filename, sheet.name = "2020Projected")

## Step 3. determine if demolished by Years 2020 and 2050

# join 2020cutoff table by usetype
cutoffs.20 <- cutoff.yrs[, c("TYPE", "Cut20"),]
cnt.raw.df <- merge(cnt.raw.df, cutoffs.20, by = "TYPE", all.x = T)

# add bin and cut fields
cnt.raw.df$Bin20 <- NA
cnt.raw.df$Bin50 <- NA

# def function for checking if a building is demolished (0) or remains (1)
CheckIfRemains <- function(yr, cut) {
  if ((yr < cut) & (yr != 0)) {
    return (0)
  } else {
    return (1)
  }
}

# iterate over all parcels
pass <- 1
for (p in 1:nrow(cnt.raw.df)) {
  par <- cnt.raw.df[p,]
  print(sprintf("Pass: %s Checking parcel with APN: %s", pass, par$APN))
  yr <- as.numeric(as.character(par$Yrfinal))
  cut <- as.numeric(as.character(par$Cut20))
  par$Bin20 <- CheckIfRemains(yr, cut)
  cnt.raw.df[p,] <- par
  pass <- pass + 1
}


## Step 4. assign 2020 status labels to parcels

# init col to store 2020 status for each parcel
cnt.raw.df$Status20 <- NA

# define function for assigning 2020 status
AssignStatus <- function(BinYEAR, BAREA, LOTAREA, TYPE) {
  
  # handle NAs in LOTAREA argument
  if (is.na(LOTAREA)) {
    LOTAREA <- 0
  }
  
  # decision tree for assignments
  if ((BinYEAR == 0) & (BAREA != 0)) {
    return("Demo")
  } else if ((BinYEAR == 1) & (BAREA != 0)) {
    return("Remain")
  } else if ((BAREA == 0) & (LOTAREA != 0) & (TYPE != "Vacant")) {
    return("LotsOpen")
  } else if (((BAREA != 0) | (LOTAREA != 0)) & (TYPE == "Vacant")) {
    return("Vacant")
  }
}

           

# iterate over parcels, assigning 2020 statuses
print("Determining status of parcels in 2020...")
pass <- 1
for (p in 1:nrow(cnt.raw.df)) {
  par <- cnt.raw.df[p,]
  print(sprintf("Pass: %s Checking parcel with APN: %s", pass, par$APN))
  BinYEAR <- as.numeric(as.character(par$Bin20))
  BAREA <- as.numeric(as.character(par$BArea))
  LOTAREA <- as.numeric(as.character(par$LOTAREA))
  TYPE <- as.character(par$TYPE)
  par$Status20 <- AssignStatus(BinYEAR, BAREA, LOTAREA, TYPE)
  cnt.raw.df[p,] <- par
  pass <- pass + 1
}

## Step 5: Generate summary stats for parcels 2020, by Status20

cnt.sum.20 <- ddply(cnt.raw.df, .(TYPE, Status20), .drop = F,
                    summarise, SUM.FS.20 = sum(BArea), 
                    SUM.LOT.20 = sum (LOTAREA))


## Export summary table of remaining buildings in 2020
remain.20 <- cnt.sum.20[cnt.sum.20$Status20 == "Remain", ] 
cnt.row <- MakeRow(remain.20, cnt.name, "SUM.FS.20")
WriteOutput(cnt.row, output.filename, sheet.name = "2020Remain")


## Export summary table of demo buildings in 2020
demo.20 <- cnt.sum.20[cnt.sum.20$Status20 == "Demo", ] 
cnt.row <- MakeRow(demo.20, cnt.name, "SUM.FS.20")
WriteOutput(cnt.row, output.filename, sheet.name = "2020Demo")


## Export summary table of open lots  in 2020
lots.20 <- cnt.sum.20[cnt.sum.20$Status20 == "LotsOpen", ] 
cnt.row <- MakeRow(lots.20, cnt.name, "SUM.LOT.20")
WriteOutput(cnt.row, output.filename, sheet.name = "2020LotsOpen")

# determine additional floorspace needs for 2020
yr16.data <- subset(cnt.sum.16, select = c("TYPE", "SUM.FS.16", "PROJ.FS.20"))
yr20.data <- cnt.sum.20[cnt.sum.20$Status20 == "Remain", ]
fs.diffs.df <- merge(yr16.data, yr20.data, by = "TYPE")

# calculate differences between remaining and projected floorspace by type
fs.diffs.df$DIFF <- fs.diffs.df$PROJ.FS.20 - fs.diffs.df$SUM.FS.20

## Export summary table of open lots  in 2020
cnt.row <- MakeRow(fs.diffs.df, cnt.name, "DIFF")
WriteOutput(cnt.row, output.filename, sheet.name = "2020Diff")



# define functions for simulations

RandomSwitch <- function(parcel.df, from.type, 
                           from.status, to.type, to.status, status.year) {
#-----------------------------------------------------------------------------#
  # PURPOSE: a function that randomly assigns the type and status 
  # of a building of a user-specified original type and status to "Vacant"
  
  # PARAMS:
  # parcel.df - a dataframe of parcel data for the county of interest
  # from.type - a string specifying the type of building to randomly 
    # select from
  # from.status - a string specifying the status of building 
    # to randomly sel from
  # to.type - bldg type to which random selection should be switched
  # to.status - 2020 bldg status to which random selection should be switched
  # status.year - the year for which the parcel's status should be switched
  
  # RETURNS: a list with 2 elements:
  #   1. the updated parcel.df passed to the function by the caller
  #   2. the APN code of the parcel that was switched
  
  
  # SIDE-EFFECTS: changes status and type of a randomly selected parcel from 
  # the parcel.df that meets supplied criteria to "Vacant"
#-----------------------------------------------------------------------------#
  
  yr <- substr(status.year, 3, 4)
  status.col <- paste0("Status", yr)
  
  # init parcel pool to select from
  pool <- subset(parcel.df, 
                 (parcel.df[,"TYPE"] == from.type) &
                 (parcel.df[,status.col] == from.status))
  
  # randomly select parcel from pool by apn code
  rand.apn <- sample(pool$APN, 1)
  
  # change type and status of randomly selected parcel
  sel.par <- pool[pool$APN == as.character(rand.apn),]
  sel.par[,"TYPE"] <- to.type
  sel.par[,status.col] <- to.status
  parcel.df[parcel.df$APN == as.character(rand.apn),] <- sel.par
  
  res.list <- list(parcel.df, rand.apn)
  return(res.list)
}
#-----------------------------------------------------------------------------#


CheckDiff <- function(parcel.df, type, status, year, area.col) {
#-----------------------------------------------------------------------------#
  # PURPOSE: a function that checks the difference between the projected and 
  # actual (i.e. remaining on the basis of cutoff year table) floorspace of
  # a specified building type.
  
  # PARAMS:
  # parcel.df - a dataframe of parcel data for the county of interest
  # type - a string specifying the building type of interest
  # status - a string or vector of strings specifying the 2020 bldg 
    # status(es) of interest
  # year- the year for which actual/projected floorspace diffs should be 
    # calculated
  # area.col - string of the column name in parcel.df that contains actual 
    # areas to be subtracted from projected areas to calculate differences
  
  
  # RETURNS: the difference of (projected_floorspace - remaining_floorspace) 
  # for buildings of the specified type
  
  # SIDE-EFFECTS: None
#-----------------------------------------------------------------------------#
  yr <- substr(as.character(year), 3, 4)
  status.col <- paste0("Status", yr)
  proj.col <- paste0("PROJ.FS.", yr)
  
  
  # subset for parcels of specified type
  t.pars <- subset(parcel.df, 
                   (parcel.df$TYPE == type) & 
                     (parcel.df[,c(status.col)] %in% status))
  
  # sum remaining floorspace
  sum.fs <- sum(na.omit(t.pars[(t.pars[,status.col] == status),][,area.col]))
  
  # fetch projected floorspace
  proj.fs <- proj.df[(proj.df[,"TYPE"] == type), ][,proj.col]
  
  # calc difference
  diff <- (proj.fs - sum.fs)
                   
  return(diff)
}

#-----------------------------------------------------------------------------#


RebuildMultiplied <- function(parcel.df, type, from.status, to.status, mult,
                              year, from.area.col, to.area.col) {
#-----------------------------------------------------------------------------#
  # PURPOSE: a function that multiplies the indicated area field of parcels of a 
  # specified type and status by a user-supplied multiplier.
  
  # PARAMS:
  #  parcel.df - a dataframe of parcel data for the county of interest
  #  type - a string specifying the type of building to be subsetted
  #  from.status - string specifying the status of buildings to be subsetted
  #  to.status - string specifying the output status of parcels whose bldg
    # area has been multiplied bu the function. 
  #  mult - the multiplier, as a numeric value 
  
  # RETURNS: the parcel.df with bldg area vals updated of parcels specified by
  # arguments passed to caller. 
  
  # SIDE-EFFECTS: None
#-----------------------------------------------------------------------------# 
  yr <- substr(as.character(year), 3, 4)
  status.col <- paste0("Status", yr)
  
  # subset parcel data for specified bldg type
  t.pars <- parcel.df[((parcel.df[,"TYPE"] == type) & 
                         (parcel.df[,status.col] == from.status)),]
  
  # if parcels exist given the criteria, apply multiplier
  if (nrow(t.pars) > 0) {
    
    # apply multiplier
    t.pars[,to.area.col] <- t.pars[,from.area.col] * mult
    
    # update Yrfinal and status fields
    t.pars$Yrfinal <- as.character(year)
    t.pars[,status.col] <- to.status
    
    # update Bin50 field if rebuilds occurrin in 2020
    if(year == 2020) {
      t.pars$Bin50 <- "1"
    }
    
    # update values in parcel df
    parcel.df[((parcel.df[,"TYPE"] == type) & 
                 (parcel.df[,status.col] == from.status)),] <- t.pars
    
    return(parcel.df)
  
  } else {
    return ("None")
  }
}
#-----------------------------------------------------------------------------#

SizeRankedSwitch <- function(parcel.df, from.type, from.status, 
                               to.type, to.status, year, area.col) {
#-----------------------------------------------------------------------------#
  # PURPOSE: a function that changes the type and status of parcels of a 
  # user specified type and status to new vals for these fields. The parcel 
  # within the candidate pool with the larges floorspace is switched. 
  
  # PARAMS:
  # parcel.df - a dataframe of parcel data for the county of interest
  # from.type - a string specifying the type of building to randomly 
  # select from
  # from.status - a string specifying the status of building 
  # to randomly sel from
  # to.type - bldg type to which random selection should be switched
  # to.status - 2020 bldg status to which random selection should be switched
  
  # RETURNS: APN code of building that was switched
  
  # SIDE-EFFECTS: changes status and type of a randomly selected parcel from 
  # the parcel.df that meets supplied criteria to "Vacant"
#-----------------------------------------------------------------------------#
  yr <- substr(as.character(year), 3, 4)
  status.col <- paste0("Status", yr)
  
  # init parcel pool to select from
  pool <- subset(parcel.df, 
                 (parcel.df[,"TYPE"] == from.type) &
                   (parcel.df[,status.col] == from.status))
  
  # find the parcel with the largest floorspace area
  lar.par <- which(pool[,area.col] == max(pool[,area.col]))
  sel.par <- pool[lar.par,]
  sel.apn <- sel.par$APN
  
  # change type and status of randomly selected parcel
  sel.par$TYPE <- to.type
  sel.par[,status.col] <- to.status
  
  # update master parcel df
  parcel.df[(parcel.df$APN == sel.apn), ] <- sel.par

  # prep result list to return
  res.list <- list(parcel.df, sel.par$APN)
  
  return(res.list)
}
#-----------------------------------------------------------------------------#

ExpandArea <- function(parcel.df, apn, expand.factor,
                       from.area.col, to.area.col) {
#-----------------------------------------------------------------------------#
  # PURPOSE: a function that increases the new floorspace of a rebuild by a 
  # specified expansion factor. The expansion factor is multiplied by the 
  # previous (demo-ed) building's area (BArea field of the parcel).
  
  # PARAMS:
  # parcel.df - a dataframe of parcel data for the county of interest
  # apn - a char string indicating the APN code of the building to expand 
  # expand.factor - numeric value by which the bldg area should be multiplied
  # from.area.col - a character string indicating the name of the area column 
    # that contains the base area that is to be multiplied
  # to.area.col - a character string indicating the name of the area column 
    # that the new (expanded) area should be slotted into
  
  # RETURNS: parcel.df with updated parcel data as specified by other arguments
  
  # SIDE-EFFECTS: updates the NewArea field of a bldg to the product of the
  # parcel's BArea field and the supplied expansion factor. 
#-----------------------------------------------------------------------------#
  
  # get demo'd bldg area
  orig.area <-  parcel.df[parcel.df$APN == apn, ][,from.area.col]
  
  # calc expanded area of rebuild
  new.area <- orig.area * expand.factor
  
  # update NewArea field of parcel df
  parcel.df[parcel.df$APN == apn, ][,to.area.col] <- new.area
  
  return(parcel.df)
  
}
#-----------------------------------------------------------------------------#

CalcBtlRatio <- function(parcel.df, type, barea.col) {
#-----------------------------------------------------------------------------#
  # PURPOSE: calculate the bldg area to lot area ratio of a particular usetype,
  # within a specific county. Omits outliers and zeros in calulation of mean.
  
  # PARAMS:
  # parcel.df - a dataframe of parcel data for the county of interest
  # type - the parcel type for which BtoL ratios are to be calculated
  # area.col - character string specifying the name of the column containing 
    # building area vals to use in calculating BtoL ratio
  
  
  # RETURNS: APN code of building that was switched
  
  # SIDE-EFFECTS: None
#-----------------------------------------------------------------------------#
  
  # init parcel pool to select from
  pool <- subset(parcel.df, (parcel.df$TYPE == type))
  
  # get barea and lotarea vals
  barea.vals <- pool[,barea.col]
  lotarea.vals <- pool$LOTAREA 
  
  # make dataframe
  btl.df <- data.frame(barea.vals, lotarea.vals)
  
  # init rm.rows
  rm.rows <- c(NULL)
  
  # elim parcels for which lotarea or barea is 0 or NA
  for(r in 1:nrow(btl.df)) {
    par <- btl.df[r,]
    if ((is.na(par$barea.vals)) | (par$barea.vals == 0) |
        (is.na(par$lotarea.vals)) | (par$lotarea.vals == 0)) {
      rm.rows <- c(rm.rows, r)
    }
  }
  btl.df <- btl.df[!(1:nrow(btl.df) %in% rm.rows), ]
  
  # calc btl vals
  btl.df$btl.ratio <- (btl.df$barea.vals/btl.df$lotarea.vals)
  
  # identify outliers
  iqr <- IQR(btl.df$btl.ratio)
  first.quant <- quantile(btl.df$btl.ratio)[2]
  third.quant <- quantile(btl.df$btl.ratio)[4]
  lower <- first.quant - 1.5 * iqr
  upper <- third.quant + 1.5 * iqr
  
  ratios <- as.numeric(btl.df$btl.ratio)
  non.outlier.ratios <- ratios[which((ratios >= lower) & (ratios <= upper))]
  
  # calc mean of non-outlier btl ratios
  mean.btl <- mean(non.outlier.ratios)
  
  return(mean.btl)
}
#-----------------------------------------------------------------------------#
  
ConvertLot <- function(parcel.df, type, btl.ratio, year, 
                       from.area.col, to.area.col) {
#-----------------------------------------------------------------------------#
  # PURPOSE: a function that converts lot area to building area of a parcel 
  # with status "LotsOpen". The parcel within the candidate
  # pool with the most complete data in its build year, units, buildings, and
  # garage fields is selected for the lot conversion. If multiple buildings are
  # are equally ranked for data completeness, a random parcel is selected from
  # this subgroup. The status of the converted parcel is changed to 
  # "LotsFilled" for the year specified by the caller.
  
  # PARAMS:
  # parcel.df - a dataframe of parcel data for the county of interest
  # btl.ratio - the max post-conversion bldg area to lot area ratio 
  # type - a string specifying the type of building stock to pull from
  # year -
  # to.area.col -
  
  # RETURNS: APN code of parcel in which lot area was converted to bldg area
    # if no lots were able to be converted, returns "None"
  
  # SIDE-EFFECTS: updates the LOTAREA and BArea fields in the input parcel df
  # of the parcel that is selected via the ranked/randomized selection step. 
#-----------------------------------------------------------------------------#
  
  yr <- substr(as.character(year), 3, 4)
  status.col <- paste0("Status", yr)
  
  ## init parcel pool to select from
  # check if usetype is "Condo" in which case, include MF parcels in pool
  if (type == "Condo") {
    pool <- subset(parcel.df, 
                   ((parcel.df$TYPE == type) | (parcel.df$TYPE == "MF")) &
                     (parcel.df[,status.col] == "LotsOpen"))
  } else {
    pool <- subset(parcel.df, 
                   (parcel.df$TYPE == type) &
                     (parcel.df[,status.col] == "LotsOpen"))
  }
  
  ## score parcels in pool by data availability
  # init data score field
  pool$dc.score <- NA
  
  # iterate over parcels in pool, scoring each on its data completeness
  for (p in 1:nrow(pool)) {
    
    # select parcel
    par <- pool[p, ]
    
    # init data score var for this parcel
    p.score <- 0
    
    if ((par$Yrbld != 0) & (!is.na(par$Yrbld))) {
      p.score <- p.score + 1
    }
    if ((par$UNITS != 0) & (!is.na(par$UNITS))) {
      p.score <- p.score + 1
    }
    if ((par$BLDGS != 0) & (!is.na(par$BLDGS))) {
      p.score <- p.score + 1
    }
    if ((par$GARSIZE != 0) & (!is.na(par$GARSIZE))) {
      p.score <- p.score + 1
    }
    
    # if usetype is "Condo", penalize "MF" parcels so they all rank lower
    # than Condo parcels and therefore will only be used if Condos run out
    if ((type == "Condo") & par$TYPE == "Condo") {
      p.score <- p.score + 5
    }
    
    # update data completeness score field of parcel df
    par$dc.score <- p.score
    
    # slot row back into pool.df
    pool[p, ] <- par
  }
  
  ## select highest ranked parcel
  top.scoring.pars <- which(pool$dc.score == max(pool$dc.score))
  sel.par <- pool[sample(top.scoring.pars, 1), ]
  
  # get apn of selected parcel
  sel.par.apn <- sel.par$APN
  
  ## determine current btl ratio of selected parcel
  if (year == 2020) {
    barea <- "BArea"
  } else {
    barea <- "NewArea"
  }
  
  btl.par <- CalcBtlRatio(sel.par, type = sel.par$TYPE, barea.col=barea)
  
  # case if no valid btl calculated due to NA or zero val for bldg area
  if (is.na(btl.par)) {
    
    # calculate how much additional lot area can be converted to bldg area
    avail.conv.area <- (sel.par$LOTAREA * btl.ratio)
    
    # allocate allowable lot area to bldg area
    sel.par[,to.area.col] <- (sel.par[,from.area.col] + avail.conv.area)
    
    # subtract converted lot area from LOTAREA field ???
    sel.par$LOTAREA <- (sel.par$LOTAREA - avail.conv.area)
    
  } else {
    
    # check that parcel's btl ratio does not already exceed county-usetype mean
    if (btl.par < btl.ratio) {
        
      # calculate how much additional lot area can be converted to bldg area
      avail.area <- (sel.par$LOTAREA * btl.ratio) - (sel.par$LOTAREA * btl.par)
      
      # allocate allowable lot area to bldg area
      sel.par[,to.area.col] <- (sel.par[,from.area.col] + avail.conv.area)
      
      # update lotarea field
      sel.par$LOTAREA <- (sel.par$LOTAREA - avail.conv.area)
      
      # change type if MF lot area was converted to fill Condo floorspace gap
      if ((sel.par$TYPE == "MF") & (type == "Condo")) {
        sel.par$TYPE <- "Condo"
      }
      
    } else {
      return("None")
    }
  }
  
  # update relevant fields field of selected parcel
  sel.par[,to.area.col] <- sel.par[,from.area.col]
  sel.par[,status.col] <- "LotsFilled"
  
  # update row of selected parcel in master parcel df
  parcel.df[parcel.df$APN == sel.par.apn, ] <- sel.par
  
  # prep result list to return
  res.list <- list(parcel.df, sel.par.apn)
  
  return(res.list)
}

#-----------------------------------------------------------------------------#

FillVacant <- function(parcel.df, to.type, btl.ratio, year) {
#-----------------------------------------------------------------------------#
  # PURPOSE: a function that randomly assigns the type and status 
  # of a building of a user-specified original type and status to "Vacant"
  
  # PARAMS:
  # from.type - a string specifying the type of building to randomly 
    # select from
  # to.type
  # btl.ratio
  # year - numeric indicating the year in which parcels of status "Vacant" 
    # should be sourced
  
  # RETURNS: the APN code of the parcel that was switched
  
  # SIDE-EFFECTS: changes status and type of a randomly selected parcel from 
  # the parcel.df that meets supplied criteria to "Vacant"
#-----------------------------------------------------------------------------#
  
  yr <- substr(as.character(year), 3, 4)
  status.col <- paste0("Status", yr)
  
  if (year == 2020) {
    from.barea.col <- "BArea"
    to.barea.col <- "NewArea"
  } else {
    from.barea.col <- "NewArea"
    to.barea.col <- "FNArea.50"
  }
    
  # init parcel pool to select from
  pool <- subset(parcel.df, 
                 (parcel.df$TYPE == "Vacant"))
  
  # rank pool of vacant parcels in order of conversion priority
  pool$conv.ord <- 3
  
  if (nrow(pool[((pool$BArea != 0) & 
                 (pool[,status.col] == "Remain")), ]) > 0) {
    
    pool[((pool[,from.barea.col] != 0) & 
            (pool[,status.col]  == "Remain")), ]$conv.ord <- 1
  
  } else if (nrow(pool[((pool$BArea != 0) & 
                      (pool[,status.col] != "Remain")), ]) > 0) {
    
    pool[((pool[,from.barea.col] != 0) & 
            (pool[,status.col] != "Remain")), ]$conv.ord <- 2
  
  } else if (nrow(pool[(pool[,from.barea.col] == 0), ]) > 0) {
    
    pool[(pool[,from.barea.col] == 0), ]$conv.ord <- 3
    
  }
  
  ## randomly select parcel to convert in order of converstion priority
  # check first priority parcels
  pool.1 <- pool[(pool$conv.ord == 1), ]
  pool.2 <- pool[(pool$conv.ord == 2), ]
  pool.3 <- pool[(pool$conv.ord == 3), ]
  
  if (nrow(pool.1) > 0) {
    rand.apn <- sample(pool.1$APN, 1)
  } else if (nrow(pool.2) > 0) {
    rand.apn <- sample(pool.2$APN, 1)
  } else if (nrow(pool.3 > 0)) {
    rand.apn <- sample(pool.3$APN, 1)
    
    # get parcel of randomly chosen APN
    sel.par <- parcel.df[which(parcel.df$APN == rand.apn), ]
    
    ## determine current btl ratio of selected parcel
    btl.par <- CalcBtlRatio(sel.par, type = sel.par$TYPE, 
                            barea.col=from.barea.col)
    
    # case if no valid btl calculated due to NA or zero val for bldg area
    if (is.na(btl.par)) {
      
      # calculate how much additional lot area can be converted to bldg area
      avail.conv.area <- (sel.par$LOTAREA * btl.ratio)
      
      # allocate allowable lot area to bldg area
      sel.par[,to.barea.col] <- (sel.par[,to.barea.col] + avail.conv.area)
      
      # update lotarea field
      sel.par$LOTAREA <- (sel.par$LOTAREA - avail.conv.area)
      
      # update Yrfinal 
      sel.par$Yrfinal <- year
      
      # if vacancy filled in 2020, update Bin50 field
      if (year == 2020) {
        sel.par$Bin50 <- 1
      }
      
    } else {
      # case if no valid btl calculated due to NA or zero val for bldg area
      if (is.na(btl.par)) {
        
        # calculate how much additional lot area can be converted to bldg area
        avail.conv.area <- (sel.par$LOTAREA * btl.ratio)
        
        # allocate allowable lot area to bldg area
        sel.par[,to.barea.col] <- (sel.par[,from.barea.col] + avail.conv.area)
        
        # update lotarea field
        sel.par$LOTAREA <- (sel.par$LOTAREA - avail.conv.area)
        
        # update Yrfinal 
        sel.par$Yrfinal <- year
        
        # if vacancy filled in 2020, update Bin50 field
        if (year == 2020) {
          sel.par$Bin50 <- 1
        }
        
        # check that parcel's btl ratio does not already exceed usetype mean
      } else if (btl.par < btl.ratio) {
          
          avail.area <- 
            (sel.par$LOTAREA * btl.ratio) - (sel.par$LOTAREA * btl.par)
          
          # allocate allowable lot area to bldg area
          sel.par[,to.barea.col] <- (sel.par[,from.barea.col] + avail.conv.area)
          
          # update lotarea field
          sel.par$LOTAREA <- (sel.par$LOTAREA - avail.conv.area)
          
          # update Yrfinal 
          sel.par$Yrfinal <- year
          
          # if vacancy filled in 2020, update Bin50 field
          if (year == 2020) {
            sel.par$Bin50 <- 1
          }
      } else {
        return("None")
      }
    }
    
    # update type and status fields
    sel.par$TYPE <- to.type
    sel.par[,status.col] <- "VacantFilled"
    
    # update master parcel df
    parcel.df[parcel.df$APN == as.character(rand.apn), ] <- sel.par
    
    # prep result list to return
    res.list <- list(parcel.df, rand.apn)
    return(res.list)
    
  } else {
    return ("None")
  }
  
  # get parcel of randomly chosen APN
  sel.par <- parcel.df[which(parcel.df$APN == rand.apn), ]
  
  # change type and status of randomly selected parcel
  sel.par[,to.barea.col] <- sel.par[,from.barea.col]
  sel.par <- pool[pool$APN == as.character(rand.apn), ]
  sel.par$TYPE <- to.type
  sel.par$Status20 <- "VacantFilled"
  
  # update master parcel df
  parcel.df[parcel.df$APN == as.character(rand.apn), ] <- sel.par
  
  # prep result list to return
  res.list <- list(parcel.df, rand.apn)
  return(res.list)
}
  
#-----------------------------------------------------------------------------#


# create field for new floorspace area in rebuilds
cnt.raw.df$NewArea <- 0

# for parcels with 2020 status Remain, set NewArea <- BArea
rem.20 <- cnt.raw.df[cnt.raw.df$Status20 == "Remain", ]
rem.20$NewArea <- rem.20$BArea
cnt.raw.df[cnt.raw.df$Status20 == "Remain", ] <- rem.20

print("Simulating building turnover for 2020...")

# set up parcel types to iterate over 
parcel.types <- fs.diffs.df$TYPE
type.iterators <- parcel.types[!(parcel.types %in% c("PUD", "Vacant"))]

# init df for storing demo multipliers
demo.mult.df <- data.frame(TYPE = character(), DEMO.MULT.20 = numeric(), 
                            stringsAsFactors = F)

# init df for storing btl ratios
btl.ratio.df <- data.frame(TYPE = character(), BTL.RATIO = numeric(), 
                           stringsAsFactors = F)

# iterate over building types
for (t in type.iterators) {
  print(sprintf("Working on buildings of type: %s", t))
   
  # subset for row of particular building type
  type.row <- fs.diffs.df[fs.diffs.df$TYPE == t, ]
  fs.diff <- type.row$DIFF
  
  # calculate bldg area to lot area ratio for usetype
  btl.ratio <- CalcBtlRatio(cnt.raw.df, t, barea.col = "BArea")
  
  # add to btl ratio df
  btl.row <- data.frame(TYPE = t, BTL.RATIO = btl.ratio)
  btl.ratio.df <- rbind(btl.ratio.df, btl.row)
  
  # case 1: difference is negative
  if (fs.diff < 0) {
    print("Floorspace difference is negative")
    print("Randomly vacating remaining buildings")
    # randomly vacate remaining buildings until diff is positive
    t.diff <- fs.diff
    while (t.diff < 0) {
      
      res <- RandomSwitch(cnt.raw.df, from.type = t, from.status = "Remain",
                          to.type = t, to.status = "Deactivated", 
                          status.year=2020)
      cnt.raw.df <- res[[1]]
      switched.apn <- res[[2]]
      print(sprintf("Vacated building on parcel with APN: %s", switched.apn))
      
      # check updated floorspace difference following switch
      t.diff <- CheckDiff(cnt.raw.df, t, status = "Remain",
                            year = 2020, area.col = "NewArea")
      print(sprintf("Updated floorspace difference: %s", t.diff))
    }
    
    # add zero value for this usetype's demo multiplier in output table
    c.row <- data.frame(TYPE = t, DEMO.MULT.20 = 0)
    demo.mult.df <- rbind(demo.mult.df, c.row)
  
  # case 1b: floorspace difference is positive but less than 100000
  } else if ((fs.diff > 0) & (fs.diff <= 100000)) {
    print("Floorspace difference is positive, but less than 100,000 sqft")
    demo.20.df <- subset(cnt.sum.20, (Status20 == "Demo") & (TYPE == t))
    demo.sum.20 <- sum(demo.20.df$SUM.FS.20)
    print("Calculating Gap-to-demo ratio...")
    demo.20.df <- subset(cnt.sum.20, (Status20 == "Demo") & (TYPE == t))
    demo.sum.20 <- sum(demo.20.df$SUM.FS.20)
    ratio <- fs.diff/demo.sum.20
    print(sprintf("Gap-to-demo ratio is: %s", ratio))
    
    # add to demo mult df
    c.row <- data.frame(TYPE = t, DEMO.MULT.20 = ratio)
    demo.mult.df <- rbind(demo.mult.df, c.row)
    
  # case 2: floorspace difference is positive
  } else if (fs.diff > 100000) {
    print("Floorspace difference is positive")
    print("Calculating Gap-to-demo ratio...")
    demo.20.df <- subset(cnt.sum.20, (Status20 == "Demo") & (TYPE == t))
    demo.sum.20 <- sum(demo.20.df$SUM.FS.20)
    ratio <- fs.diff/demo.sum.20
    print(sprintf("Gap-to-demo ratio is: %s", ratio))
    
    # add to demo mult df
    c.row <- data.frame(TYPE = t, DEMO.MULT.20 = ratio)
    demo.mult.df <- rbind(demo.mult.df, c.row)
    
    # case 2a: GTD ratio is less than 1
    if (ratio < 1) {
      print("GTD ratio is <1; rebuilding demos to close floorspace gap")
      
      # init floorspace difference tracker for this usetype
      t.diff <- CheckDiff(cnt.raw.df, t, status = "Remain",
                            year=2020, area.col="NewArea")
      
      # rebuild demo buildings until difference gap is closed
      while (t.diff > 0) {
        res <- SizeRankedSwitch(cnt.raw.df, from.type = t, 
                                from.status = "Demo", to.type = t, 
                                to.status = "Rebuild", year=2020, 
                                area.col <- "BArea")
        cnt.raw.df <- res[[1]]
        rebuilt.apn <- res[[2]]
        print(sprintf("Building with APN: %s rebuilt", rebuilt.apn))

        # update NewArea, Yrfinal and Bin50 fields of rebuild
        rebuild <- cnt.raw.df[(cnt.raw.df$APN == rebuilt.apn), ]
        rebuild$NewArea <- rebuild$BArea
        rebuild$Yrfinal <- "2020"
        rebuild$Bin50 <- "1"
        cnt.raw.df[(cnt.raw.df$APN == rebuilt.apn), ] <- rebuild
        
        # check floorspace difference
        t.diff <- CheckDiff(cnt.raw.df, t, status = c("Remain", "Rebuild"),
                              year=2020, area.col="NewArea")
        print(sprintf("Updated floorspace difference: %s", t.diff))
        
        # init expansion factor and step var to increase by on each pass
        expand.factor <- 1.5
        expand.step <- 0.5
        max.expand.factor <- 4
        
        # expand rebuilt building size until max expansion factor reached
        # or floorspace difference gap is closed
        while((t.diff > 0) & (expand.factor <= max.expand.factor)) {
          print(sprintf("Expanding area of rebuild by factor of %s", 
                        expand.factor))
          cnt.raw.df <- ExpandArea(cnt.raw.df, rebuilt.apn, expand.factor,
                                   from.area.col="BArea", 
                                   to.area.col="NewArea")
          # increase expansion factor 
          expand.factor <- expand.factor + expand.step
          t.diff <- CheckDiff(cnt.raw.df, t, status = c("Remain", "Rebuild"),
                                year=2020, area.col="NewArea")
          print(sprintf("Updated floorspace difference: %s", t.diff))
        }
      }
      
    # case 2b: GTD ratio is betwen 1 and 4 (inclusive)
    } else if ((ratio >= 1) & (ratio <= 4)) {
      print("Gap-to-demo ratio is >=1 & <= 4; upsizing rebuilds to close gap")
      
      # expand areas of rebuilds to close gap
      cnt.raw.df <- RebuildMultiplied(cnt.raw.df, type = t, 
                                    from.status = "Demo", to.status = "Rebuild",
                                    mult = ratio, year=2020,
                                    from.area.col = "BArea", 
                                    to.area.col="NewArea")
      
    # case 2c: GTD ratio is greater than 4
    } else if (ratio > 4) {
      print("Gap-to-demo ratio is >4; allocating demos to rebuilds first")
      
      # first allocate all demos if any exist
      res <- RebuildMultiplied(cnt.raw.df, type = t, 
                               from.status = "Demo", to.status = "Rebuild",
                               mult = 4, year=2020, from.area.col = "BArea", 
                               to.area.col="NewArea")
      
      # only update master parcel df if rebuilds were performed
      if (res != "None") {
        cnt.raw.df <- res
      }
      
      ## Second, fill open lots
      print("Filling open lots of same usetype second")
      
      # check whether there is avail lot area to convert
      if (t == "Condo") {
        check.lots <- cnt.raw.df[(cnt.raw.df$TYPE %in% c("Condo", "MF")) &
                                   (cnt.raw.df$Status20 == "LotsOpen"), ]
      } else {
        check.lots <- cnt.raw.df[(cnt.raw.df$TYPE == t) &
                                   (cnt.raw.df$Status20 == "LotsOpen"), ]
      }
      if (nrow(check.lots) != 0) {
        avail.lots <- TRUE
      } else {
        avail.lots <- FALSE
      }
      
      # init floorspace difference tracker for this usetype
      t.diff <- CheckDiff(cnt.raw.df, t, status = c("Remain", "Rebuild"),
                            year=2020, area.col="NewArea")
      
      # convert open lots up to allowable BTL ratios until FS difference is met
      # or convertable lot area runs out.
      while ((t.diff > 0) & (avail.lots)) {
        res <- ConvertLot(cnt.raw.df, type = t, btl.ratio, year=2020, 
                            from.area.col="BArea", to.area.col="NewArea")
        cnt.raw.df <- res[[1]] 
        converted.apn <- res[[2]]
        print(sprintf("Allocated FS from open lot on parcel with APN: %s", 
                      converted.apn))
        
        # check if no lots were available to convert
        if (res == "None") {
          avail.lots = FALSE
        } else {
        
          # update Yrfinal and Bin50 fields of parcel with converted lot area
          converted <- cnt.raw.df[(cnt.raw.df$APN == converted.apn), ]
          converted$Yrfinal <- "2020"
          converted$Bin50 <- "1"
          cnt.raw.df[(cnt.raw.df$APN == converted.apn), ] <- converted
          
          # check floorspace difference
          t.diff <- CheckDiff(cnt.raw.df, t, 
                                status = c("Remain", "Rebuild", "LotsFilled"),
                                year=2020, area.col="NewArea")
          print(sprintf("Updated floorspace difference: %s", t.diff))
        }
      }
      
      ## lastly, allocate remaining FS needs to parcels with ustype "Vacant"
      # check whether there are avail vacancies to convert
      check.vacs <- cnt.raw.df[(cnt.raw.df$TYPE == "Vacant"), ]
      if (nrow(check.vacs) != 0) {
        avail.vacancies <- TRUE
      } else {
        avail.vacancies <- FALSE
      }
      
      while ((t.diff > 0) & (avail.vacancies)) {
        print(sprintf("Floorspace difference is still positive: %s", t.diff))
        print(sprintf("Now allocating vacancies to floorspace of type: %s", t))
        
        # convert vacant parcel to active floorspace
        res <- FillVacant(cnt.raw.df, to.type = t, 
                            btl.ratio = btl.ratio, year = 2020)
        cnt.raw.df <- res[[1]]
        filled.apn <- res[[2]]
        print(sprintf("Allocated FS from vacant lot on parcel with APN: %s", 
                      filled.apn))
        
        if (res == "None") {
          avail.vacancies <- FALSE
        } else {
          # check floorspace difference
          t.diff <- CheckDiff(cnt.raw.df, t, 
                                status = c("Remain", "Rebuild", 
                                         "LotsFilled", "VacantFilled"),
                                year=2020, area.col="NewArea")
          print(sprintf("Updated floorspace difference: %s", t.diff))
        }
      }
    }
    
  }
  ## if there is still unmet floorspace requirements, export in table
  # init dataframe to store unmet gaps
  unmet.fs.gaps <- data.frame(TYPE = character(), FLOORSPACE_GAP = numeric())
  
  # check diff
  t.diff <- CheckDiff(cnt.raw.df, t, 
                        status = c("Remain", "Rebuild", 
                                   "LotsFilled", "VacantFilled"),
                        year=2020, area.col="NewArea")
  
  # generate row for this type if t.diff still positive
  if (t.diff > 0) {
    type.row <- data.frame(TYPE = t, FLOORSPACE_GAP = t.diff)
    unmet.fs.gaps <- rbind(unmet.fs.gaps, type.row)
  }
}

# write demo multipliers to output file
cnt.row <- MakeRow(demo.mult.df, cnt.name, "DEMO.MULT.20")
WriteOutput(cnt.row, output.filename, sheet.name = "2020DemoMult")

# write BTL ratios to ouptut file
cnt.row <- MakeRow(btl.ratio.df, cnt.name, "BTL.RATIO")
WriteOutput(cnt.row, output.filename, sheet.name = "2016BtoL")

# calc summary stats for 2020 statuses after simulation
## Step 1. determine 2020 building stock by building type
cnt.sum.20.post <- ddply(cnt.raw.df, .(TYPE, Status20), .drop=F, summarise, 
                         SUM.FS.20 = sum(na.omit(NewArea)), 
                         SUM.LOT.20 = sum(na.omit(LOTAREA)))

# write rebuilds to output table
rebuilds.20 <- cnt.sum.20.post[cnt.sum.20.post$Status20 == "Rebuild",]
if (nrow(rebuilds.20) == 0) {
  cnt.row <- MakeZeroRow(unique(cnt.raw.df$TYPE), cnt.name)
  WriteOutput(cnt.row, output.filename, sheet.name = "2020Rebuild") 
  
} else {
  cnt.row <- MakeRow(rebuilds.20, cnt.name, "SUM.FS.20")
  WriteOutput(cnt.row, output.filename, sheet.name = "2020Rebuild") 
}

# write lots filled to output table
lotsfilled.20 <- cnt.sum.20.post[cnt.sum.20.post$Status20 == "LotsFilled",]
if (nrow(lotsfilled.20) == 0) {
  cnt.row <- MakeZeroRow(unique(cnt.raw.df$TYPE), cnt.name)
  WriteOutput(cnt.row, output.filename, sheet.name = "2020LotsFilled") 
  
} else {
  cnt.row <- MakeRow(lotsfilled.20, cnt.name, "SUM.FS.20")
  WriteOutput(cnt.row, output.filename, sheet.name = "2020Deactivated") 
}

# write vacancies filled to output table
vacfilled.20 <- cnt.sum.20.post[cnt.sum.20.post$Status20 == "VacantFilled",]
if (nrow(vacfilled.20) == 0) {
  cnt.row <- MakeZeroRow(unique(cnt.raw.df$TYPE), cnt.name)
  WriteOutput(cnt.row, output.filename, sheet.name = "2020VacantFilled") 
  
} else {
  cnt.row <- MakeRow(vacfilled.20, cnt.name, "SUM.FS.20")
  WriteOutput(cnt.row, output.filename, sheet.name = "2020VacantFilled") 
}

# write deactivated bldgs to output table
deactiv.20 <- cnt.sum.20.post[cnt.sum.20.post$Status20 == "Deactivated",]
if (nrow(deactiv.20) == 0) {
  cnt.row <- MakeZeroRow(unique(cnt.raw.df$TYPE), cnt.name)
  WriteOutput(cnt.row, output.filename, sheet.name = "2020Deactivated") 
  
} else {
  cnt.row <- MakeRow(deactiv.20, cnt.name, "SUM.FS.20")
  WriteOutput(cnt.row, output.filename, sheet.name = "2020Deactivated") 
}

# write out unment floorspace requirements df
cnt.row <- MakeRow(unmet.fs.gaps, cnt.name, "FLOORSPACE_GAP")
WriteOutput(cnt.row, output.filename, 
            sheet.name = "2020UnmetFloorspace")


#-----------------------------------------------------------------------------#



############ PHASE II: PROJECT TO 2050 ######
print("Starting Phase II: Determining 2020 building stock")

## Step 1. Determine 2020 building stock
cnt.sum.20 <- ddply(cnt.raw.df, .(TYPE), .drop = F,
                    summarise, SUM.NewArea.20 = sum(na.omit(NewArea)))

# add 2020 stock df to output.stock.dfs
output.stock.df.lst[[2]] <- cnt.raw.df

# write output shapefile for 2016 building stock
WriteShapefile(cnt.raw.df, cnt.code, 2020)

# organize particular county's data for export
cnt.row <- MakeRow(cnt.sum.20, cnt.name, "SUM.NewArea.20")

# write to output file
WriteOutput(cnt.row, output.filename, sheet.name = "2020Stock")

## Step 2. project 2050 building stock

# susbet growth multipliers for particular county
g.mult.50.cnt <- g.mult.50[g.mult.20$County == cnt.name, ]

# transpose and organize multiplier dataframe 
g.mult.50.cnt <- data.frame(t(g.mult.50.cnt))
g.mult.50.cnt$TYPE <- rownames(g.mult.50.cnt)
g.mult.50.cnt <- g.mult.50.cnt[2:nrow(g.mult.50.cnt), ]
names(g.mult.50.cnt) <- c("MULT.50", "TYPE")
rownames(g.mult.50.cnt) <- seq(1, nrow(g.mult.50.cnt))

# merge multipliers to parcel data
cnt.sum.20 <- merge(cnt.sum.20, g.mult.50.cnt, by = "TYPE", all.x = T)
cnt.sum.20$SUM.NewArea.20 <- as.numeric(cnt.sum.20$SUM.NewArea.20)
cnt.sum.20$MULT.50 <- as.numeric(as.character(cnt.sum.20$MULT.50))

# multiply 2016 stocks by 2020 multipliers
cnt.sum.20$PROJ.FS.50 <- NA
cnt.sum.20$PROJ.FS.50 <- cnt.sum.20$SUM.NewArea.20 * cnt.sum.20$MULT.50
PROJ.FS.50.col <- cnt.sum.20[,c("TYPE", "PROJ.FS.50")]

# merge 2020 projections with projections dataframe
proj.df <- merge(proj.df, PROJ.FS.50.col, by="TYPE", all.x=T)

# organize particular county's data for export
cnt.row <- MakeRow(cnt.sum.20, cnt.name, "PROJ.FS.50")

# write to output file
WriteOutput(cnt.row, output.filename, sheet.name = "2050Projected")

## Step 3. determine if demolished by 2050

# join 2050 cutoff table by usetype
cutoffs.50 <- cutoff.yrs[, c("TYPE", "Cut50"),]
cnt.raw.df <- merge(cnt.raw.df, cutoffs.50, by = "TYPE", all.x = T)

# iterate over all parcels, checking if they are demo'd by 2050
pass <- 1
for (p in 1:nrow(cnt.raw.df)) {
  par <- cnt.raw.df[p,]
  print(sprintf("Pass: %s Checking parcel with APN: %s", pass, par$APN))
  yr <- as.numeric(as.character(par$Yrfinal))
  cut <- as.numeric(as.character(par$Cut50))
  par$Bin50 <- CheckIfRemains(yr, cut)
  cnt.raw.df[p,] <- par
  pass <- pass + 1
}

## Step 4. assign 2020 status labels to parcels

# init col to store 2050 status for each parcel
cnt.raw.df$Status50 <- NA


# iterate over parcels, assigning 2050 statuses
print("Determining statuses of parcels in 2050...")
pass <- 1
for (p in 1:nrow(cnt.raw.df)) {
  par <- cnt.raw.df[p,]
  print(sprintf("Pass: %s Checking parcel with APN: %s", pass, par$APN))
  BinYEAR <- as.numeric(as.character(par$Bin50))
  BAREA <- as.numeric(as.character(par$NewArea))
  LOTAREA <- as.numeric(as.character(par$LOTAREA))
  TYPE <- as.character(par$TYPE)
  par$Status50 <- AssignStatus(BinYEAR, BAREA, LOTAREA, TYPE)
  cnt.raw.df[p,] <- par
  pass <- pass + 1
}

## Step 5: Generate summary stats for building stock in 2050, by Status50
cnt.sum.50 <- ddply(cnt.raw.df, .(TYPE, Status50), .drop = F,
                    summarise, SUM.FS.50 = sum(NewArea),
                    SUM.LOT.50 = sum(na.omit(LOTAREA)))

## Export summary table of remaining buildings in 2050
remain.50 <- cnt.sum.50[cnt.sum.50$Status50 == "Remain", ] 
cnt.row <- MakeRow(remain.50, cnt.name, "SUM.FS.50")
WriteOutput(cnt.row, output.filename, sheet.name = "2050Remain")

## Export summary table of demo buildings in 2050
demo.50 <- cnt.sum.50[cnt.sum.50$Status50 == "Demo", ] 
cnt.row <- MakeRow(demo.50, cnt.name, "SUM.FS.50")
WriteOutput(cnt.row, output.filename, sheet.name = "2050Demo")

## Export summary table of open lots  in 2050
lots.50 <- cnt.sum.50[cnt.sum.50$Status50 == "LotsOpen", ] 
cnt.row <- MakeRow(lots.50, cnt.name, "SUM.LOT.50")
WriteOutput(cnt.row, output.filename, sheet.name = "2050LotsOpen")

# add field FNArea.50
cnt.raw.df$FNArea.50 <- 0

# for parcels with 2050 status Remain, set FNArea.50
rem.50 <- cnt.raw.df[cnt.raw.df$Status50 == "Remain", ]
rem.50$FNArea.50 <- rem.50$NewArea
cnt.raw.df[cnt.raw.df$Status50 == "Remain", ] <- rem.50

# determine additional floorspace needs for 2050
yr20.data <- subset(cnt.sum.20, 
                    select = c("TYPE", "SUM.NewArea.20", "PROJ.FS.50"))
yr50.data <- cnt.sum.50[cnt.sum.50$Status50 == "Remain", ]
fs.diffs.df <- merge(yr20.data, yr50.data, by = "TYPE")

# calculate differences between remaining and projected floorspace by type
fs.diffs.df$DIFF <- fs.diffs.df$PROJ.FS.50 - fs.diffs.df$SUM.FS.50

## Export summary table of 2050 floorspace differences
cnt.row <- MakeRow(fs.diffs.df, cnt.name, "DIFF")
WriteOutput(cnt.row, output.filename, sheet.name = "2050Diff")

print("Simulating building turnover for 2050...")

# set up parcel types to iterate over 
parcel.types <- fs.diffs.df$TYPE
type.iterators <- parcel.types[!(parcel.types %in% c("PUD", "Vacant"))]

# init df for storing demo multipliers
demo.mult.df <- data.frame(TYPE = character(), DEMO.MULT.50 = numeric(), 
                           stringsAsFactors = F)

# init df for storing btl ratios
btl.ratio.df <- data.frame(TYPE = character(), BTL.RATIO = numeric(), 
                           stringsAsFactors = F)

# iterate over building types
for (t in type.iterators) {
  print(sprintf("Working on buildings of type: %s", t))
  
  # subset for row of particular building type
  type.row <- fs.diffs.df[fs.diffs.df$TYPE == t, ]
  fs.diff <- type.row$DIFF
  
  # calculate BArea to LotArea ratio for usetype
  btl.ratio <- CalcBtlRatio(cnt.raw.df, t, barea.col="NewArea")
  
  # add to btl ratio df
  btl.row <- data.frame(TYPE = t, BTL.RATIO = btl.ratio)
  btl.ratio.df <- rbind(btl.ratio.df, btl.row)
  
  # case 1: difference is negative
  if (fs.diff < 0) {
    print("Floorspace difference is negative")
    print(sprintf("Randomly vacating remaining buildings of type: %s", t))
    # randomly vacate remaining buildings until diff is positive
    t.diff <- fs.diff
    while (t.diff < 0) {
      res <- RandomSwitch(cnt.raw.df, from.type = t, from.status = "Remain",
                          to.type = t, to.status = "Deactivated", 
                          status.year=2050)
      cnt.raw.df <- res[[1]]
      switched.apn <- res[[2]]
      print(sprintf("Vacated building on parcel with APN: %s", switched.apn))
      
      # check updated floorspace difference following switch
      t.diff <- CheckDiff(cnt.raw.df, t, status = "Remain", 
                            year=2050, area.col="FNArea.50")
      print(sprintf("Updated floorspace difference: %s", t.diff))
      
    }
    
    # add zero value for this usetype's demo multiplier in output table
    c.row <- data.frame(TYPE = t, DEMO.MULT.50 = 0)
    demo.mult.df <- rbind(demo.mult.df, c.row)
  
  # case 1b: floorspace difference is positive but less than 100000
  } else if ((fs.diff > 0) & (fs.diff <= 100000)) {
    print("Floorspace difference is positive, but less than 100,000 sqft")
    demo.50.df <- subset(cnt.sum.50, (Status50 == "Demo") & (TYPE == t))
    demo.sum.50 <- sum(demo.50.df$SUM.FS.50)
    
    print("Calculating Gap-to-demo ratio...")
    ratio <- fs.diff/demo.sum.50
    
    print(sprintf("Gap-to-demo ratio is: %s", ratio))
    
    # add to demo mult df
    c.row <- data.frame(TYPE = t, DEMO.MULT.50 = ratio)
    demo.mult.df <- rbind(demo.mult.df, c.row)
  
  # case 2: floorspace difference is positive
  } else if (fs.diff > 100000) {
    print("Floorspace difference is positive")
    demo.50.df <- subset(cnt.sum.50, (Status50 == "Demo") & (TYPE == t))
    demo.sum.50 <- sum(demo.50.df$SUM.FS.50)
   
    print("Calculating Gap-to-demo ratio...")
    ratio <- fs.diff/demo.sum.50
    
    print(sprintf("Gap-to-demo ratio is: %s", ratio))
    
    # add to demo mult df
    c.row <- data.frame(TYPE = t, DEMO.MULT.50 = ratio)
    demo.mult.df <- rbind(demo.mult.df, c.row)
    
    # case 2a: GTD ratio is less than 1
    if (ratio < 1) {
      print("GTD ratio is <1; rebuilding demos to close floorspace gap")
      
      # init floorspace difference tracker for this usetype
      t.diff <- CheckDiff(cnt.raw.df, t, status = "Remain",
                            year=2050, area.col="FNArea.50")
      
      # rebuild demo buildings until difference gap is closed
      while (t.diff > 0) {
        res <- SizeRankedSwitch(cnt.raw.df, from.type = t, 
                                from.status = "Demo", to.type = t, 
                                to.status = "Rebuild", year=2050, 
                                area.col="NewArea")
        cnt.raw.df <- res[[1]]
        rebuilt.apn <- res[[2]]
        print(sprintf("Building with APN: %s rebuilt", rebuilt.apn))
        
        # update NewArea, Yrfinal and Bin50 fields of rebuild
        rebuild <- cnt.raw.df[(cnt.raw.df$APN == rebuilt.apn), ]
        rebuild$NewArea <- rebuild$BArea
        rebuild$Yrfinal <- "2050"
        rebuild$Bin50 <- "1"
        cnt.raw.df[(cnt.raw.df$APN == rebuilt.apn), ] <- rebuild
        
        # check floorspace difference
        t.diff <- CheckDiff(cnt.raw.df, t, status = c("Remain", "Rebuild"), 
                              year=2050, area.col="FNArea.50")
        print(sprintf("Updated floorspace difference: %s", t.diff))
        
        # init expansion factor and step var to increase by on each pass
        expand.factor <- 1.5
        expand.step <- 0.5
        max.expand.factor <- 4
        
        # expand rebuilt building size until max expansion factor reached
        # or floorspace difference gap is closed
        while((t.diff > 0) & (expand.factor <= max.expand.factor)) {
          print(sprintf("Expanding area of rebuild by factor of %s", 
                        expand.factor))
          cnt.raw.df <- ExpandArea(cnt.raw.df, rebuilt.apn, expand.factor,
                                   from.area.col="NewArea",
                                   to.area.col="FNArea.50")
          # increase expansion factor 
          expand.factor <- expand.factor + expand.step
          t.diff <- CheckDiff(cnt.raw.df, t, status = c("Remain", "Rebuild"),
                                year=2050, area.col="FNArea.50")
          print(sprintf("Updated floorspace difference: %s", t.diff))
        }
      }
      # case 2b: GTD ratio is betwen 1 and 4 (inclusive)
    } else if ((ratio >= 1) & (ratio <= 8)) {
      print("Gap-to-demo ratio is >=1 & <= 8; upsizing rebuilds to close gap")
      
      # expand areas of rebuilds to close gap
      cnt.raw.df <- RebuildMultiplied(cnt.raw.df, type = t, 
                                    from.status = "Demo", to.status = "Rebuild",
                                    mult = ratio, year=2050, 
                                    from.area.col="NewArea",
                                    to.area.col="FNArea.50")
      
      # case 2c: GTD ratio is greater than 8
    } else if (ratio > 8) {
      print("Gap-to-demo ratio is >4; allocating demos to rebuilds first")
      
      # first allocate all demos if any exist
      res <- RebuildMultiplied(cnt.raw.df, type = t, 
                             from.status = "Demo", to.status = "Rebuild",
                             mult = 8, year=2050, from.area.col="NewArea",
                             to.area.col="FNArea.50")
      if (res != "None") {
        cnt.raw.df <- res
      }
      
      ## Second, fill open lots
      print("Filling open lots of same usetype second")
      
      # check whether there is avail lot area to convert
      if (t == "Condo") {
        check.lots <- cnt.raw.df[(cnt.raw.df$TYPE %in% c("Condo", "MF")) &
                                   (cnt.raw.df$Status50 == "LotsOpen"), ]
      } else {
        check.lots <- cnt.raw.df[(cnt.raw.df$TYPE == t) &
                                   (cnt.raw.df$Status50 == "LotsOpen"), ]
      }
      if (nrow(check.lots) != 0) {
        avail.lots <- TRUE
      } else {
        avail.lots <- FALSE
      }
      
      # init floorspace difference tracker for this usetype
      t.diff <- CheckDiff(cnt.raw.df, t, status = c("Remain", "Rebuild"),
                            year=2050, area.col="FNArea.50")
      
      # convert open lots up to allowable BTL ratios until FS difference is met
      # or convertable lot area runs out.
      while ((t.diff > 0) & (avail.lots)) {
        res <- ConvertLot(cnt.raw.df, type = t, btl.ratio, year=2050,
                          from.area.col="NewArea", to.area.col="FNArea.50")
        cnt.raw.df <- res[[1]] 
        converted.apn <- res[[2]]
        print(sprintf("Allocated FS from open lot on parcel with APN: %s", 
                      converted.apn))
        
        # check if no lots were available to convert
        if (res == "None") {
          avail.lots = FALSE
        } else {
          
          # update Yrfinal and Bin50 fields of parcel with converted lot area
          converted <- cnt.raw.df[(cnt.raw.df$APN == converted.apn), ]
          converted$Yrfinal <- "2050"
          cnt.raw.df[(cnt.raw.df$APN == converted.apn), ] <- converted
          
          # check floorspace difference
          t.diff <- CheckDiff(cnt.raw.df, t, 
                                status = c("Remain", "Rebuild", "LotsFilled"),
                                year=2050, area.col="FNArea.50")
          print(sprintf("Updated floorspace difference: %s", t.diff))
        }
      }
      
      ## lastly, allocate remaining FS needs to parcels with ustype "Vacant"
      # check whether there are avail vacancies to convert
      check.vacs <- cnt.raw.df[(cnt.raw.df$TYPE == "Vacant"), ]
      if (nrow(check.vacs) != 0) {
        avail.vacancies <- TRUE
      } else {
        avail.vacancies <- FALSE
      }
      
      while ((t.diff > 0) & (avail.vacancies)) {
        print(sprintf("Floorspace difference is still positive: %s", t.diff))
        print(sprintf("Now allocating vacancies to floorspace of type: %s", t))
        
        # convert vacant parcel to active floorspace
        res <- FillVacant(cnt.raw.df, to.type = t, 
                          btl.ratio = btl.ratio, year = 2050)
        cnt.raw.df <- res[[1]]
        filled.apn <- res[[2]]
        print(sprintf("Allocated FS from vacant lot on parcel with APN: %s", 
                      filled.apn))
        
        if (res == "None") {
          avail.vacancies <- FALSE
        } else {
          # check floorspace difference
          t.diff <- CheckDiff(cnt.raw.df, t, 
                                status = c("Remain", "Rebuild", 
                                           "LotsFilled", "VacantFilled"),
                                year=2050, area.col="FNArea.50")
          print(sprintf("Updated floorspace difference: %s", t.diff))
        }
      }
    }
    
  }
  ## if there is still unmet floorspace requirements, export in table
  # init dataframe to store unmet gaps
  unmet.fs.gaps <- data.frame(TYPE = character(), FLOORSPACE_GAP = numeric())
  
  # check diff
  t.diff <- CheckDiff(cnt.raw.df, t, 
                        status = c("Remain", "Rebuild", 
                                   "LotsFilled", "VacantFilled"),
                        year=2050, area.col="FNArea.50")
  
  # generate row for this type if t.diff still positive
  if (t.diff > 0) {
    type.row <- data.frame(TYPE = t, FLOORSPACE_GAP = t.diff)
    unmet.fs.gaps <- rbind(unmet.fs.gaps, type.row)
    
  }
}

# write demo multipliers to output file
cnt.row <- MakeRow(demo.mult.df, cnt.name, "DEMO.MULT.50")
WriteOutput(cnt.row, output.filename, sheet.name = "2050DemoMult")

# write BTL ratios to ouptut file
cnt.row <- MakeRow(btl.ratio.df, cnt.name, "BTL.RATIO")
WriteOutput(cnt.row, output.filename, sheet.name = "2016BtoL")

# calc summary stats for 2050 statuses after simulation
cnt.sum.50.post <- ddply(cnt.raw.df, .(TYPE, Status50), .drop=F, summarise, 
                         SUM.FS.50 = sum(na.omit(NewArea)))

# write rebuilds to output table
rebuilds.50 <- cnt.sum.50.post[cnt.sum.50.post$Status50 == "Rebuild",]
if (nrow(rebuilds.50) == 0) {
  cnt.row <- MakeZeroRow(unique(cnt.raw.df$TYPE), cnt.name)
  WriteOutput(cnt.row, output.filename, sheet.name = "2050Rebuild") 
  
} else {
  cnt.row <- MakeRow(rebuilds.50, cnt.name, "SUM.FS.50")
  WriteOutput(cnt.row, output.filename, sheet.name = "2050Rebuild") 
}

# write lots filled to output table
lotsfilled.50 <- cnt.sum.50.post[cnt.sum.50.post$Status50 == "LotsFilled",]
if (nrow(lotsfilled.50) == 0) {
  cnt.row <- MakeZeroRow(unique(cnt.raw.df$TYPE), cnt.name)
  WriteOutput(cnt.row, output.filename, sheet.name = "2050LotsFilled") 
  
} else {
  cnt.row <- MakeRow(lotsfilled.50, cnt.name, "SUM.FS.50")
  WriteOutput(cnt.row, output.filename, sheet.name = "2050Deactivated") 
}

# write vacancies filled to output table
vacfilled.50 <- cnt.sum.50.post[cnt.sum.50.post$Status50 == "VacantFilled",]
if (nrow(vacfilled.50) == 0) {
  cnt.row <- MakeZeroRow(unique(cnt.raw.df$TYPE), cnt.name)
  WriteOutput(cnt.row, output.filename, sheet.name = "2050VacantFilled") 
  
} else {
  cnt.row <- MakeRow(vacfilled.50, cnt.name, "SUM.FS.50")
  WriteOutput(cnt.row, output.filename, sheet.name = "2050VacantFilled") 
}

# write deactivated bldgs to output table
deactiv.50 <- cnt.sum.50.post[cnt.sum.50.post$Status50 == "Deactivated",]

if (nrow(deactiv.50) == 0) {
  cnt.row <- MakeZeroRow(unique(cnt.raw.df$TYPE), cnt.name)
  WriteOutput(cnt.row, output.filename, sheet.name = "2050Deactivated") 
  
} else {
  cnt.row <- MakeRow(deactiv.50, cnt.name, "SUM.FS.50")
  WriteOutput(cnt.row, output.filename, sheet.name = "2050Deactivated") 
}

# write out unment floorspace requirements df
cnt.row <- MakeRow(unmet.fs.gaps, cnt.name, "FLOORSPACE_GAP")
WriteOutput(cnt.row, output.filename, 
            sheet.name = "2050UnmetFloorspace")




############ PHASE III ############  
# create 2050 building stock
cnt.sum.50 <- ddply(cnt.raw.df, .(TYPE), .drop = F,
                    summarise, SUM.FNArea.50 = sum(na.omit(FNArea.50)))

# add 2016 stock df to output.stock.dfs
output.stock.df.lst[[3]] <- cnt.raw.df

# write output shapefile for 2050 building stock
WriteShapefile(cnt.raw.df, cnt.code, 2050)

# organize particular county's summary data for export
cnt.row <- MakeRow(cnt.sum.50, cnt.name, "SUM.FNArea.50")

# write to output file
WriteOutput(cnt.row, output.filename, sheet.name = "2050Stock")

# export output stock list
saveRDS(output.stock.df.lst, "stocks_16_20_50.RDS")





  