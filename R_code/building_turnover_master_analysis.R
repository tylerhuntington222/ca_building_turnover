system.time({
# TODO:

# change column class types appropriately in data cleaning section
# remove as.character(), as.numeric() calls later on in script

# add fx header for WriteOutput()

# remove parcel.df  param description from function headers
# remove parcel.df arguments from function calls 

# address "ifBtoL is null or 0, use state average for that usetype"
  
# - Search for NewArea and replace with NewArea.20
  
# join agroups to parcel data

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
# vec_building_turnover.R

# Type: R function script

# AUTHOR:
# Tyler Huntington, 2017
# Lawrence Berkeley National Lab Sustainable Energy Group
# Project: California Building Turnover Study
# Principal Investigator: Hanna Breunig

# PURPOSE:
# PURPOSE:
# A spatially explicit simulation model for quantifying 
# current and future energy demands of buildings on non-residential land
# parcels in the state of California.

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
# 
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
require(doSNOW)

############ SOURCE AUXILIARY FUNCTIONS ############ 
source("bt_functions.R")

############ GLOBAL ENV SETUP ######au###### 

## Init output excel file for exporting intermediate tables
output.wb <- createWorkbook()
output.filename <- "../output_data/building_turnover_output_tables.xlsx"

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

############ LOAD & CLEAN SUPPLEMENTARY DATA ############

# set name of data file with sheets containing required tables and input data
input.datafile <- "../supp_data/Steps_BuildingStockTurnover_7_6.xlsx"

## Cutoff yrs table
# load cutoff yrs table
cutoff.yrs <- read.xlsx(file = input.datafile,
                       sheetIndex = 2)
# change col class to character
cutoff.yrs$TYPE <- as.character(cutoff.yrs$TYPE)

# change type names for consistency
cutoff.yrs$TYPE[cutoff.yrs$TYPE == "Food Sale"] <- "Food.Sale"
cutoff.yrs$TYPE[cutoff.yrs$TYPE == "Food Srv"] <- "Food.Srv"
cutoff.yrs$TYPE[cutoff.yrs$TYPE == "Comm Misc"] <- "Comm.Misc"
cutoff.yrs$TYPE[cutoff.yrs$TYPE == "Heath"] <- "Health"

# change col names
names(cutoff.yrs) <- c("TYPE", "Cut20", "Cut50")

## Growth multiplier tables
# load 2020 multipliers
g.mult.20 <- read.xlsx(file = input.datafile,
                       sheetName = "2020growthmult", colClasses = "character")

# load 2050 multipliers
g.mult.50 <- read.xlsx(file = input.datafile,
                       sheetName = "2050growthmult", colClasses = "character")

# change class of county cols to character
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

## EUI tables
# load 2016 EUI data
eui.16 <- read.xlsx(file = input.datafile,
                       sheetName = "EUI2016", colClasses = "character")

# load 2020 EUI data
eui.20 <- read.xlsx(file = input.datafile,
                       sheetName = "EUI2020", colClasses = "character")
# load 2050 EUI data
eui.50 <- read.xlsx(file = input.datafile,
                       sheetName = "EUI2050", colClasses = "character")

# set col classes of EUI tables
eui.16$Join <- as.character(paste(eui.16$Join))
eui.20$Join <- as.character(paste(eui.20$Join))
eui.50$Join <- as.character(paste(eui.50$Join))

## County abbreviation codes
# load county name/abbreviation table
county.codes.df <- read.xlsx(file = input.datafile, sheetName = "CountyCodes",
                             colClasses = "character")

# set col classes to char
county.codes.df$county.name <- as.character(county.codes.df$county.name)
county.codes.df$county.code <- as.character(county.codes.df$county.code)


############ LOAD & PROCESS INPUT SHAPEFILES IN PARALLEL ############

# set up processing que from shapefiles in input_shapefiles dir
dir.files <- list.files("../input_shapefiles/")
in.shapes <- dir.files[CheckExt(dir.files)]
  
# # initialize parallel backend
# no.cores <- (detectCores() - 1) 
# cl <- makeCluster(no.cores, type = "SOCK", outfile="../run_logs/log.txt")
# registerDoSNOW(cl)

# iterate over shapefiles in 'input_shapefiles' directory
output.metalist <- foreach(shapefile = in.shapes[1:1],
          .packages = c("plyr", "dplyr", "rgdal", "sp",
                        "raster",  "rgeos", "xlsx", "doSNOW"),
          .export = c("eui.16", "eui.20", "eui.50",
                      "output.wb")) %do% {
  
  ############ LOAD FUNCTIONS ############
  source("bt_functions.R")
                          
  ############ LOAD PARCEL DATA ############ 
  in.shape.layer <- substr(shapefile, 1, nchar(shapefile)-4)
  in.shape.path <- paste0("../input_shapefiles/", shapefile)
  cnt.final <- readOGR(dsn = in.shape.path, layer = in.shape.layer)
  
  # extract data part of spatial object
  cnt.final.df <- cnt.final@data
  
  # extract coord ref system
  crs <- crs(cnt.final)

  # get county name
  cnt.name <- unique(as.character(cnt.final.df$County))
  
  # determine this county's three letter ID code
  i <- which(county.codes.df$county.name == cnt.name)
  cnt.code <- county.codes.df$county.code[i]
  
  ############ CLEAN PARCEL DATA ############ 
  
  # TEMP: test a subset of the parcel data
  percent.data <- 100
  n <- nrow(cnt.final)
  pars.to.test <- sample(1:n, (n*(percent.data/100.0)))
  
  # # TEMP: test all parcels of a particular usetype
  # pars.to.test <- which(cnt.final$TYPE == "Food Sale")
  
  # TEMP: total rows in parcel data: 115465
  
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
  cnt.raw.df$LOTAREA <- as.numeric(as.character(cnt.raw.df$LOTAREA))
  cnt.raw.df$APN <- as.character(paste(cnt.raw.df$APN))
  cnt.raw.df$Typef <- as.character(cnt.raw.df$Typef)
  cnt.raw.df$POLY_AREA <- as.numeric(cnt.raw.df$POLY_AREA)
  cnt.raw.df$POLY_AREA <- round(cnt.raw.df$POLY_AREA, 4)
  
  # change NAs to zeros in applicable numeric cols
  cnt.raw.df$LOTAREA[is.na(cnt.raw.df$LOTAREA)] <- 0
  
  # change building type names in parcel data for consistency
  cnt.raw.df$TYPE[cnt.raw.df$TYPE == "Food Srv"] <- "Food.Srv"
  cnt.raw.df$TYPE[cnt.raw.df$TYPE == "Food Sale"] <- "Food.Sale"
  cnt.raw.df$TYPE[cnt.raw.df$TYPE == "Comm Misc"] <- "Comm.Misc"
  cnt.raw.df$TYPE[cnt.raw.df$TYPE == "college"] <- "College"
  
  
  # init list of dfs for outputting building stock data in 2016, 2020 and 2050
  output.stock.df.lst <- vector("list", 3)
  
  ## Step 1. determine 2016 building stock by building type
  cnt.sum.16 <- ddply(cnt.raw.df, .(TYPE), .drop = F,
                      summarise, SUM.FS.16 = sum(BArea))
  
  # assign AGROUP field vals
  cnt.raw.df <- AssignAgroup(cnt.raw.df)
  
  # write output shapefile for 2016 building stock
  print("Exporting 2016 building stock data...")
  output.sp <- WriteShapefile(cnt.raw.df, cnt.code, 2016)
  
  # add 2016 stock df to output.stock.dfs
  output.stock.df.lst[[1]] <- output.sp
  
  # organize particular county's data for export
  cnt.row <- MakeRow(cnt.sum.16, cnt.name, "SUM.FS.16")
  
  # write to output file
  WriteOutput(cnt.row, output.filename, sheet.name = "2016Stock")
  
  ## Step 2. project 2020 building stock
  print("Applying growth multipliers to project 2020 building stock...")
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
  
  # check for which parcels reamin in 2020
  cnt.raw.df$Bin20 <- CheckIfRemains(cnt.raw.df$Yrfinal, cnt.raw.df$Cut20)
  
  
  ## Step 4. assign 2020 status labels to parcels
  
  # init col to store 2020 status for each parcel
  cnt.raw.df$Status20 <- NA
  
  # iterate over parcels, assigning 2020 statuses
  print("Determining status of parcels in 2020...")
  BinYEAR <- as.numeric(as.character(cnt.raw.df$Bin20))
  BAREA <- as.numeric(as.character(cnt.raw.df$BArea))
  LOTAREA <- as.numeric(as.character(cnt.raw.df$LOTAREA))
  TYPE <- as.character(cnt.raw.df$TYPE)
  cnt.raw.df$Status20 <- unlist(AssignStatus(BinYEAR, BAREA, LOTAREA, TYPE))
  
  
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
  # init dataframe to store unmet gaps
  unmet.fs.gaps <- data.frame(TYPE = character(), FLOORSPACE_GAP = numeric())
  
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
        t.diff <- CheckDiff(cnt.raw.df, t, status = c("Remain"),
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
        t.diff <- CheckDiff(cnt.raw.df, t, status = c("Remain"),
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
                                        from.status = "Demo", 
                                        to.status = "Rebuild",
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
        
        # convert open lots up to allowable BTL ratios until FS difference met
        # or convertable lot area runs out.
        while ((t.diff > 0) & (avail.lots)) {
          res <- ConvertLot(cnt.raw.df, type = t, btl.ratio, year=2020, 
                            from.area.col="BArea", to.area.col="NewArea")
          # check if no lots were available to convert
          if (res[[1]] == "None") {
            avail.lots = FALSE
          
          } else {
            # update master parcel df
            cnt.raw.df <- res[[1]] 
            converted.apn <- res[[2]]
            print(sprintf("Allocated FS from open lot on parcel with APN: %s", 
                          converted.apn))
            
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
          print(sprintf("Allocating vacancies to floorspace of type: %s", t))
          
          # convert vacant parcel to active floorspace
          res <- FillVacant(cnt.raw.df, to.type = t, 
                            btl.ratio = btl.ratio, year = 2020)
          
          if (res[[1]] == "None") {
            avail.vacancies <- FALSE
          } else {
            # update parcel data after vacancy filled
            cnt.raw.df <- res[[1]]
            filled.apn <- res[[2]]
            print(sprintf("Allocated FS from vacant lot on parcel with APN: %s", 
                          filled.apn))
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
    # check fs difference
    t.diff <- CheckDiff(cnt.raw.df, t, 
                        status = c("Remain", "Rebuild", 
                                   "LotsFilled", "VacantFilled"),
                        year=2020, area.col="NewArea")
    
    # generate row for this type if t.diff still positive
    if (t.diff > 0) {
      type.row <- data.frame(TYPE = t, FLOORSPACE_GAP = t.diff)
      unmet.fs.gaps <- rbind(unmet.fs.gaps, type.row)
    } else {
      type.row <- data.frame(TYPE = t, FLOORSPACE_GAP = 0)
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
  
  
  #---------------------------------------------------------------------------#
  
  
  
  ############ PHASE II: PROJECT TO 2050 ######
  print("Starting Phase II: Determining 2020 building stock")
  
  ## Step 1. Determine 2020 building stock
  cnt.sum.20 <- ddply(cnt.raw.df, .(TYPE), .drop = F,
                      summarise, SUM.NewArea.20 = sum(na.omit(NewArea)))
  
  # assign AGROUP field vals
  cnt.raw.df <- AssignAgroup(cnt.raw.df)

  # write output shapefile for 2016 building stock
  output.sp <- WriteShapefile(cnt.raw.df, cnt.code, 2020)
  
  # add 2020 stock df to output.stock.dfs
  output.stock.df.lst[[2]] <- output.sp
  
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
  cnt.raw.df$Bin50 <- CheckIfRemains(cnt.raw.df$Yrfinal, cnt.raw.df$Cut50)
  
  ## Step 4. assign 2020 status labels to parcels
  
  # init col to store 2050 status for each parcel
  cnt.raw.df$Status50 <- NA
  
  # assign 2020 statuses to parcels
  print("Determining status of parcels in 2050...")
  BinYEAR <- as.numeric(as.character(cnt.raw.df$Bin50))
  BAREA <- as.numeric(as.character(cnt.raw.df$BArea))
  LOTAREA <- as.numeric(as.character(cnt.raw.df$LOTAREA))
  TYPE <- as.character(cnt.raw.df$TYPE)
  cnt.raw.df$Status50 <- unlist(AssignStatus(BinYEAR, BAREA, LOTAREA, TYPE))
  
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
  
  # init dataframe to store unmet gaps
  unmet.fs.gaps <- data.frame(TYPE = character(), FLOORSPACE_GAP = numeric())
  
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
        t.diff <- CheckDiff(cnt.raw.df, t, status = c("Remain"), 
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
        t.diff <- CheckDiff(cnt.raw.df, t, status = c("Remain"),
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
                                        from.status = "Demo", 
                                        to.status = "Rebuild",
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
        
        # convert open lots up to allowable BTL ratios until FS difference  met
        # or convertable lot area runs out.
        while ((t.diff > 0) & (avail.lots)) {
          res <- ConvertLot(cnt.raw.df, type = t, btl.ratio, year=2050,
                            from.area.col="NewArea", to.area.col="FNArea.50")
          
          # check if no lots were available to convert
          if (res[[1]] == "None") {
            avail.lots = FALSE

          } else {
            # update master parcel df
            cnt.raw.df <- res[[1]] 
            converted.apn <- res[[2]]
            print(sprintf("Allocated FS from open lot on parcel with APN: %s", 
                          converted.apn))
            
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
          print(sprintf("Allocating vacancies to floorspace of type: %s", t))
          
          # convert vacant parcel to active floorspace
          res <- FillVacant(cnt.raw.df, to.type = t, 
                            btl.ratio = btl.ratio, year = 2050)
          if (res[[1]] == "None") {
            avail.vacancies <- FALSE
          
          } else {
          # update parcel data
            cnt.raw.df <- res[[1]]
            filled.apn <- res[[2]]
            print(sprintf("Allocated FS from vacant lot on parcel with APN: %s", 
                        filled.apn))
          
         
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
    # check diff
    t.diff <- CheckDiff(cnt.raw.df, t, 
                        status = c("Remain", "Rebuild", 
                                   "LotsFilled", "VacantFilled"),
                        year=2050, area.col="FNArea.50")
    
    # generate row for this type if t.diff still positive
    if (t.diff > 0) {
      type.row <- data.frame(TYPE = t, FLOORSPACE_GAP = t.diff)
      unmet.fs.gaps <- rbind(unmet.fs.gaps, type.row)
      
    } else {
      type.row <- data.frame(TYPE = t, FLOORSPACE_GAP = 0)
      unmet.fs.gaps <- rbind(unmet.fs.gaps, type.row)
    }
  }
  
  # write demo multipliers to output file
  cnt.row <- MakeRow(demo.mult.df, cnt.name, "DEMO.MULT.50")
  WriteOutput(cnt.row, output.filename, sheet.name = "2050DemoMult")
  
  # write BTL ratios to ouptut file
  cnt.row <- MakeRow(btl.ratio.df, cnt.name, "BTL.RATIO")
  WriteOutput(cnt.row, output.filename, sheet.name = "2020BtoL")
  
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
  
  # write out unmet floorspace requirements df
  cnt.row <- MakeRow(unmet.fs.gaps, cnt.name, "FLOORSPACE_GAP")
  WriteOutput(cnt.row, output.filename, 
              sheet.name = "2050UnmetFloorspace")
  
  
  #---------------------------------------------------------------------------#
  
  
  ############ PHASE III: ASSESS 2050 SIMULATION RESULTS ############  
  # create 2050 building stock
  cnt.sum.50 <- ddply(cnt.raw.df, .(TYPE), .drop = F,
                      summarise, SUM.FNArea.50 = sum(na.omit(FNArea.50)))
  
  # assign AGROUP field vals
  cnt.raw.df <- AssignAgroup(cnt.raw.df)
  
  # write output shapefile for 2050 building stock
  output.sp <- WriteShapefile(cnt.raw.df, cnt.code, 2050)
  
  # add 2016 stock df to output.stock.dfs
  output.stock.df.lst[[3]] <- output.sp
  
  # organize particular county's summary data for export
  cnt.row <- MakeRow(cnt.sum.50, cnt.name, "SUM.FNArea.50")
  
  # write to output file
  WriteOutput(cnt.row, output.filename, sheet.name = "2050Stock")
  
  # add this counties output stock list to metalist
  output.stock.df.lst
                        } 

# export metalist of county level stock data for 2016, 202, and 2050
saveRDS(output.metalist, "stocks_16_20_50.RDS")

})




