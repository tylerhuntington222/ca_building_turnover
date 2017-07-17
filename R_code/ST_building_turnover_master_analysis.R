# system.time({

# TODO:

# change column class types appropriately in data cleaning section
# remove as.character(), as.numeric() calls later on in script

# add fx header for WriteOutput()

# remove parcel.df  param description from function headers
# remove parcel.df arguments from function calls 

# address "ifBtoL is null or 0, use state average for that usetype"

# - Search for BArea.20 and replace with BArea.20.20

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

# PUD usetype in summary tables?



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


########### LOAD LIBRARIES ############
packages <- c("ggmap", "raster", "sp", "ggplot2", "rgeos",
              "spatialEco", "geosphere", "doParallel", "iterators",
              "foreach", "rgdal", "plyr","openxlsx")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

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
require(plyr)
require(doSNOW)
require(openxlsx)

############ SOURCE AUXILIARY FUNCTIONS ############ 
source("ST_bt_functions.R")

############ GLOBAL ENV SETUP ######au###### 

## Init output excel file for exporting intermediate tables
output.wb <- createWorkbook()
output.dir <- "../output_data/"
date <- substr(Sys.time(), 1, 10)
file <- paste0("building_turnover_output_tables_", date,".xlsx")
output.filename <- paste0(output.dir, file)

# init sheets for 2020
sheet1 <- addWorksheet(output.wb, "2016Stock")
sheet2 <- addWorksheet(output.wb, "2020Projected")
sheet3 <- addWorksheet(output.wb, "2020Remain")
sheet4 <- addWorksheet(output.wb, "2020Demo")
sheet5 <- addWorksheet(output.wb, "2020LotsOpen")
sheet6 <- addWorksheet(output.wb, "2020Diff")
sheet7 <- addWorksheet(output.wb, "2020DemoMult")
sheet8 <- addWorksheet(output.wb, "2020Rebuild")
sheet9 <- addWorksheet(output.wb, "2020LotsFilled")
sheet10 <- addWorksheet(output.wb, "2020VacantFilled")
sheet11 <- addWorksheet(output.wb, "2020Deactivated")
sheet12 <- addWorksheet(output.wb, "2016BtoL")
sheet13 <- addWorksheet(output.wb, "2020UnmetFloorspace")
sheet14 <- addWorksheet(output.wb, "2020Stock")

# init sheets for 2050
sheet15 <- addWorksheet(output.wb, "2050Projected")
sheet16 <- addWorksheet(output.wb, "2050Remain")
sheet17 <- addWorksheet(output.wb, "2050Demo")
sheet18 <- addWorksheet(output.wb, "2050LotsOpen")
sheet19 <- addWorksheet(output.wb, "2050Diff")
sheet20 <- addWorksheet(output.wb, "2050DemoMult")
sheet21 <- addWorksheet(output.wb, "2050Rebuild")
sheet22 <- addWorksheet(output.wb, "2050LotsFilled")
sheet23 <- addWorksheet(output.wb, "2050VacantFilled")
sheet24 <- addWorksheet(output.wb, "2050Deactivated")
sheet25 <- addWorksheet(output.wb, "2020BtoL")
sheet26 <- addWorksheet(output.wb, "2050UnmetFloorspace")
sheet27 <- addWorksheet(output.wb, "2050Stock")

# store list of sheet names
output.sheetnames <- names(output.wb)

# save workbook
saveWorkbook(output.wb, paste0(output.filename), overwrite = T)


############ LOAD & CLEAN SUPPLEMENTARY DATA ############

# set name of data file with sheets containing required tables and input data
input.datafile <- "../supp_data/Steps_BuildingStockTurnover_7_6.xlsx"


## Building area to lot area (BTL) ratio tables

# load subtype means
st.btl.df <- read.xlsx(xlsxFile = input.datafile, sheet = "2016Subtype_BTL")

# load typemeans
t.btl.df <- read.xlsx(xlsxFile = input.datafile, sheet = "2016Type_BTL")


## Cutoff yrs table
# load cutoff yrs table
cutoff.yrs <- read.xlsx(xlsxFile = input.datafile, sheet = 2)

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
g.mult.20 <- read.xlsx(xlsxFile = input.datafile, sheet = "2020growthmult")

# load 2050 multipliers
g.mult.50 <- read.xlsx(xlsxFile = input.datafile,
                       sheet = "2050growthmult")

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


## EUI tables
# load 2016 EUI data
eui.16 <- read.xlsx(xlsxFile = input.datafile,
                       sheet = "EUI2016")

# load 2020 EUI data
eui.20 <- read.xlsx(xlsxFile = input.datafile,
                       sheet = "EUI2020")

# load 2050 EUI data
eui.50 <- read.xlsx(xlsxFile = input.datafile,
                       sheet = "EUI2050")


# set col classes of EUI tables
eui.16$JOINB <- as.character(paste(eui.16$JOINB))
eui.20$JOINB <- as.character(paste(eui.20$JOINB))
eui.50$JOINB <- as.character(paste(eui.50$JOINB))


## County abbreviation codes
# load county name/abbreviation table
county.codes.df <- read.xlsx(xlsxFile = input.datafile, sheet = "CountyCodes")

# set col classes to char
county.codes.df$county.name <- as.character(county.codes.df$county.name)
county.codes.df$county.code <- as.character(county.codes.df$county.code)

# load building types-subtypes table (tst)
types.df <- read.xlsx(xlsxFile = input.datafile, sheet = "Building_Types")
tst.df <- types.df[, c("TYPEF", "TYPE")]
tst.df <- na.omit(tst.df)
tst.df <- unique(tst.df)
rownames(tst.df) <- 1:nrow(tst.df)
names(tst.df) <- c("Typef", "TYPE")
tst.df$Typef <- gsub(" ", ".", tst.df$Typef)
tst.df$TYPE <- gsub(" ", ".", tst.df$TYPE)

# generate subtypes table
subtypes <- unique(types.df$TYPEF)
subtypes <- na.omit(subtypes)
subtypes <- subtypes[!(subtypes %in% c("PUD", "Blank"))]
subtypes <- gsub(" ", ".", subtypes)
subtypes.df <- data.frame(Typef = subtypes, stringsAsFactors = F)

# generate types table
types <- unique(types.df$TYPE)
types <- na.omit(types)
types <- types[!(types %in% c("PUD", "Blank"))]
types <- gsub(" ", ".", types)
types.df <- data.frame(TYPE = types, stringsAsFactors = F)


############ LOAD & PROCESS INPUT SHAPEFILES IN PARALLEL ############

# set up processing que from shapefiles in input_shapefiles dir
dir.files <- list.files("../input_shapefiles/")
in.shapes <- dir.files[CheckExt(dir.files)]

# KEEP FOR FINAL DRAFT!
# initialize parallel backend
no.cores <- (detectCores() - 1)
cl <- makeCluster(no.cores, type = "SOCK", outfile="")
registerDoSNOW(cl)

# iterate over shapefiles in 'input_shapefiles' directory
output.metalist <- foreach(shapefile = in.shapes,
                           .packages = c("plyr", "dplyr", "rgdal", "sp",
                                         "raster",  "rgeos", "openxlsx",
                                         "doSNOW"),
                           .export = c("eui.16", "eui.20", "eui.50",
                                       "output.sheetnames")) %dopar% {
                  
# TEMP:
# for (shapefile in in.shapes) {
  
  
  try({
  ############ LOAD PARCEL DATA ############ 
  
  # # KEEP FOR FINAL: load shapefiles from .shp format
  # in.shape.path <- paste0("../input_shapefiles/", shapefile)
  # in.shape.layer <- substr(shapefile, 1, nchar(shapefile)-4)
  # cnt.final <- try(readOGR(dsn = in.shape.path, layer = in.shape.layer))
  
  # TEMP: load shapefiles from binary much faster
  in.shape.path <- paste0("../output_data/binary_shapefiles/", 
                          shapefile, ".RDS")
  
  cnt.final <- readRDS(in.shape.path)
  
  # get class of imported RDS data
  c <- class(cnt.final)[[1]]
  
  # check that a valid shapefile was loaded
  if(c == "SpatialPolygonsDataFrame") {
  
    # extract data part of spatial object
    cnt.final.df <- cnt.final@data
    
    # extract coord ref system
    crs <- crs(cnt.final)
    
    # get county name
    cnt.name <- unique(as.character(cnt.final.df$County))
    
    # determine this county's three letter ID code
    i <- which(county.codes.df$county.name == cnt.name)
    cnt.code <- county.codes.df$county.code[i]
    
    ############ LOAD FUNCTIONS ############
    source("ST_bt_functions.R")
    
    
    ############ CLEAN PARCEL DATA ############ 
    
    # TEMP: test a subset of the parcel data
    percent.data <- 100
    n <- nrow(cnt.final)
    pars.to.test <- sample(1:n, (n*(percent.data/100.0)))
    
    # subset for original parcel data + fields added during preprocessing steps
    las.col <- which(names(cnt.final) == "TYPE")
    typef.col <- which(names(cnt.final) == "Typef")
    altyrfn.col <- which(names(cnt.final) == "altyrfn")
    
    # handle two possibilities for altyrfn col: all lower or all caps
    if (length(altyrfn.col) == 0) {
      altyrfn.col <- which(names(cnt.final) == "ALTYRFN")
    }
    
    # elim post-processing cols to start with
    cnt.raw <- cnt.final[, c(1:las.col, typef.col, altyrfn.col)]
    
    # TEMP: subset parcels for faster testing
    cnt.raw <- cnt.raw[pars.to.test,]
    
    # extract attribute data from sp parcel data
    cnt.raw.df <- cnt.raw@data
    
    # change FIDs to APN codes
    cnt.raw <- spChFIDs(cnt.raw, as.character(cnt.raw$APN))
    
    # extract and spatialize polygons data part from cnt.raw
    cnt.raw.polys <- cnt.raw@polygons
    cnt.raw.sp <- SpatialPolygons(cnt.raw.polys, proj4string = crs)
    
    # change name of ALTYRFN col if it is in all caps for consistency
    if ("ALTYRFN" %in% names(cnt.raw.df)) {
      names(cnt.raw.df)[which(names(cnt.raw.df) == "ALTYRFN")] <- "altyrfn"
    }
    
    # change NAs to zeros in altyrfn col
    cnt.raw.df$altyrfn[is.na(cnt.raw.df$altyrfn)] <- "0"
    
    # change column classes
    cnt.raw.df$TYPE <- as.character(cnt.raw.df$TYPE)
    cnt.raw.df$altyrfn <- as.character(cnt.raw.df$altyrfn)
    cnt.raw.df$LOTAREA <- as.numeric(as.character(cnt.raw.df$LOTAREA))
    cnt.raw.df$APN <- as.character(paste(cnt.raw.df$APN))
    cnt.raw.df$Typef <- as.character(cnt.raw.df$Typef)
    cnt.raw.df$POLY_AREA <- as.numeric(as.character(cnt.raw.df$POLY_AREA))
    cnt.raw.df$POLY_AREA <- round(cnt.raw.df$POLY_AREA, 4)
    
    # change names of BArea and LOTAREA fields to be year-specific
    names(cnt.raw.df)[names(cnt.raw.df) == "BArea"] <- "BArea.16"
    names(cnt.raw.df)[names(cnt.raw.df) == "LOTAREA"] <- "LOTAREA.16"
    
    # change NAs to zeros in applicable numeric cols
    cnt.raw.df$LOTAREA.16[is.na(cnt.raw.df$LOTAREA.16)] <- 0
    
    ## Change building type names in parcel data for consistency
    # TYPE field
    cnt.raw.df$TYPE[cnt.raw.df$TYPE == "Food Srv"] <- "Food.Srv"
    cnt.raw.df$TYPE[cnt.raw.df$TYPE == "Food Sale"] <- "Food.Sale"
    cnt.raw.df$TYPE[cnt.raw.df$TYPE == "Comm Misc"] <- "Comm.Misc"
    cnt.raw.df$TYPE[cnt.raw.df$TYPE == "college"] <- "College"
    cnt.raw.df$TYPE[cnt.raw.df$TYPE == "Food Srv"] <- "Food.Srv"
    cnt.raw.df$TYPE[cnt.raw.df$TYPE == "Food Sale"] <- "Food.Sale"
    cnt.raw.df$TYPE[cnt.raw.df$TYPE == "Comm Misc"] <- "Comm.Misc"
    cnt.raw.df$TYPE[cnt.raw.df$TYPE == "college"] <- "College"
    
    # Typef field
    cnt.raw.df$Typef[cnt.raw.df$Typef == "Food Srv"] <- "Food.Srv"
    cnt.raw.df$Typef[cnt.raw.df$Typef == "Food Sale"] <- "Food.Sale"
    cnt.raw.df$Typef[cnt.raw.df$Typef == "Comm Misc"] <- "Comm.Misc"
    cnt.raw.df$Typef[cnt.raw.df$Typef == "college"] <- "College"
    cnt.raw.df$Typef[cnt.raw.df$Typef == "Food Srv"] <- "Food.Srv"
    cnt.raw.df$Typef[cnt.raw.df$Typef == "Food Sale"] <- "Food.Sale"
    cnt.raw.df$Typef[cnt.raw.df$Typef == "Comm Misc"] <- "Comm.Misc"
    cnt.raw.df$Typef[cnt.raw.df$Typef == "college"] <- "College"
    
    
    
    
    ########## INITIALIZE OUTPUT TEMPLATE ##########
    
    output.list <- vector("list", length(output.sheetnames))
    names(output.list) <- output.sheetnames
    
    ## Step 1. determine 2016 building stock by building type
    cnt.sum.16 <- ddply(cnt.raw.df, .(Typef), .drop = F,
                       summarise, SUM.FS.16 = sum(BArea.16))
    
    cnt.sum.16 <- merge(cnt.sum.16, tst.df, by = "Typef")
    
    # assign AGROUP field vals
    cnt.raw.df <- AssignAgroup(cnt.raw.df)
    
    # write output shapefile for 2016 building stock
    print("Exporting 2016 building stock data...")
    WriteShapefile(cnt.raw.df, cnt.code, 2016)
    
    # organize particular county's data for export
    cnt.row <- MakeRow(cnt.sum.16, cnt.name, "SUM.FS.16", 
                       template.df = subtypes.df, grouping = "Typef")
    
    # add 2016 stock df to output.stock.dfs
    output.list[["2016Stock"]] <- cnt.row
    
    ## Step 2. project 2020 building stock
    print("Applying growth multipliers to project 2020 building stock...")
    
    # init projected floorspace df
    proj.df <- data.frame(Typef = as.character(unique(cnt.raw.df$Typef)), 
                         stringsAsFactors = F)
    
    proj.df <- unique(merge(proj.df, tst.df, by = "Typef", all.x = T))
    
    # susbet growth multipliers for particular county
    g.mult.20.cnt <- GetCountyMultipliers(g.mult.20, cnt.name, 2020)
      
    # merge multipliers to parcel data
    cnt.sum.16 <- merge(cnt.sum.16, g.mult.20.cnt, by = "TYPE", all.x = T)
    cnt.sum.16$SUM.FS.16 <- as.numeric(cnt.sum.16$SUM.FS.16)
    cnt.sum.16$MULT.20 <- as.numeric(as.character(cnt.sum.16$MULT.20))
    
    # multiply 2016 stocks by 2020 multipliers
    cnt.sum.16$PROJ.FS.20 <- NA
    cnt.sum.16$PROJ.FS.20 <- cnt.sum.16$SUM.FS.16 * cnt.sum.16$MULT.20
    PROJ.FS.20.col <- cnt.sum.16[,c("Typef", "PROJ.FS.20")]
    
    # merge 2020 projections with projections dataframe
    proj.df <- merge(proj.df, PROJ.FS.20.col, by = "Typef", all.x=T)
    
    # organize particular county's data for export
    cnt.row <- MakeRow(cnt.sum.16, cnt.name, "PROJ.FS.20", 
                       template.df = subtypes.df, grouping = "Typef")
    
    # write to output file
    output.list[["2020Projected"]] <- cnt.row
    
    ## Step 3. determine if demolished by Years 2020 and 2050
    
    # join 2020cutoff table by usetype
    cutoffs.20 <- cutoff.yrs[, c("TYPE", "Cut20"),]
    cnt.raw.df <- merge(cnt.raw.df, cutoffs.20, by = "TYPE", all.x = T)
    
    # add bin and cut fields
    cnt.raw.df$Bin20 <- 0
    cnt.raw.df$Bin50 <- 0
    
    # check for which parcels reamin in 2020
    cnt.raw.df$Bin20 <- CheckIfRemains(cnt.raw.df$altyrfn, cnt.raw.df$Cut20)
    
    
    ## Step 4. assign 2020 status labels to parcels
    
    # init col to store 2020 status for each parcel
    cnt.raw.df$Status20 <- NA
    
    # iterate over parcels, assigning 2020 statuses
    print("Determining status of parcels in 2020...")
    BinYEAR <- as.numeric(as.character(cnt.raw.df$Bin20))
    BAREA <- as.numeric(as.character(cnt.raw.df$BArea.16))
    LOTAREA <- as.numeric(as.character(cnt.raw.df$LOTAREA.16))
    TYPE <- as.character(cnt.raw.df$TYPE)
    cnt.raw.df$Status20 <- unlist(AssignStatus(BinYEAR, BAREA, LOTAREA, TYPE))
    
    
    ## Step 5: Generate summary stats for parcels in 2020, by Status20 prior to 
    # turnover simulation (i.e. using '16 BArea and LOTAREA.16 vals)
    
    cnt.sum.20 <- ddply(cnt.raw.df, .(Typef, Status20), .drop = F,
                       summarise, SUM.FS.20 = sum(BArea.16), 
                       SUM.LOT.20 = sum (LOTAREA.16))
    
    
    ## Export summary table of remaining buildings in 2020
    remain.20 <- cnt.sum.20[cnt.sum.20$Status20 == "Remain", ] 
    cnt.row <- MakeRow(remain.20, cnt.name, "SUM.FS.20", 
                       template.df = subtypes.df, grouping = "Typef")
    output.list[["2020Remain"]] <- cnt.row
    
    ## Export summary table of demo buildings in 2020
    demo.20 <- cnt.sum.20[cnt.sum.20$Status20 == "Demo", ] 
    cnt.row <- MakeRow(demo.20, cnt.name, "SUM.FS.20",
                       template.df = subtypes.df, grouping = "Typef")
    output.list[["2020Demo"]] <- cnt.row
    
    ## Export summary table of open lots  in 2020
    lots.20 <- cnt.sum.20[cnt.sum.20$Status20 == "LotsOpen", ] 
    cnt.row <- MakeRow(lots.20, cnt.name, "SUM.LOT.20",
                       template.df = subtypes.df, grouping = "Typef")
    output.list[["2020LotsOpen"]] <- cnt.row
    
    # determine additional floorspace needs for 2020
    yr16.data <- subset(cnt.sum.16, 
                        select = c("Typef", "SUM.FS.16", "PROJ.FS.20"))
    yr20.data <- cnt.sum.20[cnt.sum.20$Status20 == "Remain", ]
    fs.diffs.df <- merge(yr16.data, yr20.data, by = "Typef")
    
    # calculate differences between remaining and projected floorspace by type
    fs.diffs.df$DIFF <- fs.diffs.df$PROJ.FS.20 - fs.diffs.df$SUM.FS.20
    
    ## Export summary table of open lots  in 2020
    cnt.row <- MakeRow(fs.diffs.df, cnt.name, "DIFF", 
                       template.df = subtypes.df, grouping = "Typef")
    output.list[["2020Diff"]] <- cnt.row
    
    # create field for bldg areas in 2020
    cnt.raw.df$BArea.20 <- 0
    
    # for parcels with 2020 status Remain, set BArea.20 <- BArea
    rem.20 <- cnt.raw.df[cnt.raw.df$Status20 == "Remain", ]
    rem.20$BArea.20 <- rem.20$BArea.16
    cnt.raw.df[cnt.raw.df$Status20 == "Remain", ] <- rem.20
    
    # init field for lot areas in 2020
    cnt.raw.df$LOTAREA.20 <- cnt.raw.df$LOTAREA.16
    
    print("Simulating building turnover for 2020...")
    
    # set up parcel types to iterate over 
    parcel.subtypes <- fs.diffs.df$Typef
    drops <- c("PUD", "Vacant")
    subtype.iterators <- parcel.subtypes[!(parcel.subtypes %in% drops)]
    
    # init df for storing demo multipliers
    demo.mult.df <- data.frame(Typef = character(), DEMO.MULT.20 = numeric(), 
                              stringsAsFactors = F)
    
    # init df for storing btl ratios
    btl.ratio.df <- data.frame(Typef = character(), BTL.RATIO = numeric(), 
                              stringsAsFactors = F)
    
    # init dataframe to store unmet gaps
    unmet.fs.gaps <- data.frame(Typef = character(), FLOORSPACE_GAP = numeric())
    
    # iterate over building subtypes
    for (st in subtype.iterators) {
      print(sprintf("Working on buildings of subtype: %s", st))
      
      # get parent type of this subtype
      t <- GetParentType(st)
     
      # subset for row of particular building type
      type.row <- fs.diffs.df[fs.diffs.df$Typef == st, ]
      fs.diff <- type.row$DIFF
      
      # calculate bldg area to lot area ratio for usetype
      btl.ratio <- CalcBtlRatio(cnt.raw.df, st, year = 2016)
      
      # TEMP: error handling for no valid btl ratio calculated
      if (is.na(btl.ratio)) {
        btl.ratio <- 0
      }
      
      # add to btl ratio df
      btl.row <- data.frame(Typef = st, BTL.RATIO = btl.ratio)
      btl.ratio.df <- rbind(btl.ratio.df, btl.row)
     
      # case 1: difference is negative
      if (fs.diff < 0) {
        print("Floorspace difference is negative")
        print("Randomly vacating remaining buildings")
        # randomly vacate remaining buildings until diff is positive
        st.diff <- fs.diff
        while (st.diff < 0) {
         
          res <- RandomSwitch(cnt.raw.df, from.stype = st, 
                              from.status = "Remain",
                              to.stype = "Vacant", to.status = "Deactivated", 
                              status.year=2020)
          cnt.raw.df <- res[[1]]
          switched.apn <- res[[2]]
          print(sprintf("Vacated building on parcel with APN: %s", switched.apn))
         
          # check updated floorspace difference following switch
          st.diff <- CheckDiff(cnt.raw.df, st, status = c("Remain"),
                             year = 2020, area.col = "BArea.20")
          print(sprintf("Updated floorspace difference: %s", st.diff))
       }
       
       # add zero value for this usetype's demo multiplier in output table
       c.row <- data.frame(Typef = st, DEMO.MULT.20 = 0)
       demo.mult.df <- rbind(demo.mult.df, c.row)
       
       # case 1b: floorspace difference is positive but less than 10000
       # case 2: floorspace difference is positive
      } else if (fs.diff > 0) {
        print("Floorspace difference is positive")
        print("Calculating Gap-to-demo ratio...")
        demo.20.df <- subset(cnt.sum.20, (Status20 == "Demo") & (Typef == st))
        demo.sum.20 <- sum(demo.20.df$SUM.FS.20)
        ratio <- fs.diff/demo.sum.20
        print(sprintf("Gap-to-demo ratio is: %s", ratio))
        
        # add to demo mult df
        c.row <- data.frame(Typef = st, DEMO.MULT.20 = ratio)
        demo.mult.df <- rbind(demo.mult.df, c.row)
       
        # case 2a: GTD ratio is less than 1
        if (ratio < 1) {
          print("GTD ratio is <1; rebuilding demos to close floorspace gap")
         
          # init floorspace difference tracker for this usetype
          st.diff <- CheckDiff(cnt.raw.df, st, status = c("Remain"),
                              year = 2020, area.col = "BArea.20")
         
          # rebuild demo buildings until difference gap is closed
          while (st.diff > 0) {
            res <- SizeRankedSwitch(cnt.raw.df, from.stype = st, 
                                    from.status = "Demo", to.stype = st, 
                                    to.status = "Rebuild", year = 2020, 
                                    area.col <- "BArea.16")
            cnt.raw.df <- res[[1]]
            rebuilt.apn <- res[[2]]
            print(sprintf("Building with APN: %s rebuilt", rebuilt.apn))
           
            # update BArea.20, altyrfn and Bin50 fields of rebuild
            rebuild <- cnt.raw.df[(cnt.raw.df$APN == rebuilt.apn), ]
            rebuild$BArea.20 <- rebuild$BArea.16
            rebuild$altyrfn <- "2020"
            rebuild$Bin50 <- "1"
            cnt.raw.df[(cnt.raw.df$APN == rebuilt.apn), ] <- rebuild
           
            # check floorspace difference
            st.diff <- CheckDiff(cnt.raw.df, st, status = c("Remain", "Rebuild"),
                                year=2020, area.col="BArea.20")
            print(sprintf("Updated floorspace difference: %s", st.diff))
           
            # init expansion factor and step var to increase by on each pass
            expand.factor <- 1.2
            expand.step <- 0.2
            max.expand.factor <- 4
           
            # expand rebuilt building size until max expansion factor reached
            # or floorspace difference gap is closed
            while((st.diff > 0) & (expand.factor <= max.expand.factor)) {
              print(sprintf("Expanding area of rebuild by factor of %s", 
                            expand.factor))
              cnt.raw.df <- ExpandArea(cnt.raw.df, rebuilt.apn, expand.factor,
                                      from.area.col="BArea.16", 
                                      to.area.col="BArea.20")
              # increase expansion factor 
              expand.factor <- expand.factor + expand.step
              st.diff <- CheckDiff(cnt.raw.df, st, status = c("Remain", "Rebuild"),
                                 year=2020, area.col="BArea.20")
              print(sprintf("Updated floorspace difference: %s", st.diff))
            }
          }
         
        # case 2b: GTD ratio is betwen 1 and 4 (inclusive)
        } else if ((ratio >= 1) & (ratio <= 4)) {
          print("GTD ratio is >= 1 & <= 4; upsizing rebuilds to close gap")
         
          # expand areas of rebuilds to close gap
          cnt.raw.df <- RebuildMultiplied(cnt.raw.df, stype = st, 
                                          from.status = "Demo", 
                                          to.status = "Rebuild",
                                          mult = ratio, year = 2020,
                                          from.area.col = "BArea.16", 
                                          to.area.col = "BArea.20")
         
        # case 2c: GTD ratio is greater than 4
        } else if (ratio > 4) {
          print("Gap-to-demo ratio is >4; allocating demos to rebuilds first")
         
          # first allocate all demos if any exist
          res <- RebuildMultiplied(cnt.raw.df, stype = st, 
                                  from.status = "Demo", to.status = "Rebuild",
                                  mult = 4, year = 2020, 
                                  from.area.col = "BArea.16", 
                                  to.area.col="BArea.20")
         
          # only update master parcel df if rebuilds were performed
          if (res != "None") {
           cnt.raw.df <- res
          }
         
          ## Second, fill open lots
          print("Filling open lots of same usetype second")
         
          # check whether there is avail lot area to convert
          avail.lots <- CheckForLots(st)
         
          # init floorspace difference tracker for this usetype
          st.diff <- CheckDiff(cnt.raw.df, st, status = c("Remain", "Rebuild"),
                             year = 2020, area.col = "BArea.20")
         
          # convert open lots up to allowable BTL ratios until FS difference met
          # or convertable lot area runs out.
          while ((st.diff > 0) & (avail.lots)) {
            res <- ConvertLot(cnt.raw.df, stype = st, btl.ratio, 
                             stock.year = 2016, sim.year=2020)
            # check if no lots were available to convert
            if (res[[1]] == "None") {
              avail.lots = FALSE
             
           } else {
             # update master parcel df
             cnt.raw.df <- res[[1]] 
             converted.apn <- res[[2]]
             print(sprintf("Allocated FS from open lot on parcel with APN: %s", 
                           converted.apn))
             
             # update altyrfn and Bin50 fields of parcel with converted lot area
             converted <- cnt.raw.df[(cnt.raw.df$APN == converted.apn), ]
             converted$altyrfn <- "2020"
             converted$Bin50 <- "1"
             cnt.raw.df[(cnt.raw.df$APN == converted.apn), ] <- converted
             
             # check floorspace difference
             st.diff <- CheckDiff(cnt.raw.df, st, 
                                 status = c("Remain", "Rebuild", "LotsFilled"),
                                 year = 2020, area.col = "BArea.20")
             print(sprintf("Updated floorspace difference: %s", st.diff))
           }
         }
         
         ## lastly, allocate remaining FS needs to parcels with ustype "Vacant" 
         # if current type is non-industrial
         
         # if parent type of current subtype is non-industrial, allocate vacancies
         if (t != "Ind") {
           
           # check whether there are avail vacancies to convert
           avail.vacancies <- CheckForVacancies(t)
           
           while ((st.diff > 0) & (avail.vacancies)) {
             print(sprintf("Floorspace difference is still positive: %s", 
                           st.diff))
             print(sprintf("Allocating vacancies to floorspace of type: %s", 
                           st))
             
             # convert vacant parcel to active floorspace
             res <- FillVacant(cnt.raw.df, to.stype = st, 
                               btl.ratio = btl.ratio, 
                               stock.year = 2016, sim.year = 2020)
             
             if (res[[1]] == "None") {
               avail.vacancies <- FALSE
             } else {
               # update parcel data after vacancy filled
               cnt.raw.df <- res[[1]]
               filled.apn <- res[[2]]
               print(sprintf("Allocated FS from vacant lot on parcel w/ APN: %s", 
                             filled.apn))
               # check floorspace difference
               st.diff <- CheckDiff(cnt.raw.df, st, 
                                   status = c("Remain", "Rebuild", 
                                              "LotsFilled", "VacantFilled"),
                                   year = 2020, area.col = "BArea.20")
               print(sprintf("Updated floorspace difference: %s", st.diff))
             }
           }
         }
       }
     }
     
     ## if there is still unmet floorspace requirements, export in table
     # check fs difference
     st.diff <- CheckDiff(cnt.raw.df, st, 
                         status = c("Remain", "Rebuild", 
                                    "LotsFilled", "VacantFilled"),
                         year = 2020, area.col = "BArea.20")
     
     # generate row for this type if st.diff still positive
     if (st.diff > 0) {
       type.row <- data.frame(Typef = st, FLOORSPACE_GAP = st.diff)
       unmet.fs.gaps <- rbind(unmet.fs.gaps, type.row)
     } else {
       type.row <- data.frame(Typef = st, FLOORSPACE_GAP = 0)
       unmet.fs.gaps <- rbind(unmet.fs.gaps, type.row)
     }
    }
    
    # write demo multipliers to output file
    cnt.row <- MakeRow(demo.mult.df, cnt.name, "DEMO.MULT.20",
                       template.df = subtypes.df, grouping = "Typef")
    output.list[["2020DemoMult"]] <- cnt.row
    
    # write BTL ratios to ouptut file
    cnt.row <- MakeRow(btl.ratio.df, cnt.name, "BTL.RATIO",
                       template.df = subtypes.df, grouping = "Typef")
    output.list[["2016BtoL"]] <- cnt.row
    
    # calc summary stats for 2020 statuses after simulation
    ## Step 1. determine 2020 building stock by building type
    cnt.sum.20.post <- ddply(cnt.raw.df, .(Typef, Status20), .drop=F, summarise, 
                            SUM.FS.20 = sum(na.omit(BArea.20)), 
                            SUM.LOT.20 = sum(na.omit(LOTAREA.20)))
    
    
    # write rebuilds to output table
    rebuilds.20 <- cnt.sum.20.post[cnt.sum.20.post$Status20 == "Rebuild",]
    if (nrow(rebuilds.20) == 0) {
     cnt.row <- MakeZeroRow(subtypes.df$Typef, cnt.name)
     output.list[["2020Rebuild"]] <- cnt.row 
     
    } else {
     cnt.row <- MakeRow(rebuilds.20, cnt.name, "SUM.FS.20", 
                        template.df = subtypes.df, grouping = "Typef")
     output.list[["2020Rebuild"]] <- cnt.row 
    }
    
    # write lots filled to output table
    lotsfilled.20 <- cnt.sum.20.post[cnt.sum.20.post$Status20 == "LotsFilled",]
    if (nrow(lotsfilled.20) == 0) {
     cnt.row <- MakeZeroRow(subtypes.df$Typef, cnt.name)
     output.list[["2020LotsFilled"]] <- cnt.row
     
    } else {
     cnt.row <- MakeRow(lotsfilled.20, cnt.name, "SUM.FS.20",
                        template.df = subtypes.df, grouping = "Typef")
     output.list[["2020Deactivated"]] <- cnt.row
    }
    
    # write vacancies filled to output table
    vacfilled.20 <- cnt.sum.20.post[cnt.sum.20.post$Status20 == "VacantFilled",]
    if (nrow(vacfilled.20) == 0) {
     cnt.row <- MakeZeroRow(subtypes.df$Typef, cnt.name)
     output.list[["2020VacantFilled"]] <- cnt.row
     
    } else {
     cnt.row <- MakeRow(vacfilled.20, cnt.name, "SUM.FS.20", 
                        template.df = subtypes.df, grouping = "Typef")
     output.list[["2020VacantFilled"]] <- cnt.row
    }
    
    # write deactivated bldgs to output table
    deactiv.20 <- cnt.sum.20.post[cnt.sum.20.post$Status20 == "Deactivated",]
    if (nrow(deactiv.20) == 0) {
     cnt.row <- MakeZeroRow(subtypes.df$Typef, cnt.name)
     output.list[["2020Deactivated"]] <- cnt.row 
     
    } else {
     cnt.row <- MakeRow(deactiv.20, cnt.name, "SUM.FS.20", 
                        template.df = subtypes.df, grouping = "Typef")
     output.list[["2020Deactivated"]] <- cnt.row
    }
    
    # write out unment floorspace requirements df
    cnt.row <- MakeRow(unmet.fs.gaps, cnt.name, "FLOORSPACE_GAP", 
                       template.df = subtypes.df, grouping = "Typef")
    
    output.list[["2020UnmetFloorspace"]] <- cnt.row
    
    
    #---------------------------------------------------------------------------#
    
    
    
    ############ PHASE II: PROJECT TO 2050 ######
    print("Starting Phase II: Determining 2020 building stock")
    
    ## Step 1. Determine 2020 building stock
    cnt.sum.20 <- ddply(cnt.raw.df, .(Typef), .drop = F,
                       summarise, SUM.BArea.20 = sum(na.omit(BArea.20)))
    
    cnt.sum.20 <- merge(cnt.sum.20, tst.df, by = "Typef")
    
    # assign AGROUP field vals
    cnt.raw.df <- AssignAgroup(cnt.raw.df)
    
    # write output shapefile for 2016 building stock
    output.sp <- WriteShapefile(cnt.raw.df, cnt.code, 2020)
    
    # organize particular county's data for export
    cnt.row <- MakeRow(cnt.sum.20, cnt.name, "SUM.BArea.20",
                       template.df = subtypes.df, grouping = "Typef")
    
    # write to output file
    output.list[["2020Stock"]] <- cnt.row
    
    ## Step 2. project 2050 building stock
    
    # get 2050 growth multiplier for county
    g.mult.50.cnt <- GetCountyMultipliers(g.mult.50, cnt.name, 2050)
    
    # merge multipliers to parcel data
    cnt.sum.20 <- merge(cnt.sum.20, g.mult.50.cnt, by = "TYPE", all.x = T)
    cnt.sum.20$SUM.BArea.20.20 <- as.numeric(cnt.sum.20$SUM.BArea.20)
    cnt.sum.20$MULT.50 <- as.numeric(as.character(cnt.sum.20$MULT.50))
    
    # multiply 2016 stocks by 2020 multipliers
    cnt.sum.20$PROJ.FS.50 <- NA
    cnt.sum.20$PROJ.FS.50 <- cnt.sum.20$SUM.BArea.20 * cnt.sum.20$MULT.50
    PROJ.FS.50.col <- cnt.sum.20[,c("Typef", "PROJ.FS.50")]
    
    # merge 2020 projections with projections dataframe
    proj.df <- merge(proj.df, PROJ.FS.50.col, by="Typef", all.x = T)
    
    # organize particular county's data for export
    cnt.row <- MakeRow(cnt.sum.20, cnt.name, "PROJ.FS.50",
                       template.df = subtypes.df, grouping = "Typef")
    
    # write to output file
    output.list[["2050Projected"]] <- cnt.row
    
    ## Step 3. determine if demolished by 2050
    
    # join 2050 cutoff table by usetype
    cutoffs.50 <- cutoff.yrs[, c("TYPE", "Cut50"),]
    cnt.raw.df <- merge(cnt.raw.df, cutoffs.50, by = "TYPE", all.x = T)
    
    # iterate over all parcels, checking if they are demo'd by 2050
    cnt.raw.df$Bin50 <- CheckIfRemains(cnt.raw.df$altyrfn, cnt.raw.df$Cut50)
    
    ## Step 4. assign 2020 status labels to parcels
    
    # init col to store 2050 status for each parcel
    cnt.raw.df$Status50 <- NA
    
    # assign 2050 statuses to parcels
    print("Determining status of parcels in 2050...")
    BinYEAR <- as.numeric(as.character(cnt.raw.df$Bin50))
    BAREA <- as.numeric(as.character(cnt.raw.df$BArea.20))
    LOTAREA <- as.numeric(as.character(cnt.raw.df$LOTAREA.20))
    TYPE <- as.character(cnt.raw.df$TYPE)
    cnt.raw.df$Status50 <- unlist(AssignStatus(BinYEAR, BAREA, LOTAREA, TYPE))
    
    ## Step 5: Generate summary stats for building stock in 2050, by Status50
    cnt.sum.50 <- ddply(cnt.raw.df, .(Typef, Status50), .drop = F,
                       summarise, SUM.FS.50 = sum(BArea.20),
                       SUM.LOT.50 = sum(na.omit(LOTAREA.20)))
    
    ## Export summary table of remaining buildings in 2050
    remain.50 <- cnt.sum.50[cnt.sum.50$Status50 == "Remain", ] 
    cnt.row <- MakeRow(remain.50, cnt.name, "SUM.FS.50", 
                       template.df = subtypes.df, grouping = "Typef")
    
    output.list[["2050Remain"]] <- cnt.row
    
    ## Export summary table of demo buildings in 2050
    demo.50 <- cnt.sum.50[cnt.sum.50$Status50 == "Demo", ] 
    cnt.row <- MakeRow(demo.50, cnt.name, "SUM.FS.50",
                       template.df = subtypes.df, grouping = "Typef")
    output.list[["2050Demo"]] <- cnt.row
    
    ## Export summary table of open lots  in 2050
    lots.50 <- cnt.sum.50[cnt.sum.50$Status50 == "LotsOpen", ] 
    cnt.row <- MakeRow(lots.50, cnt.name, "SUM.LOT.50",
                       template.df = subtypes.df, grouping = "Typef")
    output.list[["2050LotsOpen"]] <- cnt.row
    
    # add field BArea.50
    cnt.raw.df$BArea.50 <- 0
    
    # for parcels with 2050 status Remain, set BArea.50
    rem.50 <- cnt.raw.df[cnt.raw.df$Status50 == "Remain", ]
    rem.50$BArea.50 <- rem.50$BArea.20
    cnt.raw.df[cnt.raw.df$Status50 == "Remain", ] <- rem.50
    
    # init field for lot areas in 2050
    cnt.raw.df$LOTAREA.50 <- cnt.raw.df$LOTAREA.20
    
    # determine additional floorspace needs for 2050
    yr20.data <- subset(cnt.sum.20, 
                       select = c("Typef", "SUM.BArea.20.20", "PROJ.FS.50"))
    yr50.data <- cnt.sum.50[cnt.sum.50$Status50 == "Remain", ]
    fs.diffs.df <- merge(yr20.data, yr50.data, by = "Typef")
    
    # calculate differences between remaining and projected floorspace by type
    fs.diffs.df$DIFF <- fs.diffs.df$PROJ.FS.50 - fs.diffs.df$SUM.FS.50
    
    ## Export summary table of 2050 floorspace differences
    cnt.row <- MakeRow(fs.diffs.df, cnt.name, "DIFF",
                       template.df = subtypes.df, grouping = "Typef")
    
    output.list[["2050Diff"]] <- cnt.row
    
    print("Simulating building turnover for 2050...")
    
    # set up parcel types to iterate over 
    parcel.types <- fs.diffs.df$Typef
    subtype.iterators <- parcel.types[!(parcel.types %in% c("PUD", "Vacant"))]
    
    # init df for storing demo multipliers
    demo.mult.df <- data.frame(Typef = character(), DEMO.MULT.50 = numeric(), 
                              stringsAsFactors = F)
    
    # init df for storing btl ratios
    btl.ratio.df <- data.frame(Typef = character(), BTL.RATIO = numeric(), 
                              stringsAsFactors = F)
    
    # init dataframe to store unmet gaps
    unmet.fs.gaps <- data.frame(Typef = character(), FLOORSPACE_GAP = numeric())
    
    # iterate over building types
    for (st in subtype.iterators) {
      print(sprintf("Working on buildings of subtype: %s", st))
      
      t <- GetParentType(st)
      
      # subset for row of particular building type
      type.row <- fs.diffs.df[fs.diffs.df$Typef == st, ]
      fs.diff <- type.row$DIFF
      
      # calculate BArea to LotArea ratio for usetype
      print("Calculating building to lot ratio..")
      btl.ratio <- CalcBtlRatio(cnt.raw.df, st, year = 2016)
      
      # TEMP: error handling for NA or zero btl ratio calculated
      if (is.na(btl.ratio)) {
        btl.ratio <- 0
      }
      
      if (btl.ratio == 0) {
        btl.ratio <- 0
      }
      
      
      # add to btl ratio df
      btl.row <- data.frame(Typef = st, BTL.RATIO = btl.ratio)
      btl.ratio.df <- rbind(btl.ratio.df, btl.row)
     
     # case 1: difference is negative
     if (fs.diff < 0) {
       print("Floorspace difference is negative")
       print(sprintf("Randomly vacating remaining buildings of subtype: %s", 
                     st))
       # randomly vacate remaining buildings until diff is positive
       st.diff <- fs.diff
       while (st.diff < 0) {
         res <- RandomSwitch(cnt.raw.df, from.stype = st, 
                             from.status = "Remain",
                             to.stype = st, to.status = "Deactivated", 
                             status.year = 2050)
         cnt.raw.df <- res[[1]]
         switched.apn <- res[[2]]
         print(sprintf("Vacated building on parcel with APN: %s", switched.apn))
         
         # check updated floorspace difference following switch
         st.diff <- CheckDiff(cnt.raw.df, st, status = c("Remain"), 
                             year=2050, area.col="BArea.50")
         print(sprintf("Updated floorspace difference: %s", st.diff))
         
       }
       
       # add zero value for this usetype's demo multiplier in output table
       c.row <- data.frame(Typef = st, DEMO.MULT.50 = 0)
       demo.mult.df <- rbind(demo.mult.df, c.row)
       
    # case 2: floorspace difference is positive
     } 
      else if (fs.diff > 0) {
       print("Floorspace difference is positive")
       demo.50.df <- subset(cnt.sum.50, (Status50 == "Demo") & (Typef == st))
       demo.sum.50 <- sum(demo.50.df$SUM.FS.50)
       
       print("Calculating Gap-to-demo ratio...")
       ratio <- fs.diff/demo.sum.50
       
       print(sprintf("Gap-to-demo ratio is: %s", ratio))
       
       # add to demo mult df
       c.row <- data.frame(Typef = st, DEMO.MULT.50 = ratio)
       demo.mult.df <- rbind(demo.mult.df, c.row)
       
       # case 2a: GTD ratio is less than 1
       if (ratio < 1) {
         print("GTD ratio is <1; rebuilding demos to close floorspace gap")
         
         # init floorspace difference tracker for this usetype
         st.diff <- CheckDiff(cnt.raw.df, st, status = c("Remain"),
                             year=2050, area.col="BArea.50")
         
         # rebuild demo buildings until difference gap is closed
         while (st.diff > 0) {
           print("Rebuilding demo buildings until difference gap is closed")
           res <- SizeRankedSwitch(cnt.raw.df, from.stype = st, 
                                   from.status = "Demo", to.stype = st, 
                                   to.status = "Rebuild", year=2050, 
                                   area.col="BArea.20")
           cnt.raw.df <- res[[1]]
           rebuilt.apn <- res[[2]]
           print(sprintf("Building with APN: %s rebuilt", rebuilt.apn))
           
           # update BArea.20, altyrfn and Bin50 fields of rebuild
           rebuild <- cnt.raw.df[(cnt.raw.df$APN == rebuilt.apn), ]
           rebuild$BArea.20 <- rebuild$BArea.16
           rebuild$altyrfn <- "2050"
           rebuild$Bin50 <- "1"
           cnt.raw.df[(cnt.raw.df$APN == rebuilt.apn), ] <- rebuild
           
           # check floorspace difference
           st.diff <- CheckDiff(cnt.raw.df, st, status = c("Remain", "Rebuild"), 
                               year = 2050, area.col = "BArea.50")
           print(sprintf("Updated floorspace difference: %s", st.diff))
           
           # init expansion factor and step var to increase by on each pass
           expand.factor <- 1.2
           expand.step <- 0.2
           max.expand.factor <- 8
           
           # expand rebuilt building size until max expansion factor reached
           # or floorspace difference gap is closed
           while((st.diff > 0) & (expand.factor <= max.expand.factor)) {
             print(sprintf("Expanding area of rebuild by factor of %s", 
                           expand.factor))
             cnt.raw.df <- ExpandArea(cnt.raw.df, rebuilt.apn, expand.factor,
                                      from.area.col = "BArea.20",
                                      to.area.col = "BArea.50")
             # increase expansion factor 
             expand.factor <- expand.factor + expand.step
             st.diff <- CheckDiff(cnt.raw.df, st, 
                                  status = c("Remain", "Rebuild"),
                                 year = 2050, area.col = "BArea.50")
             print(sprintf("Updated floorspace difference: %s", st.diff))
           }
         }
         
         # case 2b: GTD ratio is betwen 1 and 8 (inclusive)
       } else if ((ratio >= 1) & (ratio <= 8)) {
         print("GTD ratio is >= 1 & <= 8; upsizing rebuilds to close gap")
         
         # expand areas of rebuilds to close gap
         cnt.raw.df <- RebuildMultiplied(cnt.raw.df, stype = st, 
                                         from.status = "Demo", 
                                         to.status = "Rebuild",
                                         mult = ratio, year = 2050, 
                                         from.area.col = "BArea.20",
                                         to.area.col = "BArea.50")
         
         # case 2c: GTD ratio is greater than 8
       } else if (ratio > 8) {
         print("Gap-to-demo ratio is > 8; allocating demos to rebuilds first")
         
         # first allocate all demos if any exist
         res <- RebuildMultiplied(cnt.raw.df, stype = st, 
                                  from.status = "Demo", to.status = "Rebuild",
                                  mult = 8, year=2050, from.area.col="BArea.20",
                                  to.area.col="BArea.50")
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
         st.diff <- CheckDiff(cnt.raw.df, st, status = c("Remain", "Rebuild"),
                             year = 2050, area.col = "BArea.50")
         
         # convert open lots up to allowable BTL ratios until FS difference  met
         # or convertable lot area runs out.
         while ((st.diff > 0) & (avail.lots)) {
           res <- ConvertLot(cnt.raw.df, stype = st, btl.ratio,
                             stock.year = 2020, sim.year = 2050)
           
           # check if no lots were available to convert
           if (res[[1]] == "None") {
             avail.lots = FALSE
             
           } else {
             # update master parcel df
             cnt.raw.df <- res[[1]] 
             converted.apn <- res[[2]]
             print(sprintf("Allocated FS from open lot on parcel with APN: %s", 
                           converted.apn))
             
             # update altyrfn and Bin50 fields of parcel with converted lot area
             converted <- cnt.raw.df[(cnt.raw.df$APN == converted.apn), ]
             converted$altyrfn <- "2050"
             cnt.raw.df[(cnt.raw.df$APN == converted.apn), ] <- converted
             
             # check floorspace difference
             st.diff <- CheckDiff(cnt.raw.df, st, 
                                 status = c("Remain", "Rebuild", "LotsFilled"),
                                 year=2050, area.col="BArea.50")
             print(sprintf("Updated floorspace difference: %s", st.diff))
           }
         }
         
         ## lastly, allocate remaining FS needs to parcels with ustype "Vacant"
         # if the current type in non-industrial
         
         # check if the current type is non-industrial
         if (t != "Ind") {
           
           # check whether there are avail vacancies to convert
           avail.vacancies <- CheckForVacancies(t)
           
           while ((st.diff > 0) & (avail.vacancies)) {
             print(sprintf("Floorspace difference is still positive: %s", 
                           st.diff))
             print(sprintf("Allocating vacancies to floorspace of type: %s", 
                           t))
             
             # convert vacant parcel to active floorspace
             res <- FillVacant(cnt.raw.df, to.stype = st, 
                               btl.ratio = btl.ratio, 
                               stock.year = 2020, sim.year = 2050)
             
             if (res[[1]] == "None") {
               avail.vacancies <- FALSE
               
             } else {
               # update parcel data
               cnt.raw.df <- res[[1]]
               filled.apn <- res[[2]]
               print(sprintf("Allocated FS from vacant lot on parcel w/ APN: %s", 
                             filled.apn))
               
               
               # check floorspace difference
               st.diff <- CheckDiff(cnt.raw.df, st, 
                                   status = c("Remain", "Rebuild", 
                                              "LotsFilled", "VacantFilled"),
                                   year = 2050, area.col = "BArea.50")
               print(sprintf("Updated floorspace difference: %s", st.diff))
             }
           }
         } 
       }
     }
     ## if there is still unmet floorspace requirements, export in table
     # check diff
     st.diff <- CheckDiff(cnt.raw.df, st, 
                         status = c("Remain", "Rebuild", 
                                    "LotsFilled", "VacantFilled"),
                         year = 2050, area.col = "BArea.50")
     
     # generate row for this type if st.diff still positive
     if (st.diff > 0) {
       type.row <- data.frame(Typef = st, FLOORSPACE_GAP = st.diff)
       unmet.fs.gaps <- rbind(unmet.fs.gaps, type.row)
       
     } else {
       type.row <- data.frame(Typef = st, FLOORSPACE_GAP = 0)
       unmet.fs.gaps <- rbind(unmet.fs.gaps, type.row)
     }
    }
    
    # write demo multipliers to output list
    cnt.row <- MakeRow(demo.mult.df, cnt.name, "DEMO.MULT.50",
                       template.df = subtypes.df, grouping = "Typef")
    output.list[["2050DemoMult"]] <- cnt.row
    
    # write BTL ratios to ouptut file
    cnt.row <- MakeRow(btl.ratio.df, cnt.name, "BTL.RATIO",
                       template.df = subtypes.df, grouping = "Typef")
    output.list[["2020BtoL"]] <- cnt.row
    
    # calc summary stats for 2050 statuses after simulation
    cnt.sum.50.post <- ddply(cnt.raw.df, .(Typef, Status50), .drop=F, summarise, 
                            SUM.FS.50 = sum(na.omit(BArea.20)))
    
    # write rebuilds to output table
    rebuilds.50 <- cnt.sum.50.post[cnt.sum.50.post$Status50 == "Rebuild",]
    if (nrow(rebuilds.50) == 0) {
     cnt.row <- MakeZeroRow(subtypes.df$Typef, cnt.name)
     output.list[["2050Rebuild"]] <- cnt.row
     
    } else {
     cnt.row <- MakeRow(rebuilds.50, cnt.name, "SUM.FS.50",
                        template.df = subtypes.df, grouping = "Typef")
     output.list[["2050Rebuild"]] <- cnt.row
    }
    
    # write lots filled to output table
    lotsfilled.50 <- cnt.sum.50.post[cnt.sum.50.post$Status50 == "LotsFilled",]
    if (nrow(lotsfilled.50) == 0) {
     cnt.row <- MakeZeroRow(subtypes.df$Typef, cnt.name)
     output.list[["2050LotsFilled"]] <- cnt.row 
     
    } else {
     cnt.row <- MakeRow(lotsfilled.50, cnt.name, "SUM.FS.50",
                        template.df = subtypes.df, grouping = "Typef")
     output.list[["2050Deactivated"]] <- cnt.row
    }
    
    # write vacancies filled to output table
    vacfilled.50 <- cnt.sum.50.post[cnt.sum.50.post$Status50 == "VacantFilled",]
    if (nrow(vacfilled.50) == 0) {
     cnt.row <- MakeZeroRow(subtypes.df$Typef, cnt.name)
     output.list[["2050VacantFilled"]] <- cnt.row
     
    } else {
     cnt.row <- MakeRow(vacfilled.50, cnt.name, "SUM.FS.50",
                        template.df = subtypes.df, grouping = "Typef")
     output.list[["2050VacantFilled"]] <- cnt.row
    }
    
    # write deactivated bldgs to output table
    deactiv.50 <- cnt.sum.50.post[cnt.sum.50.post$Status50 == "Deactivated",]
    
    if (nrow(deactiv.50) == 0) {
     cnt.row <- MakeZeroRow(subtypes.df$Typef, cnt.name)
     output.list[["2050Deactivated"]] <- cnt.row 
     
    } else {
     cnt.row <- MakeRow(deactiv.50, cnt.name, "SUM.FS.50",
                        template.df = subtypes.df, grouping = "Typef")
     output.list[["2050Deactivated"]] <- cnt.row 
    }
    
    # write out unmet floorspace requirements df
    cnt.row <- MakeRow(unmet.fs.gaps, cnt.name, "FLOORSPACE_GAP",
                       template.df = subtypes.df, grouping = "Typef")
    output.list[["2050UnmetFloorspace"]] <- cnt.row
    
    
    #---------------------------------------------------------------------------#
    
    
    ############ PHASE III: ASSESS 2050 SIMULATION RESULTS ############  
    # create 2050 building stock
    cnt.sum.50 <- ddply(cnt.raw.df, .(Typef), .drop = F,
                       summarise, SUM.BArea.50 = sum(na.omit(BArea.50)))
    
    # assign AGROUP field vals
    cnt.raw.df <- AssignAgroup(cnt.raw.df)
    
    # write output shapefile for 2050 building stock
    WriteShapefile(cnt.raw.df, cnt.code, 2050)
    
    
    # organize particular county's summary data for export
    cnt.row <- MakeRow(cnt.sum.50, cnt.name, "SUM.BArea.50",
                       template.df = subtypes.df, grouping = "Typef")
    
    # write to output file
    output.list[["2050Stock"]] <- cnt.row
    
    # add this counties output stock list to metalist
    output.list
  } else {
    output.list <- "No_Shapefile_Features"
    output.list
  }
  })
} 

# write output to excel spreadsheet
for (c in 1:length(output.metalist)) {
  cnty.output <- output.metalist[[c]]
  if (cnty.output != "No_Shapefile_Features") {
    for (s in 1:length(cnty.output)) {
      sheet <- names(cnty.output[s])
      data.row <- cnty.output[[s]]
      WriteOutput(data.row, output.filename, sheet)
    }
  }
}
  
# # stop cluster
stopCluster(cl)
 
# # stop system timer 
# })




