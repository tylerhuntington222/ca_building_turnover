
#-----------------------------------------------------------------------------#
# calc_subtype_btl_ratios_IO.R

# Type: 

# AUTHOR:
# Tyler Huntington, 2017
# Lawrence Berkeley National Lab Sustainable Energy Group
# Project: California Building Turnover Study
# Principal Investigator: Hanna Breunig

# PURPOSE: An I/O script to determine the median building to lot ratio for
# each CA county and then calculate state medians by building subtype.
# Zeros, null values and outliers, defined as values that are > 1.5*IQR 
# away from the median are excluded at both the county and state level 
# calculations

# RETURNS:


# OUTPUTS:



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
packages <- c("raster", "sp", "ggplot2", "rgeos",
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

############ LOAD SUPPLEMENTARY DATA ############ 

# load building types-subtypes table (tst)
input.datafile <- "../supp_data/Steps_BuildingStockTurnover_7_6.xlsx"
types.df <- read.xlsx(xlsxFile = input.datafile, sheet = "Building_Types")
tst.df <- types.df[, c("TYPEF", "TYPE")]
tst.df <- na.omit(tst.df)
tst.df <- unique(tst.df)
rownames(tst.df) <- 1:nrow(tst.df)
names(tst.df) <- c("Typef", "TYPE")
tst.df$Typef <- gsub(" ", ".", tst.df$Typef)
tst.df$TYPE <- gsub(" ", ".", tst.df$TYPE)
tst.df <- subset(tst.df, tst.df$Typef != "SmOffice")


############ SET PARAMS ############ 

# part 1: calculate county level btl ratios for each subtype 


# initialize parallel backend
no.cores <- (detectCores() - 1)
cl <- makeCluster(no.cores, type = "SOCK", outfile="")
registerDoSNOW(cl)

# iterate over county shapefiles
# set up processing que from shapefiles in input_shapefiles dir
dir.files <- list.files("../input_shapefiles/")
in.shapes <- dir.files[CheckExt(dir.files)]

  
# iterate over shapefiles in 'input_shapefiles' directory
st.btl.df <- foreach (shapefile = in.shapes, 
                      .combine = "rbind",
                      .errorhandling = "remove") %dopar% {
  
  # # KEEP FOR FINAL: load shapefiles from .shp format
  # in.shape.path <- paste0("../input_shapefiles/", shapefile)
  # in.shape.layer <- substr(shapefile, 1, nchar(shapefile)-4)
  # cnt.final <- try(readOGR(dsn = in.shape.path, layer = in.shape.layer))
  

  # TEMP: load shapefiles from binary much faster
  in.shape.path <- paste0("../output_data/binary_shapefiles/", 
                          shapefile, ".RDS")
  
  c.shape <- readRDS(in.shape.path)
                        
  # get class of imported RDS data
  c <- class(c.shape)[[1]]
  
  # check that a valid shapefile was loaded
  if(c == "SpatialPolygonsDataFrame") {
    
    # extract data part of spatial object
    c.pars <- c.shape@data
    
    # change class of cols
    c.pars$Typef <- as.character(c.pars$Typef)
    c.pars$TYPE <- as.character(c.pars$TYPE)
    
    ## Change building type names in parcel data for consistency
    # TYPE field
    c.pars$TYPE[c.pars$TYPE == "Food Srv"] <- "Food.Srv"
    c.pars$TYPE[c.pars$TYPE == "Food Sale"] <- "Food.Sale"
    c.pars$TYPE[c.pars$TYPE == "Comm Misc"] <- "Comm.Misc"
    c.pars$TYPE[c.pars$TYPE == "college"] <- "College"
    c.pars$TYPE[c.pars$TYPE == "Food Srv"] <- "Food.Srv"
    c.pars$TYPE[c.pars$TYPE == "Food Sale"] <- "Food.Sale"
    c.pars$TYPE[c.pars$TYPE == "Comm Misc"] <- "Comm.Misc"
    c.pars$TYPE[c.pars$TYPE == "college"] <- "College"
    
    # Typef field
    c.pars$Typef[c.pars$Typef == "Food Srv"] <- "Food.Srv"
    c.pars$Typef[c.pars$Typef == "Food Sale"] <- "Food.Sale"
    c.pars$Typef[c.pars$Typef == "Comm Misc"] <- "Comm.Misc"
    c.pars$Typef[c.pars$Typef == "college"] <- "College"
    c.pars$Typef[c.pars$Typef == "Food Srv"] <- "Food.Srv"
    c.pars$Typef[c.pars$Typef == "Food Sale"] <- "Food.Sale"
    c.pars$Typef[c.pars$Typef == "Comm Misc"] <- "Comm.Misc"
    
    # get county name
    c.name <- unique(as.character(c.pars$County))

    

    
    # init result row for this county
    c.row <- data.frame(County = c.name)
    
    # iterate over subtypes
    for (st in tst.df$Typef) {
      # calc median btl
      c.btl <- CalcCountyBtl(c.pars, grouping.col = "Typef", 
                             group = st, b.area.col = "BArea", 
                             lot.area.col = "LOTAREA")
      
      c.row <- cbind(c.row, c.btl)
      names(c.row)[names(c.row) == "c.btl"] <- st
    }
  }
  c.row
                      }
# close cluster
stopCluster(cl)


# PART II: Calc medians by parent types

# initialize parallel backend
no.cores <- (detectCores() - 1)
cl <- makeCluster(no.cores, type = "SOCK", outfile="")
registerDoSNOW(cl)

# iterate over county shapefiles
# set up processing que from shapefiles in input_shapefiles dir
dir.files <- list.files("../input_shapefiles/")
in.shapes <- dir.files[CheckExt(dir.files)]



# iterate over shapefiles in 'input_shapefiles' directory
t.btl.df <- foreach (shapefile = in.shapes, 
                      .combine = "rbind",
                      .errorhandling = "remove") %dopar% {
                        
  # # KEEP FOR FINAL: load shapefiles from .shp format
  # in.shape.path <- paste0("../input_shapefiles/", shapefile)
  # in.shape.layer <- substr(shapefile, 1, nchar(shapefile)-4)
  # cnt.final <- try(readOGR(dsn = in.shape.path, layer = in.shape.layer))
  
  
  # TEMP: load shapefiles from binary much faster
  in.shape.path <- paste0("../output_data/binary_shapefiles/", 
                          shapefile, ".RDS")
  
  c.shape <- readRDS(in.shape.path)
  
  # get class of imported RDS data
  c <- class(c.shape)[[1]]
  
  # check that a valid shapefile was loaded
  if(c == "SpatialPolygonsDataFrame") {
    
    # extract data part of spatial object
    c.pars <- c.shape@data
    
    # change class of cols
    c.pars$Typef <- as.character(c.pars$Typef)
    c.pars$TYPE <- as.character(c.pars$TYPE)
    
    ## Change building type names in parcel data for consistency
    # TYPE field
    c.pars$TYPE[c.pars$TYPE == "Food Srv"] <- "Food.Srv"
    c.pars$TYPE[c.pars$TYPE == "Food Sale"] <- "Food.Sale"
    c.pars$TYPE[c.pars$TYPE == "Comm Misc"] <- "Comm.Misc"
    c.pars$TYPE[c.pars$TYPE == "college"] <- "College"
    c.pars$TYPE[c.pars$TYPE == "Food Srv"] <- "Food.Srv"
    c.pars$TYPE[c.pars$TYPE == "Food Sale"] <- "Food.Sale"
    c.pars$TYPE[c.pars$TYPE == "Comm Misc"] <- "Comm.Misc"
    c.pars$TYPE[c.pars$TYPE == "college"] <- "College"
    
    # Typef field
    c.pars$Typef[c.pars$Typef == "Food Srv"] <- "Food.Srv"
    c.pars$Typef[c.pars$Typef == "Food Sale"] <- "Food.Sale"
    c.pars$Typef[c.pars$Typef == "Comm Misc"] <- "Comm.Misc"
    c.pars$Typef[c.pars$Typef == "college"] <- "College"
    c.pars$Typef[c.pars$Typef == "Food Srv"] <- "Food.Srv"
    c.pars$Typef[c.pars$Typef == "Food Sale"] <- "Food.Sale"
    c.pars$Typef[c.pars$Typef == "Comm Misc"] <- "Comm.Misc"
    
    # get county name
    c.name <- unique(as.character(c.pars$County))
    
    # init result row for this county
    c.row <- data.frame(County = c.name)
    
    # iterate over subtypes
    for (t in unique(tst.df$TYPE)) {
      # calc median btl
      c.btl <- CalcCountyBtl(c.pars, grouping.col = "TYPE", 
                             group = t, b.area.col = "BArea", 
                             lot.area.col = "LOTAREA")
      
      c.row <- cbind(c.row, c.btl)
      names(c.row)[names(c.row) == "c.btl"] <- t
    }
  }
  c.row
}

# close cluster
stopCluster(cl)

## Part 3: calculate state average btl vals for each subtype
t.btl.meds <- CalcStateBtl(t.btl.df)

st.btl.meds <- CalcStateBtl(st.btl.df)


## PART 4:
# rbind state summary stats to county level tables
types.btl <- rbind(t.btl.df, t.btl.meds)
subtypes.btl <- rbind(st.btl.df, st.btl.meds)

# export as excel workbook
wb <- createWorkbook()
output.dir <- "../output_data/"
date <- substr(Sys.time(), 1, 10)
file <- paste0("BTL_tables_2016.xlsx")
output.filename <- paste0(output.dir, file)

# init sheets for 2020
sheet1 <- addWorksheet(wb, "2020PBTL_TYPE")
sheet2 <- addWorksheet(wb, "2016BTL_Typef")

writeData(wb, "2020PBTL_TYPE", types.btl)
writeData(wb, "2016BTL_Typef", subtypes.btl)

# save workbook
saveWorkbook(wb, paste0(output.filename), overwrite = T)


