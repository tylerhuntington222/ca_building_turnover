
#-----------------------------------------------------------------------------#
# locad_clean_supp_data.R

# Type: R I/O script

# AUTHOR:
# Tyler Huntington, 2017
# Lawrence Berkeley National Lab Sustainable Energy Group
# Project: California Building Turnover Study
# Principal Investigator: Hanna Breunig

# PURPOSE:
# PURPOSE:
# A script to load, clean and export as binary files all supplementary data 
# required by the building_turnover_master_analysis.R script. This enables
# loading of binary data files in the aforementioned program, significantly
# shortening runtime.

# RETURNS:
# none

# OUTPUTS:

# binary R data files ('.RDS' extension) of the following

# demolition cutoff years table
# 2020 growth multipliers
# 2050 growth multipliers
# 2016 EUI vintages table
# 2020 EUI vintages table
# 2050 EUI vintages table
# county name/abbreviations table




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

saveRDS(g.mult.20, "g.mult.20")

# load 2050 multipliers
g.mult.50 <- read.xlsx(file = input.datafile,
                       sheetName = "2050growthmult", colClasses = "character")

saveRDS(g.mult.50, "g.mult.20")

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

saveRDS(eui.16, "eui.50")

# set col classes of EUI tables
eui.16$Join <- as.character(paste(eui.16$Join))
eui.20$Join <- as.character(paste(eui.20$Join))
eui.50$Join <- as.character(paste(eui.50$Join))


saveRDS(eui.16, "eui.16")
saveRDS(eui.16, "eui.20")
saveRDS(eui.16, "eui.50")

## County abbreviation codes
# load county name/abbreviation table
county.codes.df <- read.xlsx(file = input.datafile, sheetName = "CountyCodes",
                             colClasses = "character")

# set col classes to char
county.codes.df$county.name <- as.character(county.codes.df$county.name)
county.codes.df$county.code <- as.character(county.codes.df$county.code)