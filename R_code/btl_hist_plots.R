

# ###### TEMP: SET WD TO THIS SCRIPT LOCATION FOR FUNCTION TESTING ######
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
# ###### SET WORKING DIRECTORY ######
# this.dir <- dirname(csf())
# setwd(this.dir)
# rm(list=ls())


require(openxlsx)

data <- read.xlsx("BTL_tables_2016.xlsx", sheet = 2)

par(mfrow=c(4,4))

stypes <- colnames(data)[3:ncol(data)]
stypes <- stypes[!(stypes %in% c("Vacant", "SmOffice",  "PUD", "Misc"))]

for (st in stypes) {
  hist(data[,st][(data[,st]!=0) & (data$County != "California")],
       main = st, breaks = 20, xlab = "County Level BtoL Ratio")
}








