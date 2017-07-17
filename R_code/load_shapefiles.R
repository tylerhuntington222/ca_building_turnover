

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
require(openxlsx)

source("ST_bt_functions.R")



# set up processing que from shapefiles in input_shapefiles dir
dir.files <- list.files("../input_shapefiles/")
in.shapes <- dir.files[CheckExt(dir.files)]


# initialize parallel backend
no.cores <- (detectCores() - 1)
cl <- makeCluster(no.cores, type = "SOCK", outfile="")
registerDoSNOW(cl)

# iterate over shapefiles in 'input_shapefiles' directory
output.metalist <- 
  foreach(shapefile = in.shapes[1:length(in.shapes)],
          .packages = c("plyr", "dplyr", "rgdal", "sp",
                        "raster",  "rgeos", "doSNOW")) %dopar% {
  
  
  
  ############ LOAD PARCEL DATA ############ 
  in.shape.layer <- substr(shapefile, 1, nchar(shapefile)-4)
  in.shape.path <- paste0("../input_shapefiles/", shapefile)
  
  cnt.final <- try({
    readOGR(dsn = in.shape.path, layer = in.shape.layer)
  })
  
  saveRDS(cnt.final, paste0("../output_data/binary_shapefiles/", 
                            shapefile, ".RDS"))
  
  print(paste0("Exported binary RDS file for shapefile", shapefile))
  
                        }

