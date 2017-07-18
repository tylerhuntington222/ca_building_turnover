


# define function for checking whether a file extension is '.shp' (shapefile)
CheckExt <- Vectorize(function(x) {
  n <- nchar(x) 
  ext <- substr(x, n-3, n)
  if(ext == ".shp") {
    return(TRUE)
  } else {
    return(FALSE)
  } 
})

# define function for checking if a building is demolished (0) or remains (1)
CheckIfRemains <- Vectorize(function(yr, cut) {
  if ((as.numeric(yr) < as.numeric(cut)) & (yr != 0)) {
    return (0)
  } else {
    return (1)
  }
})

# define function for assigning 2020 status
AssignStatus <- Vectorize(function(BinYEAR, BAREA, LOTAREA, TYPE) {
  
  # handle NAs in LOTAREA argument
  if (is.na(LOTAREA)) {
    LOTAREA <- 0
  }
  
  # TEMP: handle NAs in BAREA argument -- KEEP THIS?? 
  if (is.na(BAREA)) {
    BAREA <- 0
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
  } else {
    return("Vacant")
  }
})

CalcEUIVint16 <- Vectorize(function(yr) {
  yr <- as.numeric(yr)
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
})

# assign EUIVint codes
CalcEUIVint20 <- Vectorize(function(yr) {
  yr <- as.numeric(yr)
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
})

# assign EUIVint codes
CalcEUIVint50 <- Vectorize(function(yr) {
  yr <- as.numeric(yr)
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
})

# define function for turning a county's summary stats into a 1 row df
MakeRow <- function(summary.data, county.name, col.name,
                    template.df, grouping) {

  # merge with template row
  row.df <- merge(template.df, summary.data, 
                  by = grouping, drop = F, all.x = T)
  
  # change NAs and Inf vals to zeros
  row.df[is.na(row.df[,col.name]), col.name] <- 0
  row.df[is.infinite(row.df[,col.name]), col.name] <- 0
  
  # org output data row
  rownames(row.df) <- row.df[,grouping]
  row.df <- subset(row.df, select = col.name)
  row.df <- data.frame(t(row.df))
  ncols <- ncol(row.df)
  row.df$County <- county.name
  row.df <- cbind("County" = row.df$County, row.df[1:ncols])
  
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

AssignEUIVint <- Vectorize(function(altyrfn, stock.year) {
  
  yr <- substr(stock.year, 3, 4)
  
  # determin which EUIVint calculator function to call
  fun.name <- paste0("CalcEUIVint", yr)
  
  EUIVint <- get(fun.name)(altyrfn)
  
  return(EUIVint)
  
})

WriteShapefile <- function(parcel.df, parcel.sp, county.code, year) {
  
  yr <- substr(year, 3, 4)
  # determine BArea col corresponding to the year of this shapefile's bldg stock
  stock.barea.col <- paste0("BArea.", yr)
  
  # assign EUI vint codes
  parcel.df$EUIVint <- unlist(AssignEUIVint(parcel.df$altyrfn, 
                                            stock.year = year))
  
  # join EUIs to parcels
  eui.table <- paste0("eui.", yr)
  
  parcel.df$JOINB <- paste0(parcel.df$cz, 
                           parcel.df$Typef, 
                           parcel.df$EUIVint)
  
  # get the appropriate EUI table for bldg stock year specified by caller
  eui.table <- get(eui.table)
  
  # drop superfluous columns before merging with parcel df
  all.cols <- names(eui.table)
  drop.cols <- c("TYPE", "CZ")
  eui.table <- eui.table[, !(all.cols %in% drop.cols)]
  
  # merge EUI table with parcel df
  parcel.df <- merge(parcel.df, eui.table, by = "JOINB", all.x = T)
  
  # calculate total energy consumption fields
  parcel.df$TotNGSH <- parcel.df[,stock.barea.col] * parcel.df[,"NGEUISH"]
  parcel.df$TotNGWH <- parcel.df[,stock.barea.col] * parcel.df[,"NGEUIWH"]
  parcel.df$TotNGC <- parcel.df[,stock.barea.col] * parcel.df[,"NGEUIC"]
  parcel.df$TotEC <- parcel.df[,stock.barea.col] * parcel.df[,"EEUIC"]  
  parcel.df$TotNGPH <- parcel.df[,stock.barea.col]* parcel.df[,"NGEUIPH"] 
  parcel.df$TotNGPC <- parcel.df[,stock.barea.col] * parcel.df[,"NGEUIPC"]
  
  parcel.df$TotC <- 
    parcel.df[,stock.barea.col] * 
    (parcel.df[,"NGEUIC"] + parcel.df[,"EEUIC"])
  
  parcel.df$TotH <- 
    parcel.df[,stock.barea.col] * 
    (parcel.df[,"NGEUISH"] + parcel.df[,"NGEUIWH"])
  
  # subset for fields to include in shapefile attribute table
  drop <- c()
  cols <- names(parcel.df)
  fields <- cols[!(cols %in% drop)]
  parcel.df <- subset(parcel.df, select = fields)
  
  # set fields with large numerics to chars
  parcel.df$POLY_AREA <- as.character(round(parcel.df$POLY_AREA, 3))
  
  # find BArea cols
  barea.cols <- grep("BArea", names(parcel.df), value = T)
  

  # iterate over BArea cols, changing their class to character
  for (c in barea.cols) {
    parcel.df[,c] <- as.character(round(parcel.df[,c], 1))
  }
  
 
  
  # set up spatial object for export as .shp
  rownames(parcel.df) <- parcel.df$APN
  shp <- SpatialPolygonsDataFrame(parcel.sp, parcel.df)
  
  # set output file name and paths
  layer.name <- paste0(county.code, year)
  path <- paste0("../output_shapefiles/", layer.name, ".shp")
  
  # write out shapefile
  writeOGR(shp, dsn=path, layer=layer.name, 
           driver = "ESRI Shapefile", overwrite_layer = T)
  
  return(shp)
}

# define function for writing row-wise county data to output tables
WriteOutput <- function(data.row, output.filename, sheet.name) {
  
  filepath <- paste0("../output_data/", output.filename, ".xlsx")
  
  # check if the target sheet is blank
  readsheet.df <- read.xlsx(filepath, sheet = sheet.name)
  
  if (is.null(readsheet.df)) {
    
    # write to sheet, including column names
    output.wb <- loadWorkbook(filepath)
    writeData(output.wb, sheet = sheet.name, data.row, 
                 rowNames = F, colNames = T)
    saveWorkbook(output.wb, filepath, overwrite = T)
    
  } else {
    # determine next empty row for writing to
    write.row <- nrow(readsheet.df) + 2
    
    # write to sheet, excluding column names
    output.wb <- loadWorkbook(filepath)
    writeData(output.wb, sheet = sheet.name, data.row, 
              rowNames = F, colNames = F, startRow = write.row)
    saveWorkbook(output.wb, filepath, overwrite = T)
  }
}

# define function for recoding year based on AGROUP criteria
RecodeYear <- Vectorize(function(yr) {
  yr <- as.numeric(yr)
  if ((yr < 1920) & (yr != 0)) {
    return (1920)
  } else if ((yr >= 1920) & (yr <= 1935)) {
    return (1935)
  } else if ((yr >= 1936) & (yr <= 1950)) {
    return (1950)  
  } else if ((yr >= 1951) & (yr <= 1965)) {
    return (1965)
  } else if ((yr >= 1966) & (yr <= 1980)) {
    return (1980)
  } else if ((yr >= 1981) & (yr <= 1995)) {
    return (1995) 
  } else if ((yr >= 1996) & (yr <= 2010)) {
    return (2010)
  } else if ((yr >= 2011) & (yr <= 2016)) {
    return (2016)
  } else if ((yr >= 2017) & (yr <= 2020)) {
    return (2020)
  } else if (yr >= 2021) {
    return (2050)
  } else{
    return (0)
  }
})


# define function wrapper for assigning parcels to AGROUP categories
AssignAgroup <- function(parcel.df) {
  
  parcel.df$AGROUP <- RecodeYear(parcel.df$Yrfinal)
  
  return(parcel.df)
}


# define functions for simulations

RandomSwitch <- function(parcel.df, from.stype, 
                         from.status, to.stype, to.status, status.year) {
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
  #---------------------------------------------------------------------------#
  
  yr <- substr(status.year, 3, 4)
  status.col <- paste0("Status", yr)
  
  # init parcel pool to select from
  pool <- subset(parcel.df, 
                 (parcel.df[,"Typef"] == from.stype) &
                   (parcel.df[,status.col] == from.status))
  
  # randomly select parcel from pool by apn code
  rand.apn <- sample(pool$APN, 1)
  
  # change type and status of randomly selected parcel
  sel.par <- pool[pool$APN == as.character(rand.apn),]
  sel.par[,"Typef"] <- to.stype
  sel.par["TYPE"] <- GetParentType(to.stype, key = tst.df)
  
  sel.par[,status.col] <- to.status
  parcel.df[parcel.df$APN == as.character(rand.apn),] <- sel.par
  
  res.list <- list(parcel.df, rand.apn)
  return(res.list)
}
#-----------------------------------------------------------------------------#

# A function that return the parent type of a bldg type passed by the caller
GetParentType <- function (subtype, key) {
  type <- tst.df$TYPE[which(tst.df$Typef == subtype)]
  
  return(type)
}


CheckDiff <- function(parcel.df, stype, status, year, 
                      area.col, projections.data) {
  #---------------------------------------------------------------------------#
  # PURPOSE: a function that checks the difference between the projected and 
  # actual (i.e. remaining on the basis of cutoff year table) floorspace of
  # a specified building type.
  
  # PARAMS:
  # parcel.df - a dataframe of parcel data for the county of interest
  # stype - a string specifying the building subtype of interest
  # status - a string or vector of strings specifying the 2020 bldg 
  # status(es) of interest
  # year- the year for which actual/projected floorspace diffs should be 
  # calculated
  # area.col - string of the column name in parcel.df that contains actual 
  # areas to be subtracted from projected areas to calculate differences
  
  
  # RETURNS: the difference of (projected_floorspace - remaining_floorspace) 
  # for buildings of the specified type
  
  # SIDE-EFFECTS: None
  #---------------------------------------------------------------------------#
  yr <- substr(as.character(year), 3, 4)
  status.col <- paste0("Status", yr)
  proj.col <- paste0("PROJ.FS.", yr)
  
  
  # subset for parcels of specified type and status
  t.pars <- subset(parcel.df, 
                   ((parcel.df$Typef == stype) & 
                      (parcel.df[,status.col] %in% status)))
  
  # sum remaining floorspace
  sum.fs <- sum(na.omit(t.pars[,area.col]))
  
  # fetch projected floorspace
  proj.fs <- 
    projections.data[(projections.data[,"Typef"] == stype), ][,proj.col]
  
  # calc difference
  diff <- (proj.fs - sum.fs)
  
  return(diff)
}

#-----------------------------------------------------------------------------#


RebuildMultiplied <- function(parcel.df, stype, from.status, to.status, mult,
                              year, from.area.col, to.area.col) {
  #---------------------------------------------------------------------------#
  # PURPOSE: a function that multiplies the indicated area field of parcels of a 
  # specified type and status by a user-supplied multiplier.
  
  # PARAMS:
  #  parcel.df - a dataframe of parcel data for the county of interest
  #  stype - a string specifying the subtype of bldg to work on
  #  from.status - string specifying the status of buildings to be subsetted
  #  to.status - string specifying the output status of parcels whose bldg
  # area has been multiplied bu the function. 
  #  mult - the multiplier, as a numeric value 
  
  # RETURNS: the parcel.df with bldg area vals updated of parcels specified by
  # arguments passed to caller. 
  
  # SIDE-EFFECTS: None
  #---------------------------------------------------------------------------#
  
  yr <- substr(as.character(year), 3, 4)
  status.col <- paste0("Status", yr)
  
  # subset parcel data for specified bldg type
  t.pars <- parcel.df[((parcel.df[,"Typef"] == stype) & 
                         (parcel.df[,status.col] == from.status)),]
  
  # if parcels exist given the criteria, apply multiplier
  if (nrow(t.pars) > 0) {
    
    # apply multiplier
    t.pars[,to.area.col] <- t.pars[,from.area.col] * mult
    
    # update altyrfn and status fields
    t.pars$altyrfn <- as.character(year)
    t.pars[,status.col] <- to.status
    
    # update Bin50 field if rebuilds occurrin in 2020
    if(year == 2020) {
      t.pars$Bin50 <- "1"
    }
    
    # update values in parcel df
    parcel.df[((parcel.df[,"Typef"] == stype) & 
                 (parcel.df[,status.col] == from.status)),] <- t.pars
    
    return(parcel.df)
    
  } else {
    return ("None")
  }
}
#-----------------------------------------------------------------------------#

SizeRankedSwitch <- function(parcel.df, from.stype, from.status, 
                             to.stype, to.status, year, area.col) {
  #---------------------------------------------------------------------------#
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
  #---------------------------------------------------------------------------#
  yr <- substr(as.character(year), 3, 4)
  status.col <- paste0("Status", yr)
  
  # init parcel pool to select from
  pool <- subset(parcel.df, 
                 (parcel.df[,"Typef"] == from.stype) &
                   (parcel.df[,status.col] == from.status))
  
  # find the parcel with the largest floorspace area
  lar.par <- (which(pool[,area.col] == max(pool[,area.col])))[1]
  sel.par <- pool[lar.par,]
  sel.apn <- sel.par$APN
  
  # change type and status of randomly selected parcel
  sel.par$Typef <- to.stype
  sel.par$TYPE <- GetParentType(to.stype, key = tst.df)
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
  #---------------------------------------------------------------------------#
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
  
  # SIDE-EFFECTS: updates the BArea.20 field of a bldg to the product of the
  # parcel's BArea field and the supplied expansion factor. 
  #---------------------------------------------------------------------------#
  
  # get demo'd bldg area
  orig.area <-  parcel.df[parcel.df$APN == apn, ][,from.area.col]
  
  # calc expanded area of rebuild
  new.area <- orig.area * expand.factor
  
  # update BArea.20 field of parcel df
  parcel.df[parcel.df$APN == apn, ][,to.area.col] <- new.area
  
  return(parcel.df)
  
}
#-----------------------------------------------------------------------------#

CalcBtlRatio <- function(parcel.df, stype, year) {
  #---------------------------------------------------------------------------#
  # PURPOSE: calculate the mean bldg area to lot area ratio of a particular 
  # usetype, within a specific county. 
  # Omits outliers and zeros in calulation of mean.
  
  # PARAMS:
  # parcel.df - a dataframe of parcel data for the county of interest
  # stype - the parcel subtype for which BtoL ratios are to be calculated
  # area.col - character string specifying the name of the column containing 
  # building area vals to use in calculating BtoL ratio
  
  
  # RETURNS: APN code of building that was switched
  
  # SIDE-EFFECTS: None
  #---------------------------------------------------------------------------#
  
  yr <- substr(as.character(year), 3, 4)
  barea.col <- paste0("BArea.", yr)
  lotarea.col <- paste0("LOTAREA.", yr)
  
  # init parcel pool to select from
  pool <- subset(parcel.df, (parcel.df$Typef == stype))
  
  # get barea and lotarea vals
  barea.vals <- pool[,barea.col]
  lotarea.vals <- pool[,lotarea.col]
  
  if ((length(barea.vals) == 0) | (length(lotarea.vals) == 0)) {
    return (0)
  }
      
  # make dataframe
  btl.df <- data.frame(barea.vals, lotarea.vals)
  
  # init rm.rows
  rm.rows <- c(NULL)
  
  # elim parcels for which lotarea or barea is 0 or NA
  for(r in 1:nrow(btl.df)) {
    par <- btl.df[r,]
    if ((is.na(par$barea.vals)) | (is.na(par$lotarea.vals))) {
      rm.rows <- c(rm.rows, r)
    } else if ((par$barea.vals == 0) |  (par$lotarea.vals == 0)) {
      rm.rows <- c(rm.rows, r)
    }
  }
  btl.df <- btl.df[!(1:nrow(btl.df) %in% rm.rows), ]
  
  # check that there are still vals in df to calc btl ratios
  if (nrow(btl.df) == 0) {
    return (0)
  }
  
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
  med.btl <- median(non.outlier.ratios)
  
  return(med.btl)
}
#-----------------------------------------------------------------------------#

ConvertLot <- function(parcel.df, stype, btl.ratio, stock.year, sim.year) {
  #---------------------------------------------------------------------------#
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
  # stype - a string specifying the subtype of building stock to pull from
  # year -
  # to.barea.col -
  
  # RETURNS: APN code of parcel in which lot area was converted to bldg area
  # if no lots were able to be converted, returns "None"
  
  # SIDE-EFFECTS: updates the LOTAREA and BArea fields in the input parcel df
  # of the parcel that is selected via the ranked/randomized selection step. 
  #---------------------------------------------------------------------------#
  
  t <- GetParentType(stype, key = tst.df)
  
  stock.yr <- substr(as.character(stock.year), 3, 4)
  sim.yr <- substr(as.character(sim.year), 3, 4)
  status.col <- paste0("Status", sim.yr)
  
  from.barea.col <- paste0("BArea.", stock.yr)
  to.barea.col <- paste0("BArea.", sim.yr)
  
  from.lotarea.col <- paste0("LOTAREA.", stock.yr)
  to.lotarea.col <- paste0("LOTAREA.", sim.yr)
  
  ## init parcel pool to select from
  # check if usetype is "Condo" in which case, include MF parcels in pool
  if (t == "Condo") {
    pool <- subset(parcel.df, 
                   ((parcel.df$TYPE == t) | (parcel.df$TYPE == "MF")) &
                     (parcel.df[,status.col] == "LotsOpen"))
  } else {
    pool <- subset(parcel.df, 
                   (parcel.df$TYPE == t) &
                     (parcel.df[,status.col] == "LotsOpen"))
  }
  
  # check that there are parcels in the candidate pool
  if (nrow(pool) == 0) {
    return("None")
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
    
    # boost score of condo parcels so they all rank higher
    # than MF parcels and so that MF parcels will only be used if condos run out
    if ((stype == "Condo") & par$Typef == "Condo") {
      p.score <- p.score + 5
    }
    
    # boost scores of parcels whose subtype match the subtype passed by caller
    if (par$Typef == stype) {
      par$dc.score <- p.score + 10
    }
        
    # update data completeness score field of parcel df
    par$dc.score <- p.score
    
    # update pool.df with scored parcels
    pool[p, ] <- par
    
  }
  
  ## select highest ranked parcel
  top.scoring.pars <- which(pool$dc.score == max(pool$dc.score))[1]
  sel.par <- pool[sample(top.scoring.pars, 1),]
  
  # get apn of selected parcel
  sel.par.apn <- sel.par$APN
  
  # obtain parcel w/o dc.score field from master parcel df
  sel.par <- parcel.df[parcel.df$APN == sel.par.apn,]
  
  ## determine current btl ratio of selected parcel
  btl.par <- CalcBtlRatio(sel.par, stype = sel.par$Typef, year = stock.year)
  
  # case if no valid btl calculated due to NA or zero val for bldg area
  if (is.na(btl.par)) {
    
    # calculate how much additional lot area can be converted to bldg area
    avail.area <- (sel.par[,from.lotarea.col] * btl.ratio)
    
    # allocate allowable lot area to bldg area
    sel.par[,to.barea.col] <- (sel.par[,from.barea.col] + avail.area)
    
    # subtract converted lot area from LOTAREA field 
    sel.par[,to.lotarea.col] <- (sel.par[,from.lotarea.col] - avail.area)
    
  } else {
    # check that parcel's btl ratio does not already exceed county-usetype mean
    if (btl.par < btl.ratio) {
      
      # calculate how much additional lot area can be converted to bldg area
      avail.area <- (sel.par[,from.lotarea.col] * btl.ratio) -
        (sel.par[,from.lotarea.col] * btl.par)
      
      # allocate allowable lot area to bldg area
      sel.par[,to.barea.col] <- (sel.par[,from.barea.col] + avail.area)
      
      # update lotarea field
      sel.par[,to.lotarea.col] <- (sel.par[,from.lotarea.col] - avail.area)
      
      # change type if MF lot area was converted to fill Condo floorspace gap
      if ((sel.par$TYPE == "MF") & (t == "Condo")) {
        sel.par$TYPE <- "Condo"
      }
      
    } else {
      return("None")
    }
  }
  
  # update status of parcel where conversion occurred
  sel.par[,status.col] <- "LotsFilled"
  
  # update row of selected parcel in master parcel df
  parcel.df[parcel.df$APN == sel.par.apn, ] <- sel.par
  
  # prep result list to return
  res.list <- list(parcel.df, sel.par.apn)
  
  return(res.list)
}

#-----------------------------------------------------------------------------#

FillVacant <- function(parcel.df, to.stype, btl.ratio, stock.year, sim.year) {
  #---------------------------------------------------------------------------#
  # PURPOSE: a function that randomly assigns the type and status 
  # of a building of a user-specified original type and status to "Vacant"
  
  # PARAMS:
  # from.type - a string specifying the type of building to randomly 
  # select from
  # to.stype
  # btl.ratio
  # year - numeric indicating the year in which parcels of status "Vacant" 
  # should be sourced
  
  # RETURNS: the APN code of the parcel that was switched
  
  # SIDE-EFFECTS: changes status and type of a randomly selected parcel from 
  # the parcel.df that meets supplied criteria to "Vacant"
  #---------------------------------------------------------------------------#
  t <- GetParentType(to.stype, key = tst.df)
  
  stock.yr <- substr(as.character(stock.year), 3, 4)
  sim.yr <- substr(as.character(sim.year), 3, 4)
  
  status.col <- paste0("Status", sim.yr)
  
  from.barea.col <- paste0("BArea.", stock.yr)
  to.barea.col <- paste0("BArea.", sim.yr)
  
  from.lotarea.col <- paste0("LOTAREA.", stock.yr)
  to.lotarea.col <- paste0("LOTAREA.", sim.yr)
  
  # init parcel pool to select from
  pool <- subset(parcel.df, 
                 (parcel.df$TYPE == "Vacant") & 
                   ((parcel.df[,from.barea.col] > 0) | 
                      (parcel.df[,from.lotarea.col] > 0)))
  
  # check that there are parcels in the candidate pool
  if (nrow(pool) == 0) {
    return ("None")
  }
  
  # rank pool of vacant parcels in order of conversion priority
  pool$conv.ord <- 0
  
  if (nrow(pool[((pool[,from.barea.col] != 0) & 
                 (pool[,status.col] == "Remain")), ]) > 0) {
    
    pool[((pool[,from.barea.col] != 0) & 
            (pool[,status.col]  == "Remain")), ]$conv.ord <- 1
    
  } else if (nrow(pool[((pool[,from.barea.col] != 0) & 
                        (pool[,status.col] != "Remain")), ]) > 0) {
    
    pool[((pool[,from.barea.col] != 0) & 
            (pool[,status.col] != "Remain")), ]$conv.ord <- 2
    
  } else if (nrow(pool[(pool[,from.barea.col] == 0) & 
                       (pool[,from.lotarea.col] > 0), ]) > 0) {
    
    pool[((pool[,from.barea.col] == 0) & 
            (pool[,from.lotarea.col] > 0)), ]$conv.ord <- 3
    
  }
  
  ## randomly select parcel to convert in order of converstion priority
  # check first priority parcels
  pool.1 <- pool[(pool$conv.ord == 1), ]
  pool.2 <- pool[(pool$conv.ord == 2), ]
  pool.3 <- pool[(pool$conv.ord == 3), ]
  
  if (nrow(pool.1) > 0) {
    rand.apn <- sample(pool.1$APN, 1)
    
    # get parcel of randomly chosen APN
    sel.par <- parcel.df[which(parcel.df$APN == rand.apn), ]
    
    # change type and status of randomly selected parcel
    sel.par[,to.barea.col] <- sel.par[,from.barea.col]
    sel.par$TYPE <- t
    sel.par$Typef <- to.stype
    sel.par$Status20 <- "VacantFilled"
    
  } else if (nrow(pool.2) > 0) {
    rand.apn <- sample(pool.2$APN, 1)
    
    # get parcel of randomly chosen APN
    sel.par <- parcel.df[which(parcel.df$APN == rand.apn), ]
    
    # change type and status of randomly selected parcel
    sel.par[,to.barea.col] <- sel.par[,from.barea.col]
    
  } else if (nrow(pool.3) > 0) {
    rand.apn <- sample(pool.3$APN, 1)
    
    # get parcel of randomly chosen APN
    sel.par <- parcel.df[which(parcel.df$APN == rand.apn), ]
    
  } else {
    return("None")
  }
  
  ## determine current btl ratio of selected parcel
  btl.par <- CalcBtlRatio(sel.par, stype = sel.par$Typef, 
                          year = stock.year)
  
  # case if no valid btl calculated due to NA or zero val for bldg area
  if (is.na(btl.par)) {
    
    # calculate how much additional lot area can be converted to bldg area
    avail.area <- (sel.par[,from.lotarea.col] * btl.ratio)
    
    # allocate allowable lot area to bldg area
    sel.par[,to.barea.col] <- (sel.par[,from.barea.col] + avail.area)
    
    # update lotarea field
    sel.par[,to.lotarea.col] <- (sel.par[,from.lotarea.col] - avail.area)
    
    # update altyrfn 
    sel.par$altyrfn <- sim.year
    
    # if vacancy filled in 2020, update Bin50 field
    if (sim.year == 2020) {
      sel.par$Bin50 <- 1
    }
    
  # check that parcel's btl ratio does not already exceed subtype mean
  } else if (btl.par < btl.ratio) {
    
    avail.area <- (sel.par[,from.lotarea.col] * btl.ratio) - 
      (sel.par[,from.lotarea.col] * btl.par)
    
    # allocate allowable lot area to bldg area
    sel.par[,to.barea.col] <- (sel.par[,from.barea.col] + avail.area)
    
    # update lotarea field
    sel.par[,to.lotarea.col] <- (sel.par[,from.lotarea.col] - avail.area)
    
    # update altyrfn 
    sel.par$altyrfn <- sim.year
    
    # if vacancy filled in 2020, update Bin50 field
    if (sim.year == 2020) {
      sel.par$Bin50 <- 1
    }
  } else {
    return("None")
  }
  
  # update type and status fields
  sel.par$Typef <- to.stype
  sel.par$TYPE <- t
  sel.par[,status.col] <- "VacantFilled"
  
  # update master parcel df
  parcel.df[parcel.df$APN == sel.par$APN, ] <- sel.par
  
  # prep result list to return
  res.list <- list(parcel.df, rand.apn)
  return(res.list)
}

#-----------------------------------------------------------------------------#

CheckForLots <- function(parcel.df, subtype) {
  
  type <- GetParentType(subtype, key = tst.df)
  
  if (type == "Condo") {
    check.lots <- parcel.df[(parcel.df$TYPE %in% c("Condo", "MF")) &
                               (parcel.df$Status20 == "LotsOpen"), ]
  } else {
    check.lots <- parcel.df[(parcel.df$TYPE == type) &
                               (parcel.df$Status20 == "LotsOpen"), ] 
  }
  
  if (nrow(check.lots) != 0) {
    avail.lots <- TRUE
  } else {
    avail.lots <- FALSE
  }
  
  return(avail.lots)
}


GetCountyMultipliers <- function(multipiers.data, county, year) {
 
  # subset multipliers table for this county
  g.mult.50.cnt <- multipiers.data[multipiers.data$County == county, ]
  
  # transpose and organize multiplier dataframe 
  g.mult.50.cnt <- data.frame(t(g.mult.50.cnt))
  g.mult.50.cnt$TYPE <- rownames(g.mult.50.cnt)
  g.mult.50.cnt <- g.mult.50.cnt[2:nrow(g.mult.50.cnt), ]
  yr <- substr(as.character(year), 3, 4)
  mult.col.name <- paste0("MULT.", yr)
  names(g.mult.50.cnt) <- c(mult.col.name, "TYPE")
  rownames(g.mult.50.cnt) <- seq(1, nrow(g.mult.50.cnt))
  
  return(g.mult.50.cnt)
}



CheckForVacancies <- function(parcel.df, type) {
  
  check.vacs <- parcel.df[(parcel.df[,"TYPE"] == "Vacant"),]
  if (nrow(check.vacs) != 0) {
    avail.vacancies <- TRUE
  } else {
    avail.vacancies <- FALSE
  }
  return(avail.vacancies)
}

                                  
GetStateBtl <- function(btl.data, group) {
  c.row <- btl.data[(btl.data$County == "California"),]
  state.btl <- c.row[,group]
  return(state.btl)
}


ExportSummaryTables <- function(output.metalist, output.filename) {
  
  output.filename <-  "subtype_summary_tables"
  # init excel spreadsheet for exporting output tables
  output.filepath <- CreateOutputWorkbook(output.filename)
  
  # iterate over output metalist, writing summaries to spreadsheet
  n.counties <- 1:length(output.metalist)
  template.cnty <- output.metalist[[1]]
  sheets <- names(template.cnty)
  
  for (s in sheets) {
    n <- 1
    for (c in n.counties) {
      data.row <- try(output.metalist[[c]][[s]])
      if (class(data.row) != "try-error") {
        if (n == 1) {
          df <- data.row 
        } else {
          df <- rbind(df, data.row)
        }
        n <- n+1
      }
    }
    WriteOutput(df, output.filename, s)
  }
  
  # generate secondary workbook with type-level summaries
  MakeTypeLevelOutput(output.filepath)
}


MakeTypeLevelOutput <- function(stype.output.file) {
  #---------------------------------------------------------------------------#
  # PURPOSE: Create an excel workbook with model output, summarized
  # at the building type level.
  
  # PARAMS:
  # stype.output.file - the full file path of the subtype-level summary output
    # workbook generated during the model run. 
  
  
  # RETURNS: None
  
  # SIDE-EFFECTS: Exports excel workbook to output_data/ directory
  #---------------------------------------------------------------------------#
  
  # init blank output workbook
  filename <- "type_summary_tables"
  CreateOutputWorkbook(filename) 
  
  stype.wb <- loadWorkbook(stype.output.file)
  sheets <- names(stype.wb)
  
  for (s in sheets) {
    # check if the target sheet is blank
    sheet.df <- suppressWarnings(read.xlsx(stype.output.file, sheet = s))
    
    av.sheets <- grep("BtoL", sheets, value = T)
    av.sheets <- c(av.sheets, grep("DemoMult", sheets, value = T))
    sum.sheets <- sheets[!(sheets %in% av.sheets)]
    
    ind.cols <- c("Ind", "Rfwrhs", "Hvyind", "Lgtind", "Wrhs")
    ed.cols <- c("School", "College")
    drop.cols <- c("Rfwrhs", "Hvyind", "Lgtind", 
                   "Wrhs", "School", "College")
    
    if (!(s %in% av.sheets)) {
      
      # education
      sheet.df$Education <- rowSums(sheet.df[,ed.cols])
      
      # industrial
      sheet.df$Ind <- rowSums(sheet.df[,ind.cols])
      
    } else {
      
      # education
      sheet.df$Education <- rowMeans(sheet.df[,ed.cols])
      
      # industrial
      sheet.df$Ind <- rowMeans(sheet.df[,ind.cols])
    }
    
    # drop subtype cols
    sheet.df <- sheet.df[,!(colnames(sheet.df) %in% drop.cols)]
    
    # write to sheet, including column names
    output.filename <- paste0("../output_data/", filename, ".xlsx")
    output.wb <- loadWorkbook(paste(output.filename))
    writeData(output.wb, sheet = s, sheet.df, 
              rowNames = F, colNames = T)
    saveWorkbook(output.wb, output.filename, overwrite = T)
  }
}

    

CheckIfOutlier <- function(btl.df, group, county.btl) {
  
  # get barea and lotarea vals
  vals <- btl.df[(btl.df$County != "California"),group]
  
  # drop NAs and zeros
  vals <- vals[!(is.na(vals))]
  vals <- vals[vals != 0]
  
  # identify outliers
  iqr <- IQR(vals)
  first.quant <- quantile(vals)[2]
  third.quant <- quantile(vals)[4]
  lower <- first.quant - 1.5 * iqr
  upper <- third.quant + 1.5 * iqr
    
 if ((county.btl < lower) | (county.btl > upper)) {
   return(TRUE)
 } else {
   return(FALSE)
 }
  
}

  
CalcCountyBtl <- function(parcel.df, grouping.col, 
                          group, b.area.col, lot.area.col) {
  #---------------------------------------------------------------------------#
  # PURPOSE: calculate the median bldg area to lot area ratio of a particular 
  # usetype, within a specific county. 
  # Omits outliers and zeros in calulation of median.
  
  # PARAMS:
  # parcel.df - a dataframe of parcel data for the county of interest
  # group - the parcel subtype for which BtoL ratios are to be calculated
  # area.col - character string specifying the name of the column containing 
  # building area vals to use in calculating BtoL ratio
  
  
  # RETURNS: APN code of building that was switched
  
  # SIDE-EFFECTS: None
  #---------------------------------------------------------------------------#
  
  # init parcel pool to select from
  parcel.df[,grouping.col] <- as.character(parcel.df[,grouping.col])
  pool <- subset(parcel.df, (parcel.df[,grouping.col] == group))
  
  # get barea and lotarea vals
  barea.vals <- as.numeric(as.character(pool[,b.area.col]))
  lotarea.vals <- as.numeric(as.character(pool[,lot.area.col]))
  
  if ((length(barea.vals) == 0) | (length(lotarea.vals) == 0)) {
    return (0)
  }
  
  # make dataframe
  btl.df <- data.frame(barea.vals, lotarea.vals)
  
  # init rm.rows
  rm.rows <- c(NULL)
  
  # elim parcels for which lotarea or barea is 0 or NA
  for(r in 1:nrow(btl.df)) {
    par <- btl.df[r,]
    if ((is.na(par$barea.vals)) | (is.na(par$lotarea.vals))) {
      rm.rows <- c(rm.rows, r)
    } else if ((par$barea.vals == 0) |  (par$lotarea.vals == 0)) {
      rm.rows <- c(rm.rows, r)
    }
  }
  btl.df <- btl.df[!(1:nrow(btl.df) %in% rm.rows), ]
  
  # check that there are still vals in df to calc btl ratios
  if (nrow(btl.df) == 0) {
    return (0)
  }
  
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
  
  # calc median of non-outlier btl ratios
  med.btl <- median(non.outlier.ratios)
  
  return(med.btl)
}

CalcStateBtl <- function(btl.df) {
  #---------------------------------------------------------------------------#
  # PURPOSE: calculate the median bldg area to lot area ratio of a particular 
  # usetype, for all counties in the state of California
  # Omits outliers and zero-values in calulation of median.
  
  # PARAMS:
  # btl.df - a dataframe of county level BtoL vals across usetypes in which
    # each row is a county and each column is a usetype category. 
  
  # RETURNS: A one-row data frame containing state median BtoL vals for all
    # usetypes in input btl.df
  
  # SIDE-EFFECTS: None
  #---------------------------------------------------------------------------#
  
  groups <- names(btl.df)[2:ncol(btl.df)]
  
  # init result df
  res.df <- data.frame(County = "California")
  
  for (g in groups) {
    # get barea and lotarea vals
    vals <- btl.df[,g]
    
    # drop NAs and zeros
    vals <- vals[!(is.na(vals))]
    vals <- vals[vals != 0]
    
    # identify outliers
    iqr <- IQR(vals)
    first.quant <- quantile(vals)[2]
    third.quant <- quantile(vals)[4]
    lower <- first.quant - 1.5 * iqr
    upper <- third.quant + 1.5 * iqr
    
    ratios <- as.numeric(vals)
    non.outlier.ratios <- ratios[which((ratios >= lower) & (ratios <= upper))]
    
    # calc median of non-outlier btl ratios
    med.btl <- median(non.outlier.ratios)
    
    # update result df
    res.df <- cbind(res.df, med.btl)
    names(res.df)[names(res.df) == "med.btl"] <- g
  }
  
  return(res.df)
}



CreateOutputWorkbook <- function(filename) {
  
  output.wb <- createWorkbook()
  output.dir <- "../output_data/"
  output.filepath <- paste0(output.dir, filename, ".xlsx")
  
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
  output.sheetnames <<- names(output.wb)
  
  # save workbook
  saveWorkbook(output.wb, paste0(output.filepath), overwrite = T)
  
  return(as.character(output.filepath))
  
}


LoadSuppData <- function(input.datafile) {
  
  
  ## Building area to lot area (BTL) ratio tables
  
  # load subtype means
  st.btl.df <<- read.xlsx(xlsxFile = input.datafile, sheet = "2016Subtype_BtoL")
  
  # load typemeans
  t.btl.df <<- read.xlsx(xlsxFile = input.datafile, sheet = "2016Type_BtoL")
  
  
  ## Cutoff yrs table
  # load cutoff yrs table
  cutoff.yrs <<- read.xlsx(xlsxFile = input.datafile, sheet = 2)
  
  # change col class to character
  cutoff.yrs$TYPE <<- as.character(cutoff.yrs$TYPE)
  
  # change type names for consistency
  cutoff.yrs$TYPE[cutoff.yrs$TYPE == "Food Sale"] <<- "Food.Sale"
  cutoff.yrs$TYPE[cutoff.yrs$TYPE == "Food Srv"] <<- "Food.Srv"
  cutoff.yrs$TYPE[cutoff.yrs$TYPE == "Comm Misc"] <<- "Comm.Misc"
  
  # change col names
  names(cutoff.yrs) <<- c("TYPE", "Cut20", "Cut50")
  
  ## Growth multiplier tables
  # load 2020 multipliers
  g.mult.20 <<- read.xlsx(xlsxFile = input.datafile, sheet = "2020growthmult")
  
  # load 2050 multipliers
  g.mult.50 <<- read.xlsx(xlsxFile = input.datafile,
                         sheet = "2050growthmult")
  
  # change class of county cols to character
  g.mult.20$County <<- as.character(g.mult.20$County)
  g.mult.50$County <<- as.character(g.mult.50$County)
  
  # elim whitespace chars from county names
  g.mult.20$County <<- trimws(g.mult.20$County)
  g.mult.50$County <<- trimws(g.mult.50$County)
  
  # change class of numeric cols to numeric
  for (col in 2:ncol(g.mult.20)) {
    g.mult.20[, col] <<- as.numeric(g.mult.20[, col])
  }
  
  for (col in 2:ncol(g.mult.50)) {
    g.mult.50[, col] <<- as.numeric(g.mult.50[, col])
  }
  
  
  ## EUI tables
  # load 2016 EUI data
  eui.16 <<- read.xlsx(xlsxFile = input.datafile,
                      sheet = "EUI2016")
  
  # load 2020 EUI data
  eui.20 <<- read.xlsx(xlsxFile = input.datafile,
                      sheet = "EUI2020")
  
  # load 2050 EUI data
  eui.50 <<- read.xlsx(xlsxFile = input.datafile,
                      sheet = "EUI2050")
  
  
  # set col classes of EUI tables
  eui.16$JOINB <<- as.character(paste(eui.16$JOINB))
  eui.20$JOINB <<- as.character(paste(eui.20$JOINB))
  eui.50$JOINB <<- as.character(paste(eui.50$JOINB))
  
  
  ## County abbreviation codes
  # load county name/abbreviation table
  county.codes.df <<- read.xlsx(xlsxFile = input.datafile, sheet = "CountyCodes")
  
  # set col classes to char
  county.codes.df$county.name <<- as.character(county.codes.df$county.name)
  county.codes.df$county.code <<- as.character(county.codes.df$county.code)
  
  # load building types-subtypes table (tst)
  types.df <<- read.xlsx(xlsxFile = input.datafile, sheet = "Building_Types")
  tst.df <<- types.df[, c("TYPEF", "TYPE")]
  tst.df <<- na.omit(tst.df)
  tst.df <<- unique(tst.df)
  rownames(tst.df) <<- 1:nrow(tst.df)
  names(tst.df) <<- c("Typef", "TYPE")
  tst.df$Typef <<- gsub(" ", ".", tst.df$Typef)
  tst.df$TYPE <<- gsub(" ", ".", tst.df$TYPE)
  tst.df <<- subset(tst.df, tst.df$Typef != "SmOffice")
  
  
  # generate subtypes table
  subtypes <<- unique(types.df$TYPEF)
  subtypes <<- na.omit(subtypes)
  subtypes <<- subtypes[!(subtypes %in% c("PUD", "Blank"))]
  subtypes <<- gsub(" ", ".", subtypes)
  subtypes.df <<- data.frame(Typef = subtypes, stringsAsFactors = F)
  
  # generate types table
  types <<- unique(types.df$TYPE)
  types <<- na.omit(types)
  types <<- types[!(types %in% c("PUD", "Blank"))]
  types <<- gsub(" ", ".", types)
  types.df <<- data.frame(TYPE = types, stringsAsFactors = F)
  

}


ProcessCountyParcels <- function(shapefile, run.years = c(2016, 2020, 2050)) {
  
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
    
    # # TEMP: test a subset of the parcel data
    # percent.data <- 5
    # n <- nrow(cnt.final)
    # pars.to.test <- sample(1:n, (n*(percent.data/100.0)))
    
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
    
    # # TEMP: subset parcels for faster testing
    # cnt.raw <- cnt.raw[pars.to.test,]
    
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
    cnt.raw.df$TYPE[cnt.raw.df$TYPE == "SmOffice"] <- "Office"
    
    # Typef field
    cnt.raw.df$Typef[cnt.raw.df$Typef == "Food Srv"] <- "Food.Srv"
    cnt.raw.df$Typef[cnt.raw.df$Typef == "Food Sale"] <- "Food.Sale"
    cnt.raw.df$Typef[cnt.raw.df$Typef == "Comm Misc"] <- "Comm.Misc"
    cnt.raw.df$Typef[cnt.raw.df$Typef == "college"] <- "College"
    cnt.raw.df$Typef[cnt.raw.df$Typef == "Food Srv"] <- "Food.Srv"
    cnt.raw.df$Typef[cnt.raw.df$Typef == "Food Sale"] <- "Food.Sale"
    cnt.raw.df$Typef[cnt.raw.df$Typef == "Comm Misc"] <- "Comm.Misc"
    cnt.raw.df$Typef[cnt.raw.df$Typef == "college"] <- "College"
    cnt.raw.df$Typef[cnt.raw.df$Typef == "SmOffice"] <- "Office"
    
    
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
    WriteShapefile(cnt.raw.df, cnt.raw.sp, cnt.code, 2016)
    
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
      t <- GetParentType(st, key = tst.df)
      
      # subset for row of particular building type
      type.row <- fs.diffs.df[fs.diffs.df$Typef == st, ]
      fs.diff <- type.row$DIFF
      
      # calculate bldg area to lot area ratio for usetype
      btl.ratio <- CalcBtlRatio(cnt.raw.df, st, year = 2016)
      
      # Error handling for no valid btl ratio calculated
      if (is.na(btl.ratio)) {
        # add btl of zero to btl ratio df
        btl.row <- data.frame(Typef = st, BTL.RATIO = 0)
        btl.ratio.df <- rbind(btl.ratio.df, btl.row)
        btl.ratio <- GetStateBtl(year, group = st)
        
      } else if (btl.ratio == 0) {
        # add btl of zero to btl ratio df
        btl.row <- data.frame(Typef = st, BTL.RATIO = 0)
        btl.ratio.df <- rbind(btl.ratio.df, btl.row)
        btl.ratio <- GetStateBtl(st.btl.df, group = st)
        
      } else {
        
        # check if this county-subtype btl ratio is an outlier for the state
        is.outlier <- CheckIfOutlier(st.btl.df, st, btl.ratio)
        
        if (is.outlier) {
          btl.row <- data.frame(Typef = st, BTL.RATIO = btl.ratio)
          btl.ratio.df <- rbind(btl.ratio.df, btl.row)
          btl.ratio <- GetStateBtl(st.btl.df, group = st)
          
        } else {
          
          # add the calculated btl to btl ratio df
          btl.row <- data.frame(Typef = st, BTL.RATIO = btl.ratio)
          btl.ratio.df <- rbind(btl.ratio.df, btl.row)
        }
      }
      
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
          print(sprintf("Vacated building on parcel with APN: %s", 
                        switched.apn))
          
          # check updated floorspace difference following switch
          st.diff <- CheckDiff(cnt.raw.df, st, status = c("Remain"),
                               year = 2020, area.col = "BArea.20",
                               projections.data = proj.df)
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
                               year = 2020, area.col = "BArea.20",
                               projections.data = proj.df)
          
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
            st.diff <- CheckDiff(cnt.raw.df, st, 
                                 status = c("Remain", "Rebuild"),
                                 year=2020, area.col="BArea.20",
                                 projections.data = proj.df)
            
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
              st.diff <- CheckDiff(cnt.raw.df, st, 
                                   status = c("Remain", "Rebuild"),
                                   year = 2020, area.col = "BArea.20",
                                   projections.data = proj.df)
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
          if (class(res) != "character") {
            cnt.raw.df <- res
          }
          
          ## Second, fill open lots
          print("Filling open lots of same usetype second")
          
          # check whether there is avail lot area to convert
          avail.lots <- CheckForLots(cnt.raw.df, st)
          
          # init floorspace difference tracker for this usetype
          st.diff <- CheckDiff(cnt.raw.df, st, status = c("Remain", "Rebuild"),
                               year = 2020, area.col = "BArea.20", 
                               projections.data = proj.df)
          print("3")
          # convert open lots up to allowable BTL ratios until FS difference met
          # or convertable lot area runs out.
          while ((st.diff > 0) & (avail.lots)) {
            res <- ConvertLot(cnt.raw.df, stype = st, btl.ratio, 
                              stock.year = 2016, sim.year=2020)
            # check if no lots were available to convert
            if (class(res) == "character") {
              avail.lots = FALSE
              
            } else {
              # update master parcel df
              cnt.raw.df <- res[[1]] 
              converted.apn <- res[[2]]
              print(sprintf("Allocated FS from open lot on parcel with APN: %s", 
                            converted.apn))
              
              # update altyrfn and Bin50 fields 
              converted <- cnt.raw.df[(cnt.raw.df$APN == converted.apn), ]
              converted$altyrfn <- "2020"
              converted$Bin50 <- "1"
              cnt.raw.df[(cnt.raw.df$APN == converted.apn), ] <- converted
              
              # check floorspace difference
              st.diff <- CheckDiff(cnt.raw.df, st, 
                                   status = c("Remain", "Rebuild", 
                                              "LotsFilled"),
                                   year = 2020, area.col = "BArea.20",
                                   projections.data = proj.df)
              print(sprintf("Updated floorspace difference: %s", st.diff))
            }
          }
          
          ## lastly, allocate remaining FS needs to parcels with ustype "Vacant" 
          # if current type is non-industrial
          
          # if parent type of current subtype is not "Ind", allocate vacancies
          if (t != "Ind") {
            
            # check whether there are avail vacancies to convert
            avail.vacancies <- CheckForVacancies(cnt.raw.df, t)
            
            while ((st.diff > 0) & (avail.vacancies)) {
              print(sprintf("Floorspace difference is still positive: %s", 
                            st.diff))
              print(sprintf("Allocating vacancies to floorspace of type: %s", 
                            st))
              
              # convert vacant parcel to active floorspace
              res <- FillVacant(cnt.raw.df, to.stype = st, 
                                btl.ratio = btl.ratio, 
                                stock.year = 2016, sim.year = 2020)
              
              if (class(res) == "character") {
                avail.vacancies <- FALSE
              } else {
                # update parcel data after vacancy filled
                cnt.raw.df <- res[[1]]
                filled.apn <- res[[2]]
                print(paste0("Allocated FS from vacant lot on parcel w/ APN: ", 
                             filled.apn))
                # check floorspace difference
                st.diff <- CheckDiff(cnt.raw.df, st, 
                                     status = c("Remain", "Rebuild", 
                                                "LotsFilled", "VacantFilled"),
                                     year = 2020, area.col = "BArea.20",
                                     projections.data = proj.df)
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
                           year = 2020, area.col = "BArea.20",
                           projections.data = proj.df)
      
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
    cnt.sum.20.post <- ddply(cnt.raw.df, .(Typef, Status20), .drop=F, 
                             summarise, SUM.FS.20 = sum(na.omit(BArea.20)), 
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
      output.list[["LotsFilled"]] <- cnt.row
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
    
    
    #-------------------------------------------------------------------------#
    
    
    ############ PHASE II: PROJECT TO 2050 ######
    print("Starting Phase II: Determining 2020 building stock")
    
    ## Step 1. Determine 2020 building stock
    cnt.sum.20 <- ddply(cnt.raw.df, .(Typef), .drop = F,
                        summarise, SUM.BArea.20 = sum(na.omit(BArea.20)))
    
    cnt.sum.20 <- merge(cnt.sum.20, tst.df, by = "Typef")
    
    # assign AGROUP field vals
    cnt.raw.df <- AssignAgroup(cnt.raw.df)
    
    # write output shapefile for 2016 building stock
    output.sp <- WriteShapefile(cnt.raw.df, cnt.raw.sp, cnt.code, 2020)
    
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
    parcel.subtypes <- fs.diffs.df$Typef
    
    drops <- c("PUD", "Vacant")
    subtype.iterators <- parcel.subtypes[!(parcel.subtypes %in% drops)]
    
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
      
      t <- GetParentType(st, key = tst.df)
      
      # subset for row of particular building type
      type.row <- fs.diffs.df[fs.diffs.df$Typef == st, ]
      fs.diff <- type.row$DIFF
      
      # calculate BArea to LotArea ratio for usetype
      print("Calculating building to lot ratio..")
      btl.ratio <- CalcBtlRatio(cnt.raw.df, st, year = 2016)
      
      # TEMP: error handling if no valid btl ratio calculated
      if (is.na(btl.ratio)) {
        # add btl of zero to btl ratio df
        btl.row <- data.frame(Typef = st, BTL.RATIO = 0)
        btl.ratio.df <- rbind(btl.ratio.df, btl.row)
        btl.ratio <- GetStateBtl(year, group = st)
        
      } else if (btl.ratio == 0) {
        # add btl of zero to btl ratio df
        btl.row <- data.frame(Typef = st, BTL.RATIO = 0)
        btl.ratio.df <- rbind(btl.ratio.df, btl.row)
        btl.ratio <- GetStateBtl(st.btl.df, group = st)
        
      } else {
        
        # check if this county-subtype btl ratio is an outlier for the state
        is.outlier <- CheckIfOutlier(st.btl.df, st, btl.ratio)
        
        if (is.outlier) {
          btl.row <- data.frame(Typef = st, BTL.RATIO = btl.ratio)
          btl.ratio.df <- rbind(btl.ratio.df, btl.row)
          btl.ratio <- GetStateBtl(st.btl.df, group = st)
          
        } else {
          
          # add the calculated btl to btl ratio df
          btl.row <- data.frame(Typef = st, BTL.RATIO = btl.ratio)
          btl.ratio.df <- rbind(btl.ratio.df, btl.row)
        }
      }
      
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
          print(sprintf("Vacated building on parcel with APN: %s", 
                        switched.apn))
          
          # check updated floorspace difference following switch
          st.diff <- CheckDiff(cnt.raw.df, st, status = c("Remain"), 
                               year=2050, area.col="BArea.50",
                               projections.data = proj.df)
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
                               year=2050, area.col="BArea.50",
                               projections.data = proj.df)
          
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
            st.diff <- CheckDiff(cnt.raw.df, st, 
                                 status = c("Remain", "Rebuild"), 
                                 year = 2050, area.col = "BArea.50",
                                 projections.data = proj.df)
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
                                   year = 2050, area.col = "BArea.50",
                                   projections.data = proj.df)
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
                                   mult = 8, year=2050, 
                                   from.area.col="BArea.20",
                                   to.area.col="BArea.50")
          
          # only update master parcel df if rebuilds were performed
          if (class(res) != "character") {
            cnt.raw.df <- res
          }
          
          ## Second, fill open lots
          print("Filling open lots of same usetype second")
          
          # check whether there is avail lot area to convert
          avail.lots <- CheckForLots(cnt.raw.df, st)
          
          # init floorspace difference tracker for this usetype
          st.diff <- CheckDiff(cnt.raw.df, st, status = c("Remain", "Rebuild"),
                               year = 2050, area.col = "BArea.50",
                               projections.data = proj.df)
          
          # convert open lots up to allowable BTL ratios until FS diff met
          # or convertable lot area runs out.
          while ((st.diff > 0) & (avail.lots)) {
            res <- ConvertLot(cnt.raw.df, stype = st, btl.ratio,
                              stock.year = 2020, sim.year = 2050)
            
            # check if no lots were available to convert
            if (class(res) == "character") {
              avail.lots = FALSE
              
            } else {
              # update master parcel df
              cnt.raw.df <- res[[1]] 
              converted.apn <- res[[2]]
              print(sprintf("Allocated FS from open lot on parcel with APN: %s", 
                            converted.apn))
              
              # update altyrfn and Bin50 fields of parcel with converted lotarea
              converted <- cnt.raw.df[(cnt.raw.df$APN == converted.apn), ]
              converted$altyrfn <- "2050"
              cnt.raw.df[(cnt.raw.df$APN == converted.apn), ] <- converted
              
              # check floorspace difference
              st.diff <- CheckDiff(cnt.raw.df, st, 
                                   status = c("Remain", "Rebuild", 
                                              "LotsFilled"),
                                   year = 2050, area.col = "BArea.50",
                                   projections.data = proj.df)
              print(sprintf("Updated floorspace difference: %s", st.diff))
            }
          }
          
          ## lastly, allocate remaining FS needs to parcels with ustype "Vacant"
          # if the current type in non-industrial
          
          # check if the current type is non-industrial
          if (t != "Ind") {
            
            # check whether there are avail vacancies to convert
            avail.vacancies <- CheckForVacancies(cnt.raw.df, t)
            
            while ((st.diff > 0) & (avail.vacancies)) {
              print(sprintf("Floorspace difference is still positive: %s", 
                            st.diff))
              print(sprintf("Allocating vacancies to floorspace of type: %s", 
                            t))
              
              # convert vacant parcel to active floorspace
              res <- FillVacant(cnt.raw.df, to.stype = st, 
                                btl.ratio = btl.ratio, 
                                stock.year = 2020, sim.year = 2050)
              
              if (class(res) == "character") {
                avail.vacancies <- FALSE
                
              } else {
                # update parcel data
                cnt.raw.df <- res[[1]]
                filled.apn <- res[[2]]
                print(paste0("Allocated FS from vacant lot on parcel w/ APN: ", 
                             filled.apn))
                
                
                # check floorspace difference
                st.diff <- CheckDiff(cnt.raw.df, st, 
                                     status = c("Remain", "Rebuild", 
                                                "LotsFilled", "VacantFilled"),
                                     year = 2050, area.col = "BArea.50",
                                     projections.data = proj.df)
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
                           year = 2050, area.col = "BArea.50",
                           projections.data = proj.df)
      
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
    
    
    #-------------------------------------------------------------------------#
    
    
    ############ PHASE III: ASSESS 2050 SIMULATION RESULTS ############  
    # create 2050 building stock
    cnt.sum.50 <- ddply(cnt.raw.df, .(Typef), .drop = F,
                        summarise, SUM.BArea.50 = sum(na.omit(BArea.50)))
    
    # assign AGROUP field vals
    cnt.raw.df <- AssignAgroup(cnt.raw.df)
    
    # write output shapefile for 2050 building stock
    WriteShapefile(cnt.raw.df, cnt.raw.sp, cnt.code, 2050)
    
    
    # organize particular county's summary data for export
    cnt.row <- MakeRow(cnt.sum.50, cnt.name, "SUM.BArea.50",
                       template.df = subtypes.df, grouping = "Typef")
    
    # write to output file
    output.list[["2050Stock"]] <- cnt.row
    
    # add this counties output stock list to metalist
    output.list
  }
  
  else {
    output.list <- "No_Shapefile_Features"
    output.list
  }
}
  
  
  
  
  

  
  
  
  
  
  
  
    
    
    
    