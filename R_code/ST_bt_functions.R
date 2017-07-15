


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
  if ((yr < cut) & (yr != 0)) {
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
                    template.df, grouping, NAs.to.zeros = T) {
  
  # merge with template row
  row.df <- merge(template.df, summary.data, 
                  by = grouping, drop = F, all.x = T)
  rownames(summary.data) <- summary.data[,grouping]
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

AssignEUIVint <- Vectorize(function(Yrfinal, stock.year) {
  
  yr <- substr(stock.year, 3, 4)
  col.name <- "EUIVint"
  
  # determin which EUIVint calculator function to call
  fun.name <- paste0("CalcEUIVint", yr)
  
  EUIVint <- get(fun.name)(Yrfinal)
  
  return(EUIVint)
  
})

WriteShapefile <- function(parcel.df, county.code, year, 
                           parcel.sp = cnt.raw.sp) {
  
  yr <- substr(year, 3, 4)
  # determine BArea col corresponding to the year of this shapefile's bldg stock
  stock.barea.col <- paste0("BArea.", yr)
  
  # assign EUI vint codes
  parcel.df$EUIVint <- unlist(AssignEUIVint(parcel.df$Yrfinal, 
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
  
  # calculate total energy consumption, heating, and cooling fields
  parcel.df$TotNGSH <- parcel.df[,stock.barea.col]*parcel.df[,"NGEUISH"]
  parcel.df$TotNGWH <- parcel.df[,stock.barea.col]*parcel.df[,"NGEUIWH"]
  parcel.df$TotNGC <- parcel.df[,stock.barea.col]*parcel.df[,"NGEUIC"]
  parcel.df$TotEC <- parcel.df[,stock.barea.col]*parcel.df[,"EEUIC"]
  parcel.df$TotC <- parcel.df[,stock.barea.col]*parcel.df[,"NGEUIPC"]
  parcel.df$TotH <- parcel.df[,stock.barea.col]*parcel.df[,"NGEUIPH"]

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
  
  # check if the target sheet is blank
  readsheet.df <- read.xlsx(output.filename, sheet = sheet.name)
  
  if (is.null(readsheet.df)) {
    
    # write to sheet, including column names
    output.wb <- loadWorkbook(output.filename)
    writeData(output.wb, sheet = sheet.name, data.row, 
                 rowNames = F, colNames = T)
    saveWorkbook(output.wb, output.filename, overwrite = T)
    
  } else {
    # determine next empty row for writing to
    write.row <- nrow(readsheet.df) + 2
    
    # write to sheet, excluding column names
    output.wb <- loadWorkbook(output.filename)
    writeData(output.wb, sheet = sheet.name, data.row, 
              rowNames = F, colNames = F, startRow = write.row)
    saveWorkbook(output.wb, output.filename, overwrite = T)
  }
}

# define function for recoding year based on AGROUP criteria
RecodeYear <- Vectorize(function(yr) {
  
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
  sel.par["TYPE"] <- GetParentType(to.stype)
  
  sel.par[,status.col] <- to.status
  parcel.df[parcel.df$APN == as.character(rand.apn),] <- sel.par
  
  res.list <- list(parcel.df, rand.apn)
  return(res.list)
}
#-----------------------------------------------------------------------------#

# A function that return the parent type of a bldg type passed by the caller
GetParentType <- function (subtype, key = tst.df) {
  type <- tst.df$TYPE[which(tst.df$Typef == subtype)]
  
  return(type)
}


CheckDiff <- function(parcel.df, stype, status, year, 
                      area.col, projections.data = proj.df) {
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
    
    # update Yrfinal and status fields
    t.pars$Yrfinal <- as.character(year)
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
  lar.par <- which(pool[,area.col] == max(pool[,area.col]))
  sel.par <- pool[lar.par,]
  sel.apn <- sel.par$APN
  
  # change type and status of randomly selected parcel
  sel.par$Typef <- to.stype
  sel.par$TYPE <- GetParentType(to.stype)
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
  status.col <- paste0("Status", yr)
  barea.col <- paste0("BArea.", yr)
  lotarea.col <- paste0("LOTAREA.", yr)
  
  # init parcel pool to select from
  pool <- subset(parcel.df, (parcel.df$Typef == stype))
  
  # get barea and lotarea vals
  barea.vals <- pool[,barea.col]
  lotarea.vals <- pool[,lotarea.col]
  
  #TEMP: 
  print(lotarea.vals, echo = T)
  print(barea.vals, echo = T)
  
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
  
  t <- GetParentType(stype)
  
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
    if ((st == "Condo") & par$Typef == "Condo") {
      p.score <- p.score + 5
    }
    
    # boost scores of parcels whose subtype match the subtype passed by caller
    if (par$Typef == st) {
      par$dc.score <- p.score + 10
    }
        
    # update data completeness score field of parcel df
    par$dc.score <- p.score
    
    # update pool.df with scored parcels
    pool[p, ] <- par
    
  }
  
  ## select highest ranked parcel
  top.scoring.pars <- which(pool$dc.score == max(pool$dc.score))
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
  t <- GetParentType(to.stype)
  
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
    
    # update Yrfinal 
    sel.par$Yrfinal <- sim.year
    
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
    
    # update Yrfinal 
    sel.par$Yrfinal <- sim.year
    
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

CheckForLots <- function (subtype) {
  
  type <- GetParentType(st)
  
  if (type == "Condo") {
    check.lots <- cnt.raw.df[(cnt.raw.df$TYPE %in% c("Condo", "MF")) &
                               (cnt.raw.df$Status20 == "LotsOpen"), ]
  } else {
    check.lots <- cnt.raw.df[(cnt.raw.df$TYPE == type) &
                               (cnt.raw.df$Status20 == "LotsOpen"), ] 
  }
  
  if (nrow(check.lots) != 0) {
    avail.lots <- TRUE
  } else {
    avail.lots <- FALSE
  }
  
  return(avail.lots)
}

    
    


GetCountyMultipliers <- function (multipiers.data, county, year) {
 
  # subset multipliers table for this county
  g.mult.50.cnt <- multipiers.data[multipiers.data$County == cnt.name, ]
  
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



CheckForVacancies <- function (type) {
  
  check.vacs <- cnt.raw.df[(cnt.raw.df[,"TYPE"] == "Vacant"),]
  if (nrow(check.vacs) != 0) {
    avail.vacancies <- TRUE
  } else {
    avail.vacancies <- FALSE
  }
  return(avail.vacancies)
}

                                  
                        
    
    
    
    