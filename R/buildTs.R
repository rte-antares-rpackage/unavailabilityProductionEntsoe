#' Transform RDS file to csv
#'
#'
#' @param path \code{character} data path (refear to a CSV file)
#' @param outFile \code{character} file to write (.csv)
#' @param minDate \code{POSIXct} minimum of dates
#' @param maxDate \code{POSIXct} maximum of dates
#' @param overlappingvalues \code{character} operation for overlap, can be "sum" or "min"
#' @param clipping \code{boolean}, clipping times series by maximum capacity of produciton
#'
#' @examples
#' \dontrun{
#'   giveTS("FRindispo2.csv", outFile = "FRindispoTS.csv")
#' }
#'
#' @import data.table
#'
#' @export
giveTS <- function(path, outFile,
                   minDate = as.POSIXct("2015-01-01", tz = "UTC"),
                   maxDate = as.POSIXct("2018-12-31", tz = "UTC"),
                   overlappingvalues = "max", clipping = FALSE){
  
  . <- nominalPower<-powerSystemResourcesName <- unavailablePowerMW <- NULL
  dta <- fread(path)
  dta <- cleanAfterRead(dta)
  seqHour <- seq(minDate, maxDate, by = "hour")
  dta$id <- as.factor(dta$id)
  dta$quantityIndispo <- dta$nominalPower  - dta$availablePower
  dta[is.na(powerSystemResourcesName)]$powerSystemResourcesName <- paste0("PU.", dta[is.na(powerSystemResourcesName)]$registeredResourceName)
  dta[powerSystemResourcesName == ""]$powerSystemResourcesName <- paste0("PU.", dta[powerSystemResourcesName == ""]$registeredResourceName)
  
  dtatp <- copy(dta)
  
  allFold <- unique(dta$productionType)
  gsub(" ", "_", allFold)
  dta <- split(dta, dta$powerSystemResourcesName)
  end <- rbindlist(lapply(dta, function(X){
    X <- .dbind(X, seqHour, overlappingvalues = overlappingvalues)
    X
  }))
  
  if(clipping){
    
  dtatp <- dtatp[, .SD, .SDcols = c("powerSystemResourcesName", "nominalPower")]
  dtatp <- dtatp[order(powerSystemResourcesName)]
  dtatp <- dtatp[,.(nominalPower = max(nominalPower)), by = "powerSystemResourcesName"]
  end <- merge(end,dtatp, by = "powerSystemResourcesName" )
  toWarn <- end[unavailablePowerMW>nominalPower]
  toWarn <- unique(toWarn$powerSystemResourcesName)
  if(length(toWarn)>0){
    sapply(toWarn, function(X){
      warning(paste0("clipping done for ", X) )
    })
    
    end[unavailablePowerMW>nominalPower]$unavailablePowerMW <- end[unavailablePowerMW>nominalPower]$nominalPower 
  }
  end$nominalPower <- NULL
  }
  
  fwrite(end, outFile)
  cat("Done!\n")
  invisible()
}

.dbind <- function(dt, seqHour, overlappingvalues)
{
  gc()
  
  unavailablePowerMW <- powerSystemResourcesName <- productionType <- NULL
  cat(paste0("Build time serie for ", dt[1]$powerSystemResourcesName, "\n"))
  # print(dt[1]$powerSystemResourcesname)
  emptyTable <- data.table(date = seqHour,  unavailablePowerMW = 0)
  
  ##Affect min and max if not full hour
  for(gg in 1:nrow(dt)){
    
    ##For first hour
    be <- dt[gg]$start
    qte <- dt[gg]$quantityIndispo
    if(round(be, "hour") != be){
      ratio <- (60 - minute(be))/60
      emptyTable <- .assignFLh(emptyTable,be,  qte, ratio, overlappingvalues)
    }
    

    ##For last hour
    be <- dt[gg]$end
    qte <- dt[gg]$quantityIndispo
    if(round(be, "hour") != be){
      ratio <- (minute(be))/60
      emptyTable <- .assignFLh(emptyTable,be,  qte, ratio, overlappingvalues)
  }
  }
  
  
  for(i in 1:nrow(dt)){
    if(overlappingvalues == "max"){

    emptyTable[date %between% c(dt[i]$start, trunc(dt[i]$end, "hour")- 1 ) &
                 unavailablePowerMW<dt[i]$quantityIndispo,
               unavailablePowerMW := dt[i]$quantityIndispo]
      
    }
    if(overlappingvalues == "sum"){
      emptyTable[date %between% c(dt[i]$start, trunc(dt[i]$end, "hour")- 1 ) ,
                 unavailablePowerMW :=  unavailablePowerMW + dt[i]$quantityIndispo]
    }
    
    
  }
  emptyTable[, powerSystemResourcesName:=dt[1]$powerSystemResourcesName]
  emptyTable[, productionType:=dt[1]$productionType]

  emptyTable
}


#' Product ts from csv file
#'
#'
#' @param path \code{character} csv file product by \code{\link{giveTS}}
#' @param outputFile \code{character} csv output file
#' @param aggregatedCol \code{character} column to aggregate, can be : "date", "hour", "month", "year", "productionType"
#' can also be a vector (c("month", "year"))
#'
#' @param colDcast \code{character}, render result by columns : see exemple
#' @examples
#'
#' \dontrun{
#'  aggregateIndispo("NLindispoTS.csv", "agg1.csv", c("date", "hour", "productionType"))
#'  aggregateIndispo("NLindispoTS.csv", "agg2.csv",
#'   c("date", "hour", "productionType"), colDcast = "productionType")
#' }
#' @export
aggregateIndispo <- function(path, outputFile, aggregatedCol, colDcast = NULL){
  DATES <- unavailablePowerMW <- registeredResourceName <- powerSystemResourcesName <- NULL
  
  
  dt <- fread(path)
  dt$date <-fasttime::fastPOSIXct(dt$date, tz = "UTC")


  if("date" %in% aggregatedCol){
    aggregatedCol[which(aggregatedCol=="date")] <- "DATES"
    dt[,DATES := as.Date(date)]
  }

  if("hour" %in% aggregatedCol){
    dt[,hour := hour(date)]
  }

  if("month" %in% aggregatedCol){
    dt[,month := month(date)]
  }
  if("year" %in% aggregatedCol){
    dt[,year := year(date)]
  }

  
  
  

  end <- dt[, sum(unavailablePowerMW), by = c(aggregatedCol)]
  
  if("DATES"%in%names(end) & "hour"%in%names(end) ){

  
    end$hour <- ifelse(nchar( end$hour) ==2, as.character(end$hour ), paste0("0", end$hour ))
    end$DATES <- paste0(end$DATES, " ", end$hour, ":00:00")
    end$hour <- NULL
  
  }
  
  setnames(end, "V1", " unavailablePowerMW")

  print(end)
  
  if(!is.null(colDcast)){

    left <- names(end)[!names(end)%in%c(colDcast, " unavailablePowerMW")]
    rigth <- colDcast
    left <- paste(left, collapse = "+")
    formula <-paste0(left, "~", rigth)
    end <- dcast(end, formula,value.var = " unavailablePowerMW")
    fwrite(end, outputFile)
    cat(paste0(outputFile, " create."))
    return(invisible())
  }else{
    fwrite(end, outputFile)
    cat(paste0(outputFile, " create."))
    return(invisible())
  }
}


.assignFLh <- function(emptyTable,be,  qte, ratio, overlappingvalues)
{
  unavailablePowerMW <- NULL
  concern <- trunc(be, "hour")
  indispo <- qte*ratio
  if(overlappingvalues == "max")
  {
    emptyTable[date == concern & unavailablePowerMW<indispo]$unavailablePowerMW <- indispo
  }
  if(overlappingvalues == "sum")
  {
    emptyTable[date == concern]$unavailablePowerMW <- emptyTable[date == concern]$unavailablePowerMW + indispo
  }
  emptyTable
}