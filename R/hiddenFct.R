.getDataToXML <- function(toget)
{
  tempD <- tempdir()
  ##Rm old xml
  rdName <- sample(LETTERS, 15, replace = TRUE)
  rdName <- paste0(rdName, collapse = "")

  tempD <- paste0(tempD, "/", rdName)
  dir.create(tempD)
  temp <- tempfile(tmpdir = tempD)

  old <- list.files(tempD, full.names = TRUE)
  old <- old[grepl(".xml", old)]
  file.remove(old)

  GET(toget, write_disk(temp, overwrite = TRUE))
  ##test if no data

  try({
    xmlT <- xmlToList(xmlParse(temp, error = NULL))

    noData <- grepl( "No matching data found",xmlT$Reason$text)
    if(noData){return("nodata")}
  }, silent = TRUE)

  utils::unzip(temp, exdir = tempD)

  fileS <- list.files(tempD)
  fileS <- fileS[grepl(".xml", fileS)]
  allXML <- paste0(tempD, "/", fileS)

  allXML
}

.dpaselist <- function(xmlFile){
  DTlist <- xmlToList(xmlParse(xmlFile))
  TID <- DTlist$mRID
  if(is.null(TID))TID <- NA
  revNumber <- DTlist$revisionNumber
  if(is.null(revNumber))revNumber <- NA
  TYPE <- DTlist$type
  if(is.null(TYPE))TYPE <- NA
  createTid <- DTlist$createdDateTime
  if(is.null(createTid))createTid <- NA
  start <- DTlist$unavailability_Time_Period.timeInterval$start
  if(is.null(start))start <- NA
  end <- DTlist$unavailability_Time_Period.timeInterval$end
  if(is.null(end))end <- NA
  allinfo <- DTlist$TimeSeries
  businessType <- allinfo$businessType
  if(is.null(businessType))businessType <- NA
  mRID <- allinfo$mRID
  if(is.null(mRID))mRID <- NA
  mesureUnit <- allinfo$quantity_Measure_Unit.name
  if(is.null(mesureUnit))mesureUnit <- NA
  reason = DTlist$Reason$code
  if(is.null(reason))reason <- NA




  # curvetype <- allinfo$curveType
  # if(is.null(curvetype))curvetype <- NA
  RegisteredResource <- allinfo$production_RegisteredResource.mRID$text
  if(is.null(RegisteredResource))RegisteredResource <- NA
  RegisteredResourcename   <- allinfo$production_RegisteredResource.name
  if(is.null(RegisteredResourcename))RegisteredResourcename <- NA
  # loc <- allinfo$production_RegisteredResource.location.name
  # if(is.null(loc))loc <- NA
  psrtype <- allinfo$production_RegisteredResource.pSRType.psrType
  if(is.null(psrtype))psrtype <- NA
  powerSystemResourcesmRID <- allinfo$production_RegisteredResource.pSRType.powerSystemResources.mRID$text
  if(is.null(powerSystemResourcesmRID))powerSystemResourcesmRID <- NA
  powerSystemResourcesname <- allinfo$production_RegisteredResource.pSRType.powerSystemResources.name
  if(is.null(powerSystemResourcesname))powerSystemResourcesname <- NA
  nominalPower <- allinfo$production_RegisteredResource.pSRType.powerSystemResources.nominalP$text
  if(is.null(nominalPower))nominalPower <- NA

  qtInfo <- .dparsQuantity(allinfo$Available_Period)





  dt <- data.table(id = TID,
                   revisionNumber = revNumber,
                   type = TYPE,
                   # loc = loc,
                   publicationTime = createTid,
                   start = start,
                   end = end,
                   businessType = businessType,
                   mesureUnit = mesureUnit,
                   # curvetype = curvetype,
                   registeredResource = RegisteredResource,
                   registeredResourceName = RegisteredResourcename,
                   powerSystemResourcesmRID = powerSystemResourcesmRID,
                   powerSystemResourcesName = powerSystemResourcesname,
                   nominalPower = nominalPower,
                   psrtype = psrtype,
                   availablePower = qtInfo$mqt,
                   variabilty = qtInfo$qti,
                   reason = reason)
  dt
}


.generateRequest <- function(token, stardday,
                            endday,
                            BiddingZone_Domain,
                            docType, docStatus = "A05", entsoeHttp, EIC){

  periodStart <- paste0(gsub("-", "", stardday), "0000")
  periodEnd <- paste0(gsub("-", "", endday), "0000")
  if(is.null(EIC))
  {
  toget <- sprintf("%sDocumentType=%s&BiddingZone_Domain=%s&securityToken=%s&DocStatus=%s&periodStart=%s&periodEnd=%s",
                   entsoeHttp, docType,BiddingZone_Domain, token, docStatus, periodStart, periodEnd)
  }else{
    toget <- sprintf("%sDocumentType=%s&BiddingZone_Domain=%s&securityToken=%s&DocStatus=%s&periodStart=%s&periodEnd=%s&RegisteredResource=%s",
                     entsoeHttp, docType,BiddingZone_Domain, token, docStatus, periodStart, periodEnd, EIC)
  }
  toget
}

.addColumn <- function(out, match, on, code, endname)
{
  On <- NULL
  ##Add psrtype
  matchTP <- match[On == on]
  matchTP[, On := NULL]
  setnames(matchTP, "Code", code)
  setnames(matchTP, "Meaning", endname)
  if(all(is.na(out$psrtype))){
    out[, c(endname) := NA]
    return(out)
  }
  out <- merge(out, matchTP, by = code, all.x = TRUE)
  out
}

###Add columns
.addAllColumns <- function(out, docType, BiddingZone_Domain, docStatus){
  
  On <- Code <- BiddingZone <- NULL
  
  
  print(docStatus)
  docStatus2 <- docStatus
  match <- fread(system.file("Match.csv", package = "unavailabilityProductionEntsoe"))
  ##Add BiddingZone_Domain

  bzName <- match[On == "zone" & Code == BiddingZone_Domain]$Meaning
  if(length(bzName)>0)
  {
    out[, BiddingZone :=bzName]
  }else{
    out[, BiddingZone := NA]
  }
  out$BiddingZone_Domain <- BiddingZone_Domain
  out$downloadTime <-as.POSIXct(round(Sys.time(), "secs"))


  out <- .addColumn(out, match, "psrtype", "psrtype", "productionType")
  out <- .addColumn(out, match, "BusinessType", "businessType", "outageType")
  out[, docType := ifelse(docType == 'A80', "Generation", "Produciton")]
  out[, docStatus := ifelse(docStatus2 == 'A05', "Active", "Cancel")]

  out
}

.cleanUnva <- function(out)
{
  #Sys.setenv(TZ='UTC') # to reproduce OP's example

  revisionNumber <- nominalPower <- availablePower <- NULL
  out$start <- as.POSIXct(out$start, format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
  out$end <- as.POSIXct(out$end, format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
  out$publicationTime <- as.POSIXct(out$publicationTime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  out[, revisionNumber := as.numeric(revisionNumber)]
  out[, nominalPower := as.numeric(nominalPower)]
  out[, availablePower := as.numeric(availablePower)]
  setcolorder(out, c("id" ,"registeredResource" ,"registeredResourceName" ,
              "powerSystemResourcesmRID" ,"powerSystemResourcesName" ,
              "BiddingZone" ,"productionType" ,"outageType" ,"start" ,
              "end" ,"nominalPower" ,"availablePower" ,"mesureUnit" ,"publicationTime" ,"revisionNumber" ,"businessType" ,"psrtype" ,"type" ,"variabilty" ,
              "reason" ,"BiddingZone_Domain" ,"docType" ,"docStatus" ,"downloadTime"))
  out
}

.chavauchement <- function(out){
  start <- powerSystemResourcesName <- NULL
    
  dubble <- rbindlist(sapply(1:nrow(out), function(X){
    outX <- out[X]
    outTp <- out[-X]
    end <- outTp[( start < outX$end & end > outX$start ) &
                   powerSystemResourcesName == outX$powerSystemResourcesName]
    if(nrow(end)>0)
    {
      end <- rbind(end, outX)
      end <- rbind(end, data.table(id = paste0(sample(LETTERS, 8), collapse = "")), fill = TRUE)
      end
    }
    end
  }, simplify = FALSE))
  dubble <- dubble[!duplicated(dubble$id)]
  # dubble <- dubble[order(powerSystemResourcesname, start)]
  dubble

}

