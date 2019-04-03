#' Load Estimed Net Transfer Capacity from ENTSOE
#'
#' getDataOutagesProduction request ensoe API.
#'
#' @param entsoeHttp \code{character} enstoe API defalut : "https://transparency.entsoe.eu/api?"
#' @param ... others params
#' 
#' 
#' @examples
#' \dontrun{
#' 
#' getEstimedNetTransferCapacity(
#'   securityToken = "3ea2c90d-8ecb-4452-898a-263ea835f498",
#'   documentType="A61",
#'   contract_MarketAgreement.Type = "A01",
#'   in_Domain = "10YFR-RTE------C",
#'   out_Domain = "10YGB----------A",
#'   periodStart = "2018-10-12",
#'   periodEnd = "2018-10-12")
#' }
#'
#' @export 
getEstimedNetTransferCapacity <- function(entsoeHttp = "https://transparency.entsoe.eu/api?", ...){
  req = .getnerateRequest(...)
  req = paste0(entsoeHttp, req)
  req
  xmlfiles <- .getDataToXML(req)
  xmlfiles = list.files(xmlfiles, full.names = TRUE)
  library(XML)
  library(data.table)
  DTlist <- xmlToList(xmlParse(xmlfiles))
  names(DTlist)
  
  allINfo = data.table(
    mRID = DTlist$mRID,
    indomain = DTlist$TimeSeries$in_Domain.mRID$text,
    outdomain = DTlist$TimeSeries$out_Domain.mRID$text
  )
  
  X = DTlist$TimeSeries
  
  output = data.table(allINfo, rbindlist(lapply(DTlist[names(DTlist)=="TimeSeries"], .dParseTs)))
  output$value <- as.numeric(output$value)
  output
  
  
}

.dParseTs <- function(X){
  begin <- X$Period$timeInterval$start
  end <- X$Period$timeInterval$end
  begin <- as.POSIXct(begin, format = "%Y-%m-%dT%H:%MZ", tz = "UTC")  
  
  per <- per[names(per)=="Point"]
  allP <- rbindlist(lapply(per, function(Y){
    data.table(timestamp = begin + 1800 + 3600 * as.numeric(Y$position), value = Y$quantity)
  }))
}


.getnerateRequest = function(...){
  dots <- list(...)
  dots$periodStart <- paste0(gsub("-", "", dots$periodStart), "0000")
  dots$periodEnd <- paste0(gsub("-", "", dots$periodEnd), "2300")
  allName = c()
  for(i in 1:length(dots)){
    allName[i] = sprintf("%s=%s", names(dots)[i],dots[[i]])
  }
  paste(allName, sep = "", collapse = "&")
}
