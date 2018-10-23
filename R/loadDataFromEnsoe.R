#' Load unavailability of production from Ensoe
#'
#' getDataOutagesProduction request ensoe API and create RDS file who contains unavailability of production.
#'
#' @param bz \code{character} build zone, transparency.entsoe.eu API guide
#' @param docType \code{character} docType, A80 for generation unit and A77 for produciton unit
#' @param docStatus \code{character} docStatus "A05" : Active, "A09" : Cancel
#' @param start \code{character} start date
#' @param end \code{character} end date
#' @param EIC \code{character} optional, powerSystemResourcesmRID default NULL -> "All"
#' @param fileToMerge \code{character} csv where data will be save, you can also add information in an existing file.
#' @param by \code{character} load data by day, week, month, year?
#' Maximum of documents load by request is 200 so if data are not correctly try an other timestep.
#' @param token \code{character}, ENTSOE token
#' @param entsoeHttp \code{character}, ENTSOE adress, defalut https://transparency.entsoe.eu/api?
#' 
#' @examples
#' \dontrun{
#'
#'  #PROD : "A77" Gene : "A80"
#'  token <- "Mytoken"
#'  #BiddingZone_Domain = "10YFR-RTE------C"
#'  #BiddingZone_Domain = "10YBE----------2"
#'  #BiddingZone_Domain = "10YNL----------L"
#'  #BiddingZone_Domain = "10Y1001A1001A63L"
#'
#'  #BiddingZone_Domain = "10YDE-VE-------2"    #GRT Allemand 50Hertz
#'  #BiddingZone_Domain = "10YDE-RWENET---I"    #GRT Allemand Amprion
#'  #BiddingZone_Domain = "10YDE-EON------1"    #GRT Allemand TennetDE
#'  #BiddingZone_Domain = "10YDE-ENBW-----N"    #GRT Allemand TransnetBW
#'  #BiddingZone_Domain = "10YAT-APG------L"    #Autriche
#'
#'  #NL
#'  getDataOutagesProduction(token = token, bz = "10YNL----------L",
#'   docType = "A80",
#'    start = "2015-01-01", end = "2018-12-31", fileToMerge = "NLindispo.csv")
#'  getDataOutagesProduction(token = token, "10YNL----------L", docType = "A77",
#'    start = "2015-01-01", end = "2019-01-01", fileToMerge = "NLindispo.csv", by = "year")
#'
#'  #BE
#'  getDataOutagesProduction(token = token, bz = "10YBE----------2", docType = "A80",
#'    start = "2015-01-01", end = "2018-12-31", fileToMerge = "BEindispo.csv")
#'  getDataOutagesProduction(token = token, "10YBE----------2", docType = "A77",
#'    start = "2015-01-01", end = "2018-12-31", fileToMerge = "BEindispo.csv")
#'
#'  #FR
#'  getDataOutagesProduction(token = token, bz = "10YFR-RTE------C", docType = "A80",
#'    start = "2015-01-01", end = "2018-12-31", fileToMerge = "FRindispo.csv")
#'  getDataOutagesProduction(token = token, "10YFR-RTE------C", docType = "A77",
#'    start = "2015-01-01", end = "2018-12-31", fileToMerge = "FRindispo.csv")
#'
#'  #DE
#'  getDataOutagesProduction(bz = "10YDE-VE-------2", docType = "A80",
#'    start = "2015-01-01", end = "2018-12-31", fileToMerge = "DEindispo.csv")
#'  getDataOutagesProduction("10YDE-VE-------2", docType = "A77",
#'   start = "2015-01-01", end = "2019-01-01", fileToMerge = "DEindispo.csv", by = "year")
#'
#'
#'  getDataOutagesProduction(token = token, bz = "10YDE-RWENET---I", docType = "A80",
#'    start = "2015-01-01", end = "2018-12-31", fileToMerge = "DEindispo.csv")
#'  getDataOutagesProduction(token = token, bz = "10YDE-RWENET---I", docType = "A77",
#'    start = "2015-01-01", end = "2019-01-01", fileToMerge = "DEindispo.csv", by = "year")
#'
#'  getDataOutagesProduction(token = token, bz = "10YDE-EON------1", docType = "A80",
#'    start = "2015-01-01", end = "2019-01-01", fileToMerge = "DEindispo.csv", by = "month")
#'
#'  ##Week (for +200 row)
#'  getDataOutagesProduction(token = token, bz = "10YDE-EON------1", docType = "A80",
#'    start = "2015-08-01", end = "2015-09-10", fileToMerge = "DEindispo.csv")
#'  getDataOutagesProduction(token = token, "10YDE-EON------1", docType = "A80",
#'    start = "2016-06-01", end = "2016-09-10", fileToMerge = "DEindispo.csv")
#'  getDataOutagesProduction(token = token, bz = "10YDE-EON------1", docType = "A80",
#'    start = "2016-07-27", end = "2016-08-03", fileToMerge = "DEindispo.csv", by = "day")
#'  getDataOutagesProduction(token = token, bz = "10YDE-EON------1", docType = "A80",
#'    start = "2016-12-01", end = "2017-01-10", fileToMerge = "DEindispo.csv")
#'  getDataOutagesProduction(token = token, bz = "10YDE-EON------1", docType = "A80",
#'    start = "2017-06-01", end = "2017-09-10", fileToMerge = "DEindispo.csv")
#'
#'  getDataOutagesProduction(token = token, bz = "10YDE-EON------1", docType = "A77",
#'    start = "2015-01-01", end = "2019-01-01", fileToMerge = "DEindispo.csv")
#'
#'  getDataOutagesProduction(token = token, bz = "10YDE-ENBW-----N", docType = "A80",
#'    start = "2015-01-01", end = "2019-01-01", fileToMerge = "DEindispo.csv", by = "month")
#'  getDataOutagesProduction(token = token, bz = "10YDE-ENBW-----N", docType = "A77",
#'    start = "2015-01-01", end = "2019-01-01", fileToMerge = "DEindispo.csv", by = "year")
#'
#'  getDataOutagesProduction(token = token, bz = "10YAT-APG------L", docType = "A80",
#'    start = "2015-01-01", end = "2019-01-01", fileToMerge = "DEindispo.csv", by = "month")
#'  getDataOutagesProduction(token = token, bz = "10YAT-APG------L", docType = "A77",
#'    start = "2015-01-01", end = "2019-01-01", fileToMerge = "DEindispo.csv", by = "year")
#'
#'
#' }
#' @import httr XML data.table fasttime shiny
#'
#' @export
getDataOutagesProduction <- function(token, bz = "10YNL----------L",
                                     docType = "A80",
                                     docStatus = "A05",
                                     start = "2017-01-01",
                                     end = "2017-12-31",
                                     EIC = NULL,
                                     fileToMerge = "NLindispo.csv",
                                     by = "week",
                                     entsoeHttp = "https://transparency.entsoe.eu/api?")
{
  powerSystemResourcesName <- registeredResourceName <- powerSystemResourcesName <- powerSystemResourcesName <-  NULL
  be <- as.Date(start)
  en <- as.Date(end)
  seqD <- seq(be, en, by = by)
  seqD <- c(seqD, en)
  outList <- list()

  ##Log infos
  requestValue = "OK"
  ttl <- "Parameters of the request"
  usRq <- paste0("build_zone : ", bz, "____",
                 "start" , " : ", start, "____",
                 "end", " : ", end, "____",
                 "docType", " : ", docType, "____",
                 "docStatus", " : ", docStatus, "____",
                 "by", " : ", by)

  rqErr <- "Everything is ok."
  er <- NA

  for(i in 1:(length(seqD)-1)){


    OK <- NULL
    end2 <- NULL
    numError = 0
    while( is.null(OK) & numError < 10) {
      numError = numError + 1
      try({
        req <- .generateRequest(token = token, seqD[i],
                                seqD[i + 1], bz, docType,
                                docStatus = docStatus, entsoeHttp = entsoeHttp, EIC  = EIC )
        print(seqD[i])
        xmlfiles <- ""
        xmlfiles <- .getDataToXML(req)
        if(xmlfiles[1] != "nodata"){
        end2 <- rbindlist(sapply(xmlfiles, .dpaselist, simplify = FALSE))
        }
        OK <- 1
      })


      if(is.null(OK))Sys.sleep(1)
      if(numError == 9){
        requestValue <- "NOK"

        rqErr <- "The following data has not been downloaded :"
        er <- c(er, req)
      }

    }

    if(!is.null(end2) & xmlfiles[1] != "nodata")
    {
      #outListend <- end
      out <- .addAllColumns(end2,
                            docType = docType,
                            BiddingZone_Domain = bz,
                            docStatus = docStatus)

      #Clean out
      out <- .cleanUnva(out)

      ##Save out

      if(!file.exists(fileToMerge)){
        out <- out[order(powerSystemResourcesName, registeredResourceName)]
        fwrite(out, fileToMerge)
      }else{

        out2 <- fread(fileToMerge)
        out2 <- cleanAfterRead(out2)

        out <- rbindlist(list(out, out2))
        out <- out[!duplicated(out$id)]
        out <- out[order(powerSystemResourcesName, registeredResourceName)]
        
        fwrite(out, fileToMerge)
      }
    }


  }
  ##Write log
  ender <- data.table(request = c(ttl, usRq, rqErr, er))
  fwrite(ender, gsub("-", "",gsub(":", "",gsub(" ", "", paste0(requestValue, "log_getDataOutagesProduction", Sys.time(), ".txt")))), col.names = FALSE)



  if(file.exists(fileToMerge))
  {fread(fileToMerge)}
  else{"No data download"}

}

#' Transform RDS file to csv
#'
#'
#' @param dta \code{data.table} data.table read with read.RDS
#'
#' @import data.table
#'
#' @export
cleanAfterRead <- function(dta){
  dta$start <- as.POSIXct(dta$start, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  dta$end <- as.POSIXct(dta$end, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  dta$publicationTime <- as.POSIXct(dta$publicationTime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  dta$downloadTime <- as.POSIXct(dta$downloadTime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  dta
}
