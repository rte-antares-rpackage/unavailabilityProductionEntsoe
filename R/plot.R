#' unavailability time serie for generation unit or produciton unit
#'
#' @description Time serie vizualisation of unavailability
#'
#' @param path \code{character} csv file
#' @param name \code{character} powerSystemResourcesName(s)
#'
#' @examples
#' \dontrun{
#' 
#' 
#' getDataOutagesProduction(token = token, bz = "10YBE----------2", docType = "A80",
#'                          start = "2018-10-01", end = "2018-12-31", fileToMerge = "BE.csv")
#' 
#' 
#' getDataOutagesProduction(token = token, "10YBE----------2", docType = "A77",
#'                          start = "2018-10-01", end = "2018-12-31", fileToMerge = "BE.csv")
#' 
#' 
#' 
#' giveTS("BE.csv", outFile = "BETS.csv", overlappingvalues = "sum")
#' unavailabilityTS("BETS.csv", c("PU.DOEL 1", "PU.DOEL 2", "PU.DOEL 4"))
#' 
#' }
#'
#' @import rAmCharts
#' @export
unavailabilityTS <- function(path, name){
  powerSystemResourcesName <- NULL
  dt <- fread(path)
  Rnam <-name
  dt <- dt[powerSystemResourcesName%in%Rnam]
  dt <- dcast(dt, date~powerSystemResourcesName, value.var = "unavailablePowerMW")
  Rnam <- names(dt)[2:ncol(dt)]
  names(dt)[2:ncol(dt)] <- paste0("X", 2:ncol(dt))
  
  
  
  graphS <- sapply(1:length(Rnam), function(X){
    rAmCharts::amGraph(valueField = names(dt)[X+1], title = Rnam[X])
  })
  
  #Plot
  pipeR::pipeline(
    amSerialChart(dataProvider = dt, theme = 'light', categoryField = 'date'),
    setGraphs(graphS),
    setCategoryAxis(labelRotation = 90),
    setChartCursor(),
    setExport(),
    addTitle(text = "Unavailability power per generation / production unit"),
    setLegend(enabled = TRUE, useGraphSetting = TRUE),
    plot(width = "100%", height = "600px")
  )
  
  
}


#' unavailability time serie after aggregation
#'
#' @description Time serie vizualisation of unavailability
#'
#' @param path \code{character} csv file
#' @param name \code{character} powerSystemResourcesName(s)
#'
#' @examples
#' \dontrun{
#'
#'
#' getDataOutagesProduction(token = token, bz = "10YBE----------2", docType = "A80",
#'                          start = "2018-10-01", end = "2018-12-31", fileToMerge = "BE.csv")
#'
#'
#' getDataOutagesProduction(token = token, "10YBE----------2", docType = "A77",
#'                          start = "2018-10-01", end = "2018-12-31", fileToMerge = "BE.csv")
#'
#'
#'
#' giveTS("BE.csv", outFile = "BETS.csv", overlappingvalues = "sum")
#' aggregateIndispo("BETS.csv", "BEAGG.csv",
#'   c("date", "hour", "productionType"), colDcast = "productionType")
#'   
#'   
#' unavailabilityPUTS("BEAGG.csv", c("Fossil Gas", "Nuclear"))
#'
#'
#' }
#'
#' @import rAmCharts
#' @export
unavailabilityPUTS <- function(path, name){
  powerSystemResourcesName <- NULL
  dt <- fread(path)
  Rnam <-name
  Rnam <- Rnam[Rnam%in%names(dt)]
  
  
  dt <- dt[,.SD, .SDcols = c("DATES", Rnam)]
  Rnam <- names(dt)[2:ncol(dt)]
  names(dt)[2:ncol(dt)] <- paste0("X", 2:ncol(dt))



  graphS <- sapply(1:length(Rnam), function(X){
    rAmCharts::amGraph(valueField = names(dt)[X+1], title = Rnam[X])
  })

  #Plot
  pipeR::pipeline(
    amSerialChart(dataProvider = dt, theme = 'light', categoryField = 'DATES'),
    setGraphs(graphS),
    setCategoryAxis(labelRotation = 90),
    setChartCursor(),
    setExport(),
    addTitle(text = "Unavailability power per production type"),
    setLegend(enabled = TRUE, useGraphSetting = TRUE),
    plot(width = "100%", height = "600px")
  )


}


#' Load unavailability of production from Ensoe
#'
#' @description getDataOutagesProduction request ensoe API and create RDS file who contains unavailability of production.
#'
#' @param path \code{character} csv file
#'
#' @examples
#' \dontrun{
#'   runIndispoApp("FFFindispoTEST8006.csv")
#' }
#'
#' @export
runIndispoApp <- function(path)
{
  
  start <- end <- BiddingZone_Domain <- quantityIndispo <- nominalPower <-nominalPower <-availablePower <-outageType <-productionType <- . <- new <- NULL
  
  
  
  ui <- fluidPage(
    
    tags$head(
      tags$style(HTML("
.legend { list-style: none; }
.legend li { float: left; margin-right: 10px; }
                      .legend span { border: 1px solid #ccc; float: left; width: 40px; height: 20px; margin: 2px; }
                      /* your colors */
                      .legend .superawesome { background-color: #a94f00; }
                      .legend .awesome { background-color: #aa0000; }

    "))
    ),
    
    checkboxInput("doubleOnly", "Show only overlapping outages", FALSE),
    dateRangeInput(inputId = "date", label = "Date UTC", start = "2015-01-01", end = Sys.Date()+365   ),
    uiOutput("inil"),

    selectInput("planed", "Planned outage", choices = list(Yes = "Planned maintenance", No = "Unplanned outage"), multiple = TRUE,
                selected =c("Planned maintenance","Unplanned outage") ),
    uiOutput("fil"),
    uiOutput("availableGroup"),
    column(12, 
    HTML(
      '<ul class="legend">
    <li><span class="superawesome"></span> Planned outage</li>
    <li><span class="awesome"></span> Unplanned outage</li>
</ul><br>'
      
      
      
    )),
    column(12, timevis::timevisOutput("mytime"))
   
  )

  server <- function(input, output, session) {

    initialData <- reactive({
      #toLoad <- paste0(path)
      #out <- readRDS(toLoad)

      out <- fread(path)
      out <- cleanAfterRead(out)
      out <- out[start %between% c(as.POSIXct(input$date[1]), as.POSIXct(input$date[2]))| end %between% c(as.POSIXct(input$date[1]), as.POSIXct(input$date[2]))]
      out$powerSystemResourcesName[is.na( out$powerSystemResourcesName)] <- paste0("PU.", out$registeredResourceName[is.na( out$powerSystemResourcesName)])
      out$powerSystemResourcesName[out$powerSystemResourcesName == ""] <- paste0("PU.", out$registeredResourceName[out$powerSystemResourcesName == ""])
      
      
      out
    })

    dta1 <- reactive({
      DT <- initialData()
      DT <- DT[BiddingZone_Domain%in%input$zoneName]

      if(input$doubleOnly){
        DT <- .chavauchement(DT)
      }

      data.table(DT)
    })

    dta2 <- reactive({
      if(is.null(input$planed))return(NULL)
      if(is.null(input$planed))return(NULL)
      DT <- dta1()

      DT[, quantityIndispo := nominalPower  - availablePower]

      DT <- DT[outageType %in% input$planed]
      DT <- DT[productionType%in%input$prodType]

      if(nrow(DT)>0){

        style = ifelse(DT$outageType == "Planned maintenance",
                       "background-color: #00AA00;border-color: #00AA00;",
                       "background-color: #AA0000;border-color: #AA0000;")
        content <- paste0("<b>", DT$id, "</b>")

        nam <- names(DT)
        DTtemp <- copy(DT)
        
        for(vv in 1:ncol(DTtemp)){
          nn <- names(DTtemp)[vv]
          DTtemp[, c(nn) := .(paste0(nn, " : ",DTtemp[[nn]] ))]
        }
        
        
        DTtemp[ , new := do.call(paste, c(.SD, sep = "\n"))]
        
        title <-DTtemp$new
        type = ""

      }else{
        style = character(0)
        content = character(0)
        title = character(0)
        type = character(0)

      }

      print(DT)
      DT <- data.table(
        id      = as.character(DT$id),
        content = content,
        productionType = DT$productionType,
        start   = DT$start,
        end     = DT$end,
        title = title,
        outageType = DT$outageType,
        group = DT$powerSystemResourcesName,
        style = style,
        type = type)
        return(DT)
    })

    allProd <- reactive({
      unique(dta2()$group)
    })


    output$inil <- renderUI({
      selectInput(inputId = "zoneName" , label = "BiddingZone Domain", choices = c(unique(initialData()$BiddingZone_Domain)) )
    })


    output$fil <- renderUI({
      selectInput("prodType", "Production Type", sort(unique(initialData()$productionType)), multiple = TRUE)
    })

    output$availableGroup <- renderUI({
      div(
        column(2,selectInput("powerSystemResourcesName", "Ressource name", sort(allProd()), multiple = TRUE)),
        column(1,checkboxInput("powerSystemResourcesnameAll", "all Ressource name", FALSE))

      )
    })

    gp <- reactive({
      if(is.null(input$powerSystemResourcesnameAll))return(NULL)
      group = data.table(id = allProd(),
                         content = allProd())
      if(input$powerSystemResourcesnameAll){
        return(group[content %in% allProd()])

      }else{
        return(group[content %in% input$powerSystemResourcesName])

      }
    })

    output$mytime <- timevis::renderTimevis({
      if(is.null(input$powerSystemResourcesnameAll))return(timevis::timevis())
      #dt <- dta2()[group %in% input$powerSystemResourcesName]

      if(input$powerSystemResourcesnameAll){
        dt <- dta2()

      }else{
        dt <-  dta2()[group %in% input$powerSystemResourcesName]

      }
      

      timevis::timevis(dt, groups = gp(),
              options = list(showTooltips = TRUE)
      )})
  }

  shinyApp(ui = ui, server = server)
}
