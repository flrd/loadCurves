#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# attach packages
library(shiny)
library(jsonlite)
library(shinyjs)
library(bslib)
library(uuid)

# get today's date
today <- Sys.Date()



# Define UI for application that draws a histogram
ui <- fixedPage(
    
    # include script to enable copy to clipboard functionality
    tags$head(tags$script(src = "js/copy.js")),

    # includeCSS("www/css/sidebar.css"),
    
    # styling of the app using bslib package: 
    # https://cran.r-project.org/web/packages/bslib/index.html
    theme = bslib::bs_theme(version = 5,
                            bootswatch = "zephyr",
                            code_font = font_google("Inconsolata"),
                            base_font = font_google("Lato"),
                            primary = "#6200E9",
                            success = "#00BD9A",
                            info = "#4186E0",
                            warning = "#11A859",
                            danger = "#F9491B",
                            font_scale = 0.9, 
                            spacer = ".7rem"),

    # Set up shinyjs
    shinyjs::useShinyjs(),

    # Application title
    tags$h2("Generate a load curve", class = "display-6 pb-3"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      # position = "right"
      # fluid = FALSE,
  
        sidebarPanel(width = 3,

# energy type -------------------------------------------------------------

        tags$div(
          class = "row"
          ,tags$label(
            class = "col-sm-5 col-form-label"
            ,"Energy type"
            ,`for`= "energyType"
          )
          ,tags$div(
            class = "col-sm-7"
            ,selectInput(
              inputId = "energyType"
              ,label = NULL
              ,choices = c("Electricity", "Gas")
              ,selected = "Electricity"
            )
          )
        ),


# desired output ----------------------------------------------------------

        tags$div(
          class = "row"
          ,tags$label(
            class = "col-sm-5 col-form-label"
            ,"Output"
            ,`for`= "outputFormat"
          )
          ,tags$div(
            class = "col-sm-7"
            ,selectInput(
              inputId = "outputFormat"
              ,label = NULL
              ,choices = c("JSON", "MSCONS")
              ,selected = "JSON"
            )
          )
        ),


# Interval ----------------------------------------------------------------

        tags$div(
          class = "row"
          ,tags$label(
            class = "col-sm-5 col-form-label"
            ,"Interval"
            ,`for`= "interval"
          )
          ,tags$div(
            class = "col-sm-7"
            ,selectInput(
              "interval",
              label = NULL,
              choices =
                list(
                  "5 minutes" = "5 mins",
                  "15 minutes" = "15 mins",
                  "30 minutes" = "30 mins",
                  "60 minutes" = "hour",
                  "1 day" = "day"),
              selected = "hour")
          )
        ),


# total consumption -------------------------------------------------------

        tags$div(
          class = "row"
          ,tags$label(
            class = "col-sm-5 col-form-label"
            ,"Quantity"
            ,`for`= "totalConsumption"
          )
          ,tags$div(
            class = "col-sm-7"
            ,tags$div(
              class="form-group shiny-input-container"
              ,tags$input(
                id = "totalConsumption"
                ,type = "number"
                ,class = "form-control"
                ,value = NA_integer_
                ,placeholder = "0 — 999999.99"
                ,min = 0
                ,max = 999999
                ,step = 1
              )
            )
          )
        ),


# Period ------------------------------------------------------------------

        dateRangeInput(
          inputId = "period"
          ,"Period"
          ,start = today - 1L
          ,end = today
          ,separator = "to"
          ,format = "yyyy, M d"
        ),


# location (MaLo / MeLo) --------------------------------------------------

        textInput(
          "marketLocation"
          ,"Location"
        ),


# market part (Strom: MSB, Gas: Netzbetreiber) --------------------------------------------------

        textInput(
          "marketPartnerNumber"
          ,"Market partner number"
        ),


# Register --------------------------------------------------

        textInput(
          "register"
          ,"Register"
          ,placeholder = "e.g. 1-1:1.29.0"
        ),


# Receiver, only needed when MSCONS format is desired ---------------------

        div(
          id = "receiverInput",
          textInput(
            "receiver"
            ,"Receiver"
          )
        ) |>
          # CSS to hide the element by default
          htmltools::tagAppendAttributes(
            style = "display:none"),




# Action button (generate output) -----------------------------------------

        div(
          class="d-grid gap-2 pt-3"
          ,actionButton(
            "generate"
            ,"Generate"
            ,class = "btn-primary fw-normal fs-6"
            )
          ,actionButton(
            "copyToClipboard"
            ,label = "Copy to clipboard"
            ,class = "btn-secondary fw-normal fs-6"
            )
          )


          ,tags$small(class = "p-2"
            ,tags$div(
              tags$p("Spotted something or have an improvement to suggest?", 
              tags$a("Open an issue", href = "https://github.com/flrd/loadCurves/issues", target="_blank")
              ),
              )
            )
        ),


        mainPanel(width = 9,
          verbatimTextOutput("outputMessage") |>
            # make the text output editable
            htmltools::tagAppendAttributes(contenteditable="true")
          )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, server) {
  
    # dateRange input returns date class, here we make it date-time class
    periodStart <- reactive({ as.POSIXlt(input$period[[1]]) })
    periodEnd <- reactive({ as.POSIXlt(input$period[[2]]) })
    
    # energyType equals 1 for elec, 2 for gas, same for locationType
    energyType <- locationType <- reactive({ if(elecTrue()) 1L else 2L })
    
    
    # based on the specified interval, calculate its duration in minutes
    valueInterval <- reactive({ 
      switch(
        EXPR = input$interval,
        "5 mins" = 5L,
        "15 mins" = 15L,
        "30 mins" = 30L,
        "hour" = 60L,
        "day" = 1440L
      )
    })
    
    
    # based on start date, end date and interval, generate a time sequence
    # starting at periodStart to periodEnd
    timeSequence <- reactive({
      interval(
        from = periodStart()
        ,to = periodEnd()
        ,by = input$interval
      )
    })
    
    
    # get the length of the time sequence
    n <- reactive({ length(timeSequence()) })
    
    
    # date-times based on desired output format in JSON object
    timeSequenceJSON <- reactive({ format(timeSequence(), format = "%Y-%m-%d %H:%M:%S") })
    
    
    # for MSCONS we need 2 time sequences:
    # 1st for the start of measurement, 2nd for end of period
    # end of period equals start + selected interval
    # because POSIXt calculates in seconds, we multiply interval * 60 (seconds)
    timeSequenceStartMSCONS <- reactive({ format(timeSequence()[-n()], format = "%Y%m%d%H%M") })
    timeSequenceEndMSCONS <- reactive({ format(timeSequence()[-1], format = "%Y%m%d%H%M") })
    
    
    # calculate quantities
    quantity <- reactive({
      
      list(
        "JSON" = values(n = n(), totalConsumption = input$totalConsumption),
        "MSCONS" = values(n = n() - 1, totalConsumption = input$totalConsumption)
        )
      })
    
    
    # check for energy type: TRUE for electricity, FALSE for gas
    elecTrue <- reactive({ input$energyType == "Electricity" })
    
    
    # change the placeholder in the text inputs
    # conditionally on the energy type
    observeEvent(input$energyType, {

        # change the placeholder for:
        # market location, market partner, receiver, register
        
        Map(f = placeholder
            ,inputId = list("marketLocation"
                            ,"marketPartnerNumber"
                            ,"receiver"
                            ,"register")
            ,condition = elecTrue()
            
            # to be shown for electricity contracts
            ,valuesElec = list("Market location"
                              ,"Metering point operator"
                              ,"BDEW / ILN number"
                              ,"e.g. 1-1:1.29.0")
            
            # to be shown for gas contracts
            ,valuesGas = list("Meter location"
                               ,"Grid operator"
                               ,"VDEW / ILN number"
                               ,"e.g. 7-1:3.1.0")
            )
      })
    
    # show a notification when copy button was clicked
    observeEvent(input$copyToClipboard, {
      showNotification(
        "Output copied!",
        closeButton = FALSE,
        duration = 2.5,
        type = "warning" 
      )
    })
    

    # limit the maximum allowed characters in the Market location input
    # 11 : if energy type is electricity (MaLo-ID)
    # 33 : if energy type is gas (MeLo-ID)
    numberOfChars <- reactive(ifelse(elecTrue(), "11", "33"))
    observe({
        shinyjs::runjs(sprintf("$('#marketLocation').attr('maxlength', %s)", numberOfChars()))
    })
    
    # TODO: when there are >11 chars in the input element, and a users picks
    # elec. again, we should show a notification that only 11 chars are allowed
    # https://shiny.rstudio.com/articles/notifications.html


    # if desired output is MSCONS, show receiverInput element
    observe({
        shinyjs::toggle("receiverInput", anim = TRUE, condition = input$outputFormat == "MSCONS")
    })
    

# generate data structure to be converted to JSON -------------------------
      
    
      loadCurves <- reactive({
        sprintf(
          '{"marketPartnerNumber":"%s","register":"%s","energyType":%d,"valueInterval":%d,"locationNumber":"%s","locationType":%d,"createdBy":"string",'
          ,input$marketPartnerNumber, input$register, energyType(), valueInterval(), input$marketLocation, locationType()
          )})

      loadCurveSets <- reactive({
        sprintf(
          '{"channel":1,"periodStart":"%s","periodEnd":"%s","quality":1,"createdBy":"string",'
          ,format(input$period[[1]], "%Y-%m-%d 00:00:00"), format(input$period[[2]], "%Y-%m-%d 00:00:00")
          )})

      channelInformation <- reactive({
        sprintf(
          '{"messageReference":"%s","sender":"%s","messageDateTime":"%s"}',
          uuid::UUIDgenerate() |> strtrim(width = 32), input$marketPartnerNumber, Sys.time()
          )})
      
      qty <- reactive({
        paste(
          sprintf('{"timestamp":"%s","quantity":"%s","quality":1,"createdBy":"string"}'
                  , timeSequenceJSON(), quantity()[["JSON"]]),
          collapse = ","
        )
      })

      # jsonMinified <- reactive({
      #   sprintf('{"loadCurves":[%s"loadCurveSets":[%s"channelInformation":%s,"values":[%s]}]}]}'
      #           ,loadCurves(), loadCurveSets(), channelInformation(), qty())
      #   })
      
      jsonMinified <- reactive({
        JSON(
          loadCurves = loadCurves()
          ,loadCurveSets = loadCurveSets()
          ,channelInformation = channelInformation()
          ,values = qty()
        )
      })
      
      jsonPrettified <- reactive({
        jsonMinified() |> jsonlite::prettify(indent = 2)
      })
    
    
      # calculate the DTM_QTY segments for MSCONS output
      # there will be n - 1 values in the end
      DTM_QTY <- reactive({
        valuesMSCONS(
          intervalStarts = timeSequenceStartMSCONS()
          ,intervalEnds = timeSequenceEndMSCONS()
          ,quantity = quantity()[["MSCONS"]]
          )
      })
      
      
      outputMessage <- eventReactive(input$generate, ignoreInit = TRUE, {
        
        if(input$outputFormat == "JSON") {
          
          jsonPrettified()
        
          } else {
          MSCONS(
            sender = input$marketPartnerNumber
            ,receiver = input$receiver
            ,marketLocation = input$marketLocation
            ,register = input$register
            ,periodStart = periodStart()
            ,periodEnd = periodEnd()
            ,DTM_QTY = DTM_QTY()
            ,n = n()
            ,totalConsumption = input$totalConsumption
            ,energyType = input$energyType
            )
          }
        
        
        })
      
      output$outputMessage <- renderPrint(outputMessage())
    
}

# Run the application 
shinyApp(ui = ui, server = server)


