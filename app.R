#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(jsonlite)
library(shinyjs)
library(bslib)
library(tippy)
library(shinyFeedback)


today <- Sys.Date()



# Define UI for application that draws a histogram
ui <- fixedPage(
    
    # include script to enable copy to clipboard functionality
    tags$head(tags$script(src = "js/copy.js")),
    
    # styling of the app using bslib package: 
    # https://cran.r-project.org/web/packages/bslib/index.html
    theme = bslib::bs_theme(version = 5,
                            bootswatch = "zephyr",
                            code_font = font_google("Inconsolata"),
                            base_font = font_google("Lato"),
                            primary = "#6200E9",
                            success = "#00BD9A",
                            info = "#4186E0", 
                            warning = "#FDB700",
                            danger = "#F9491B",
                            font_scale = 0.9, 
                            spacer = ".7rem"),

    # Set up shinyjs
    shinyjs::useShinyjs(),
    
    # set up shiny feedback
    # shinyFeedback::useShinyFeedback(),
    
    # tippy 
    use_tippy(),
    

    # Application title
    titlePanel("Generate a load curve"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
                  
        sidebarPanel(

# energy type -------------------------------------------------------------

        selectInput(
            "energyType"
            ,"Contract type"
            ,choices = c("Electricity", "Gas")
            ,selected = "Electricity"
        ),

# desired output ----------------------------------------------------------

        selectInput(
            "outputFormat"
            ,label = "Output format"
            ,choices = list(
                "JSON"
                ,"MSCONS"),
            selected = "JSON"
        ),

        tags$hr(),


# location (MaLo / MeLo) --------------------------------------------------

# input validation: https://rstudio.github.io/shinyvalidate/
        textInput(
            "marketLocation"
            ,"Location"
        ),


# market part (Strom: MSB, Gas: Netzbetreiber) --------------------------------------------------

        textInput(
            "marketPartnerNumber"
            ,"Market partner"
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

            tags$hr(),


# Period ------------------------------------------------------------------

        dateRangeInput(
            inputId = "period"
            ,"Period"
            ,start = today - 1L
            ,end = today
            ,separator = "to"
            ,format = "yyyy, M d"
            ),


# Interval ----------------------------------------------------------------

        selectInput(
            "interval",
            "Interval",
            choices =
                list(
                    "15 minutes" = "15 mins",
                    "60 minutes" = "hour",
                    "1 day" = "day"),
            selected = "hour"),



# Action button (generate output) -----------------------------------------
    
        fluidRow(
            
            # todo: change class of columns from 'col sm-6' to 'col 6'
            column(
                width = 6,
                actionButton(
                    "generate"
                    ,"Generate"
                    ,class = "btn-primary"
                    )
                ),
            column(
                width = 6,
                align = "right",
                
                # UI outputs for the copy-to-clipboard buttons
                actionButton(
                    "copyToClipboard"
                    ,label = icon("clipboard")
                    ,class = "btn btn-outline-dark"
                    ) |>
                    # adds tooltip from tippy pac
                    with_tippy(
                        "Copy to clipboard"
                        ,placement = "right"
                        ,arrow = TRUE
                        ,animation = "scale-subtle"
                        ,duration = "[500,0]"
                        ),
                )
            )
),


        mainPanel(
          verbatimTextOutput("outputMessage") |>
            # make the text output editable
            htmltools::tagAppendAttributes(contenteditable="true")
          )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # check for energy type
    elecTrue <- reactive({ input$energyType == "Electricity" })
    
    # change the placeholder in the market location input
    # conditionally on the energy type
    observeEvent(input$energyType, {

        # # change the placeholder for:
        # market location
        # market partner
        # market partner
        # register"
        
        Map(f = placeholder
            ,inputId = list("marketLocation"
                            ,"marketPartnerNumber"
                            ,"receiver"
                            ,"register")
            ,condition = elecTrue()
            
            # to be shown for electricity contracts
            ,valueTRUE = list("Market location"
                              ,"Metering point operator"
                              ,"BDEW / ILN number"
                              ,"e.g. 1-1:1.29.0")
            
            # to be shown for gas contracts
            ,valueFALSE = list("Meter location"
                               ,"Grid operator"
                               ,"VDEW / ILN number"
                               ,"e.g. 7-1:3.1.0")
        )
        }
    )

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


# gather input values to be used to create output formats -----------------

    # start and end date of the dateRange picker input
    periodStart <- reactive(midnight(input$period[[1]]))
    periodEnd <- reactive(midnight(input$period[[2]]))
    
    # check the desired output
    desiredOutput <- reactive(input$outputFormat)
    
    observe({
        toggle("receiverInput", anim = TRUE, condition = desiredOutput() == "MSCONS")
    })
    

# generate data structure to be converted to JSON -------------------------
    
    
      energyType <- reactive({ ifelse(elecTrue(), 1L, 2L) })
      valueInterval <- reactive({ 
        switch(
          EXPR = input$interval,
          "15 mins" = 15L,
          "hour" = 60L,
          "day" = 1440L
        )
      })
      
      # based on the user specified interval, generate a time sequence
      # starting at periodStart to periodEnd
      timeSequenceJSON <- reactive({
        interval(
          from = periodStart()
          ,to = periodEnd()
          ,by = input$interval
          ,msconsFormat = FALSE 
        )
      })
      
      timeSeries <- reactive(
        valuesJSON( # -----> to be replaced with function values() when valuesMSCONS() is finished
          timestamp = timeSequenceJSON()
          ,quantity = quantityRandom(
            n = length(timeSequenceJSON())
            # ,missingValues = input$missingValues
          )
        )
      )
      
      outputMessage <- eventReactive(input$generate, ignoreInit = TRUE, {
        
        if(input$outputFormat == "JSON") {
          
          loadCurves <- loadCurves(
            marketPartnerNumber = input$marketPartnerNumber,
            register = input$register,
            energyType = energyType(),
            valueInterval = valueInterval(),
            locationNumber = input$marketLocation,
            locationType = energyType()
          )
          
          # put all the pieces together, convert to JSON
          loadCurveSets <- loadCurveSets(
            periodStart = periodStart()
            ,periodEnd = periodEnd()
          )
          
          channelInformation <- channelInformation(
            messageReference = messageReference()
            ,sender = input$marketPartnerNumber
          )
          
          loadCurveSets[[1]][["channelInformation"]] <- I(channelInformation)
          loadCurveSets[[1]][["values"]] <- list(timeSeries())
          loadCurves[["loadCurveSets"]] <- loadCurveSets
          
          return(toJSON(list("loadCurves" = loadCurves), pretty = TRUE))
        
          } else {
          msconsOutput(
            sender = input$marketPartnerNumber
            ,receiver = input$receiver
            ,marketLocation = input$marketLocation
            ,register = input$register
            ,timeSeries = interval(
              from = periodStart()
              ,to = periodEnd()
              ,by = input$interval
              ,msconsFormat = TRUE
              )
            ,quantity = pi
            )
          }
        })      

    output$outputMessage <- renderPrint(outputMessage())
    
}

# Run the application 
shinyApp(ui = ui, server = server)


