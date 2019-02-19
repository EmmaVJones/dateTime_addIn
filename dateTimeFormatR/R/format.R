


dateTimeSwitcherFromSelection <- function(){

  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()

  # Set the default data to use based on the selection.
  text <- context$selection[[1]]$text
  defaultData <- text


  # custom function
  dateTimeSwitcher <- function(dateVal, concatinatedInputs){
    as.POSIXct(dateVal, format = paste(concatinatedInputs, sep=''))
  }

  dateComponentChoicesTable <- data.frame(Code = c('%a','%A','%b','%B','%c','%d','%H', '%I','%j','%m','%M','%p','%S',
                                                   '%U','%w','%W','%x','%X','%y','%Y','%z','%Z',' ','-','/',':'),
                                          Meaning = c('Abbreviated weekday','Full weekday','Abbreviated month','Full month',
                                                      'Locale-specific date and time','Decimal date', 'Decimal hours (24 hour)',
                                                      'Decimal hours (12 hour)','Decimal day of the year','Decimal month',
                                                      'Decimal minute','Locale-specific AM/PM','Decimal second',
                                                      'Decimal week of the year (starting on Sunday)','Decimal Weekday (0=Sunday)',
                                                      'Decimal week of the year (starting on Monday)','Locale-specific Date',
                                                      'Locale-specific Time','2-digit year','4-digit year','Offset from GMT',
                                                      'Time zone (character)',
                                                      'Space Separator','Dash Separator','Slash Separator','Colon'))


  ui <- miniPage(
    gadgetTitleBar('Date Time Switcher'),
    miniTabstripPanel(
      miniTabPanel("Data", icon = icon("mouse-pointer"),#sliders
                   miniContentPanel(
                     h5('Original Date:'),
                     textOutput('originalFormat'),
                     h5("Updated POSIXct Date:"),
                     textOutput('results'),
                     hr(),
                     fillRow(
                       actionButton("add", "Add Date Component")),
                     verbatimTextOutput('test'),
                     padding = 0
                   )),
      miniTabPanel('Table', icon = icon("table"),
                   miniContentPanel(tableOutput('exampleTable')))
    )
  )


  server <- function(input, output, session) {

    # Collect inputs.
    dataString <- input$data



    output$originalFormat <- renderPrint({
      dataString
    })

    output$exampleTable <- renderTable({dateComponentChoicesTable})

    observeEvent(input$add, {
      insertUI(
        selector = "#add",
        where = "beforeBegin",
        ui = selectInput(paste0("txt", input$add),
                         "Choose your value", choices = dateComponentChoicesTable[1], selected = NULL)
      )
    })



    output$test <- renderPrint({
      input$add
    })

    dateFormat <- reactive({
      trimws(paste(input$txt1, input$txt2, input$txt3, input$txt4, input$txt5,
                   input$txt6, input$txt7, input$txt8, input$txt9, input$text10,
                   sep=''), 'r')
      dateTimeSwitcher(dataString, trimws(paste(input$txt1, input$txt2, input$txt3, input$txt4, input$txt5,
                                             input$txt6, input$txt7, input$txt8, input$txt9, input$text10,
                                             sep=''), 'r'))
    })

    output$results <- renderPrint({
      req(dateFormat())
      dateFormat()
    })

    observeEvent(input$done, {
      stopApp(dateFormat())
    })
  }


  runGadget(shinyApp(ui, server), viewer = dialogViewer("dateFixer", width = 400, height = 1000)) #browserViewer())# #





}


