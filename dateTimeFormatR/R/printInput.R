printInput <- function(){

  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()

  # Set the default data to use based on the selection.
  text <- context$selection[[1]]$text
  defaultData <- text


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

    observeEvent(input$done, {
      stopApp(dataString)
    })
  }

  runGadget(shinyApp(ui, server), viewer = dialogViewer("dateFixer", width = 400, height = 1000)) #browserViewer())# #





}

