# R 3.5.1

library(tidyverse)
library(miniUI)
library(shiny)

# Bring in conventionals and only keep tiny subset for testing
#test <- read_csv('data/CONVENTIONALS_20171010.csv')
#test <- test[1:25,]
#write.csv(test, 'data/conventionalsTest.csv', row.names = F)

# Bring in test data
test <- read_csv('data/conventionalsTest.csv')

# first build function that will do the work
dateVal <- test$FDT_DATE_TIME[1]
class(dateVal)

separator <- '/'
monthSelection <- 'm'
daySelection <- 'd'
yearSelection <- 'Y'
hourSelection <- 'H'
minuteSelection <- 'M'
secondSelection <- NA


as.POSIXct(dateVal, format = paste('%', monthSelection, separator,
                                   '%', daySelection, separator,
                                   '%', yearSelection, sep=''))
as.POSIXct(dateVal, format = paste('%', monthSelection, separator,
                                   '%', daySelection, separator,
                                   '%', yearSelection, " ",
                                   '%', hourSelection, ":",
                                   '%', minuteSelection, sep=''))



as.POSIXct(dateVal, format="%m/%d/%Y %H:%M")




dateTimeSwitcher <- function(dateVal){
  as.POSIXct(dateVal, format = paste('%',firstSelection, separator,
                                     '%', secondSelection, separator,
                                     '%', thirdSelection, sep=''))
}


# UI
dateComponentChoices <- c('M','m','B','b','D','d','Y','y','H','h','M','m','S','s')

ui <- miniPage(
  gadgetTitleBar('Date Time Switcher'),
  miniContentPanel(
    selectInput('select1','First Date Component', choices = dateComponentChoices),
    selectInput('select2','Second Date Component', choices = dateComponentChoices),
    selectInput('select3','Third Date Component', choices = dateComponentChoices),
    #selectInput('select4','First Date Component', choices = dateComponentChoices),
    #selectInput('select4','First Date Component', choices = dateComponentChoices),
    #selectInput('select5','First Date Component', choices = dateComponentChoices),
    #selectInput('select6','First Date Component', choices = dateComponentChoices),
    textOutput('results')
  )
)

server <- function(input, output, session) {
 
  output$results <- renderPrint({
    req(input$select1, input$select2, input$select3)
    dateTimeSwitcher(dateVal, input$select1, input$select2, input$select3)
  })
}

runGadget(shinyApp(ui, server), viewer = dialogViewer("dateFixer"))
    