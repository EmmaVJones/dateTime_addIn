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

as.POSIXct(dateVal, format="%m/%d")


dateTimeSwitcher <- function(dateVal, firstValue, secondValue, thirdValue, fourthValue, fifthValue, 
                             sixthValue, seventhValue, eigthValue, ninthValue, tenthValue){
                             #dateSeparator, firstSelection, secondSelection,thirdSelection, dateTimeseparator, fourthSelection, fifthSelection, timeSeparator){
  
  #if(is.na(firstValue)){return(NA)}
  #if(is.na(secondValue)){return(as.POSIXct(dateVal, format = paste(firstValue, sep='')) )}
  #if(is.na(thirdValue)){return(as.POSIXct(dateVal, format = paste(firstValue, secondValue, sep='')) ) }
  #if(is.na(fourthValue)){as.POSIXct(dateVal, format = paste(firstValue, secondValue,
  #                                                         thirdValue, sep=''))  }
#}
#dateTimeSwitcher(dateVal,'%m','/','%d')
  as.POSIXct(dateVal, format = paste(firstValue, secondValue, thirdValue, fourthValue, fifthValue, 
                                     sixthValue, seventhValue, eigthValue, ninthValue, tenthValue, 
                                     sep=''))
  #as.POSIXct(dateVal, format = paste(firstSelection, dateSeparator,
  #                                   secondSelection, dateSeparator,
  #                                   thirdSelection, dateTimeseparator, 
  #                                   fourthSelection, timeSeparator, fifthSelection, sep=''))
}
#dateTimeSwitcher(dateVal, '/','%m','%d','%Y', ' ','%H',"%M",":")
dateTimeSwitcher(dateVal,'%m','/','%d','/','%Y',NA,NA,NA,NA,NA )
paste('%m','/','%d','/','%Y',NA,NA,NA,NA,NA ,      sep='')







dateTimeSwitcher <- function(dateVal,concatinatedInputs){
  as.POSIXct(dateVal, format = paste(concatinatedInputs, sep=''))
}


dateTimeSwitcher(dateVal, paste('%m','/','%d','/','%Y',' ','%H',":","%M"))


# UI
dateComponentChoices <- data.frame(Code= c('M','m','B','b','D','d','Y','y','H','h','M','m','S','s'))


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
                     textOutput('originalFormat'),
                     hr(),
                     #selectInput('sepDate', 'Date separation', choices = c('-','/','_',':',' ')),
                     selectInput('select1','First Date Component', choices = dateComponentChoicesTable[1], selected = NULL),
                     selectInput('select2','Second Date Component', choices = dateComponentChoicesTable[1]),
                     selectInput('select3','Third Date Component', choices = dateComponentChoicesTable[1]),
                     selectInput('select4','Fourth Date Component', choices = dateComponentChoicesTable[1]),
                     selectInput('select5','Fifth Date Component', choices = dateComponentChoicesTable[1]),
                     selectInput('select6','Sixth Date Component', choices = dateComponentChoicesTable[1]),
                     selectInput('select7','Seventh Date Component', choices = dateComponentChoicesTable[1]),
                     textOutput('results')
    )),
    miniTabPanel('Table', icon = icon("table"),
                 miniContentPanel(tableOutput('exampleTable')))
  )
)

server <- function(input, output, session) {
 
  output$originalFormat <- renderPrint({
    dateVal
  })
  
  output$exampleTable <- renderTable({dateComponentChoicesTable})
  
  dateFormat <- reactive({
    dateTimeSwitcher(input$select1, input$select2, input$select3,
                     input$select4, input$select5, input$select6)
    #req(input$sepDate, input$select1, input$select2, input$select3)
    #if(is.na(dateTimeSwitcher(dateVal, input$sepDate, input$select1, input$select2, input$select3))){
    #  print('Incorrect Formatting')
    #} else {
    #  dateTimeSwitcher(dateVal, input$sepDate, input$select1, input$select2, input$select3)
    #}
  })
  
  output$results <- renderPrint({
    req(dateFormat())
    dateFormat()
      })
  
  observeEvent(input$done, {
    stopApp(dateFormat())
  })
}


runGadget(shinyApp(ui, server), viewer = dialogViewer("dateFixer"))
#dateTimeSwitcher(dateVal,'/', 'mon', 'd', 'Y')

