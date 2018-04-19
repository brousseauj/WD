#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Employtics"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
     fluidRow(uiOutput('dataSelect')),
     fluidRow(helptext('Select Primary filter:')),
     fluidRow(uiOutput('PempSelector1')),fluidRow( uiOutput('PratingsSelector2')),
     fluidRow(actionButton('apply','Apply'))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
     textOutput('result')
      # fluidRow(column(5,uiOutput('selector1')), column(5,uiOutput('selector2'))),
      #fluidRow(plotlyOutput('plot'))
      )
    )
  )
)

