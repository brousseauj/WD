library(needs)
needs(
  stringr,
  data.table,
  tm,
  ggplot2,
  wordcloud,
  readxl,
  dplyr,
  quanteda,
  topicmodels,
  reshape,
  plotly,
  shiny,
  rCharts
)
options(stringsAsFactors = F)
source('~/Documents/Git Clones/WD/VOE/getTopics_Terms.R')

# Define UI for data upload app ----
ui <- fluidPage(# App title ----
                titlePanel("CommentR"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    # Input: Select a file ----
                    fileInput("FileInput", "Choose file"),
                    
                    # Input: Horizontal Line ----
                    tags$hr(),
                    
                    #Input: Select Terms or Topics
                    radioButtons(
                      "analysisType",
                      "Analysis Type",
                      choices = c(Topic = "Topic",
                                  Term = "Terms"),
                      selected = "Topic"
                    )
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(tabsetPanel(
                    tabPanel('Raw Data View', DT::dataTableOutput("table")),
                    tabPanel(
                      'Plot',
                      showOutput("groupedPlotData", lib = 'nvd3'),
                      #Input: Invert Graph
                      checkboxInput('invert', 'Invert Graph', value = F),
                      # Input: Select Prct or Count
                      radioButtons(
                        "countType",
                        "Count Type",
                        choices = c(Prct = "Prct",
                                    Count = "Count"),
                        selected = "Prct"
                      ),
                      
                      uiOutput('selectGroup')
                    ),
                    tabPanel(
                      'Word Clouds',
                      fluidRow(uiOutput('selectGroup2')),
                      fluidRow(plotOutput('wordcloud',width = "100%",height ='800')),
                      fluidRow(plotOutput('keyness',width='100%',height='600'))
                    )
                  ))
                ))


# Define server logic to read selected file and Visualize----
server = function(input, output) {
  options(shiny.maxRequestSize = 60 * 1024 ^ 2)
  datasetInput <- reactive({
    infile <- input$FileInput
    if (is.null(infile))
      return(NULL)
    read.csv(infile$datapath, header = TRUE)
  })
  
  
  d1 = reactive({
    if (input$analysisType == 'Topic') {
      temp <- getTopics(datasetInput(), groups = input$selectGroup)
    }
    else{
      temp <- getTerms(datasetInput(),
                       groups = input$selectGroup,
                       n = 15)
    }
  })
  
  # Table View --------------------------------------------------------------
  
  output$table = DT::renderDataTable(datasetInput(), options = list(scrollX = TRUE))
  
  # Data manipulatation ----------------------------------------------------
  
  output$selectGroup = renderUI({
    colnames <- names(datasetInput())
    # Dropdown for selecting groups
    selectizeInput(
      "selectGroup",
      "Choose groupings:",
      multiple = T,
      options = list(placeholder = 'Select a max of 3 different fields'),
      choices  = colnames
    )
  })
  
  output$selectGroup2 = renderUI({
    colnames <- names(datasetInput())
    # Dropdown for selecting groups on word Clouds
    selectizeInput(
      "selectGroup2",
      "Choose groupings:",
      multiple = T,
      options = list(
        placeholder = 'Select a max of 1 different fields'
      ),
      choices  = colnames
    )
  })
  
  output$groupedPlotData = renderChart2({
    dp = d1()[variable == input$countType,]
    if (input$invert == F) {
      varXY = (names(dp)[1])
      p = nPlot(
        as.formula(paste('value ~', varXY)),
        group = 'group',
        data = dp,
        type = 'multiBarHorizontalChart'
      )
      
      p$set(title = paste(input$analysisType))
      p$chart(margin = list(left = 100))
      p$xAxis(rotateLabels = -45)
      p$yAxis(axisLabel = ifelse(
        input$countType == 'Prct',
        sprintf('%s', '% of Comments'),
        sprintf('%s', 'Total Count')
      ))
      return(p)
    }
    else{
      varXY = (names(dp)[1])
      p = nPlot(
        value ~ group,
        group = paste(varXY),
        data = dp,
        type = 'multiBarHorizontalChart'
      )
      p$set(title =  input$analysisType)
      p$chart(margin = list(left = 100))
      p$xAxis(rotateLabels = -45)
      p$yAxis(axisLabel = ifelse(
        input$countType == 'Prct',
        sprintf('%s', '% of Comments'),
        sprintf('%s', 'Total Count')
      ))
      return(p)
    }
  })
  
  dCorp = reactive({
    
    x_corpus = corpus(as.data.frame(datasetInput()),
                      text_field = length(x),
                      docid_field = 1)
    
    if (is.null(input$selectGroup2)) {
      
      xdfm = dfm(
        x_corpus,
        remove = c(stopwords(),uselessWords),
        tolower = T,
        thesaurus = likeWords,
        verbose = T,
        remove_punct = T,
        remove_numbers = T
      )
    }
    else{
      xdfm = dfm(
        x_corpus,
        remove = c(stopwords(),uselessWords),
        tolower = T,
        thesaurus = likeWords,
        verbose = T,
        remove_punct = T,
        remove_numbers = T,
        groups = input$selectGroup2
      )
    }
  })
  #### WordCloud
  output$wordcloud = renderPlot({

   d1 = dCorp()
    
    withProgress(message = 'Building Wordclouds',
                 detail = 'This may take a while...',
                 value = 0,
                 {
                   for (i in 1:15) {
                     incProgress(1 / 50)
                     Sys.sleep(0.25)
                   }
                   if (!is.null(input$selectGroup2)) {
                     textplot_wordcloud(
                       d1,
                       comparison = T,
                       max.words = 50,title.size=2)
                     
                   }
                   else{
                     textplot_wordcloud(d1,
                                        max.words = 50)
                   }
                 })
  })
  ### Output: Keyness
  output$keyness = renderPlot({
    d1 = dCorp()
    withProgress(message = 'Building Wordclouds',
                 detail = 'This may take a while...',
                 value = 0,
                 {
                   for (i in 1:15) {
                     incProgress(1 / 50)
                     Sys.sleep(0.25)
                   }
    if(!is.null(input$selectGroup2)){
      dTemp = textstat_keyness(d1,measure='chi2',sort=T)
      textplot_keyness(dTemp,show_reference=T,n=15)
    }
  })
  })
}

# Create Shiny app ----
shinyApp(ui, server)
