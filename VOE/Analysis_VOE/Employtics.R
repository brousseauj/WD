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
  rCharts,
  ggthemes,
  shinyWidgets,DT
)
options(stringsAsFactors = F)
source('~/Documents/Git Clones/WD/VOE/getTopics_Terms.R')

# Define UI for data upload app ----
ui <- fluidPage(# App title ----
                titlePanel("Employtics"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    # Input: Select a file ----
                    fileInput("FileInput", "Choose file"),
                    
                    # Input: Horizontal Line ----
                    tags$hr(),
                    
                  
                    uiOutput('textField'),
                    uiOutput('docIdField')
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(tabsetPanel(
                    
                    ## Table View
                    tabPanel('Raw Data View', DT::dataTableOutput("table")),
                    
                    ## Plots
                    tabPanel(
                      'Plot',
                      
                      fluidRow(column(
                        4,
                        radioButtons(
                          "analysisType",
                          "Analysis Type",
                          choices = c(Topic = "Topic",
                                      Term = "Terms"),
                          selected = "Topic"
                        )),
                        column(
                          4,
                          radioButtons(
                            "countType",
                            "Count Type",
                            choices = c(Prct = "Prct",
                                        Count = "Count"),
                            selected = "Prct"
                          )
                        )
                      ), 
                      
                      fluidRow(column(4,uiOutput('selectGroup'))),
                      fluidRow(column(10,showOutput("groupedPlotData", lib = 'nvd3'))),
                      #Input:invert 
                      fluidRow(column(2,checkboxInput('invert', 'Invert Graph', value = F)),
                              conditionalPanel('analysisType'=='Term',
                                                column(2,uiOutput('termsToShow')))),
                      fluidRow(uiOutput('sampleSize1'))
                      ),
                      #Input: Number of words to show
                      
                    
                    #W Wordclouds
                    tabPanel(
                      'Word Clouds',
                      fluidRow(uiOutput('selectGroup2')),
                      fluidRow(plotOutput(
                        'wordcloud', width = "100%", height = '800'
                      )),
                      fluidRow(uiOutput('targetKeyness')),
                      fluidRow(plotOutput(
                        'keyness', width = '100%', height = '600'
                      ))
                    ),
                    
                    ## KWIC
                    tabPanel(
                      'Keyword in Context',
                      # fluidRow(uiOutput('SelectGroup3')),
                      fluidRow(column(4,textInput("keyword", "Enter keyword :")),
                      fluidRow(column(4, sliderInput("context", "Enter number of words for context :",
                                                     min = 1, max = 10,
                                                     value = 5))),
                      fluidRow(column(4,uiOutput('selectGroup4'))),
                      fluidRow(column(4,uiOutput('subsetSelect'))),
                      fluidRow(column(10,DT::dataTableOutput("kwicTable"))))
                    )
                  ))
                ))


###                         ####
###                         ####
###         SERVER FILER    ####
###                         ####
###                         ####

# Define server logic to read selected file and Visualize----
server = function(input, output) {
  options(shiny.maxRequestSize = 60 * 1024 ^ 2)
  
  # Raw Data Input
  datasetInput <- reactive({
    infile <- input$FileInput
    if (is.null(infile))
      return(NULL)
    read.csv(infile$datapath, header = TRUE)
  })

  
  d1 = reactive({
    temp = as.data.table(datasetInput())
    
    if(is.null(input$selectGroup)){
      if (input$analysisType == 'Topic') {
        temp <- getTopics(temp,text_field = input$textField,docid_field = input$docIdField)
      }
      else{
        temp <- getTerms(temp,
                         n = input$termsToShow,text_field = input$textField,docid_field = input$docIdField)
      }}
      else{
        if (input$analysisType == 'Topic') {
          temp <- getTopics(temp, groups = input$selectGroup,text_field = input$textField,docid_field = input$docIdField)
            }
      else{
      temp <- getTerms(temp,
                       groups = input$selectGroup,
                       n = input$termsToShow,text_field = input$textField,docid_field = input$docIdField)
    }}
  })
  
  
  
  #DF Corpus
  df_corpus = reactive({
    dTemp = datasetInput()
    df_corpus = corpus(
      as.data.frame(dTemp),
      text_field = input$textField,
      docid_field = input$docIdField
    )
  })
  
  # Raw data view
  
  output$table = DT::renderDataTable(datasetInput(), options = list(scrollX = TRUE))
  
  # Text Field
  output$textField = renderUI({
    colnames <- names(datasetInput())
    # Dropdown for selecting groups
    selectizeInput(
      "textField",
      "Which column contains the text you want to analyze?",
      multiple = T,
      options = list(placeholder = 'Select one column',maxItems=1),
      choices  = colnames
    )
  })
  
  # DocID Field
  output$docIdField = renderUI({
    colnames <- names(datasetInput())
    # Dropdown for selecting groups
    selectizeInput(
      "docIdField",
      "Which column contains the document ID field? (usually the Employee ID field)",
      multiple = T,
      options = list(placeholder = 'Select one column',maxItems=1),
      choices  = colnames
    )
  })
  

# Bar Plots ---------------------------------------------------------------

  # #sample Size Calculation
  # d1 = 
  # 
  # output$sampleSize1 = renderUI({
  #   
  # })
  
  #Select groupings for bar plots
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
  
  #Select groupings for KWIC
  output$selectGroup4 = renderUI({
    colnames <- names(datasetInput())
    # Dropdown for selecting groups
    selectizeInput(
      "selectGroup4",
      "Choose groupings:",
      options = list(placeholder = 'Select your group:',maxItems=1),
      choices  = colnames
    )
  })
  
  ## Select fields for KWIC
  output$subsetSelect = renderUI({

    dTemp = datasetInput()
    nameFields = unique(dTemp[[input$selectGroup4]])
    # Dropdown for selecting groups
    selectizeInput(
      "subsetSelect",
      "Choose your field:",
      options = list(placeholder = 'Select your group:',maxItems=1),
      choices  = nameFields
    )
  })
  
  #Select number of terms to show for Top Terms
  output$termsToShow =renderUI({
  
    fluidRow(column(8, sliderInput("termsToShow", "Enter number of words to show :",
                                   min = 1, max = 20,
                                   value = 10)))
  })
  
  #Select target for Keyness
  output$targetKeyness = renderUI({
    dfCorp = dCorp()
    targetNames = docnames(dfCorp)
    selectizeInput('targetKeyness',
                   'Choose Target:',
                   multiple = T,
                   choices = targetNames)
  })
  
  #Select Groupings for word clouds
  output$selectGroup2 = renderUI({
    colnames <- names(datasetInput())
    # Dropdown for selecting groups on word Clouds
    selectizeInput(
      "selectGroup2",
      "Choose groupings:",
      multiple = T,
      options = list(placeholder = 'Select a max of 1 different fields'),
      choices  = colnames
    )
  })
  
  #Select Groupings for KWIC
  output$selectGroup3 = renderUI({
    colnames <- names(datasetInput())
    # Dropdown for selecting groups on word Clouds
    selectizeInput(
      "selectGroup3",
      "Choose groupings:",
      multiple = T,
      options = list(placeholder = '-------'),
      choices  = colnames
    )
  })
  
  output$groupedPlotData = renderChart2({
    
    if(is.null(input$selectGroup)){
      dp = d1()[variable == input$countType, ]
      varXY = (names(dp)[1])
      p = nPlot(
        as.formula(paste('value ~', varXY)),
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
    dp = d1()[variable == input$countType, ]
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
    }}
  })
  
  dCorp = reactive({
    x_corpus = corpus(
      as.data.frame(datasetInput()),
      text_field = input$textField,
      docid_field = input$docIdField
    )
    
    if (is.null(input$selectGroup2)) {
      xdfm = dfm(
        x_corpus,
        remove = c(stopwords('english'), uselessWords),
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
        remove = c(stopwords('english'), uselessWords),
        tolower = T,
        thesaurus = likeWords,
        verbose = T,
        remove_punct = T,
        remove_numbers = T,
        groups = input$selectGroup2
      )
    }
  })
  ## Output: Plot WordCloud
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
                       max.words = 20,
                       title.size = 1
                     )
                     
                   }
                   else{
                     textplot_wordcloud(d1,
                                        max.words = 20)
                   }
                 })
  })
  ### Output: Keyness
  output$keyness = renderPlot({
    if (!is.null(input$selectGroup2)) {
      d1 = dCorp()
      d1 = dfm_trim(d1, min_count = 2)
      withProgress(message = 'Building Wordclouds',
                   detail = 'This may take a while...',
                   value = 0,
                   {
                     for (i in 1:15) {
                       incProgress(1 / 50)
                       Sys.sleep(0.25)
                     }
                     
                     dTemp = textstat_keyness(
                       d1,
                       measure = 'chi2',
                       sort = T,
                       target = input$targetKeyness
                     )
                     p = textplot_keyness(dTemp, show_reference = T, n = 10)
                     p = p + scale_fill_manual(values = c("#00AB8E", "#EE2737")) + theme_hc() +
                       theme(
                         axis.title.y = element_blank(),
                         axis.text.y =
                           element_blank(),
                         axis.ticks.y =
                           element_blank()
                       )
                     p
                     
                   })
    }
  })
  
  ## Output: KWIC

  # subset data for corpus
  
    df_corpus1 <- reactive({
      df = as.data.table(datasetInput())
      if(!is.null(input$selectGroup4) & !is.null(input$subsetSelect))
      {
        df = df[df[[input$selectGroup4]] %in% input$subsetSelect,]
      }
      df
    })
  
  output$kwicTable=renderDataTable({
    if(!is.null(input$selectGroup4) & !is.null(input$subsetSelect)){
    dtemp = df_corpus1()
    dtemp = corpus(as.data.frame(dtemp),text_field=input$textField,docid_field=input$docIdField)
    x = kwic(x = dtemp,pattern=input$keyword,window=input$context)
    x = as.data.table(x)
    x[,4:6]}
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
