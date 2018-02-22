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
  shinyjs,
  shinyWidgets,DT,shinythemes,
  ggrepel,tidyverse,treemapify
)
options(stringsAsFactors = F)
source('~/Documents/Git Clones/WD/VOE/getTopics_Terms.R')

# Define UI for data upload app ----
ui <- fluidPage(# App title ----
                theme = shinytheme('flatly'),
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
                    # tabPanel('Raw Data View', DT::dataTableOutput("table")),
                    
                    # ## Plots
                    # tabPanel(
                    #   'Plot',
                    #   
                    #   fluidRow(column(
                    #     4,
                    #     radioButtons(
                    #       "analysisType",
                    #       "Analysis Type",
                    #       choices = c(Themes = "Topic",
                    #                   Term = "Terms"),
                    #       selected = "Topic"
                    #     )),
                    #     column(
                    #       4,
                    #       radioButtons(
                    #         "countType",
                    #         "Count Type",
                    #         choices = c(Prct = "Prct",
                    #                     Count = "Count"),
                    #         selected = "Prct"
                    #       )
                    #     )
                    #   ), 
                    #   
                    #   fluidRow(column(4,uiOutput('selectGroup'))),
                    #   fluidRow(column(10,showOutput("groupedPlotData", lib = 'nvd3'))),
                    #   #Input:invert 
                    #   fluidRow(column(2,checkboxInput('invert', 'Invert Graph', value = F)),
                    #           conditionalPanel('analysisType'=='Term',
                    #                             column(2,uiOutput('termsToShow')))),
                    #   fluidRow(uiOutput('sampleSize1'))
                    #   ),
                   
                    tabPanel(
                      'Key Words',
                      fluidRow(column(
                        4,
                        radioButtons(
                          "analysisType",
                          "Analysis Type",
                          choices = c(Themes = "Themes",
                                      Term = "Term"),
                          selected = "Themes"
                        )
                      ),
                      column(4, uiOutput('varType'))),
                      fluidRow( column(4, uiOutput('group1')), column(4,uiOutput('numKeywords')
                      )),
                    fluidRow(plotlyOutput('keywords'))
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
                  )
                )
                ))


###                         ####
###                         ####
###         SERVER FILE     ####
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
    
    if(is.null(input$group1)){
      if (input$analysisType == 'Themes') {
        temp <- getTopics(temp,text_field = input$textField,docid_field = input$docIdField)
      }
      else{
        temp <- getTerms(temp,
                         n = input$numKeywords,text_field = input$textField,docid_field = input$docIdField)
      }}
      else{
        if (input$analysisType == 'Themes') {
          temp <- getTopics(temp, groups = input$group1,text_field = input$textField,docid_field = input$docIdField)
            }
      else{
      temp <- getTerms(temp,
                       groups = input$group1,
                       n = input$numKeywords,text_field = input$textField,docid_field = input$docIdField)
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
  

# New key word -----------------------------------------------------------

  output$varType = renderUI({
      radioButtons(
        "varType",
        "Count Type",
        choices = c(Prct = "Prct", Count = "Count"),
        selected = "Prct"
      )
  })
  output$numKeywords = renderUI({
      sliderInput(
        "numKeywords",
        "Enter number of words to Show :",
        min = 1,
        max = 20,
        value = 5
      )
  })
    output$group1 = renderUI({
      colnames <- names(datasetInput())
      # Dropdown for selecting groups on word Clouds
      selectizeInput(
        "group1",
        "Choose groupings:",
        multiple = T,
        options = list(placeholder = 'Select fields',maxItems=3),
        choices  = colnames
      )
    })
    
    ## Data and Plot for keywords
    
    output$keywords = renderPlotly({
      if(!FileInput)
      if (input$analysisType == 'Term') {
        ## Term + No Group
        if (is.null(input$group1)) {
          temp = d1()[variable == input$varType, ]
          temp$Term = factor(temp$Term, levels = temp$Term[order(temp$value)])
          p = ggplot(temp, aes(Term, value)) + geom_point() + coord_flip() +
            theme_minimal() + xlab('') + ylab(ifelse(input$varType=='Prct','% of Comments','Number of Comments'))
          ggplotly(p)
        }
        
        else{
          ## Term + Group
          d0 = df_corpus()
          d0 = dfm(
            d0,
            remove = c(stopwords(), uselessWords),
            tolower = T,
            groups = input$group1,
            thesaurus = likeWords,
            verbose = T,
            remove_punct = T,
            remove_numbers = T
          )
          d0 = getKeyness(d0, numOut = input$numKeywords)
          d0$Term = factor(d0$Term, levels = d0$Term[order(d0$chi2)])
          p = ggplot(d0, aes(Term, chi2)) +
            geom_point(
              aes(colour = chi2),
              shape = 16,
              size = 3,
              show.legend = F
            ) +
            scale_color_gradient(low = "#0091ff", high = "#f0650e") +
            facet_wrap(~ groupNames,scales='free') + coord_flip() +
            ylab('Importance Level (Chi2)') + xlab('') +
            theme_minimal()
          ggplotly(p)
        }
      }
      else{
        ## Topics No Groups
        if (is.null(input$group1)) {
          d0 = d1()[variable == input$varType, ]
          d0$Topic = factor(d0$Topic, levels = d0$Topic[order(d0$value)])
          p = ggplot(d0, aes(Topic, value)) +
            geom_point(
              aes(colour = value),
              shape = 16,
              size = 3,
              show.legend = F
            ) +
            scale_color_gradient(low = "#0091ff", high = "#f0650e") +
            coord_flip() +
            ylab(ifelse(input$varType=='Prct','% of Comments','Number of Comments')) + xlab('') +
            theme_minimal()
          ggplotly(p)
        }
        else{
          ## Topics + Groups
          d0 = d1()[variable == input$varType, ]
          p = ggplot(d0, aes(Topic, value)) +
            geom_point(
              aes(colour = value),
              shape = 16,
              size = 3,
              show.legend = F
            ) +
            scale_color_gradient(low = "#0091ff", high = "#f0650e") +
            coord_flip() +facet_wrap(~ group)+
            ylab(ifelse(input$varType=='Prct','% of Comments','Number of Comments')) + xlab('') +
            theme_minimal()
          ggplotly(p)
        }
      }
      
    })

#  end new wordcloud ------------------------------------------------------


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
