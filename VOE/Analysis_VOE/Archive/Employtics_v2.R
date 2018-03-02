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
  shinyWidgets,
  DT,
  shinythemes,
  ggrepel,
  tidyverse,
  treemapify
)
options(stringsAsFactors = F)
source('~/Documents/Git Clones/WD/VOE/getTopics_Terms.R')

# Define UI for data upload app ----
ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # App title ----
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
      tabPanel(
        'Understand Why',
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
        fluidRow(column(4, uiOutput('group1')), column(4, uiOutput('numKeywords'))),
        fluidRow(plotlyOutput('keywords')),
        fluidRow(uiOutput('context')),
        fluidRow(DT::dataTableOutput("table1"))
      )
<<<<<<< HEAD


=======
      
      
      
      # ## KWIC
      # tabPanel(
      #   'Keyword in Context',
      #   # fluidRow(uiOutput('SelectGroup3')),
      #   fluidRow(column(4,textInput("keyword", "Enter keyword :")),
      #   fluidRow(column(4, sliderInput("context", "Enter number of words for context :",
      #                                  min = 1, max = 10,
      #                                  value = 5))),
      #   fluidRow(column(4,uiOutput('selectGroup4'))),
      #   fluidRow(column(4,uiOutput('subsetSelect'))),
      #   fluidRow(column(10,DT::dataTableOutput("kwicTable"))))
      # )
>>>>>>> c6f5169df786f4896d3fdfaf7d327f3cc74c853f
    ))
  )
)


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
    read.csv(infile$datapath,
             header = TRUE,
             check.names = F)
    
  })
  
  # Text Field
  output$textField = renderUI({
    if (!is.null(datasetInput())) {
      temp = datasetInput()
      
      selectizeInput(
        "textField",
        "Which column contains the text you want to analyze?",
        multiple = T,
        options = list(placeholder = 'Select one column', maxItems = 1),
        choices  = colnames(temp)
      )
    }
  })
  
  # DocID Field
  output$docIdField = renderUI({
    if (!is.null(datasetInput())) {
      temp = datasetInput()
      # Dropdown for selecting groups
      selectizeInput(
        "docIdField",
        "Which column contains the document ID field? (usually the Employee ID field)",
        multiple = T,
        options = list(placeholder = 'Select one column', maxItems = 1),
        choices  = colnames(temp)
      )
    }
  })
  
  
  
  d1 = reactive({
    temp = as.data.table(datasetInput())
    
    if (is.null(input$group1)) {
      if (input$analysisType == 'Themes') {
        temp <-
          getTopics(temp,
                    text_field = input$textField,
                    docid_field = input$docIdField)
      }
      else{
        temp <- getTerms(
          temp,
          n = input$numKeywords,
          text_field = input$textField,
          docid_field = input$docIdField
        )
      }
    }
    else{
      if (input$analysisType == 'Themes') {
        temp <-
          getTopics(
            temp,
            groups = input$group1,
            text_field = input$textField,
            docid_field = input$docIdField
          )
      }
      else{
        temp <- getTerms(
          temp,
          groups = input$group1,
          n = input$numKeywords,
          text_field = input$textField,
          docid_field = input$docIdField
        )
      }
    }
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
      options = list(placeholder = 'Select fields', maxItems = 3),
      choices  = colnames
    )
  })
  
  ## Data and Plot for keywords
  
  d0 = reactive({
    if (!is.null(input$FileInput) &&
        !is.null(input$textField) && !is.null(input$docIdField)) {
      if (input$analysisType == 'Term') {
        ## Term + No Group
        if (is.null(input$group1)) {
          d0 = d1()[variable == input$varType, ]
          d0$Term = factor(d0$Term, levels = d0$Term[order(d0$value)])
          key <- row.names(d0)
          d0 <- data.frame(d0, key)
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
          key <- seq(1:nrow(d0))
          d0 <- data.frame(d0, key)
        }
      }
      else{
        ## Topics No Groups
        if (is.null(input$group1)) {
          d0 = d1()[variable == input$varType, ]
          d0$Topic = factor(d0$Topic, levels = d0$Topic[order(d0$value)])
          key <- row.names(d0)
          d0 <- data.frame(d0, key)
        }
        else{
          ## Topics + Groups
          d0 = d1()[variable == input$varType, ]
          
          d0 = d0[order(d0$Topic, value),]
          key <- row.names(d0)
          d0 <- data.frame(d0, key)
        }
      }
    }
  })

  
  output$keywords = renderPlotly({
    d0 = d0()
    if (!is.null(input$FileInput) &&
        !is.null(input$textField) && !is.null(input$docIdField)) {
      if (input$analysisType == 'Term') {
        ## Term + No Group
        if (is.null(input$group1)) {
          p = ggplot(d0, aes(Term, value, key = key)) +
            geom_point(
              aes(),
              shape = 16,
              size = 3,
              show.legend = F
            ) +
            coord_flip() +
            theme_minimal() +
            xlab('') + ylab(ifelse(
              input$varType == 'Prct',
              '% of Comments',
              'Number of Comments'
            ))
          ggplotly(p)
        }
        
        else{
          ## Term + Group
          p = ggplot(d0, aes(Term, chi2, key = key, key2 = group)) +
            geom_point(
              aes(colour = chi2),
              shape = 16,
              size = 3,
              show.legend = F
            ) +
            scale_color_gradient(low = "#0091ff", high = "#f0650e") +
<<<<<<< HEAD
            facet_wrap( ~ group, scales = 'free') + coord_flip() +ylim(c(0,max((d0$chi2*.1)+d0$chi2)))+
=======
            facet_wrap( ~ group, scales = 'free') + coord_flip() +
>>>>>>> c6f5169df786f4896d3fdfaf7d327f3cc74c853f
            ylab('Importance Level (Chi2)') + xlab('') +
            theme_minimal()
          ggplotly(p)
        }
      }
      else{
        ## Topics No Groups
        if (is.null(input$group1)) {
          p = ggplot(d0, aes(Topic, value, key = key)) +
            geom_point(
              aes(colour = value),
              shape = 16,
              size = 3,
              show.legend = F
            ) +
            scale_color_gradient(low = "#0091ff", high = "#f0650e") +
<<<<<<< HEAD
            coord_flip() +ylim(c(0,max((d0$value*.1)+d0$value)))+
=======
            coord_flip() +
>>>>>>> c6f5169df786f4896d3fdfaf7d327f3cc74c853f
            ylab(ifelse(
              input$varType == 'Prct',
              '% of Comments',
              'Number of Comments'
            )) + xlab('') +
            theme_minimal()
          ggplotly(p)
        }
        else{
          p = ggplot(d0, aes(Topic, value, key = key)) +
            geom_point(
              aes(colour = value),
              shape = 16,
              size = 3,
              show.legend = F
            ) +
            scale_color_gradient(low = "#0091ff", high = "#f0650e") +
<<<<<<< HEAD
            coord_flip() + facet_wrap( ~ group,scales='free') +ylim(c(0,max((d0$value*.1)+d0$value)))+
=======
            coord_flip() + facet_wrap( ~ group) +
>>>>>>> c6f5169df786f4896d3fdfaf7d327f3cc74c853f
            ylab(ifelse(
              input$varType == 'Prct',
              '% of Comments',
              'Number of Comments'
            )) + xlab('') +
            theme_minimal()
          ggplotly(p) %>% layout(dragmode = 'select')
        }
      }
    }
  })
  
  output$selection <- renderPrint({
    s <- event_data("plotly_click")
    data.frame(s)
    s
  })
  
  selection2 <- reactive({
    s <- event_data("plotly_click")
    df <- data.frame(s)
    df
  })
  
  
  output$context = renderUI({
    sliderInput(
      "context",
      "Enter number of words for context :",
      min = 1,
      max = 50,
      value = 25
    )
  })
  
  output$table1 = renderDT({
    selection2 = selection2()
    d0 = d0()
    d2 <- d0 %>% filter(key == selection2$key)
    
    d2 = as.data.table(d2, stringsAsFactors = F)
    if (!is.null(input$FileInput) &&
        !is.null(input$textField) &&
        !is.null(input$docIdField)) {
      if (input$analysisType == 'Term') {
        ## Term + No Group
        if (is.null(input$group1)) {
          dtemp = df_corpus()
          y = kwic(
            x = dtemp,
            pattern = d2[[1]],
            window = input$context,
            valuetype = 'glob',
            case_insensitive = T
          )
          y = as.data.table(y)
          y = y[, 4:6]
          y$new = do.call('paste', y)
          y = y[, !c(1:3)]
<<<<<<< HEAD
=======
          datatable(y,options = list(searchHighlight=T,search = list(search=d2[[1]])))
>>>>>>> c6f5169df786f4896d3fdfaf7d327f3cc74c853f
        }
        else{
          ## Term + Group
          dtemp = df_corpus()
          if (length(input$group1) == 1) {
            dtemp = corpus_subset(x = dtemp,
                                  subset = get(input$group1) == d2$group)
            y = kwic(
              x = dtemp,
              pattern = d2$Term,
              window = input$context,
              valuetype = 'glob',
              case_insensitive = T
            )
            y = as.data.table(y)
            y = y[, 4:6]
            y$new = do.call('paste', y)
            y = y[, !c(1:3)]
<<<<<<< HEAD
       
=======
            datatable(y,options = list(searchHighlight=T,search = list(search=d2[[1]])))
>>>>>>> c6f5169df786f4896d3fdfaf7d327f3cc74c853f
            
          }
          else if (length(input$group1) == 2) {
            groupNames = (d0$group[key == selection2()$key])
            groupNames = strsplit(groupNames, "[.]")
            
            dtemp = corpus_subset(
              x = dtemp,
              subset = get(input$group1[[1]]) ==
                groupNames[[1]][1] &
                get(input$group1[[2]]) == groupNames[[1]][2]
            )
            
            y = kwic(
              x = dtemp,
              pattern = d2$Term,
              window = input$context,
              valuetype = 'glob',
              case_insensitive = T
            )
            y = as.data.table(y)
            y = y[, 4:6]
            y$new = do.call('paste', y)
            y = y[, !c(1:3)]
<<<<<<< HEAD

=======
            datatable(y,options = list(searchHighlight=T,search = list(search=d2[[1]])))
            
>>>>>>> c6f5169df786f4896d3fdfaf7d327f3cc74c853f
          }
          else if (length(input$group1) == 3) {
            groupNames = (d0$group[key == selection2()$key])
            groupNames = strsplit(groupNames, "[.]")
            
            dtemp = corpus_subset(
              x = dtemp,
              subset = get(input$group1[[1]]) ==
                groupNames[[1]][1] &
                get(input$group1[[2]]) == groupNames[[1]][2] &
                get(input$group1[[3]]) == groupNames[[1]][3]
            )
            
            y = kwic(
              x = dtemp,
              pattern = d2$Term,
              window = input$context,
              valuetype = 'glob',
              case_insensitive = T
            )
            y = as.data.table(y)
            y = y[, 4:6]
            y$new = do.call('paste', y)
            y = y[, !c(1:3)]
<<<<<<< HEAD
=======
            datatable(y,options = list(searchHighlight=T,search = list(search=d2[[1]])))
>>>>>>> c6f5169df786f4896d3fdfaf7d327f3cc74c853f
          }
          
        }
      }
      else{
        ## Topics No Groups
        if (is.null(input$group1)) {
          dtemp = df_corpus()
          y = kwic(
            x = dtemp,
            pattern = d2$Topic,
            window = input$context,
            valuetype = 'glob',
            case_insensitive = T
          )
          y = as.data.table(y)
          y = y[, 4:6]
          y$new = do.call('paste', y)
          y = y[, !c(1:3)]
          print(d2[[1]])
<<<<<<< HEAD
=======
          datatable(y,options = list(searchHighlight=T,search = list(search=d2[[1]])))
>>>>>>> c6f5169df786f4896d3fdfaf7d327f3cc74c853f
        }
        else{
          ## Topics + Groups
          dtemp = df_corpus()
          if (length(input$group1) == 1) {
            dtemp = corpus_subset(x = dtemp,
                                  subset = get(input$group1) == d2$group)
            y = kwic(
              x = dtemp,
              pattern = d2$Topic,
              window = input$context,
              valuetype = 'glob',
              case_insensitive = T
            )
            y = as.data.table(y)
            y = y[, 4:6]
            y$new = do.call('paste', y)
            y = y[, !c(1:3)]
<<<<<<< HEAD

=======
            
>>>>>>> c6f5169df786f4896d3fdfaf7d327f3cc74c853f
          }
          else if (length(input$group1) == 2) {
            groupNames = (d0$group[key == selection2()$key])
            groupNames = strsplit(groupNames, "[.]")
            
            dtemp = corpus_subset(
              x = dtemp,
              subset = get(input$group1[[1]]) ==
                groupNames[[1]][1] &
                get(input$group1[[2]]) == groupNames[[1]][2]
            )
            
            y = kwic(
              x = dtemp,
              pattern = d2$Topic,
              window = input$context,
              valuetype = 'glob',
              case_insensitive = T
            )
            y = as.data.table(y)
            y = y[, 4:6]
            y$new = do.call('paste', y)
            y = y[, !c(1:3)]
            
          }
          else if (length(input$group1) == 3) {
            groupNames = (d0$group[key == selection2()$key])
            groupNames = strsplit(groupNames, "[.]")
            
            dtemp = corpus_subset(
              x = dtemp,
              subset = get(input$group1[[1]]) ==
                groupNames[[1]][1] &
                get(input$group1[[2]]) == groupNames[[1]][2] &
                get(input$group1[[3]]) == groupNames[[1]][3]
            )
            
            y = kwic(
              x = dtemp,
              pattern = d2$Topic,
              window = input$context,
              valuetype = 'glob',
              case_insensitive = T
            )
            y = as.data.table(y)
            y = y[, 4:6]
            y$new = do.call('paste', y)
            y = y[, !c(1:3)]
            
          }
        }
      }
    }
    
    
    
  })
}


# Create Shiny app ----
shinyApp(ui, server)
