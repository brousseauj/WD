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
  sentimentr
)
options(stringsAsFactors = F)
source('~/Documents/Git Clones/WD/WD/VOE/getTopics_Terms.R')

# Define UI for data upload app ----
ui <- fluidPage(
  tags$head(tags$style('#text1{font-family: "Cera PRO Regular";
                       }'
  )
  ),
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
      
      
      uiOutput('textField')
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(tabsetPanel(
      
      tabPanel(
        'Snap Shot',
        fluidRow(column(4, uiOutput('group2')), column(4, uiOutput('numKeywords2'))),
        fluidRow(plotOutput(
          'wordcloud', width = "800px",height='600px'))
      ),
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
        fluidRow(column(4,checkboxInput(inputId ='groupPlot',label = 'Group Plot?',value = F))),
        
        fluidRow(plotlyOutput('keywords')),
        fluidRow(uiOutput('context')),
        fluidRow(DT::dataTableOutput("table1")))
      
    )))
  )



###                         ####
###                         ####
###         SERVER FILE     ####
###                         ####
###                         ####

# Define server logic to read selected file and Visualize----
server = function(input, output) {
  options(shiny.maxRequestSize = 60 * 1024 ^ 2)
  
  
  # Raw Data Input ----------------------------------------------------------
  
  dataset <- reactive({
    infile <- input$FileInput
    if (is.null(infile))
      return(NULL)
    x = read.csv(infile$datapath,
             header = T,sep = ',',na.strings = c(" ","","NA",'na',"n/a",'N/A','N/a','nil'),check.names = F)
    
    row.names(x) = sprintf('a_%s',seq(1:nrow(x)))
    print(head (x))
    return(x)
  })
  

  # Text Field --------------------------------------------------------------
  
  
  output$textField = renderUI({
   
      temp = dataset()
     
      selectizeInput(
        "textField",
        "Which column contains the text you want to analyze?",
        multiple = T,
        options = list(placeholder = 'Select one column', maxItems = 1),
        choices  = colnames(temp)
      )
    
  })
  
  
  # Build Corpus and DFM ---------------------------------------------------
  
  # df_corpus

  
  # getTopics / getTerms from Raw Input -------------------------------------
  
  
  
  d1 = reactive({
    temp = as.data.table(dataset())
    temp = temp[!is.na(temp[[input$textField]]),]
    
    
    if (is.null(input$group1)) {
      if (input$analysisType == 'Themes') {
        temp <-
          getTopics(temp,
                    text_field = input$textField
                    )
      }
      else{
        temp <- getTerms(
          temp,
          n = input$numKeywords,
          text_field = input$textField
        )
      }
    }
    else{
      if (input$analysisType == 'Themes') {
        temp <-
          getTopics(
            temp,
            groups = input$group1,
            text_field = input$textField
            
          )
      }
      else{
        temp <- getTerms(
          temp,
          groups = input$group1,
          n = input$numKeywords,
          text_field = input$textField
          
        )
      }
    }
  })
  
  
  
  
  # Build Corpus ------------------------------------------------------------
  
  
  df_corpus = reactive({
    dTemp = dataset()
    dTemp = dTemp[!is.na(dTemp[[input$textField]]),]
    
    df_corpus = corpus(
      as.data.frame(dTemp),
      text_field = input$textField
      
    )
  })
  
  
  # Count Type  -------------------------------------------------------------
  
  
  output$varType = renderUI({
    radioButtons(
      "varType",
      "Count Type",
      choices = c(Prct = "Prct", Count = "Count"),
      selected = "Prct"
    )
  })
  
  
  # Number of Keywords to show ----------------------------------------------
  
  
  
  output$numKeywords = renderUI({
    sliderInput(
      "numKeywords",
      "Enter number of words to Show :",
      min = 1,
      max = 20,
      value = 10
    )
  })
  output$group1 = renderUI({
    colnames <- names(dataset())
    # Dropdown for selecting groups on word Clouds
    selectizeInput(
      "group1",
      "Choose groupings:",
      multiple = T,
      options = list(placeholder = 'Select fields', maxItems = 3),
      choices  = colnames
    )
  })
  
  
  # Data for dot plots ------------------------------------------------------
  
  
  
  d0 = reactive({
    if (!is.null(input$FileInput) &&
        !is.null(input$textField)) {
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
          d0$Term = d0$feature
          d0$Term = as.character(d0$Term)
          key <- seq(1:nrow(d0))
          d0$Term = gsub('[[:digit:]]+', '', d0$Term)
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
  
  
  # Plot dot plots Themes and Topics ----------------------------------------
  
  
  
  output$keywords = renderPlotly({
    d0 = d0()
    if (!is.null(input$FileInput) &&
        !is.null(input$textField)) {
      if (input$analysisType == 'Term') {
        ## Term + No Group
        if (is.null(input$group1)) {
          p = ggplot(d0, aes(reorder(Term,value), value, key = key)) +
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
          if (input$groupPlot==T){
            p = ggplot(d0, aes(reorder(Term, chi2),chi2, key = key,colour=group)) +
              geom_point() +
              coord_flip() +
              ylab('Importance Level (Chi2)') + xlab('') +
              theme_minimal()
            ggplotly(p)
          }
          ## Term + Group
          else{
            p = ggplot(d0, aes(reorder(Term, chi2),chi2, key = key, key2 = group)) +
              geom_point(
                aes(colour = chi2),
                shape = 16,
                size = 3,
                show.legend = F
              ) +
              scale_color_gradient(low = "#0091ff", high = "#f0650e") +
              coord_flip() + facet_wrap(~group,scales='free')+
              ylab('Importance Level (Chi2)') + xlab('') +
              theme_minimal()
            ggplotly(p)
          }
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
            coord_flip() +
            ylab(ifelse(
              input$varType == 'Prct',
              '% of Comments',
              'Number of Comments'
            )) + xlab('') +
            theme_minimal()
          ggplotly(p)
        }
        else{
          if (input$groupPlot==T){
            p = ggplot(d0, aes(Topic, value, key = key)) +
              geom_point(
                aes(colour = group),
                shape = 16,
                size = 3,
                show.legend = F
              ) +
              scale_color_gradient(low = "#0091ff", high = "#f0650e") +
              coord_flip()  +
              ylab(ifelse(
                input$varType == 'Prct',
                '% of Comments',
                'Number of Comments'
              )) + xlab('') +
              theme_minimal()
            ggplotly(p) %>% layout(dragmode = 'select')
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
              coord_flip() + facet_wrap( ~ group) +
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
    }
  })
  
  # 
  
  # Click Information -------------------------------------------------------
  
  
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
  
  
  # Number of context words -------------------------------------------------
  
  
  output$context = renderUI({
    sliderInput(
      "context",
      "Enter number of words for context :",
      min = 1,
      max = 50,
      value = 25
    )
  })
  
  
  # Render KWIC Table -------------------------------------------------------
  
  
  
  output$table1 = renderDT({
    selection2 = selection2()
    d0 = d0()
    d2 <- d0 %>% filter(key == selection2$key)
    
    d2 = as.data.table(d2, stringsAsFactors = F)
    if (!is.null(input$FileInput) &&
        !is.null(input$textField)) {
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
          y$Verbatims = do.call('paste', y)
          y = y[, !c(1:3)]
          y = unique(y)
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
            y$Verbatims = do.call('paste', y)
            y = y[, !c(1:3)]
            y = unique(y)
            
          }
          else if (length(input$group1) == 2) {
            groupNames = (d0$group[d0$key == selection2()$key])
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
            y$Verbatims = do.call('paste', y)
            y = y[, !c(1:3)]
            y = unique(y)
            
          }
          else if (length(input$group1) == 3) {
            groupNames = (d0$group[d0$key == selection2()$key])
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
            y$Verbatims = do.call('paste', y)
            y = y[, !c(1:3)]
            y = unique(y)
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
          y$Verbatims = do.call('paste', y)
          y = y[, !c(1:3)]
          y = unique(y)
          
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
            y$Verbatims = do.call('paste', y)
            y = y[, !c(1:3)]
            y = unique(y)
            
          }
          else if (length(input$group1) == 2) {
            groupNames = (d0$group[d0$key == selection2()$key])
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
            y$Verbatims = do.call('paste', y)
            y = y[, !c(1:3)]
            y = unique(y)
            
          }
          else if (length(input$group1) == 3) {
            groupNames = (d0$group[d0$key == selection2()$key])
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
            y$Verbatims = do.call('paste', y)
            y = y[, !c(1:3)]
            y = unique(y)
            
          }
        }
      }
    }
    
  })
  
  
  # Num of Keywords - Wordcloud ---------------------------------------------
  
  
  
  output$numKeywords2 = renderUI({
    sliderInput(
      "numKeywords2",
      "Enter number of words to Show :",
      min = 1,
      max = 200,
      value = 100
    )
  })
  
  
  # Grouping for wordcloud --------------------------------------------------
  
  
  output$group2 = renderUI({
    colnames <- names(dataset())
    # Dropdown for selecting groups on word Clouds
    selectizeInput(
      "group2",
      "Choose groupings:",
      multiple = T,
      options = list(placeholder = 'Select fields', maxItems = 3),
      choices  = colnames
    )
  })
  
  
  # Plot Wordcloud ----------------------------------------------------------
  
  ### New Wordcloud with chi2
  
  wcDf = reactive({
    if (!is.null(input$FileInput) &&
        !is.null(input$textField)) {
      temp = as.data.table(dataset())
      temp = temp[!is.na(temp[[input$textField]]),]
      
      ## Term + No Group
      if (is.null(input$group2)) {
        temp <- getTerms(
          temp,
          n = input$numKeywords2,
          text_field = input$textField,
          
        )
        temp = temp[variable == 'Prct',]
      }
      else{
        d0 = df_corpus()
        d0 = dfm(
          d0,
          remove = c(stopwords(), uselessWords),
          tolower = T,
          groups = input$group2,
          thesaurus = likeWords,
          verbose = T,
          remove_punct = T,
          remove_numbers = T
        )
        d0 = getKeyness(d0, numOut = 20)
        d0$Term = d0$feature
        d0$Term = as.character(d0$Term)
        d0$Term = gsub('[[:digit:]]+', '', d0$Term)
        d0 = as.data.table(d0)
        
        
        
        
      }
    }
  })
  
  
  
  output$wordcloud = renderPlot({
    if (is.null(input$group2)){
      
      wcDf() %>% slice (1:input$numKeywords2) %>%
        ggplot + aes(x=1,y=1,size=value,label=Term,color=value)+
        guides(fill=FALSE)+
        geom_text_repel(segment.size = 0) +
        scale_size(range = c(2, 15), guide = T) +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = NULL) +
        labs(x = '', y = '') +
        theme_classic()+theme(legend.position="none")
    }
    else{
      d2 = wcDf()
      d2 %>% 
        ggplot + aes(x=1,y=1,size=chi2,label=Term,color=group)+
        
        geom_text_repel(segment.size = 0) +
        scale_size(range = c(2, 15)) +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = NULL) +
        labs(x = '', y = '') +
        theme_classic()+theme(legend.text=element_text(size=14))+
        guides(colour = guide_legend(override.aes = list(size=14)))
    }
    
  })
  
  
}





# Create Shiny app ----
shinyApp(ui, server)
