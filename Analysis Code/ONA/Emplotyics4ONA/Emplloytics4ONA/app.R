# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mongolite)
library(data.table)
library(plotly)
library(ggplot2)
library(RJSONIO)
library(rlist)
library(reticulate)
library(plyr)
library(dplyr)
library(forcats)
library(purrr)
library(ggalt)
library(DT)




onaGM = mongo(collection = 'o_n_a__g_m',
              db = 'surveys',
              url = 'mongodb://localhost')




# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  # Application title
  titlePanel("Employtics"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fluidRow(uiOutput('dataSelect')),
      fluidRow(helpText('Select Primary filter:')),
      fluidRow(uiOutput('PempSelector1')),
      fluidRow(helpText('Select Comparison filter:')),
      fluidRow(uiOutput('CempSelector1')),
      # ,fluidRow( uiOutput('PratingsSelector2')),
      fluidRow(actionButton('apply','Apply'))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(plotlyOutput('plot')),
      fluidRow(htmlOutput("nSize")),
      br(),
      fluidRow(DT::dataTableOutput("textTable"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # Data Load ---------------------------------------------------------------
  
  
  output$dataSelect = renderUI({
    selectizeInput('dataSelect',
                   label = 'Select Data Source: ',
                   selected='Exit Survey',
                   choices = c('ONA - Gary'))
  })
  
  
  dataset = reactive({
    req(input$dataSelect)  
      m = onaGM
    
    return(m)
  })
  
  
  
  empChoices=reactive({
    req(input$dataSelect)
    m = dataset()
    
    #Get unique lines from surveys.employee
    empChoices = m$distinct({'employee'})
    
    #Find unique choices
    d1 = lapply(empChoices, function(x) {
      (unique(x))
    })
    d1 = d1[2:8]
    d1 = lapply(d1,function(d1)d1[d1 != ""])
    
    #build choices list
    choices = c(list(
      'Job Profile Name' = d1$JobProfileName,
      'Job Family' = d1$JobFamily,
      'Job Family Group' = d1$JobFamilyGroup,
      'Job Level' = d1$JobLevel,
      'Legacy Company'=d1$LegacyCompany,
      'Management Level' = d1$ManagementLevel,
      'SVP' = d1$SVP,
      'Manager' = d1$Manager,
      'Primary Work Country' = d1$PrimaryWorkCountry,
      'Region' = d1$Region,
      'Track' = d1$Track))
   
    return(choices)
  })
  
  
  
  #Dict for empchoices --------------------------------------------------------
  # Translate proper format for DB format
  toEmp = c('JobProfileName','JobLevel','JobFamily','JobFamilyGroup','LegacyCompany','ManagementLevel','SVP','Manager','PrimaryWorkCountry','Region','Track')
  fromEmp = c('Job Profile Name','Job Level','Job Family','Job Family Group','Legacy Company','Management Level','SVP','Manager','Primary Work Country','Region','Track')
  mapEmp = setNames(toEmp,fromEmp)
  mapper = function(df,to = toEmp,from=fromEmp){
    temp1 = df
    temp1 = as.matrix(temp1)
    map = setNames(to,from)
    groupings = (temp1[,1])
    fields = (temp1[,2])
    groupings = map[groupings]
    l = list(groupings,fields)
    l = do.call(rbind,l)
    l = as.data.table(l)
    return(l)
  }
  # Primary Selector - Employee -------------------------------------------------
  
  
  output$PempSelector1 = renderUI({
    selectizeInput('PempSelector1',
                   'Employee Attributes:',
                   choices = empChoices(),
                   multi = T)
  })
  
  output$CempSelector1 = renderUI({
    selectizeInput('CempSelector1',
                   'Employee Attributes:',
                   choices = empChoices(),
                   multi = T)
  })
  
  
  # output$PratingsSelector2 = renderUI({
  #   selectizeInput('PempSelector1',
  #                  'Ratings Attributes:',
  #                  choices = ratingsChoices(),
  #                  multi = T)
  # })
  
  
  # Get Primary selector and Groupings --------------------------------------
  
  convertInputs_fields = function(){
    d2 = NULL
    for (i in 1:length(input$PempSelector1)) {
      d1 = c(names(empChoices)[empChoices %>% map_lgl( ~ input$PempSelector1[i] %in% .)],
             input$PempSelector1[i])
      d2 = rbind(d2, d1)
    }
    d2 = mapper(d2)
    d2 = as.data.table(d2)
    return(d2)
  }
  
  # convertInputs_groups = function(){
  #   d2 = NULL
  #   # print(length(input$PempSelector1))
  #   for (i in 1:length(input$PempSelector1)) {
  #     d1 = c(names(empChoices)[empChoices %>% map_lgl( ~ input$PempSelector1[i] %in% .)],
  #            input$PempSelector1[i])
  #     d2 = rbind(d2, d1)
  #     d2 = as.data.table(d2)
  #     return(d2)
  #   }
  # }
  
  
  primary = reactive({
    req(input$PempSelector1)
    print(input$PempSelector1)
    empChoices = empChoices()
    # groupings = c('employeeStatus',
    #               'gender',
    #               'jobFammilyGroup',
    #               'jobLevel',
    #               'legacyCompany',
    #               'region' ,
    #               'workCountry')
    d2 = NULL
    for (i in 1:length(input$PempSelector1)) {
      d1 = c(names(empChoices)[empChoices %>% map_lgl( ~ input$PempSelector1[i] %in% .)],
             input$PempSelector1[i])
      d2 = rbind(d2, d1)
    }
    d2 = mapper(d2)
    d2 = as.data.table(d2)
    print(d2)
    return(d2)
  })
  
  
  
  # Get Comparison selector and Groupings --------------------------------------
  comp = reactive({
    req(input$CempSelector1)
    empChoices = empChoices()
    d2 = NULL
    for (i in 1:length(input$CempSelector1)) {
      d1 = c(names(empChoices)[empChoices %>% map_lgl( ~ input$CempSelector1[i] %in% .)],
             input$CempSelector1[i])
      d2 = rbind(d2, d1)
    }
    d2 = mapper(d2)
    d2 = as.data.table(d2)
    return(d2)
  })
  
  
  
  # Get Data ----------------------------------------------------------------
  
  
  fullDF = reactive({
    
    # if (is.null(input$PempSelector1)){
    # 
    # }
    
    m = dataset()
    len = m$count()
    x = m$iterate()
    x = x$batch(size = len)
    emp=NULL
    for (i in 1:length(x)){
      emp[i]=x[[i]]$employee
    }
    emp = rbindlist(emp,fill = T)
    
    text = NULL
    for (i in 1:length(x)){
      text[i]=x[[i]]$comments
    }
    
    # answers = NULL
    # 
    # for (i in 1:length(x)){
    #   answers[i]=x[[i]]$answers
    # }
    # answers = rbindlist(answers,fill=T)
    
    
    comments = NULL
    for(i in 1:length(text)){
      comments[i] = text[[i]]$comment
    }
    
    tokens = NULL
    for(i in 1:length(text)){
      tokens[i] =list(unlist(text[[i]]$tokens))
    }
    
    
    df = cbind(emp,comments)
    
  })
  
  PfilteredDF = reactive({
    
    df = fullDF()
    df= as.data.frame(df)
    filt = primary()
(filt)
    d2 = NULL
    setDT(df)
    # No filters
    if(is.null(input$PempSelector1) & is.null(input$CempSelector1)){
      return(df)
    }
    
    #Primary filter 
    else if(!is.null(filt)){
      column = unlist(filt[1,])
      value = unlist(filt[2,])
      tofilter = split(value,column)
      tokeep = apply(do.call(rbind,lapply(names(tofilter),function(x){
        `[[`(df,x) %in% tofilter[[x]]
      })),2,all)
      df = df[tokeep==TRUE,]
      
      
    }
  })
  
  
  CfilteredDF = reactive({
    
    df = fullDF()
    df= as.data.frame(df)
    filt = comp()
    d2 = NULL
    setDT(df)
    # No filters
    if(is.null(input$CempSelector1)){
      return(df)
    }
    
    #Comp filter 
    else{
      column = unlist(filt[1,])
      value = unlist(filt[2,])
      tofilter = split(value,column)
      tokeep = apply(do.call(rbind,lapply(names(tofilter),function(x){
        `[[`(df,x) %in% tofilter[[x]]
      })),2,all)
      df = df[tokeep==TRUE,]
      
    }
  })
  
  
  lemmaFile = read.csv('~/github/WD/src/Employtics/lemmatization-en.txt', sep = '\t', as.is = TRUE,
                       header = FALSE)
  
  tokens_convert <- function(x, from, to) {
    type <- attr(x, 'types')
    type_new <- to[match(type, from)]
    type_new <- ifelse(is.na(type_new), type, type_new)
    attr(x, 'types') <- type_new
    quanteda:::tokens_recompile(x)
  }
  
  
  
  
  allData = reactive({
    #Both Empty
    if(is.null(input$CempSelector1) && is.null(input$PempSelector1)){
      return(fullDF())
    }
    #Primary Only
    else if(!is.null(input$PempSelector1) && is.null(input$CempSelector1)){
      pdf = PfilteredDF()
      cdf = fullDF()
      # 
      # print('Primary ---------')
      # print(pdf)
      # print('Comp ---------')
      # print(cdf)
      ## Combine two with labels
      
      pdf$PC = 'Primary'
      cdf$PC = 'Comparison'
      
      df = rbind(pdf,cdf)
      
    }
    #Primary and Comp 
    else if(!is.null(input$PempSelector1) && !is.null(input$CempSelector1)){
      pdf = PfilteredDF()
      cdf = CfilteredDF()
      pdf$PC = 'Primary'
      cdf$PC = 'Comparison'
      
      df = rbind(pdf,cdf)
    }
  })
  
  
  
  # Build Corpus ----------------------------------------------------
  
  
  xCorp = reactive({
    x=corpus(as.data.frame(allData()),text_field = 'comments')
    
    
  })
  
  # Build Primary DFM -------------------------------------------------------
  
  
  xDFM = reactive({
    
    xCorp = xCorp()
    x = tokens(xCorp) %>% tokens_remove(c(stopwords('english'),'na',
                                          'n',
                                          'n a',
                                          'none',
                                          'western digital',
                                          'wd','nil','comment','seem','like','ot','sir',
                                          'go','set','get','can','x','0','#name'),padding=T) %>%
      dfm(remove_numbers = TRUE, remove_punct = TRUE,
          ngrams = 1:3,remove_symbols=T,concatenator= ' ')
    x = dfm_replace(x,pattern = lemmaFile$V2,lemmaFile$V1)
    return(x)
  })
  
  # Get Sample Size  ------------------------------------------------------
  
  sampleSize = reactive ({
    df = as.data.table(allData())
    
    if(!is.null(input$CempSelector1) && !is.null(input$PempSelector1)){
      
      primary = nrow(df[PC=='Primary'])
      comparison = nrow(df[PC == 'Comparison'])
      
      df = data.table(count = c(primary,comparison))
      
      
    }
    else if (is.null(input$CempSelector1) && !is.null(input$PempSelector1)){
      primary = nrow(df[PC=='Primary'])
      comparison = nrow(df[PC == 'Comparison'])
      
      df = data.table(count = c(primary,comparison))
      
      
    }
    else if (is.null(input$CempSelector1) && is.null(input$PempSelector1)){
      
      df = data.table(count = nrow(df))
      
    }
    
    
    
  })
  
  # Get Top Features --------------------------------------------------------
  
  
  getTopTerms = reactive({
    
    sampleSize=sampleSize()
    if(!is.null(input$CempSelector1) && !is.null(input$PempSelector1)){
      
      x = as.data.table(textstat_frequency(xDFM(),n=500,groups = 'PC'))
      x$Prct = ifelse(x$group=='Primary',x$docfreq/sampleSize$count[1]*100,x$docfreq/sampleSize$count[2]*100)
      x = dcast.data.table(x,feature~group,value.var = 'Prct')
      x[is.na(x)]=0
      x$diff = x$Primary - x$Comparison
      x = x[order(-x$Primary,-x$Comparison)]
      x = x[1:20,]
      key <- row.names(x)
      x <- data.frame(x, key)
      return(x)
      
    }
    else if (is.null(input$CempSelector1) && !is.null(input$PempSelector1)){
      x = as.data.table(textstat_frequency(xDFM(),n=500,groups = 'PC'))
      
      x$Prct = ifelse(x$group=='Primary',x$docfreq/sampleSize$count[1]*100,x$docfreq/sampleSize$count[2]*100)
      x = dcast.data.table(x,feature~group,value.var = 'Prct')
      # print(x)
      x[is.na(x)]=0
      x$diff = x$Primary- x$Comparison
      x = x[order(-x$Primary,-x$Comparison)]
      x = x[1:20,]
      key <- row.names(x)
      x <- data.frame(x, key)
      return(x)
    }
    else if (is.null(input$CempSelector1) && is.null(input$PempSelector1)){
      x = as.data.table(textstat_frequency(xDFM(),n=500))
      x$Prct = x$docfreq/sampleSize$count[1]*100
      x = x[order(-x$Prct)]
      x = x[1:20,]
      key <- row.names(x)
      x <- data.frame(x, key)
      return(x)
    }
    
    
  })
  
  
  
  # Plots -------------------------------------------------------------------
  
  
  # dot plot ----------------------------------------------------------------
  
  plot = reactive({
    x = getTopTerms()
    
    
    if (!is.null(input$CempSelector1) &&
        !is.null(input$PempSelector1)) {
      x$feature = factor(x$feature,
                         levels=x$feature[order(x$Primary)])
      
      p <- plot_ly(x, color = I("gray80")) %>%
        add_segments(x = ~Primary, xend = ~Comparison, y = ~feature, yend = ~feature, showlegend = FALSE) %>%
        add_markers(x = ~Primary, y = ~feature, name = "Primary", color = I("red")) %>%
        add_markers(x = ~Comparison, y = ~feature, name = "Comparison", color = I("blue")) %>%
        layout(
          title = "",
          xaxis = list(title = "% of Comments"),
          yaxis = list(title = ''),
          margin = list(l = 100)
        )
      p
      
      
    }
    else if (is.null(input$CempSelector1) &&
             !is.null(input$PempSelector1)) {
      x$feature = factor(x$feature,
                         levels=x$feature[order(x$Primary)])
      p <- plot_ly(x, color = I("gray80")) %>%
        add_segments(x = ~Primary, xend = ~Comparison, y = ~feature, yend = ~feature, showlegend = FALSE) %>%
        add_markers(x = ~Primary, y = ~feature, name = "Primary", color = I("red")) %>%
        add_markers(x = ~Comparison, y = ~feature, name = "Comparison", color = I("blue")) %>%
        layout(
          title = "",
          xaxis = list(title = "% of Comments"),
          yaxis = list(title = ''),
          margin = list(l = 100)
        )
      p
      
    }
    else if (is.null(input$CempSelector1) &&
             is.null(input$PempSelector1)) {
      x$feature = factor(x$feature,
                         levels=x$feature[order(x$Prct)])
      p <- plot_ly(x, color = I("gray80")) %>%
        add_segments(x = ~Prct, xend = ~Prct, y = ~feature, yend = ~feature, showlegend = FALSE) %>%
        add_markers(x = ~Prct, y = ~feature, color = I("red")) %>%
        # add_markers(x = ~Comparison, y = ~feature, name = "Comparison", color = I("blue")) %>%
        layout(
          title = "",
          xaxis = list(title = "% of Comments"),
          yaxis = list(title = ''),
          margin = list(l = 100)
        )
      p
      
      
      
      # 
      # p = ggplot(x, aes(x = fct_rev(fct_inorder(feature)),size=6,alpha=.8,key=key)) +
      #   geom_point(aes(y = Prct ,colour='red')) +
      #   xlab('')+ylab('% of Comments')+
      #   coord_flip()
      # ggplotly(p)
      
    }
    
    
    
    
  })
  
  clickSelection = reactive({
    s = event_data('plotly_click')
    df = data.frame(s)
    return(df)
    
  })
  
  
  output$textTable = renderDataTable({
    if(!is.null(clickSelection)){
    
    if (!is.null(input$CempSelector1) &
        !is.null(input$PempSelector1))  {
      click = clickSelection()
      xCorp=xCorp()
      # print(click$curveNumber)
      # print(click)
      if(click$curveNumber == 1){
        req(clickSelection())
        x1 = corpus_subset(x = xCorp(), subset = PC=='Primary')
        y = kwic(x1, as.character(click$y),window=1000)
        y = as.data.table(y[, 4:6])
        y$Verbatims = do.call('paste', y)
        y = y[,!c(1:3)]
        y = unique(y)
        return(y)
      }
      
      else{
        x1 = corpus_subset(x = xCorp, subset = PC=='Comparison')
        y = kwic(x1, as.character(click$y),window=1000)
        y = as.data.table(y[, 4:6])
        y$Verbatims = do.call('paste', y)
        y = y[,!c(1:3)]
        y = unique(y)
        return(y)
      }
      
      
    }
    else if (is.null(input$CempSelector1) &&
             !is.null(input$PempSelector1)) {
      click = clickSelection()
      xCorp=xCorp()
      # print(click$curveNumber)
      # print(click)
      if(click$curveNumber == 1){
        req(clickSelection())
        x1 = corpus_subset(x = xCorp(), subset = PC=='Primary')
        y = kwic(x1, as.character(click$y),window=1000)
        y = as.data.table(y[, 4:6])
        y$Verbatims = do.call('paste', y)
        y = y[,!c(1:3)]
        y = unique(y)
        return(y)
      }
      
      else{
        x1 = corpus_subset(x = xCorp, subset = PC=='Comparison')
        y = kwic(x1, as.character(click$y),window=1000)
        y = as.data.table(y[, 4:6])
        y$Verbatims = do.call('paste', y)
        y = y[,!c(1:3)]
        y = unique(y)
        return(y)
      }
      
    }
    else if (is.null(input$CempSelector1) &&
             is.null(input$PempSelector1)) {
      df = as.data.table(allData())
      click = clickSelection()
      xCorp = xCorp()
      y = kwic(xCorp,click$y,window=1000)
      y = as.data.table(y[, 4:6])
      y$Verbatims = do.call('paste', y)
      y = y[,!c(1:3)]
      y = unique(y)
      return(y)
      
      
    }
    
    }
  })
  
  
  
  
  
  output$plot = renderPlotly({plot()})
  
  output$nSize = renderText({
    sampleSize = sampleSize()
    if(nrow(sampleSize)==1){
      HTML(paste('Sample Size:',sampleSize))
    }
    else{
      HTML(paste('Sample Size','<br>','Primary:',sampleSize[1,],'<br>','Comparison:',sampleSize[2,]))
    }
  })
  
  
  # Time Series -------------------------------------------------------------
  
  tsData = reactive({
    
    
    
    
  })
  
  output$ts = renderPlotly({tsData()})
  
}


# Run the application 
shinyApp(ui = ui, server = server)
#shiny::runApp(display.mode="showcase")