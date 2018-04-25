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



exit_survey = mongo(collection = 'exit_survey',
                    db = 'surveys',
                    url = 'mongodb://localhost')
eng_survey = mongo(collection = 'engagement_survey',
                   db = 'surveys',
                   url = 'mongodb://localhost')
new30 =  mongo(collection = 'new_hire30_survey',
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
                   choices = c('Exit Survey', 'Engagement Survey','New Hire 30 Day'))
  })
  
  
  dataset = reactive({
    req(input$dataSelect)  
    
    if (input$dataSelect == 'Exit Survey') {
      
      m = exit_survey
    }
    else if (input$dataSelect == 'Engagement Survey') {
      
      m = eng_survey
    }
    else if (input$dataSelect == 'New Hire 30 Day') {
      
      m = new30
    }
    # len = m$count()
    # x = m$iterate()
    # x = x$batch(size = len)
    return(m)
  })
  
  
  
  empChoices=reactive({
    req(input$dataSelect)
    m = dataset()
    
    #Get unique lines from surveys.employee
    empChoices = (m$distinct({'employee'}))
    
    #Find unique choices
    d1 = lapply(empChoices, function(x) {
      (unique(x))
    })
    d1 = d1[2:8]
    d1 = lapply(d1,function(d1)d1[d1 != ""])
    
    #build choices list
    choices = c(list(
      'Gender' = d1$gender,
      'Employee Status' = d1$employeeStatus,
      'Job Family Group' =  d1$jobFamilyGroup,
      'Job Level' =  d1$jobLevel,
      'Legacy Company' = d1$legacyCompany,
      'Region'= d1$region,
      'Work Country' = d1$workCountry
      # ,
      # 'Groupings'=list('Employee Status'= names(d1)[1],
      #                  'Gender' = names(d1)[2],
      #                  'Job Family Group'= names(d1)[3],
      #                  'Job Level'=names(d1)[4],
      #                  'Legacy Company' = names(d1)[5],
      #                  'Region' = names(d1)[6],
      #                  'Work Country' = names(d1)[7])
    ))
    return(choices)
  })
  
  # ratings — for later -----------------------------------------------------
  
  
  #  ratingsChoices = reactive({
  #    req(input$dataSelect)
  #    m = dataset()
  #    
  #    #Get unique lines from surveys.answers
  #     if (input$dataSelect == 'Engagement Survey'){
  #       ratingsChoices = m$find()
  #       ratingsChoices = ratingsChoices$answers
  #       ratingsChoices = do.call(rbind.data.frame,ratingsChoices)
  #       ratingsChoices = apply(X = ratingsChoices,MARGIN = 2,FUN = unique)
  #     }
  #    else{
  #   ratingsChoices= m$distinct({'answers'})
  #    }
  #    
  #    #Find unique choices
  #    d1 = lapply(ratingsChoices, function(x) {
  #      (unique(x))
  #    })
  #    d1 = d1[2:length(d1)]
  #    d1 = lapply(d1,function(d1)d1[d1 != ""])
  #  
  # if (input$dataSelect == 'Exit Survey'){
  #   choices = c(list( "Primary reason for joining" = d1$primaryForJoin,
  #                     "Secondary reason for joining" = d1$secondaryForJoin,
  #                     "Tertiary for joining" = d1$tertiaryForJoin,
  #                     "Primary for leaving" = d1$primaryForLeaving,
  #                     "Secondary for leaving" = d1$secondaryForLeaving,
  #                     "Tertiary for leaving" = d1$tertiaryForLeaving,
  #                     "Incidient for leaving?" = d1$incidentForLeaving,
  #                     "Likely to recommend WD" = d1$likelyToRecommend,
  #                     "Compare WD to other companies" = d1$compareWD))
  #   }
  #  else if (input$dataSelect == 'New Hire 30 Day'){
  #    choices = c(list('Position was described accurately' = d1$accuratePosition, 
  #                     "Good communication after job offer" = d1$communication, 
  #                     "Smooth transition from HR to Manager" = d1$smoothTransition, 
  #                     "New hire oreintation was affective" = d1$NewHireAffective, 
  #                     "Appropriate amount of training" =  d1$receivedTraining, 
  #                     "Know how to contact HR Central" = d1$contactHRCentral, 
  #                     "Know how to contact IT" = d1$contactIT ,
  #                     "Agreement - Vision, values, and history" = d1$agreementVision,
  #                     "Agreement - Benefit offerings" = d1$agreementNewHireBenefits, 
  #                     "Agreement - Performance management philosophy" = d1$agreementNewHirePerformance ,
  #                     "Agreement - IT (Computer, phone, network
  #                      accounts, email" = d1$agreementIT, 
  #                     "Agreement - Facilities (Cube or office, desk, chair)" = d1$agreementFacilities ,
  #                     "Agreement - Process guidance (Intranet, expenses, purchasing)" = d1$agreementProcessGuidance ,
  #                     "Agreement - Intro to co-workers" = d1$agreementCoworkerIntroduction, 
  #                     "Agreement - Transportation" = d1$agreementTransportation, 
  #                     "Manager helped define expectations" = d1$defineExpectations, 
  #                     "Clear link between my work and WD objectives" = d1$clearLinkObjectives, 
  #                     "Feeback from manager about Performance" = d1$improvePerformance, 
  #                     "Discussing concerns with manager" = d1$discussingConcerns, 
  #                     "Systems and processes needed to do my job" = d1$systemsAndProcesses, 
  #                     "Able to perform my job on my own in acceptable amount of time" = d1$jobPerformanceTime ,
  #                     "Apply discretionary effort" = d1$discretionary, 
  #                     "Values of WD align with my own" = d1$values, 
  #                     "Sense of beloning" = d1$belonging, 
  #                     "Statisfied with decision to join" = d1$statisfiedDecision, 
  #                     "WD is a good place to work" =d1$goodPlaceToWork, 
  #                     "Employee in 3-5 years?" = d1$future35years))
  #    x1 = choices
  #    x2 = rapply(x1, function(x) ifelse(x=='Strongly Agree' | x=='Agree','Pos',ifelse(x=='Not Applicable',x,'Neg')), how = "replace")
  #    x2 = lapply(x2,function(x) unique(x))
  #    choices = x2
  #  }
  #   else if(input$dataSelect == 'Engagement Survey'){
  #     choices = c(list(
  #       
  #       "Clear link between my work and WD" =d1$clearLink,
  #       "Manger gives me feedback about performance" = d1$managerGivesFeedback,
  #       "Comfortable discussing concerns with Manager" = d1$comfortableDiscussingConcernsWithManager,
  #       "Appropriately involved in decisions" = d1$involvedInDecisions,
  #       "Sufficient effort to gather opinions" = d1$effortToGetOpinions,
  #       "Recommend WD to a friend" = d1$recommendWD,
  #       "Feeling of personal accomplishment" = d1$feelingPersonalAccomplishment,
  #       "Encouraged to improve processes" = d1$encouragedToReinvent,
  #       "WD is making changes to compete" = d1$wdIsMakingChangesToCompete,
  #       "People here take responsibility for actions" = d1$peopleTakeResponsibilityForActions,
  #       "Effective cooperations between departments" = d1$affectiveCooperationBetweenDepartments,
  #       "People cooperate to get things done" = d1$peopleCooperateToGetThingsDone,
  #       "Accomplishments are recognized" = d1$accomplishmentsAreRecognized,
  #       "Systems to do my job" = d1$systemsToDoJob,
  #       "Processes to do my job" = d1$processesToDoJob,
  #       "Team has resources to do the job" = d1$teamHasResourcesToDoJob,
  #       "Opportunity to improve skills" = d1$givenOpportunityToImproveSkills,
  #       "Job makes use of my skills" = d1$jobMakesUseOfSkills,
  #       "Career advancement at WD" = d1$careerAdvancement,
  #       "Advancement given fairly"= d1$advancementGivenFairly,
  #       "WD commited to exceeding customer expectations" = d1$WDCommittedToExceedingCustomerExpectations,
  #       "I can see a future at WD over the next 3-5" = d1$seeAFutureAtWD,
  #       "Clear strategy of the company" = d1$clearStrategyOfCompany,
  #       "Clear performance goals" = d1$clearPerformanceGoals,
  #       "Meet with manager to discuss goals" = d1$meetingWithManagerToDiscussGoals,
  #       "Meaningful talk about Performance" = d1$meaningfulTalkAboutPerformance,
  #       "WD supports communities" = d1$WDSupportsCommunities,
  #       "Volunteer program reinforces company values" = d1$volunteerReinforcesValues,
  #       "Volunteered in the last year" = d1$volunteerInLastYear)
  #     )
  #   }
  #   
  # 
  #    
  #  return(choices)
  #  })
  
  #Dict for empchoices --------------------------------------------------------
  # Translate proper format for DB format
  toEmp = c("employeeStatus","gender","jobFamilyGroup","jobLevel","legacyCompany","region","workCountry")
  fromEmp = c("Employee Status","Gender","Job Family Group","Job Level","Legacy Company","region","Work Country")
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
  
  convertInputs_groups = function(){
    d2 = NULL
    # print(length(input$PempSelector1))
    for (i in 1:length(input$PempSelector1)) {
      d1 = c(names(empChoices)[empChoices %>% map_lgl( ~ input$PempSelector1[i] %in% .)],
             input$PempSelector1[i])
      d2 = rbind(d2, d1)
      d2 = as.data.table(d2)
      return(d2)
    }
  }
  
  
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
    return(d2)
  })
  
  
  
  # Get Comparison selector and Groupings --------------------------------------
  comp = reactive({
    req(input$CempSelector1)
    print(input$CempSelector1)
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
    
    answers = NULL
    
    for (i in 1:length(x)){
      answers[i]=x[[i]]$answers
    }
    answers = rbindlist(answers,fill=T)
    
    
    comments = NULL
    for(i in 1:length(text)){
      comments[i] = text[[i]]$comment
    }
    
    tokens = NULL
    for(i in 1:length(text)){
      tokens[i] =list(unlist(text[[i]]$tokens))
    }
    
    
    df = cbind(emp,comments,answers)
    
  })
  
  PfilteredDF = reactive({
    
    df = fullDF()
    df= as.data.frame(df)
    filt = primary()
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