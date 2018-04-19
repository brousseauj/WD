#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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
source('~/github/WD/Employtics_revamp/global.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  

# Data Load ---------------------------------------------------------------

  output$dataSelect = renderUI({
    selectizeInput('dataSelect',
                   label = 'Select Data Source: ',
                   selected='Exit Survey',
                   choices = c('Exit Survey', 'Engagement Survey','New Hire 30 Day'))
  })

  
  dataset = reactive({
    temp = input$dataSelect
    print(temp)
    
    if (temp == 'Exit Survey') {
    
      m = exit_survey
    }
    else if (temp == 'Engagement Survey') {
   
      m = eng_survey
    }
    else if (temp == 'New Hire 30 Day') {
    
      m = new30
    }
    # len = m$count()
    # x = m$iterate()
    # x = x$batch(size = len)
    return(m)
  })

# Build Choice Options ----------------------------------------------------
  
  empChoices=reactive({
    req(input$dataSelect)
    m = dataset()
    
  empChoices = (m$distinct({'employee'}))
    
    d1 = lapply(empChoices, function(x) {
      (unique(x))
    })
    d1 = d1[2:8]
    d1 = lapply(d1,function(d1)d1[d1 != ""])
    
    choices = c(list(
                'Gender' = d1$gender,
                'Employee Status' = d1$employeeStatus,
                'Job Family Group' =  d1$jobFamilyGroup,
                'Job Level' =  d1$jobLevel,
                'Legacy Company' = d1$legacyCompany,
                'Region'= d1$region,
                'Work Country' = d1$workCountry,
                'Groupings'=list('Employee Status'= names(empChoices)[1],
                                 'Gender' = names(empChoices)[2],
                                 'Job Family Group'= names(empChoices)[3],
                                 'Job Level'=names(empChoices)[4],
                                 'Legacy Company' = names(empChoices)[5],
                                 'Region' = names(empChoices)[6],
                                 'Work Country' = names(empChoices)[7])
                ))
    return(choices)
  })
  
# temp1 = data.frame(x = c('Gender','Male'),y=c('Employee Status','Active'))
# Dict for choices --------------------------------------------------------
to = c("employeeStatus","gender","jobFamilyGroup","jobLevel","legacyCompany","region","workCountry")
from = c("Employee Status","Gender","Job Family Group","Job Level","Legacy Company","region","Work Country")
map = setNames(to,from)
mapper = function(temp1){
  temp1 = as.matrix(temp1)
  groupings = (temp1[1,])
  fields = (temp1[2,])
  groupings = map[groupings]
  l = list(groupings,fields)
  l = do.call(rbind,l)
  l = as.data.table(l)
  return(l)
}


# Primary Selector --------------------------------------------------------

  
  output$selector1 = renderUI({
    selectizeInput('selector1',
                   'Primary:',
                   choices = empChoices(),
                   multi = T)
  })
  

# Comparison Selector -----------------------------------------------------

  
  output$selector2 = renderUI({
    selectizeInput('selector2',
                   'Comparison:',
                   choices = empChoices(),
                   multi = T)
  })
  


# Primary Output and Corpus Build -----------------------------------------
  

### This gets the selection from Primary and which group it belongs to
  primarySelector = reactive({
    
    choices=empChoices()
    req(input$selector1)
    d2=NULL
    for (i in 1:length(input$selector1)){
      d1=c(
        names(choices)[choices %>% map_lgl(~input$selector1[i] %in% .)],
        input$selector1[i]
      )
      d2 = cbind(d2,d1)
    }
    return(d2)
  })


### This gets the selection from Comparison and which group it belongs to  
  compSelector = reactive({
    
    choices=empChoices()
    req(input$selector2)
    d2=NULL
    for (i in 1:length(input$selector2)){
      d1=c(
        names(choices)[choices %>% map_lgl(~input$selector2[i] %in% .)],
        input$selector2[i]
      )
      d2 = cbind(d2,d1)
    }
    print(as.data.table(d2))
    return(d2)
  })
  
  
  ### This retrieves the data with the filter option for mongolite
  primaryData = reactive({
    tempIN = primarySelector()
    temp = input$dataSelect
    exit_survey = mongo(collection = 'exit_survey',
                        db = 'surveys',
                        url = 'mongodb://localhost')
    eng_survey = mongo(collection = 'engagement_survey',
                       db = 'surveys',
                       url = 'mongodb://localhost')
    
    new30 =  mongo(collection = 'new_hire30_survey',
                   db = 'surveys',
                   url = 'mongodb://localhost')
    
    if(temp=='Exit Survey'){
      m = exit_survey
    }
    if(temp=='Engagement Survey'){
      m=eng_survey
    }
    if(temp == 'New Hire 30 Day'){
      m= new30
    }
    x = primarySelector()
    x = as.data.frame(mapper(x))
    nlength = ncol(x)
    temp=NULL
    head(x)
    for (i in 1:nlength){
      temp[i] = sprintf('"employee.%s":"%s"',x[1,i],x[2,i])
    }
  print(temp)
    m$find('{"employee.gender":"Male"}')
    filt = paste(temp,collapse = ',')
    d1 = m$find(paste0('"{',filt,'}"'))
    return(d1)
  })
  
  output$result = renderText({
    print(head(primaryData()))
  })
# 
#   primary_df = reactive({
#     d1 = primary()
#     df = dataset()
#     d2=NULL
#     if(nrow(d1)==1){
#       d2 = df[get(d1[1,1])==d1[1,2]]
#     }
#     else if(nrow(d1)==2){
#       d2 = df[get(d1[1,1])==d1[1,2] &
#                 get(d1[2,1])==d1[2,2]]
#     }
# 
#     else{
#       d2 = df[get(d1[1,1])==d1[1,2] &
#                 get(d1[2,1])==d1[2,2] &
#                 get(d1[3,1])==d1[3,2]]
#     }
# 
# 
#   })

})
