library(needs)
options(stringsAsFactors = F)
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
  reshape,plotly
)

uselessWords=c('na',
'n',
'n a',
'none',
'western digital',
'wd','nil','comment','seem','like','ot','sir')

topics = dictionary(list(
  workLifeBalance = c("work life", "work/life", "worklife"),
  manager = c(
    "manager",
    "managememnt",
    "director",
    'supervisor',
    'superviser',
    'supervisors',
    'supervisers'
  ),
  communication = c("communication", "communicate", 'communications'),
  benefits = c('health care','benefits','vision','dental','maternity leave','parental leave','paternity leave'),
  perks = c('perks'),
  family = c("family", 'families'),
  compensation = c(
    "comp",
    "pay",
    "salary",
    "bonus",
    "rsu",
    "stock",
    "wages",
    'paychecks',
    'paycheck'
  )
))


likeWords = dictionary(list(
  manager = c('manager', 'managers'),
  employees = c('employee', 'employees'),
  company = c('company', 'companies'),
  salary = c('salaries', 'salary'),
  benefit = c('benefits', 'benefits'),
  questions = c('questions', 'question'),
  coworkers = c('coworkers','coworker','co-workers','coworkers')
))

 # x = read.csv('~/Desktop/wdps7a_flat_file_abbr.csv')

getTopics = function(x, groups = NA, scheme = "docfreq", text_field=length(x), docid_field=1) {
  x1=as.data.table(x)
  toRemove = is.na(x1[[text_field]])
  x1 = x1[!toRemove,]
  x = corpus(as.data.frame(x1),text_field = text_field,docid_field = (docid_field))
  x = dfm(x,dictionary=topics)
  if (!is.na(groups)) {
    ## Find Top Features by groups
    y = topfeatures(x,
                    groups = groups,
                    scheme = "docfreq")
    group_names = names(y)
  
    outPut = list()
    for (i in 1:length(y)) {
      tempd0 = data.frame(as.list(y[i]))
      setDT(tempd0, keep.rownames = T)
      colnames(tempd0) = c('Topic', 'Count')
      tempd0$Topic = tolower(tempd0$Topic)
      if (length(groups) == 2) {
        group_names_split = as.data.frame(group_names)
        group_names_split <-
          data.frame(do.call('rbind', strsplit(
            as.character(group_names_split$group_names),
            '.',
            fixed = TRUE
          )))
        group1 = groups[1]
        group2 = groups[2]
        
        tempd0[, Prct := (tempd0[, 2] / nrow(x1[!is.na(get(group1) == group_names_split[i, 1] &
                                                  get(group2) == group_names_split[i, 2])])) * 100]
        
      }
      else if (length(groups) == 3) {
        group_names_split = as.data.frame(group_names)
        group_names_split <-
          data.frame(do.call('rbind', strsplit(
            as.character(group_names_split$group_names),
            '.',
            fixed = TRUE
          )))
        group1 = groups[1]
        group2 = groups[2]
        group3 = groups[3]
        
        tempd0[, Prct := tempd0[, 2] / nrow(x1[get(group1) == group_names_split[i, 1] &
                                                 get(group2) == group_names_split[i, 2] &
                                                 get(group3) == group_names_split[i, 3]]) * 100]
        
      }
      else{
        tempd0[, Prct := (tempd0[, 2] / nrow(!is.na(x1[get(groups) == group_names[i]]))*100)]
      }
      tempd0 = melt(tempd0, measure.vars = c('Prct', 'Count'))
      tempd0$group = group_names[i]
      outPut[[i]] = tempd0
      
    }
    y = rbindlist(outPut, use.names = T, fill = T)
    y
  }
  else {
    y = topfeatures(x, scheme = "docfreq")
    y = as.data.table((y), keep.rownames = T)
    colnames(y) = c("Topic", "Count")
    y[, Prct := Count / nrow(x1) * 100]
    y = melt(y,
             id.vars = 'Topic',
             measure.vars = c('Count', 'Prct'))
  }
  
  return(y)
}



getTerms = function(x,
                    n,
                    groups = NA,
                    scheme = "docfreq",
                    text_field = length(x),
                    docid_field = 1) {
  
  
  if (length(groups) > 3) {
    stop(
      'A max of three groups can be provided. Please build a new column that is a combination of two others to proceed.'
    )
  }
  
  x1 = as.data.table(x)
  toRemove = is.na(x1[[text_field]])
  x1 = x1[!toRemove,]
  x_corpus = corpus(as.data.frame(x1),
                    text_field = text_field,
                    docid_field = docid_field)
  x = dfm(
    x_corpus,
    remove = c(stopwords(), uselessWords),
    tolower = T,
    thesaurus = likeWords,
    verbose = T,
    remove_punct = T,
    remove_numbers = T
  )
  if (!is.na(groups)) {
    ## Find Top Features by groups
    y = topfeatures(x,
                    n = n,
                    groups = groups,
                    scheme = "docfreq")
    group_names = names(y)
    
    ## Manipulate named vectors into data.frame and melt to long form
    
    ### Handling multiple groups
    #   Constrained to only 2 factors in groups, anything more will require feature engineering to build a new column for it
    #   Building a new col will need to be bone pre-analysis
    ###
    
    ## Cleaning Req's
    # - no NA's
    outPut = list()
    for (i in 1:length(y)) {
      tempd0 = data.frame(as.list(y[i]))
      setDT(tempd0, keep.rownames = T)
      colnames(tempd0) = c('Term', 'Count')
      tempd0$Term = tolower(tempd0$Term)
      if (length(groups) == 2) {
        group_names_split = as.data.frame(group_names)
        group_names_split <-
          data.frame(do.call('rbind', strsplit(
            as.character(group_names_split$group_names),
            '.',
            fixed = TRUE
          )))
        group1 = groups[1]
        group2 = groups[2]
        
        tempd0[, Prct := (tempd0[, 2] / nrow(x1[get(group1) == group_names_split[i, 1] &
                                                  get(group2) == group_names_split[i, 2]])) * 100]
        
      }
      else if (length(groups) == 3) {
        group_names_split = as.data.frame(group_names)
        group_names_split <-
          data.frame(do.call('rbind', strsplit(
            as.character(group_names_split$group_names),
            '.',
            fixed = TRUE
          )))
        group1 = groups[1]
        group2 = groups[2]
        group3 = groups[3]
        
        tempd0[, Prct := tempd0[, 2] / nrow(x1[get(group1) == group_names_split[i, 1] &
                                                 get(group2) == group_names_split[i, 2] &
                                                 get(group3) == group_names_split[i, 3]]) * 100]
        
      }
      else{
        tempd0[, Prct := tempd0[, 2] / nrow(x1[get(groups) == group_names[i]]) *
                 100]
      }
      tempd0 = melt(tempd0, measure.vars = c('Prct', 'Count'))
      tempd0$group = group_names[i]
      outPut[[i]] = tempd0
      
    }
    y = rbindlist(outPut, use.names = T, fill = T)
    y
  }
  else {
    y = topfeatures(x, n = n, scheme = "docfreq")
    y = as.data.table((y), keep.rownames = T)
    colnames(y) = c("Term", "Count")
    y$Term = tolower(y$Term)
    y[, Prct := Count / nrow(x1) * 100]
    y = melt(y,
             id.vars = 'Term',
             measure.vars = c('Count', 'Prct'))
  }
  
  return(y)
}

getKeyness = function(t1,numOut=30){
  outp=list()
  for ( i in 1:length(docnames(t1))){
    y=textstat_keyness(t1,target = docnames(t1)[[i]])
    y$group = docnames(t1)[i]
    y = y[1:numOut,]
    outp = rbind(y,outp)
    
  }
  outp$Term = row.names(outp)
  return(outp)
}

# 
# test = getTerms(x,groups='Gender',n=100)
# # test = getTopics(x,groups = c('Region','Gender'))

