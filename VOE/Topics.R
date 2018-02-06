library(needs)
options(stringsAsFactors = F)
needs(data.table, tm, ggplot2, wordcloud, readxl, dplyr, quanteda, topicmodels,reshape)

topics = dictionary(list(
  workLifeBalance = c("work life", "work/life", "worklife"),
  manager = c("manager", "managememnt", "director",'supervisor','superviser','supervisors','supervisers'),
  communication = c("communication","communicate",'communications'),
  family = c("family",'families'),
  compensation = c("comp", "pay", "salary","bonus", "rsu", "stock", "wages",'paychecks','paycheck')))


likeWords = dictionary(list(
  manager = c('manager','managers'),
  employees = c('employee','employees'),
  company = c('company','companies'),
  salary = c('salaries','salary'),
  benefit = c('benefits','benefits'),
  questions=c('questions','question')
))

topTopics = function(x, n, groups = NA, scheme = "docfreq",text_field=length(x),docid_field=1) {
  x1=as.data.table(df)
  x = corpus(as.data.frame(x1),text_field = text_field,docid_field = docid_field)
  x = dfm(x,dictionary=topics)
  if (!is.na(groups)) {
    y = topfeatures(x, n = n, groups = groups, scheme = "docfreq")
    group_names = paste0('Count_',names(y))
    y = data.table(t(rbindlist(lapply(y, as.data.frame.list), use.names = T)),
      keep.rownames = T)
    colnames(y) = c("Topics", group_names)
   
    ## Calculate Percent of Group
    for (i in 1:(length(y)-1)){
      inputVec = unique(x1[[groups]])
      # colname = paste0('Prct',inputVec[i])
      lengthVec = nrow(x1[get(groups)==inputVec[i]]) 
      y[,paste0('Prct_',inputVec[i]):= y[,i+1,with=F]/lengthVec*100]
    }
    
    y = melt.data.table(y,id.vars = 'Topics',measure.vars = c(2:length(y)),variable.factor = F)
    y=cbind(y,colsplit(y$variable,split = '_',names = c('MeasureType','Groups')))
    y$variable=NULL
  } else {
    y = topfeatures(x, n = n, scheme = "docfreq")
    y = as.data.table((y), keep.rownames = T)
    colnames(y) = c("Topic", "Total")
    y[,PrctTopic:=y$Total/nrow(x1)*100]
  
  }
  return(y)
}

topTerms = function(x, n, groups = NA, scheme = "docfreq",text_field=length(x),docid_field=1) {
  x1=as.data.table(df)
  x_corpus=corpus(as.data.frame(x1),text_field = text_field,docid_field = docid_field)
  x = dfm(x_corpus,remove=c(stopwords(),'na','n','n a','none','western digital','wd'),
          tolower = T,thesaurus = likeWords,verbose = T,remove_punct=T,remove_numbers=T)
  if (!is.na(groups)) {
  
    ## Find Top Features by groups
    y = topfeatures(x, n = n, groups = groups, scheme = "docfreq")
    group_names = names(y)
    
    ## Manipulate named vectors into data.frame and melt to long form
    outPut =list()
    for (i in 1:length(y)){
      tempd0 = data.frame(as.list(y[i]))
      setDT(tempd0,keep.rownames = T)
      colnames(tempd0)=c('Term','Count')
      tempd0$Term = tolower(tempd0$Term)
      tempd0[,Prct:=tempd0[,2]/nrow(x1[get(groups)==group_names[i]])*100]
      tempd0 = melt(tempd0,measure.vars = c('Prct','Count'))
      tempd0$group = group_names[i]
      outPut[[i]]=tempd0
      
    }
  y = rbindlist(outPut,use.names = T,fill = T)
  } else {

    y = topfeatures(x, n = n, scheme = "docfreq")
    y = as.data.table((y), keep.rownames = T)
    colnames(y) = c("Term", "Count")
    y$Term=tolower(y$Term)
    y[,Prct:=Count/nrow(x1)*100]
    y =melt(y,id.vars = 'Term',measure.vars = c('Count','Prct'))
  }

  return(y)
}

# topTopics(df,groups='Region',n=100)
# topTerms(df,groups='Region',n=100)
