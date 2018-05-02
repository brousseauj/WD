library(needs)
needs(quanteda,data.table,ggplot2,topicmodels)


df = fread('~/Desktop/Data/Transformation ONA Survey - Raw Data.csv',na.strings = c('na','N/A','n/a','No','no','NA','nothing','None','NIL','None.','none','Nothing'))
df$text = paste(df$Q7,df$Q8)

xCorp = corpus(df,text_field = 'text',docid_field = 'Email')

x1 = tokens(xCorp,remove_numbers = T, remove_punct = T,
            remove_symbols = T, remove_separators = TRUE,
           remove_hyphens = FALSE,
            concatenator = "_") %>% tokens_remove(stopwords()) %>% dfm()

dtm = convert(x1,to = 'topicmodels')
lda = LDA(dtm,k=10
topics = as.data.table(terms(lda, 10))


mydict <- dictionary(list(Topic1 = topics$`Topic 1`,
                          Topic2 = topics$`Topic 2`,
                          Topic3 = topics$`Topic 3`,
                          Topic4 = topics$`Topic 4`,
                          Topic5 = topics$`Topic 5`,
                          Topic6 = topics$`Topic 6`,
                          Topic7 = topics$`Topic 7`,
                          Topic8 = topics$`Topic 8`,
                          Topic9 = topics$`Topic 9`,
                          Topic10 = topics$`Topic 10`))
x2 = dfm(x1,dictionary = mydict)
docnames(x2) =df$Email
numByTopicByUser = textstat_frequency(x2,groups = 'Email')

numByTopic = textstat_frequency(x2)


write.csv(numByTopicByUser,'~/Desktop/topicsByUser.csv')
write.csv(numByTopic,'~/Desktop/topicsByDoc.csv')
write.csv(topics,'~/Desktop/topics.csv')





