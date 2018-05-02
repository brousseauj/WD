library(needs)
needs(quanteda,data.table,ggplot2,topicmodels)


df = fread('~/Desktop/Data/Transformation ONA Survey - Raw Data.csv',na.strings = c('na','N/A','n/a','No','no','NA','nothing','None','NIL','None.','none','Nothing'))
df$text = paste(df$Q8,df$Q11)

xCorp = corpus(df,text_field = 'text')

x1 = tokens(xCorp,remove_numbers = T, remove_punct = T,
            remove_symbols = T, remove_separators = TRUE,
           remove_hyphens = FALSE,
            concatenator = "_") %>% tokens_remove(stopwords()) %>% dfm()

dtm = convert(x1,to = 'topicmodels')

lda = LDA(dtm,k=10)
terms(lda, 10)
