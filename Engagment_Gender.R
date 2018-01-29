library(needs)
options(stringsAsFactors = F)
needs(data.table,tm,ggplot2,wordcloud,readxl,dplyr,quanteda,topicmodels)


df = read_xlsx('~/Desktop/wdps7a_flat_file (2).xlsx')
df = data.table(df)
df = df[!is.na(Gender),]

df[is.na(df)]='NA'
# Negative Impact ---------------------------------------------------------


negImpact = corpus(x = as.data.frame(df), docid_field = 'employee id',text_field = "Currently what negatively impacts your level of commitment?")
negImpact_dfm = dfm(negImpact,ngrams=1:4,remove=c(stopwords('english'),'na'),remove_punct=T,remove_numbers=T)
negImpact_topTerms=as.data.table(textstat_frequency(negImpact_dfm,groups='Gender'))
negImpact_topTerms[group=='Female',primary:=docfreq/nrow(df[df$Gender=='Female'])*100]
negImpact_topTerms[group=='Male',primary:=docfreq/nrow(df[df$Gender=='Male'])*100]
write.csv(x = topfeatures(negImpact_dfm,n=100,groups='Gender',scheme='docfreq'),'negImpact_topTerms.csv')

kwic(corpus_subset(negImpact,Gender=='Female'),'management',window=10,valuetype='glob')
kwic(corpus_subset(negImpact,Gender=='Female'),'work',window=10,valuetype='glob')
kwic(corpus_subset(negImpact,Gender=='Female'),'job',window=10,valuetype='glob')



# Positive Impact ---------------------------------------------------------


posImpact = corpus(x = as.data.frame(df), docid_field = 'employee id',text_field = "Currently what positively impacts your level of commitment?")
posImpact_dfm = dfm(posImpact,ngrams=1:4,remove=c(stopwords('english'),'na'),remove_punct=T,remove_numbers=T)
posImpact_topTerms=as.data.table(textstat_frequency(posImpact_dfm,groups='Gender'))
posImpact_topTerms[group=='Female',primary:=docfreq/nrow(df[df$Gender=='Female'])*100]
posImpact_topTerms[group=='Male',primary:=docfreq/nrow(df[df$Gender=='Male'])*100]
write.csv(x = topfeatures(posImpact_dfm,n=100,groups='Gender',scheme = 'docfreq'),'posImpact_topTerms.csv')

kwic(corpus_subset(posImpact,Gender=='Female'),'work',window=5,valuetype='glob')
kwic(corpus_subset(posImpact,Gender=='Female'),'team',window=5,valuetype='glob')
kwic(corpus_subset(posImpact,Gender=='Female'),'management',window=5,valuetype='glob')

kwic(corpus_subset(posImpact,Gender=='Male'),'good',window=5,valuetype='glob')
kwic(corpus_subset(posImpact,Gender=='Male'),'company',window=5,valuetype='glob')


# Questions not asked ---------------------------------------------------------


haventAsked = corpus(x = as.data.frame(df), docid_field = 'employee id',text_field = "What haven't we asked that you hoped we would ask and what is your response?")
haventAsked_dfm = dfm(haventAsked,ngrams=1:4,remove=c(stopwords('english'),'na'),remove_punct=T,remove_numbers=T)
haventAsked_topTerms=as.data.table(textstat_frequency(haventAsked_dfm,groups='Gender'))
haventAsked_topTerms[group=='Female',primary:=docfreq/nrow(df[df$Gender=='Female'])*100]
haventAsked_topTerms[group=='Male',primary:=docfreq/nrow(df[df$Gender=='Male'])*100]
write.csv(x = topfeatures(haventAsked_dfm,n=100,groups='Gender',scheme='docfreq'),'haventAsked_topTerms.csv')

kwic(corpus_subset(haventAsked,Gender=='Male'),'working',window=9,valuetype='glob')
kwic(corpus_subset(haventAsked,Gender=='Male'),'time',window=9,valuetype='glob')
kwic(corpus_subset(haventAsked,Gender=='Male'),'management',window=9,valuetype='glob')
kwic(corpus_subset(haventAsked,Gender=='Male'),'salary',window=9,valuetype='glob')

kwic(corpus_subset(haventAsked,Gender=='Female'),'future',window=9,valuetype='glob')
kwic(corpus_subset(haventAsked,Gender=='Female'),'better',window=9,valuetype='glob')



prop.table(table(df$Region,df$Gender))*100
prop.table(table(df$Region[df$Region=='AMERICAS'],df$Gender[df$Region=='AMERICAS']))*100
prop.table(table(df$Region[df$Region=='APAC'],df$Gender[df$Region=='APAC']))*100
prop.table(table(df$Region[df$Region=='EMEA'],df$Gender[df$Region=='EMEA']))*100

ggplot(df,aes(x=Gender,fill=Region))+geom_bar(stat='count',position='dodge')+ theme(text=element_text(size=16))+ggtitle('Total Number of Respondents by Region')

# slicing -----------------------------------------------------------------
x = tokens(df$`Currently what positively impacts your level of commitment?`,verbose=T)
y=as.list(x)

df$tokens = cbind(df,x)


y1 = as.data.frame(y)
setDT(y1,keep.rownames = T)

x1 = textstat_frequency(x,n=10)

ggplot(x1,aes(x = nrow(x1):1,y=frequency))+geom_point()+facet_wrap(~group)

