library(needs)
options(stringsAsFactors = F)
needs(data.table,ggplot2,dplyr,scales,reshape2)


old = fread(input = '~/Desktop/Data/6Month Survey Test/New Hire Survey 6 Months_oldway.csv',check.names = T)
new = fread(input= '/Users/jb1000249384/Desktop/Data/6Month Survey Test/New Hire Survey 6 Months_newway.csv',check.names = T)

# Change Likert to Numeric ------------------------------------------------

old[,7:19] = lapply(old[,7:19],function(x) ifelse(x=='Strongly Agree',5,ifelse(x=='Agree',4,ifelse(x=='Neutral',3,ifelse(x=='Disagree',2,1)))))

newMelt = melt(new[,1:15],id.vars = c('Respondent.ID','surveyDate','Email.Address','EmployeeId'))
oldMelt = melt(old,id.vars=c('Respondent.ID','End.Date','Email.Address','employeeID','gender','region'))

newMelt = newMelt %>% group_by(variable)%>% mutate(value = rescale(value,to=c(0,5)))%>%mutate(survey = 'new')
oldMelt = oldMelt %>% group_by(variable)%>% mutate(survey = 'old')

df = rbind(newMelt,oldMelt)
df= as.data.table(df[!is.na(df$value),])

ggplot(df,aes(x = value,fill=survey))+  geom_histogram(bins = 5,
                                                       aes(y = ..density..))+facet_wrap(.~variable)



# Stats -------------------------------------------------------------------

stats = df %>% group_by(survey,variable) %>% summarize(mean = mean(value),sd=sd(value),var=var(value))

statsMelt = melt(stats,id.vars=c('survey','variable'),variable.name = 'stat')

ggplot(statsMelt,aes(x=survey,y = value,fill=survey))+geom_bar(stat = 'identity')+facet_grid(stat~variable)



# extremes ----------------------------------------------------------------

extMelt = melt(new[,1:16],id.vars = c('Respondent.ID','surveyDate','Email.Address','EmployeeId'))
ggplot(extMelt,aes(x=as.numeric(value)))+geom_histogram(bins=20)




# Text analysis -----------------------------------------------------------

needs(tidytext,quanteda)

xCorp = corpus(new,text_field='comments')
xDFM = tokens(xCorp,remove_numbers = T, remove_punct = T,
                   remove_symbols = T, remove_separators = TRUE,
                   remove_hyphens = FALSE) %>% tokens_remove(c(stopwords(),'western','western digital','wd','digital')) %>%
  tokens_ngrams(1:4, concatenator = " ") %>%
  dfm()

xCorp = tokens(xCorp,remove_numbers = T, remove_punct = T,
              remove_symbols = T, remove_separators = TRUE,
              remove_hyphens = FALSE) %>% tokens_remove(c(stopwords(),'western','western digital','wd','digital')) %>%
  tokens_ngrams(1:4, concatenator = " ") 
textstat_collocations(xCorp)

  x = docfreq(xDFM)
  
  

# sentiment ---------------------------------------------------------------
needs(sentimentr, corrplot)

text = get_sentences(new$comments)  
sentiment = sentiment_by(text)

new$sentiment = sentiment$ave_sentiment

newdf = new[,c(5:15,17)]



rmeans = rowMeans(newdf[,1:11])

df2 = as.data.table(cbind(rmeans,sentiment$ave_sentiment))

mod1 = lm(formula = sentiment~.,data = newdf)
step(mod1,direction = 'both')
summary(lm(formula = sentiment ~ accuratePosition, data = newdf))
corrplot.mixed(cor(newdf,use = 'complete.obs'),order='hclust')

rmeans = rowMeans(newdf[,1:11])


