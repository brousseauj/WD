library(needs)
needs(mongolite,data.table,anomalize,tidyverse,anytime,tidytext,cleanNLP)


tidyverse_cran_downloads %>%
  # Data Manipulation / Anomaly Detection
  time_decompose(count, method = "stl") %>%
  anomalize(remainder, method = "iqr") %>%
  time_recompose() %>%
  # Anomaly Visualization
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.25) +
  labs(title = "Tidyverse Anomalies", subtitle = "STL + IQR Methods") 


m=mongo(collection = 'new_hire30_survey',
        db = 'surveys',
        url = 'mongodb://localhost')
len = m$count()
x = m$iterate()
x = x$batch(size = len)


emp = NULL
for (i in 1:length(x)) {
  emp[i] = x[[i]]$employee
}
emp = rbindlist(emp, fill = T)

text = NULL
for (i in 1:length(x)) {
  text[i] = x[[i]]$comments
}

answers = NULL

for (i in 1:length(x)) {
  answers[i] = x[[i]]$answers
}
answers = rbindlist(answers, fill = T)


comments = NULL
for (i in 1:length(text)) {
  comments[i] = text[[i]]$comment
}

tokens = NULL
for (i in 1:length(text)) {
  tokens[i] = list(unlist(text[[i]]$tokens))

}
tokens = data.table(tokens)

df = cbind(emp, comments, answers,tokens)



# Time series for top terms -----------------------------------------------
library(janeaustenr)
library(dplyr) 


y = df
names(y)[13]='date'

y = y %>%
  # tibbletime::as_tbl_time(index = date)%>%
  # mutate(date = as.Date(date)) %>%
  # group_by(date) %>%
  unnest_tokens(word,comments) %>%
  anti_join(stop_words) %>%
  count(word,sort = T) 
  # filter(word %in% top_n(y,n = 3,wt = count))
  # top_n(n=3,wt=count)
dput(y)

y %>%
  # Data Manipulation / Anomaly Detection
  time_decompose(count, method = "stl") %>%
  anomalize(remainder, method = "iqr") %>%
  time_recompose() %>%
  # Anomaly Visualization
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.25) +
  labs(title = "Tidyverse Anomalies", subtitle = "STL + IQR Methods") 





# 
# y = top_n(word,n=3,wt=count)
# 
#     
#   t1=tidyverse_cran_downloads
#  
#   ggplot(y,aes(date,n,group=word))+geom_line()+geom_smooth()
  
  
# Clean NLP ---------------------------------------------------------------


cnlp_init_udpipe()  
install.packages('udpipe')








df$tokens = lapply(df$tokens,function(x)paste(x,collapse = ' '))


xCorp = corpus(as.data.frame(df), text_field = 'comments')
# xtok = tokens(xCorp) %>% tokens_remove(
#   c(
#     stopwords('english'),
#     'na'),
#   padding = T
# )

xdfm=  dfm(xCorp,
    remove_numbers = TRUE,
    remove_punct = TRUE,
    ngrams = 1:3,
    remove_symbols = T,
    concatenator = ' '
  )



x3= as.data.table(textstat_frequency(xdfm,n=500,groups='gender'))
library(quanteda)
update.packages('quanteda')




library(topicmodels)
x2= NA
x3 = 'test'
paste(x2,x3)
