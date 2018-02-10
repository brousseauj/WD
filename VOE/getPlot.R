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
  reshape,plotly,rCharts,wesanderson
)

plot_colors = c("#c0843a",
                "#7f62b8",
                "#9dac3b",
                "#b74c7d",
                "#46c19a",
                "#b84c3f",
                "#69a150")

x=test
x = na.omit(x)
getPlot_bar=function(x,CountType='Prct'){
  if(x[['group']])
  d0 = x[variable==CountType,]
  p= d0 %>%  ggplot(aes(x=d0[[1]],y=value,fill=group))+
    geom_bar(stat = 'identity',position='dodge')+
    coord_flip()+
    xlab('')+
    ylab('% of Conversation')+ggtitle('Topic Intensity')+scale_fill_manual(values=plot_colors)
  ggplotly(p)
}

getPlot_word
df = read.csv('~/Desktop/wdps7a_flat_file (2).csv')
test = getTopics(df,groups='Gender')
