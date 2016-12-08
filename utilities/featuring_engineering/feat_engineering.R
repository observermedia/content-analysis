###
#Feature engineering
###




# HTML pics ---------------------------------------------------------------


num_images <- 0
for (i in 1:nrow(df)){
  num_images[i]<-sum(str_count( df$content[i], "img class="))
}

df$num_images <- num_images




# HTML_links --------------------------------------------------------------

num_links <- 0
for (i in 1:nrow(df)){
  num_links[i]<-sum(str_count( df$content[i], "<a href="))
}
df$num_links <- num_links

wiki_links <- 0
for (i in 1:nrow(df)){
  wiki_links[i]<-sum(str_count( df$content[i], '<a href="https://en.wikipedia'))
}

df$wiki_links <- wiki_links


# Sentiment ---------------------------------------------------------------

#load in output from python sentiment analysis script
sent_tmp <- read.csv('sent_scores.csv')
sent_tmp<- sent_tmp[,c(4:8)]


# Complexity score --------------------------------------------------------

fles_tmp <- read.csv('word_complex_scores.csv')
fles_tmp <- fles_tmp[,c(4,5)]


# Bind to main dataframe for modeling --------------------------------------------------

merge_tmp<- as.data.frame(cbind(df$num_images,df$num_links,df$wiki_links,as.character(df$link)))

colnames(merge_tmp) <- c('num_images','num_links','wiki_links','Page')
merge_tmp$Page<-gsub("http://observer.com", "", merge_tmp$Page ,perl = TRUE)
sent_tmp$Page <- gsub("http://observer.com", "", sent_tmp$Page ,perl = TRUE)
fles_tmp$Page <- gsub("http://observer.com", "", fles_tmp$Page ,perl = TRUE)

merge1<- merge(x=merge_tmp,y=sent_tmp, by = "Page")
merge2<- merge(x=merge1,y=fles_tmp, by = "Page")





