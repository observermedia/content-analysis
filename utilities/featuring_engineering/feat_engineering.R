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



# Sentiment ---------------------------------------------------------------

#load in output from python sentiment analysis script






# Bind to main dataframe --------------------------------------------------

merge_tmp<- as.data.frame(cbind(df$num_images,df$num_links,as.character(df$link)))

colnames(merge_tmp) <- c('num_images','num_links','Page')
merge_tmp$Page<-gsub("http://observer.com", "", merge_tmp$Page ,perl = TRUE)


test<- merge(x=bindme,y=merge_tmp, by = "Page")


