

#subset on topic
#df is cleaned dataframe containing articles used for primary topic modeling
#topic is topic to subset on (i.e. discover subtopics within this topic)
#nb_topics is number of topics for LDA

model_subtopic <- function(df,topic,nb_topics){
  df<- df[df$topic == topic,]
    
  #create dtm
  sub_corpus <- VCorpus(VectorSource(df$content))
  dtmsub <- DocumentTermMatrix(sub_corpus, control = list(weighting = weightTf))
  
  rowsums <- apply(dtmsub,1,sum)
  dtmsub.new <- dtmsub[rowsums > 0, ]
  sub_corpusnew <- sub_corpus[-as.numeric(empty.rows)]
  dfsub.new<-df.new[-as.numeric(empty.rows),]
  
  
  
  num_topics = 5
  
  
  
  lda_objectsub <- LDA(dtmsub.new,nb_topics)
  return (lda_objectsub)
  
}


df <- df.new[df.new$topic == 'US Politics',]

#create dtm
content<- VCorpus(VectorSource(df$content))
content <- tm_map(content,content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')), mc.cores=1)

content.clean <- tm_map(content, stripWhitespace)                                 #strip whitespace
content.clean <- tm_map(content.clean,removeNumbers)                              #remove numbers
content.clean <- tm_map(content.clean,removePunctuation)                          #remove punctuation
content.clean <- tm_map(content.clean,content_transformer(tolower))               #convert to lower case
content.clean <- tm_map(content.clean,removeWords,stopwords("english"))           #removestop words
content.clean <- tm_map(content.clean,stemDocument)                               #stem words

#create dtm
dtm <- DocumentTermMatrix(content.clean, control = list(weighting = weightTf))
rowsums <- apply(dtm,1,sum)
empty.rows <- dtm[rowsums == 0, ]$dimnames[1][[1]]
dtm.new <- dtm[rowsums > 0, ]
content.new <- content[-as.numeric(empty.rows)]
df.new<-df[-as.numeric(empty.rows),]

#topic model
lda_object<- LDA(dtm,5)

#output viz
vizme <- topicmodels_json_ldavis(lda_object,content,dtm)
serVis(vizme,out.dir = 'viz_subtest')
