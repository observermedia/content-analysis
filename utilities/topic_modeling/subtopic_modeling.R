
#load in output from text_analysis script..
load('LDA_7topics.RData')



#set parameters
nb_topics <- 5
subtopic<- 'Culture'



df <- df.new[df.new$topic == subtopic,]

#create dtm
content<- VCorpus(VectorSource(df$content))
content <- tm_map(content,content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')), mc.cores=16)

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
dfsub.new<-df[-as.numeric(empty.rows),]

#topic model
lda_object<- LDA(dtm,5)

#output viz
vizme <- topicmodels_json_ldavis(lda_object,content,dtm)
serVis(vizme,out.dir = paste('viz_sub',subtopic,sep = ''))







