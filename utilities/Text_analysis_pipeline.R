###
#Text analysis pipeline
###

#Load data -> clean -> topic modeling -> output results and viz


#Ultimately wrap this whole pipeline into function...


# Packages ----------------------------------------------------------------


require(rjson)
#require(RJSONIO)
require(NLP)
require(tm)
require(topicmodels)
require(SnowballC)
require(LDAvis)

source('topicmodel_utilities.R')




# Load Data ---------------------------------------------------------------



json_file <- fromJSON(file='clean_data2.json') 
json_file <- lapply(json_file,function(x){
  x[sapply(x,is.null)]<-NA
  unlist(x)
})

#bind together as dataframe and remove NA's
df <- as.data.frame(do.call('rbind',json_file))
df<-na.omit(df)
df$content <- as.character(df$content)

#remove json_file
rm(json_file)

# Clean text for topic modeling and create DTM -------------------


#TODO: Fix so it reads in more than 1 document to corpus
#remove anything between  [ ] 
df$content <-gsub("\\[[^\\]]*\\]", "",df$content,perl = TRUE)
#remove html tags from content
df$content <- gsub("<.*?>", "", df$content, perl = TRUE)
#remove newline character
df$content <- gsub("[\r\n]", "", df$content, perl = TRUE)

#wtf.. I need to encode the corpus to UTF-8-MAC? .. Doesnt work on my ubuntu machine.. but works on mac
content<- VCorpus(VectorSource(df$content))
#Currently runs on 2 cores, specificy mc.cores param to adjust
content <- tm_map(content,content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), mc.cores=1)

content.clean <- tm_map(content, stripWhitespace)                                 #strip whitespace
content.clean <- tm_map(content.clean,removeNumbers)                              #remove numbers
content.clean <- tm_map(content.clean,removePunctuation)                          #remove punctuation
content.clean <- tm_map(content.clean,content_transformer(tolower))               #convert to lower case
content.clean <- tm_map(content.clean,removeWords,stopwords("english"))           #removestop words
content.clean <- tm_map(content.clean,stemDocument)                               #stem words

#create dtm
dtm <- DocumentTermMatrix(content.clean, control = list(weighting = weightTf))

#removing empty rows from dtm (i.e. empty docs)
#Keep 1-1 correspondance between dtm and corpus


rowsums <- apply(dtm,1,sum)
empty.rows <- dtm[rowsums == 0, ]$dimnames[1][[1]]
dtm.new <- dtm[rowsums > 0, ]
content.new <- content[-as.numeric(empty.rows)]
df.new<-df[-as.numeric(empty.rows),]


# Topic Modeling ----------------------------------------------------------

#TODO: optimize number of topics & discussion of trade off
num_topics = 5

#Start with lda we can build from here 
lda_object<- LDA(dtm.new,num_topics)

#CTM
#ctm_object<- CTM(dtm.new,num_topics)

#etc...

# Explore TM results (end of TM pipe) ------------------------------------------------------


#Look at top 10 words in each topic
terms(lda_object,10)

#look at top topics within first 5 documents
# look at the top topics within the first 5 documents
topics(lda_object, 5)[,1:5]


# cluster documents in topic space
document.topic.probabilities <- lda_object@gamma  # topic distribution for each document
topic.space.kmeans.clusters <- kmeans(document.topic.probabilities, num_topics)

table(topic.space.kmeans.clusters$cluster, df$`toptopics$topic`)


#Within cluster sum of squares
#i.e. distance from each point to centroid, want to minimize to acheive tight cluster
topic.space.kmeans.clusters$withinss



topic.space.clustered.news <- split(df.new, topic.space.kmeans.clusters$cluster)
topic.space.clustered.news[[1]][[1]]
topic.space.clustered.news[[1]][[2]]
topic.space.clustered.news[[1]][[3]]


#Generate viz --opens in browser 
vizme <- topicmodels_json_ldavis(lda_object,content.new,dtm.new)
serVis(vizme,out.dir = 'sample_data_viz')



# Assign topic labels + model subtopics -----------------------------------

#Assing topic names here
#topicnames <- c('')

gammaDF <- as.data.frame(lda_object@gamma) 
names(gammaDF) <- c(1:num_topics) #assign topic names here

toptopics <- as.data.frame(cbind(document = row.names(gammaDF), 
                                 topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))

#look at distribution of primary topic 
ggplot(data=toptopics,aes(x=topic)) + geom_bar(fill='#99c2ff',colour='black') + geom_text(stat='count',aes(label=..count..),vjust=-1)


#bind topic assignment to original df
df.new<-cbind(df.new,toptopics$topic)

#can now filter on a particular topic and topic model again to discover subtopics











