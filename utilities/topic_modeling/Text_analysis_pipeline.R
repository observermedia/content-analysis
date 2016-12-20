###
#Text analysis pipeline
###

#Load data -> clean -> topic modeling -> output results and viz

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

json_file <- fromJSON(file='clean_data_links.json') 
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

mystops<-read.csv('mystops.csv')
mystops<- mystops$a
# Clean text for topic modeling and create DTM -------------------

#uncomment below line if want to downsample to increase algorithm convergence time
#df<- df[sample(nrow(df), 4500), ]

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
content.clean <- tm_map(content,removeNumbers)                                    #remove numbers
content.clean <- tm_map(content.clean,removePunctuation)                          #remove punctuation
content.clean <- tm_map(content.clean,content_transformer(tolower))               #convert to lower case
content.clean <- tm_map(content.clean,removeWords,mystops)                        #removestop words
content.clean <- tm_map(content.clean,stemDocument)                               #stem words
content.clean <- tm_map(content.clean, stripWhitespace)                           #strip whitespace

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

num_topics <- 5
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


# Generate Interactive viz ------------------------------------------------
#Generate viz --opens in browser 
vizme <- topicmodels_json_ldavis(lda_object,content,dtm)
serVis(vizme,out.dir = 'viz_5_Primarytopics')



# Assign topic labels + model subtopics -----------------------------------

#Assing topic names here
topicnames <- c("Politics_B","Entertainment_Culture","Business_tech_internetCulture","Politics_A","Art_leisure")


gammaDF <- as.data.frame(lda_object@gamma) 
names(gammaDF) <- c(topicnames) #assign topic names here

toptopics <- as.data.frame(cbind(document = row.names(gammaDF), 
                                 topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))

#look at distribution of primary topic 
ggplot(data=toptopics,aes(x=topic)) + geom_bar(fill='#99c2ff',colour='black') + geom_text(stat='count',aes(label=..count..),vjust=-1)


#bind topic assignment to original df
df.new<-cbind(df.new,toptopics$topic)
df.new$topic <- df.new$'toptopics$topic'

df<-cbind(df.new,gammaDF)
temp_df<-cbind(temp_df,gammaDF)

#can now filter on a particular topic and topic model again to discover subtopics









