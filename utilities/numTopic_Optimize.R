

require(rjson)
#require(RJSONIO)
require(NLP)
require(tm)
require(topicmodels)
require(SnowballC)
require(LDAvis)



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


#Want to sample b/c to taxing to run on full data set
sample_size <- 1000
df<-df[sample(nrow(df), sample_size), ]


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


rowsums <- apply(dtm,1,sum)
empty.rows <- dtm[rowsums == 0, ]$dimnames[1][[1]]
dtm.new <- dtm[rowsums > 0, ]
content.new <- content[-as.numeric(empty.rows)]
df.new<-df[-as.numeric(empty.rows),]




max_nb_toics <- 40

result <- FindTopicsNumber(
  dtm.new,
  topics = seq(from = 2, to = max_nb_toics, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2,
  verbose = TRUE
)


#Plot and view results to make decision
FindTopicsNumber_plot(result)
