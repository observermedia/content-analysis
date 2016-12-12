#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

###
#Topic model production
#input: nb_topics (for LDA)
#output: (Interactive topic viz, topic distrubiton plot, .csv with topic probability scores and primary topic)
###

# Packages and Functions----------------------------------------------------------------
if (!require(rjson)){
  install.packages(rjson)
}
if (!require(NLP)){
  install.packages(NLP)
}
if (!require(tm)){
  install.packages(tm)
}

if (!require(topicmodels)){
  install.packages(topicmodels)
}
if (!require(SnowballC)){
  install.packages(SnowballC)
}
if (!require(LDAvis)){
  install.packages(LDAvis)
}

topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  # Required packages
  if (!require(topicmodels)){
    install.packages(topicmodels)
  }
  if (!require(dplyr)){
    install.packages(dplyr)
  }
  if (!require(stringi)){
    install.packages(stringi)
  }
  if (!require(tm)){
    install.packages(tm)
  }
  if (!require(LDAvis)){
    install.packages(LDAvis)
  }
  
  # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  temp_frequency <- inspect(doc_term)
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  rm(temp_frequency)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
  #vizme <- topicmodels_json_ldavis(lda_object,content.new,dtm.new)
  #serVis(vizme)
}

# Load Data and global variables---------------------------------------------------------------

nb_topics <- as.numeric(args[1])

json_file <- fromJSON(file='clean_data_links.json') 
json_file <- lapply(json_file,function(x){
  x[sapply(x,is.null)]<-NA
  unlist(x)
})

#bind together as dataframe and remove NA's
df <- as.data.frame(do.call('rbind',json_file))
df<-na.omit(df)
df$content <- as.character(df$content)

#remove json_file (clean up some memory)
rm(json_file)

mystops<-read.csv('mystops.csv')
mystops<- mystops$a
# Clean text for topic modeling and create DTM -------------------

#remove anything between  [ ] 
df$content <-gsub("\\[[^\\]]*\\]", "",df$content,perl = TRUE)
#remove html tags from content
df$content <- gsub("<.*?>", "", df$content, perl = TRUE)
#remove newline character
df$content <- gsub("[\r\n]", "", df$content, perl = TRUE)

content<- VCorpus(VectorSource(df$content))
#Currently runs on 1 cores, specificy mc.cores param to adjust
#depending on OS, need to change encoding from 'UTF-8-MAC' for linux use just 'UTF-8'
#wtf.. I need to encode the corpus to UTF-8-MAC? .. Doesnt work on my ubuntu machine.. but works on mac
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
lda_object<- LDA(dtm.new,nb_topics)

#CTM
#ctm_object<- CTM(dtm.new,num_topics)


# Explore TM results in viz ------------------------------------------------------

vizme <- topicmodels_json_ldavis(lda_object,content.new,dtm.new)
serVis(vizme,out.dir = 'viz_topics',open.browser = FALSE)

# Assign topic labels + model subtopics -----------------------------------
topicnames <- c(1:nb_topics)

gammaDF <- as.data.frame(lda_object@gamma) 
names(gammaDF) <- c(topicnames) #assign topic names here

toptopics <- as.data.frame(cbind(document = row.names(gammaDF), 
                                 topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))

#look at distribution of primary topic 
ggplot(data=toptopics,aes(x=topic)) + geom_bar(fill='#99c2ff',colour='black') + geom_text(stat='count',aes(label=..count..),vjust=-1)
ggsave("topic_distribution.pdf")

#bind topic assignment to original df
df.new<-cbind(df.new,toptopics$topic)
df.new$topic <- df.new$'toptopics$topic'
df.new <- subset(df, select=-c('toptopics$topic'))

#write dataframe final as csv
write.csv(df.new,'topicmodel_output.csv')





