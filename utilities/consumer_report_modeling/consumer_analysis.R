###
#Consumer report analysis
###

#1)load data with links -> 2)clean -> 3)bind to gamma df -> 4)merge on link with consumer data -> 5)model




# 1) ----------------------------------------------------------------------


#Load data
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



#Load consumer data
y14 <- read.csv('Content Report - Top 2500 - 20140101-20141231.csv', skip=6)
y15 <- read.csv('Content Report - Top 2500 - 20150101-20151231.csv', skip=6)
y16 <- read.csv('Content Report - Top 2500 - 20160101-20160930.csv', skip=6)

consumer_reports<- rbind(y14,y15,y16)
consumer_reports<-consumer_reports[!duplicated(consumer_reports$Page), ]


# 2) ----------------------------------------------------------------------

df.new <- temp_df

# #remove anything between  [ ] 
# df$content <-gsub("\\[[^\\]]*\\]", "",df$content,perl = TRUE)
# #remove html tags from content
# df$content <- gsub("<.*?>", "", df$content, perl = TRUE)
# #remove newline character
# df$content <- gsub("[\r\n]", "", df$content, perl = TRUE)
# 
# #wtf.. I need to encode the corpus to UTF-8-MAC? .. Doesnt work on my ubuntu machine.. but works on mac
# content<- VCorpus(VectorSource(df$content))
# #Currently runs on 2 cores, specificy mc.cores param to adjust
# content <- tm_map(content,content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')), mc.cores=16)
# 
# content.clean <- tm_map(content, stripWhitespace)                                 #strip whitespace
# content.clean <- tm_map(content.clean,removeNumbers)                              #remove numbers
# content.clean <- tm_map(content.clean,removePunctuation)                          #remove punctuation
# content.clean <- tm_map(content.clean,content_transformer(tolower))               #convert to lower case
# content.clean <- tm_map(content.clean,removeWords,stopwords("english"))           #removestop words
# content.clean <- tm_map(content.clean,stemDocument)                               #stem words
# 
# #create dtm
# dtm <- DocumentTermMatrix(content.clean, control = list(weighting = weightTf))
# 
# 
# rowsums <- apply(dtm,1,sum)
# empty.rows <- dtm[rowsums == 0, ]$dimnames[1][[1]]
# dtm.new <- dtm[rowsums > 0, ]
# content.new <- content[-as.numeric(empty.rows)]
# df.new<-df[-as.numeric(empty.rows),]







# 3) ----------------------------------------------------------------------

bindme <- cbind(gammaDF,df.new)

bindme$Page<-gsub("http://observer.com", "", bindme$link ,perl = TRUE)

bindme<- merge(x=bindme,y=merge_tmp, by = "Page")



# 4) ----------------------------------------------------------------------

modelme<- merge(x=consumer_reports, y=bindme, by = "Page")
modelme$Pageviews <- as.numeric(gsub(",","",modelme$Pageviews))
modelme$WordCount <-sapply(gregexpr("\\W+", modelme$content), length) + 1
modelme$WordCountTitle <- sapply(gregexpr("-+", modelme$title), length) + 1


# 5)  ----------------------------------------------------------------------

#Start baisc regression models
y <- modelme$X..Exit
mean(y)
y <-ifelse(y >= .60, 1, 0)
#modelme <- modelme[,c(6:12,23:57)]


reg1<- glm(y~modelme$Culture+modelme$Music+modelme$Art+modelme$`International politics`+modelme$Startups+modelme$`US Politics`+modelme$`NY Politics`,family=binomial(link='logit'),data=modelme)
summary(reg1)



subset<-modelme[modelme$topic == 'Art',]
y <- subset$Bounce.Rate
mean(y)
y <-ifelse(y >= .675, 1, 0)
Art<- glm(y~subset$Art+subset$Art1 + subset$Art2 + subset$Art3 + subset$Art4 + subset$Art5 + subset$WordCount + subset$WordCountTitle,family=binomial(link='logit'),data=modelme)
summary(Art)


subset<-modelme[modelme$topic == 'Music',]
y <- subset$Bounce.Rate
mean(y)
y <-ifelse(y >= .675, 1, 0)
Music<- glm(y~subset$Music+subset$Music1 + subset$Music2 + subset$Music3 + subset$Music4 + subset$Music5 + subset$WordCount + subset$WordCountTitle,family=binomial(link='logit'),data=modelme)
summary(Music)


subset<-modelme[modelme$topic == 'Culture',]
y <- subset$Bounce.Rate
mean(y)
y <-ifelse(y >= .675, 1, 0)
Culture<- glm(y~subset$Culture+subset$Culture1 + subset$Culture2 + subset$Culture3 + subset$Culture4 + subset$Culture5 + subset$WordCount + subset$WordCountTitle,family=binomial(link='logit'),data=modelme)
summary(Culture)


subset<-modelme[modelme$topic == 'Startups',]
y <- subset$Bounce.Rate
mean(y)
y <-ifelse(y >= .775, 1, 0)
Startups<- glm(y~subset$Startups+subset$Startups1 + subset$Startups2 + subset$Startups3 + subset$Startups4 + subset$Startups5 + subset$WordCount + subset$WordCountTitle,family=binomial(link='logit'),data=modelme)
summary(Startups)



subset<-modelme[modelme$topic == 'US Politics',]
y <- subset$Bounce.Rate
mean(y)
y <-ifelse(y >= .775, 1, 0)
US_Pol<- glm(y~subset$`US Politics`+subset$US_Pol1 + subset$US_Pol2 + subset$US_Pol3 + subset$US_Pol4 + subset$US_Pol5 + subset$WordCount + subset$WordCountTitle,family=binomial(link='logit'),data=modelme)
summary(US_Pol)





# PageViews ---------------------------------------------------------------

y <- modelme$Pageviews
mean(y)
modelme <- modelme[,c(6:12,23:59)]


reg1<- lm(y~modelme$Culture+modelme$Music+modelme$Art+modelme$`International politics`+modelme$Startups+modelme$`US Politics`+modelme$`NY Politics` + modelme$WordCount + modelme$WordCountTitle)
summary(reg1)



subset<-modelme[modelme$topic == 'Art',]
y <- subset$Pageviews
mean(y)
Art<- lm(y~subset$Art+subset$Art1 + subset$Art2 + subset$Art3 + subset$Art4 + subset$Art5 + subset$WordCount + subset$WordCountTitle)
summary(Art)


subset<-modelme[modelme$topic == 'Music',]
y <- subset$Pageviews
mean(y)
Music<- lm(y~subset$Music+subset$Music1 + subset$Music2 + subset$Music3 + subset$Music4 + subset$Music5 + subset$WordCount + subset$WordCountTitle)
summary(Music)


subset<-modelme[modelme$topic == 'Culture',]
y <- subset$Pageviews
mean(y)
Culture<- glm(y~subset$Culture+subset$Culture1 + subset$Culture2 + subset$Culture3 + subset$Culture4 + subset$Culture5 + subset$WordCount + subset$WordCountTitle)
summary(Culture)


subset<-modelme[modelme$topic == 'Startups',]
y <- subset$Pageviews
mean(y)
Startups<- lm(y~subset$Startups+subset$Startups1 + subset$Startups2 + subset$Startups3 + subset$Startups4 + subset$Startups5 + subset$WordCount + subset$WordCountTitle)
summary(Startups)



subset<-modelme[modelme$topic == 'US Politics',]
y <- subset$Pageviews
mean(y)
US_Pol<- lm(y~subset$`US Politics`+subset$US_Pol1 + subset$US_Pol2 + subset$US_Pol3 + subset$US_Pol4 + subset$US_Pol5 + subset$WordCount + subset$WordCountTitle)
summary(US_Pol)




# Average time on page ----------------------------------------------------


time <- modelme$Avg..Time.on.Page

minute<- 0
second<-0
hr<-0
i<-1
for (i in 1:length(time)){
  minute[i]<-as.numeric(unlist(strsplit(as.character(time[i]), split=':', fixed=TRUE))[2])
  second[i]<-as.numeric(unlist(strsplit(as.character(time[i]), split=':', fixed=TRUE))[3])
}

modelme$time<- (minute*60) + second





y <- modelme$time
mean(y)

reg1<- lm(y~modelme$Culture+modelme$Music+modelme$Art+modelme$`International politics`+modelme$Startups+modelme$`US Politics`+modelme$`NY Politics` + modelme$WordCount + modelme$WordCountTitle)
summary(reg1)


subset<-modelme[modelme$topic == 'Art',]
y <- subset$time
mean(y)
Art<- lm(y~subset$Art+subset$Art1 + subset$Art2 + subset$Art3 + subset$Art4 + subset$Art5 + subset$WordCount + subset$WordCountTitle)
summary(Art)


subset<-modelme[modelme$topic == 'Music',]
y <- subset$time
mean(y)
Music<- lm(y~subset$Music+subset$Music1 + subset$Music2 + subset$Music3 + subset$Music4 + subset$Music5 + subset$WordCount + subset$WordCountTitle)
summary(Music)


subset<-modelme[modelme$topic == 'Culture',]
y <- subset$time
mean(y)
Culture<- glm(y~subset$Culture+subset$Culture1 + subset$Culture2 + subset$Culture3 + subset$Culture4 + subset$Culture5 + subset$WordCount + subset$WordCountTitle)
summary(Culture)


subset<-modelme[modelme$topic == 'Startups',]
y <- subset$time
mean(y)
Startups<- lm(y~subset$Startups+subset$Startups1 + subset$Startups2 + subset$Startups3 + subset$Startups4 + subset$Startups5 + subset$WordCount + subset$WordCountTitle)
summary(Startups)



subset<-modelme[modelme$topic == 'US Politics',]
y <- subset$time
mean(y)
US_Pol<- lm(y~subset$`US Politics`+subset$US_Pol1 + subset$US_Pol2 + subset$US_Pol3 + subset$US_Pol4 + subset$US_Pol5 + subset$WordCount + subset$WordCountTitle)
summary(US_Pol)




