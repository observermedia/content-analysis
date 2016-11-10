

modelme$Pageviews <- as.numeric(gsub(",","",modelme$Pageviews))
modelme$WordCount <-sapply(gregexpr("\\W+", modelme$content), length) + 1
modelme$WordCountTitle <- sapply(gregexpr("-+", modelme$title), length) + 1


export <- modelme[,-c(13,14,15,16,17)]
export<- export[,-c(14,15,16)]

export$ldaTopic<- export$topic
export<-export[,-14]

write.csv(export,'topic_reports.csv')



#finding correlations between consumer metrics and content wc
cor.test(modelme$X..Exit,modelme$WordCount)
cor.test(modelme$Bounce.Rate,modelme$WordCount)
cor.test(modelme$Pageviews,modelme$WordCount)
cor.test(modelme$time,modelme$WordCount)



#finding correlations between consumer metrics and title wc
cor.test(modelme$X..Exit,modelme$WordCountTitle)
cor.test(modelme$Bounce.Rate,modelme$WordCountTitle)
cor.test(modelme$Pageviews,modelme$WordCountTitle)
cor.test(modelme$time,modelme$WordCountTitle)

library(corrplot)
M <- cbind(modelme$X..Exit,modelme$Bounce.Rate,modelme$Pageviews,modelme$time,modelme$WordCount,modelme$WordCountTitle)
colnames(M) <- c("% Exit","Bounce Rate","Page Views","Avg Time","WC","WC Title")
M <- cor(M)
corrplot(M, method="circle")






lm<-lm(modelme$Bounce.Rate~modelme$WordCount)
summary(lm)






d<-density(modelme$time)
plot(d)
