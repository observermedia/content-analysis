


modelme <- read.csv('model_dataframe.csv')


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

#Correlation plots
M <- cbind(modelme$X..Exit,modelme$Bounce.Rate,modelme$Pageviews,modelme$time,modelme$WordCount,modelme$WordCountTitle)
colnames(M) <- c("% Exit","Bounce Rate","Page Views","Avg Time","WC","WC Title")
M <- cor(M)
corrplot(M, method="circle")


