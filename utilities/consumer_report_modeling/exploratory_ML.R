
###
#Exploratory ML
###

library(caret)



#load data
setwd('~/Data')
load('consumer_reports_envr.RData')



# PreProcessing -----------------------------------------------------------
#Remove non-numeric columns
nums <- sapply(modelme, is.numeric)
modelme2<- modelme[,nums]

#Zero-fill NA values
modelme2[is.na(modelme2)] <- 0


#Look for variable with near zero variance
  #No variables are nzv...looks good here
nzv <- nearZeroVar(modelme2, saveMetrics= TRUE)


#Look at correlated predictors
  #bad for regression models good for other types of models
  #looks good here.. no variables with correlation > .75
descrCor <-  cor(modelme2)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)


#Look at variables with linear-dependence (i.e. co-linearlity)
  #bad for regression models
comboInfo <- findLinearCombos(modelme2)
comboInfo #suggests remove column 45 (i.e. NY_Pol5)
modelme2<-modelme2[, -comboInfo$remove]

#Center and scale (normalize) all variables
  #intercept term (in LR) will now be interpreted as the expected value of Y
preProcValues <- preProcess(modelme2, method = c("center", "scale"))
modelme2<- predict(preProcValues, modelme2)


#Filter out all articles where page views is greater than 3SDs away from mean
#basically removing outliers
modelme2 <- modelme2[!modelme2$Pageviews > (mean(modelme2$Pageviews) + 3*sd(modelme2$Pageviews)),]
modelme2 <- modelme2[!modelme2$Pageviews < (mean(modelme2$Pageviews) -  3*sd(modelme2$Pageviews)),]

# Modeling ----------------------------------------------------------------

###
#Page Views
###
regPageViews1<- lm(Pageviews~.,data=modelme2)
summary(regPageViews1)

#Step wise model
min.model <- lm(Pageviews ~ 1, data=modelme2)
biggest <- formula(lm(Pageviews~.,modelme2))
step(min.model, direction='forward', scope=biggest)

regPageViews2 <- lm( Pageviews ~ X..Exit + WordCount + flesh_score + 
                      NY_Pol2 + compound_score + int_pol4 + int_pol2 + Art1 + Music3 + 
                      int_pol5 + Culture1 + Startups2 + num_links_observer + US_Pol5 + 
                      NY_Pol1 + Music1 + Startups5 + US_Pol4 + Bounce.Rate + Music5 + 
                      Music4, data = modelme2)


summary(regPageViews2)
coefplot(regPageViews2)


###
#Avg Time on page
###
regAvgTime1<- lm(time~.,data=modelme2)
summary(regAvgTime1)

#Step wise model
min.model <- lm(time ~ 1, data=modelme2)
biggest <- formula(lm(time~.,modelme2))
step(min.model, direction='forward', scope=biggest)

regAvgTime2 <- lm(formula = time ~ WordCount + X..Exit + Bounce.Rate + compound_score + 
                     Pageviews + Culture2 + NY_Pol4 + int_pol4 + Art4 + Art3 + 
                     flesh_score + Music1 + Culture5 + int_pol3 + Music5 + Culture3 + 
                     neg_score + int_pol1 + NY_Pol2 + US_Pol4 + Startups2 + Startups3 + 
                     Music + int_pol5 + Art + Startups4 + Art2, data = modelme2)

summary(regAvgTime2)
coefplot(regAvgTime2)




