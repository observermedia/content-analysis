
###
#Exploratory ML
###

library(caret)
library(betareg)
library(coefplot)
#library(arm)
library(ggplot2)



#load data
setwd('~/Data')
load('consumer_reports_envr.RData')
modelme <- read.csv('model_dataframe.csv')
modelme <- modelme[,-c(1,7)]


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

exit <- modelme2$X..Exit
bounce <- modelme2$Bounce.Rate
preProcValues <- preProcess(modelme2, method = c("center", "scale"))
modelme2<- predict(preProcValues, modelme2)
modelme2$X..Exit <- exit
modelme2$Bounce.Rate <- bounce

#Filter out all articles where page views is greater than 3SDs away from mean
#basically removing outliers
modelme2 <- modelme2[!modelme2$X..Exit > (mean(modelme2$X..Exit) + 3*sd(modelme2$X..Exit)),]
modelme2 <- modelme2[!modelme2$X..Exit < (mean(modelme2$X..Exit) -  3*sd(modelme2$X..Exit)),]

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

regPageViews2 <- lm( Pageviews ~ X..Exit + time + neg_score + NY_Pol2 + 
                       flesh_score + WordCount + int_pol2 + `International politics` + 
                       Music3 + Art1 + Bounce.Rate + Culture1 + Startups2 + num_links_observer + 
                       NY_Pol1 + US_Pol5 + Music5 + US_Pol4 + Startups5 + Music1 + 
                       compound_score + int_pol3 + Music4, data = modelme2)


summary(regPageViews2)
coefplot(regPageViews2, "Page Views")


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
coefplot(regAvgTime2,title="Avg Time on Page")





###
#% Exit
###
#Full model
betareg <- betareg(modelme2$X..Exit ~ Pageviews                     
                   +Bounce.Rate                   
                   +Politics_B1                   
                   +Politics_B2                   
                   +Politics_B3                   
                   +Politics_B4                   
                   +Politics_B5                   
                   +Entertainment_Culture1        
                   +Entertainment_Culture2        
                   +Entertainment_Culture3        
                   +Entertainment_Culture4        
                   +Entertainment_Culture5        
                   +Business_tech_internetCulture1
                   +Business_tech_internetCulture2
                   +Business_tech_internetCulture3
                   +Business_tech_internetCulture4
                   +Business_tech_internetCulture5
                   +Politics_A1                   
                   +Politics_A2                   
                   +Politics_A3                   
                   +Politics_A4                   
                   +Politics_A5 
                   +Art_leisure1                  
                   +Art_leisure2                  
                   +Art_leisure3                  
                   +Art_leisure4
                   +politics_B                    
                   +Entertainment_Culture         
                   +Business_tech_internetCulture 
                   +Politics_A
                   +num_images                    
                   +num_links                     
                   +wiki_links                    
                   +pos_score                     
                   +neg_score                     
                   +neu_score                     
                   +compound_score                
                   +flesh_score                   
                   +WordCount                     
                   +WordCountTitle                
                   +time,
                   data=modelme2,link = "clogclog")
summary(betareg)
sapply(c("logit","probit","cloglog","cauchit","loglog"), 
       function(x) logLik(update(betareg,link=x)))

#Modeling after feature selection
betareg2 <- betareg(modelme2$X..Exit ~ Pageviews                     
                   +Bounce.Rate                   
                   +Entertainment_Culture2        
                   +Entertainment_Culture3        
                   #+Business_tech_internetCulture1
                   #+Business_tech_internetCulture2
                   +Entertainment_Culture         
                   #+Business_tech_internetCulture 
                   +Politics_A
                   +WordCount                     
                   +time,
                   data=modelme2,link = "cauchit")
summary(betareg2)
sapply(c("logit","probit","cloglog","cauchit","loglog"), 
       function(x) logLik(update(betareg2,link=x)))
plot(betareg2)
lrtest(betareg,betareg2)
coefplot.glm(betareg2)



###
#Bounce Rate
###

bouncereg<- betareg(modelme2$Bounce.Rate ~ Pageviews
                    +X..Exit                       
                    +Politics_B1                   
                    +Politics_B2                   
                    +Politics_B3                   
                    +Politics_B4                   
                    +Politics_B5                   
                    +Entertainment_Culture1        
                    +Entertainment_Culture2        
                    +Entertainment_Culture3        
                    +Entertainment_Culture4        
                    +Entertainment_Culture5        
                    +Business_tech_internetCulture1
                    +Business_tech_internetCulture2
                    +Business_tech_internetCulture3
                    +Business_tech_internetCulture4
                    +Business_tech_internetCulture5
                    +Politics_A1                   
                    +Politics_A2                   
                    +Politics_A3                   
                    +Politics_A4                   
                    +Politics_A5                   
                    +Art_leisure1                  
                    +Art_leisure2                  
                    +Art_leisure3                  
                    +Art_leisure4+politics_B                    
                    +Entertainment_Culture         
                    +Business_tech_internetCulture 
                    +Politics_A                    
                    +num_images                    
                    +num_links                     
                    +wiki_links                    
                    +pos_score                     
                    +neg_score                     
                    +neu_score                     
                    +compound_score                
                    +flesh_score                   
                    +WordCount                     
                    +WordCountTitle                
                    +time,
                    data=modelme2)
summary(bouncereg)
sapply(c("logit","probit","cloglog","cauchit","loglog"), 
       function(x) logLik(update(betareg2,link=x)))


bouncereg2 <- betareg(modelme2$Bounce.Rate ~ Pageviews
                                  +X..Exit                       
                                  +Politics_B5                   
                                  +Entertainment_Culture1        
                                  +Business_tech_internetCulture3
                                  +Politics_A3                  
                                  +Art_leisure2                  
                                  +Art_leisure3                  
                                  +time,
                                  data=modelme2)
summary(bouncereg2)
sapply(c("logit","probit","cloglog","cauchit","loglog"), 
       function(x) logLik(update(betareg2,link=x)))

lrtest(bouncereg,bouncereg2)
coef(betareg2)
