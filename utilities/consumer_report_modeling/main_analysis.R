###
#Exploratory ML
###
library(caret)
library(betareg)
library(coefplot)
#library(arm)
library(ggplot2)



#load data
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
comboInfo
modelme2<-modelme2[, -comboInfo$remove]

#Center and scale (normalize) all variables
  #intercept term (in LR) will now be interpreted as the expected value of Y
exit <- modelme2$X..Exit
bounce <- modelme2$Bounce.Rate
preProcValues <- preProcess(modelme2, method = c("center", "scale"))
modelme2<- predict(preProcValues, modelme2)

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

regPageViews2 <- lm( Pageviews ~ Politics_A + WordCount + Business_tech_internetCulture4 + 
                       Politics_A3 + Politics_A4 + X..Exit + Bounce.Rate + 
                       Business_tech_internetCulture2 + neg_score + Politics_A1, 
                     data = modelme2)

summary(regPageViews2)
coefplot(regPageViews2, "Page Views",intercept = FALSE)

###
#Avg Time on page
###
regAvgTime1<- lm(time~.,data=modelme2)
summary(regAvgTime1)

#Step wise model
min.model <- lm(time ~ 1, data=modelme2)
biggest <- formula(lm(time~.,modelme2))
step(min.model, direction='forward', scope=biggest)

regAvgTime2 <- lm(formula = time ~ WordCount + Business_tech_internetCulture + 
                    X..Exit + Bounce.Rate + politics_B + num_links + Politics_B2 + 
                    Art_leisure2 + neg_score + Business_tech_internetCulture5 + 
                    flesh_score + Politics_B4 + Art_leisure5 + Entertainment_Culture2 + 
                    Entertainment_Culture5 + Entertainment_Culture4 + Politics_A3 + 
                    Art_leisure3 + Politics_A4, data = modelme2)

summary(regAvgTime2)
coefplot(regAvgTime2,title="Avg Time on Page",intercept=FALSE)


###
#% Exit
###

#need none scaled version of reponse variable to fit into beta distribution
modelme2$X..Exit <- exit
#Full model
exitreg <- betareg(modelme2$X..Exit ~ Pageviews                     
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
summary(exitreg)
sapply(c("logit","probit","cloglog","cauchit","loglog"), 
       function(x) logLik(update(exitreg,link=x)))

#Modeling after feature selection
exitreg2 <- betareg(modelme2$X..Exit ~ Pageviews                     
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
summary(exitreg2)
plot(exitreg2) #diagnostic plots
sapply(c("logit","probit","cloglog","cauchit","loglog"), 
       function(x) logLik(update(exitreg2,link=x)))


#plot
coefs <- as.data.frame(exitreg2$coefficients$mean[-1])
#I have to manually enter S.E's i cant extract them from this betareg datatype
coefs<-cbind(coefs,c(0.01096, 0.01156, 0.01205, 0.01121, 0.01425, 0.01139, 0.01193, 0.01178))
colnames(coefs) <- c("Estimate","se")
coefs$vars <- rownames(coefs)

ggplot(coefs, aes(vars, Estimate)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se), 
                lwd=1, colour="red", width=0) +
  geom_errorbar(aes(ymin=Estimate - se, ymax=Estimate + se), 
                lwd=2.5, colour="blue", width=0) +
  geom_point(size=4, pch=21, fill="yellow") +
  theme_bw() +
  labs(title='Beta Regression Coefs: % Exit') +
  coord_flip()

###
#Bounce Rate
###

#rescale %exit and unscale bounce rate
preProcValues <- preProcess(modelme2, method = c("center", "scale"))
modelme2<- predict(preProcValues, modelme2)
modelme2$Bounce.Rate <- bounce

#full Model
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
       function(x) logLik(update(exitreg2,link=x)))



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
       function(x) logLik(update(exitreg2,link=x)))

#Test liklihood for significant difference between two models
#smaller model with non-statistically different LH is better
lrtest(bouncereg,bouncereg2)

coefs <- as.data.frame(bouncereg2$coefficients$mean[-1])
#I have to manually enter S.E's i cant extract them from this betareg datatype
coefs<-cbind(coefs,c(0.009287,0.009913,0.010929,0.009515,0.009282,0.009448,0.009641,0.008910,0.009693))
colnames(coefs) <- c("Estimate","se")
coefs$vars <- rownames(coefs)

ggplot(coefs, aes(vars, Estimate)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se), 
                lwd=1, colour="red", width=0) +
  geom_errorbar(aes(ymin=Estimate - se, ymax=Estimate + se), 
                lwd=2.5, colour="blue", width=0) +
  geom_point(size=4, pch=21, fill="yellow") +
  theme_bw() +
  labs(title='Beta Regression Coefs: Bounce Rate') +
  coord_flip()


