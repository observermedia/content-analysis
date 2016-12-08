


sapply(c("logit","probit","cloglog","cauchit","loglog"), 
       function(x) logLik(update(MODEL,link=x)))





m1 = lm(mpg ~ wt + cyl + carb, data=mtcars)
coefs = as.data.frame(summary(m1)$coefficients[-1,1:2])
names(coefs)[2] = "se" 
coefs$vars = rownames(coefs)



ggplot(coefs, aes(vars, Estimate)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se), 
                lwd=1, colour="red", width=0) +
  geom_errorbar(aes(ymin=Estimate - se, ymax=Estimate + se), 
                lwd=2.5, colour="blue", width=0) +
  geom_point(size=4, pch=21, fill="yellow") +
  theme_bw()





coefs <- as.data.frame(betareg2$coefficients$mean)
coefs<-cbind(coefs,c(0.12575,0.01096,0.16934,0.01205,0.01121,0.01425,0.01139,0.01193,0.01178))
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
  coord_flip()
