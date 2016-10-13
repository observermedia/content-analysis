

library(topicmodels)

harmonicMean <- function(logLikelihoods, precision=2000L) {
  library("Rmpfr")
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

# Using this if method = Gibbs
#k = 20
#burnin = 1000
#iter = 1000
#keep = 50

# generate numerous topic models with different numbers of topics
sequ <- seq(2, 25, 1) # in this case a sequence of numbers from 1 to 50, by ones.
fitted_many <- lapply(sequ, function(k) LDA(dtm.new, k = k))


# extract logliks from each topic 
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

# inspect
plot(sequ, hm_many, type = "l")

# compute optimum number of topics
sequ[which.max(hm_many)]
