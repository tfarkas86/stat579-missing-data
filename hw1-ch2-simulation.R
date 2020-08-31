#### Stat 579: Statistical Analysis With Missing Data
#### R handout 1

## - Simulation excploration for mean imputation

## (1)Distortion of associations
## (2)Overconfidence in results

# Clean your environment
rm(list=ls())

# Set a seed so you can reproduce all the results
set.seed(1345)


## First, do one run of the simulation: generate one dataset and compute estimates we want

# generating fully observed data (x_1, x_2, y), then make some y numbers missing (MCAR)
# linear relationship: E(y) = x_1 + 2 x_2 

# Sample size
n <- 10
# x1 
x1 <- rnorm(n)
x1
# x2|x1 
pbeta1 <- c(0,0)
x2 <- rnorm(n, mean = pbeta1[1] + pbeta1[2]*x1)
x2
# y|(x1,x2)
pbeta2 <- c(3,1,2)
y <- rnorm(n, mean = pbeta2[1] + pbeta2[2]*x1 + pbeta2[3]*x2)
y

# The full data 
full_data <- data.frame(x1=x1, x2=x2, y=y)
full_data
pairs(full_data)

# Now generate missing data under MCAR,
#  response indicators are independent with prob of missingness p_mis

obs_data <- full_data

p_mis <- .2
set.seed(1345)
runif(n)
set.seed(1345)
mis_ind <- rep(runif(n) < p_mis)
mis_ind

obs_data[,3][mis_ind] <- NA
obs_data

# Estimated means with observed data in each column
mu1 <- colMeans(obs_data, na.rm=TRUE)

# Mean imputation

imp_data <- mapply(
  function(x, y){
    x[is.na(x)] <- y 
    return(x)
  }, 
  obs_data, mu1)

class(imp_data)  #matrix, need to change to data frame

imp_data <- as.data.frame(imp_data)

# Regression coefficients estimation

# True values
pbeta

# Regress y on x1 and x2 based on the imputed data
lm_imp<- lm(y ~ x1 + x2, data=imp_data)
summary(lm_imp)
# Coefficients estimated based on imputed data
lm_imp$coefficients


# Regress y on x1 and x2 based on the complete cases
lm_cc <- lm(y ~ x1 + x2, data=obs_data) 
summary(lm_cc)
# same as using data=obs_data[complete.cases(obs_data),]
# Coefficients estimated based on complete cases
lm_cc$coefficients

# How about means?

# Mean imputation estimator of the mean of y, mu_mimp
mean(imp_data$y)
mean(full_data$y)
mean(obs_data$y,na.rm=TRUE)

# And its naively estimated variance, var_naive_mimp
var(imp_data$y)/n

# Compare with estimated variance based on observed values
var(obs_data$y, na.rm=TRUE)/sum(!is.na(obs_data$y))

# But these are results for a single realization of the data.
# We care about properties of these estimators over repeated sampling!

###########################################################################################
###########################################################################################
## Start with simulation study
rm(list=ls())

##some parameters that my change, set them as variables in a function
#Recall that
# Sample size
n <- 1000
# Number of repetitions
r <- 1000
# True betas 
pbeta <- c(3, 1, 2)
# Missing data generated under MCAR, 
pmis <- .2

#define a simulation function with parameters n, pbeta, pmis, r

simufun <- function(n,pbeta,pmis,r){

# Matrices to store simulation results:
#  - three betas under mean imputation, three betas from complete cases
results1 <- matrix(NA,r,6)
#  - mean of x3, naive variance of mean, complete case var of mean
results2 <- matrix(NA,r,3)


colnames(results1) <- c("beta0_imp","beta1_imp","beta2_imp",
                                  "beta0_cc","beta1_cc","beta2_cc")

colnames(results2) <- c("mu_mimp","var_naive_mimp","var_ac")

#loop
for(i in 1:r){
  #first generate a sample
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- rnorm(n, mean = pbeta[1] + pbeta[2]*x1 + pbeta[3]*x2)
  
  # The full data
  full_data <- data.frame(x1=x1, x2=x2, y=y)
  
  # Now generate missing data under MCAR, so we have observed data with missing obsns
  obs_data <- full_data
  mis_inds <- runif(n) < pmis
  obs_data[,3][mis_inds] <- NA
  
  # Estimated means with observed data in each column
  mu1 <- colMeans(obs_data, na.rm=TRUE)
  
  # Mean imputation, now we also have imputed full data
  imp_data <- mapply(
    function(x, y){
      x[is.na(x)] <- y 
      return(x)
    }, 
    obs_data, mu1)
  
  
  imp_data <- as.data.frame(imp_data)
  
  # Regress y on x1 and x2 based on the imputed data
  lm_imp <- lm(y ~ x1 + x2, data=imp_data)
  
  # Regress y on x1 and x2 based on the complete cases
  lm_cc <- lm(y ~ x1 + x2, data=obs_data)
  
  # Store beta estimates based on imputed data and complete cases
  results1[i,] <- c(lm_imp$coefficients, lm_cc$coefficients)
  
  # Mean imputation estimator of the mean of y
  mu_mimp <- mean(imp_data$y)
  
  # And its naively estimated variance
  var_naive_mimp <- var(imp_data$y)/n
  
  # Compare with estimated variance based on obs values
  var_ac <- var(obs_data$y, na.rm=TRUE)/sum(!is.na(obs_data$y))
  
  results2[i,] <- c(mu_mimp,var_naive_mimp,var_ac)
  }

meanresults1<-colMeans(results1)
meanresults2<-colMeans(results2)

list(meanresults1=meanresults1,meanresults2=meanresults2)
}

##now we are done with simulation function, next run simulations with specified parameters, for example
# Sample size
n <- 1000
# Number of repetitions
r <- 1000
# True betas 
pbeta <- c(3, 1, 2)
# Missing data generated under MCAR, 
pmis <- .2

simufun(n,pbeta,pmis,r)


