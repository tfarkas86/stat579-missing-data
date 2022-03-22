####################Chapter 7 Sensitivity Analysis####################################################
#(1) Perform sensitivity analysis on the mammalsleep dataset by adding and subtracting 
#some amount from the imputed values for variable sws.
#Use delta <- c(8, 6, 4, 2, 0, -2, -4, -6, -8)
#(2) investigate the influence on the regression model:  lm(sws ~ log10(bw) + odi)
#######################################################################

library("mice")
library("lattice")

##data
data(mammalsleep)
dim(mammalsleep)
nrow(mammalsleep)  #62
head(mammalsleep)
# bw: Body weight (kg)
# sws: Slow wave ("nondreaming") sleep (hrs/day)
# odi: Overall danger index (1-5) based on the above two indices and other information, 
# 1 = least danger (from other animals), 5 = most danger (from other animals)
summary(mammalsleep$sws)

## Summarize missing data patterns
md.pattern(mammalsleep)
#   1st column: number of rows with that response patt.
#   Last colmn: number of missing entries in that patt.
#   Last row: number of rows with missingness in that col.

fluxplot(mammalsleep)
# Variables with higher outflux are (potentially) the more powerful predictors. 
# Variables with higher influx depend stronger on the imputation model. 
# When points are relatively close to the diagonal, it indicates that influx 
# and outflux are balanced.

# The variables in the upper left corner have the more complete information, 
# so the number of missing data problems for this group is relatively small. 

# The variables in the middle (if there are) 
# Missing data problems are more severe, but potentially this group could 
# also contain important variables. 

#The lower (bottom) variables have an outflux 
# less than 0.4, so their predictive power is limited. Also, this group has
# a relatively higher influx, and, thus, depend more highly on the imputation model.


## Create a \delta vector that represents the following adjustment values  
#   0 for MAR, and others for MNAR.
delta <- c(8, 6, 4, 2, 0, -2, -4, -6, -8)

#Perform a dry run (using maxit = 0) in mice. List the number of missing values per variable.
ini <- mice(mammalsleep[, -1], maxit=0, print=F)
ini$nmis
meth<- ini$meth
meth   #pmm: Predictive mean matching, all variables with missing data are continuous
meth["ts"]<- "~ I(sws + ps)"
meth
pred <- ini$pred
pred
pred[c("sws", "ps"), "ts"] <- 0
pred
post <- ini$post
post
imp.all <- vector("list", length(delta))

#The recipe for creating MNAR imputations for $\delta \neq 0$ uses the 
# post-processing facility of mice. This allows to change the imputations 
#by deducting a value of $\delta$ from the values just imputed.
#m=5 is the default of MI, you can omit it
for (i in 1:length(delta)) {
  d <- delta[i]
  cmd <- paste("imp[[j]][, i] <- imp[[j]][, i] +", d)
  post["sws"] <- cmd
  imp <- mice(mammalsleep[, -1], m = 5, meth=meth, pred=pred, post = post, maxit = 10, 
              seed = i * 22, print=FALSE)
  imp.all[[i]] <- imp
}

#inspect the imputations
#delta <- c(8, 6, 4, 2, 0, -2, -4, -6, -8), total of 9 objects in the list
#delta =0,  the fifth object from the list
complete(imp.all[[5]])
bwplot(imp.all[[5]])
#delta=-8, the ninth object from the list
# We can see that the adjustment has an effect on the imputations for
# sws and, thus, on those for example, ts, mis.
bwplot(imp.all[[9]])

#xyplot

xyplot(imp.all[[5]],mls~sws | .imp)
xyplot(imp.all[[5]],ts~mls | .imp)
xyplot(imp.all[[5]],sws~log10(bw) | .imp)

xyplot(imp.all[[9]],sws~log10(bw) | .imp)

xyplot(imp.all[[5]],sws~odi | .imp)

xyplot(imp.all[[9]],sws~odi | .imp)

#fit regression models
# The fit object contains the five imputed regression models for 
# the adjustment scenario at hand. For example, the \delta=8 scenario is contained in fit1.
fit1 <- with(imp.all[[1]], lm(sws ~ log10(bw) + odi))
fit1
summary(fit1)
pool_fit<-pool(fit1)
class(pool_fit)
summary(pool_fit)

# Fit the 9 adjustment scenario together
output <- sapply(imp.all, function(x) summary(pool(with(x, lm(sws ~ log10(bw) + odi)))))
cbind(delta, as.data.frame(t(output)))

#The estimates for different \delta are not close. 
# A clear trend for the estimates for the intercept and for bw emerges.
# Thus, the results are not essentially the 
# same under all specified mechanisms and the outcomes can be deemed sensitive to the assumed mechanism.

# but we also notice that \delta = 8 is too large compared to mean of sws 8.673. 
# Choosing unreasonably large values may always influence your estimates. Therefore; 
# choosing values that are reasonable given your suspicions of an assumed breach 
# of the MAR assumption is vital.



