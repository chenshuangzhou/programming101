################ TEST ############################

setwd("C:/Users/Kevin/Desktop/test")

x = c("ROCR","boot",                            # session 2
      "np","rdrobust",                          # session 3
      "survival","Hmisc",                       # session 4
      "lme4","lattice","ICC","lmerTest",        # session 5
      "geepack","doBy","MESS","lme4",           # session 6
      "lasso2","glmnet","ridge",                # session 7
      "ipw","survey","survival")                # session 8

lapply(x, install.packages, character.only = TRUE)
lapply(x, require, character.only = TRUE)


########## CMED6040 Lecture #####

data <- read.csv("http://web.hku.hk/~ehylau/sars08.csv")

## Log-likelihood models, theta

# setting log-likelihood link dropping the constant term: 60 out of 568 have certain outcome (60 vs. 508)
    # example 1 - statistics 
logL <- function(theta){60*log(theta) + 508*log(1-theta)}
curve(logL, from=0, to=0.25)
        # Find the maximum log likelihood value
optim(0.1, logL, method="CG", control=list(fnscale=-1))     # by default, fnscale indicates minimum, whereas -1 indicates maximum
optim(0.1, logL, method="Brent", lower=0.01, upper=0.25, control=list(fnscale=-1))
        # alternatively, you may define a new function -logL for minimization

    # example 2 - making graph
logL.exact <- function(theta){dbinom(60, 568, theta, log=T)}
curve(logL.exact, from=0, to=0.25, ylim=c(-50,0), las=1, 
xlab=expression(paste('Prevalence (',theta,')')), ylab='log(L)')

    # Example 3 - Question 1c
logL.pois <- function(lambda){330*log(lambda) - 720100*lambda}
curve(logL.pois, from=0, to=0.001)
optim(0.0001, logL.pois, method="Brent", lower=0.00001, upper=0.001, control=list(fnscale=-1))

## Bootstrapping
# Area Under ROC curve (AUROC)
    library(ROCR)
    # prediction(scores, true values) # where true values should be vector of 0s and 1s
    # performance(prediction.object, measure="auc")
    # performance.object@y.values[[1]]

sars <- read.csv("http://web.hku.hk/~ehylau/sars08.csv") 
    sars.glm <- glm  (case ~ age + male + fever + sorethroat + cough, data=sars, family=binomial)
    sars$pred <- predict(sars.glm, type="response")     # type=response gives the predicted probabilities
    pred.obj <- prediction(sars$pred, sars$case)        # prediction(scores, true value)
    perf.obj <- performance(pred.obj, measure="auc")    
    auroc <- perf.obj@y.values[[1]] 

# bootstrapping - non-parametric bootstrap
mvc <- read.csv("http://web.hku.hk/~ehylau/mvc.csv") 
lm.mvc <- lm(height ~ age, mvc) 
summary(lm.mvc)
confint(lm.mvc)[2, ]        # 95% CI for second variable

    # calculate standard error of beta 
bootlm <- function (m, original.data){  
    beta <- rep (NA, m)  
    for (i in 1:m){
        newdata <- original.data[sample(1:41, 41, replace=TRUE), ]    
        b.lm <- lm(height ~ age, data=newdata)    
        beta[i] <- coef(b.lm)[2]d}   
    return(beta)
} 

b.beta <- bootlm(1000, original.data=mvc ) 
mean(b.beta) # bootstrapped mean of beta
sd(  b.beta) # bootstrapped standard error of the estimate of the mean
quantile(b.beta, c(0.025, 0.975)) # 95% CI of mean

# bootstrapping - residual bootstrap
lm.mvc <- lm(height ~ age, mvc)
mvc$residual <- resid(lm.mvc)
mvc$height.pred <- predict(lm.mvc)
resbootlm <- function (m, original.data){
    beta <- rep (NA, m)
    for (i in 1:m){
        b.resid <- original.data$residual[sample(1:41, 41, replace=TRUE)]
        newdata <- original.data
        newdata$height.b <- newdata$height.pred + b.resid
        b.lm <- lm(height.b ~ age, data=newdata)
        beta[i] <- coef(b.lm)[2]
        }
    return(beta)
}

resb.beta <- resbootlm(1000, original.data=mvc)
mean(resb.beta) # residual bootstrapped mean of beta
sd(resb.beta) # residual bootstrapped standard error of the
estimate of the mean
quantile(resb.beta, c(0.025, 0.975)) # 95% CI of mean

plot(density(resb.beta, bw=0.05), 
main = "Density plot from 1000 bootstrap estimates", xlab = "Bootstrap estimate",
cex.lab=1.2, las=1)

# bootstrapping - parametric bootstrap

# Confidence Interval for AUROC
b.auroc <- rep(NA,1000)
for (i in 1:1000){
    sars.new <- sars[sample(1:800, 800, replace=TRUE),]
    b.pred.obj <- prediction(sars.new$pred, sars.new$case)
    b.perf.obj <- performance(b.pred.obj, measure="auc")
    b.auroc[i] <- b.perf.obj@y.values[[1]]
}
mean(b.auroc)
quantile(b.auroc, c(0.025,0.975))

## "Boot" application - MMVC example
library(boot)   # boot(data, statistic, R)
    # example 1
mvclm.out <- function (data, indices){
    newdata <- data[indices, ]
    b.lm <- lm(height ~ age, data=newdata)
    return(coef(b.lm)["age"])
}

mvc.b.out <- boot(data=mvc, statistic=mvclm.out, R=1000)
boot.ci(mvc.b.out)

    # example 2
auroc.b.out <- function (data, indices){   
    sars.new <- sars[indices,]   
    sars.glm <- glm (case ~ age + male + fever + sorethroat + cough, data=sars.new, family=binomial)   
    sars.new$pred <- predict(sars.glm, type="response")  
    b.pred.obj <- prediction(sars.new$pred, sars.new$case)   
    b.perf.obj <- performance(b.pred.obj, measure="auc")   
    return(b.perf.obj@y.values[[1]])  
} 

sars.b.out <- boot(data=sars, statistic=auroc.b.out, R=1000) 
boot.ci(sars.b.out)

# Simulation of normal distribution for bootstrap
sample <- rnorm(64, 2, 1)
mean(sample)
var(sample)


## P-value calculation 
    # Method I: Direct calculation of test statistic , then p-value.
        # Assume known population variance
        teststat <- (mean(sample)-0)/(1/sqrt(64))
        pvalue <- pnorm(-abs(teststat), mean=0, sd=1, lower.tail=T) + pnorm(abs(teststat), mean=0, sd=1, lower.tail=F)

        # Assume population variance unknown
        test <- t.test(sample,mu=0) # Ho: mu=0
        pvalue <- test$p.value

    # Method II: Simulate the sampling distribution of "estimate of population mean",
        # then check for percentile of observed mean
        reference <- rnorm(10000, 0, 1/sqrt(64))
        mean(sample) > max(reference) # mean(sample) is even larger than max(reference)

    # Method III: Simulate samples of size 64 from N(0,1) [the hypothesized distribution of data] 
        # and count how often mean is more extreme than the "observed mean"
        count <- 0
        for (i in 1:1000) {
            hypo <- rnorm(64,0,1)
            if (mean(hypo) >= mean(sample)) count <- count + 1
        }
        count/1000


# Bootstrap method for non-normal estimate
# The weight and height of 100 healthy men are stored in the file “BMI.csv”
bmi <- read.csv("http://web.hku.hk/~ehylau/BMI.csv")
bmi$bmi <- bmi$weight/bmi$height^2

# Calculate the BMI of the 100 healthy men and plot the distribution of weight, height and BMI in a panel.
par(mfrow=c(3,1))
plot(density(bmi$height))
plot(density(bmi$weight))
plot(density(bmi$bmi))

# Assess the normality of the three variables (weight, height and BMI).
    # Test normality
    # (Google Search: Test normality, R) or ??normality
    # Shapiro-Wilk Normality Test - shapiro.test 

    shapiro.test(bmi$height)
    shapiro.test(bmi$weight)
    shapiro.test(bmi$bmi)   # p=0.81, 0.53, 0.04 respectively


# Calculate the bootstrap estimate of the mean and the corresponding 95% percentile bootstrap confidence interval for BMI, from 1000 bootstrap samples
b.bmi <- rep(NA,1000)
for (i in 1:1000){
    bmi.new <- bmi[sample(1:100, 100, replace=TRUE),]
    b.bmi[i] <- mean(bmi.new$weight/bmi.new$height^2)
}

mean(b.bmi)
quantile(b.bmi, c(0.025, 0.975))

# Obtained the 95% BCa confidence interval for BMI.
library(boot)

bmi.f <- function(data, indices){
	bmi.new <- bmi[indices,]
	return(mean(bmi.new$bmi))
}

b.bmi.out <- boot(data=bmi, statistic=bmi.f, R=1000)
boot.ci(b.bmi.out)




### Regression Discontinuity Design (RDD) - library(np)
alc <- read.csv('examplealcohol.csv')
alc$MLDA <- 1*(alc$month>=0)        # make positive value of "month" variable into 1, and else as 0

## global approach
rd1 <- lm(alc.suicide ~ MLDA + month + MLDA*month, data=alc)
rd1b <- lm(alc.suicide ~ MLDA + month, data=alc)    # interaction term can be dropped if no change in slope
summary(rd1b)


# model specification
rd1c <- lm(alc.suicide ~ MLDA + month + I(month^2) + I(month^3), data=alc)  # I() allows calculation inside lm() without creating new vector
    # alternative: 
rd1c <- lm(alc.suicide ~ MLDA + poly(month, degree=3), data=alc)    # include quadratic and cubic terms of month variable

## local approach - library(np) # np - non-parametric
npregbw(y ~ x, bws, ckertype, regtype, bandwidth.compute=T, data)   # ckertype (gaussian, epanechnikov, or uniform); regtype (lc,ll)

require(np)
data <- read.csv ("http://web.hku.hk/~ehylau/digoxin.csv")
plot(data$age, data$bmi, type="p", xlab="Age", ylab="BMI", las=1)
bmi.bw <- npregbw(bmi~age, bandwidth.compute=TRUE, ckertype="gaussian", regtype="ll", data=data)        # obtain bandwidth
bmi.bw <- npregbw(bmi~age, bws=3, bandwidth.compute=F, ckertype="gaussian", regtype="ll", data=data)        # change bandwidth

bmi.lp <- npreg(bws = bmi.bw)

with(data, plot(age[order(age)], predict(bmi.lp)[order(age)],
type='l', las=1, xlab="Age", ylab="BMI", xlim=c(30,90), ylim=c(15,45)))

with(data, points(age, bmi, pch=19, col="red"))

## RDD with local regression- library(rdrobust)
rd.l <- rdrobust(alc$alc.suicide, alc$month, all=T)
summary(rd.l)


### Survival Analysis - library(survival)

hpa <- read.csv("examplehpa.csv")
require(survival)

## Kaplan-Meier estiamte and plots
hpa.km1 <- survfit(Surv(time, event,type="right")~1, data=hpa)      # no grouping; right censoring
plot(hpa.km1, col='blue')

    # plot of Kaplan-Meier estiamte for two groups
hpa.km2 <- survfit(Surv(time, event, type="right")~staining, data=hpa)      # by group of "staining"
plot(hpa.km2, col=c(4,2),conf.int=F)
    # model diagnostics
plot(hpa.km2, fun="cloglog", lty=1:2, mark.time=T)

    # calculate p value
survdiff(Surv(time, event, type="right")~staining, data=hpa)        # calculate p value

## Cox Model / proportional hazard model
hpa.cox <- coxph(Surv(time, event)~staining, data=hpa)
hpa.cox;summary(hpa.cox)

    # model diagnostics 
hpa.cox.zph <- cox.zph(hpa.cox)
hpa.cox.zph     # check if any significant interaction with time (p < 0.05?)
plot(hpa.cox.zph)

## Accelerated failure time models
hpa.aft1 <- survreg(Surv(time, event)~staining,data=hpa, dist="lognormal")
hpa.aft1
exp(coef(hpa.aft1)[2])
exp(confint(hpa.aft1))

    # plot
plot(hpa.km2, lty=1:2)
curve(1-plnorm(x, meanlog=hpa.aft1$coef[1], sdlog=hpa.aft1$scale), add=TRUE, lty=1)
curve(1-plnorm(x, meanlog=hpa.aft1$coef[1]+hpa.aft1$coef[2], sdlog=hpa.aft1$scale),add=TRUE, lty=2)

## Censoring: right, left and interval censoring
survreg(Surv(timeL,timeR,event=3,type="interval")~1)        # type("right", "left"), "~1" no grouping

## Multiple imputation for missing data - library(Hmisc)
    # Missing completely at random (MCAR): missingness independent of all variables 
        # complete case analysis
        # nearest neighbor imputation, 
        # mean imputation
            mvc.miss$age[1:10] <- mean(mvc.miss$age, na.rm=T)
            
            n = nrow(ohio)
            n.missing <- 100
            missing.x <- sample(1:n, n.missing, replace=F)
            missing.y <- sample(c(3,4), n.missing, replace=T)
    # Missing at random (MAR): missingness independent of unobserved variables
        # regression imputation
            lm.impute <- lm(age~height, data=mvc.miss)
            mvc.miss$age[1:10] <- predict(lm.impute,mvc.miss)[1:10]
        # inverse probability weighting
        # multiple imputation
            mvc.impute <- transcan(~MVC + age + height, n.impute=50,shrink=T, data=mvc.miss, imputed=T)
            mvc.lm.impute <- fit.mult.impute(MVC ~ age + height, lm, mvc.impute, data=mvc.miss)
            mvc.impute$imputed$age
    # Missing not at random (MNAR): missingness dependent on unobserved variables

### HLM - library(ICC)

    # multileveled dataset setup
        n.group <- 10; y <- 1:1000
        group <- rep(1:n.group, each=length(y)/n.group)
        rand.y <- sample(y, length(y), replace=F)
## ICC groupings - rou = Vb/(Vb+Vw) (1: all unites within clusters are identical; 0: groupings are not informative)
    ICCest(as.factor(group), y)         # groupings by sorted values
    ICCest(as.factor(group), rand.y)    # randomly assigned groupings

## OLS - library(lme4); library(lmerTest)
    # data and model
    mlos <- read.csv('examplemlos.csv')
    lm.los <- lm(los~age, data=mlos)
    summary(lm.los)
    with(mlos, plot(los~hosp,ylim=c(0,6), cex=0.5,las=1))
    
    # show data
    require(lattice)
    xyplot(los~age|hosp,
    data=mlos, type=c('p','r'))

    # random intercept model
    require(lme4)
    hlm.los1 <- lmer(los ~ 1 + (1|hosp), data=mlos)     # 1|hosp: random intercept by hospitals 
    summary(hlm.los1)       # intercept shows the length of days of basis
        # ICC = variance of between group (intercept) / (intercept+residual); if close to 1 indicating group diff and vice versa
    
    # random intercept with age
    hlm.los2 <- lmer(los ~ age + (1|hosp), data=mlos)
    summary(hlm.los2)

    # t-test for fixed effect model
    require(lmerTest)
    hlm.los2 <- lmer(los ~ age + (1|hosp), data=mlos)
    summary(hlm.los2)   # check fixed effect on age to see p value
    coef(hlm.los2)      # estimated coefficients 
    detach("package:lmerTest",unload=TRUE)

    # likelihood ratio test (LRT) for the fixed intercept model
    hlm.los2.ml <- lmer(los ~ age + (1|hosp), data=mlos, REML=FALSE)
    hlm.los20.ml <- lmer(los ~ 1 + (1|hosp), data=mlos, REML=FALSE)
    anova(hlm.los20.ml, hlm.los2.ml)        # chi-square not significant indicating no significant difference

    # LRT for random intercept model
    lm.los20 <- lm(los ~ age, data=mlos)
    lrt <- as.numeric(2*abs(logLik(lm.los20)-logLik(hlm.los2.ml)))
    pchisq(lrt, df=1, lower.tail=F)         # p value for chi square is 0, indicating model significant

    # LRT for random intercept and slope model
        # example 1
    hlm.los3 <- lmer(los ~ age + (age|hosp), data=mlos)
    summary(hlm.los3)       # p value = 0.267 indicating not significant effect of age
        # example 2
    hlm.los4 <- lmer(los ~ age + size + (age|hosp), data=mlos,REML=FALSE)
    summary(hlm.los4)       # size medium significant indicating it is associated with shorter maternity LOS

    anova(hlm.los4, hlm.los3)   # hospital size comparing with that of controlled indicating no difference or size is not significant (p=0.137)

    # goodness of fit
    AIC(hlm.los1, hlm.los2, hlm.los3, hlm.los4)

    #          df      AIC
    # hlm.los1  3 2575.896  lowest AIC selected
    # hlm.los2  4 2587.711
    # hlm.los3  6 2591.020
    # hlm.los4  8 2577.887

    # residuals by fitted value
    plot(hlm.los2, resid(.)~fitted(.))
    # residuals by age
    plot(hlm.los2, resid(., scaled=TRUE) ~ age | hosp, abline = 0)
    # qq plot for random effect
    qqmath(hlm.los2)

### Longitudinal analysis - library(geepack)
## plot data - Spaghetti plot
    # to show temporal trends and corresponding proportions; jitter() to avoid overlapping
data(ohio)
with(ohio,interaction.plot(age, id, jitter(resp), ylab='wheeze',legend=F, lty=1, col=gray(0.7)))
with(ohio,interaction.plot(age, id, jitter(resp), ylab='wheeze',legend=F, lty=1, col=sample(1:20, max(id), replace=T))) # colored plot

    # fitting a GLM model
glm.ohio <- glm(resp~age+smoke, family=binomial, data=ohio)
summary(glm.ohio)

    # fitting a GEE regression model
        # GEE model - wald chi square 
            # example 1
        gee.indp <- geeglm(resp~age+smoke, family=binomial, data=ohio,id=id, corstr = "independence")
        summary(gee.indp)
        
            # example 2
        gee.exch <- geeglm(resp~age+smoke, family=binomial, data=ohio, id=id, corstr = "exchangeable")
        summary(gee.exch)

        # compare models
        gee.indp0 <- geeglm(resp ~ age, family=binomial, data=ohio, id=id, corstr="independence")
        anova(gee.indp, gee.indp0)      

        # show CI
        require(doBy)
        esticon(gee.indp, c(0,0,1))     # (0,0,1) follows a0b0+a1b1+a2b2+... (0,0,1) indicates the third variable 

        exp(esticon(gee.indp, c(0,0,1))[c("Estimate","Lower","Upper")])

        # GEE CI
        exp(esticon(gee.indp, c(0,0,1))[c("Estimate","Lower","Upper")])
        exp(esticon(gee.indp, c(0,1,0))[c("Estimate","Lower","Upper")])


    # Comparison between GLM and GEE
        # GLM ignored the dependence between observations
        # GLM usually overestimates the standard errors of time-dependent predictors; Between-subject variability was not accounted for
        # Usually underestimate the standard errors of time-independent predictors; Consider multiple measurements as additional independent samples

    # fitting GEE models with correlation structure
    require(MESS)
        # Quasi-likelihood under the independence model information criterion (QIC)
    QIC(gee.indp); QIC(gee.exch); QIC(gee.ar1); QIC(gee.unstr)

        # interaction between two variables
    gee.int.ar1 <- geeglm(resp~age*smoke, family=binomial, data=ohio, id=id, corstr = "ar1")
    summary(gee.int.ar1)        # check interaction term

### Conversion of data format in R - library(reshape)
    ## from long to wide format for GEE analysis
    data(ohio)
    ohio$ex.age <- ohio$age+9
    ohio.w <- reshape(ohio, v.names = c("age","resp"), idvar="id", timevar = "ex.age", direction = "wide")
        # resp and age variables are changing along the time; id as id variable; time variance by "ex.age"

    ## from wide to long format for GEE analysis
    ohio.l <- reshape(ohio.w, varying = list(c(3,5,7,9),c(4,6,8,10)), v.names = c("age","resp"), idvar="id", times=1:4, direction = "long")
    ohio.l <- ohio.l[order(ohio.l$id, ohio.l$time),]

### LASSO regression - library(lasso2); library(glmnet)

## training data setup
    set.seed(6789)
    x <- c(0:8) / 8 * 2 * pi
    y <- 3 * sin(x) + rnorm(n=9)
    data <- data.frame(x, y)

    # polynomial regression model fitting training data
    lm1 <-lm(y ~ x, data = data)
    lm3 <-lm(y ~ x + I(x^2) + I(x^3), data = data)
    lm6 <-lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6), data = data)
    lm8 <-lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8), data = data)

    # sine model fitting training data
    set.seed(12345)
    x <- c(0:8) / 8 * 2 * pi
    y <- 3 * sin(x) + rnorm(n=9)
    another <- data.frame(x, y)

    # testing data
    x <- c(1:12) / 2
    y <- 3 * sin(x) + rnorm(n=12)
    test <- data.frame(x, y)

## real dataset
    # read data and assign factors
    library(lasso2)
    help(Prostate) # For more details of the data set
    data(Prostate)

    # divide training and testing data
    set.seed(23456)
    TestingIndex <- sample(1:nrow(Prostate), nrow(Prostate)*0.2)    # assign 1-20 to each row of data without replacement, so only take 20 cases
        # TestingIndex <- sample(1:nrow(Prostate), 20)
    Training <- Prostate[-TestingIndex, ]
    Testing <- Prostate[TestingIndex, ]

    # backward selection
    Full <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, data = Training)
    Backward1 <- step(Full, direction = "backward")

        # alternatives
        Alt <- lm(lpsa ~ . - age - svi, data = Training)
            # Selection according to F-test or Chi-squared test
        Backward2 <- step(Full, direction = "backward", test = "F")
        Backward3 <- step(Full, direction = "backward", test = "Chisq")


    # Forward selection
    Null <- lm(lpsa ~ 1, data = Training)
    Forward <- step(Null, scope = list(lower = Null, upper = Full), direction = "forward")

    # Stepwise selection
    Stepwise1 <- step(Full, direction = "both")
    Stepwise2 <- step(Null, scope = list(lower = Null, upper = Full), direction = "both")


## Ridge regression - penalized regression, shrink regression 
    library(glmnet)

    # Training <- Prostate[-TestingIndex, ]
    # Testing <- Prostate[TestingIndex, ]

    x <- model.matrix(lpsa ~ . - 1, data = Training)        # extract predictor from dataset; remove (-1) intercept column, 
    y <- Training$lpsa                                      # 
    Ridge1 <- glmnet(x, y, alpha = 0, lambda = c(0.5, 1))   # %Dev = R2; larger lambda means larger penalty and causes lower R2; alpha by default=1 for lasso; =0 for ridge; =0.5 for elnet; x input matrix; y response matrix
    coef(Ridge1)        # show columns of lambda of 1 and 0.5

    # basics
    Ridge2 <- glmnet(x, y, alpha = 0)
    plot(Ridge2, xvar="lambda", label=TRUE) # display coefficient by lambda changed
    plot(Ridge2, xvar="dev", label=TRUE)    # display coefficient by deviances (R2) changed

    # k-fold cross validation - bootstrapping of ridge regression
    set.seed(56789)
    Ridge2.cv <- cv.glmnet(x, y, alpha = 0)
    plot(Ridge2.cv)     # mean square error shown for each log(lambda)

    min(Ridge2.cv$cvm)  
    log(Ridge2.cv$lambda.1se)

    coef(Ridge2, s = Ridge2.cv$lambda.1se)
    
    x.test <- model.matrix(lpsa ~ . - 1, data = Testing)
    Ridge2.Pred <- predict(Ridge2, newx = x.test, s = Ridge2.cv$lambda.1se)

## LASSO regression
    LASSO1 <- glmnet(x, y, alpha = 1, lambda = c(0.25, 0.5, 0.75, 1))
    coef(LASSO1)

    LASSO2 <- glmnet(x, y, alpha = 1)
    plot(LASSO2, xvar="lambda", label=TRUE)
    plot(LASSO2, xvar="dev", label=TRUE)

    # cross-validation 
    set.seed(54321)
    LASSO2.cv <- cv.glmnet(x, y, alpha = 1)
    plot(LASSO2.cv)
        # minimum MSE
    min(LASSO2.cv$cvm)  # [1] 0.5341739 
    log(LASSO2.cv$lambda.min)
        # 1-SE rule
    log(LASSO2.cv$lambda.1se)   # [1] -1.962277
    coef(LASSO2, s = LASSO2.cv$lambda.1se)
    
    LASSO2.Pred <- predict(LASSO2, newx = x.test, s = LASSO2.cv$lambda.1se) # the number of non-zero coefficients

## Multicollinearity
    Prostate.col <- Prostate
    Prostate.col$new1 <- Prostate.col$lcavol + Prostate.col$lweight

        # OLS
    lm(formula = lpsa ~ lcavol + lweight + new1, data = Prostate.col)

## Elastic net
    x <- model.matrix(lpsa ~ . - 1, data = Training)
    y <- Training$lpsa
    x.test <- model.matrix(lpsa ~ . - 1, data = Testing)

    library(glmnet)
    Elastic000 <- glmnet(x, y, alpha = 0)
    Elastic005 <- glmnet(x, y, alpha = 0.05)
    Elastic010 <- glmnet(x, y, alpha = 0.1)
    Elastic020 <- glmnet(x, y, alpha = 0.2)
    Elastic050 <- glmnet(x, y, alpha = 0.5)
    Elastic080 <- glmnet(x, y, alpha = 0.8)
    Elastic090 <- glmnet(x, y, alpha = 0.9)
    Elastic095 <- glmnet(x, y, alpha = 0.95)
    Elastic100 <- glmnet(x, y, alpha = 1)

    plot(Elastic000, xvar="lambda", label=TRUE)
    plot(Elastic005, xvar="lambda", label=TRUE)
    plot(Elastic010, xvar="lambda", label=TRUE)
    plot(Elastic020, xvar="lambda", label=TRUE)
    plot(Elastic050, xvar="lambda", label=TRUE)
    plot(Elastic080, xvar="lambda", label=TRUE)
    plot(Elastic090, xvar="lambda", label=TRUE)
    plot(Elastic095, xvar="lambda", label=TRUE)
    plot(Elastic100, xvar="lambda", label=TRUE)

    # cross-validation to determine lambda
    set.seed(76543)
    Elastic.cv <- cv.glmnet(x, y, alpha = 0.5)
    plot(Elastic.cv)

    # minimum MSE
    min(Elastic.cv$cvm)
    log(Elastic.cv$lambda.min)

    # 1-SE rule 
    log(Elastic.cv$lambda.1se)
    coef(Elastic050, s = Elastic.cv$lambda.1se)
    Elastic.Pred <- predict(Elastic050, newx = x.test, s = Elastic.cv$lambda.1se)

### Marginal structural model

    # using weights
    library(ipw)
    temp <- ipwpoint(exposure = a, family = "binomial", link = "logit", numerator = ~ 1, denominator = ~ l, data = simdat)
    summary(temp$ipw.weights)
        # show distribution of weight
        ipwplot(weights = temp$ipw.weights,logscale = FALSE,main = "Stabilized weights",xlim = c(0, 8))

    # using unstabilized weight
    temp.uns <- ipwpoint(exposure = a, family = "binomial", link ="logit", denominator = ~ l, data = simdat)
    summary(temp.uns$ipw.weights)
        # show distribution of weight
        ipwplot(weights = temp.uns$ipw.weights,
        logscale = FALSE,
        main = "Unstabilized weights",
        xlim = c(0, 8))

simdat$unsw <- temp.uns$ipw.weights
msm.uns <- svyglm(y ~ a, design = svydesign(~ 1, weights = ~unsw, data = simdat))
summary(msm.uns)    

#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
### Assignment / Tutorial #############################################################################
#######################################################################################################
# For public health surveillance purposes, a study has been carried out to assess the prevalence of obesity (defined as BMI>25) among male HKU undergraduates. The study took a random sample of 185 undergraduate males in May 2019, and assessed each of their heights and weights. In total, 13 students were found to be obese.

# 1. Write down the likelihood function corresponding to a binomial model for the observed data, in terms of the parameter θ representing the population prevalence.
    𝑙(𝜃) ~ 𝜃^13*(1−𝜃)^172 
    # or
    log𝑙(𝜃)=13log(𝜃)+172log(1−𝜃)

# 2. Plot a figure, with clear axis label and formatting, showing the likelihood function for θ ranging from 0 to 1.
    curve(x^13 * (1-x)^172, from=0, to=1, xlab="theta", ylab="Likelihood", main="Likelihood of theta between 0 and 1")

# 3. By finding the parameter value which maximizes the likelihood function, or otherwise, quote the maximum likelihood estimate of θ.
    optimize(function(x) x^13 * (1-x)^172, interval=c(0,1), maximum=TRUE)
    # the maximum value is 0.07 or 13/183
    # or
    loglik <- function(x) 13*log(x) + 172*log(1-x) 
    optim(par=0.01, loglik, lower=0.01, upper=0.2, method="L-BFGS-B", control=list(fnscale=-1))

# 4. Using a formula, approximation, or simulation, give the 95% confidence interval of θ.
    𝑛𝜃~𝑁(𝑛𝜃,𝑛𝜃1−𝜃) to obtain CI of 13 ± 1.96 × sqrt(185 × 13 / 185 × (1 − 13/185)) = (6.2, 19.8)
    (6.2, 19.8)/185 = (0.03, 0.11)

# 5. What is the p-value of the maximum likelihood estimate of θ? You may decide the H0 and use any combination of formulas, approximations or simulations.
        teststat <- (13/185)/(1/sqrt(13))
        pvalue <- pnorm(-abs(teststat), mean=0, sd=1, lower.tail=T) + pnorm(abs(teststat), mean=0, sd=1, lower.tail=F)
        # or 
        H0: miu = 0.05
        binom.test(13, 185, 0.05)

# 6. Result
    95% confident prevalence is between 3% and 11%. Result depends on binomial model being correct, which assumes independence between students, and that our sample is representative, etc.

# 7 Using the likelihood ratio method, obtain a 95% confidence interval for 𝜃; 2|log𝑙(E(𝜃))−log𝑙(𝜃)| ~ chi^2
    # function set up
    mle <- 13/185                                                           # population mean; expected mean value of the population
    logL <- function(theta) return(13*log(theta)+172*log(1-theta))
    logL.ratio <- function(theta) return(2*abs(logL(mle)-logL(theta)))

    # likelihood ratio statistics for different theta
    curve(logL.ratio, from=mle*0.5, to=2*mle, xlab=expression(theta))
    abline(h=qchisq(0.95,1), lty=2)                                         # chi square 95%CI
    abline(v=mle)

    # obtaining the 95% CI
    lr.f <- function(theta){return(abs(2*abs(logL(mle)-logL(theta))-qchisq(0.95,1)))}
        theta.lr.lb1 <- optim(0.04, lr.f, method="Brent", lower=0.03, upper=0.05)$par       # set the boundary (0.03,0.05) according to graph; set initial value 0.04 to search
        theta.lr.ub1 <- optim(0.12, lr.f, method="Brent", lower=0.1, upper=0.13)$par
        c(theta.lr.lb1, theta.lr.ub1) # 95% CI is (0.04-0.11)
    lr.f2 <- function(theta){return((2*abs(logL(mle)-logL(theta))-qchisq(0.95,1))^2)}
        theta.lr.lb2 <- optim(0.04, lr.f2, method="Brent", lower=0.03, upper=0.05)$par
        theta.lr.ub2 <- optim(0.12, lr.f2, method="Brent", lower=0.1, upper=0.13)$par
        c(theta.lr.lb2, theta.lr.ub2) # 95% CI is (0.04-0.11)

# 8. Using the bootstrap method, obtain a 95% confidence interval for 𝜃
    # create a dataset - it's just 0 and 1s (obese or not)
    data.obese <- data.frame(id=1:185, obese=c(rep(1,13),rep(0,172)))
    obese.out <- function(data, indices){
        newdata <- data[indices,]
        return(mean(newdata$obese))
    }

    require(boot)
    obese.boot.out <- boot(data=data.obese, statistic=obese.out, R=1000)
    boot.ci(obese.boot.out)

# 9. Estimate the overall prevalence using maximum likelihood method. Suppose the study was also carried out in 7 other tertiary institutions. Their results are summarized below:
    n.obese <- c(13, 18, 21, 10, 11, 10, 17, 12)
    n.sample <- c(185, 161, 272, 154, 85, 101, 221, 150)

    logL.i <- function(theta,n.obese,n.sample) return(n.obese*log(theta)+(n.sample-n.obese)*log(1-theta))
    logL.all <- function(theta) return(sum(logL.i(theta, n.obese, n.sample)))

    all.out <- optim(0.07, logL.all, method="Brent", lower=0.0001, upper=0.9999, control=list(fnscale=-1))      # Bounds on the variables for the "L-BFGS-B" method, or bounds in which to search for method "Brent".
    all.out         # 0.084 = 8.4%

# 10. Suppose it was hypothesized that institutions which were able to recruit more participants (e.g. n > 200) may have a different prevalence of obesity. Estimate the relative difference using the maximum likelihood method. You may assume that the obesity prevalence is 𝜃 for schools with fewer participants, and 𝑘𝜃 for schools with more participants.
    large.sample <- 1*(n.sample > 200)

    logL.i.group <- function(theta,n.obese,n.sample) return(n.obese*log(theta[1]*theta[2]^large.sample)+(n.sample-n.obese)*log(1-theta[1]*theta[2]^large.sample))
    logL.group <- function(theta) return(sum(logL.i.group(theta, n.obese, n.sample)))

    group.out <- optim(c(0.07,1), logL.group, method="L-BFGS-B", lower=c(0.0001,0.0001), upper=c(0.49,2), control=list(fnscale=-1))     # Bounds on the variables for the "L-BFGS-B" method, or bounds in which to search for method "Brent".
    group.out       # k = 0.87075491 = 87%

    # illustration: log-likelihood curvature
    # (sample from 1 institution)
    logL <- function(theta) return(13*log(theta)+172*log(1-theta))

    # higher curvature -> lower uncertainty / narrower confidence interval
    curve(logL, from=0.01, to=0.2)

# 11. When the sample size is large, according to maximum likelihood theory, where 𝐼^−1(𝜃) is the information matrix. 𝜕^2𝑙ogL(𝜃)/𝜕𝜕*𝜕𝜃′ is the second derivative of the log-likelihood, also named Hessian, which can be obtained by setting “hessian=T” in the optim function in R. Compute the standard error for the estimated prevalence of obesity in the first tertiary institution and calculate its 95% confidence interval.
    single.out <- optim(0.05, logL, method="Brent", lower=0.01, upper=0.2, control=list(fnscale=-1), hessian=T)

    # 95% CI from likelihood theory
    se.theta <- sqrt(-1/single.out$hessian)
    single.out$par+c(-1,1)*qnorm(0.975)*se.theta

# 12. Referring to (10.), compute the 95% confidence interval for k and test the hypothesis H0: k=1.
    group.out <- optim(c(0.07,1), logL.group, method="L-BFGS-B", lower=c(0.0001,0.0001), upper=c(0.49,2),control=list(fnscale=-1), hessian=T)
    est.k <- group.out$par[2]
    se.k <- sqrt(solve(-group.out$hessian)[2,2])    # compute inverse of the matrix
    # 95% CI
    est.k+c(-1,1)*qnorm(0.975)*se.k
    # Wald statistics < 1.96: do not reject null hypothesis
    (est.k-1) / se.k
    # correlation between estimates
    cov2cor(-solve(group.out$hessian))

# 13. Perform a likelihood ratio test for (12.)
    logL.i.group.k1 <- function(theta,n.obese,n.sample) return(n.obese*log(theta[1])+ (n.sample-n.obese)*log(1-theta[1]))
    logL.group.k1 <- function(theta) return(sum(logL.i.group.k1(theta, n.obese, n.sample)))

    group.k1.out <- optim(0.07, logL.group.k1, method="L-BFGS-B", lower=0.0001, upper=0.99,control=list(fnscale=-1))

    est.theta.group.k1 <- group.k1.out$par[1]
    est.theta.group <- group.out$par[1]

    logL0 <- logL.group.k1(c(est.theta.group.k1))
    logL1 <- logL.group(c(est.theta.group, est.k))

    LRT <- 2*abs(logL0-logL1)
    pchisq(LRT, df=1, lower.tail=F)

# 14. A study was conducted in 2007 to investigate household transmission of influenza virus (Cowling et al., 2008, PLoS ONE). An extract of the data regarding duration of illness in index cases may be loaded via flu <- read.csv("http://web.hku.hk/~ehylau/pilot2007.csv"). Fit a proportional hazards model to time to cessation of symptoms adjusting for age (0-6y, 7-15y and ≥16y), flu type, antibiotics, antiviral and antihistamine prescription. Present your results in a table.
    library(survival)
    hpa.cox <- coxph(Surv(time, event) ~ staining, data=flu)

    # age group
    flu$ageg = cut(flu$age,c(0,6,15,81))
    flu.cox <- coxph(Surv(time, event) ~ ageg+flu.type+antibiotics+antiviral+antihistamine, data=flu)


# 15. Assess the proportional hazards assumption for the model in (14). [2 mark] 
    flu.zph = cox.zph(flu.cox)

# 16. Fit 3 parametric accelerated failure time models to time to cessation of symptoms with the same variables as in (1), using lognormal, Weibull and exponential distributions. Present your results in a table. [5 mark] 
    # AFT models with different distributions
    aft1 = survreg(Surv(time, event) ~ ageg+flu.type+antibiotics+antiviral+antihistamine, data=flu,dist="lognormal")
    aft2 = survreg(Surv(time, event) ~ ageg+flu.type+antibiotics+antiviral+antihistamine, data=flu,dist="weibull")
    aft3 = survreg(Surv(time, event) ~ ageg+flu.type+antibiotics+antiviral+antihistamine, data=flu,dist="exponential")

    # result extraction
    Lognormal=exp(coef(aft1)); Weibull=exp(coef(aft2)); Exponential=exp(coef(aft3))
    a=exp(confint(aft1)); b=exp(confint(aft1)); c=exp(confint(aft1))

    # combine to table
    cbind(Lognormal,a,Weibull,b,Exponential,c)

# 17. Selection the best model in (16). [1 mark] 
    a4 = AIC(aft1,aft2,aft3)
    rownames(a4) <- c("Lognormal","Weibull","Exponential")
        # According to AIC indicator, theWeibull model with the lowest AIC (179.20) is suggested to be the best model.


# 18. Assess the model fitness of the selected model in (4). [2 marks] 
    plot(residuals(aft1, type='deviance'), ylab = "residual") 
    plot(residuals(aft2, type='deviance'), ylab = "residual") 
    plot(residuals(aft3, type='deviance'), ylab = "residual") 
    # Exponential distribution AFT model appears to have the best goodness of fit in terms of its deviance residuals, which symmetrically distributed with expected value 0.

# 19. Using the same predictor variables, fit 3 parametric accelerated failure time models to time to cessation of viral shedding, using lognormal, Weibull and exponential distributions. Present your results in a table. [5 marks]  [Hint: The left censored time at time=0 may cause problem. In that case you may change it to a small number, e.g. 0.0001 without affecting the results practically] 

    aft1c = survreg(Surv(time, event > 0.0000001, type = 'left') ~ ageg+flu.type+antibiotics+antiviral+antihistamine, data=flu, dist='lognormal')
    aft2c = survreg(Surv(time, event > 0.0000001, type = 'left') ~ ageg+flu.type+antibiotics+antiviral+antihistamine, data=flu, dist='weibull')
    aft3c = survreg(Surv(time, event > 0.0000001, type = 'left') ~ ageg+flu.type+antibiotics+antiviral+antihistamine, data=flu, dist='exponential')

    # result extraction
    Lognormal=exp(coef(aft1c)); Weibull=exp(coef(aft2c)); Exponential=exp(coef(aft3c))
    a=exp(confint(aft1c)); b=exp(confint(aft1c)); c=exp(confint(aft1c))

    # combine to table
    cbind(Lognormal,a,Weibull,b,Exponential,c)


# 20. Selection the best model in (6). [1 mark] 
    a7 = AIC(aft1c,aft2c,aft3c)
    rownames(a7) <- c("Lognormal","Weibull","Exponential")
    # According to AIC indicator, theWeibull model with the lowest AIC (179.20) is suggested to be the best model.



# 21. Assess the model fitness of the selected model in (20). [2 marks] 
    plot(residuals(aft1c, type='deviance'), ylab = "residual") 
    plot(residuals(aft2c, type='deviance'), ylab = "residual") 
    plot(residuals(aft3c, type='deviance'), ylab = "residual") 
    # Exponential distribution AFT model appears to have the best goodness of fit in terms of its deviance residuals, which symmetrically distributed with expected value 0.

# 22. Draw an overall conclusion based on the above analyses. [3 marks]
    # Among all proective factors, antibiotics significantly contributed to the longer duration between censored time to cessation of viral shedding; while the others did not significantly contribute to the measured outcome. 
    # Accelerated failure time model with lognormal distribution was favored for a better model fitting; while exponential distribution AFT model was favored in the situation of left censored time approach.  

# 23. An observational study investigated the effect of influenza vaccination on influenza infection in the general population. To estimate the vaccine effect, potential confounding effects and effect modification will be considered. Data are available from 2000 adults aged 20 to 60y (saved in ‘fluvaccine.csv’), with the following variables. 
# Read the dataset into R. Suppose that based on literature review and a preliminary analysis, the causal structure can be summarized by the following DAG
    # read in dataset
    flu <- read.csv("fluvaccine.csv")

    lr0 <- glm(flu~vac+shealth+vac*smoking, data=flu, family=binomial)
    exp(coef(lr0))
    exp(confint(lr0))

# 24. Suppose the same dataset have some missing data (‘fluvaccine_m1.csv’). Study the dataset especially on the missingness of the data
    flu.m1 <- read.csv('fluvaccine_m1.csv')

    # Visualize missing pattern
    require(VIM)
    aggr_plot <- aggr(flu.m1, sortVars=T, numbers=T)    # data demonstration
    marginplot(flu.m1[,c(4,5)])                         # marginal plot 
    marginplot(flu.m1[,c(4,7)])

    flu.m1$complete <- complete.cases(flu.m1)           # mark cases with or without complete cases
    boxplot(shealth~complete, data=flu.m1)              # show var "shealth" with complete info
    with(flu.m1, prop.table(table(vac, complete),2))
    with(flu.m1, prop.table(table(smoking, complete),2))

# 25. Complete case analysis
    lr0.m1 <- glm(flu~vac+shealth+vac*smoking, data=flu.m1, family=binomial)
    summary(lr0.m1)
    exp(coef(lr0.m1))
    exp(confint(lr0.m1))

# 26. Construct imputed dataset using variables in the final analysis model
    require(Hmisc)
    
    flu.m1.impute <- transcan(~flu+vac+shealth+smoking, n.impute=50, shrink=T, data=flu.m1, imputed=T)      # shrink=T to avoid overfitting; imputed=T to save imputed value
    flu.m1.impute$imputed$shealth[,1]
    flu.m1.impute$imputed$vac[,1]

    # switch to aregImpute
    flu.m1.impute <- aregImpute(~flu+vac+shealth+smoking, n.impute=50, data=flu.m1) # imputed data saved
    flu.m1.impute$imputed$shealth[,1]
    flu.m1.impute$imputed$vac[,1]

    # Visualize imputed data
    # shealth
    boxplot(flu.m1$shealth, border='blue', at=1, xlim=c(0,3))
    boxplot(flu.m1.impute$imputed$shealth[,1], border='red', at=2, add=T)
    boxplot(flu.m1$shealth, border='blue', at=1, xlim=c(0,53))
    for (i in 1:50) boxplot(flu.m1.impute$imputed$shealth[,i], border='red', at=i+1, add=T)

    # compare imputed and orginal data on variable "vac"
    prop.table(table(flu.m1$vac))                       
    prop.table(table(flu.m1.impute$imputed$vac[,1]))

    for(i in 1:50) print(prop.table(table(flu.m1.impute$imputed$vac[,i])))  # show all imputed data

# 27. Fit regression model on imputed datasets
    lr0.m1.impute <- fit.mult.impute(flu ~vac+shealth+vac*smoking, glm, flu.m1.impute, data=flu.m1, family=binomial)    # formula, fitter (model), xtrans, data,approach
    summary(lr0.m1.impute)
    exp(coef(lr0.m1.impute))
    exp(confint(lr0.m1.impute))

# 28. dataset with missing data (not MCAR)
    flu.m2 <- read.csv('fluvaccine_m2.csv')
    aggr_plot <- aggr(flu.m2, sortVars=T, numbers=T)
    marginplot(flu.m2[,c(4,5)])
    marginplot(flu.m2[,c(4,6)])
    marginplot(flu.m2[,c(4,7)])

# 29. complete case analysis
    lr0.m2 <- glm(flu~vac+shealth+vac*smoking, data=flu.m2, family=binomial)
    summary(lr0.m2)
    exp(coef(lr0.m2))
    exp(confint(lr0.m2))


# 30. multiple imputation using variables in the final model
    flu.m2.impute <- aregImpute(~flu+vac+shealth+smoking, n.impute=50, data=flu.m2) # imputation method
    flu.m2.impute$imputed$vac

    # Visualize imputed data
    # shealth
    boxplot(flu.m2$shealth, border='blue', at=1, xlim=c(0,3))
    boxplot(flu.m2.impute$imputed$shealth[,1], border='red', at=2, add=T)

    boxplot(flu.m2$shealth, border='blue', at=1, xlim=c(0,53))
    for (i in 1:50) boxplot(flu.m2.impute$imputed$shealth[,i], border='red', at=i+1, add=T)

    # vac
    prop.table(table(flu$vac))
    prop.table(table(flu.m2$vac))
    prop.table(table(flu.m2.impute$imputed$vac[,1]))
    for(i in 1:50) print(prop.table(table(flu.m2.impute$imputed$vac[,i])))

    # comparing the distribution of vaccination for different shealth score, original vs imputed dataset
    prop.table(table(flu.m2$shealth, flu.m2$vac),1)
    vac.miss.m2 <- which(is.na(flu.m2$vac))
    shealth.miss.m2 <- which(is.na(flu.m2$shealth))
    imputed.shealth.m2.i1 <- flu.m2$shealth
    imputed.shealth.m2.i1[shealth.miss.m2] <- flu.m2.impute$imputed$shealth[,1]     # replace missing data with imputed values
    prop.table(table(imputed.shealth.m2.i1[vac.miss.m2], flu.m2.impute$imputed$vac[,1]),1)

    lr0.m2.impute <- fit.mult.impute(flu ~vac+shealth+vac*smoking,glm,flu.m2.impute, data=flu.m2, family=binomial)
    summary(lr0.m2.impute)

    exp(coef(lr0.m2.impute))
    exp(confint(lr0.m2.impute))

# 31. using all variables for imputation
    flu.m2.impute2 <- aregImpute(~flu+vac+shealth+smoking+age+male+bmi+abT, n.impute=50, data=flu.m2)
    flu.m2.impute2$imputed$vac

    lr0.m2.impute2 <- fit.mult.impute(flu ~vac+shealth+vac*smoking, glm, flu.m2.impute2, data=flu.m2, family=binomial)
    summary(lr0.m2.impute2)

    exp(coef(lr0.m2.impute2))
    exp(confint(lr0.m2.impute2))

# 32. Suppose the same dataset have more missing data (up to 30% missing for key variables, ‘fluvaccine_m3.csv’). Study the dataset especially on the missingness of the data.
    flu.m3 <- read.csv('fluvaccine_m3.csv')
    aggr_plot <- aggr(flu.m3, sortVars=T, numbers=T)
    marginplot(flu.m3[,c(4,7)])     # 4th (shealth) and 7th (vac) columns

    # complete case analysis
    lr0.m3 <- glm(flu~vac+shealth+vac*smoking, data=flu.m3, family=binomial)
    summary(lr0.m3)

    exp(coef(lr0.m3))
    exp(confint(lr0.m3))

    # multiple imputation using all variables
    flu.m3.impute <- aregImpute(~flu+vac+shealth+smoking+age+male+bmi+abT, n.impute=50, data=flu.m3)

    lr0.m3.impute <- fit.mult.impute(flu ~vac+shealth+vac*smoking, glm, flu.m3.impute, data=flu.m3, family=binomial)
    summary(lr0.m3.impute)

    exp(coef(lr0.m3.impute))
    exp(confint(lr0.m3.impute))

# 33. Perform the regression analysis again using multiple imputation method. Compare results with multiple imputation method

# 34. read in data and draw spaghetti plot
    data <- read.csv('ambc.csv')

    require(lattice)
    with(data,interaction.plot(time,id,pm, legend=F, lty=1, col=gray(0.7), xlab='follow up', ylab='Mean PM2.5', las=1)) # factors time and id; response variable pm
    with(data,interaction.plot(time,id,ambc, legend=F, lty=1, col=gray(0.7), xlab='follow up', ylab='AMBC', las=1))

    require(ICC)
    with(data, ICCest(as.factor(id), ambc)) # column name indicating individual or group id; measurement variable ambc

# 35. Explore the relationship between indoor PM2.5 and AMBC content for some subjects
    xyplot(ambc~pm|id, data=data, type=c('p','r'))

# 36. Fit an ordinary regression model to predict AMBC by age, male, and PM2.5.
    l0 <- lm(ambc~male+age+pm, data=data)
    summary(l0)
    confint(l0)

    ICCest(as.factor(data$id), resid(l0))       # ICC = 0.23; id and residuals of l0

# 37. fitting GEE in 4 different approaches - specify correlational structure assuming cases are conditionally independent
    library(geepack)
    g.indp <- geeglm(ambc~male+age+pm, family=gaussian, id=id, corstr="independence", data=data)
    g.exch <- geeglm(ambc~male+age+pm, family=gaussian, id=id, corstr="exchangeable", data=data)
    g.ar1 <- geeglm(ambc~male+age+pm, family=gaussian, id=id, corstr="ar1", data=data)
    g.unstr <- geeglm(ambc~male+age+pm, family=gaussian, id=id, corstr="unstructured", data=data)

# 38. Select the best model; comparing models
    library(MESS)
    QIC(g.indp); QIC(g.exch); QIC(g.ar1); QIC(g.unstr)  # select lowest QIC and QIC

    rbind(QIC(g.indp), QIC(g.exch), QIC(g.ar1), QIC(g.unstr))  # select lowest QIC and QIC

# 39. Summarize your results from the selected model in (38) in the following table
    summary(g.ar1)
    library(doBy)
    est.male <- esticon(g.ar1, c(0,1,0,0))  # male
    est.age <- esticon(g.ar1, c(0,0,1,0))  # age
    est.pm <- esticon(g.ar1, c(0,0,0,1))  # pm
    
    rbind(est.male,est.age,est.pm)      # show all estimates


    ICCest(as.factor(data$id), resid(g.ar1))    # check ICC = 0.2327

# 40. Fit a linear mixed model with random intercept to predict AMBC by age, male, and PM2.5.
    require(lme4)
    lme1 <- lmer(ambc~male+age+pm+(1|id), data=data)
    summary(lme1)
    confint(lme1, method="Wald")

ICCest(as.factor(data$id), resid(lme1))

# 41. Calculate the intraclass correlation (ICC) for the model in 40.
0.0452/(0.0452+0.1454)      # random effect model group variable variance = Vb; IV's vairance = Vw. Vb/(Vb+Vw)

# 42. fitting linear mixed model (random intercept and slope) of age, male, pm predicting ambc
    lme2 <- lmer(ambc~male+age+pm+(1+pm|id), data=data)     # lme3 <- lmer(ambc~male+age+pm+(pm|id), data=data)
    summary(lme2)
    confint(lme2, method="Wald")

# 43. convert data to wide form 
    data.wide <- reshape(data, v.names=c("pm","ambc"), idvar="id", timevar="time", direction="wide")
    head(data.wide)     # v.names: vars in long formt corresponding to multiple vars in wide format; idvar: case id including multiple cases in long format

    l.multi <- lm(ambc.5~male+age+pm.1+pm.2+pm.3+pm.4+pm.5, data=data.wide)
    summary(l.multi)
    confint(l.multi)

    require(car)
    vif(l.multi)

    data.long <- reshape(data.wide, varying = list(c(4,6,8,10,12),c(5,7,9,11,13)),v.names=c("pm","ambc"), idvar="id",times=1:5,direction="long")    # list position of columnes; v.names - initial name; times- time points

# 44. LASSO regression
    x <- model.matrix(ambc.5~-1+male+age+pm.1+pm.2+pm.3+pm.4+pm.5, data=data.wide)
    y <- data.wide$ambc.5

    require(glmnet)
    lasso.cv <- cv.glmnet(x,y,alpha=1)
    plot(lasso.cv)
    lambda.cv <- lasso.cv$lambda.min

    lasso.opt <- glmnet(x, y, alpha=1, lambda=lambda.cv)
    coef(lasso.opt)

    lambda.1se <- lasso.cv$lambda.1se
    lasso.1se <- glmnet(x, y, alpha=1, lambda=lambda.1se)
    coef(lasso.1se)





