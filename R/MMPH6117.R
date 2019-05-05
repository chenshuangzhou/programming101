read.table("D:/OneDrive/research/1personal/Geriatrics/meta/test.txt",header=T,sep="\t",na.strings = "NA")              # Office - Lenovo X1

library(ggplot2);library(MASS);library(car);library(bda);library(survival);library(MatchIt);library(metafor);library(AER)


### CMED 6020, MMPH6117 Advanced Statistical Methods I #############################################
# SESSION 1 ##########################################
mvc <- read.csv("http://web.hku.hk/~ehylau/mvc.csv")

dim(data) # check the number of cases and variables
colname(data) # see variable names
head(data)    # specify the number of rows
tail(data)
relevel       # reset the reference group

aggregate(y ~ x1+x2, data=dataname, FUN='mean')
prop.table(table, margin)   # margin - by row/column


## ggplot help
http://www.cookbook-r.com/Graphs/index.html

# histogram
hist(mvc$MVC, axes=FALSE, xlim=c(0, 600), 
    ylim=c(0, 8), font.lab=2, cex.lab=1.2, 
    cex.main=1.5, col=grey(0.8),   
    xlab="MVC", ylab="Frequency", 
    main="Histogram of MVC")
axis(1, pos=-0.2, lwd=3.5, font=2)
axis(2, pos=-20, lwd=3.5, font=2, las=1)

# scattered plot
plot(MVC ~ age, data=mvc, xlab="Age (years)", 
    ylab="Quadriceps muscle (newtons)", 
    xlim=c(20, 70), ylim=c(0, 600), 
    cex=1.5, cex.lab=1.2, 
    font.lab=2, font.axis=2, las=1)

# line plot


# save graph to pdf
pdf("d:/figure1.pdf", width=6, height=4)
plot(MVC ~ age, data=mvc, 
    xlab = "Age (years)", 
    ylab = "Quadriceps muscle (newtons)",  
    xlim = c(20, 70), ylim = c(0, 600), 
    cex = 1.5, cex.lab = 1.2, 
    font.lab = 2,  font.axis = 2, las = 1) 
dev.off() 



# convert numeric to factor
cut(x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE, ...) 

# record categorical - setting reference group
x.f <- cut(x,c(1,3,6,8), label=c('low','med','high'), include.lowest=T, right=F) 
relevel(x.f, ref='med')                      # setting 'med' as the reference group
factor(x.f, c('high', 'med', 'low'))   # setting 'high' as reference group in such order


# SESSION 3 - Poisson distribution and GLM ##########################################

# Poisson distribution has only 1 parameter, the mean and variance are assumed to be the same
  # log of the mean 
    # link function: where logit(y) = log(y/(1-y))
  # rates in Poisson distribution

# Model check
  # compare the actual probability and expected ones 
  # check residual deviance/df <=1 indicates the model has good fitting; otherwise the model fails
  # or use chi square test to compare the model wit null hypothesized model
# 

dpois(x=2,lambda=5)   # density of Poisson distribution, show the xth number of incident, the probability is the corresponding one
rpois(n=10, lambda=5)
var(rpois(n=10, lambda=5))


for (i in 10^(1:5)){
  check = rbind(check,shapiro.test(rpois(n=1000,lambda=i))[2])
}


horse = read.table("C:/Users/chens/OneDrive/research/2school/PhD Courses/CMED 6020 MMPH6117 Advanced Statistical Methods I/3 GLM/GLM1/Example - horse.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)
summary(horse)
hist(horse$death, breaks=0:5)         # clustered by 1 unit of x from 0 to 5
mean(horse$death); var(horse$death)   # mean is close to variance
table(horse$death);table(horse$corps)

  # deaths ~ year
boxplot(deaths~year,data=horse, xlab="year", ylab="deaths")   # boxplot by year

horse.year <- aggregate(horse$deaths, by=list(horse$year), sum)   
colnames(horse.year) <- c('year', 'deaths') 
plot(horse.year$year, horse.year$deaths, type='l', xlab='year', ylab='deaths')    # line plot
plot(h[,1],h[,2],type='l',xlab='year',ylab='deaths')        # or, without colname function, use matrix function

  # deaths ~ corps
boxplot(deaths~corps, data=horse, xlab="corps", ylab="deaths")


# GLM - ordinal linear, logistic, poisson, negative binomial models
summary(glm(deaths ~ 1, data=horse, family=poisson))    # "~1" indicates fit in intercept only
summary(glm(deaths ~- 1, data=horse, family=poisson))    # "-1" leaves intercept out

pois.horse <- glm(deaths ~ 1, data=horse, family=poisson) 
coef(pois.horse)            # or 'pois.horse$coef'
exp(coef(pois.horse))       # to obtain the mean from the link function by taking the exp; mean(deaths)=0.61
exp(confint(pois.horse))    # obtain the confidence interval from the link function by taking the exp; CI(deaths) = [0.5,0.724]

  # model check - comparison
prop.table(table(horse$deaths))   # = table(horse$deaths)/sum(table(horse$deaths))
round(dpois(0:5,0.61),2)          # density distribution of poisson, rounded up to the second digit
nrow(horse)*dpois(0:5,exp(coef(pois.horse)))  # reverse to the expected counts of deaths based on the data volumn using the poisson distribtion

class(horse$corps)
horse$corps <- as.factor(horse$corps)     # corps is the categorical variable rather than continuous variables
summary(glm(deaths~corps, data=horse, family=poisson))    # reference group is corp 2 by default


pois.corps <- glm(deaths~corps, data=horse, family=poisson) 
round(exp(coef(pois.corps)),2)    # the exp coefficient indicate the time's relationship between each group to the reference group

cbind(coef(pois.corps),confint(pois.corps))
round(exp(cbind(coef(pois.corps), confint(pois.corps))),2)    # transformed exp coefficient of each group in relation to the reference group

class(horse$corps)
horse$corps=as.factor(horse$corps)                      # turn continuous into categorical variables
summary(glm(deaths~corps, data=horse, family= poisson)) 

pois.corps=glm(deaths~corps, data=horse, family= poisson)
round(exp(coef(pois.corps)),2)
round(exp(cbind(coef(pois.corps),confint(pois.corps))),2) # revert to the table with coefficient and confidence interval


# counts and rates
lung = read.table("C:/Users/chens/OneDrive/research/2school/PhD Courses/CMED 6020 MMPH6117 Advanced Statistical Methods I/3 GLM/GLM1/Example - lung.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)

pois.lung <- glm(count~offset(log(pop))+city+age.gp, data=lung, family=poisson)   # offset function enable in coefficient of 1 for offset log term, in consideration of count/population rates
round(exp(cbind(coef(pois.lung), confint(pois.lung))),3)    # transformation of coefficients; incidents increase as aging

plot(predict(pois.lung, type='response'), lung$count, 
    xlim=c(0,15), ylim=c(0,15), xlab='predicted', ylab='observed') 
abline(a=0, b=1)     #  Compare predicted and observed data - model checking

# observed incidences 
with(lung,round(count[city=="Fredericia"]
pop[city=="Fredericia"]*1000,2)) 
 
# predicted incidences 
new <- data.frame(city="Fredericia", age.gp=lung$age.gp[1:6], pop=lung$pop[1:6]) 
round(predict(pois.lung, newdata=new, type='response')/ lung$pop[lung$city=="Fredericia"]*1000,2)

# model checking 2 - goodness-of-fit
summary(pois.lung) 
deviance(pois.lung)/df.residual(pois.lung)    # model checking

# Variance is much larger than mean in poisson distribution - alpha indicates more randomness/ variance
#Example: lambda=5, gamma mean=1, variance=2
rpois(10000, 5*rgamma(10000,1/2,1/2))

epi = read.table("C:/Users/chens/OneDrive/research/2school/PhD Courses/CMED 6020 MMPH6117 Advanced Statistical Methods I/3 GLM/GLM1/Example - epilepsy.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)
mean(epi$y); var(epi$y)

require(MASS)
summary(glm(y~1,data=epi))
nb.epilepsy0 <-glm(y~1,data=epi)
nb.therapy1 <- glm.nb(y~therapy, data=epi)
nb.therapy2 <- glm.nb(y~therapy+x+age, data=epi)
epi$log10x <-  log10(epi$x)
nb.therapy3 <- glm.nb(y~therapy+log10x+age, data=epi)
round(exp(rbind(cbind(coef(nb.therapy1),confint(nb.therapy1)),
    cbind(coef(nb.therapy2),confint(nb.therapy2)),
    cbind(coef(nb.therapy3),confint(nb.therapy3)))),3)

# model comparison
AIC(nb.epilepsy0, nb.therapy1, nb.therapy2, nb.therapy3)    # AIC the lowest indicates best fit

# # SESSION 4, Session 5 ####################################################################################
 # Multicollinearity
   # Scatterplot between all predictor variables
   # Variance inflation factor (VIF) - inflated SE
     # x (to be tested) as the predictor and the rest being IV
     # VIF > 10 -> Multicollinearity
     # centering if it is polynomial model
    # No direct explanation of the x2 (collineated IV) on the outcome variable
   # Strategy on Multicollinearity
     # Do nothing: because coefficients and standard errors are unbiased; however, there is inefficient estimation; overall, should follow the objectives and assumptions 
     # Increase sample size
     # Polynomial terms and interactions: centering (subtracting variable by its mean)
       # squared or interaction with centered term to solve 
 # Confounding effect (C):
   # Xc affect both IV and DV
   # DAG: directed acyclic graph 
   argue the residual confounding effect in obsevational data
   # minimizing confounding effects
     # variable selection: p value, AIC
     # relative change in estiamte > 10%
     meet crtieria: associations between C with X and Y; check if C is not the mediator between X -> Y
   # Indicated impact of cofounder on outcome variable;  C has impact on both X and Y
   # Include all confounders in the model while reporting that residuals confounders do not have large impact on the result
 
  # C -> X    C -> Y    Direction   Change from unadjusted to adjusted estiamte
  # direct    direct    positive    unadjusted > adjusted 
  # direct    inverse   negative    unadjusted < adjusted 
  # direct    inverse   positive    unadjusted > adjusted 
  # inverse   direct    negative    unadjusted < adjusted 

  # Multi-level structure 
    # Violating assumption of homoscedasticity
  # Measurement error
    # Imprecise measurement in predictors will attenuate estimated coefficients toward zero
  # Interaction Effect
  # Mediation
    # Baron and Kenny criteria

## multicollinearity
mvc <- read.csv("http://web.hku.hk/~ehylau/mvc.csv")
mvc$height.sq=mvc$height^2
pairs(mvc)      # no strong patterns between variables if it is not in a linear pattened 
summary(lm(MVC~age+height+height.sq, data=mvc))   # check r^2, estimate and std. error to see if SE inflated
# mvc$height.c=mvc$height-mean(mvc$height)        # create centered term

library(car)
mvc.lm3 <- lm(MVC~age+height+height.sq, data=mvc) # 
vif(mvc.lm3)                                      # both height and height.sq have VIF>>10


mvc$ct.height <- scale(mvc$height, scale=F)       # standardizing (centering) variable; "scale" : division of standard deviation
mvc$ct.height.sq <- mvc$ct.height^2 
mvc.lm4 <- lm(MVC~age+ct.height+ct.height.sq, data=mvc) # VIF is lower than 10, multicollinearity is solved
# mvc.lm4 <- lm(MVC~age+height+ct.height.sq, data=mvc) # same result with different scale of intercepts
vif(mvc.lm4)   

## confounding, moderation, mediation
cardio = read.table("C:/Users/chens/OneDrive/research/2school/PhD Courses/CMED 6020 MMPH6117 Advanced Statistical Methods I/3 GLM/GLM3/Example - cardio.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)

  # confounder check-up 1: path analysis
  # X: phy; Y: sfrs; C: ses  
summary(lm(phy~ses, data=cardio))         # c->x
summary(lm(sfrs~ses, data=cardio))        # c->y
summary(lm(ses~phy, data=cardio))         # x->c; whether c being mediator of x->y

  # confounder check-up 2: relative change in estimate
summary(lm(sfrs~phy, data=cardio))        # obtain coefficient of phy=-0.0348
summary(lm(sfrs~phy+ses, data=cardio))    # coefficient of phy=-0.0445; relative change=(445-348)/348=27.9% > 10%, indicating confounding effect from ses
summary(lm(sfrs~phy+age, data=cardio))    # coefficient of phy=-0.03505; relative change=(3505-3480)/3480=0.7% < 10%, indicating non-confounding effect from age

  # confounder check-up 3: AIC
require(MASS) 
cf2 <- lm(sfrs~phy+ses, data=cardio) 
stepAIC(cf2)       # initial model has the lowest AIC. If suspecting SES being the confounder, the AIC will increase. ##??increase of AIC indicates SES is the confounder; testing a series of assumptions; p value here indicates significance of null hypothesis of each model in the stepping process

## interaction effect
age.int <- lm(sfrs~phy*age+ses, data=cardio)  # in format of "exposure" * "effect modifier" + "confounder", such as "phy+age+phy*age+ses"
# A*B = main effects of A and B, interaction term of A and B
summary(age.int)   # check significance level of interaction term 

  # compare model without interaction term to gain the main effect
summary(lm(sfrs~phy+age+ses, data=cardio))

## mediation
  # Approach 1
  # Steps: M ~ X; Y ~ M; Y ~ X; Y ~ X + M (for reference to evaluate direct and indirect effects)
  # 1. indirect effect of x -> m
summary(lm(bmi~phy, data=cardio)) 
  # 2. indirect effect of m -> y
summary(lm(sfrs~bmi, data=cardio)) 
  # 3. direct effect of x -> y
summary(lm(sfrs~phy, data=cardio))        # Bp=-0.035
  # 4. overall model for comparison
summary(lm(sfrs~phy+bmi, data=cardio))    # Bp=-0.041; beta is not attenuated, indicating the mediation effect does not hold

  # Approach 2.1
  # Sobel' test. alpha: X->M; beta: M->Y in Y~X+M; tau: X -> Y
alpha = coef(summary(lm(bmi~phy, data=cardio)))[2,1]        # M~X
alphaSE = coef(summary(lm(bmi~phy, data=cardio)))[2,2]      
beta = coef(summary(lm(sfrs~phy+bmi, data=cardio)))[3,1]    # Y~X+M
betaSE = coef(summary(lm(sfrs~phy+bmi, data=cardio)))[3,2]
z = alpha*beta/sqrt(alpha^2*betaSE^2+beta^2*alphaSE^2)      # Sobel's test
p = 2*pnorm(z,lower.tail=F)                                 # p value for sobel's test

  # Approach 2.2
  # Other Sobel's test
library(bda)
with(cardio,mediation.test(bmi,phy,sfrs)) # mediation.test(mv,iv,dv); significant result indicates significant mediator


# sobel's test = the mediation path is significantly from 0

library(bda)
mediation.test(mv,iv,dv)  # mv mediator
mediation.test(cardio$bmi,cardio$phy,cardio$sfrs)

with(cardio, mediation.test(bmi, phy, sfrs))



# SESSION 6 PSA ####################################################################################

## Case control study
  # types of case control study: matched case-control study, nested case-control study, risk set sampling
  # matching: balancing certain characteristics between groups to increase efficiency
  # types of matching: individually matching (paired; 1v1, 1vX), frequency matching

## Conditional logistic regression
  # logit(Pij) = Ai + B1 X1ij +... 
    # A - alpha, characteristics of each stratum (matched set)
    # B - beta, coefficient
    # i = the matched set
    # j = individuals

library(survival) # for clogit  
# case.status ~ exposure + strata(matched.set) 

mers = read.table("C:/Users/chens/OneDrive/research/2school/PhD Courses/CMED 6020 MMPH6117 Advanced Statistical Methods I/4 logistic and PSM/examplemers.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)

## conditional logistic regression
clr.mers <- clogit(case ~ dromedary + sheep + smoking + strata(strata), data=mers)  # strata: matched cases
summary(clr.mers)   # exposure to dromedary is significantly associated with MERS infection (exp(coef)OR=9.9, 95%CI=1.8-54.8); smoking is significantly associated with MERS infection (OR=14.9, 95%CI=2.6-87.1)

## PSA: the likelihood of selecting the specific options
  # To analyze quasi-experiment data
    # quasi experiment: little control on the allocation of treatment and associating factors
    # selection bias/group nonequivalence
      # treatment may tend to select patients with certain characteristics
      # patients with certain characteristics may select treatment
      # patients across different treatments may not be comparable
  # To balance observed characteristics across treatment 
    # so that more accurate estimates of the treatment effect can be estimated
  # Allow analysis on factors associated with treatment assignment
  # To adjust confounding effects

mi=read.table("C:/Users/chens/OneDrive/research/2school/PhD Courses/CMED 6020 MMPH6117 Advanced Statistical Methods I/4 logistic and PSM/examplemi.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)

## summary 
summary(mi)
with(mi, table(trt, death)) # table of treatment options and mortality rate
round(with(mi, prop.table(table(trt, death),1)),3)  # proportion of mortality rate by treatment groups; prop.table - 1 by row, 2 by column; treatment group with 15.6% mortality rate, control with 19.2%
with(mi,t.test(death~trt))  # death rates are no different between groups

## graphics
library(ggplot2)
mi$trt <- as.factor(mi$trt) # turn continuous to categorical var
 
# death ~ age * treatment 
ggplot(mi, aes(x=age, fill=trt)) + geom_histogram(binwidth=1) + facet_grid(trt ~ .) 
# death ~ risk * treatment
ggplot(mi, aes(x=risk, fill=trt)) + geom_histogram(binwidth=1) + facet_grid(trt ~ .) 
# death ~ severity * treatment
ggplot(mi, aes(x=severity, fill=trt)) + geom_histogram(binwidth=1) + facet_grid(trt ~ .) 
ggplot(mi, aes(x=severity, fill=factor(trt))) + geom_histogram(binwidth=1) + facet_grid(trt ~ .) 

  # integrated table
h1n1 = read.table("C:/Users/chens/OneDrive/research/2school/PhD Courses/CMED 6020 MMPH6117 Advanced Statistical Methods I/Assignment/h1n1pdm.csv",header=T,sep=",",na.strings = "NA")

ftable(with(h1n1, table(et, agegp, psq)))[1:3,] 
round(prop.table(ftable(with(h1n1, table(et, agegp, psq)))[1:3,],2),3) 
ftable(with(h1n1, table(et, agegp, psq)))[4:6,] 
round(prop.table(ftable(with(h1n1, table(et, agegp, psq)))[4:6,],2),3)

ftable(with(h1n1, table(et, male, psq)))[c(2,4),] 
round(prop.table(ftable(with(h1n1, table(et, male, psq)))[1:2,],2)[2,],3) 
round(prop.table(ftable(with(h1n1, table(et, male, psq)))[3:4,],2)[2,],3)

ftable(with(h1n1, table(et, mv, psq)))[c(2,4),] 
round(prop.table(ftable(with(h1n1, table(et, mv, psq)))[1:2,],2)[2,],3) 
round(prop.table(ftable(with(h1n1, table(et, mv, psq)))[3:4,],2)[2,],3) 

ftable(with(h1n1, table(et, asthma, psq)))[c(2,4),] 
round(prop.table(ftable(with(h1n1, table(et, asthma, psq)))[1:2,],2)[2,],3) 
round(prop.table(ftable(with(h1n1, table(et, asthma, psq)))[3:4,],2)[2,],3) 

ftable(with(h1n1, table(et, copd, psq)))[c(2,4),] 
round(prop.table(ftable(with(h1n1, table(et, copd, psq)))[1:2,],2)[2,],3) 
round(prop.table(ftable(with(h1n1, table(et, copd, psq)))[3:4,],2)[2,],3) 


## Balance Approach 1 - Propensity Score
  # Functionality and rationale
    # the probability (propensity) of assigning to treatment for individuals
    # depends on the covariates / factors
    # does not depend on the outcome
    # to compare individuals with similar propensity scores (treatment vs. non-treatment)
    # any difference in the outcome should then be due to the treatment effect only
  # Assumptions
    # conditional independence / unconfoundedness: Y being independent from treatment assignment 
    # common support/overlap condition: a comparison group in each condition of treatment

  # modeling with example of MI data
ps.model <- glm(trt ~ age + risk + severity, data=mi, family=binomial) # treatment group with propensity score in consideration of age, risk and severity
mi$ps <- predict(ps.model, type='response')   # generate propensity score in each case for further matching

  # check distribution of data with propensity score 
ggplot(mi, aes(x=ps, fill=trt)) + geom_histogram(binwidth=0.01) + facet_grid(trt ~ .)   # more people choosing newer drug (treatment=1) with higher propensity score; common support, the comparison by condition of treatment emerges as the other variables are paired up

## Balance Approach 2 - Stratification - stratify by propensity scores into such as 5 groups with weighted mean of stratum-specified treatment effects 
ps.boundary <- quantile(mi$ps, 0:5/5)   # create 6 groups with even probability 
mi$psq <- cut(mi$ps, ps.boundary, right=F, include.lowest=T, label=1:5) # propensity grouping quintiles

  # death ~ age * treatment by PS strata
ggplot(mi, aes(x=trt, y=age)) + geom_boxplot(aes(fill=trt)) + facet_grid(psq ~ ., labeller=label_both) + coord_flip() # grouped by propensity score categories
  # death ~ risk (* counts) by PS strata
ggplot(mi, aes(x=risk)) + geom_histogram(binwidth=1, fill='blue') + facet_grid(trt ~ psq, labeller=label_both) # check distribution of treatment groups being similar or not
  # death ~ severity (* counts) by PS strata
ggplot(mi, aes(x=severity)) + geom_histogram(binwidth=1, fill='blue') + facet_grid(trt ~ psq, labeller=label_both) 

## Balance Approach 3 - T score for balance of treatment groups; t-test scores are modified greatly/adjusted by standard deviation
summary(lm(age~trt, data=mi))       # t score for age = 4.41 for unadjusted (in treatment==1); as well as for risk and severity
summary(lm(age~trt+psq, data=mi))   # t score for age = 0.73 as adjusted  (in treatment==1); as well as for risk and severity
  # distance (T score) shows the balanced distribution for treatment groups

## Treatment Effect Calculation 
mean.tp <- aggregate(death~trt+psq, data=mi, FUN=mean)    # Mean for each combination of treatment group and propensity groups
count.tp <- aggregate(death~trt+psq, data=mi, FUN=length) # counting numbers for each combinations of ...
cbind(mean.tp, count.tp$death) 

  # strata specific mean
str.mean <- mean.tp$death[mean.tp$et==1] - mean.tp$death[mean.tp$et==0] 

  # overall treatment effect 
n.psq <- as.numeric(table(h$psq))    # numbers of cases in each stratum
overall <- sum((mean.tp$death[mean.tp$et==1] - mean.tp$death[mean.tp$et==0])*n.psq)/sum(n.psq) 

  # overall treatment effect = (80*(0.04-0.11) + 78*(0.17-0.20) + 82*(0.17-0.15) + 80*(0.15-0.31) + 80*(0.20-0.25)) / 400 = -0.0603 ((treatment1-treatment0)* n)

  # variance of estimated treatment effect
var.tp <- aggregate(death~trt+psq, data=mi, FUN=var) 
cbind(var.tp, count.tp$death)   
sum((var.tp$death/count.tp$death)*rep(n.psq, each=2)^2)/sum(n.psq)^2

  # CI = M +/- SE = (-0.14, 0.02)
var.tp <- aggregate(death~et+psq, data=h1n1, FUN=var) 
var.over <- sum((var.tp$death/count.tp$death)*rep(n.psq, each=2)^2)/sum(n.psq)^2 
lowb <- overall - 1.96*sqrt(var.over) 
uppb <- overall + 1.96*sqrt(var.over) 
print(c(lowb, uppb)) 

## Propensity score matching function for case-control data
library(MatchIt)
m.out <- matchit(trt ~ age + risk + severity, data = mi, method = "nearest", subclass=20) # 20 classes using nearest neighbor method; interaction term can be added into the matching model

matched.mi <- match.data(m.out)       # extract cases for each class
matched.mi[matched.mi$subclass==1,]   # extract cases for certain class

  # calculation with propensity score
clr.mi <- clogit(death ~ trt + strata(subclass), data=matched.mi) 
summary(clr.mi)

## Propensity score weighting (inverse probability) to balance fair distribution 
  # treatment effect by PS weighting method
with(mi, sum(death/ps*(trt==1)-death/(1-ps)*(trt==0))/nrow(mi))   # 95%CI=(-0.163, 0.033)

  # logistic regression modeling with propensity score s the only predictor to predict outcome
lr.ps <- glm(death~trt+ps, data=mi, family=binomial) 
summary(lr.ps)  # OR for treamtent1 is 0.611 (trt1 coefficient)

exp(coef(lr.ps)) 
exp(confint(lr.ps)) # 95%CI=(0.35, 1.06)


# SESSION 7 Meta-analysis ####################################################################################

## Missing Data
  # potential reasons: dropouts, incomplete response, censored
  # measures: 
    # missing completely at random (MCAR) - missingness ~! all vars (complete case analysis; nearest neighbor imputation; mean imputation)
    # missing at random (MAR) - missingness ~! unobserved vars (regression imputation; multiple imputation; inverse probability wrighting)
    # missing not at random (MNAR) - missingness ~ unobserved vars
  # Inverse probability weighting
    # pi percent of probability of being observed of a sample is weighted by 1/%

  # IPW example 1 - Create data with missing values
set.seed(123) # unify our results
n <- 500
bmi.m <- runif(n, 25,40)
bmi.f <- runif(n, 15,30)
bmi.full <- c(bmi.m, bmi.f)	# the combined dataset
pi <- rep(c(1,0.2), each=n)	# as probability of 100% and 20%
obs <- rbinom(2*n,1,pi) 	  # create observation of 1 with 20% chance; simulate the missing status
bmi.obs <- bmi.full[obs==1] # observations with response

par(mfrow=c(1,2))
hist(bmi.obs, breaks=c(15,20,25,30,35,40))
hist(bmi.full, breaks=c(15,20,25,30,35,40))

mean(bmi.obs)   # result=30.8 this estimate is biased due to missing (completed case analysis) values

  # Inverse calculation with weights (## Inverse Probability Weighting (IPW))
pi.obs <- pi[obs==1]  # result = 27.1
sum(bmi.obs/pi.obs)/(2*n) # weighted calculation

  # IPW example 2 - counterfactuals: assuming same individual receiving two different treatment

## ???
mi <- read.csv("examplemi.csv")
ps.model <- glm(trt ~ age + risk + severity, data=mi,
family=binomial)
mi$ps <- predict(ps.model, type='response')


## Meta Analysis
  # fixed effects model: 
    # assuming true effect of intervention is the same across studies. 
    # Homogeneous studies. 
    # outcome variable assumed to be normally distributed
  # random effects model: 
    # asumting true intervention effect of each study comes from a larger population.
    # Heterogenous studies
      # clinical diversity: Variability in participants, interventions and outcomes
      # methodological diversity: Variability in study design and risk of bias; need to control for confounders
      # statistical heterogeneity: 
        # Variability in the intervention effects across studies
        # Violate the assumption for fixed effects model
        # Random effects model allows the true effect to be different across studies
      # Clinical / methodological diversity should be addressed in the systematic review
      # Focus on statistical heterogeneity in the following slides (Cochrane Handbook for Systematic Reviews of Interventions, 2011)
      # Cochran's Q test: compare each estimate with their average. Larger Q indicates hetergeniety. Need large sample size.
      # Higgins' I^2: 0-30% low; 30-60% moderate; 50-90% substantial

  # effect size:
    # RR: relative risk - poisson regression 
    # OR: odds ratio - logistc regression
  # Inverse variance weighting
    # assign more weight to studies with higher precision (larger sample size)
    # the inverse of variance is roughly proportional to the sample size
    # the variance of the overall estiamte will be minimized

# RMA demonstration
ecig <- read.csv('exampleecig.csv')
  # OR: 1 being neutral; greater than 1 indicates useful of utilization of EC, while lower than 1 is useless
  # lower and upper bounds of 95%CI: 
require(metafor)

  # 1. Convert OR to log OR (beta in logistic regression; assumed to be normal) - yi in RMA
ecig$logOR <- log(ecig$OR)
  # 2. Derive the standard error for log OR from the 95% CI - sei in RMA
ecig$se.logOR <- (log(ecig$OR.ub)-log(ecig$OR.lb))/(2*1.96)
  # 3. Carry out meta analysis using fix effects model
ecig.fe <- rma(yi=logOR, sei=se.logOR, slab=study,method="FE", data=ecig) # FE - fixed effect; label "study"
  # 4. overall estimate 
# names(ecig.fe)  # show variable names
with(ecig.fe, exp(c(b, ci.lb, ci.ub)))  # the overall estimate is 0.77 (exp^b) with 95%CI of 0.69 to 0.87
  # 5. Heterogeneity
    # Clinical diversity – Variability in participants, interventions and outcomes 
    # Methodological diversity – Variability in study design and risk of bias 
    # Statistical heterogeneity – Variability in the intervention effects across studies; Violate the assumption for fixed effects model; Random effects model allows the true effect to be different across studies 
    # Cochran's Q (H0: no heterogeneity among studies; chi square)
    # Higgins' I2
      # df = k - 1
      # I2 ~ heterogeneity
      # low 0-30%; moderate 30-60%; substantial 50-90%

  # in fixed model
summary(ecig.fe)  # show Q indicator
Q <- ecig.fe$QE   # extract Q score
I2 <- (Q-(ecig.fe$k-1))/Q * 100  # high I2 indicates poor model; need random effect model

  # in random effect modeling 
ecig.re <- rma(yi=logOR, sei=se.logOR, slab=study,method="REML", data=ecig) # REML - random effect; having increased estiamtes and larger CI. 
with(ecig.re, exp(c(b, ci.lb, ci.ub)))    # overall estimate and CI (wider than that of fixed modeling)

forest(ecig.re, transf=exp, refline=1)    # "showweights=T";"transf=exp" - transformation from logit beta to coefficient; "refline=0" - reference line (log functions indicator lies on 1 for no neutral position)

## Asymmetry
  # Asymmetry test 1 - funnel plot: asymmetry may arise from multiple reasons
    # Reporting biases 
    # Poor methodological quality 
    # True heterogeneity 
    # Chances - missing studies; publication bias
funnel(ecig.re, atransf=exp)  # heterogeneity, biases; y axis: standard error

  # Asymmetry test 2 - regression test 
regtest(ecig.re)  # Egger's test; lowest sample size > 10; significance indicates asymmetry

  # Fix for asymmetry - trim and fill


# SESSION 8 Instrumental Variable Analysis ###############################################################
bmi = read.table("D:/OneDrive/research/2school/PhD Courses/CMED 6020 MMPH6117 Advanced Statistical Methods I/6 Instrumental Variable/examplebmiiva.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)

library("AER")  # ivreg() for 2SLS

# X: bmi_friend
# Y: bmi_adol
# Z: bmi_parent - instrumental variable

## IVA
  # outcome wth selection bias due to unobserved variable
# IV's characteristics: Z-> X -> Y (C contains observed and unobserved confounder)
  # associated with exposure X
  # uncorrelated with error
  # uncorrelated with unobserved confounders
  # X being the complete mediator between Z and Y
  # confounder: 
  # estimation of treatment effect: 
    # Wald estimator: Bxy = Bzy/Bzx; S.E. from Fieller's theorom
    # 2SLS: 2 stage least square
      # 1 stage: Z -> X 
      # 2 stage: X -> Y


  # Wald test: Bxy = Bzy/Bzx to obtain estiamtes
bmi.zy = lm(bmi_adol~bmi_parent,data=bmi)
bmi.zx = lm(bmi_friend~bmi_parent,data=bmi)
b.zy = coef(bmi.zy)[2]
b.zx = coef(bmi.zx)[2]
Wald = b.zy/b.zx      # calculate effect of x->y
  # Wald test: using Fieller's theorem to obtain variance, S.E.
    # vcov() to obtain variance and co-variance matrix, the same as standard error in the model
var.zy = vcov(bmi.zy)[2,2]    # coef(summary(bmi.zy))[4] - the fourth number in regression matrix 
var.zx = vcov(bmi.zx)[2,2]
var.xy = (b.zy/b.zx)^2*(var.zy/b.zy^2+var.zx/b.zx^2)  
sqrt(var.xy)    # SE sqrt from variance

## 2SLS; y ~ x|z; x endogenous var, z instrumental var; y ~ ex + en | ex + z 
bmi.2sls <- ivreg(bmi_adol~bmi_friend|bmi_parent, data=bmi) 
summary(bmi.2sls)   # conclusion: an increase of 1 kg/m2 in close friend’s average BMI is associated with 1.02 kg/m2 increase of an adolescent’s BMI, after controlled for unmeasured confounders 

## Quality of IV - Weak instrumental variable
  # weak instrument
    # IV correlate with unobserved determinants of outcome
      # cov(Z,e) != 0; IV associated with unobserved confounder 
      # no significant advantage over linear regression
    # weak correlation with treatment X
      # large variance in beta (x->y), CI
      # potential bias in estimation of beta (x->y)
  # Features
    # large variance in beta
    # potential bias in the estimation of beta
  # Tests
    # F-stats < 10 - weak instrument in treatment/endogenous regressor
    # Durbin-Wu-Hausman (DWH) for endogeneity of regressor
      # two models: basic regression model and 2SLS
      # (beta(IVA)-beta(LS))/sqrt(var(IVA)^2-var(LS)^2); if > 1.96, indicates IV better
      # H0: IV and LS both consistent, but LS (least square estimates) is efficient, following  normal distribution (IV being less efficient; try LS)
      # H1: Only IV is consistent; (IV should be used)
      # cannnot justify choice of IV
    # Mendelian randomization
      # genetic variant as the IV
        # random allocation of alleles (paternally and maternally inherited) 
        # hence not associated with any measured / unmeasured confounders 
        # gene is chosen to have known link to a phenotype of interest; genetic variance - number
      # weakness
        
  # F test
bmi.lm <- lm(bmi_friend~bmi_parent, data=bmi)  # summary(bmi.lm); F stats >> 10, indicating a good IV
bmi.lm0 <- lm(bmi_friend~1, data=bmi)          # model without IV 
anova(bmi.lm, bmi.lm0)                         # compare F in model with and without IV, F > 10 indicates IV useful


### Meta analysis #############################################################################
require(metafor)




# exercise 1 ##########################################
# mvc <- read.csv("http://web.hku.hk/~ehylau/mvc.csv") 

n = (1:10)
f1 = -3*n + 7
f2 = 2*(-0.5)^(n-1)
plot(x=n, y =f1)
plot(x=n, y =f2, type="l")

# 1. Using R, generate the first 10 terms of the arithmetic sequence an = -3n + 7 (a1 = 4, a2 = 1, a3 = -2,...)
An = function(x){
  f = 0
  n = 1
  ns = fs = c()
  for(n in 1:x){
    f = -3*n + 7
    print(paste("a",n," = ",f,sep=''))
    ns = cbind(ns,n)
    fs = cbind(fs,f)
    n = n + 1
  }
  plot(x = ns, y = fs, xlab="n", ylab="An",
      xlim = c(1,x), ylim = c(-3*x+7, -3*1+7), type = "l")
}


# 2. Using R, generate the first 10 terms of the geometric sequence gn = 2*(-0.5)^(n-1) (g1 = 2, g2 = -1, g3 = 0.5,...)
Gn = function(x){
  f = 0
  n = 1
  ns = fs = c()
  for(n in 1:x){
    f = 2*(-0.5)^(n-1) 
    print(paste("g",n," = ",f,sep=''))
    ns = cbind(ns,n)
    fs = cbind(fs,f)
    n = n + 1
  }
  plot(x = ns, y = fs, xlab="n", ylab="An",
  xlim = c(min(ns),max(ns)), ylim = c(min(fs),max(fs)), type = "l")
}

# exercise 2 ##########################################
# 1.
set.seed(1)
x = rnorm(10000,0,2)
y = rnorm(10000,2,3)
xy = rnorm(10000,2,5)
xy2 = x + y

par(mfrow=c(2,2))

hist(x, axes=FALSE, xlim=c(-12,12), ylim=c(0, 3000), font.lab=2, cex.lab=1.2, cex.main=1.5, col=grey(0.8),  xlab="X", ylab="Frequency", main="Histogram of X") 
hist(y, axes=FALSE, xlim=c(-12,12), ylim=c(0, 3000), font.lab=2, cex.lab=1.2, cex.main=1.5, col=grey(0.8),  xlab="Y", ylab="Frequency", main="Histogram of Y") 
plot(y ~ x, xlab="X", ylab="Y", xlim=c(-12,12), ylim=c(-12,12), cex=1.5, cex.lab=1.2, font.lab=2, font.axis=2, las=1) 
hist(xy, axes=FALSE, xlim=c(-12,12), ylim=c(0, 3000), font.lab=2, cex.lab=1.2, cex.main=1.5, col=grey(0.8),  xlab="X+Y", ylab="Frequency", main="Histogram of X+Y") 
hist(xy2, axes=FALSE, xlim=c(-12,12), ylim=c(0, 3000), font.lab=2, cex.lab=1.2, cex.main=1.5, col=grey(0.8),  xlab="X+Y(simulated)", ylab="Frequency", main="Histogram of x+Y") 

mean(x); sd(x)
mean(y); sd(y)
mean(xy); sd(xy)

# 2.
mvc <- read.csv("http://web.hku.hk/~ehylau/mvc.csv") 
ht <- cut(mvc$height,c(168,173), include.lowest=T, right=F) 
lm(mvc$MVC ~ mvc$age+ht)

label=c('155-167','168-172','173-180'), 


# Tutorial 1 ####################################################################################
# 1. Central limit theorem
# 1(a)
u0 <- runif(3,1,3)    # number of variables, mean, std
mean(u0)

# 1(b)
u1 <- matrix(runif(3*1000,1,3), nrow=3)
u1.mean <- colMeans(u1)

# 1(c)
mean(u1.mean)
var(u1.mean)

plot(density(u1.mean))
hist(u1.mean)

# 1(e)
u2 <- matrix(runif(30*1000,1,3), nrow=30)
u2.mean <- colMeans(u2)

mean(u2.mean)
var(u2.mean)

plot(density(u2.mean))
hist(u2.mean)

# 1(f)
hist(sample(1:5, 1000, replace=T, prob=c(0.1,0.35,0.1,0.35,0.1)), breaks=0:5, freq=F, main="")

b1 <- matrix(sample(1:5, 3*1000, replace=T, prob=c(0.1,0.35,0.1,0.35,0.1)), nrow=3)
b1.mean <- colMeans(b1)

plot(density(b1.mean))
hist(b1.mean, breaks=0:50/10)


b2 <- matrix(sample(1:5, 30*1000, replace=T, prob=c(0.1,0.35,0.1,0.35,0.1)), nrow=30)
b2.mean <- colMeans(b2)

plot(density(b2.mean))
hist(b2.mean, breaks=0:50/10)

# 1(g)
??normality

shapiro.test(u1.mean)
shapiro.test(u2.mean)

shapiro.test(b1.mean)
shapiro.test(b2.mean)

b3 <- matrix(sample(1:5, 50*1000, replace=T, prob=c(0.1,0.35,0.1,0.35,0.1)), nrow=50)
b3.mean <- colMeans(b3)

plot(density(b3.mean))
hist(b3.mean, breaks=0:50/10)

shapiro.test(b3.mean)

# 2. MVC
mvc <- read.csv("http://web.hku.hk/~ehylau/mvc.csv") 
summary(mvc)

# a) Define a variable which categorized the male alcoholics into younger (≤ 40y) and older adults (> 40y). Name the variable as “younger”. 
mvc$younger = ifelse(mvc$age <= 40,1,0)   # mvc$younger <- cut(mvc$age,c(min(mvc$age),40,max(mvc$age)), lab=c(1,0), include.lowest=T)
  # mvc$younger <- cut(mvc$age,c(min(mvc$age),40,max(mvc$age)), lab=c(1,0), include.lowest=T)

# b) Calculate the mean MVC for the two age groups
m1 = mean(mvc$MVC[mvc$younger==1])
m2 = mean(mvc$MVC[mvc$younger==0])  
  # aggregate(MVC~younger,by=list(mvc$younger),mean); aggregate(MVC~younger,data=mvc,mean)

t.test(MVC~younger,data=mvc)

# c) Draw a boxplot of MVC by age categories. 
boxplot(MVC~younger, data=mvc, xlab = 'Younger adults', ylab = 'MVC', main = 'Boxplot of MVC by age group' )

# d)  Draw a scatterplot between height and MVC. Add a linear regression line in the figure to show the relation. 
plot(mvc$height, mvc$MVC, xlab = 'Height', ylab = 'MVC', main='Scatter plot between Height and MVC')
abline(lm(MVC ~ height, data = mvc))

reg$coef[1]     # the first output of the coefficient in regression model

# e)  Draw a 2x2 panel of scatterplot showing the scatterplot and regression linear as in (d) for male alcoholics aged > 20, 30, 40 and 50y respectively. For comparison purpose, use the same limits for the x and y-axes. Please also label the figures. 
par(mfrow=c(2,2), mar=c(4,4,1,1))
age.lower <- 2:5*10

for (i in 1:4){
  plot(mvc$height, mvc$MVC, xlab = 'Height', ylab = 'MVC', type='n', las=1)
  temp.mvc <- mvc[mvc$age>age.lower[i], ]
  points(temp.mvc$height, temp.mvc$MVC, col=gray(0.7), pch=19)
  temp.lm.mvc <- lm(MVC ~ height, data = temp.mvc)
  abline(temp.lm.mvc)
  legend("topleft", paste('age > ', age.lower[i], 'y', sep=''))
}

  # type = n # nothing in the graph

# f)  Add the linear regression equations in the figures. 
par(mfrow=c(2,2), mar=c(4,4,1,1))
age.lower <- 2:5*10

for (i in 1:4){
  plot(mvc$height, mvc$MVC, xlab = 'Height', ylab = 'MVC', type='n', las=1)
  temp.mvc <- mvc[mvc$age>age.lower[i], ]
  points(temp.mvc$height, temp.mvc$MVC, col=gray(0.7), pch=19)
  temp.lm.mvc <- lm(MVC ~ height, data = temp.mvc)
  abline(temp.lm.mvc)
  legend("topleft", paste('age > ', age.lower[i], 'y', sep=''))
  legend("bottomright",paste('MVC =', format(round(temp.lm.mvc$coef[1],1),nsmall=1), 
    ' + ', format(round(temp.lm.mvc$coef[2],1),nsmall=1), 'age', sep=''), bty='n', text.font=2)
}

# example: by 5 years age groups
#windows(width=6, height=10)
pdf('d:/figure1.pdf', width=6, height=10)
par(mfrow=c(4,2), mar=c(4,4,1,1))
age.lower <- 4:11*5

for (i in 1:8){
  plot(mvc$height, mvc$MVC, xlab = 'Height', ylab = 'MVC', type='n', las=1)
  temp.mvc <- mvc[mvc$age>age.lower[i], ]
  points(temp.mvc$height, temp.mvc$MVC, col=gray(0.7), pch=19)
  temp.lm.mvc <- lm(MVC ~ height, data = temp.mvc)
  abline(temp.lm.mvc)
  legend("topleft", paste('age > ', age.lower[i], 'y', sep=''))
  legend("bottomright",paste('MVC =', format(round(temp.lm.mvc$coef[1],1),nsmall=1), 
' + ', format(round(temp.lm.mvc$coef[2],1),nsmall=1), 'age', sep=''), bty='n', text.font=2)
}
dev.off()

# g)  It is proposed that a quadratic relation between MVC and height may exist. Fit a linear regression to test this hypothesis.
lm.mvc <- lm(MVC ~ height, data=mvc)

mvc$height2 <- mvc$height^2
lm.mvc2 <- lm(MVC ~ height + height2, data=mvc)

summary(lm.mvc2)

# h) Compare the model with or without quadratic terms of height using AIC. 
AIC(lm.mvc, lm.mvc2) #AIC difference < 2 -> prefer the more parsimonious model

# i) Perform stepwise selection for the model with predictors age, height and height2
require(MASS)
lm.mvc3 <- lm(MVC ~ age + height + height2, data=mvc)
step.mvc <- stepAIC(lm.mvc3, direction="both")

summary(step.mvc)

# scope: specific the range of models to be selected
step.mvc2 <- stepAIC(lm.mvc3, direction="both", scope=list(lower=~height))
step.mvc3 <- stepAIC(lm.mvc3, direction="both", scope=list(lower=~height, upper=~age*height+age*height2))


# j)  Based on the fitted model with age and height (without the squared term) only as predictors, calculate the predicted MVC for a male alcoholic of age 50y and height of 170cm. 
lm.mvc4 <- lm(MVC ~ age + height, data=mvc)

new <- data.frame(age=50, height=170)
predict(lm.mvc4, new, interval="prediction")

# k): Predict the MVC for a male alcoholic with the same age but with height 220cm. Compare the prediction intervals with (j).
new2 <- data.frame(age=50, height=220)
predict(lm.mvc4, new2, interval="prediction")

new3 <- data.frame(age=50, height=150:220)
pred.mvc4 <- predict(lm.mvc4, new3, interval="prediction")

plot(new3$height, pred.mvc4[,"fit"], type='l', ylim=c(0,1000), xlab="height", ylab="predicted MVC", las=1)
lines(new3$height, pred.mvc4[,"lwr"], lty=2)
lines(new3$height, pred.mvc4[,"upr"], lty=2)
text(220, 1000, "at age 50y", adj=1, font=2)
polygon(c(rep(min(mvc$height),2),rep(max(mvc$height),2)),c(-50,1100,1100,-50), border=NA, col=rgb(0,0.5,0,0.2))


# Tutorial 2 ####################################################################################
flu <- read.csv("fluvaccine.csv")

# Q1 descriptive analysis
summary(flu)

# cross-tabulation between vaccination and flu infection
table(flu$vac, flu$flu)
prop.table(table(flu$vac, flu$flu),1)

with(flu,boxplot(shealth~vac))
with(flu,boxplot(shealth~flu))

# Q2 fitting a regression to estimate crude vaccination effect
lr.fv <- glm(flu~vac, data=flu, family=binomial)

summary(lr.fv)

exp(coef(lr.fv)["vac"])   # take coeffecient of variable "vac"; or coef(lr.fv)[2]
exp(confint(lr.fv)) # based on likelihood method
exp(confint.default(lr.fv)) # based on normality

# The crude vaccine effect is 32.6% (95% CI: 0-43.3%) protection

# Q3 confirming confounding effect of self-reported health
# standard rules
lr.vs <- glm(vac~shealth, data=flu, family=binomial)
lr.fs <- glm(flu~shealth, data=flu, family=binomial)

summary(lr.vs)
summary(lr.fs)

# change in estimate
lr.fv <- glm(flu~vac, data=flu, family=binomial)
lr.fvs <- glm(flu~vac+shealth, data=flu, family=binomial)

summary(lr.fv)
summary(lr.fvs)

(coef(lr.fvs)[2]-coef(lr.fv)[2])/coef(lr.fv)[2]     # >> 10%

# automatic variable selection
require(MASS)
stepAIC(lr.fvs)     # still changes of AIC after shealth is removed indicating confounding effects

# all 3 methods will select shealth as a confounder

# Q4 Stratification by shealth
flu$shealth.cat <- cut(flu$shealth, c(0,5,7,10), label=1:3, right=T, include.lowest=T)

lr.fv.strat <- list()
for (i in 1:3) {
lr.fv.strat[[i]] <- glm(flu~vac, data=flu, family=binomial, subset=(shealth.cat==i))
print(summary(lr.fv.strat[[i]]))
}

# approximate pooled estimate
(coef(lr.fv.strat[[1]])[2]+coef(lr.fv.strat[[2]])[2]+coef(lr.fv.strat[[3]])[2])/3

# Q5 Assessing potential confouder - suppose age is a suspected confounder
# standard rules
lr.va <- glm(vac~age, data=flu, family=binomial)
lr.fa <- glm(flu~age, data=flu, family=binomial)

summary(lr.va)
summary(lr.fa)

# there was no significant association between age and vaccination

# change in estimate
lr.fvsa <- glm(flu~vac+shealth+age, data=flu, family=binomial)

summary(lr.fvs)
summary(lr.fvsa)

(coef(lr.fvsa)[2]-coef(lr.fvs)[2])/coef(lr.fvs)[2]

# there was < 10% change in the vaccine effect estimate

# automatic variable selection
require(MASS)
stepAIC(lr.fvsa)

# Based on AIC, age will be selected (age is also significant)
# but no obvious association between age and vaccination
# similar results for BMI, smoking and sex
# antibody titers lie in the causal pathway from vaccination to infection

# Q6 effect modification by smoking and age
lr.fvs.s <- glm(flu~vac+shealth+vac*smoking, data=flu, family=binomial)
summary(lr.fvs.s)

lr.fvs.a <- glm(flu~vac+shealth+vac*age, data=flu, family=binomial)
summary(lr.fvs.a)

# there is evidence of the interaction effect between vaccination and smoking
# indicating the effect modification of smoking
# there is no significant age-vaccine interaction

# Q7 vaccination effect for smoker and non-smoker.
# non-smoker
exp(lr.fvs.s$coef["vac"])
exp(confint.default(lr.fvs.s)["vac",])

# AOR = 0.36, 95% CI = 0.23-0.58

# smoker
exp(lr.fvs.s$coef["vac"]+lr.fvs.s$coef["vac:smoking"])
se.vac.smoker <- sqrt(vcov(lr.fvs.s)[2,2]+vcov(lr.fvs.s)[5,5]+2*vcov(lr.fvs.s)[2,5])
# or alternatively, using matrix
# se.vac.smoker <- sqrt(t(c(1,1)) %*% vcov(lr.fvs.s)[c(2,5), c(2,5)] %*% c(1,1))
exp(lr.fvs.s$coef["vac"]+lr.fvs.s$coef["vac:smoking"] + c(-1,1)*qnorm(0.975)*se.vac.smoker)

# AOR = 0.96, 95% CI = 0.42-2.22

# Q8 assess collinearity 
require(car)
vif(lr.fvs.a)

# vif for vac and vac:age is large
flu$age.ct <- scale(flu$age, scale=F)

lr.fvs.act <- glm(flu~vac+shealth+vac*age.ct, data=flu, family=binomial)
summary(lr.fvs.act)

# cannot reduce collinearity with binary variable by centering

vif(lr.fvs.s)
# no collinearity problem for model with vaccine x smoking interaction

# Q9 antibody titer as mediator
flu$logabT <- log(flu$abT, base=2)

# antibody titer associated with vaccination?
lr.abTv <- glm(logabT~vac, data=flu)
summary(lr.abTv)

# antibody titer associated with flu infection?
lr.fabT <- glm(flu~logabT, data=flu, family=binomial)
summary(lr.fabT)

# vaccine associated with flu infection?
summary(lr.fvs.s)

# vaccine effect attenuated?
lr.fvsabT.s <- glm(flu~vac+shealth+logabT+vac*smoking, data=flu, family=binomial)
summary(lr.fvsabT.s)

# The data suggests that antibody titer is a mediator

# Q10 stepAIC - stepwise selection
# start from the basic model, to 
step.model <- stepAIC(lr.fv,
    scope = list(upper = ~vac+shealth+bmi+vac*age+vac*smoking, lower = ~vac))

# Q11 residual plot
final.model <- lr.fvs.s

resid <- rstudent(final.model)
plot(resid)

# Residual plots for logistic regression, along with binary variables are not straightforward
# to interpret

# Q12 ROC curve [for reference only] 
require(ROCR)
fm.pred <- predict(final.model, type="response")

# ROC curve
pred <- prediction(fm.pred, flu$flu)
perf <- performance(pred,"tpr","fpr")
plot(perf)

# AUROC
performance(pred,"auc")@y.values[[1]]

# Q13 Summarize results
exp(final.model$coef)
exp(confint.default(final.model))

round(exp(cbind(final.model$coef,confint.default(final.model))),2)

#### Tutorial 3 #############################################################################

fish = read.table("D:/OneDrive/research/2school/PhD Courses/CMED 6020 MMPH6117 Advanced Statistical Methods I/6.5 tutorial 3/fishconsumption.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)
require(metafor)

fish$strat <- 0
fish$strat[c(21, 22, 24, 25)] <- 1

fish$logrr <- log(fish$rr)
fish$se.logrr <- (log(fish$rr.ub)-log(fish$rr.lb))/(2*1.96)

library(metafor)

# fit a fixed effects model
fish.fe <- rma(yi=logrr, sei=se.logrr, slab=study, method="FE", 
data=fish, subset=strat==0)

fish.fe

# Overall estimate by fixed effects model
with(fish.fe, exp(c(b, ci.lb, ci.ub)))

# calculate the overall effect by inverse variance weighting
ivw.est <- with(fish[fish$strat==0,], sum(logrr/se.logrr^2)/sum(1/se.logrr^2))
ivw.var <- with(fish[fish$strat==0,], 1/sum(1/se.logrr^2))

# Q-statistics and I2
summary(fish.fe)

Q <- fish.fe$QE
I2 <- (Q-(fish.fe$k-1))/Q * 100


fish.re <- rma(yi=logrr, sei=se.logrr, slab=study, method="REML", 
data=fish, subset=strat==0)

# Overall estimate by random effects model
with(fish.re, exp(c(b, ci.lb, ci.ub)))

# forest plot based on the random effects model
forest(fish.re, transf=exp, refline=1)

# x axis in log scale, with weights
forest(fish.re, atransf=exp, refline=0, at=log(c(0.1,0.25,1,4,20)), showweights=T)

fish.re <- rma(yi=logrr, sei=se.logrr, slab=paste(study, year, country, sex, sep=", "), method="REML", 
data=fish, subset=strat==0)

forest(fish.re, atransf=exp, refline=0, at=log(c(0.1,0.25,1,4,20)), 
xlim=c(-6,5.5), showweights=T, xlab="Relative risk (log scale)")
text(-6, 28, "Study", pos=4, font=2)
text(5.5, 28, "Relative risk [95% CI]", pos=2, font=2)

# Funnel plot
funnel(fish.re, atransf=exp)

# Egger's test
regtest(fish.re)

# Meta regression on sex
fish$sex.spec <- 1*(fish$sex!="Both")
fish.sex <- fish[fish$sex.spec==1,]
fish.sex$sex <- droplevels(fish.sex$sex)

fish.re.sex <- rma(yi=logrr~sex, sei=se.logrr, slab=study, method="REML", 
data=fish.sex)

fish.re.sex

fish.re.year <- rma(yi=logrr~year, sei=se.logrr, slab=study, method="REML", 
data=fish, subset=strat==0)

fish.re.year 

# Sensitivity analysis
fish$lq <- 0
fish$lq[fish$study=="Smith"|fish$study=="Colangelo"] <- 1

fish.re.sen <- rma(yi=logrr, sei=se.logrr, slab=study, method="REML", 
data=fish, subset=(strat==0 & lq==0))

with(fish.re, exp(c(b, ci.lb, ci.ub)))
with(fish.re.sen, exp(c(b, ci.lb, ci.ub)))

# Leave one out sensitivity analysis
leave1out(fish.re, transf=exp)

# Trim and fill sensitivity analysis
fish.cut <- fish[fish$study!="Albanese",]

fish.cut.re <- rma(yi=logrr, sei=se.logrr, slab=paste(study, year, country, sex, sep=", "), method="REML", 
data=fish.cut, subset=strat==0)

funnel(fish.cut.re, atransf=exp)
regtest(fish.cut.re)

fish.cut.tf <- trimfill(fish.cut.re)
with(fish.cut.tf, exp(c(b, ci.lb, ci.ub)))

funnel(fish.cut.tf)


### Assignment 1 (submit to Moodle by 11:55pm, Mar 14, 2019) #######################################################################

### Association of invasive meningococcal disease and environmental and virologic exposure 

 

### Weekly counts of invasive meningococcal disease (IMD) reported cases from country A in 1997-2000 were collected and analyzed, to study associations between IMD and environmental and virologic exposure. The file IMD.csv stored the following variables which were found to be significant factors from the literature in other places: 

## flu: influenza isolation rate from laboratory 
## maxtemp: weekly maximum temperature 
## IMD: weekly number of IMD cases 
## pop: population size 

# Data:  
temp = read.table("C:/.../imd.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)


# a) Fit a poisson regression model to predict weekly IMD cases using flu and maxtemp, accounting for the increasing population size in 1997-2000. Summarize your results in a table.  [4 marks] 
imd = glm(imd~offset(log(pop))+flu+maxtemp, data=temp, family=poisson); 
summary(imd)
## Predictors of "flu" (p<0.001) and "maxtemp" (p<0.001) significantly predict the outcome variable (AIC = 21459)
exp(coef(imd))
exp(confint(imd))
## One unit of increase in influenza isolation rate results in 5 units of increase in weekly number of IMD cases; 
## one unit of increase in weekly maximum temperature results in 1 unit of increase in weekly number of IMD cases.

# b) Assess the goodness of fit of the model. [1 marks] 
deviance(imd)/df.residual(imd)
## The ratio of residual deviance to df is 98.21 >> 1, indicating undesirable goodness-of-fit. 
 

# c) Quote the mean and variance of the weekly IMD cases. Is there any evidence of overdispersion?  [1 marks] 
mean(temp$imd)/var(temp$imd)      # 0.00817
require(MASS)
summary(glm.nb(imd~1, data=temp))
## Overdispersion exists for two reasons: 1) variance is much greater; 2) Theta = 1.385; exp(4.91255)+exp(4.91255)^2/1.385=13487.7 while var(temp$imd)=16644.14

# d) Fit a negative binomial regression model to predict weekly IMD cases and summarize your results in a table. [4 marks] 
## is it necessary to have an offset term offset(log(pop)) in the model???
require(MASS)
nb.imd =  glm.nb(imd~offset(log(pop))+flu+maxtemp, data=temp)   
summary(nb.imd)    
round(exp(cbind(coef(nb.imd),confint(nb.imd))),3)

##                     2.5 %  97.5 %
## (Intercept) 89.270 58.331 138.424
## flu          5.832  0.792  42.873
## maxtemp      1.018  0.978   1.059
## One unit of increase in influenza isolation rate results in 5 units of increase in weekly number of IMD cases; 
## one unit of increase in weekly maximum temperature results in 1 unit of increase in weekly number of IMD cases.

deviance(nb.imd)/df.residual(nb.imd)    # The goodness-of-fit is 1.1266, changing from 98.22, reducing by 98.8%


# e) Assess the goodness of fit of the model. [1 marks] 
AIC(nb.imd)       # AIC=2453.2, greatly smaller than the original model (AIC=21461)
 

# f) Calculate the AIC for the poisson and negative binomial regression models and select the best model based on AIC and goodness of fit.  [1 marks] 
AIC(imd,nb.imd)

##        df       AIC
## imd     3 21461.423
## nb.imd  4  2453.243
## Negative binomial regression model is much smaller in AIC than poisson model; thus the model is more favored.


# g) Assess if the linear effects for the variables flu and maxtemp are adequate. [2 marks] 
pairs(temp)
## The scatter plot has shown that there is no clear linear effects for the variables flu and maxtemp.
 
resid.imd<-rstudent(nb.imd) 
par(mfrow=c(2,1)) 
plot(data$flu, resid.imd) 
plot(data$maxtemp, resid.imd)

# h) Assess if there is any collinearity problem in your final model? [2 marks] 
require(car)
vif(nb.imd)

##      flu  maxtemp
## 1.019897 1.019897
## VIF for "flu" and "maxtemp" are both 1.0199 << 10, indicating no significant collinearity issue in the model.


# i) Assess if there is any unexplained serial correlation after fitting your final model.  [2 marks] 
stres.nb.imd = rstudent(nb.imd)
plot(stres.nb.imd)
plot(temp$flu,stres.nb.imd)
plot(temp$maxtemp,stres.nb.imd)
## The scatter plot shows unexplained correlations 

 # plot(resid.imd) 
 # resid.imd<-rstudent(nb.imd) 
 # par(mfrow=c(2,1)) 
 # acf(resid.imd) 
 # pacf(resid.imd)


# j) Draw an overall conclusion on the findings. [3 marks] 

## The model of influenza isolation rate (IV1) and weekly maximum temperature (IV2) predicting weekly number of IMD cases (DV) fits better in the negative binomial regression. The model details are as follows: 1) The goodness-of-fit of negative binomial model is better than glm model with AIC greatly reduced to 2453. 2) The final model does not hold significant prediction of the outcome variable. 3) One unit of increase in influenza isolation rate (insignificantly) results in 5 units of increase in weekly number of IMD cases; one unit of increase in weekly maximum temperature (insignificantly) results in 1 unit of increase in weekly number of IMD cases. 4) There is no clear linear relationships between IVs and DV; 5) The model does not have collinearity problem. 
 

# k) Based on the final model, predict the number of IMD cases in a week, if the influenza isolation rate is 0.3, with a maximum temperature of 5oC and a population of 110 million.  [2 marks] 

(exp(coef(nb.imd)[1]) + exp(coef(nb.imd)[2])*0.3 + exp(coef(nb.imd)[3])*5)*110000000/mean(temp[,4])
## 94

newdata <- data.frame(flu=0.3,maxtemp=5, pop=110*1e6) 
predict(nb.imd, newdata, type="response") 

# l) Based on the final model, which of the conditions below will have a higher population risk of IMD, so more attention should be paid to suspected IMD cases for earlier treatment? [2 marks] 
# 1. Influenza isolation rate = 0.1, maximum temperature = 10oC 
# 2. Influenza isolation rate = 0.4, maximum temperature = -5oC 

(exp(coef(nb.imd)[1]) + exp(coef(nb.imd)[2])*0.1 + exp(coef(nb.imd)[3])*10)*110000000/mean(temp[,4])        # 97.98
(exp(coef(nb.imd)[1]) + exp(coef(nb.imd)[2])*0.4 + exp(coef(nb.imd)[3])*(-5))*110000000/mean(temp[,4])      # 84.73

## Condition 1 has a higher population risk of IMD.


# The predicted incidence rates are 114 and 147 per 100 million for conditions 1 and 2 respectively Therefore, condition 2 has a higher risk of IMD and more attention should be paid to suspected IMD cases during periods with similar conditions.  
pop.size <- 1e8 # just assume a population size 
newdata.cond <- data.frame(flu=c (0.1,0.4),maxtemp=c(10,-5), pop=pop.size) inc.cond <- round(predict(nb.imd, newdata.cond, type="response")/pop.size*1e8,0) 
	# or alternatively, Based on the final model, the predicted risk is given by E(Y)/pop = exp(-14.04 + 1.76 ∙ flu + 0.02∙ maxtemp 
exp(sum(coef(nb.imd)*c(1,0.1,10)))*1e8 exp(sum(coef(nb.imd)*c(1,0.4,-5)))*1e8 



# Assignment 2 ####################################################################################
# a) Calculate the crude mortality rates of patients receiving early and late oseltamivir treatment respectively. [2 marks] 
  # The crude death mortality of patients was 5.9% and 5.6% in late treatment and early treatment of oseltamivir respectively. 
h1n1 <- read.csv(file="h1n1pdm.csv") 
head(h1n1) 
with(h1n1,table(et,death)) 
round(with(h1n1, prop.table(table(et, death),1)),3) 

# b) Describe the patient characteristics across the two treatment groups. [3 marks] 
  # Patients receiving the early treatment tends to be older, more likely for women, more severe with the use of mechanic ventilation, more likely to have asthma condition, and more likely to have COPD condition. 
ftable(with(h1n1, table(male, et))) 
round(prop.table(ftable(with(h1n1, table(male, et))),2),3) 
ftable(with(h1n1, table(agegp, et))) 
round(prop.table(ftable(with(h1n1, table(agegp, et))),2),3) 
ftable(with(h1n1, table(mv, et))) 
round(prop.table(ftable(with(h1n1, table(mv, et))),2),3) 
ftable(with(h1n1, table(asthma, et))) 
round(prop.table(ftable(with(h1n1, table(asthma, et))),2),3) 
ftable(with(h1n1, table(copd, et))) 
round(prop.table(ftable(with(h1n1, table(copd, et))),2),3) 
ggplot(h1n1, aes(x=agegp, fill=factor(et))) + geom_bar() + facet_grid(et ~ .) 
ggplot(h1n1, aes(x=male, fill=factor(et))) + geom_bar() + facet_grid(et ~ .) 
ggplot(h1n1, aes(x=mv, fill=factor(et))) + geom_bar() + facet_grid(et ~ .) 
ggplot(h1n1, aes(x=asthma, fill=factor(et))) + geom_bar() + facet_grid(et ~ .) 
ggplot(h1n1, aes(x=copd, fill=factor(et))) + geom_bar() + facet_grid(et ~ .)

# c) Estimate the propensity score of receiving early oseltamivir treatment by including all available predictors as main effects in a logistic regression model. Comment on the main characteristics of patients receiving early treatment. [4 mark] 
  # Discuss both OR and p-value/95% CI for each factor. 
  # In general, patients receiving early treatment tend to be older, more severe with the use of mechanic ventilation and more likely to have COPD which were shown statistical significance at 5%
h1n1$et<-as.factor(h1n1$et) 
ps.model<-glm(et~factor(agegp)+male+mv+asthma+copd, data=h1n1,family=binomial) 
summary(ps.model) 
round(exp(cbind(coef(ps.model),confint(ps.model))),2) 
h1n1$ps<-predict(ps.model,type='response') 
ggplot(h1n1,aes(x=ps,fill=et))+geom_histogram(binwidth=0.05)+facet_grid(et~.)

# d) Propensity score stratification (by propensity score quintiles) will be used to analyze the effect of early treatment. Quote the propensity score quintiles. [2 marks] 
  # We divided the propensity score (i.e. the predicted mean from the logistic model) into five equal groups by the following cutoff points: 0.121, 0.227, 0.234 and 0.345.
ps.boundary<-quantile(h1n1$ps,0:5/5) 

#  e) Assess the balance of the patient characteristics and sample size across treatment groups by propensity score strata. Is there any potential way to improve the balance? [4 marks] 
  # The numbers of patients in each group were 307, 330, 205, 411 and 347 respectively. The percentages of patients with early treatment were 8.5%, 16.4%, 21.0%, 28.5% and 38.3% in each group. 
h1n1$psq=cut(h1n1$ps,ps.boundary,right=F,include.lowest=T,label=1:5) 
n.psq<-as.numeric(table(h1n1$psq)) 
n.psq 
round(with(h1n1, prop.table(table(psq, et),1)),3) 
  # The balance of patient characteristics across treatment groups are satisfactory for most variables, except there was some imbalance for asthma and COPD. The balance can be potentially improved by consider a more complicated propensity score model, such as including interaction terms

ftable(with(h1n1, table(et, agegp, psq)))[1:3,] 
round(prop.table(ftable(with(h1n1, table(et, agegp, psq)))[1:3,],2),3) 
ftable(with(h1n1, table(et, agegp, psq)))[4:6,] 
round(prop.table(ftable(with(h1n1, table(et, agegp, psq)))[4:6,],2),3) 

ftable(with(h1n1, table(et, male, psq)))[c(2,4),] 
round(prop.table(ftable(with(h1n1, table(et, male, psq)))[1:2,],2)[2,],3) 
round(prop.table(ftable(with(h1n1, table(et, male, psq)))[3:4,],2)[2,],3) 

ftable(with(h1n1, table(et, mv, psq)))[c(2,4),] 
round(prop.table(ftable(with(h1n1, table(et, mv, psq)))[1:2,],2)[2,],3) 
round(prop.table(ftable(with(h1n1, table(et, mv, psq)))[3:4,],2)[2,],3) 

ftable(with(h1n1, table(et, asthma, psq)))[c(2,4),] 
round(prop.table(ftable(with(h1n1, table(et, asthma, psq)))[1:2,],2)[2,],3) 
round(prop.table(ftable(with(h1n1, table(et, asthma, psq)))[3:4,],2)[2,],3) 

ftable(with(h1n1, table(et, copd, psq)))[c(2,4),] 
round(prop.table(ftable(with(h1n1, table(et, copd, psq)))[1:2,],2)[2,],3) 
round(prop.table(ftable(with(h1n1, table(et, copd, psq)))[3:4,],2)[2,],3)

# f) Calculate the stratum-specific and overall effect of early treatment (simple mean of the 5 strata). Please also provide the 95% CI for the overall treatment effect. [4 marks] 
  # The stratum-specific effectiveness are -0.0249, -0.0024, -0.0261, -0.0116 and -0.0183. The overall effectiveness is -0.0156 (early vs. late), the weighted mean of the stratum-specific effectiveness. The 95% CI for the overall effectiveness is (-0.0402, 0.0091). 
mean.tp<-aggregate(death~et+psq,data=h1n1,FUN=mean) 
count.tp<-aggregate(death~et+psq,data=h1n1,FUN=length) 
cbind(mean.tp,count.tp$death) 

  # strata specific mean 
str.mean <- mean.tp$death[mean.tp$et==1] - mean.tp$death[mean.tp$et==0] str.mean  

  # overall weighted mean 
overall <- sum((mean.tp$death[mean.tp$et==1] - mean.tp$death[mean.tp$et==0])*n.psq)/sum(n.psq) overall 

  # 95% CI 
var.tp <- aggregate(death~et+psq, data=h1n1, FUN=var) 
var.over <- sum((var.tp$death/count.tp$death)*rep(n.psq, each=2)^2)/sum(n.psq)^2 
lowb <- overall - 1.96*sqrt(var.over) 
uppb <- overall + 1.96*sqrt(var.over) 
print(c(lowb, uppb)) 

# g) Estimate the overall treatment effect using conditional logistic regression. [3 marks] 
  # The odds ratio of the overall treatment effect (early vs. late) is 0.77 (0.46, 1.28). 
library(survival) 
clog.h1n1<-clogit(death~et+strata(psq),data=h1n1) 
summary(clog.h1n1) 

# h) Based on the above results, comment on the effectiveness of early treatment of oseltamivir in reducing mortality and validity of the results in terms of control for confounders. [3 marks] 
 
  # The overall effectiveness of 1.6% indicates that the death rate in late treatment is higher than in early treatment.  
  # However, the corresponding 95% CI indicates that the difference is not statistically significant 
  # The patient characteristics have been satisfactorily balanced across treatment groups for most variables and should have controlled for most of the confounding effects from observed factors. 
  # Residual confounding may not be fully controlled by the propensity score method.  
 