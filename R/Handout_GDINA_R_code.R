#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%                                                                                 %#
#%                 R code for Cognitive Diagnosis Modeling:                        %#
#%           A General Framework Approach and Its Implementation in R              %#
#%                   National Taichung University of Education                     %#
#%                                                                                 %#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


####################################################################################
#                                                                                  #
#                               4 Model Estimation                                 #
#                                                                                  #
####################################################################################

# D:/OneDrive/research/1personal/Programming/R/CDM   # Lenovo X1

# Load the GDINA package
library(GDINA)
# data
data1 <- read.table(file = "D:/OneDrive/research/1personal/Programming/R/CDM/data1.dat",header = TRUE)
head(data1)

# Q-matrix
Q1 <- read.table(file = "D:/OneDrive/research/1personal/Programming/R/CDM/Q1.txt")
Q1
#####################################
#
# 4.1 Estimation of the G-DINA Model
#
#####################################
#Fit the data using G-DINA model
fit1 <- GDINA(dat = data1, Q = Q1)      # default model = GDINA
fit1
summary(fit1)

#To extract item parameters, we can use coef function, as in
coef(fit1)

# item success probabilities for item 15
coef(fit1, withSE = TRUE)[[15]]                     # show possibility of each item

# delta parameters for item 15
coef(fit1, what = "delta", withSE = TRUE)[[15]]     # with standard error at the item of 15; delta the default interpretation (summation of all methods)

# population proportions when saturated model
# is used for joint attribute distribution
coef(fit1, what = "lambda")                         # high order structure

#To plot the probabilities of success for each item
plot(fit1, what = "IRF", item = c(9,15))

plot(fit1, what = "IRF", item = 15, withSE = TRUE)  # SE shown in CI

#Individual attribute patterns
head(personparm(fit1))                              # 
att.est = personparm(fit1)  


#MAP estimates    # paper - 
head(personparm(fit1, what = "MAP"))  

#MLE estimates
head(personparm(fit1, what = "MLE"))

#probability of mastering each attribute
head(personparm(fit1, what = "mp"))

plot(fit1, what = "mp", person = c(1,15,20,25))     # compare 4 examinees with methods of mp 

extract(fit1,"discrim")                             # discriminative index for each item

extract(fit1,"posterior.prob")                      # same to lambda method in this case

#################################
#
# 4.2 Estimation of Reduced CDMs
#
#################################
fit2 <- GDINA(dat = data1, Q = Q1, model = "DINA")
fit2

# guessing and slip
coef(fit2, what = "gs",withSE = TRUE)               # guessing method

# Fit RRUM to the data
fit3 <- GDINA(dat = data1, Q = Q1, model = "RRUM")

# print delta parameters
coef(fit3, what = "delta")

# pi and r - original parameterization for RRUM
coef(fit3, what = "rrum")                           # r,j,k


# Fit different CDMs to different items
models <- c(rep("GDINA",4),"LLM","ACDM","GDINA", "LLM","RRUM","ACDM","RRUM","ACDM","DINA","RRUM","RRUM")

fit5 <- GDINA(dat = data1, Q = Q1, model = models)
fit5

####################################################################
#
#4.3 Modeling Joint Attribute Distribution Using Higher-order Models
#
####################################################################
# higher-order G-DINA model (By default: Rasch model)
HOfit1 <- GDINA(dat = data1, Q = Q1, model = "GDINA", att.dist = "higher.order")

# print higher-order structural parameters
coef(HOfit1,"lambda")

# print higher-order person ability EAP estimates
head(personparm(HOfit1,"HO"))

HOfit2 <- GDINA(dat = data1, Q = Q1, model = "GDINA", att.dist = "higher.order",
                higher.order = list(model = "2PL"))

####################################################################################
#                                                                                  #
#                               5 Model Fit Evaluation                             #
#                                                                                  #
####################################################################################

# Load the GDINA package
library(GDINA)
data1 <- read.table(file = "data1.dat",header = TRUE)
# Q-matrix
Q1 <- read.table(file = "Q1.txt")
# Fit the DINO model
# verbose can be used to control what
# to be printed during the estimation
fit1 <- GDINA(dat = data1, Q = Q1, model = "DINO", verbose = 0)

summary(fit1)

logLik(fit1)

deviance(fit1)

AIC(fit1)

BIC(fit1)

npar(fit1)

ifit <- itemfit(fit1)
ifit
summary(ifit)
plot(ifit)

# fit1 is based on the DINO model
# fit2 is based on the G-DINA model
fit2 <- GDINA(dat = data1,Q = Q1, model = "GDINA",verbose = 0)
anova(fit1,fit2)

# Fit different CDMs to different items
models <- c(rep("GDINA",4),"LLM","ACDM","GDINA", "LLM","RRUM","ACDM","RRUM","ACDM","DINA","RRUM","RRUM")


fit3 <- GDINA(dat = data1, Q = Q1, model = models)

anova(fit1,fit2,fit3)


####################################################################################
#                                                                                  #
#                               6 Model Comparison                                 #
#                                                                                  #
####################################################################################

## comparison can be done between nested models. models, in fact, are compared with saturated models


data1 <- read.table(file = "data1.dat",header = TRUE)
Q1 <- read.table(file = "Q1.txt")
fit1 <- GDINA(dat = data1,Q = Q1,verbose = 0)



mc <- modelcomp(fit1)
mc

# Wald test statistics and p values (efficient comparison in contrast to LR)
extract(mc, what = "stats")         # baseline and basic factor items are static
extract(mc, what = "pvalues")
# N0: no difference between the simple modle and saturated model
# N1: difference between ... 
# the largest number of p value, e.g. LLm to choose for item 5
# if all p values are significant, GDINA model should be selected


#degrees of freedom
extract(mc, what = "df")
mc2 <- modelcomp(fit1,item = c(4,7,10),models=c("DINA","LLM"))
mc2

####################################################################################
#                                                                                  #
#                               7 Q-matrix Validation                              #
#                                                                                  #
####################################################################################

# Load the GDINA package
library(GDINA)

data1 <- read.table(file = "D:/OneDrive/research/1personal/Programming/R/CDM/data1.dat",header = TRUE)
# Q-matrix
Q1 <- read.table(file = "D:/OneDrive/research/1personal/Programming/R/CDM/Q1.txt")
#Fit the data using G-DINA model
fit <- GDINA(dat = data1,Q = Q1, model = "GDINA",verbose = 0)

# Conduct Q-matrix validation # The default eps = 0.95
Qvalid <- Qval(fit)

# Print the suggested Q-matrix
Qvalid                            # the attributes with asterisks are the critical change point
plot(Qvalid, item = 10)           # red dot represents the origin point versus the critical change； modified q-vectors can be used

# print varsigma^2
t(extract(Qvalid, what = "varsigma"))   # transpose of matrix - t()

# print PVAF
t(extract(Qvalid, what = "PVAF"))


####################################################################################
#                                                                                  #
#   8 Differential Item Functioning, Classification Accuracy and Data Simulation   #
#                                                                                  #
####################################################################################

# ----------8.1 Differential Item Functioning-------------

gr <- c(rep(1,400),rep(2,437))

Q1 <- read.table(file = "D:/OneDrive/research/1personal/Programming/R/CDM/Q1.txt")
data1 <- read.table(file = "D:/OneDrive/research/1personal/Programming/R/CDM/data1.dat",header = TRUE)

# N0: no difference
# N1: difference

difout <- dif(dat = data1, Q = Q1, group = gr)
difout

difout2 <- dif(dat = data1, Q = Q1, group = gr,model="DINA")    # Wald test
difout2

difout3 <- dif(dat = data1, Q = Q1, group = gr, model="DINA",method = "LR")   # LR test
difout3

# -------- 8.2 Classification accuracy------------

fit1 <- GDINA(dat = data1, Q = Q1)        
CA(fit1)

# ------------ 8.3 Data Simulation --------------

# # -- guessing and slip parameters for each item
# # need to be specified in a matrix or data frame
# # of dimension J x 2 (column 1 is guessing and
# # column 2 is slip)
N <- 5000
Q <- Q1
J <- nrow(Q)
gs <- data.frame(guess = rep(0.1,J),slip = rep(0.2,J))
gs

# Simulated DINA model
set.seed(12345)
sim <- simGDINA(N,Q,gs.parm = gs,model = "DINA")

dat <- extract(sim, what = "dat")

att <- extract(sim, what = "attribute")

# fit DINA model
fitsim <- GDINA(dat,Q,model = "DINA",verbose = 0)
# evaluate classification rates
ClassRate(att,personparm(fitsim))

m <- c(rep("DINA",3),rep("DINO",3),
       rep("ACDM",3),rep("LLM",2),
       rep("RRUM",2),rep("GDINA",2))
set.seed(12345)
sim2 <- simGDINA(N, Q, gs.parm = gs, model = m)
# print item parameters
extract(sim2, what = "catprob.parm")

fit1 <- GDINA(data1,Q1,verbose = 0)
ip <- coef(fit1)
delta <- coef(fit1,"delta")

set.seed(12345)
N <- 3000
K <- 3
#higher-order person ability
theta <- rnorm(N)
#higher-order attribute parameters
lambda <- data.frame(a=rep(1,K),b=rnorm(K))
lambda

sim2 <- simGDINA(N,Q1,catprob.parm = ip,
                 att.dist = "higher.order",
                 higher.order.parm = list(theta = theta,lambda = lambda))
sim3 <- simGDINA(N,Q1,delta.parm = delta,
                 att.dist = "higher.order",
                 higher.order.parm = list(theta = theta,lambda = lambda))


####################################################################################
#                                                                                  #
#                           9 Answers to Exercises                                 #
#                                                                                  #
####################################################################################

#-------------------------
# 9.1 Answers to Section 4
#-------------------------

library(GDINA)
data2 <- read.table(file = "D:/OneDrive/research/1personal/Programming/R/CDM/data2.dat", header = TRUE)
# Q-matrix
Q2 <- read.table(file = "D:/OneDrive/research/1personal/Programming/R/CDM/Q2.txt")

exefit1 <- GDINA(dat = data2, Q = Q2,
                 model = "GDINA",verbose = 0)

coef(exefit1,withSE = TRUE)[[15]]

extract(exefit1,"posterior.prob")

summary(exefit1)

plot(exefit1,item = c(5,8,15))

personparm(exefit1)[3,]

exefit2 <- GDINA(dat = data2, Q = Q2, model = "LLM")
summary(exefit2)
npar(exefit2)

exefit3 <- GDINA(dat = data2, Q = Q2, model = "DINA")
coef(exefit3,"gs",withSE = T)

exefit4 <- GDINA(dat = data2, Q = Q2, model = "DINA", att.dist = "higher.order",verbose = 0)
coef(exefit4,"lambda")

#-------------------------
#Answers to Section 5
#-------------------------

data2 <- read.table(file = "data2.dat",header = TRUE)
Q2 <- read.table(file = "Q2.txt")
fit.dina <- GDINA(data2,Q2,model="DINA")
fit.dino <- GDINA(data2,Q2,model="DINO")
fit.gdina <- GDINA(data2,Q2,model="GDINA")

ift.dina <- itemfit(fit.dina)
ift.dino <- itemfit(fit.dino)
ift.gdina <- itemfit(fit.gdina)
ift.dina
ift.dino
ift.gdina

plot(ift.dino, adjusted = FALSE)

AIC(fit.dina)
AIC(fit.dino)
AIC(fit.gdina)

BIC(fit.dina)
BIC(fit.dino)
BIC(fit.gdina)

anova(fit.dina,fit.dino,fit.gdina)

fittedCDM <- c(rep("GDINA",4),"LLM","DINA","ACDM","DINO","LLM","LLM",
               "RRUM","LLM","ACDM","ACDM","RRUM")

exefit <- GDINA(data2,Q2,model=fittedCDM)
npar(exefit)
itemfit(exefit)
anova(exefit,fit.gdina)


#-------------------------
#Answers to Section 6
#-------------------------

data2 <- read.table(file = "data2.dat", header = TRUE)
Q2 <- read.table(file = "Q2.txt")

exefit2 <- GDINA(dat = data2, Q = Q2, model = "GDINA")
exemcl <- modelcomp(exefit2)

p <- extract(exemcl,"pvalues")
# which is max p-value
m <- apply(p,1,function(x){
  if(all(x<.05)){
    return(0)
  }else{
    return(which.max(x))
  }})
# create model
model <- c(rep(0,4),m)

exefit3 <- GDINA(data2,Q2,model = model)
itemfit(exefit3)

anova(exefit2,exefit3)

#-------------------------
#Answers to Section 7
#-------------------------

library("GDINA")
data2 <- read.table(file = "data2.dat", header = TRUE)
Q2 <- read.table(file = "Q2.txt")
fit <- GDINA(data2,Q2)

Qv <- Qval(fit)
Qv

Qv2 <- Qval(fit,eps=0.9)
Qv2

plot(Qv,item = 5)
plot(Qv2,item = 5)

new.Q <- extract(Qv2,"sug.Q")
new.fit <- GDINA(data2,new.Q,verbose = 0)
itemfit(fit)
itemfit(new.fit)

AIC(fit)
AIC(new.fit)


BIC(fit)
BIC(new.fit)

#-------------------------
#Answers to Section 8
#-------------------------


library("GDINA")
data2 <- read.table(file = "data2.dat", header = TRUE)
Q2 <- read.table(file = "Q2.txt")

fit <- GDINA(data2,Q2)
CA(fit)

gs <- matrix(0.2,nrow = 15, ncol = 2)
sim <- simGDINA(N = 500, Q = Q2, gs.parm = gs, model = "GDINA")
simdat <- extract(sim,"dat")

gr <- c(rep(1,400),rep(2,437))
difout <- dif(dat = data2, Q = Q2, group = gr, model = "GDINA")
difout

#-------------------------
#Answers to Section 9
#-------------------------

library(GDINA)
#Reading the data
data3 <- read.table(file="D:/OneDrive/research/1personal/Programming/R/CDM/data3.dat")
Q3 <- read.table(file="D:/OneDrive/research/1personal/Programming/R/CDM/Q3.txt")

#Fit the GDINA model with monotonicity constraint (selection of one with higher order)
mod <- GDINA(dat=data3,Q=Q3,model="GDINA",mono.constraint = TRUE,verbose = 0)  # saturated model
summary(mod)
mod

#Preliminary Item Fit Evaluation
itemfit_initial <- itemfit(mod, p.adjust.methods = "bonferroni")
plot(itemfit_initial)                                                 # heat map: item 1 highly correlated with many other items

#Perform Q-matrix validation using eps=0.9
Q3valid <- Qval(mod, eps = 0.9)                                       
Q3valid

#Examine the mesaplots for the items with suggested new q-vectors
plot(Q3valid,item=1,data.label=FALSE,type="best", eps = 0.9)          # 0101 # modify q matrix based on q3valid result
plot(Q3valid,item=3,data.label=FALSE,type="best", eps = 0.9)          # 0100 (being same with originals)
plot(Q3valid,item=20,data.label=FALSE,type="best", eps = 0.9)         # 0010 (the same)
plot(Q3valid,item=33,data.label=FALSE,type="best", eps = 0.9)         # 0001 no original point recorded

#We only need to change the q-vector for items 1 and 33
Q3[1,] <- c(0,1,0,1)                                                  # modeify q matrix 
Q3[33,] <- c(0,0,0,1)
#we modified the provisional Q-matrix

#Recalibrate the model using the modified Q-matrix
mod_rev <- GDINA(dat=data3,Q=Q3,model="GDINA",mono.constraint = TRUE,verbose = 0)
summary(mod_rev)
mod_rev

#Examine the plot of the IRF of Item 16
plot(x = mod_rev, what="IRF", item=16)

#Modify the model for Item 16 and fit it to the data again
models2 <- c(rep("GDINA",15),"DINO",rep("GDINA",17))                  # check item 16 
mod_rev2 <- GDINA(dat=data3,Q=Q3,model=models2,mono.constraint = TRUE,verbose = 0)
summary(mod_rev2)
mod_rev2

#Check absolute fit - item 16 does not fit
itemfit2 <- itemfit(mod_rev2)                                         # DINO not suitable for item 16
plot(itemfit2)
#misfitting item 16

#Check absolute fit of the fitted GDINA model (for all items)
itemfit <- itemfit(mod_rev)
plot(itemfit)
#all items fit based on adjusted pvalues

#Fit reduced CDMs
mod_dina <- GDINA(dat=data3,Q=Q3, model="DINA", mono.constraint = T,verbose = 0)
mod_dino <- GDINA(dat=data3,Q=Q3, model="DINO", mono.constraint = T,verbose = 0)
mod_acdm <- GDINA(dat=data3,Q=Q3, model="ACDM", mono.constraint = T,verbose = 0)
mod_rrum <- GDINA(dat=data3,Q=Q3, model="RRUM", mono.constraint = T,verbose = 0)
mod_llm <- GDINA(dat=data3,Q=Q3, model="LLM", mono.constraint = T,verbose = 0)

# Perform Model Comparison ******
anova(mod_dina, mod_dino, mod_acdm, mod_rrum, mod_llm, mod_rev)     # mod-rev - saturated model
  # GDINA better than these five reduced models
  # simple models are all rejected, while saturated model is selected

# Wald Test for Item Fit Evaluation
wald_rev <- modelcomp(mod_rev)               # tests specified models for each items
wald_rev

#Fit the model based on Wald test
models_wald <- c("LLM","ACDM","GDINA","ACDM","GDINA",
                 "GDINA","ACDM","GDINA","GDINA","GDINA",
                 "GDINA","ACDM","LLM","GDINA","GDINA",
                 "ACDM","GDINA","GDINA","ACDM","GDINA",
                 "GDINA","GDINA","GDINA","GDINA","LLM",
                 "GDINA","LLM","GDINA","GDINA","GDINA",
                 "GDINA","GDINA","GDINA")

mod_wald <- GDINA(dat=data3,Q=Q3,model=models_wald,
                  mono.constraint = T,verbose = 0)

# Examine the model fit of the modified model based on Wald test
itemfit_wald <- itemfit(mod_wald)
plot(itemfit_wald)
# all items fit based on adjusted pvalues

# Perform model comparison
anova(mod_wald,mod_rev)       # no significant difference, thus simple model (Wald test can be retained)

# Perform DIF using Wald and LR tests
gr <- c(rep(1,600),rep(2,610))

difout_wald <- dif(dat=data3,Q=Q3,group=gr,method="wald",mono.constraint=T)
difout_wald

difout_LR <- dif(dat=as.matrix(data3),Q=Q3,group=gr,method="LR",
                 mono.constraint=T, LR.type = "free.all",
                 difitem = 7)
difout_LR
  # item 4 has signiifcant difference

# classification accuracy
CA_all <- CA(mod_wald)        # 
CA_all

# Item Selectioon

# Discrimination Index
disc <- extract(mod_wald,"discrim")     # find the most and least discriminating items; 

# Deleting two low discrminating items
data_low <- data3[,c(-3,-20)]
Q_low <- Q3[c(-3,-20),]

#Refitting the data without items 3 and 20
mod_low <- GDINA(dat=data_low,Q=Q_low,model=models_wald[c(-3,-20)],
                 mono.constraint = TRUE,verbose = 0)

#Classification rate
CA_low <- CA(mod_low)                   # compare with original accuracy rate (0.82) and has a small drop
CA_low

# Deleting two highly discrminating items; the accurary rate drops more
data_high <- data3[,c(-1,-2)]
Q_high <- Q3[c(-1,-2),]

#Refitting the data without items 1 and 2
mod_high <- GDINA(dat=data_high, Q=Q_high, models=models_wald[c(-1,-2)],
                  mono.constraint = TRUE, verbose=0)

#classification Rate
CA_high <- CA(mod_high)
CA_high

#Test-level Classification Accuracy
round(rbind("all"=CA_all$tau,
            "no low"=CA_low$tau,
            "no high"=CA_high$tau),2)

#Attribute-level Classification Accuracy
round(rbind("all"=CA_all$tau_k,
            "no low"=CA_low$tau_k,
            "no high"=CA_high$tau_k),2)

#Pattern-level Classification Accuracy
round(rbind("all"=CA_all$tau_l,
            "no low"=CA_low$tau_l,
            "no high"=CA_high$tau_l),2)

  # the changes of accuracy rates after drop of highly and lowly discriminating items