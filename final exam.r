# Q2

msm <- read.csv("exam_question2.csv")

# a
msm$logrr <- log(msm$prev)
msm$se.logrr <- (log(msm$prev.ub)-log(msm$prev.lb))/(2*1.96)

msm.fe <- rma(yi=logrr, sei=se.logrr, slab=study, method="FE", data=msm)

# Overall estimate by fixed effects model
with(msm.fe, exp(c(b, ci.lb, ci.ub)))

# b
Q <- msm.fe$QE   # extract Q score
I2 <- (Q-(msm.fe$k-1))/Q * 100  # high I2 indicates poor model; need random effect model

# c

msm.re <- rma(yi=logrr, sei=se.logrr, slab=study, method="REML", data=msm)
with(msm.re, exp(c(b, ci.lb, ci.ub)))

# d
forest(msm.re, transf=exp, refline=1,order=msm.re$year) 

# e
funnel(msm.re, atransf=exp)
regtest(msm.re)

# f

msm.cut <- msm[msm$year>2010,]

msm.cut.re <- rma(yi=logrr, sei=se.logrr, slab=paste(study, year, country, sep=", "), method="REML", data=msm.cut)

funnel(msm.cut.re, atransf=exp)
regtest(msm.cut.re)

msm.cut.tf <- trimfill(msm.cut.re)
with(msm.cut.tf, exp(c(b, ci.lb, ci.ub)))



####################################################################################################3
# Q3
bh <- read.csv("exam_question3.csv")

# a
summary(lm(poor.bone~low.calc, data=bh))

# b
pairs(bh)

summary(glm(poor.bone~phy.index, data=bh, family=binomial))
summary(glm(low.calc~phy.index, data=bh, family=binomial))

# c
summary(lm(poor.bone~low.calc*male,data=bh))
summary(lm(poor.bone~low.calc+male,data=bh))

summary(lm(poor.bone~low.calc*age,data=bh))
summary(lm(poor.bone~low.calc+age,data=bh))

# d
bh.d=summary(lm(poor.bone~low.calc*male+age+phy.index+bmi+low.income, data=bh))

hb = as.vector(coef(bh.d)[,1]+coef(bh.d)[,2]*1.96)
lb = as.vector(coef(bh.d)[,1]-coef(bh.d)[,2]*1.96)

cbind(coef(bh.d),lb,hb)

# e
summary(lm(poor.bone~low.calc+age+phy.index+bmi+low.income, data=bh, subset=(male==1)))
summary(lm(poor.bone~low.calc+age+phy.index+bmi+low.income, data=bh, subset=(male==0)))

# f
bh.f=lm(poor.bone~low.calc*male+age+phy.index+bmi+low.income, data=bh)

data <- data.frame(age=10,male=1, phy.index=65,bmi=mean(bh$bmi),low.income=1,low.calc=1)
predict(bh.f, data, interval="prediction")


