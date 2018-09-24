work = read.table("D:/OneDrive/research/1personal/social work students/path analysis/180310.txt",header=T,sep="\t",skipNul = T)
a = work
# View(a)


# level b: bsoc - social, bpsy - psychological, bmot - lack of motivation
# level d: dsw - extending social welfare, dpt - psychotherapeutic treatment, dms - minimising state assitance
# level e: esoc - society oriented, eind - individual oriented
# level ij - willingness to engage in policy practice 



library(RAMpath)

attach(a)
model1 = '          # full model 
ij ~ eind + esoc
eind + esoc ~ dsw + dpt + dms
dsw + dpt + dms ~ bsoc + bpsy + bmot
'

model2 = '          # model in article
ij ~ eind + esoc
eind ~ dsw
esoc ~ dpt
dsw ~ bsoc
dpt ~ bsoc + bpsy
dms ~ bmot
'


model2.1 = '          # model in article
ij ~ eind + esoc
eind ~ dsw
dsw ~ bsoc
dpt ~ bsoc + bpsy
'


model3 = '          # tested model 
ij ~ eind + esoc
eind + esoc ~ dsw + dpt + dms
dsw + dpt + dms ~ bsoc + bpsy + bmot + csoc + cpsy + cmot
'

detach(a)


### path analysis
# fit = sem(model, data = a)
# summary(fit, fit.measures = T)

# model comparison
fit1 = sem(model1, data = a)
fit2 = sem(model2, data = a)
fit2.1 = sem(model2.1, data = a)
fit3 = sem(model3, data = a)

anova(fit1,fit2,fit2.1,fit3)  # model 2.1 and model 3 can be further compared
anova(fit2.1,fit3)

################ Stage 2 ###############
### change: participate in policy practice - i+j5 as dependent variable


a[,"dv"] = a[,"j5"]


attach(a)


model4 = '         
dv ~ eind + esoc
eind ~ dsw
esoc ~ dpt
dsw ~ bsoc
dpt ~ bsoc + bpsy
dms ~ bmot
'

model5 = '         
dv ~ eind + esoc
eind ~ dsw
dsw ~ bsoc
dpt ~ bsoc + bpsy
dms ~ bmot
'

detach(a)

fit4 = sem(model4, data = a)
summary(fit4) # output
anova(fit1,fit4)  # compare with full model
