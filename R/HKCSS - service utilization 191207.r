### Notes
# dementia, non-dementia
# male, female
# met/unmet needs
# generations

### HKCSS data on Unmet Need of Caregivers ###
library(xlsx);
library(outreg);
library(plyr);
library(psych);
library(stargazer);
library(interplot);
library(Hmisc);
# library(VIM)      # visualization of missing data
   # aggr(d,prop=F,numbers=T)
   # matrixplot(d)
############### add number of caregivers

data = work = read.csv("C:/Users/chens/OneDrive/research/Projects/4 HKCSS/191122 HKCSS.csv",header=T,na.strings = "NA")  # Office - Dell Inspiron 16
# data = read.csv("D:/OneDrive/research/Projects/4 HKCSS/190908 HKCSS (no missing).csv",header=T,na.strings = "NA")  # Office - Dell Inspiron 16

## Var names
data$CMon=data$CGMon
data$CF=data$B13     # CG frequency
data$comb=data$B16f
data$dr=data$C12     # dyadic relation
data$fr=data$C10T   # family relation

# with adult children caregivers 
ac = data[data$CRtype2=="2" | data$CRtype2=="3",]     # 742

w = ac[ac$workCG=="1",]     # 494  
nw = ac[ac$workCG=="0",]    # 252

# dementia population among working caregivers
d  = w[w$B16b=="1",]    # 181
nd = w[w$B16b=="0",]    # 309


# ggpairs(data = d, columns = 2:10, title = "bivariates")


### Anderson Model on Service Utialization ####
   # predisposing (age[ageCR], gender[genderCR], marital status[null], ethnicity[null] and family size[null]; +[resid])
   # enabling (education level[eduCG], family support[C10T(relationship)], access to services[ADL_UN?], travel time to the nearest health facility[?], medical expense per capita[?], and health insurance coverage[?]), 
   # need factors (chronic disease)[phyFra], actual needs[ADL_UN], with the utilization of health services (i.e. physician visit and hospitalization).

# description table
# head(describe(un),10)   # show basic description of first 10 variables 
   attach(d);table1.1  = rbind.fill(describe(ageCG),describe(ageCR),describe(phyFra),describe(ADL_UN));detach()
   attach(nd);table1.2 = rbind.fill(describe(ageCG),describe(ageCR),describe(phyFra),describe(ADL_UN));detach()
   table1 = cbind(table1.1,table1.2)

   reg1 = (glm(US ~ ageCR+genderCR+ageCG+genderCG+phyFra+ADL_UN+eduCG+economicCG+fr+resid+CF,data=d, family=poisson))
   reg2 = (glm(US ~ ageCR+genderCR+ageCG+genderCG+phyFra+ADL_UN+eduCG+economicCG+fr+resid+CF,data=nd, family=poisson))
   table2 = outreg(list(reg1,reg2))

   write.csv(table1,file="C:/Users/chens/Desktop/table1.csv")
   write.csv(table2,file="C:/Users/chens/Desktop/table2.csv")

### Pearlin's SPM Model ###

   reg3 = (glm(US ~ ageCG+genderCG+eduCG+economicCG+C12+LvHm+phyFra+GF12Positive+burden+resid+CRtype3+fr+depressive+C6T,data=d, family=poisson))
   reg4 = (glm(US ~ ageCG+genderCG+eduCG+economicCG+C12+LvHm+phyFra+GF12Positive+burden+resid+CRtype3+fr+depressive+C6T,data=nd, family=poisson))
   table3 = outreg(list(reg3,reg4))

   write.csv(table3,file="C:/Users/chens/Desktop/table3.csv")


### Andersen's model on unmet needs

   reg5 = (glm(ADL_UN ~ ageCR+genderCR+ageCG+genderCG+CGMarry+eduCG+resid+dr+CF+economicCG+phyFra+depressive+US,data=d, family=poisson))
   reg6 = (glm(ADL_UN ~ ageCR+genderCR+ageCG+genderCG+CGMarry+eduCG+resid+dr+CF+economicCG+phyFra+depressive+US,data=nd, family=poisson))
   
   ADL1  = (glm( ADL_UN ~ ageCR+genderCR+ageCG+genderCG+CGMarry+eduCG+resid+dr+CF+economicCG+phyFra+depressive+B16c+US+C5T,data=d,  family=poisson,na.action='na.omit'))   # heart issues
   ADL2  = (glm( ADL_UN ~ ageCR+genderCR+ageCG+genderCG+CGMarry+eduCG+resid+dr+CF+economicCG+phyFra+depressive+B16c+US+C5T,data=nd, family=poisson,na.action='na.omit'))   # heart issues
   ADL3  = (glm( ADL_UN ~ ageCR+genderCR+ageCG+genderCG+CGMarry+eduCG+resid+dr+CF+economicCG+phyFra+depressive+B16c+US*C5T,data=d,  family=poisson,na.action='na.omit'))   # heart issues
   ADL4  = (glm( ADL_UN ~ ageCR+genderCR+ageCG+genderCG+CGMarry+eduCG+resid+dr+CF+economicCG+phyFra+depressive+B16c+US*C5T,data=nd, family=poisson,na.action='na.omit'))   # heart issues
   IADL1 = (glm(IADL_UN ~ ageCR+genderCR+ageCG+genderCG+CGMarry+eduCG+resid+dr+CF+economicCG+phyFra+depressive+B16c+US+C5T,data=d,  family=poisson,na.action='na.omit'))   # heart issues
   IADL2 = (glm(IADL_UN ~ ageCR+genderCR+ageCG+genderCG+CGMarry+eduCG+resid+dr+CF+economicCG+phyFra+depressive+B16c+US+C5T,data=nd, family=poisson,na.action='na.omit'))   # heart issues
   IADL3 = (glm(IADL_UN ~ ageCR+genderCR+ageCG+genderCG+CGMarry+eduCG+resid+dr+CF+economicCG+phyFra+depressive+B16c+US*C5T,data=d,  family=poisson,na.action='na.omit'))   # heart issues
   IADL4 = (glm(IADL_UN ~ ageCR+genderCR+ageCG+genderCG+CGMarry+eduCG+resid+dr+CF+economicCG+phyFra+depressive+B16c+US*C5T,data=nd, family=poisson,na.action='na.omit'))   # heart issues
   table4 = outreg(list(ADL1,ADL2,ADL3,ADL4,IADL1,IADL2,IADL3,IADL4))
  
   write.csv(table4,file="C:/Users/chens/Desktop/table.csv")


############

   # interplot(ADL3, var1 = 'US',var2 = 'C5T', predPro = FALSE) + ggtitle("Average Conditional Effects")

   # impute(d$C5T,median)
   # impute(d$US,median)
   ADLd  = interplot(ADL3, var1 = "C5T",var2 = "US", predPro = TRUE, var2_vals = c(min(d$US,na.rm=T), max(d$US,na.rm=T))) + ggtitle("Unmet Need of ADL on PAC by US among CG of Dementia Population") + scale_colour_discrete(guide = guide_legend(title = "Mean"), labels = c("Low PAC", "High PAC")) + scale_fill_discrete(guide = guide_legend(title = "Intervals"), labels = c("Low PAC", "High PAC")) + theme(legend.position = c(.1, .8), legend.justification = c(0, .5))
   ADLnd  = interplot(ADL4, var1 = "C5T",var2 = "US", predPro = TRUE, var2_vals = c(min(d$US,na.rm=T), max(d$US,na.rm=T))) + ggtitle("Unmet Need of ADL on PAC by US among CG of Other Population") + scale_colour_discrete(guide = guide_legend(title = "Mean"), labels = c("Low PAC", "High PAC")) + scale_fill_discrete(guide = guide_legend(title = "Intervals"), labels = c("Low PAC", "High PAC")) + theme(legend.position = c(.1, .8), legend.justification = c(0, .5))
   IADLd = interplot(IADL3, var1 = "C5T",var2 = "US", predPro = TRUE, var2_vals = c(min(d$US,na.rm=T), max(d$US,na.rm=T))) + ggtitle("Unmet Need of IADL on PAC by US among CG of Dementia Population") + scale_colour_discrete(guide = guide_legend(title = "Mean"), labels = c("Low PAC", "High PAC")) + scale_fill_discrete(guide = guide_legend(title = "Intervals"), labels = c("Low PAC", "High PAC")) + theme(legend.position = c(.1, .8), legend.justification = c(0, .5))
   IADLnd  = interplot(IADL4, var1 = "C5T",var2 = "US", predPro = TRUE, var2_vals = c(min(d$US,na.rm=T), max(d$US,na.rm=T))) + ggtitle("Unmet Need of IADL on PAC by US among CG of Other Population") + scale_colour_discrete(guide = guide_legend(title = "Mean"), labels = c("Low PAC", "High PAC")) + scale_fill_discrete(guide = guide_legend(title = "Intervals"), labels = c("Low PAC", "High PAC")) + theme(legend.position = c(.1, .8), legend.justification = c(0, .5))

  
   grid.arrange(ADLd,ADLnd,IADLd,IADLnd, ncol=2, nrow=2)    # library(gridExtra)

   # plot_3val <- interplot(ADL3, var1 = "US",var2 = "C5T", predPro = TRUE, var2_vals = c(min(d$C5T), max(d$C5T))) + ggtitle("Conditional Predicted Probabilities for \nCitizens with Low and High Incomes") + scale_colour_discrete(guide = guide_legend(title = "Income"), labels = c("Low", "High")) + scale_fill_discrete(guide = guide_legend(title = "Income"), labels = c("Low", "High")) + theme(legend.position = c(0, .8), legend.justification = c(0, .5))
   
   

## Framework
   # DV: utilization of service in need [US]
   # IV: unmet need of CR in services[D2_UN], in ADL[ADL_UN], in IADL[IADL_UN], health status [FrailtyT], CR support [CRCsp,CREsp,CRFsp,CRdm], CG support [CGCsp,CGEsp,CGFsp,CGdm]
   # CV: demographics of CR[genderCR, ageCR, resCR, B15p/cohabit(living alone)], of CG[genderCG, ageCG, workCG, eduCG, maritalCG, economicCG, incomeCG], caregiving[CMon], PAC [C5T], ZBI [C6T], caregiving hours weekly [B13], relationship[C12, C10T] 
   # MV: meaning [PAC, PACas,PACel]

      m1 = glm(US ~ ageCG+genderCG+eduCG+CGMarry+B13+economicCG+DemCom+ADL_UN+C6T+C10T,data=w, family=poisson)
      m2 = glm(US ~ ageCG+genderCG+eduCG+CGMarry+B13+economicCG+DemCom+ADL_UN+C6T*C10T,data=w, family=poisson)
      m3 = glm(US ~ ageCG+genderCG+eduCG+CGMarry+B13+economicCG+DemCom+ADL_UN+C6T+C10T,data=nw, family=poisson)
      m4 = glm(US ~ ageCG+genderCG+eduCG+CGMarry+B13+economicCG+DemCom+ADL_UN+C6T*C10T,data=nw, family=poisson)

      summary(glm(US ~ ageCG+genderCG+eduCG+CGMarry+B13+economicCG+DemCom+ADL_UN+C6T+C10T,data=w, family=poisson))
      summary(glm(US ~ ageCG+genderCG+eduCG+CGMarry+B13+economicCG+DemCom+ADL_UN+C6T+C10T,data=nw, family=poisson))

## Variables - basic check (finished)
   # ad1=un[,c("D1_UN", "D2_UN", "ADL_UN", "IADL_UN", "phyFra", "psyFra", "socFra")]
   # pairs(ad1)  # between DV and IVs
   # cor(ad1,method = c("pearson", "kendall", "spearman"))
   # cor(ad1,na.rm=T)
   # describe(ad1)
   # table(c(D1_UN, D2_UN, ADL_UN, IADL_UN, FrailtyT))
   # hist(D1_UN, breaks=0:6)

## Models
   #   # Model 1: CR demographics -> CR UN ***
   #   m1.1 = glm(ADL_UN ~ genderCR+ageCR+CMon+CF+comb+CRCsp+CREsp+CRdm, data=data, family=poisson)   
   #   m1.2 = glm(ADL_UN ~ genderCR+ageCR+CMon+CF+CRCsp*comb+CREsp*comb+CRdm*comb, data=data, family=poisson)     
   #   m1.3 = glm(IADL_UN ~ genderCR+ageCR+CMon+CF+comb+CRCsp+CREsp+CRdm, data=data, family=poisson)   
   #   m1.4 = glm(IADL_UN ~ genderCR+ageCR+CMon+CF+CRCsp*comb+CREsp*comb+CRdm*comb, data=data, family=poisson)     
   #      # stargazer(m1.1,m1.2,m1.3,m1.4,title="ModelResult",column.labels=c('ADL','Interaction','IADL','Interaction'),align=T,type="text",out="table.htm")
   #
   #   # Model 2: CR UN -> CR Health *
   #   m2.1 = glm(FrailtyT ~ ADL_UN+IADL_UN+genderCR+ageCR, data=data)  
   #   m2.2 = glm(phyFra ~ ADL_UN+IADL_UN+genderCR+ageCR, data=data)  
   #   m2.3 = glm(psyFra ~ ADL_UN+IADL_UN+genderCR+ageCR, data=data)  
   #   m2.4 = glm(socFra ~ ADL_UN+IADL_UN+genderCR+ageCR, data=data)  
   #      # stargazer('m2.1',m2.2,m2.3,'m2.4',title="ModelResult",column.labels=c('Frailty','Phy Frail','Psy Frail','Soc Frail'),align=T,type="text",out="table.htm")
   #   # m2.2, m2.3, m2.3
   #
   #   # Model 3: CR UN -> CG ZBI | PAC
   #   m3.1 = glm(ZBI4Score ~ ageCG+genderCG+CF+economicCG+ADL_UN+IADL_UN+phyFra+psyFra+PAC+C12+C10T, data=w)
   #   m3.2 = glm(ZBI4Score ~ ageCG+genderCG+CF+economicCG+ADL_UN+IADL_UN+phyFra+psyFra+PAC+C12+C10T, data=nw)
   #      # stargazer(m3.1,m3.2,title="ModelResult",column.labels=c('Working','Non-working'),align=T,type="text",out="table.htm")
   #
   #   # Model 4: CR Health -> CG utilization of services in need | PAC
   #   m4.1 = glm(US ~ ZBI4Score+workCG, data=data, family=poisson) 
   #   m4.2 = glm(US ~ ZBI4Score*workCG, data=data, family=poisson) 
   #      # stargazer(m4.1,m4.2,title="ModelResult",column.labels=c('Main','Interaction'),align=T,type="text",out="table.htm")
   #
   #      # check actual coefficients if model is desirable
   #         # t1 = cbind(exp(coef(m1)),exp(confint(m1)))
   #         # t1.1 = cbind(exp(coef(m1.1)),exp(confint(m1.1)))
   #         # t1.2 = cbind(exp(coef(m1.2)),exp(confint(m1.2)))

   #   # stepAIC(m1,direction="both")     # library(MASS)

      # Model 1: CR demographics + PAC -> frailty
         # summary(glm(US ~ genderCG+ageCG+workCG+eduCG+economicCG+incomeCG+ADL_UN+IADL_UN+C6T+C7,data=data, family=poisson))        
         # summary(glm(US ~ genderCG+ageCG+workCG+eduCG+economicCG+ADL_UN+IADL_UN+C6T+C7,data=data, family=poisson))

## Model Tables 
# Regression Models - export to Excel
   # Model Table Set 1
      # table1 = outreg(list(m1.1,m1.2,m1.3,m1.4))
      # table2 = outreg(list(m2.1,m2.2,m2.3,m2.4))
      # table3 = outreg(list(m3.1,m3.2))
      # table4 = outreg(list(m4.1,m4.2))

   #  reg1 = (glm(US ~ ageCR+genderCR+ageCG+genderCG+phyFra+ADL_UN+eduCG+economicCG+fr+resid+CF,data=d, family=poisson))
   #  reg2 = (glm(US ~ ageCR+genderCR+ageCG+genderCG+phyFra+ADL_UN+eduCG+economicCG+fr+resid+CF,data=nd, family=poisson))
   #  table2 = outreg(list(reg1,reg2))

# Combine all tables
   # table = rbind.fill(table1,table2,table3,table4)     # require library(plyr)

# output to excel


   # write.csv(table1,file="C:/Users/chens/Desktop/test1.csv")
   # write.csv(table2,file="C:/Users/chens/Desktop/test2.csv")
   # write.csv(table3,file="C:/Users/chens/Desktop/test3.csv")
   # write.csv(table4,file="C:/Users/chens/Desktop/test4.csv")


#anova12=anova(linear1,linear2)
#anova23=anova(linear2,linear3)
#anova34=anova(linear3,linear4)
#anova14=anova(linear1,linear4)
#stargazer(anova12,anova23,anova34,anova14,title="Model Comparison",align=T,type="text",out="table.htm")


library(jiebaR)
library(rJava)
library(Rwordseg) # install.packages("Rwordseg", repos = "http://R-Forge.R-project.org")


### Word Cloud
data = read.table("C:/Users/chens/Desktop/wordcloud.text")

segmentCN("C:/Users/chens/Desktop/wordcloud.text",returnType="tm")

wk = worker()
segment("C:/Users/chens/Desktop/wordcloud.text", wk)
     

