### Projects
# 1. working caregivers including dementia population
# 2. compare adult children and spouses about PACs
# 3. ??
# 

### Packages loading ###
library(xlsx); library(corrr)
library(outreg); library(ggplot2); library(plyr); library(psych); library(stargazer); library(interplot); library(gridExtra); library(GGally)

### Anderson Theory ####
   # predisposing (age[ageCR], gender[genderCR], marital status[null], ethnicity[null] and family size[null]; +[resid])
   # enabling (education level[eduCG], family support[C10T(relationship)], access to services[ADL_UN?], travel time to the nearest health facility[?], medical expense per capita[?], and health insurance coverage[?]), 
   # need factors (chronic disease)[phyFra], actual needs[ADL_UN], with the utilization of health services (i.e. physician visit and hospitalization).




### Data of HKCSS ###
data = work = read.csv("C:/Users/chens/OneDrive/research/Projects/4 HKCSS/191216 HKCSS.csv",header=T,na.strings = "NA")  # Office - Dell Inspiron 16

## Var names changes
data$CMon=data$CGMon
data$CF=data$B13     # CG frequency
data$dep=data$B16f   # depression
data$dem=data$B16b   # dementia
data$heart=data$B16c   # heart issue
data$dr=data$C12     # dyadic relation
data$fr=data$C10T    # family relation
data$ZBI=data$C6T    # family relation

### Sub-data 
s = data[data$CRtype2=="1",]     # spouses: 768 
a = data[data$CRtype2=="2" | data$CRtype2=="3",]     # adult children: 742

w  = a[a$workCG=="1",]   # working: 494  
nw = a[a$workCG=="0",]   # non-working: 252

### Project 1: 

## description

t.test(s$ageCR,data$drugb)

ggpairs(data = data, columns = c("ageCG","genderCG","CGMarry","eduCG","resid","dr","economicCG","phyFra","dem","ADL_UN","IADL_UN","ZBI","PAC"), title = "bivariates")
cor1 = data[,c("ageCG","genderCG","CGMarry","eduCG","resid","dr","economicCG","phyFra","dem","ADL_UN","IADL_UN","ZBI","PAC")]

write.csv(correlate(cor1),file="C:/Users/chens/Desktop/cor1.csv")



## model testing
m1.0  = (glm(UNS ~ ageCG+genderCG+CGMarry+eduCG+resid+dr+economicCG+phyFra+dem+ADL_UN+IADL_UN+ZBI+PAC, data=w, family=poisson,na.action='na.omit')) 
m1.1  = (glm(UNS ~ ageCG+genderCG+CGMarry+eduCG+resid+dr+economicCG+phyFra+dem+ADL_UN+IADL_UN+ZBI*PAC, data=w, family=poisson,na.action='na.omit')) 
m2.0  = (glm(UNS ~ ageCG+genderCG+CGMarry+eduCG+resid+dr+economicCG+phyFra+dem+ADL_UN+IADL_UN+ZBI+PAC,data=nw, family=poisson,na.action='na.omit')) 
m2.1  = (glm(UNS ~ ageCG+genderCG+CGMarry+eduCG+resid+dr+economicCG+phyFra+dem+ADL_UN+IADL_UN+ZBI*PAC,data=nw, family=poisson,na.action='na.omit')) 

write.csv(outreg(list(m1.0,m1.1,m2.0,m2.1)),file="C:/Users/chens/Desktop/table.csv")


### graphs
g1   = interplot(m1.1, var1 = "PAC",var2 = "ZBI", predPro = TRUE, var2_vals = c(min(  w$ZBI,na.rm=T), max( w$ZBI,na.rm=T))) + ggtitle("Service Unmet Need on PAC by ZBI among working caregivers") + scale_colour_discrete(guide = guide_legend(title = "Mean"), labels = c("Lowest ZBI", "Highest ZBI")) + scale_fill_discrete(guide = guide_legend(title = "Intervals"), labels = c("Lowest ZBI", "Highest ZBI")) + theme(legend.position = c(.1, .8), legend.justification = c(0, .5)) + ylab("Estimated Coefficient for Unmet Need of Services")
g2   = interplot(m2.1, var1 = "PAC",var2 = "ZBI", predPro = TRUE, var2_vals = c(min( nw$ZBI,na.rm=T), max(nw$ZBI,na.rm=T))) + ggtitle("Service Unmet Need on PAC by ZBI among non-working caregivers") + scale_colour_discrete(guide = guide_legend(title = "Mean"), labels = c("Lowest ZBI", "Highest ZBI")) + scale_fill_discrete(guide = guide_legend(title = "Intervals"), labels = c("Lowest ZBI", "Highest ZBI")) + theme(legend.position = c(.1, .8), legend.justification = c(0, .5)) + ylab("Estimated Coefficient for Unmet Need of Services")

grid.arrange(g1,g2, ncol=1, nrow=2)    # library(gridExtra)


### Project 2: 

## description

t.test(s$ageCR,a$ageCR)


ggpairs(data = data, columns = c("ageCR","genderCR","ageCG","genderCG","CGMarry","eduCG","resid","dr","CF","economicCG","workCG","phyFra","dem","depressive","ADL_UN","IADL_UN","ZBI","PAC"), title = "bivariates")
cor2 = data[,c("ageCR","genderCR","ageCG","genderCG","CGMarry","eduCG","resid","dr","CF","economicCG","workCG","phyFra","dem","depressive","ADL_UN","IADL_UN","ZBI","PAC")]

write.csv(correlate(cor2),file="C:/Users/chens/Desktop/cor2.csv")


## model testing
m1.0  = (glm(UNS ~ ageCR+genderCR+ageCG+genderCG+CGMarry+eduCG+resid+dr+CF+economicCG+workCG+phyFra+dem+depressive+ADL_UN+IADL_UN+ZBI+PAC, data=s, family=poisson,na.action='na.omit')) 
m1.1  = (glm(UNS ~ ageCR+genderCR+ageCG+genderCG+CGMarry+eduCG+resid+dr+CF+economicCG+workCG+phyFra+dem+depressive+ADL_UN+IADL_UN+ZBI*PAC, data=s, family=poisson,na.action='na.omit')) 
m2.0  = (glm(UNS ~ ageCR+genderCR+ageCG+genderCG+CGMarry+eduCG+resid+dr+CF+economicCG+workCG+phyFra+dem+depressive+ADL_UN+IADL_UN+ZBI+PAC, data=a, family=poisson,na.action='na.omit')) 
m2.1  = (glm(UNS ~ ageCR+genderCR+ageCG+genderCG+CGMarry+eduCG+resid+dr+CF+economicCG+workCG+phyFra+dem+depressive+ADL_UN+IADL_UN+ZBI*PAC, data=a, family=poisson,na.action='na.omit')) 

write.csv(outreg(list(m1.0,m1.1,m2.0,m2.1)),file="C:/Users/chens/Desktop/table1.csv")

g1   = interplot(m1.1, var1 = "PAC",var2 = "ZBI", predPro = TRUE, var2_vals = c(min(s$ZBI,na.rm=T), max(s$ZBI,na.rm=T))) + ggtitle("Service Unmet Need on PAC by ZBI among spouse caregivers") + scale_colour_discrete(guide = guide_legend(title = "Mean"), labels = c("Lowest ZBI", "Highest ZBI")) + scale_fill_discrete(guide = guide_legend(title = "Intervals"), labels = c("Lowest ZBI", "Highest ZBI")) + theme(legend.position = c(.1, .8), legend.justification = c(0, .5)) + ylab("Estimated Coefficient for Unmet Need of Services")
g2   = interplot(m2.1, var1 = "PAC",var2 = "ZBI", predPro = TRUE, var2_vals = c(min(a$ZBI,na.rm=T), max(a$ZBI,na.rm=T))) + ggtitle("Service Unmet Need on PAC by ZBI among child caregivers") + scale_colour_discrete(guide = guide_legend(title = "Mean"), labels = c("Lowest ZBI", "Highest ZBI")) + scale_fill_discrete(guide = guide_legend(title = "Intervals"), labels = c("Lowest ZBI", "Highest ZBI")) + theme(legend.position = c(.1, .8), legend.justification = c(0, .5)) + ylab("Estimated Coefficient for Unmet Need of Services")

grid.arrange(g1,g2, ncol=1, nrow=2)    # library(gridExtra)
