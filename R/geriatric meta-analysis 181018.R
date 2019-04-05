### Library packages
library(meta);library(metafor);library(nlme);library(checkmate);library(forestplot);library(grid);
library(officer);library(stargazer);library(outreg);library(SDMTools);library(PRISMAstatement)

### Data Retrieval from "test.txt" for [dementia intervention project]
meta=read.table("C:/Users/chens/OneDrive/research/1personal/Geriatrics/meta/test.txt",header=T,sep="\t",na.strings = "NA")  # Dell
meta=read.table("C:/Users/Student RA/vscode temp/test.txt",header=T,sep="\t",na.strings = "NA")                             # CoA
meta=read.table("D:/OneDrive/research/1personal/Geriatrics/meta/test.txt",header=T,sep="\t",na.strings = "NA")  # Lenovo

### Data Retrieval from "test.txt" for [meaning project]

forest(dat$yi, dat$vi, atransf = exp, ylim = c(-3.5, 16), at = log(c(0.05, 0.25, 1, 4, 20)), xlim = c(-9, 7), slab = paste(dat$author, dat$year, sep = ", ")) 
res <- rma(yi, vi, mods = cbind(ablat), data = dat) 
preds <- predict(res, newmods = c(10, 30, 50)) 
addpoly(preds$pred, sei = preds$se, atransf = exp, mlab = c("10 Degrees", "30 Degrees", "50 Degrees")) 
text(-9, 15, "Author(s) and Year", pos = 4, font = 2) 
text(7, 15, "Relative Risk [95% CI]", pos = 2, font = 2) 
abline(h = 0)


### [meaning project]

### [dementia intervention project]
  # Cohen's d columns for all intervention types across 3 measurement types
  attach(meta)
    dMMSEpre=(m1e.pre-m1c.pre)/(sqrt((s1e.pre*s1e.pre*(experiment.n-1)+s1c.pre*s1c.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dMMSEpost=(m1e.post-m1c.post)/(sqrt((s1e.post*s1e.post*(experiment.n-1)+s1c.post*s1c.post*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dMMSEexp=(m1e.post-m1e.pre)/(sqrt((s1e.post*s1e.post*(experiment.n-1)+s1e.pre*s1e.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dMMSEcon=(m1c.post-m1c.pre)/(sqrt((s1c.post*s1c.post*(experiment.n-1)+s1c.pre*s1c.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))

    dADLpre=(m2e.pre-m2c.pre)/(sqrt((s2e.pre*s2e.pre*(experiment.n-1)+s2c.pre*s2c.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dADLpost=(m2e.post-m2c.post)/(sqrt((s2e.post*s2e.post*(experiment.n-1)+s2c.post*s2c.post*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dADLexp=(m2e.post-m2e.pre)/(sqrt((s2e.post*s2e.post*(experiment.n-1)+s2e.pre*s2e.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dADLcon=(m2c.post-m2c.pre)/(sqrt((s2c.post*s2c.post*(experiment.n-1)+s2c.pre*s2c.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))

    dQOLpre=(m3e.pre-m3c.pre)/(sqrt((s3e.pre*s3e.pre*(experiment.n-1)+s3c.pre*s3c.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dQOLpost=(m3e.post-m3c.post)/(sqrt((s3e.post*s3e.post*(experiment.n-1)+s3c.post*s3c.post*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dQOLexp=(m3e.post-m3e.pre)/(sqrt((s3e.post*s3e.post*(experiment.n-1)+s3e.pre*s3e.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dQOLcon=(m3c.post-m3c.pre)/(sqrt((s3c.post*s3c.post*(experiment.n-1)+s3c.pre*s3c.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))

  # Calculate J score for g and d transition
    J=1-(3/(4*(experiment.n+contrast.n-2)-1))

  # g calculation
    gMMSEpre=J*dMMSEpre
    gMMSEpost=J*dMMSEpost
    gMMSEexp=J*dMMSEexp
    gMMSEcon=J*dMMSEcon

    gADLpre=J*dADLpre
    gADLpost=J*dADLpost
    gADLexp=J*dADLexp
    gADLcon=J*dADLcon

    gQOLpre=J*dQOLpre
    gQOLpost=J*dQOLpost
    gQOLexp=J*dQOLexp
    gQOLcon=J*dQOLcon    

    meta=cbind(meta,dMMSEpre,dMMSEpost,dMMSEexp,dMMSEcon,dADLpre,dADLpost,dADLexp,dADLcon,dQOLpre,dQOLpost,dQOLexp,dQOLcon,
              gMMSEpre,gMMSEpost,gMMSEexp,gMMSEcon,gADLpre,gADLpost,gADLexp,gADLcon,gQOLpre,gQOLpost,gQOLexp,gQOLcon)
  detach(meta)

  write.csv(meta,file="C:/Users/chens/OneDrive/research/1personal/Geriatrics/meta/temp.csv",row.names=T)    # Dell
  write.csv(meta,file="C:/Users/Student RA/vscode temp/temp.csv",row.names=T)                               # CoA
  write.csv(meta,file="C:/Users/admin/OneDrive/research/1personal/Geriatrics/meta/temp.csv",row.names=T)    # Lenovo


  meta1  =meta[which(meta$MMSE==1),]                         # MMSE
  meta1.3=meta[which(meta$MMSE==1 & meta$intervention2==3),] # MMSE * nursing
  meta1.2=meta[which(meta$MMSE==1 & meta$intervention2==2),] # MMSE * psychological 
  meta1.1=meta[which(meta$MMSE==1 & meta$intervention2==1),] # MMSE * comprehensive
  meta2  =meta[which(meta$ADL==1),]                          # ADL 
  meta2.3=meta[which(meta$ADL==1 & meta$intervention2==3),]  # ADL * nursing
  meta2.2=meta[which(meta$ADL==1 & meta$intervention2==2),]  # ADL * psychological
  meta2.1=meta[which(meta$ADL==1 & meta$intervention2==1),]  # ADL * comprehensive
  meta3  =meta[which(meta$QOL==1),]                          # QOL 
  meta3.3=meta[which(meta$QOL==1 & meta$intervention2==3),]  # QOL * nursing
  meta3.2=meta[which(meta$QOL==1 & meta$intervention2==2),]  # QOL * psychological 
  meta3.1=meta[which(meta$QOL==1 & meta$intervention2==1),]  # QOL * comprehensive 

### Demographic Information 
##  TABLE 1 - Literature Info - edit in excel file "meta-analysis" (the newest version 180325)
##  TABLE 2 - Demographic Info
attach(meta)
  summary(male);summary(female)
  
  table(year)
  table(year,intervention2)
  table(year,MMSE);table(year,ADL);table(year,QOL)
  
  table(location3)
  table(location3,intervention2)
  table(location3,MMSE);table(location3,ADL);table(location3,QOL)
  
  summary(average.age)
  summary(sample.size)
  table(intervention2)
  # summary(m1e.pre);summary(m1e.post);summary(m2e.pre);summary(m2e.post);summary(m3e.pre);summary(m3e.post);
  # sum(average.age*sample.size,na.rm=T)/sum(sample.size,na.rm=T)   # group average age
  # TO ADD: city level
detach(meta)

## TABLE 3 - detailed demographic on year, age, and locations
# crosstable()

age1=meta[which(meta$age1859==1),]
  table(age1859)
  attach(age1);table(MMSE);table(ADL);table(QOL);table(intervention2)
  detach(age1)

age2=meta[which(meta$age6064==1),]
  attach(age2)
  table(age6064);table(intervention2);table(MMSE);table(ADL);table(QOL)
  detach(age2)

age3=meta[which(meta$age6570==1),]
  attach(age3)
  table(age6570);table(intervention2);table(MMSE);table(ADL);table(QOL)
  detach(age3)
  
age4=meta[which(meta$age7080==1),]
  attach(age4)
  table(age7080);table(intervention2);table(MMSE);table(ADL);table(QOL)
  detach(age4)
  
age5=meta[which(meta$age80.==1),]
  attach(age5)
  table(age80.);table(intervention2);table(MMSE);table(ADL);table(QOL)
  detach(age5)

## TABLE 2
attach(meta)

  # year by intervention and measurement 
  cbind(table(year,intervention2),table(year,MMSE),table(year,ADL),table(year,QOL))
  cbind(table(location,intervention2),table(location,MMSE),table(location,ADL),table(location,QOL))
  cbind(table(intervention2,MMSE),table(intervention2,ADL),table(intervention2,QOL))
  # age group frequency

  # age group by intervention and measurement
  cbind(table(age1859,intervention2),table(age6064,intervention2),table(age6569,intervention2),table(age7080,intervention2),table(age80.,intervention2),
        table(age1859,MMSE),table(age1859,ADL),table(age1859,QOL),
        table(age6064,MMSE),table(age6064,ADL),table(age6064,QOL),
        table(age6569,MMSE),table(age6569,ADL),table(age6569,QOL),
        table(age7080,MMSE),table(age7080,ADL),table(age7080,QOL),
        table(age80.,MMSE),table(age80.,ADL),table(age80.,QOL)
        )
  # gender percentage by intervention and measurement 
  g1=sum(male,na.rm=T);g2=sum(female,na.rm=T);g3=g1+g2
  g1/g3;g2/g3

  g1=cbind(sum(male[which(intervention2==1)],na.rm=T),sum(male[which(intervention2==2)],na.rm=T),sum(male[which(intervention2==3)],na.rm=T))
  g2=cbind(sum(female[which(intervention2==1)],na.rm=T),sum(female[which(intervention2==2)],na.rm=T),sum(female[which(intervention2==3)],na.rm=T))
  g3=g1+g2; g1/g3;g2/g3

  g1=cbind(sum(male[which(MMSE==1)],na.rm=T),sum(male[which(ADL==1)],na.rm=T),sum(male[which(QOL==1)],na.rm=T))
  g2=cbind(sum(female[which(MMSE==1)],na.rm=T),sum(female[which(ADL==1)],na.rm=T)，sum(female[which(QOL==1)],na.rm=T))
  g3=g1+g2; g1/g3;g2/g3

  # age (continuous)
  sum(average.age*sample.size,na.rm=T)/sum(sample.size)
  wt.sd(average.age,sample.size)

  # sample size (continuous)
  mean(sample.size)
  sd(sample.size)
  
  # Measurement scoring by pre/post interventions
  
  sink("result.txt")
  range(meta1$m1e.pre);mean(meta1$m1e.pre);sd(meta1$m1e.pre)
  range(meta1$m1e.post);mean(meta1$m1e.post);sd(meta1$m1e.post)
  range(meta2$m2e.pre);mean(meta2$m2e.pre);sd(meta2$m2e.pre)
  range(meta2$m2e.post);mean(meta2$m2e.post);sd(meta2$m2e.post)
  range(meta3$m3e.pre);mean(meta3$m3e.pre);sd(meta3$m3e.pre)
  range(meta3$m3e.post);mean(meta3$m3e.post);sd(meta3$m3e.post)
  sink()

### TABLE 4-6 - Intervention Effect Analysis (MMSE, ADL, QOL)
## Data Prep - calculate Cohen's d and Hedges' g for MMSE, ADL and QOL  # already done earlier

  # making graphs
  sink("result.txt")
  cbind(dMMSEpre,dADLpre,dQOLpre,na.rm=T)
  cbind(gMMSEpre,gADLpre,gQOLpre,na.rm=T)
  sink()

  # factor table
    # c=matrix(NA,89,6)
    # c[,1]=dMMSEpost
    # c[,2]=dADLpost
    # c[,3]=dQOLpost
    # c[,4]=gMMSEpost
    # c[,5]=gADLpost
    # c[,6]=gOTHERpost

### TABLE 4 -  Intervention Effect Analysis on MMSE
##  Prepare sub-dataset - MMSE, ADL, QOL
  # meta1.3=meta[which(meta$MMSE==1 & meta$intervention2==3),]
  # meta1.2=meta[which(meta$MMSE==1 & meta$intervention2==2),]
  # meta1.1=meta[which(meta$MMSE==1 & meta$intervention2==1),]
  # meta2.3=meta[which(meta$ADL==1 & meta$intervention2==3),]
  # meta2.2=meta[which(meta$ADL==1 & meta$intervention2==2),]
  # meta2.1=meta[which(meta$ADL==1 & meta$intervention2==1),]
  # meta3.3=meta[which(meta$QOL==1 & meta$intervention2==3),]
  # meta3.2=meta[which(meta$QOL==1 & meta$intervention2==2),]
  # meta3.1=meta[which(meta$QOL==1 & meta$intervention2==1),]

  t1=matrix(,27,12)
## MMSE: between-group and pre/post comparison 
  attach(meta1.1)   # Intervention = 3 - Nursing
  # pre intervention
    # observation M, SD
    t1[1,1]=sum((m1e.pre*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[1,2]=sqrt(sum(s1e.pre*s1e.pre*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2

    # contrast M and SD
    t1[2,1]=sum((m1c.pre*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[2,2]=sqrt(sum(s1c.pre*s1c.pre*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[3,1]=sum((m1e.pre*experiment.n-meta1.3$m1c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[3,2]=sqrt(abs(sum((s1e.pre*s1e.pre*experiment.n-s1c.pre*s1c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[3,3]=mean(sum(dMMSEpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[3,4]=mean(sum(gMMSEpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))

  # post intervention
    # observation M, SD
    t1[1,5]=sum((m1e.post*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[1,6]=sqrt(sum(s1e.post*s1e.post*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2
    
    # contrast M and SD
    t1[2,5]=sum(m1c.post*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[2,6]=sqrt(sum(s1c.post*s1c.post*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[3,5]=sum((m1e.post*experiment.n-meta1.3$m1c.post*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[3,6]=sqrt(abs(sum((s1e.post*s1e.post*experiment.n-s1c.post*s1c.post*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[3,7]=mean(sum(dMMSEpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[3,8]=mean(sum(gMMSEpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    
    
  # post-pre intervention
    # observation M, SD
    t1[1,9]=sum(((m1e.post-m1e.pre)*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[1,10]=sqrt(abs(sum((s1e.post*s1e.post-s1e.pre*s1e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2

    # contrast M and SD
    t1[2,9]=sum(((m1c.post-m1c.pre)*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[2,10]=sqrt(abs(sum((s1c.post*s1c.post-s1c.pre*s1e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2
    
    # observation - contrast M, SD, d, g
    t1[3,9]=sum((m1e.post*experiment.n-m1c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[3,10]=sqrt(abs(sum((s1e.post*s1e.post*experiment.n-s1c.pre*s1c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[3,11]=mean(sum((dMMSEpost-dMMSEpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[3,12]=mean(sum((gMMSEpost-gMMSEpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
  detach(meta1.1)

  attach(meta1.2)   # Intervention = 2 - Psychological
  # pre intervention
    # observation M, SD
    t1[4,1]=sum((m1e.pre*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[4,2]=sqrt(sum(s1e.pre*s1e.pre*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2

    # contrast M and SD
    t1[5,1]=sum((m1c.pre*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[5,2]=sqrt(sum(s1c.pre*s1c.pre*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[6,1]=sum((m1e.pre*experiment.n-m1c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[6,2]=sqrt(abs(sum((s1e.pre*s1e.pre*experiment.n-s1c.pre*s1c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[6,3]=mean(sum(dMMSEpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[6,4]=mean(sum(gMMSEpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))


  # post intervention
    # observation M, SD
    t1[4,5]=sum((m1e.post*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[4,6]=sqrt(sum(s1e.post*s1e.post*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2
    
    # contrast M and SD
    t1[5,5]=sum((m1c.post*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[5,6]=sqrt(sum(s1c.post*s1c.post*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[6,5]=sum((m1e.post*experiment.n-m1c.post*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[6,6]=sqrt(abs(sum((s1e.post*s1e.post*experiment.n-s1c.post*s1c.post*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[6,7]=mean(sum(dMMSEpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[6,8]=mean(sum(gMMSEpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    
    
  # post-pre intervention
    # observation M, SD
    t1[4,9]=sum(((m1e.post-m1e.pre)*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[4,10]=sqrt(abs(sum((s1e.post*s1e.post-s1e.pre*s1e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2

    # contrast M and SD
    t1[5,9]=sum(((m1c.post-m1c.pre)*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[5,10]=sqrt(abs(sum((s1c.post*s1c.post-s1c.pre*s1e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2
    
    # observation - contrast M, SD, d, g
    t1[6,9]=sum((m1e.post*experiment.n-m1c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[6,10]=sqrt(abs(sum((s1e.post*s1e.post*experiment.n-s1c.pre*s1c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[6,11]=mean(sum((dMMSEpost-dMMSEpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[6,12]=mean(sum((gMMSEpost-gMMSEpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
  detach(meta1.2)

  attach(meta1.3)   # Intervention = 1 - Comprehensive
  # pre intervention
    # observation M, SD
    t1[7,1]=sum((m1e.pre*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[7,2]=sqrt(sum(s1e.pre*s1e.pre*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2

    # contrast M and SD
    t1[8,1]=sum((m1c.pre*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[8,2]=sqrt(sum(s1c.pre*s1c.pre*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[9,1]=sum((m1e.pre*experiment.n-m1c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[9,2]=sqrt(abs(sum((s1e.pre*s1e.pre*experiment.n-s1c.pre*s1c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[9,3]=mean(sum(dMMSEpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[9,4]=mean(sum(gMMSEpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))


  # post intervention
    # observation M, SD
    t1[7,5]=sum((m1e.post*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[7,6]=sqrt(sum(s1e.post*s1e.post*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2
    
    # contrast M and SD
    t1[8,5]=sum((m1c.post*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[8,6]=sqrt(sum(s1c.post*s1c.post*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[9,5]=sum((m1e.post*experiment.n-m1c.post*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[9,6]=sqrt(abs(sum((s1e.post*s1e.post*experiment.n-s1c.post*s1c.post*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[9,7]=mean(sum(dMMSEpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[9,8]=mean(sum(gMMSEpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))


  # post-pre intervention
    # observation M, SD
    t1[7,9]=sum(((m1e.post-m1e.pre)*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[7,10]=sqrt(abs(sum((s1e.post*s1e.post-s1e.pre*s1e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2

    # contrast M and SD
    t1[8,9]=sum(((m1c.post-m1c.pre)*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[8,10]=sqrt(abs(sum((s1c.post*s1c.post-s1c.pre*s1e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2
    
    # observation - contrast M, SD, d, g
    t1[9,9]=sum((m1e.post*experiment.n-m1c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[9,10]=sqrt(abs(sum((s1e.post*s1e.post*experiment.n-s1c.pre*s1c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[9,11]=mean(sum((dMMSEpost-dMMSEpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[9,12]=mean(sum((gMMSEpost-gMMSEpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
  detach(meta1.3)

## ADL: between-group and pre/post comparison 
  attach(meta2.1)   # Intervention = 3 - Nursing
  # pre intervention
    # observation M, SD
    t1[10,1]=sum((m2e.pre*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[10,2]=sqrt(sum(s2e.pre*s2e.pre*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2

    # contrast M and SD
    t1[11,1]=sum((m2c.pre*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[11,2]=sqrt(sum(s2c.pre*s2c.pre*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[12,1]=sum((m2e.pre*experiment.n-m2c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[12,2]=sqrt(abs(sum((s2e.pre*s2e.pre*experiment.n-s2c.pre*s2c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[12,3]=mean(sum(dADLpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[12,4]=mean(sum(gADLpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))


  # post intervention
    # observation M, SD
    t1[10,5]=sum((m2e.post*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[10,6]=sqrt(sum(s2e.post*s2e.post*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2
    
    # contrast M and SD
    t1[11,5]=sum((m2c.post*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[11,6]=sqrt(sum(s2c.post*s2c.post*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[12,5]=sum((m2e.post*experiment.n-m2c.post*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[12,6]=sqrt(abs(sum((s2e.post*s2e.post*experiment.n-s2c.post*s2c.post*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[12,7]=mean(sum(dADLpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[12,8]=mean(sum(gADLpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    
    
  # post-pre intervention
    # observation M, SD
    t1[10,9]=sum(((m2e.post-m2e.pre)*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[10,10]=sqrt(abs(sum((s2e.post*s2e.post-s2e.pre*s2e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2

    # contrast M and SD
    t1[11,9]=sum(((m2c.post-m2c.pre)*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[11,10]=sqrt(abs(sum((s2c.post*s2c.post-s2c.pre*s2e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2
    
    # observation - contrast M, SD, d, g
    t1[12,9]=sum((m2e.post*experiment.n-m2c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[12,10]=sqrt(abs(sum((s2e.post*s2e.post*experiment.n-s2c.pre*s2c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[12,11]=mean(sum((dADLpost-dADLpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[12,12]=mean(sum((gADLpost-gADLpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
  detach(meta2.1)

  attach(meta2.2)   # Intervention = 2 - Psychological
  # pre intervention
    # observation M, SD
    t1[13,1]=sum((m2e.pre*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[13,2]=sqrt(sum(s2e.pre*s2e.pre*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2

    # contrast M and SD
    t1[14,1]=sum((m2c.pre*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[14,2]=sqrt(sum(s2c.pre*s2c.pre*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[15,1]=sum((m2e.pre*experiment.n-m2c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[15,2]=sqrt(abs(sum((s2e.pre*s2e.pre*experiment.n-s2c.pre*s2c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[15,3]=mean(sum(dADLpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[15,4]=mean(sum(gADLpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))


  # post intervention
    # observation M, SD
    t1[13,5]=sum((m2e.post*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[13,6]=sqrt(sum(s2e.post*s2e.post*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2
    
    # contrast M and SD
    t1[14,5]=sum((m2c.post*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[14,6]=sqrt(sum(s2c.post*s2c.post*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[15,5]=sum((m2e.post*experiment.n-m2c.post*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[15,6]=sqrt(abs(sum((s2e.post*s2e.post*experiment.n-s2c.post*s2c.post*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[15,7]=mean(sum(dADLpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[15,8]=mean(sum(gADLpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    
    
  # post-pre intervention
    # observation M, SD
    t1[13,9]=sum(((m2e.post-m2e.pre)*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[13,10]=sqrt(abs(sum((s2e.post*s2e.post-s2e.pre*s2e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2

    # contrast M and SD
    t1[14,9]=sum(((m2c.post-m2c.pre)*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[14,10]=sqrt(abs(sum((s2c.post*s2c.post-s2c.pre*s2e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2
    
    # observation - contrast M, SD, d, g
    t1[15,9]=sum((m2e.post*experiment.n-m2c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[15,10]=sqrt(abs(sum((s2e.post*s2e.post*experiment.n-s2c.pre*s2c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[15,11]=mean(sum((dADLpost-dADLpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[15,12]=mean(sum((gADLpost-gADLpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
  detach(meta2.2)

  attach(meta2.3)   # Intervention = 1 - Comprehensive
  # pre intervention
    # observation M, SD
    t1[16,1]=sum((m2e.pre*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[16,2]=sqrt(sum(s2e.pre*s2e.pre*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2

    # contrast M and SD
    t1[17,1]=sum((m2c.pre*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[17,2]=sqrt(sum(s2c.pre*s2c.pre*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[18,1]=sum((m2e.pre*experiment.n-m2c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[18,2]=sqrt(abs(sum((s2e.pre*s2e.pre*experiment.n-s2c.pre*s2c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[18,3]=mean(sum(dADLpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[18,4]=mean(sum(gADLpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))


  # post intervention
    # observation M, SD
    t1[16,5]=sum((m2e.post*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[16,6]=sqrt(sum(s2e.post*s2e.post*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2
    
    # contrast M and SD
    t1[17,5]=sum((m2c.post*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[17,6]=sqrt(sum(s2c.post*s2c.post*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[18,5]=sum((m2e.post*experiment.n-m2c.post*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[18,6]=sqrt(abs(sum((s2e.post*s2e.post*experiment.n-s2c.post*s2c.post*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[18,7]=mean(sum(dADLpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[18,8]=mean(sum(gADLpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))


  # post-pre intervention
    # observation M, SD
    t1[16,9]=sum(((m2e.post-m2e.pre)*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[16,10]=sqrt(abs(sum((s2e.post*s2e.post-s2e.pre*s2e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2

    # contrast M and SD
    t1[17,9]=sum(((m2c.post-m2c.pre)*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[17,10]=sqrt(abs(sum((s2c.post*s2c.post-s2c.pre*s2e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2
    
    # observation - contrast M, SD, d, g
    t1[18,9]=sum((m2e.post*experiment.n-m2c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[18,10]=sqrt(abs(sum((s2e.post*s2e.post*experiment.n-s2c.pre*s2c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[18,11]=mean(sum((dADLpost-dADLpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[18,12]=mean(sum((gADLpost-gADLpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
  detach(meta2.3)


## QOL: between-group and pre/post comparison 
  attach(meta3.1)   # Intervention = 3 - Nursing
  # pre intervention
    # observation M, SD
    t1[19,1]=sum((m3e.pre*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[19,2]=sqrt(sum(s3e.pre*s3e.pre*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2

    # contrast M and SD
    t1[20,1]=sum((m3c.pre*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[20,2]=sqrt(sum(s3c.pre*s3c.pre*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[21,1]=sum((m3e.pre*experiment.n-m3c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[21,2]=sqrt(abs(sum((s3e.pre*s3e.pre*experiment.n-s3c.pre*s3c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[21,3]=mean(sum(dQOLpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[21,4]=mean(sum(gQOLpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))


  # post intervention
    # observation M, SD
    t1[19,5]=sum((m3e.post*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[19,6]=sqrt(sum(s3e.post*s3e.post*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2
    
    # contrast M and SD
    t1[20,5]=sum((m3c.post*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[20,6]=sqrt(sum(s3c.post*s3c.post*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[21,5]=sum((m3e.post*experiment.n-m3c.post*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[21,6]=sqrt(abs(sum((s3e.post*s3e.post*experiment.n-s3c.post*s3c.post*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[21,7]=mean(sum(dQOLpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[21,8]=mean(sum(gQOLpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    
    
  # post-pre intervention
    # observation M, SD
    t1[19,9]=sum(((m3e.post-m3e.pre)*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[19,10]=sqrt(abs(sum((s3e.post*s3e.post-s3e.pre*s3e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2

    # contrast M and SD
    t1[20,9]=sum(((m3c.post-m3c.pre)*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[20,10]=sqrt(abs(sum((s3c.post*s3c.post-s3c.pre*s3e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2
    
    # observation - contrast M, SD, d, g
    t1[21,9]=sum((m3e.post*experiment.n-m3c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[21,10]=sqrt(abs(sum((s3e.post*s3e.post*experiment.n-s3c.pre*s3c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[21,11]=mean(sum((dQOLpost-dQOLpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[21,12]=mean(sum((gQOLpost-gQOLpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
  detach(meta3.1)

  attach(meta3.2)   # Intervention = 2 - Psychological
  # pre intervention
    # observation M, SD
    t1[22,1]=sum((m3e.pre*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[22,2]=sqrt(sum(s3e.pre*s3e.pre*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2

    # contrast M and SD
    t1[23,1]=sum((m3c.pre*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[23,2]=sqrt(sum(s3c.pre*s3c.pre*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[24,1]=sum((m3e.pre*experiment.n-m3c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[24,2]=sqrt(abs(sum((s3e.pre*s3e.pre*experiment.n-s3c.pre*s3c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[24,3]=mean(sum(dQOLpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[24,4]=mean(sum(gQOLpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))


  # post intervention
    # observation M, SD
    t1[22,5]=sum((m3e.post*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[22,6]=sqrt(sum(s3e.post*s3e.post*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2
    
    # contrast M and SD
    t1[23,5]=sum((m3c.post*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[23,6]=sqrt(sum(s3c.post*s3c.post*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[24,5]=sum((m3e.post*experiment.n-m3c.post*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[24,6]=sqrt(abs(sum((s3e.post*s3e.post*experiment.n-s3c.post*s3c.post*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[24,7]=mean(sum(dQOLpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[24,8]=mean(sum(gQOLpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    
    
  # post-pre intervention
    # observation M, SD
    t1[22,9]=sum(((m3e.post-m3e.pre)*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[22,10]=sqrt(abs(sum((s3e.post*s3e.post-s3e.pre*s3e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2

    # contrast M and SD
    t1[23,9]=sum(((m3c.post-m3c.pre)*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[23,10]=sqrt(abs(sum((s3c.post*s3c.post-s3c.pre*s3e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2
    
    # observation - contrast M, SD, d, g
    t1[24,9]=sum((m3e.post*experiment.n-m3c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[24,10]=sqrt(abs(sum((s3e.post*s3e.post*experiment.n-s3c.pre*s3c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[24,11]=mean(sum((dQOLpost-dQOLpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[24,12]=mean(sum((gQOLpost-gQOLpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
  detach(meta3.2)

  attach(meta3.3)   # Intervention = 1 - Comprehensive
  # pre intervention
    # observation M, SD
    t1[25,1]=sum((m3e.pre*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[25,2]=sqrt(sum(s3e.pre*s3e.pre*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2

    # contrast M and SD
    t1[26,1]=sum((m3c.pre*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[26,2]=sqrt(sum(s3c.pre*s3c.pre*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[27,1]=sum((m3e.pre*experiment.n-m3c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[27,2]=sqrt(abs(sum((s3e.pre*s3e.pre*experiment.n-s3c.pre*s3c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[27,3]=mean(sum(dQOLpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[27,4]=mean(sum(gQOLpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))


  # post intervention
    # observation M, SD
    t1[25,5]=sum((m3e.post*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[25,6]=sqrt(sum(s3e.post*s3e.post*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2
    
    # contrast M and SD
    t1[26,5]=sum((m3c.post*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[26,6]=sqrt(sum(s3c.post*s3c.post*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[27,5]=sum((m3e.post*experiment.n-m3c.post*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[27,6]=sqrt(abs(sum((s3e.post*s3e.post*experiment.n-s3c.post*s3c.post*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[27,7]=mean(sum(dQOLpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[27,8]=mean(sum(gQOLpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))


  # post-pre intervention
    # observation M, SD
    t1[25,9]=sum(((m3e.post-m3e.pre)*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[25,10]=sqrt(abs(sum((s3e.post*s3e.post-s3e.pre*s3e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2

    # contrast M and SD
    t1[26,9]=sum(((m3c.post-m3c.pre)*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[26,10]=sqrt(abs(sum((s3c.post*s3c.post-s3c.pre*s3e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2
    
    # observation - contrast M, SD, d, g
    t1[27,9]=sum((m3e.post*experiment.n-m3c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[27,10]=sqrt(abs(sum((s3e.post*s3e.post*experiment.n-s3c.pre*s3c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[27,11]=mean(sum((dQOLpost-dQOLpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[27,12]=mean(sum((gQOLpost-gQOLpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
  
    detach(meta3.3)


  write.csv(t1,file="C:/Users/chens/Desktop/Intervention Effect Analysis.csv",row.names=FALSE)
  
  write.csv(t1,file="C:/Users/Student RA/vscode temp/Intervention Effect Analysis.csv",row.names=FALSE)


### TABLE 7 Model Comparison 
  ## Fixed Effect Model by measurement
    fem1=metacont(experiment.n,m1e.post,s1e.post,contrast.n,m1c.post,s1c.post,data=meta1,sm="SMD",byvar=intervention2) # MMSE
    fem2=metacont(experiment.n,m2e.post,s2e.post,contrast.n,m2c.post,s2c.post,data=meta2,sm="SMD",byvar=intervention2) # ADL
    fem3=metacont(experiment.n,m3e.post,s3e.post,contrast.n,m3c.post,s3c.post,data=meta3,sm="SMD",byvar=intervention2) # QOL
    # eliminate null rows

    # significant heterogeneity between groups, suggesting random effect model.
    # Robust Multichip Average measures check (methods=FE/REML)
    res1=rma(n1i=experiment.n,n2i=contrast.n,m1i=m1e.post,m2i=m1c.post,sd1i=s1e.post,sd2i=s1c.post,measure="SMD",method="FE",data=meta1,slab=authoryear)
    res2=rma(n1i=experiment.n,n2i=contrast.n,m1i=m1e.post,m2i=m1c.post,sd1i=s1e.post,sd2i=s1c.post,measure="SMD",method="FE",data=meta2,slab=authoryear)
    res3=rma(n1i=experiment.n,n2i=contrast.n,m1i=m1e.post,m2i=m1c.post,sd1i=s1e.post,sd2i=s1c.post,measure="SMD",method="FE",data=meta3,slab=authoryear)
    
    
    res1.1=rma(n1i=experiment.n,n2i=contrast.n,m1i=m1e.post,m2i=m1c.post,sd1i=s1e.post,sd2i=s1c.post,measure="SMD",method="FE",data=meta1.1,slab=authoryear)
    res1.2=rma(n1i=experiment.n,n2i=contrast.n,m1i=m1e.post,m2i=m1c.post,sd1i=s1e.post,sd2i=s1c.post,measure="SMD",method="FE",data=meta1.2,slab=authoryear)
    res1.3=rma(n1i=experiment.n,n2i=contrast.n,m1i=m1e.post,m2i=m1c.post,sd1i=s1e.post,sd2i=s1c.post,measure="SMD",method="FE",data=meta1.3,slab=authoryear)
    res2.1=rma(n1i=experiment.n,n2i=contrast.n,m1i=m2e.post,m2i=m2c.post,sd1i=s2e.post,sd2i=s2c.post,measure="SMD",method="FE",data=meta2.1,slab=authoryear)
    res2.2=rma(n1i=experiment.n,n2i=contrast.n,m1i=m2e.post,m2i=m2c.post,sd1i=s2e.post,sd2i=s2c.post,measure="SMD",method="FE",data=meta2.2,slab=authoryear)
    res2.3=rma(n1i=experiment.n,n2i=contrast.n,m1i=m2e.post,m2i=m2c.post,sd1i=s2e.post,sd2i=s2c.post,measure="SMD",method="FE",data=meta2.3,slab=authoryear)
    res3.1=rma(n1i=experiment.n,n2i=contrast.n,m1i=m3e.post,m2i=m3c.post,sd1i=s3e.post,sd2i=s3c.post,measure="SMD",method="FE",data=meta3.1,slab=authoryear)
    res3.2=rma(n1i=experiment.n,n2i=contrast.n,m1i=m3e.post,m2i=m3c.post,sd1i=s3e.post,sd2i=s3c.post,measure="SMD",method="FE",data=meta3.2,slab=authoryear)
    res3.3=rma(n1i=experiment.n,n2i=contrast.n,m1i=m3e.post,m2i=m3c.post,sd1i=s3e.post,sd2i=s3c.post,measure="SMD",method="FE",data=meta3.3,slab=authoryear)
      # n1i/n2i - patient number; m1i/m2i - mean score of two groups; sd1i/sd2i - std of score of groups
  

  
  ## Random Effect Model
  #    attach(meta)
  #      ratio1=abs((m1e.post-m1e.pre)/(m1c.post-m1c.pre))
  #      ratio2=abs((m2e.post-m2e.pre)/(m2c.post-m2c.pre))
  #      ratio3=abs((m3e.post-m3e.pre)/(m3c.post-m3c.pre))
  #    detach(meta)
  #    meta_REM=cbind(meta,ratio1,ratio2,ratio3)
  #    
  #    REM1=meta_REM[which(is.na(ratio1)==F),]
  #    REM2=meta_REM[which(is.na(ratio2)==F),]
  #    REM3=meta_REM[which(is.na(ratio3)==F),]
  #    
  #    MMSE = lme(ratio1 ~ average.age+m1e.pre,random = ~ 1|intervention2, data = REM1,na.action=na.omit)  
  #    ADL = lme(ratio2 ~ average.age+intervention2,random = ~ 1|average.age, data = REM2,na.action=na.omit)  
  #    QOL = lme(ratio3 ~ average.age+intervention2,random = ~ 1|average.age, data = REM3,na.action=na.omit)  
  #    summary(MMSE)
  #    summary(ADL)
  #    summary(QOL)
  #    anova(MMSE)
  #    anova(ADL)
  #    anova(QOL)


### Figures
##  Forest Plots
  # forestplot()
  # metabin()       # library(meta)
  # trimfill()      # sensitivity assessment


  # grid.text("My custom title", .5, .9, gp=gpar(cex=2))



  ## decrease marginal space so the full space is used
  par(mar=c(4,4,1,2),cex=1.2,font=2,mfrow=c(1,1)) 
  
  # par(mar=c(4,4,1,2),cex=1.2,font=2,mfrow=c(3,3)) 

  ## MMSE * Comprehenswive
    attach(meta1.1)
    forest(res1.1,xlim=c(-12,6),
            ilab=cbind(experiment.n,m1e.post,s1e.post,contrast.n,m1c.post,s1c.post),
            ilab.x=c(-10,-8.5,-7,-5.5,-4,-2.5),cex=1.2)
    detach(meta1.1)

    grid.text("MMSE Measurement X Comprehensive Intervention", 0.48, 0.99, gp=gpar(cex=1.2))
    text(c(-8.5,-4),30,c("Experiment Group","Control Group"))     # counted from the bottom
    text(c(-10,-8.5,-7,-5.5,-4,-2.5),29,c("Sample Size","Mean","Std.","Sample Size","Mean","Std."))
    text(6,29,"SMD[95% CI]",pos=2)
    text(-11, -0.99, pos=4, cex=1.2, bquote(paste("for All Studies (Q = ",
      .(formatC(res1.1$QE, digits=2, format="f")), ", df = ", .(res1.1$k-res1.1$p),
      ", p = ", .(formatC(res1.1$QEp, digits=2, format="f")), "; ", I^2, " = ",
      .(formatC(res1.1$I2, digits=1, format="f")), "%)")))


  ## MMSE * Psychological
    attach(meta1.2)
    forest(res1.2,xlim=c(-12,6),ilab=cbind(experiment.n,m1e.post,s1e.post,contrast.n,m1c.post,s1c.post),
            ilab.x=c(-10,-8.5,-7,-5.5,-4,-2.5),cex=.8)
    detach(meta1.2)

    grid.text("MMSE measurement with Psychological Intervention", 0.48, 0.99, gp=gpar(cex=1.2))
    text(c(-8.5,-4),18,c("Experiment Group","Control Group"))     # counted from the bottom
    text(c(-10,-8.5,-7,-5.5,-4,-2.5),17,c("Sample Size","Mean","Std.","Sample Size","Mean","Std."))
    text(6,17,"SMD[95% CI]",pos=2)
    text(-11, -0.99, pos=4, cex=1.2, bquote(paste("for All Studies (Q = ",
      .(formatC(res1.2$QE, digits=2, format="f")), ", df = ", .(res1.2$k-res1.2$p),
      ", p = ", .(formatC(res1.2$QEp, digits=2, format="f")), "; ", I^2, " = ",
      .(formatC(res1.2$I2, digits=1, format="f")), "%)")))

  ## MMSE * Nursing
    attach(meta1.3)
    forest(res1.3,xlim=c(-12,6),ilab=cbind(experiment.n,m1e.post,s1e.post,contrast.n,m1c.post,s1c.post),
            ilab.x=c(-10,-8.5,-7,-5.5,-4,-2.5),cex=1.2)
    detach(meta1.3)

    grid.text("MMSE measurement with Nursing Intervention", 0.48, 0.99, gp=gpar(cex=1.2))
    text(c(-8.5,-4),31,c("Experiment Group","Control Group"))     # counted from the bottom
    text(c(-10,-8.5,-7,-5.5,-4,-2.5),30,c("Sample Size","Mean","Std.","Sample Size","Mean","Std."))
    text(6,30,"SMD[95% CI]",pos=2)
    text(-11, -0.99, pos=4, cex=1.2, bquote(paste("for All Studies (Q = ",
      .(formatC(res1.3$QE, digits=2, format="f")), ", df = ", .(res1.3$k-res1.3$p),
      ", p = ", .(formatC(res1.3$QEp, digits=2, format="f")), "; ", I^2, " = ",
      .(formatC(res1.3$I2, digits=1, format="f")), "%)")))


  ## ADL * Comprehenswive 
    attach(meta2.1)
    forest(res2.1,xlim=c(-16,4),ilab=cbind(experiment.n,m2e.post,s2e.post,contrast.n,m2c.post,s2c.post),
            ilab.x=c(-13,-11.5,-10,-8.5,-7,-5.5),cex=1.2)
    detach(meta2.1)

    grid.text("ADL measurement with Comprehensive Intervention", 0.48, 0.99, gp=gpar(cex=1.2))
    text(c(-11.5,-7),35,c("Experiment Group","Control Group"))     # counted from the bottom
    text(c(-13,-11.5,-10,-8.5,-7,-5.5),34,c("Sample Size","Mean","Std.","Sample Size","Mean","Std."))
    text(4,34,"SMD[95% CI]",pos=2)
    text(-15, -0.99, pos=4, cex=1.2, bquote(paste("for All Studies (Q = ",
      .(formatC(res2.1$QE, digits=2, format="f")), ", df = ", .(res2.1$k-res2.1$p),
      ", p = ", .(formatC(res2.1$QEp, digits=2, format="f")), "; ", I^2, " = ",
      .(formatC(res2.1$I2, digits=1, format="f")), "%)")))

  ## ADL * Psychological 
    attach(meta2.2)
    forest(res2.2,xlim=c(-16,4),ilab=cbind(experiment.n,m2e.post,s2e.post,contrast.n,m2c.post,s2c.post),
            ilab.x=c(-13,-11.5,-10,-8.5,-7,-5.5),cex=1.2)
    detach(meta2.2)

    grid.text("ADL measurement with Psychological Intervention", 0.48, 0.99, gp=gpar(cex=1.2))
    text(c(-11.5,-7),12,c("Experiment Group","Control Group"))     # counted from the bottom
    text(c(-13,-11.5,-10,-8.5,-7,-5.5),11.5,c("Sample Size","Mean","Std.","Sample Size","Mean","Std."))
    text(4,11.5,"SMD[95% CI]",pos=2)
    text(-14.8, -0.99, pos=4, cex=1.2, bquote(paste("for All Studies (Q = ",
      .(formatC(res2.2$QE, digits=2, format="f")), ", df = ", .(res2.2$k-res2.2$p),
      ", p = ", .(formatC(res2.2$QEp, digits=2, format="f")), "; ", I^2, " = ",
      .(formatC(res2.2$I2, digits=1, format="f")), "%)")))

  ## ADL * Nursing
    attach(meta2.3)
    forest(res2.3,xlim=c(-13,4),ilab=cbind(experiment.n,m2e.post,s2e.post,contrast.n,m2c.post,s2c.post),
            ilab.x=c(-11,-9.5,-8,-6.5,-5,-3.5),cex=1.2)
    detach(meta2.3)

    grid.text("ADL measurement with Nursing Intervention", 0.48, 0.99, gp=gpar(cex=1.2))
    text(c(-9.5,-5),29,c("Experiment Group","Control Group"))     # counted from the bottom
    text(c(-11,-9.5,-8,-6.5,-5,-3.5),28,c("Sample Size","Mean","Std.","Sample Size","Mean","Std."))
    text(4,28,"SMD[95% CI]",pos=2)
    text(-12.4, -0.99, pos=4, cex=1.2, bquote(paste("for All Studies (Q = ",
      .(formatC(res2.3$QE, digits=2, format="f")), ", df = ", .(res2.3$k-res2.3$p),
      ", p = ", .(formatC(res2.3$QEp, digits=2, format="f")), "; ", I^2, " = ",
      .(formatC(res2.3$I2, digits=1, format="f")), "%)")))

  ## QOL * Comprehenswive
    attach(meta3.1)
    forest(res3.1,xlim=c(-12,9),ilab=cbind(experiment.n,m3e.post,s3e.post,contrast.n,m3c.post,s3c.post),
            ilab.x=c(-10,-8.5,-7,-5.5,-4,-2.5),cex=1.2)
    detach(meta3.1)

    grid.text("QOL measurement with Comprehensive Intervention", 0.48, 0.99, gp=gpar(cex=1.2))
    text(c(-8.5,-4),10,c("Experiment Group","Control Group"))     # counted from the bottom
    text(c(-10,-8.5,-7,-5.5,-4,-2.5),9.5,c("Sample Size","Mean","Std.","Sample Size","Mean","Std."))
    text(9,9.5,"SMD[95% CI]",pos=2)
    text(-11, -0.99, pos=4, cex=1.2, bquote(paste("for All Studies (Q = ",
      .(formatC(res3.1$QE, digits=2, format="f")), ", df = ", .(res3.1$k-res3.1$p),
      ", p = ", .(formatC(res3.1$QEp, digits=2, format="f")), "; ", I^2, " = ",
      .(formatC(res3.1$I2, digits=1, format="f")), "%)")))


  ## QOL * Psychological
    attach(meta3.2)
    forest(res3.2,xlim=c(-12,6),ilab=cbind(experiment.n,m3e.post,s3e.post,contrast.n,m3c.post,s3c.post),
            ilab.x=c(-10,-8.5,-7,-5.5,-4,-2.5),cex=1.2)
    detach(meta3.2)

    grid.text("QOL measurement with Psychological Intervention", 0.48, 0.92, gp=gpar(cex=1.2))
    text(c(-8.5,-4),5,c("Experiment Group","Control Group"))     # counted from the bottom
    text(c(-10,-8.5,-7,-5.5,-4,-2.5),4.5,c("Sample Size","Mean","Std.","Sample Size","Mean","Std."))
    text(6,4.5,"SMD[95% CI]",pos=2)
    text(-11, -0.99, pos=4, cex=1.2, bquote(paste("for All Studies (Q = ",
      .(formatC(res3.2$QE, digits=2, format="f")), ", df = ", .(res3.2$k-res3.2$p),
      ", p = ", .(formatC(res3.2$QEp, digits=2, format="f")), "; ", I^2, " = ",
      .(formatC(res3.2$I2, digits=1, format="f")), "%)")))

  ## QOL * Nursing
    attach(meta3.3)
    forest(res3.3,xlim=c(-12,6),ilab=cbind(experiment.n,m3e.post,s3e.post,contrast.n,m3c.post,s3c.post),
            ilab.x=c(-10,-8.5,-7,-5.5,-4,-2.5),cex=1.2)
    detach(meta3.3)

    grid.text("QOL measurement with Nursing Intervention", 0.48, 0.95, gp=gpar(cex=1.2))
    text(c(-8.5,-4),7,c("Experiment Group","Control Group"))     # counted from the bottom
    text(c(-10,-8.5,-7,-5.5,-4,-2.5),6.5,c("Sample Size","Mean","Std.","Sample Size","Mean","Std."))
    text(6,6.5,"SMD[95% CI]",pos=2)
    text(-11, -0.99, pos=4, cex=1.2, bquote(paste("for All Studies (Q = ",
      .(formatC(res3.3$QE, digits=2, format="f")), ", df = ", .(res3.3$k-res3.3$p),
      ", p = ", .(formatC(res3.3$QEp, digits=2, format="f")), "; ", I^2, " = ",
      .(formatC(res3.3$I2, digits=1, format="f")), "%)")))

# funnel plots, radial plot, 11 plot, baujat plot
  
   
  #MMSE * comprehensive
  par(mfcol=c(2,2))
  funnel(res1,main="Funnel Plot")
  radial(res1,main="Radial Plot")
  qqnorm(res1,main="Normal QQ Plot")
  baujat(res1,main="Baujat Plot")
  
  par(mfcol=c(2,2))
  funnel(res2,main="Funnel Plot")
  radial(res2,main="Radial Plot")
  qqnorm(res2,main="Normal QQ Plot")
  baujat(res2,main="Baujat Plot")
  
  par(mfcol=c(2,2))
  funnel(res3,main="Funnel Plot")
  radial(res3,main="Radial Plot")
  qqnorm(res3,main="Normal QQ Plot")
  baujat(res3,main="Baujat Plot")
  
  
  
  
  par(mfcol=c(2,2))
  funnel(res1.1,main="Funnel Plot")
  radial(res1.1,main="Radial Plot")
  qqnorm(res1.1,main="Normal QQ Plot")
  baujat(res1.1,main="Baujat Plot")

  #MMSE * psychological
  par(mfcol=c(2,2))
  funnel(res1.2,main="Funnel Plot")
  radial(res1.2,main="Radial Plot")
  qqnorm(res1.2,main="Normal QQ Plot")
  baujat(res1.2,main="Baujat Plot")

  #MMSE * nursing
  par(mfcol=c(2,2))
  funnel(res1.3,main="Funnel Plot")
  radial(res1.3,main="Radial Plot")
  qqnorm(res1.3,main="Normal QQ Plot")
  baujat(res1.3,main="Baujat Plot")


  #ADL * comprehensive
  par(mfcol=c(2,2))
  funnel(res2.1,main="Funnel Plot")
  radial(res2.1,main="Radial Plot")
  qqnorm(res2.1,main="Normal QQ Plot")
  baujat(res2.1,main="Baujat Plot")
  
  #ADL * psychological 
  par(mfcol=c(2,2))
  funnel(res2.2,main="Funnel Plot")
  radial(res2.2,main="Radial Plot")
  qqnorm(res2.2,main="Normal QQ Plot")
  baujat(res2.2,main="Baujat Plot")

  #ADL * nursing
  par(mfcol=c(2,2))
  funnel(res2.3,main="Funnel Plot")
  radial(res2.3,main="Radial Plot")
  qqnorm(res2.3,main="Normal QQ Plot")
  baujat(res2.3,main="Baujat Plot")

  #QOL * comprehensive
  par(mfcol=c(2,2))
  funnel(res3.1,main="Funnel Plot")
  radial(res3.1,main="Radial Plot")
  qqnorm(res3.1,main="Normal QQ Plot")
  baujat(res3.1,main="Baujat Plot")
  
  #QOL * psychological
  par(mfcol=c(2,2))
  funnel(res3.2,main="Funnel Plot")
  radial(res3.2,main="Radial Plot")
  qqnorm(res3.2,main="Normal QQ Plot")
  baujat(res3.2,main="Baujat Plot")

  #QOL * nursing
  par(mfcol=c(2,2))
  funnel(res3.3,main="Funnel Plot")
  radial(res3.3,main="Radial Plot")
  qqnorm(res3.3,main="Normal QQ Plot")
  baujat(res3.3,main="Baujat Plot")


  # sensitivity test
  plot(influence(res1))
  plot(influence(res2))
  plot(influence(res3))
  
 
  plot(influence(res1.1))
  plot(influence(res1.2))
  plot(influence(res1.3))
  plot(influence(res2.1))
  plot(influence(res2.2))
  plot(influence(res2.3))
  plot(influence(res3.1))
  plot(influence(res3.2))
  plot(influence(res3.3))

##  Output using base:: or 
 sink("result.txt")
 print(summary(fem1))
 print(summary(fem2))
 print(summary(fem3))
 sink()

 ## Flow chart / diagram
par(mar=c(4,4,1,2),cex=1.2,font=2,mfrow=c(1,1)) 

 prisma(found=9681,
       found_other = 729,
       no_dupes = 10378, 
       screened = 9356, 
       screen_exclusions = 9123, 
       full_text = 233,
       full_text_exclusions = 22, 
       qualitative = 108, 
       quantitative = 89,
       extra_dupes_box = TRUE)




(TI = ((caretak* OR caregiv* OR caring OR career OR care) AND (meaning-making OR make meaning OR making sense))
 AND TS= ((tool* OR model OR scale OR theor*) AND (make sense OR sense making OR value OR identity OR filial piety OR family) 
 NOT (nursing care OR nurse OR decision-making OR hospital OR school OR education OR teachers OR student OR pet OR animal)))