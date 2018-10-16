### Library packages
library(meta);library(metafor);library(nlme)

### Data Retrieval from "test.txt"
meta=read.table("C:/Users/chens/OneDrive/research/1personal/Geriatrics/meta/test.txt",header=T,sep="\t",na.strings = "NA")

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

### TABLE 4-6 - Intervention Effect Analysis (MMSE, ADL, QOL)
## Data Prep - calculate Cohen's d and Hedges' g for MMSE, ADL and QOL
  # factors not recognizable as numbers
  attach(meta)
  m1e.pre = as.numeric(m1e.pre)
  m1c.pre = as.numeric(m1c.pre)
  
  # Cohen's d
    # MMSE: between-group and pre-post comparison
    dMMSEpre=(m1e.pre-m1c.pre)/(sqrt((s1e.pre*s1e.pre*(experiment.n-1)+s1c.pre*s1c.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dMMSEpost=(m1e.post-m1c.post)/(sqrt((s1e.post*s1e.post*(experiment.n-1)+s1c.post*s1c.post*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dMMSEexp=(m1e.post-m1e.pre)/(sqrt((s1e.post*s1e.post*(experiment.n-1)+s1e.pre*s1e.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dMMSEcon=(m1c.post-m1c.pre)/(sqrt((s1c.post*s1c.post*(experiment.n-1)+s1c.pre*s1c.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))

    # ADL: between-group and pre-post comparison
    dADLpre=(m2e.pre-m2c.pre)/(sqrt((s2e.pre*s2e.pre*(experiment.n-1)+s2c.pre*s2c.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dADLpost=(m2e.post-m2c.post)/(sqrt((s2e.post*s2e.post*(experiment.n-1)+s2c.post*s2c.post*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dADLexp=(m2e.post-m2e.pre)/(sqrt((s2e.post*s2e.post*(experiment.n-1)+s2e.pre*s2e.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dADLcon=(m2c.post-m2c.pre)/(sqrt((s2c.post*s2c.post*(experiment.n-1)+s2c.pre*s2c.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))

    # QOL: between-group and pre-post comparison
    dQOLpre=(m3e.pre-m3c.pre)/(sqrt((s3e.pre*s3e.pre*(experiment.n-1)+s3c.pre*s3c.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dQOLpost=(m3e.post-m3c.post)/(sqrt((s3e.post*s3e.post*(experiment.n-1)+s3c.post*s3c.post*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dQOLexp=(m3e.post-m3e.pre)/(sqrt((s3e.post*s3e.post*(experiment.n-1)+s3e.pre*s3e.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))
    dQOLcon=(m3c.post-m3c.pre)/(sqrt((s3c.post*s3c.post*(experiment.n-1)+s3c.pre*s3c.pre*(contrast.n-1))/(experiment.n+contrast.n-2)))
  

  # Hedges' g
    # J calculation
    J=1-(3/(4*(experiment.n+contrast.n-2)-1))
    # g calculation
    gMMSEpre=J*dMMSEpre
    gMMSEpost=J*dMMSEpost
    gMMSEexp=J*dMMSEpre
    gMMSEcon=J*dMMSEcon
    
    gADLpre=J*dADLpre
    gADLpost=J*dADLpost
    gADLexp=J*dADLpre
    gADLcon=J*dADLcon
    
    gQOLpre=J*dQOLpre
    gQOLpost=J*dQOLpost
    gQOLexp=J*dQOLpre
    gQOLcon=J*dQOLcon

  detach(meta)
  
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

  meta1.3=meta[which(meta$MMSE==1 & meta$intervention2==3),]
  meta1.2=meta[which(meta$MMSE==1 & meta$intervention2==2),]
  meta1.1=meta[which(meta$MMSE==1 & meta$intervention2==1),]
  meta2.3=meta[which(meta$ADL==1 & meta$intervention2==3),]
  meta2.2=meta[which(meta$ADL==1 & meta$intervention2==2),]
  meta2.1=meta[which(meta$ADL==1 & meta$intervention2==1),]
  meta3.3=meta[which(meta$QOL==1 & meta$intervention2==3),]
  meta3.2=meta[which(meta$QOL==1 & meta$intervention2==2),]
  meta3.1=meta[which(meta$QOL==1 & meta$intervention2==1),]

  t1=matrix(,27,12)
## MMSE: between-group and pre/post comparison 
  attach(meta1.3)   # Intervention = 3 - Nursing
  # pre intervention
    # observation M, SD
    t1[1,1]=sum((m1e.pre*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[1,2]=sqrt(sum(s1e.pre*s1e.pre*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2

    # contrast M and SD
    t1[2,1]=sum((m1c.pre*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[2,2]=sqrt(sum(s1c.pre*s1c.pre*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[3,1]=sum((m1e.pre*experiment.n-m1c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
    t1[3,2]=sqrt(abs(sum((s1e.pre*s1e.pre*experiment.n-s1c.pre*s1c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
    t1[3,3]=mean(sum(dMMSEpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    t1[3,4]=mean(sum(gMMSEpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))


  # post intervention
    # observation M, SD
    t1[1,5]=sum((m1e.post*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
    t1[1,6]=sqrt(sum(s1e.post*s1e.post*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2
    
    # contrast M and SD
    t1[2,5]=sum((m1c.post*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
    t1[2,6]=sqrt(sum(s1c.post*s1c.post*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
    
    # observation - contrast M, SD, d, g
    t1[3,5]=sum((m1e.post*experiment.n-m1c.post*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
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
  detach(meta1.3)

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

  attach(meta1.1)   # Intervention = 1 - Comprehensive
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
  detach(meta1.1)


## ADL: between-group and pre/post comparison 
  attach(meta2.3)   # Intervention = 3 - Nursing
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
  detach(meta2.3)

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

  attach(meta2.1)   # Intervention = 1 - Comprehensive
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
  detach(meta2.1)


## QOL: between-group and pre/post comparison 
  attach(meta3.3)   # Intervention = 3 - Nursing
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
  detach(meta3.3)

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

  attach(meta3.1)   # Intervention = 1 - Comprehensive
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
  detach(meta3.1)

  write.csv(t1,file="C:/Users/chens/Desktop/Intervention Effect Analysis.csv",row.names=FALSE)
  
### TABLE 7 Model Comparison 
  ## Fixed Effect Model by measurement
    fem1=metacont(experiment.n,m1e.post,s1e.post,contrast.n,m1c.post,s1c.post,data=meta,sm="SMD",byvar=intervention2)
    fem2=metacont(experiment.n,m2e.post,s2e.post,contrast.n,m2c.post,s2c.post,data=meta,sm="SMD",byvar=intervention2)
    fem3=metacont(experiment.n,m3e.post,s3e.post,contrast.n,m3c.post,s3c.post,data=meta,sm="SMD",byvar=intervention2)
      # significant heterogeneity between groups, suggesting random effect model.
      # Robust Multichip Average measures check (methods=FE/REML)
      rma1=rma(n1i=experiment.n,n2i=contrast.n,m1i=m1e.post,m2i=m1c.post,sd1i=s1e.post,sd2i=s1c.post,measure="SMD",method="FE",data=meta)
      rma2=rma(n1i=experiment.n,n2i=contrast.n,m1i=m2e.post,m2i=m2c.post,sd1i=s2e.post,sd2i=s2c.post,measure="SMD",method="FE",data=meta)
      rma3=rma(n1i=experiment.n,n2i=contrast.n,m1i=m3e.post,m2i=m3c.post,sd1i=s3e.post,sd2i=s3c.post,measure="SMD",method="FE",data=meta)
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
  forestplot()
  metabin()       # library(meta)
  trimfill()      # sensitivity assessment

# funnel plots, radial plot, 11 plot, baujat plot
  
  # overall
  par(mfcol=c(2,2))
  funnel(res1,main="Funnel Plot")
  radial(res1,main="Radial Plot")
  qqnorm(res1,main="Normal QQ Plot")
  baujat(res1,main="Baujat Plot")
    
  #MMSE
  par(mfcol=c(2,2))
  funnel(res1,main="Funnel Plot")
  radial(res1,main="Radial Plot")
  qqnorm(res1,main="Normal QQ Plot")
  baujat(res1,main="Baujat Plot")

  #ADL
  par(mfcol=c(2,2))
  funnel(res2,main="Funnel Plot")
  radial(res2,main="Radial Plot")
  qqnorm(res2,main="Normal QQ Plot")
  baujat(res2,main="Baujat Plot")
  
  
  #QOL
  par(mfcol=c(2,2))
  funnel(res3,main="Funnel Plot")
  radial(res3,main="Radial Plot")
  qqnorm(res3,main="Normal QQ Plot")
  baujat(res3,main="Baujat Plot")
  
  
# sensitivity test
plot(influence(res1))
plot(influence(res2))
plot(influence(res3))

# publication bias check
metabias(model1,method.bias="linreg",plotit=T,k.min=5)