### Library packages
library(meta);library(metafor);library(nlme)

### Data Retrieval from "test.txt"
meta1=read.table("D:/OneDrive/research/1personal/Geriatrics/meta/test.txt",header=T,sep="\t",na.strings = "NA")

### Demographic Information 
##  TABLE 1 - Literature Info - edit in excel file "meta-analysis" (the newest version 180325)
##  TABLE 2 - Demographic Info
attach(meta1)
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
detach(meta1)

## TABLE 3 - detailed demographic on year, age, and locations
age1=meta1[which(meta1$age1859==1),]
  table(age1859)
  attach(age1);table(MMSE);table(ADL);table(QOL);table(intervention2)
  detach(age1)

age2=meta1[which(meta1$age6064==1),]
  attach(age2)
  table(age6064);table(intervention2);table(MMSE);table(ADL);table(QOL)
  detach(age2)

age3=meta1[which(meta1$age6570==1),]
  attach(age3)
  table(age6570);table(intervention2);table(MMSE);table(ADL);table(QOL)
  detach(age3)
  
age4=meta1[which(meta1$age7080==1),]
  attach(age4)
  table(age7080);table(intervention2);table(MMSE);table(ADL);table(QOL)
  detach(age4)
  
age5=meta1[which(meta1$age80.==1),]
  attach(age5)
  table(age80.);table(intervention2);table(MMSE);table(ADL);table(QOL)
  detach(age5)

### TABLE - Intervention Effect Analysis (MMSE, ADL, QOL)
  ## Data Prep - calculate Cohen's d and Hedges' g for MMSE, ADL and QOL
      # factors not recognizable as numbers
      attach(meta1)
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
        
        gOTHERpre=J*dQOLpre
        gOTHERpost=J*dQOLpost
        gOTHERexp=J*dQOLpre
        gOTHERcon=J*dQOLcon
  
      detach(meta1)
    
    # factor table
      
      c=matrix(NA,89,6)
      c[,1]=dMMSEpost
      c[,2]=dADLpost
      c[,3]=dQOLpost
      c[,4]=gMMSEpost
      c[,5]=gADLpost
      c[,6]=gOTHERpost

    
