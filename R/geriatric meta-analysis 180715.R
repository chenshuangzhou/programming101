#### data perp
meta1=read.table("D:/OneDrive/research/1personal/Geriatrics/meta/test.txt",header=T,sep="\t",na.strings = "NA")
library(meta);library(metafor)
library(nlme)

  # meta1=read.table("C:/Users/admin/Desktop/test.txt",header=T,sep="\t")

#### Demographic

## Table 1 - edit in excel file
## Table 2 demographic data

attach(meta1)
table(year)
summary(average.age)
table(location3)
summary(sample.size)
summary(male);summary(female)
table(year,intervention2)
table(year,MMSE);table(year,ADL);table(year,QOL)
table(location3,intervention2)
table(location3,MMSE);table(location3,ADL);table(location3,QOL)
summary(m1e.pre);summary(m1e.post);summary(m2e.pre);summary(m2e.post);summary(m3e.pre);summary(m3e.post);
table(intervention2)
sum(average.age*sample.size,na.rm=T)/sum(sample.size,na.rm=T)   # group average age
detach(meta1)

## Table 3 
  age1=meta1[which(meta1$age1859==1),]
    attach(age1)
    table(age1859)
    table(intervention2)
    table(MMSE);table(ADL);table(QOL)
    detach(age1)
  
  age2=meta1[which(meta1$age6064==1),]
    attach(age2)
    table(age6064)
    table(intervention2)
    table(MMSE);table(ADL);table(QOL)
    detach(age2)
  
  age3=meta1[which(meta1$age6570==1),]
    attach(age3)
    table(age6570)
    table(intervention2)
    table(MMSE);table(ADL);table(QOL)
    detach(age3)
    
  age4=meta1[which(meta1$age7080==1),]
    attach(age4)
    table(age7080)
    table(intervention2)
    table(MMSE);table(ADL);table(QOL)
    detach(age4)
    
  age5=meta1[which(meta1$age80.==1),]
    attach(age5)
    table(age80.)
    table(intervention2)
    table(MMSE);table(ADL);table(QOL)
  detach(age5)
    
    
## Table 4-6 MMSE,ADL,QOL

  #### Data prep
    
    # factors not recognizable as numbers
    attach(meta1)
    m1e.pre = as.numeric(m1e.pre)
    m1c.pre = as.numeric(m1c.pre)
    detach(meta1)
    
    
    attach(meta1)
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
    
  
    ## Hedges' g
   
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

    
    write.table(c,"C:/Users/ThinPak/Desktop/c.xls", sep="\t", col.names=F,row.names=F, quote=TRUE, na="NA")
    
  #######  Tables 4-6 ############
  # MMSE: between-group and per-pst comparison
  
   b=matrix(,12,12)
    attach(meta1)
    # pre intervention
      # observation M, SD
      b[1,1]=sum((m1e.pre*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
      b[1,2]=sqrt(sum(s1e.pre*s1e.pre*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2
  
      # contrast M and SD
      b[2,1]=sum((m1c.pre*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
      b[2,2]=sqrt(sum(s1c.pre*s1c.pre*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
     
      # observation - contrast M, SD, d, g
      b[3,1]=sum((m1e.pre*experiment.n-m1c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
      b[3,2]=sqrt(abs(sum((s1e.pre*s1e.pre*experiment.n-s1c.pre*s1c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
      b[3,3]=mean(sum(dMMSEpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
      b[3,4]=mean(sum(gMMSEpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))

  
    # post intervention
      # observation M, SD
      b[1,5]=sum((m1e.post*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
      b[1,6]=sqrt(sum(s1e.post*s1e.post*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2
      
      # contrast M and SD
      b[2,5]=sum((m1c.post*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
      b[2,6]=sqrt(sum(s1c.post*s1c.post*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
      
      # observation - contrast M, SD, d, g
      b[3,5]=sum((m1e.post*experiment.n-m1c.post*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
      b[3,6]=sqrt(abs(sum((s1e.post*s1e.post*experiment.n-s1c.post*s1c.post*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
      b[3,7]=mean(sum(dMMSEpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
      b[3,8]=mean(sum(gMMSEpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
      
      
    # post-pre intervention
      # observation M, SD
      b[1,9]=sum(((m1e.post-m1e.pre)*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
      b[1,10]=sqrt(abs(sum((s1e.post*s1e.post-s1e.pre*s1e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2

      # contrast M and SD
      b[2,9]=sum(((m1c.post-m1c.pre)*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
      b[2,10]=sqrt(abs(sum((s1c.post*s1c.post-s1c.pre*s1e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2
      
      # observation - contrast M, SD, d, g
      b[3,9]=sum((m1e.post*experiment.n-m1c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
      b[3,10]=sqrt(abs(sum((s1e.post*s1e.post*experiment.n-s1c.pre*s1c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
      b[3,11]=mean(sum((dMMSEpost-dMMSEpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
      b[3,12]=mean(sum((gMMSEpost-gMMSEpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
    
   
  
  # ADL: between-group and per-pst comparison
      
  
     # pre intervention
      # observation M, SD
      b[5,1]=sum((m2e.pre*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
      b[5,2]=sqrt(sum(s2e.pre*s2e.pre*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2
      
      # contrast M and SD
      b[6,1]=sum((m2c.pre*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
      b[6,2]=sqrt(sum(s2c.pre*s2c.pre*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
      
      # observation - contrast M, SD, d, g
      b[7,1]=sum((m2e.pre*experiment.n-m2c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
      b[7,2]=sqrt(abs(sum((s2e.pre*s2e.pre*experiment.n-s2c.pre*s2c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
      b[7,3]=mean(sum(dADLpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
      b[7,4]=mean(sum(gADLpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
      
      
      # post intervention
      # observation M, SD
      b[5,5]=sum((m2e.post*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
      b[5,6]=sqrt(sum(s2e.post*s2e.post*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2
      
      # contrast M and SD
      b[6,5]=sum((m2c.post*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
      b[6,6]=sqrt(sum(s2c.post*s2c.post*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
      
      # observation - contrast M, SD, d, g
      b[7,5]=sum((m2e.post*experiment.n-m2c.post*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
      b[7,6]=sqrt(abs(sum((s2e.post*s2e.post*experiment.n-s2c.post*s2c.post*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
      b[7,7]=mean(sum(dADLpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
      b[7,8]=mean(sum(gADLpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
      
      
      # post-pre intervention
      # observation M, SD
      b[5,9]=sum(((m2e.post-m2e.pre)*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
      b[5,10]=sqrt(abs(sum((s2e.post*s2e.post-s2e.pre*s2e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2
      
      # contrast M and SD
      b[6,9]=sum(((m2c.post-m2c.pre)*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
      b[6,10]=sqrt(abs(sum((s2c.post*s2c.post-s2c.pre*s2c.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2
      
      # observation - contrast M, SD, d, g
      b[7,9]=sum((m2e.post*experiment.n-m2c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
      b[7,10]=sqrt(abs(sum((s2e.post*s2e.post*experiment.n-s2c.pre*s2c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
      b[7,11]=mean(sum((dADLpost-dADLpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
      b[7,12]=mean(sum((gADLpost-gADLpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
      
  
      
  # QOL: between-group and per-pst comparison
      
      # pre intervention
      # observation M, SD
      b[9,1]=sum((m3e.pre*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
      b[9,2]=sqrt(sum(s3e.pre*s3e.pre*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2
      
      # contrast M and SD
      b[10,1]=sum((m3c.pre*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
      b[10,2]=sqrt(sum(s3c.pre*s3c.pre*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
      
      # observation - contrast M, SD, d, g
      b[11,1]=sum((m3e.pre*experiment.n-m3c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
      b[11,2]=sqrt(abs(sum((s3e.pre*s3e.pre*experiment.n-s3c.pre*s3c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
      b[11,3]=mean(sum(dQOLpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
      b[11,4]=mean(sum(gOTHERpre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
      
      
      # post intervention
      # observation M, SD
      b[9,5]=sum((m3e.post*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
      b[9,6]=sqrt(sum(s3e.post*s3e.post*experiment.n/sum((experiment.n),na.rm=T),na.rm=T))/2
      
      # contrast M and SD
      b[10,5]=sum((m3c.post*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
      b[10,6]=sqrt(sum(s3c.post*s3c.post*contrast.n/sum((contrast.n),na.rm=T),na.rm=T))/2
      
      # observation - contrast M, SD, d, g
      b[11,5]=sum((m3e.post*experiment.n-m3c.post*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
      b[11,6]=sqrt(abs(sum((s3e.post*s3e.post*experiment.n-s3c.post*s3c.post*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
      b[11,7]=mean(sum(dQOLpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
      b[11,8]=mean(sum(gOTHERpost*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
      
      
      # post-pre intervention
      # observation M, SD
      b[9,9]=sum(((m3e.post-m3e.pre)*experiment.n),na.rm=T)/sum(experiment.n,na.rm=T)
      b[9,10]=sqrt(abs(sum((s3e.post*s3e.post-s3e.pre*s3e.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2
      
      # contrast M and SD
      b[10,9]=sum(((m3c.post-m3c.pre)*contrast.n),na.rm=T)/sum(contrast.n,na.rm=T)
      b[10,10]=sqrt(abs(sum((s3c.post*s3c.post-s3c.pre*s3c.pre)*experiment.n/sum((experiment.n),na.rm=T),na.rm=T)))/2
      
      # observation - contrast M, SD, d, g
      b[11,9]=sum((m3e.post*experiment.n-m3c.pre*contrast.n),na.rm=T)/sum(experiment.n+contrast.n,na.rm=T)
      b[11,10]=sqrt(abs(sum((s3e.post*s3e.post*experiment.n-s3c.pre*s3c.pre*contrast.n),na.rm=T)/sum((experiment.n+contrast.n),na.rm=T)))
      b[11,11]=mean(sum((dQOLpost-dQOLpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
      b[11,12]=mean(sum((gOTHERpost-gOTHERpre)*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T))
  
      write.csv(b,file="C:/Users/ThinPak/Desktop/b.csv",row.names = FALSE)
    
      detach(meta1) 
      
  ### Fixed effects models
      # fixed-effect model check
      model1=metacont(experiment.n,m1e.post,s1e.post,contrast.n,m1c.post,s1c.post,data=meta1,sm="SMD",byvar=intervention2)
      model2=metacont(experiment.n,m2e.post,s2e.post,contrast.n,m2c.post,s2c.post,data=meta1,sm="SMD",byvar=intervention2)
      model3=metacont(experiment.n,m3e.post,s3e.post,contrast.n,m3c.post,s3c.post,data=meta1,sm="SMD",byvar=intervention2)
      
      # significant hetergeniety between groups, suggesting random effect model.
      
            # RMA model check
      res1=rma(n1i=experiment.n,n2i=contrast.n,m1i=m1e.post,m2i=m1c.post,sd1i=s1e.post,sd2i=s1c.post,measure="SMD",method="FE",data=meta1)
      res2=rma(n1i=experiment.n,n2i=contrast.n,m1i=m2e.post,m2i=m2c.post,sd1i=s2e.post,sd2i=s2c.post,measure="SMD",method="FE",data=meta1)
      res3=rma(n1i=experiment.n,n2i=contrast.n,m1i=m3e.post,m2i=m3c.post,sd1i=s3e.post,sd2i=s3c.post,measure="SMD",method="FE",data=meta1)
      
      ## random effect model
      attach(meta1)
      ratio1=abs((m1e.post-m1e.pre)/(m1c.post-m1c.pre))
      ratio2=abs((m2e.post-m2e.pre)/(m2c.post-m2c.pre))
      ratio3=abs((m3e.post-m3e.pre)/(m3c.post-m3c.pre))
      detach(meta1)
      meta1=cbind(meta1,ratio1,ratio2,ratio3)
      
      meta1.1=meta1[which(is.na(ratio1)==F),]
      meta1.2=meta1[which(is.na(ratio2)==F),]
      meta1.3=meta1[which(is.na(ratio3)==F),]
      
      MMSE = lme(ratio1 ~ average.age+m1e.pre,random = ~ 1|intervention2, data = meta1.1,na.action=na.omit)  
      ADL = lme(ratio2 ~ average.age+intervention2,random = ~ 1|average.age, data = meta1.2,na.action=na.omit)  
      QOL = lme(ratio3 ~ average.age+intervention2,random = ~ 1|average.age, data = meta1.3,na.action=na.omit)  
      summary(MMSE)
      summary(ADL)
      summary(QOL)
      anova(MMSE)
      anova(ADL)
      anova(QOL)
  

## forest and other plots - download to files
      
    # forest model 1
    attach(meta1)
    
    forest(res1,xlim=c(-12,10),ilab=cbind(experiment.n,m1e.post,s1e.post,contrast.n,m1c.post,s1c.post),
           ilab.x=c(-10,-8.5,-7,-5.5,-4,-2.5),cex=.8)
    
    forest(res2,xlim=c(-22,8),ilab=cbind(experiment.n,m1e.post,s1e.post,contrast.n,m1c.post,s1c.post),
           ilab.x=c(-20,-18.5,-17,-15.5,-14,-12.5),cex=.8)
    
    forest(res3,xlim=c(-12,5),ilab=cbind(experiment.n,m1e.post,s1e.post,contrast.n,m1c.post,s1c.post),
           ilab.x=c(-10,-8.5,-7,-5.5,-4,-2.5),cex=.8)
    
    
    
    ref1=par(cex=.8,font=2) 
    text(c(-8.5,-4),50,c("Experiment Group","Control Group"))
    text(c(-10,-8.5,-7,-5.5,-4,-2.5),48,c("Experiment G n","Experiment G M","Experiment G SD","Control G n","Control G M","Control G SD"))
    text(-14,48,"Included",pos=4)
    text(6.7,48,"SMD [95% CI]",pos=2)
    par(ref1)
    
    
    
    detach(meta1)
      
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

## demographic info
# meta1.1=na.omit(meta1)

attach(meta1)
table(year)
table(location3)
summary(sample.size)
summary(average.age)
table(ADL)
table(MMSE)
table(QOL)
table(intervention1)
table(intervention2)
detach(meta1)


####################
### calculate MMSE, ADL, QOL average pre score including experiemnt and contrast groups

## figure making


# select intervention groups

meta1.1=meta1[which(meta1$intervention2==1),] # comprehensive treatment
meta1.2=meta1[which(meta1$intervention2==2),] # psychological treatment
meta1.3=meta1[which(meta1$intervention2==3),] # nursing treatment

a=matrix(0,25,2)
  # Grand population 
attach(meta1)
  a[1,1]=sum(m1e.pre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[1,2]=sum(m1e.post*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[2,1]=sum(m1c.pre*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  a[2,2]=sum(m1c.post*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  
  a[3,1]=sum(m2e.pre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[3,2]=sum(m2e.post*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[4,1]=sum(m2c.pre*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  a[4,2]=sum(m2c.post*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  
  a[5,1]=sum(m3e.pre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[5,2]=sum(m3e.post*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[6,1]=sum(m3c.pre*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  a[6,2]=sum(m3c.post*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  
  detach(meta1)
  

  
  # Intervention 1  
  attach(meta1.1)
  
  a[7,1]=sum(m1e.pre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[7,2]=sum(m1e.post*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[8,1]=sum(m1c.pre*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  a[8,2]=sum(m1c.post*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  
  a[9,1]=sum(m2e.pre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[9,2]=sum(m2e.post*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[10,1]=sum(m2c.pre*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  a[10,2]=sum(m2c.post*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  
  a[11,1]=sum(m3e.pre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[11,2]=sum(m3e.post*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[12,1]=sum(m3c.pre*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  a[12,2]=sum(m3c.post*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  
  detach(meta1.1)


  
  # Intervention 2  
  attach(meta1.2)
  
  a[13,1]=sum(m1e.pre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[13,2]=sum(m1e.post*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[14,1]=sum(m1c.pre*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  a[14,2]=sum(m1c.post*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  
  a[15,1]=sum(m2e.pre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[15,2]=sum(m2e.post*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[16,1]=sum(m2c.pre*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  a[16,2]=sum(m2c.post*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  
  a[17,1]=sum(m3e.pre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[17,2]=sum(m3e.post*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[18,1]=sum(m3c.pre*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  a[18,2]=sum(m3c.post*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  
  detach(meta1.2)


  # Intervention 3  
  attach(meta1.3)
  
  a[19,1]=sum(m1e.pre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[19,2]=sum(m1e.post*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[20,1]=sum(m1c.pre*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  a[20,2]=sum(m1c.post*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  
  a[21,1]=sum(m2e.pre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[21,2]=sum(m2e.post*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[22,1]=sum(m2c.pre*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  a[22,2]=sum(m2c.post*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  
  a[23,1]=sum(m3e.pre*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[23,2]=sum(m3e.post*experiment.n,na.rm=T)/sum(experiment.n,na.rm=T)
  a[24,1]=sum(m3c.pre*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  a[24,2]=sum(m3c.post*contrast.n,na.rm=T)/sum(contrast.n,na.rm=T)
  
  detach(meta1.3)
  
write.csv(a,file="a.csv",row.names = FALSE)

# MMSE

mean(m1e.post,na.rm=T)
mean(m1c.pre,na.rm=T)
mean(m1c.post,na.rm=T)

# ADL
mean(m2e.pre,na.rm=T)
mean(m2e.post,na.rm=T)
mean(m2c.pre,na.rm=T)
mean(m2c.post,na.rm=T)

# QOL
mean(m3e.pre,na.rm=T)
mean(m3e.post,na.rm=T)
mean(m3c.pre,na.rm=T)
mean(m3c.post,na.rm=T)



attach(meta1)
MMSEpre.avg=(experiment.n*m1e.pre+contrast.n*m1c.pre)/sample.size*MMSE  # ???
ADLpre.avg=(experiment.n*m2e.pre+contrast.n*m2c.pre)/sample.size*ADL
QOLpre.avg=(experiment.n*m3e.pre+contrast.n*m3c.pre)/sample.size*QOL

MMSEpost.avg=(experiment.n*m1e.post+contrast.n*m1c.post)/sample.size*MMSE
ADLpost.avg=(experiment.n*m2e.post+contrast.n*m2c.post)/sample.size*ADL
QOLpost.avg=(experiment.n*m3e.post+contrast.n*m3c.post)/sample.size*QOL

detach(meta1)

summary(MMSEpre.avg,na.rm=T)
summary(ADLpre.avg,na.rm=T)
summary(QOLpre.avg,na.rm=T)
summary(MMSEpost.avg,na.rm=T)
summary(ADLpost.avg,na.rm=T)
summary(QOLpost.avg,na.rm=T)


##***********************************

# MMSE(1) post group
forest(res2,xlim=c(-12,6),ilab=cbind(n2.1,m2.1,s2.1,n2.2,m2.2,s2.2),ilab.x=c(-10,-8.5,-7,-5.5,-4,-2.5),cex=.8)

ref1=par(cex=.8,font=2) 
  text(c(-8.5,-4),44,c("Experiment Group","Control Group"))
  text(c(-10,-8.5,-7,-5.5,-4,-2.5),42,c("Exp G n","Exp G M","Exp G SD","Con G n","Con G M","Con G SD"))
  text(-14,42,"Included",pos=4)
  text(6,42,"SMD[95% CI]",pos=2)
  
# MMSE(1) pre group    
forest(res3,xlim=c(-12,6),ilab=cbind(n3.1,m3.1,s3.1,n3.2,m3.2,s3.2),ilab.x=c(-10,-8.5,-7,-5.5,-4,-2.5),cex=.8)
  
ref2=par(cex=.8,font=2) 
  text(c(-8.5,-4),44,c("ʵ����","������"))
  text(c(-10,-8.5,-7,-5.5,-4,-2.5),42,c("ʵ����n","ʵ����M","ʵ����SD","������n","������M","������SD"))
  text(-14,42,"Included",pos=4)
  text(6,42,"SMD[95% CI]",pos=2)
  

#### filter for metacont(): SMD,heterogeneity;forest();funnel()
meta2.1=metacont(n2.1,m2.1,s2.1,n2.2,m2.2,s2.2,sm="MD")
meta3.1=metacont(n3.1,m3.1,s3.1,n3.2,m3.2,s3.2,sm="MD")

forest(meta3.1,comb.random=F)
funnel(meta3.1)
metabias(meta3.1,method.bias="linreg",plotit=T,k.min=5)


### data output
write.csv(meta1, file="C:/Users/chens/OneDrive/research/1personal/elder/meta/test.csv")