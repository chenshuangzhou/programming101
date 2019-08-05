setwd("C:/Users/chens/OneDrive/research/Projects/1 Meaning Making - VL/Systematic review on MM and Caregiver - VL/qualitative synthesis")

# a = readLines("G-baby boomer.txt")
# 
# words = unlist(strsplit(a, "\\W"))
# words = words[words != ""]
# b = as.matrix(tail(sort(table(tolower(words))),decreasing=F, 100))
# 
# temp <- list.files(pattern="*.txt")
# x=NULL
# for (i in 1:length(temp)) {
#   temp2 = readLines(temp[i])
#   x <- rbind(x,temp2)
# }

#################################
### text solutions ###
list <- list.files(pattern="*.txt")
n=length(list)
x=matrix(NA,100,2*n)
stopwords = c("","of","and","to","the","a","day","in","for","on","or","is","at","be","but","as")

### loop process ###
for (i in 1:length(list)) {
  temp = readLines(list[i])
  words = unlist(strsplit(temp, "\\W"))
    for (j in 1:length(stopwords)) {
      words = gsub(paste0('\\<',stopwords[[j]],'\\>'),"", words) 
    }
  words = words[words != ""]
  b = as.matrix(head(sort(table(tolower(words)),decreasing=T),100),colname=T)
  x[,2*i-1] = rownames(b)
  x[,2*i] = b[,1]
}

# all.files <- list.files(path = "C:/Users/chens/OneDrive/research/Projects/1 Meaning Making - VL/Systematic review on MM and Caregiver - VL/qualitative synthesis/data",full.names = T,pattern = ".txt")
# mylist <- lapply(all.files,function(i) read.table(i))
# mydata <- do.call('rbind',mylist)


write.csv(x,file="C:/Users/chens/OneDrive/research/Projects/1 Meaning Making - VL/Systematic review on MM and Caregiver - VL/qualitative synthesis/data/output.csv",row.names=FALSE)

##### Narratives ###
setwd("C:/Users/chens/OneDrive/research/Projects/1 Meaning Making - VL/Systematic review on MM and Caregiver - VL/qualitative synthesis")
m=temp=read.csv("narratives 19073.csv",header=T,na.strings = "NA")

  # colnames(m)

###### Categories ###########
# Gender
male = m[m$CGGender=="Male",]
female = m[m$CGGender=="Female",]

# Generations
bb = m[m$birth=="Baby Boomer",]
silent = m[m$birth=="Silent" | m$birth=="Silent 1917",]
xyz = m[m$birth=="X" | m$birth=="Y" | m$birth=="Z",]
x = m[m$birth=="X",]

# Relationships X gender X generation
husband = m[m$CGRole=="Husband",]   # husband-wife dyads - data 1
  hwb = husband[husband$birth=="Baby Boomer",] # data 1.1
  hwx = husband[husband$birth=="X",]           # data 1.2
wife = m[m$CGRole=="Wife",]         # wife-husband dyads - data 2
  whb = wife[wife$birth=="Baby Boomer",] # data 2.1
  whx = wife[wife$birth=="X",]           # data 2.2
daughter = m[m$CGRole=="Daughter" | m$CGRole=="Daughter-in-law",]
  df = daughter[daughter$CRRole=="Father",]    # daughter-father dyads - data 3
    dfb = df[df$birth=="Baby Boomer",] # data 3.1
    dfx = df[df$birth=="X",]           # data 3.2
  dm = daughter[daughter$CRRole=="Mother",]    # daughter-mother dyads - data 4
    dmb = dm[dm$birth=="Baby Boomer",] # data 4.1
    dmx = dm[dm$birth=="X",]           # data 4.2
son = m[m$CGRole=="Son",]
  sf = son[son$CRRole=="Father",]    # son-father dyads - data 5
    sfb = sf[sf$birth=="Baby Boomer",] # data 5.1
    sfx = sf[sf$birth=="X",]           # data 5.2
  sm = son[son$CRRole=="Mother",]    # son-mother dyads - data 6
    smb = sm[sm$birth=="Baby Boomer",] # data 5.1
    smx = sm[sm$birth=="X",]           # data 5.2

others = m[m$CGRole=="Brother" | m$CGRole=="housemate" | m$CGRole=="Nephew" | m$CGRole=="Niece" | m$CGRole=="Sister" | m$CGRole=="Sister-in-law",]


###### Final Function ######

wf = function(data) {
  # word frequency on narratives
    stopwords = c("","i","I","it","It","of","and","And","to","To","the","The","a","A","day","in","In","for","For","on","On","or","Or","is","at","At","be","Be","but","But","as","As","s")
    nr = as.matrix(data$Narratives)
    temp1 = (paste(nr, collapse =" "))[[1]]
    words1 = unlist(strsplit(temp1,"\\W"))
      for (i in 1:length(stopwords)) {words1 = gsub(paste0('\\<',stopwords[[i]],'\\>'),"", words1)}
    words1 = words1[words1 != ""]
    nf1 = as.matrix(head(sort(table(toupper(words1)),decreasing=T),100),colname=T)
  # word frequency on codes
    vars = c("Value","PurposeRR","PurposeRG","RelationPurposeCR","PurposeGR","PurposeGG","RelationPurposeCG","OutcomeR","OutcomeG","PriorR","PriorG","Support","Adaptation","WellbeingG")
    x = matrix(,100,30)
    j = 1
    for (k in vars) {
      theme = as.matrix(data[,k])
      temp2 = (paste(theme, collapse =" "))[[1]]
      words2 = unlist(strsplit(temp2,"\\W"))
      words2 = words2[words2 != ""]
      if(length(words2)!=0){c=as.matrix(sort(table(tolower(words2)),decreasing=T),colname=T)} else {c=matrix(0,2,1,dimnames=list(c("0","0"),"c"))}
      x[1:dim(c)[1],2*j-1] = rownames(c)
      x[1:dim(c)[1],2*j] = c[,1]
    j = j + 1
    }
  # combine output table
    x[,29] = rownames(nf1)
    x[,30] = nf1[,1]
    colnames(x) = c(rep(vars,each=2),"KW","Counts")
  # Output
    # View(x)
    name = readline("file name?")
    write.csv(x,file=paste0(name,".csv"),row.names=FALSE)
}


##### Correlation analysis ############ package: stringr


library(stringr)
subdata = ("hwb","hwx","whb","whx","dfb","dfx","dmb","dmx","sfb","sfx","smb","smx")

for (n in subdata) {
  temp3 = (paste(n[,"Value"], collapse =" "))[[1]]
  words3 = unlist(strsplit(temp3,"\\W[^(-1)(-2)]"))
  words3 = unlist(words3[words3 != ""])

}

d = data.frame()
for (i in 1:dim(data)[1]) {
  
  
  d = append(d,unlist(str_extract_all(words3,"[CFP]P")))
}

unlist(strsplit(temp1,"\\W"))


words3 <- c("apples x4", "bag of flour", "bag of sugar", "milk x2","MC2","MC-2","mc","RC1","gc2","cat","Hat","Pat","PP")
str_extract_all(shopping_list, "[a-z]+")
str_extract_all(shopping_list, "\\b[a-z]+\\b")
str_extract_all(shopping_list, "\\d")

a = unlist(str_extract_all(shopping_list,pattern = "[M][A-Za-z]+(-)*[0-9]?"))


#########################################################################################################################

###### Word Frequency ###########
stopwords = c("","i","I","it","It","of","and","And","to","To","the","The","a","A","day","in","In","for","For","on","On","or","Or","is","at","At","be","Be","but","But","as","As","s")

### Narrative Word Frequency - nf
nf = function(data){
  n = as.matrix(data$Narratives)
  temp = (paste(n, collapse =" "))[[1]]
  words = unlist(strsplit(temp,"\\W"))
    for (i in 1:length(stopwords)) {
      words = gsub(paste0('\\<',stopwords[[i]],'\\>'),"", words)
      }
  words = words[words != ""]
  nf1 = as.matrix(head(sort(table(tolower(words)),decreasing=T),100),colname=T)
  # View(nf1)
  # name = readline("file name?")
  # write.csv(nf1,file=paste0(name,".csv"),row.names=FALSE)
}

### Codes Word Frequency - cf

# vars = c("Value,PurposeRR, PurposeRG,RelationPurposeCR,PurposeGR, PurposeGG,RelationPurposeCG OutcomeR,OutcomeG,PriorR.,PriorG.,Support,Adaptation,WellbeingG")

cf = function(data){
  vars = c("Value","PurposeRR","PurposeRG","RelationPurposeCR","PurposeGR","PurposeGG","RelationPurposeCG","OutcomeR","OutcomeG","PriorR","PriorG","Support","Adaptation","WellbeingG")
  x = matrix(,30,28)
  j = 1
  for (k in vars) {
    n = as.matrix(data[,k])
    temp = (paste(n, collapse =" "))[[1]]
    words = unlist(strsplit(temp,"\\W"))
    words = words[words != ""]
    if(length(words)!=0){c=as.matrix(sort(table(tolower(words)),decreasing=T),colname=T)} else {c=matrix(0,2,1,dimnames=list(c("0","0"),"c"))}
    x[1:dim(c)[1],2*j-1] = rownames(c)
    x[1:dim(c)[1],2*j] = c[,1]
  j = j + 1
  }
  # View(x)
  # name = readline("file name?")
  # write.csv(x,file=paste0(name,".csv"),row.names=FALSE)
}



