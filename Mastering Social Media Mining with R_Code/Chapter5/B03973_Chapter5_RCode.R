### Author : Sharan Kumar Ravindran
require(devtools)
install_github("cscheid/rgithub")
library(github)
library(stringr)

# Authentication

#Paste your ID between the quotes
client.id <- "XXXXXXXXXXXXXXXXXX"

#Paste your secret key between the quotes
client.secret <- "XXXXXXXXXXXXXXXXXXXXXX"

ctx = interactive.login(client.id, client.secret)


###############Using the package rgithub #############

# Getting our data
get.myself(ctx)
head(me)
me$content$public_repos

get.my.repositories(ctx)
get.public.events(ctx)
get.my.following(ctx)
get.users(ctx)
get.all.repositories(ctx = get.github.context())
?get.all.repositories

##########Collecting the data
library(jsonlite)
library(stringr)
# Set your working directory accordingly
setwd("C:/Users/Accelyst/Desktop/SMM/Chapter 5/Data")
getwd()
topuser <- read.csv("TopUsers.csv")
uname <- as.character(topuser$Username)
compdata <- ""
i=12
# 60 request per hour
for (i in 120:nrow(topuser))
{
  # Paste your client ID and secret ket in the below code
  data2 <- fromJSON(paste0("https://api.github.com/users/", str_trim(uname[i], side = "both"), "/repos?client_id=XXXXXXXXXXXXXXXXXXXXXXXx&client_secret=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
  data2 <- data2[,-(4)]
  #data2 <- unclass(compdata)
  compdata <- rbind(compdata, data2)
  print(i)
}
head(compdata)
nrow(compdata)

write.csv(compdata, file ="samp1.csv")

########Formating the Data

# Read the data
activeusers <- read.csv("ActiveUsers.csv")

# Change date to format supported by R
format.git.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%SZ", tz = "GMT")
}
?as.POSIXct
# Updating the column with new date format
activeusers$created_at <- format.git.date(activeusers$created_at)
activeusers$updated_at <- format.git.date(activeusers$updated_at)
activeusers$pushed_at <- format.git.date(activeusers$pushed_at)

head(activeusers)

# Subsetting required data
# To check which columns to select
colnames(activeusers)
# Selecting the required data
ausersubset <- activeusers[,c("id","name","full_name","private","description","fork","created_at","updated_at","pushed_at","homepage","size","stargazers_count","watchers_count","language","has_issues","has_downloads","has_wiki","has_pages","forks_count","open_issues_count","forks","open_issues","watchers")]


head(ausersubset)
# Replace True and False with 1 and 0
ausersubset$private <- as.integer(ausersubset$private)
ausersubset$fork <- as.integer(ausersubset$fork)
ausersubset$has_issues <- as.integer(ausersubset$has_issues)
ausersubset$has_downloads <- as.integer(ausersubset$has_downloads)
ausersubset$has_wiki <- as.integer(ausersubset$has_wiki)
ausersubset$has_pages <- as.integer(ausersubset$has_pages)

# Getting the user name
ausersubset$full_name <- sapply(strsplit(as.character(ausersubset$full_name), split='/', fixed=TRUE), function(x) (x[1]))
head(ausersubset$full_name)

# Flag for presence of website/webpage
ausersubset$has_web <- as.numeric(grepl(".", ausersubset$homepage))
head(ausersubset)

# Length of the description
ausersubset$desclen <- nchar(as.character(ausersubset$description))

# Day difference from current date for created, updated and pushed
ausersubset$dayscreated <- as.integer(difftime(Sys.Date(),ausersubset$created_at , units = c("days")))
ausersubset$daysupdated <- as.integer(difftime(Sys.Date(),ausersubset$updated_at , units = c("days")))
ausersubset$dayspushed <- as.integer(difftime(Sys.Date(),ausersubset$pushed_at , units = c("days")))


####################
##############EDA


sapply(ausersubset, class)

summary(ausersubset)

apply(ausersubset, 2, sd)
########################################
########## Graphical Analysis

library(ggplot2)
## Histogram
lang <- table(ausersubset$language)
lang <- data.frame(lang)
order(lang$Freq)
lang[order(-lang$Freq),]

q <- qplot(ausersubset$language,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram for Language", 
      xlab = "Language",  
      fill=I("blue"), 
      col=I("red")) 

q + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(file="language.png", dpi=500)

# Bar chart for the top languages
a <- table(ausersubset$language)
a <- as.data.frame(a)
a <- a[with(a, order(-Freq)), ]
toplang <- a[2:21,]
colnames(toplang) <- c("Language","Count")

q <- qplot(x=Language, y=Count, 
                       data=toplang, geom="bar", stat="identity",
                       position="dodge") 

q + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(file="top-language.png", dpi=500)

###### Box plot

forbplot <- ausersubset[c("watchers_count", "forks_count", "open_issues_count", "stargazers_count")]
boxplot(forbplot, outline = FALSE)

dev.copy(png,filename="boxplot.png", width=600, height=600);
dev.off ();

# Box plot -2
library(reshape2)
colnames(ausersubset)
dat.m <- melt(ausersubset, id.vars='open_issues_count', measure.vars=c('watchers_count'))
library(ggplot2)
p <- ggplot(dat.m) +
  geom_boxplot(aes(x=open_issues_count, y=value, color=variable), outlier.shape = NA, notch = TRUE, notchwidth= 0.5) + 
  scale_y_continuous(limits = quantile(ausersubset$stargazers_count, c(0, 0.6)))

ggsave(file="boxplot-bi.png", dpi=500)

# Pie chart: Issues in the repositary

pie <- ggplot(ausersubset, aes(x = factor(1), fill = factor(ausersubset$has_issues))) + geom_bar(width = 1) 
pie + coord_polar(theta = "y")  
ggsave(file="pie-chart.png", dpi=500)




### Trend Analysis
install.packages("data.table")
library(data.table)
trenddata <- ausersubset[c("updated_at", "id")]
trenddata$updated_at <- as.POSIXct(strptime(trenddata$updated_at, "%Y-%m-%d"))

tdata <- table(trenddata$updated_at)
tdata <- as.data.frame(tdata)
head(tdata, 50)
colnames(tdata) <- c("Date","Repositories")
tdata$Date <- as.Date(tdata$Date)
tdata1 <- tail(tdata, 75)

q <- ggplot(data=tdata1, aes(x=Date, y=Repositories, group=1)) +
     geom_line() + 
     geom_point()

q + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(file="line-chart.png", dpi=500)


### Heatmap

colnames(ausersubset)
newdata <- ausersubset[c("id","full_name","size","watchers_count", "forks_count", "open_issues_count", "desclen", "dayscreated", "daysupdated", "dayspushed")]
sd <- sqldf("select full_name, count(id), avg(size),  sum(watchers_count), sum(forks_count), sum(open_issues_count), avg(desclen), avg(dayscreated), avg(daysupdated), avg(dayspushed) from newdata group by full_name")
colnames(sd) <- c("Name", "Repositories", "AverageSize", "Watchers", "Forks", "Issues", "Avg_desc_length", "Avg_days_since_created", "Avg_days_since_updated", "Avg_days_since_pushed")

row.names(sd) <- sd$Name
sd <- sd[order(-sd$Repositories),] 
sd <- sd[1:40,]
names(sd)
head(sd)
sd <- sd[,2:10]
sdmat <- as.matrix(sd)
sd_heatmap <- heatmap(sdmat, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,8))
dev.copy(png,filename="heatmap-users.png", width=600, height=875);
dev.off ();

####################################################
############### Correlation Analysis

colnames(ausersubset)
cordata <- ausersubset[c("id","full_name","size","watchers_count", "forks_count", "open_issues_count", "desclen", "dayscreated", "daysupdated", "dayspushed")]
cdata <- sqldf("select full_name, count(id), avg(size),  sum(watchers_count), sum(forks_count), sum(open_issues_count), avg(desclen), avg(dayscreated), avg(daysupdated), avg(dayspushed) from cordata group by full_name")
colnames(cdata) <- c("Name", "Repositories", "AverageSize", "Watchers", "Forks", "Issues", "Avg_desc_length", "Avg_days_since_created", "Avg_days_since_updated", "Avg_days_since_pushed")

head(cdata)

# 1 correlation
cor(cdata$Forks, cdata$Watchers)
ggplot(cdata, aes(x=Forks, y=Watchers)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm)
ggsave(file="scatter-plot.png", dpi=500)

# 2 correlation
cor(cdata$Avg_days_since_created, cdata$Avg_days_since_pushed)
ggplot(cdata, aes(x=Avg_days_since_created, y=Avg_days_since_pushed)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm)
ggsave(file="C:/Users/Sharan/Desktop/SMM/Chapter 5/Pics/scatter-plot2.png", dpi=500)

# 3 correlation
ggplot(cdata, aes(x=Avg_days_since_created, y=Avg_days_since_pushed)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth() 
ggsave(file="scatter-plot3.png", dpi=500)

# 4 correlation

cordata <- ausersubset[c("id","full_name","size","watchers_count", "forks_count", "open_issues_count", "desclen", "dayscreated", "daysupdated", "dayspushed", "has_issues")]
cdata <- sqldf("select full_name, count(id), avg(size),  sum(watchers_count), sum(forks_count), sum(open_issues_count), avg(desclen), avg(dayscreated), avg(daysupdated), avg(dayspushed), sum(has_issues) from cordata group by full_name")
colnames(cdata) <- c("Name", "Repositories", "AverageSize", "Watchers", "Forks", "Issues", "Avg_desc_length", "Avg_days_since_created", "Avg_days_since_updated", "Avg_days_since_pushed", "IssuesF")
head(cdata)

cdata$IssuesF <- as.factor(cdata$IssuesF)
cdata$IssuesF[cdata$Issues < 10]  = 0
cdata$IssuesF[cdata$Issues >= 10]  = 1
cdata$IssuesF <- as.factor(cdata$IssuesF)

ggplot(cdata, aes(x=Avg_days_since_created, y=Avg_days_since_pushed, color=IssuesF)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE)
ggsave(file="scatter-plot4.png", dpi=500)


## Language Correlation
head(ausersubset)
ldata <- ausersubset[c("full_name","language")]
head(ldata)

pivoting <- data.table(ldata)
pivotdata<-dcast.data.table(pivoting, full_name ~ language, fun.aggregate=length, value.var="language")

ncol(pivotdata)
head(pivotdata)
pivotdata <- as.data.frame(pivotdata)
pivotdata <- pivotdata[,2:70]
cormatrix <- cor(pivotdata)
diag(cormatrix) <- NA 
cormatrix[upper.tri (cormatrix)] <- NA
finalcor <- melt(cormatrix)
head(finalcor)
filteredcordata <- finalcor[ which(finalcor$value > 0.4),]
filteredcordata

#############
## Moving Correlation


mdata <- ausersubset[c("created_at", "watchers_count", "forks_count")]

mdata$created_at <- as.POSIXct(strptime(mdata$created_at, "%Y-%m-%d"))
mdata$watchers_count <- as.numeric(mdata$watchers_count)
mdata$forks_count <- as.numeric(mdata$forks_count)
#mdata$created_at <- as.factor(as.character(mdata$created_at))

install.packages("zoo")
library(data.table)
library(zoo)
DT <- data.table(mdata)
m1 <- DT[, sum(forks_count ), by = created_at]
m2 <- DT[, sum(watchers_count ), by = created_at]

m1 <- as.data.frame(m1)
m2 <- as.data.frame(m2)

m <- merge(m1, m2, by = "created_at") 
colnames(m) <- c("Date", "forks", "watchers")
mdata <- m
mdata1 <- tail(mdata, 300)
rownames(mdata1) <- mdata1$Date
mdata1 <- mdata1[,-1]
head(mdata1, 30)
r1 <- rollapplyr(mdata1, 30, function(x) cor(x[,1],x[,2]), by.column=FALSE)
r1 <- as.data.frame(r1)

r0 <- tail(mdata$Date, 271)
r0 <- as.data.frame(r0)
resultcor <- cbind(r0,r1)
colnames(resultcor) <- c("Date","Corr")
resultcor$Date <- as.Date(resultcor$Date)

resultcor <- tail(resultcor, 75)

q <- ggplot(data=resultcor, aes(x=Date, y=Corr, group=1)) +
  geom_line() + 
  geom_point()

q + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(file="roll-corr.png", dpi=500)

########################### END ###################################
