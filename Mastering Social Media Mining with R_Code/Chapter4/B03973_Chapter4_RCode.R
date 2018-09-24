############################## Code for Chapter 4 #############
####################Author : Sharan Kumar Ravindran###########
install.packages("Rtools")

install.packages("D:/R Packages/httr_0.6.0.tar.gz", repos = NULL)
#httr version 0.6
library(devtools)
library(Rtools)
library(curl)
library(httr)

install_github("pablobarbera/instaR/instaR")
library(instaR)

app_id <- "paste your ID here"
app_secret <- "paste your key here"
token <- instaOAuth(app_id, app_secret)

setwd("C:\\Users\\Accelyst\\Desktop\\SMM\\Chapter 4\\Data")
getwd()
# Functions

# getting pictures with #MachuPicchu hashtag
MachuPicchu <- searchInstagram("MachuPicchu", token, n=10, folder="MachuPicchu")
head(MachuPicchu,2)
names(MachuPicchu)
head(MachuPicchu$comments_count)

# getting pictures with #MachuPicchu hashtag from that location
MachuPicchu <- searchInstagram("MachuPicchu", token, n=10, lat= 13.1633, lng= 72.5456, folder="MachuPicchu")


# downloading my 100 most recent pictures
instag <- getUserMedia("instagram", token, n=100, folder="instagram")

names(instag)
head(instag)

# User profile
usr <- getUser("barackobama", token)
head(usr)
names(usr)

# get list of followers of a user
instaf <- getFollowers("instagram", token)

head(instaf,2)
names(instaf)
nrow(instaf)

# get list of users a given user follows
instaff <- getFollows("instagram", token)
head(instaff,3)
nrow(instaff)

# Getting the comments
comm <- getComments("1026058420312409485_25025320", token)
comm$text
tail(comm)
names(comm)
?getComments
class(comm)
# recent 150 comments

#Getting the tag Count
tag1 <- getTagCount("greece", token)
tag1
tag2 <- getTagCount("obama", token)
tag2


#############Preparing data


userAndTags <- read.csv("UsersAndHashtags.csv")
names(userAndTags)
head(userAndTags)

users <- userAndTags$Users
users <- as.matrix(users)

usermedia = data.frame(matrix("", ncol = 16, nrow = 0))
names(um)
for (i in 7:nrow(users))
{
  um <- getUserMedia(users[i,1], token, n=20, folder="users")
  usermedia <- rbind(usermedia, um)
  print(i)
}
nrow(usermedia)
write.csv(usermedia,"usermedia.csv")
usermedia <- read.csv("usermedia.csv")
head(usermedia)


tags <- userAndTags$Hashtags
tags <- as.matrix(tags)
hashmedia = data.frame(matrix("", ncol = 16, nrow = 0))
for (i in 24:nrow(tags))
{
  hm <- searchInstagram(tags[i,1], token, n=100, lat = NULL, lng = NULL, folder="places")
  hashmedia <- rbind(hashmedia, hm)
  print(i)
}
nrow(hashmedia)
write.csv(hashmedia,"hashmedia.csv")
hashmedia <- read.csv("hashmedia.csv")
head(hashmedia)



alldata <- rbind(usermedia, hashmedia)
write.csv(alldata,"alldata.csv")
alldata <- read.csv("alldata.csv")
head(alldata)

### Follows data
users <- userAndTags$Users
users <- as.matrix(users)
?getFollows
userfollows = data.frame(matrix("", ncol = 7, nrow = 0))
for (i in 42:nrow(users))
{
  uf <- getFollows(users[i,1], token)
  auf <- cbind(users[i,1], uf)
  userfollows <- rbind(userfollows, auf)
  print(i)

}
nrow(userfollows)
write.csv(userfollows, "userfollows.csv")

userfollows <- read.csv("userfollows.csv")
head(userfollows)

## Consolidated user profile

users <- userAndTags$Users
users <- as.matrix(users)
userprofiles = data.frame(matrix("", ncol = 8, nrow = 0))
for (i in 7:nrow(users))
{
  #uf <- getFollows(users[i,1], token)
  usrp <- getUser(users[i,1], token)
  userprofiles <- rbind(userprofiles, usrp)
  print(i)
  
}
write.csv(userprofiles, "userprofiles.csv")
userprofiles <- read.csv("userprofiles.csv")
head(userprofiles, 5)
names(userprofiles)

# who got most followers?
mostfollowed <- userprofiles[with(userprofiles, order(-followed_by)), ]
head(mostfollowed$full_name, 15)

# who follows most?
mostfollows <- userprofiles[with(userprofiles, order(-follows)), ]
head(mostfollows$full_name, 15)

# who got most media?
mostmedias <- userprofiles[with(userprofiles, order(-media_count)), ]
head(mostmedias$full_name, 15)

# Overal top users
userprofiles$overallmetric <- ((userprofiles$media_count/max(userprofiles$media_count)) + (userprofiles$followed_by/max(userprofiles$followed_by)) +(userprofiles$followed_by/max(userprofiles$followed_by)))*100
overallmet <- userprofiles[with(userprofiles, order(-overallmetric)), ]
head(overallmet$full_name, 15)
head(overallmet)

# most commented
head(alldata)
mostcomm <- alldata[with(alldata, order(-comments_count)), ]
head(mostcomm, 1)

# most liked
head(alldata)
mostlikes <- alldata[with(alldata, order(-likes_count)), ]
head(mostlikes, 1)

# All location
library(sqldf)
names(alldata)
allloc <- sqldf("select distinct location_name from alldata")
allloc <- na.omit(allloc)
head(allloc, 20)
nrow(allloc)

#location with most likes
loclikes <- sqldf("select location_name, sum(likes_count) as totlikes from alldata group by location_name")
loc <- loclikes[with(loclikes, order(-totlikes)), ]
loc <- na.omit(loc)
head(loc, 25)


#location most talked about
loccomments <- sqldf("select location_name, sum(comments_count) as totcomm from alldata group by location_name")
loccomm <- loccomments[with(loccomments, order(-totcomm)), ]
loccomm <- na.omit(loccomm)
head(loccomm, 15)

# Most liked - for user
names(alldata)
userlikes <- sqldf("select username, sum(likes_count) as totlikes from alldata group by username")
user <- userlikes[with(userlikes, order(-totlikes)), ]
user <- na.omit(user)
head(user, 25)

# word cloud
#Formating the words
library(wordcloud)
library(tm)

words <- strsplit(as.character(alldata$caption), " ")
words <- lapply(words, function(x) x[grep("^[A-Za-z0-9]+$", x)])
words <- unlist(words)
words <- tolower(words)
words <- words[-grep("^[rm]t$", words)]

#Remove stop words
stopWords <- stopwords("en")
"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0
words <- words[words %!in% stopWords]

allwords <- as.data.frame(table(words))
wc <- wordcloud(allwords$words, allwords$Freq, random.order = FALSE, min.freq=5, colors = brewer.pal(2, "Dark2"))

dev.copy(png,filename="wordcloud.png", width=600, height=875);
dev.off ();

#Most occuring location 
locations <- sqldf("select location_name, count(location_id) as locid from alldata group by location_name")
location <- locations[with(locations, order(-locid)), ]
location <- na.omit(location)
head(location,5)


# Clustering
library(fpc)
head(alldata)
data <- alldata
cdata <- data.frame(data$type ,data$comments_count, data$likes_count, data$filter)
colnames(cdata) <- c("type","comments","likes","filter")
cdata$filter <- as.integer(cdata$filter)
cdata$type <- as.integer(cdata$type)
cdata$lencap <- nchar(as.character(data$caption))

# Ideal number of clusters
head(cdata)
clusters<- pamk(cdata)
n<-clusters$nc

# Determine number of clusters
#Code from Tal Galili's post based on Kabacoff's book - http://www.r-statistics.com/2013/08/k-means-clustering-from-r-in-action/ 
wss <- (nrow(cdata)-1)*sum(apply(cdata,2,var))
for (i in 2:25) wss[i] <- sum(kmeans(cdata, 
                                     centers=i)$withinss)
plot(1:25, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

dev.copy(png,filename="elbowMethod.png", width=600, height=875);
dev.off ();
## k means
# K-Means Cluster Analysis
fit <- kmeans(cdata, 4)
#Number of elements in each clusters
table(fit$cluster)
# get cluster means 
aggregate(cdata,by=list(fit$cluster),FUN=mean)
# plotting cluster
library(fpc)
plotcluster(cdata, fit$cluster)
dev.copy(png,filename="scatterPlot.png", width=600, height=875);
dev.off ();


# Recommendations
library(data.table)


userfollows <- read.csv("userfollows.csv")
head(userfollows)
names(userfollows)

fdata <- data.frame(userfollows$users.i..1., userfollows$username)
colnames(fdata) <- c("user","follows")
head(fdata)

# Pivoting the data
pivoting <- data.table(fdata)
pivotdata<-dcast.data.table(pivoting, follows ~ user, fun.aggregate=length, value.var="user")


write.csv(pivotdata, "pivot-follows.csv")

# After deletion of the index column and the null user

# Read the pivoted data
data<-read.csv("pivot-follows.csv")
head(data)
colnames(data)

# Drop the column named "user"
data.ubs <- (data[,!(names(data) %in% c("users"))])

# Method: 1
# Function to calculate the cosine between two vectors
getCosine <- function(x,y) 
{
  dat <- cbind(x,y)
  #f <- as.matrix(dat)
  f <- as.data.frame(dat)
  # Remove the rows with zeros
  datn<- f[-which(rowSums(f==0)>0),]
  #colnames(datn)<-c("x","y")
  #dat <- as.data.frame(datn)
  if(nrow(datn) > 2)
  {
    this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  }
  else
  {
    this.cosine <- 0
  }
  return(this.cosine)
}



# Create a placeholder dataframe listing item vs. item
data.ubs.similarity  <- matrix(NA, nrow=ncol(data.ubs),ncol=ncol(data.ubs),dimnames=list(colnames(data.ubs),colnames(data.ubs)))

############ Perform any 1 #####################################

# Method: 1
# Lets fill in those empty spaces with cosine similarities
# Loop through the columns
for(i in 1:ncol(data.ubs)) {
  # Loop through the columns for each column
  for(j in 1:ncol(data.ubs)) {
    # Fill in placeholder with cosine similarities
    data.ubs.similarity[i,j] <- getCosine(as.matrix(data.ubs[i]),as.matrix(data.ubs[j]))
  }
  print(i)
}


# Back to dataframe - Similarity matrix
data.ubs.similarity <- as.data.frame(data.ubs.similarity)
# Below step required for method 3: Co-occurrence- Replace NA with 0
data.ubs.similarity[is.na(data.ubs.similarity)] <- 0
head(data.ubs.similarity)


# Get the top 10 neighbours for each
data.neighbours <- matrix(NA, nrow=ncol(data.ubs.similarity),ncol=11,dimnames=list(colnames(data.ubs.similarity)))
for(i in 1:ncol(data.ubs)) 
{
  # Setting threshold for avoiding zeros
  n <- length(data.ubs.similarity[,i])
  thres <- sort(data.ubs.similarity[,i],partial=n-10)[n-10]
  if(thres > 0.020)
  {
    # Choosing the top 10 recommendation
    data.neighbours[i,] <- (t(head(n=11,rownames(data.ubs.similarity[order(data.ubs.similarity[,i],decreasing=TRUE),][i]))))
  }
  else
  {
    data.neighbours[i,] <- ""
  }
}
head(data.neighbours)


# Writing the recommendation to a file
write.csv(data.neighbours, "SimilarUsersresults.csv")



#####################END###################