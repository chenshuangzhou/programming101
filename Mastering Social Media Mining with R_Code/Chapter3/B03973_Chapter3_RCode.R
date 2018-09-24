# Rfacebook package required
install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
# Dependency for Rfacebook
library(devtools)
library(Rfacebook)



# Copy and paste the token generated and pass it to the token variable 
token<- "paste your token here"
# Pass your user ID to the function "getUsers"

me <- getUsers("778278022196130", token, private_info = TRUE)
me$name   # My name
me$hometown # My home town


require("Rfacebook")
# Using your Apps Authentication
fb_oauth <- fbOAuth(app_id="11", app_secret="XX",extended_permissions = TRUE)

save(fb_oauth, file="fb_oauth")
load("fb_oauth")


# Please use your token in the below code. And check for its status. 
token<- "XXXXXXXXX"
friends <- getFriends(token, simplify = TRUE)
head(friendsz) # To see few of your friends


friends_data <- getUsers(friends$id, token, private_info = TRUE)
table(friends_data$gender)  # gender

table(substr(friends_data$locale, 1, 2)) #Language

table(substr(friends_data$locale, 4, 5))  # country

table(friends_data$relationship_status)

#Get the Like data for a user
likes <- getLikes(user="me", n=100, token=token)
head(likes)
?getLikes

getCheckins(user="me", n = 10, token, tags = FALSE)

# Query to pull friends
friends <- getFQL("SELECT uid2 FROM friend WHERE uid1=me()", token=token)

# Details about friends
friends <- getFQL("SELECT uid, name, pic_square FROM user WHERE uid = me() OR uid IN (SELECT uid2 FROM friend WHERE uid1 = me())", token=token)


network <- getNetwork(token, format="adj.matrix")
head(network)

require(igraph)
social_graph <- graph.adjacency(network)
layout <- layout.drl(social_graph,options=list(simmer.attraction=0))

plot(social_graph, vertex.size=10, vertex.color="green", 
     vertex.label=NA, 
     vertex.label.cex=0.5,
     edge.arrow.size=0, edge.curved=TRUE,layout=layout.fruchterman.reingold)


dev.copy(png,filename="C:/Users/Sharan/Desktop/SMM-03-network.png", width=600, height=600);
dev.off ();


# Measuring Degree
degree(social_graph, v=V(social_graph), mode = c("all", "out", "in", "total"), loops = TRUE, normalized = FALSE) 
degree.distribution(social_graph, cumulative = FALSE)

#Measuring Betweenness
betweenness(social_graph, v=V(social_graph), directed = TRUE, weights = NULL, nobigint = TRUE, normalized = FALSE)

# Measuring Closeness
closeness(social_graph, vids=V(social_graph), mode = c("out", "in", "all", "total"), weights = NULL, normalized = FALSE)

# Cluster in network
is.connected(social_graph, mode=c("weak", "strong"))
clusters(social_graph, mode=c("weak", "strong"))


# Community
network_Community <- walktrap.community(social_graph)
modularity(network_Community)
plot(network_Community, social_graph, vertex.size=10, vertex.label.cex=0.5, vertex.label=NA, edge.arrow.size=0, edge.curved=TRUE,layout=layout.fruchterman.reingold)
dev.copy(png,filename="C:/Users/Sharan/Desktop/SMM-03-community.png", width=600, height=600);
dev.off ();



######### Facebook Page data
page <- getPage("TED", token, n = 500)
head(page, n=2)

posts <- searchFacebook(string = "talks", token, n = 500, since = "25 november 2013 00:00", until = "25 november 2013 23:59")

# Page with maximum likes
page[which.max(page$likes_count), ]


# Most trending posts
page <- getPage("TED", token, n = 5000)
head(page, n=2)
# Filtering recent data
pageRecent <- page[which(page$created_time > "2015-04-01"), ]
# sort by number of likes
top <- pageRecent[order(- pageRecent$likes),]
head(top, n=2)

# Checking trend
post1 <- getPost("29092950651_10155447292600652", token, n = 1000, likes = FALSE, comments = FALSE)
post2 <- getPost("29092950651_10155447292600652", token, n = 1000, likes = FALSE, comments = FALSE)
post2
###########################
# What peole talk


post_id <- head(page$id, n = 1)  ## ID of most recent post
post <- getPost(post_id, token, n = 1000, likes = TRUE, comments = TRUE)
head(post$comments, n=2)
comments <- post$comments
library(sqldf)
influentialusers <- sqldf("select from_name, sum(likes_count) as totlikes from comments group by from_name")
head(Infusers)
influentialusers$totlikes <- as.numeric(influentialusers$totlikes)
top <- influentialusers[order(- influentialusers$totlikes),]
head(top, n=10)


# Most influential user
post_id <- head(page$id, n = 100)
head(post_id, n=10)
post_id <- as.matrix(post_id)
allcomments <- ""
# Collecting all the commments from all the 100 posts
for (i in 1:nrow(post_id))
{
  post <- getPost(post_id[i,], token, n = 1000, likes = TRUE, comments = TRUE)
  comments <- post$comments
  allcomments <- rbind(allcomments, comments)
}

influentialusers <- sqldf("select from_name, sum(likes_count) as totlikes from allcomments group by from_name")
influentialusers$totlikes <- as.numeric(influentialusers$totlikes)
top <- influentialusers[order(- influentialusers$totlikes),]
head(top, n=20)




### Page Performance

## convert Facebook date format to R date format
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}
## aggregate metric counts over month
aggregate.metric <- function(metric) {
  m <- aggregate(page[[paste0(metric, "_count")]], list(month = page$month), mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}

# Get the post from the page bimtrichy
page <- getPage("bimtrichy", token, n = 500)

page$datetime <- format.facebook.date(page$created_time)
page$month <- format(page$datetime, "%Y-%m")
df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)
df <- do.call(rbind, df.list)

# plot the trend chart
library(ggplot2)
library(scales)
ggplot(df, aes(x = month, y = x, group = metric)) + geom_line(aes(color = metric)) + scale_x_date(breaks = "years", labels = date_format("%Y")) + scale_y_log10("Average count per post", breaks = c(10, 100, 1000, 10000, 50000)) + theme_bw() + theme(axis.title.x = element_blank()) + ggtitle("Facebook Page Performance")
# saving the image locally
ggsave(file="C:/Users/Sharan/Desktop/SMM-format-trend.png", dpi=500)



####  Spam detection

page <- getPage("beach4all", token, n = 500)

post_id <- head(page$id, n = 100)
head(post_id, n=10)
post_id <- as.matrix(post_id)
allcomments <- ""
# Collecting all the commments from all the 100 posts
for (i in 1:nrow(post_id))
{
  post <- getPost(post_id[i,], token, n = 1000, likes = TRUE, comments = TRUE)
  comments <- post$comments
  allcomments <- rbind(allcomments, comments)
}
# converting to data frame
allcomments <- as.data.frame(allcomments)
# create a new column
allcomments$chars <- ""
# Populate the new column with number of characters in message
allcomments$chars <- nchar(allcomments$message)

allcomments$url <- ""
allcomments$url <- grepl(".com", allcomments$message)

allcomments$spam <- ""
#head(allcomments, n = 15)
#nrow(allcomments)
# subsetting top 100 rows as train data
train <- allcomments[1:100,]
# rest of the data as test data
test <- allcomments[101:nrow(allcomments),]

# writing the train data into local disk
write.csv(train,"C:/Users/Sharan/Desktop/Chapter 3/comment-train.csv" )
write.csv(test,"C:/Users/Sharan/Desktop/Chapter 3/comment-test.csv" )


train <- read.csv("C:/Users/Sharan/Desktop/Chapter 3/comment-train.csv" )
test <- read.csv("C:/Users/Sharan/Desktop/Chapter 3/comment-test.csv" )


newTrain <- train[,c("likes_count", "chars", "url", "spam")]
newTest <- test[,c("likes_count", "chars", "url", "spam")]

glm.out = glm(spam ~ ., family=binomial(logit), data=newTrain)

prediction <- predict(glm.out,newTest, type = "response")
newTest$spam <- prediction
newTest <- cbind(test$from_name, newTest)
head(newTest, 20)


## News feed in facebook

# Pull the new feed
newsfeed <- getNewsfeed(token, n = 200)
# check the news feed retrieved
head(newsfeed, 20)

# Convert the date format 
newsfeed$datetime <- format.facebook.date(newsfeed$created_time)
# get the current date
currdate <- Sys.time()
# Getting the priority score based on the recency of the post 
maxdiff <- max(difftime(currdate, newsfeed$datetime, units="hours"))
newsfeed$priority <- maxdiff - difftime(currdate, newsfeed$datetime, units="hours")
newsfeed$priority <- as.numeric(newsfeed$priority)
# Function to normalize the values in R
fnpriority <- function(x){(x-min(x))/(max(x)-min(x))}
# Normalizing the columns required
newsfeed$priority <- fnpriority(newsfeed$priority) *100
newsfeed$plikes_count <- fnpriority(newsfeed$likes_count) *100
newsfeed$pcomments_count <- fnpriority(newsfeed$comments_count) *100
newsfeed$pshares_count <- fnpriority(newsfeed$shares_count) *100
# Computing the score
newsfeed$score <- newsfeed$plikes_count + newsfeed$pcomments_count + newsfeed$pshares_count + newsfeed$priority
# Sort the score based on the descending order
newsfeed <- newsfeed[order(-newsfeed$score),]
toppages <- newsfeed[,c("from_id", "from_name", "type", "score")]
head(toppages, 20)

# Recommendations for users

friends <- getFriends(token, simplify = TRUE)
head(friends, 26)
friendsid1 <- "paste your friends id here"
friendsid2 <- "paste your friends id here"
friendsid3 <- "paste your friends id here"
friend1 <- getLikes(friendsid1, n = 100, token)
friend2 <- getLikes(friendsid2, n = 100, token)
friend3 <- getLikes(friendsid3, n = 100, token)
friend1$user <- "friend1"
friend2$user <- "friend2"
friend3$user <- "friend3"

friendlikedata <- rbind(friend1, friend2, friend3)
head(friendlikedata)

forRecc <- friendlikedata[,c("user", "id")]

write.csv(forRecc,"C:/Users/Sharan/Desktop/Chapter 3/forRecc.csv", row.names = FALSE, col.names = NA)

library(arules)
data = read.transactions(file="C:/Users/Sharan/Desktop/Chapter 3/forRecc.csv", rm.duplicates= FALSE, format="single",sep=",",cols =c(1,2));
head(data, 10)
nrow(data)
inspect(data)
rules <- apriori(data,parameter = list(sup = 0.2, conf = 0.001, target="rules", minlen=3, maxlen=5));
inspect(rules);

