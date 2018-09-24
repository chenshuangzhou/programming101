#### Social Media Mining
#### Chapter 6 - More networks
#### Author: Sharan Kumar Ravindran

# Set your working directory accordingly
setwd("C:/Users/Accelyst/Desktop/SMM/Chapter 6")
getwd()

install.packages("SocialMediaMineR")
install.packages("XML")
install.packages("stringi")
library(SocialMediaMineR)


get_facebook("http://www.bbc.com/")
get_pinterest("http://www.bbc.com/")
get_reddit("http://www.bbc.com/")
get_stumbleupon("http://www.bbc.com/")
get_twitter("http://www.bbc.com/")
get_url("http://goo.gl/muN6lV")


news_urls <- c(
  "http://www.bbc.com/",
  "http://www.euronews.com/",
  "http://www.cnn.com/",
  "http://www.nytimes.com/",
  "http://www.guardian.co.uk/",
  "http://www.globalpost.com/",
  "http://www.france24.com/",
  "http://www.aljazeera.com/",
  "http://www.reuters.com/",
  "http://www.foxnews.com/",
  "http://www.nbcnews.com/",
  "http://www.huffingtonpost.com/",
  "http://www.wsj.com/",
  "http://www.ndtv.com/")

allresults <- get_socialmedia(news_urls, sleep.time = 0)
allresults


#### Getting the review from a website
urll <- 'http://www.amazon.com/gp/video/detail/B00L83TQR6?ie=UTF8&redirect=true&ref_=s9_nwrsa_gw_g318_i1'
library(XML)
doc <- htmlParse(urll)
doc
review <- xpathSApply(doc,'//div[@class="a-section"]',xmlValue)
data.frame(review)

### wiki
# retrieve table contents
# # option 1
library(XML)
library(RCurl)
url <- "http://en.wikipedia.org/wiki/Wikipedia:List_of_Wikipedians_by_number_of_edits"
tabs <- getURL(url)
?getURL
tabs <- readHTMLTable(tabs, stringsAsFactors = F)
head(tabs)

# option 2
library(httr)
url1 <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
tabs <- GET(url1)
tabs <- readHTMLTable(rawToChar(tabs$content), stringsAsFactors = F)
head(tabs)
class(tabs)
tablecontent <- tabs$`NULL`
head(tablecontent, 10)

## Other info
install.packages("Rtools")
library(devtools)
install_github("Ironholds/WikipediR")
library(WikipediR)
# random page 
randomContent <- random_page("en","wikipedia")
head(randomContent)
# Specific Page
pageContent <- page_content("en","wikipedia", page_name = "A._P._J._Abdul_Kalam")
head(pageContent)
# revisions made to the page
revision_diff("en","wikipedia", revisions = 674484862, direction = "next")
# back links from a page
page_backlinks("en","wikipedia", page = "A._P._J._Abdul_Kalam")


# User contribution
user_contributions("en", "wikipedia", username = "Ironholds", properties = "comment")
#Retrieving information from a Wikimedia project
data <- user_information("en", "wikipedia", user_names = "rsharankumar", properties = c("blockinfo", "groups", "implicitgroups", "rights", "editcount", "registration", "emailable", "gender"))
sample <- data$query$users[[1]]
sample <- as.data.frame(sample)

###Tumblr
install.packages("tumblR")
install.packages("base64enc")
install.packages("httpuv")
library(tumblR)

consumer_key <-'<paste your consumer key here>'
consumer_secret <- '<paste your consumer secret here>'
appname <- '<paste your app name here>'
tokenURL <- 'http://www.tumblr.com/oauth/request_token'
accessTokenURL <- 'http://www.tumblr.com/oauth/access_token'
authorizeURL <- 'http://www.tumblr.com/oauth/authorize'


app <- oauth_app(appname, consumer_key, consumer_secret)
endpoint <- oauth_endpoint(tokenURL, authorizeURL, accessTokenURL)
token <- oauth1.0_token(endpoint, app)

url <- "ifpaintingscouldtext.tumblr.com"

# url of blogs avatar
avatar(base_hostname = url, size =512)



info.blog(base_hostname = url, api_key = consumer_key)

likes(base_hostname = url, limit = 20, offset = 0, api_key = consumer_key)

follow(url = url, token = token, consumer_key = consumer_key, consumer_secret = consumer_secret)
followers(base_hostname = url, limit = 20, offset = 0, token=token, consumer_key = consumer_key, consumer_secret = consumer_secret)


#method 2

library(devtools)
install_github("klapaukh/tumblR")
setup_tumblr_apikey(consumer_secret)
all = get_posts("staff.tumblr.com")
posts = all$posts
head(posts)

get_info("staff.tumblr.com")
get_likes("staff.tumblr.com")


### Quora

udata <- fromJSON("http://quora.christopher.su/users/Sharan-Kumar-R")
fromJSON("http://quora.christopher.su/users/Sharan-Kumar-R/activity/answers")
fromJSON("http://quora.christopher.su/users/Sharan-Kumar-R/activity/user_follows")
fromJSON("http://quora.christopher.su/questions/If-space-is-3-dimensional-can-time-also-be-3-dimensional")

### google maps

install.packages("RgoogleMaps")
library(RgoogleMaps)
getGeoCode("Big Ben")
getGeoCode("10 Downing Street")
getGeoCode("London Eye")

BigBenMap <- GetMap(center="Big Ben", zoom=13)
PlotOnStaticMap(BigBenMap)
dev.copy(png,filename="bigben.png", width=600, height=600);
dev.off ();


lat = c(51.5007292,51.5033635,51.503324);
lon = c(-0.1246254,-0.1276248,-0.119543);
center = c(mean(lat), mean(lon));
zoom <- min(MaxZoom(range(lat), range(lon)));

Map <- GetMap(center=center, zoom=zoom,markers = paste0("&markers=color:blue|label:B|",
                                                          "51.5007292,-0.1246254&markers=color:green|label:D|51.5033635,-0.1276248&markers=",
                                                          "color:red|color:red|label:L|51.503324,-0.119543"), destfile = "MyTile1.png");
PlotOnStaticMap(Map)
dev.copy(png,filename="london.png", width=600, height=600);
dev.off ();


# Ploting on the map
geodata <- read.csv("geodata.csv")
head(geodata)
center = c(mean(geodata$latitude), mean(geodata$longitude));

map <- GetMap(center=center, zoom=2,
              size=c(480,480),destfile = file.path(tempdir(),"meuse.png"),
              maptype="mobile", SCALE = 1);
par(cex=1.5)
bubbleMap(geodata, coords = c("longitude", "latitude"), map=map,
          zcol='comments_count', key.entries = 100+ 100 * 2^(0:4));
dev.copy(png,filename="bubblemap.png", width=600, height=600);
dev.off ();


####linkedin

library(devtools)
install_github("mpiccirilli/Rlinkedin")
library(Rlinkedin)

app_name <- "<paste your App name here>"
consumer_key <- "<paste consumer key here>"
consumer_secret <- "<paste your consumer secret here>"


in.auth <- inOAuth(app_name, consumer_key, consumer_secret)

# Search companies
search.comp <- searchCompanies(in.auth, keywords = "LinkedIn")
head(search.comp)

getProfile(token = in.auth, connections = FALSE, id = NULL)



## Blogger

fromJSON("https://www.googleapis.com/blogger/v3/blogs/263662486407512058?key=<paste your key here>")

GET("https://www.googleapis.com/blogger/v3/blogs/263662486407512058/pageviews?range=all&key=<paste your key here>")

######Foursquare
library(fromJSON)
#user
fromJSON("https://api.foursquare.com/v2/users/self?oauth_token=<pasye your token here>")

#venue
fromJSON("https://api.foursquare.com/v2/venues/40a55d80f964a52020f31ee3?oauth_token=<paste your token here>")

#venues tips
fromJSON("https://api.foursquare.com/v2/venues/514d613de4b0ab03fe0601fb/tips?oauth_token=<paste your token here>")


# tips
fromJSON("https://api.foursquare.com/v2/tips/49f083e770c603bbe81f8eb4?oauth_token=<paste your token here>")


