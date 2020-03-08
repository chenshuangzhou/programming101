library(bibliometrix)

#biblioshiny()

#Data loading and converting
D <- readFiles("C:/Users/Article-Big data and artificial intelligence/isi279.txt")

#the object D can be converted in a data frame using the function convert2df
M <- convert2df(D, dbsource = "isi", format = "plaintext")

#Bibliometric Analysis
results <- biblioAnalysis(M, sep = ";")
S <- summary(object = results, k = 10, pause = FALSE) #summary and plot of results

plot(x = results, k = 10, pause = FALSE)

#Analysis of Cited References
M$CR[1]
CR <- citations(M, field = "article", sep = ";") #To obtain the most frequent cited manuscripts in the sample
CR$Cited[1:10]

CR <- citations(M, field = "author", sep = ";") #To obtain the most frequent cited first authors
CR$Cited[1:10]

CR <- citations(M, field = "Sources", sep = ";") #To obtain the most frequent cited first authors
CR$Cited[1:10]

CR <- localCitations(M, sep = "; ") #To obtain the most frequent local cited authors
#This works only with WOS data for now
CR$Authors[1:10,]
CR$Papers[1:10,]
CR$Sources[1:10,]

##Authors' Dominance ranking
DF <- dominance(results, k = 10)
DF

#Authors' h-index
indices <- Hindex(M, authors="MACHARIS C", sep = ";",years=10) #need to change authors name o get valid output
# Bornmann's impact indices:
indices$H
# Bornmann's citations
indices$CitationList

#To calculate the h-index of the first 10 most productive authors
authors=gsub(","," ",names(results$Authors)[1:10])

indices <- Hindex(M, authors, sep = ";",years=50)

indices$H

#Lotka's Law coefficient estimation
L <- lotka(results)
# Author Productivity. Empirical Distribution
L$AuthorProd
# Beta coefficient estimate
L$Beta
## [1] 3.25276
# Constant
L$C
## [1] 0.6263423
# Goodness of fit
L$R2
# P-value of K-S two sample test
L$p.value


###compare the two distributions using plot function:
# Observed distribution
Observed=L$AuthorProd[,3]

# Theoretical distribution with Beta = 2
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))

plot(L$AuthorProd[,1],Theoretical,type="l",col="red",ylim=c(0, 1), xlab="Articles",ylab="Freq. of Authors",main="Scientific Productivity")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical (B=2)","Observed"),col=c("red","blue"),lty = c(1,1,1),cex=0.6,bty="n")

####Bibliographic coupling
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ";")
#bib coupling for research clusters
net=networkPlot(NetMatrix, type= "mds",normalize = "association", cluster= "louvain",  n = 50, size=4, size.cex = T, label=T, labelsize=3, label.cex=T, remove.isolates = T, Title = "Bibliographic coupling of articles")
#to run vosviewer in bibliometrix
#net=networkPlot(NetMatrix, type= "vosviewer", normalize = "association", cluster= "louvain",  n = 50, vos.path="C:/Users/ziaul/Documents/VOSviewer.jar")

#Bibliographic co-citation
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
net <- networkPlot(NetMatrix, n = 20, type = "kamada", Title = "Co-Citation",labelsize=0.5)  

##Bibliographic collaboration
#authors' collaboration network:
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "authors", sep = ";")
net <- networkPlot(NetMatrix, n = 20, type = "kamada", Title = "Author collaboration",labelsize=0.5)
#or a country collaboration network:
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
net <- networkPlot(NetMatrix, n = 20, type = "kamada", Title = "Country collaboration",labelsize=0.5)


####
#Visualizing bibliographic networks

# Create a country collaboration network
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.8)

#####Co-Citation Network
# Create a co-citation network
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
# Plot the network
net=networkPlot(NetMatrix, n = 20, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,label.short = TRUE, edgesize = 5)


##Keyword co-occurrences
# Create keyword co-occurrencies network
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)


#### Conceptual Structure using keywords
CS <- conceptualStructure(M, method="CA", field="ID", minDegree=2, k.max = 5, stemming=f, labelsize=8,documents=20)

Clusters=Map$words[order(Map$words$Cluster,-Map$words$Occurrences),]

### Create a historical citation network
histResults <- histNetwork(M, n = 20, sep = ".  ") #works with WOS data only

# Plot a historical co-citation network
net <- histPlot(histResults, size = FALSE,label=TRUE, arrowsize = 0.5)#works with WOS data only

