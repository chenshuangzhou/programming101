## File location examples:
read.table("C:/Users/chens/OneDrive/research/1personal/Geriatrics/meta/test.txt",header=T,sep="\t",na.strings = "NA")  # Office - Dell Inspiron 16
read.table("C:/Users/Student RA/vscode temp/test.txt",header=T,sep="\t",na.strings = "NA")                             # CoA 
read.table("D:/OneDrive/research/1personal/Geriatrics/meta/test.txt",header=T,sep="\t",na.strings = "NA")              # Office - Lenovo X1
read.table("C:/Users/chens/OneDrive/research/1personal/Geriatrics/meta/test.txt",header=T,sep="\t",na.strings = "NA")              # Home - Dell Gaming



## PRISMA Flow Chart
library(PRISMAstatement)

prisma(found = 3701,
       found_other = 21,
       no_dupes = 3531, 
       screened = 3531, 
       screen_exclusions = 3486, 
       full_text = 45,
       full_text_exclusions = 2, 
       qualitative = 43, 
       quantitative = 28,
       extra_dupes_box = TRUE)


## Writing Progress Function

p = function(){
  progress = read.table("C:/Users/chens/OneDrive/research/1personal/Programming/progress.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)
  progress = as.data.frame(progress)
  print(aggregate(progress,by=list(progress$Project),max))
  n = dim(progress)[1]
  progress[n+1,1] = readline("The date today?")
  progress[n+1,2] = readline("The day today? (1-7)")
  progress[n+1,3] = readline("The word counts?")
  progress[n+1,4] = readline("Project name?")
  progress[n+1,5] = readline("Percentage of achievement?")
  write.table(progress,file="C:/Users/chens/OneDrive/research/1personal/Programming/progress.csv",sep=",",row.names=F)
  aggregate(progress,by=list(progress$Project),max)
}


progress = read.table("D:/OneDrive/research/1personal/Programming/progress.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)
progress1 = progress[order(progress$Project,progress$Status),]


## Word Cloud ######################################

installed.packages("wordcloud")
installed.packages("wordcloud2")
installed.packages("jiebaR")
install.packages("Rwordseg", repos = "http://R-Forge.R-project.org")
install.packages("jiebaRD")
install.packages("jiebaR")

library(wordcloud)
library(jiebaR)


m = read.table("C:/Users/chens/OneDrive/research/Projects/1 Meaning Making - VL/Systematic review on MM and Caregiver - VL/MM.csv",header=T,sep=",",na.strings = "NA") 




# conditional search
progress = read.table("C:/Users/chens/OneDrive/research/1personal/Programming/progress.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)

  # Search by project
project = progress[progress$Project  == "meaning table",]$Word
sum(project); mean(project)

  # Search by date range
date = progress[progress$Date  >= 190201 & progress$Date <= 190214,]$Word
sum(date); mean(date)

  # Search by days
day = progress[progress$Day  == 3,]$Word
sum(day); mean(day)

  # Search by month
month = progress[substr(progress$Date,3,4) == "02",]$Word
sum(month); mean(month)



### Beginning of Notes ##############################################################33

summary(mtcars)
a=c(1,4,2,7,6)
plot(x=nchar(a),y=a)
points(4.5,3)
plot(1:5,y=a)
points(4.5,3,2,3)
plot(sin,-pi,pi)
plot(0:10,log(0:10,base=2),type="b")
plot(log2,0,10)
plot(sin,-pi,pi)
plot(sin,-pi,pi,type="l")
mtcars

x = paste("x",1:100,sep="") # merge character and numeric values, separater being none

## substituion, replacement
gsub(" ","",res) # take out space in characters
sub(pattern = "Adam", replacement = "world", text)

x5=c(seq.Date(as.Date("1999/9/3"),as.Date("2002/3/3"),by="6 months"))
print(x5)

c5=c("1999/9/10","2000/2/10","2000/9/16","2001/2/23","2001/9/9","2002/3/28")
as.Date(c5)

gsub("TRUE""FALSE","Yes""No",c(abs(as.Date(x5)-as.Date(c5))<=21))

abs(as.Date(x5)-as.Date(c5))<=21

1/8/2012
Books:
  Brian Everitt and Torsten Hothorn, A Handbook of Statistical Analyses Using R, Chap-man & Hall/CRC, 2006.
programming and less on analysis: Owen Jones, Robert Maillardet, Andrew Robinson, Introduction to Scienti c Program-ming and Simulation Using R, Chapman & Hall/CRC, 2009.
In depth: John Chambers, Software for Data Analysis: Programming with R, Springer, 2008.

download:
  r-reference card

Lecture 1
Comprehensive R Archive Network (CRAN)
To install 
R, visit the R website,
http://www.r-project.org
and follow the installation directions (see the FAQ section for more details).

You will rst be asked to selected a CRAN site, it is preferred that you select the site closest to you in order to reduce network load.
Next select the Windows operating system.  
For Windows you will want to install the base system (actual R program)and R tools (tools for creating 
                                                                                    R packages).

R console:
  [1]: reference number
%%: remainder
NaN: not a number
?[function name] for help
help.search ("key words")
save the file: [name].R
# for comments
; for multiple commands in the same line
rm(list=ls(all=TRUE))
To comment a line of code use a #.
.last.value: retrieve the last value in the system

for (I in 1:1E10)
  There is no block commenting in R.
Tip
Comments are notes that help users understand portions of your code.
They are also useful reminders to yourself of what was done and why it
was done. Including meaningful comments in code is a major part of
writing good programs.

In addition to comments, it is helpful to make good use of white space
(returns and indentions) to make code readable.

Adding horizontal lines

install.packages("phmm")
library(phmm)
require(packages)

Lecture 2

x = (1,3,5); x[x<4]  # logic vector in a vector, return 1,3
x = c(-1,1:3,NA); x[is.na(x)] = 0 # replace 0 for NA


Seq: sequence of integers
by: interval
len= length
?Syntax for help
use the same length when using replacement of vector
?'[<-'   :  how to do assignment
'[<-' (c(8,9), x [1:3])
in the matrix: %*% means matrix product, each element in first matrix times all the elements in the same row of the second matrix
diag(1,3): with the numeric "1" in a 3 by 3 matrix



Lecture 3
Numeric constant immediately followed by an L is an integer number,
exp(3.14i)+1
exp(complex(real=0, imaginary=pi)i)+1
exp(1i*pi)+1
&& only compares the first element if there are two vectors compared
x=1:10
x[(x%%2==0) | (x > 5)]

if there is data missing, variable ==1 can be problematic 
identical(x, 1)

all.equal(x^2,2[digits after 0])
p <- c(TRUE, TRUE, FALSE, FALSE)
q <- c(TRUE, FALSE, TRUE, FALSE)
!(p & q) == !p | !q
!(p | q) == !p & !q
animal <- c("bird", "horse", "fish")
food <- c("seeds", "hay", "worms")
paste(animal, food, sep=".") # Pairwise joining of animals and food
#search for the color()
grep("red", colors(), value=TRUE)

grep("^[^a-p]", colors(), value=TRUE)  # the first ^ means one more extra position before []; the second ^ means "not"
?regex
d <- c("Oct-17-1979","Oct-13-1971","Oct-13-1960","Oct-15-1925","Oct-16-1909")
as.Date(d, format="%b-%d-%Y")
as.Date("2013/1/7")-as.Date("2013/4/27")+1
a <- seq.Date (as.Date("2013/1/7"),as.Date("2013/4/27"),by="days")
cut.Date(a,breaks="Mondays")
weekdays(Monday) to set monday for the beginning of the week

Lecture 4
factor(rep(1:2,4),level=2:1)

> x <- factor(substring("biostatistics", 1:13, 1:13), levels=letters)
> x
[1] b i o s t a t i s t i c s
Levels: a b c d e f g h i j k l m n o p q r s t u v w x y z
> factor(x)
[1] b i o s t a t i s t i c s
Levels: a b c i o s t
> x[,drop=TRUE]
[1] b i o s t a t i s t i c s
Levels: a b c i o s t

grade = runif(25,min=50,max=100)

letter=gl(n=4,k=1,labels=paste([A-D]),include.lowest=TRUE)
cut(grade,seq(50,100,by=10), include.lowest=TRUE,right=FALSE,labels=c("A","B","C","D","E"))


ozone=airquality$Ozone
which(is.na(ozone))
sum(is.na(ozone))

as.numeric("123") #turn to the number

x=c(-3,-.2,0,.3,2)
(x+1)*(x>=-1 & x<0)*x+(1-x)*(x>=0 & x<1)*x
curve((1+x)*(x>=-1 & x<0)*x+(1-x)*(x>=0 & x<1)*x,-2,2)


x <- c(5,4,2,8,9,10)
n <- length(x)
sort.x <- sort(x)
if(n%%2==0) {
  median <- (sort.x[n/2]+sort.x[1+n/2])/2
} else {
  median <- sort.x[(n+1)/2]
}


#syntax of IF
if(condition ) expression if TRUE
if(condition ) {
  expressions if TRUE
}
if(condition )expression if TRUE else expression if FALSE
if(condition ) {
  expressions if TRUE
} else {
  expressions if FALSE
}


ifelse(x<=1,NA,x)

x <- c(5,4,2,8,9,10)
n <- length(x)
s <- sort(x)
ifelse(n%%2==0,c(s[n/2]/2+s[1+n/2]/2),c(s[(n+1)/2]))

y=runif(100)
central <- function(y, measure) {
  switch(measure,
         Mean = ,
         mean = mean(y),
         median = median(y),
         geometric = prod(y)^(1/length(y)),
         "Invalid Measure")
}
> y <- runif(100)
> central(y, "mean")
[1] 0.5101253
> central(y, "Mean")
[1] 0.5101253         #return the value of the next argument
> central(y, "Median")
[1] "Invalid Measure"
> central(y, "geometric")
[1] 0.3949147

median


y=runif(100)
central <- function(y, measure) {
  switch(tolower(measure),
         mean = mean(y),
         median = median(y),
         geometric = prod(y)^(1/length(y)),
         "Invalid Measure")
}

#for loop
a=1
for(i in 1:10){
a=a*i
cat(i,a,"\n")
}

#while loop
i=a=1
while(i<11){
a=a*i
i=i+1
cat(i,a,"\n")
}

#repeat loop
i=a=1
repeat{
a=a*i
cat(i,a,"\n")
i=i+1
if(i>10) break
}


f <- 1
for(i in 1:10) {
  f <- f*i
  cat(i, f, "\n")
}
f==prod(c(1:10))


f <- 1
for(i in 1:10) {
  f <- f*i
  cat(i, f)
}




i=1:100
f=1
if (i>=1) {
  f=f*i
}else break
f



#practice (1, 3, 6, 10, 15, 21, 28, 36, 45, 55)
out=numeric(10)
for (i in 1:10) {
  out[i]=sum(1:i)
}

###
cumsum(1:10)

####
f=c(rep(1,9))
for (i in 1:9){
  f[i+1]=f[i]+i+1
}
f

n=100
startTime <- proc.time()[3]
pb <- winProgressBar(title="Progress Bar", min=0, max=n)
for(i in 1:n) {
  Sys.sleep(.1) # Suspend execution of R expressions for .1 seconds
  setWinProgressBar(pb, i, title=paste(round((i/n)*100),"% Complete"))
}
Sys.sleep(.5)
close(pb)
elapseTime <- proc.time()[3]-startTime
cat("Elapsed Time:",floor(elapseTime/60),"min",elapseTime%%60,"sec \n")

###Lecture 5

x=matrix(c(a,b,c,d),2,2,byrow=TRUE,dimnames=list(c("yes","no"),c("yes","no")))
x=matrix(c(189,104,10845,10933),2,2,
        dimnames=list("Treatment"=c("Placebo","Aspirin"),
                      "Myocardial Infarction"=c("Yes","No")))
odds.ratio=function(X){
  (X[1,1]*X[2,2])/(X[1,2]*X[2,1])
}

odds.ratio(X)

###usage of cat()function
need to specify "\n" for end of each line


#package for Odds Ratio
odds.ratio <- function(X, conf.level=.95) {
  OR <- (X[1,1]*X[2,2])/(X[1,2]*X[2,1])
  logOR.SE <- sqrt(sum(1/X))
  alpha <- 1-conf.level
  CI.lower <- exp(log(OR) - qnorm(1-alpha/2)*logOR.SE)
  CI.upper <- exp(log(OR) + qnorm(1-alpha/2)*logOR.SE)
  cat("Odds Ratio = ", OR, "\n",
      conf.level*100, "% Confidence Interval = (",
      CI.lower, ", ", CI.upper, ")\n", sep="")
  # Different approaches for returning results
  OR
  #return(OR)
  #invisible(OR)   #only return the specified value  when asking for assigned variable such as x=OR(X)
  #out <- list(OR=OR, CI=c(CI.lower, CI.upper), conf.level=conf.level)
  #return(out)      #return the list "out" for that assignment
}
# Unassigned and assigned function results
odds.ratio(X)
OR <- odds.ratio(X)


a=matrix(c(189,104,10845,10933),2,2,
  dimnames=list("Treatment"=c("Placebo","Aspirin"),
	"Myocardial Infarction"=c("Yes","No")))
or=a[1,1]*a[2,2]/(a[1,2]*a[2,1])



#Lecture 6
"main="Y vs X"" is the title of the [plot]
type="n" means nothing but plain on the graph

x <- rnorm(50); y <- rnorm(50)
group <- rbinom(50, size=1, prob=.5)

plot(sort(x), sort(y), type="l", lty=2, lwd=2, col="blue")


# Plot a 5th order polynomial
curve(3*x^5-5*x^3+2*x, from=-1.25, to=1.25, lwd=2, col="blue")
# Plot the gamma density
curve(dgamma(x, shape=2, scale=1), from=0, to=7, lwd=2, col="red")
# Plot multiple curves, notice that the first curve determines the x-axis
curve(dnorm, from=-3, to=5, lwd=2, col="red")
curve(dnorm(x, mean=2), lwd=2, col="blue", add=TRUE)
# Add vertical lines at the means
lines(c(0, 0), c(0, dnorm(0)), lty=2, col="red")
lines(c(2, 2), c(0, dnorm(2, mean=2)), lty=2, col="blue")

# multi or 3D graph 
library(TeachingDemos) # Contains rotate.persp()
# Evaluate z on a grid given by x and y
x <- y <- seq(-1, 1, len=25)
z <- outer(x, y, FUN=function(x,y) -x*y*exp(-x^2-y^2))
# Contour plots
contour(x,y,z, main="Contour Plot")    #contour
filled.contour(x,y,z, main="Filled Contour Plot")
filled.contour(x,y,z, color.palette = heat.colors)  #color.palette for combinatin of colors
filled.contour(x,y,z, color.palette = colorRampPalette(c("red", "white", "blue")))
persp(x,y,z, shade=.75, col="green3") # 3-D Surface Plot
rotate.persp(x,y,z) # Rotate 3-D Surface Plot
# Add 2-D components to a 3-D Surface plot
# view is the "viewing transformation matrix" needed by trans3d
view <- persp(x,y,z, shade=.75, col="green3")
# Point at (x=1, y=1, z=.01)
points(trans3d(1,1,.1, view), cex=2, col="red", pch=19)
text(trans3d(1,1,.1, view), "(1,1,0.1)", pos=1, font=2)
# Line of z vs x, when y=-.5
lines(trans3d(x, y=-.5, z[7,], view), lwd=2, col="red")


#bivariate normal
library(TeachingDemos)
# Bivariate Normal Density
# x: 2x1 vector, mu: 2x1 mean vector, Sigma: 2x2 covariance matrix
bivariate.normal <- function(x, mu, Sigma) {
  exp(-.5*t(x-mu)%*%solve(Sigma)%*%(x-mu))/sqrt(2*pi*det(Sigma))
}

mu <- c(0,0)
Sigma <- matrix(c(1,.5,.5,1), nrow=2)
x <- y <- seq(-3, 3, len=25)
# Evaluate the bivariate normal density for each value of x and y
z <- outer(x, y,
           FUN=function(x, y, ...){
             apply(cbind(x,y), 1, bivariate.normal, ...)
           }, mu=c(0,0), Sigma=Sigma) #mu and sigma goes to "..." areas
# Filled contour and surface plot of the bivariate normal density
filled.contour(x,y,z, main="Bivariate Normal Density", color.palette=topo.colors)
persp(x, y, z, shade=.75, col="red", main="Bivariate Normal Density")
rotate.persp(x, y, z)

# Figure with two plots side by side
par(mfrow=c(1,2))
plot(rnorm(100), main="Figure 1", pch=19, col="red")
plot(rnorm(100), main="Figure 2", pch=5, col="blue")
# Create layout
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), heights=c(.5,1))
layout.show(3) # View layout
# Create layout
layout(matrix(c(2,0,1,3),2,2, byrow=TRUE), widths=c(3,.5), heights=c(.5,3))
layout.show(3) # View layout
# Plot scatterplot and boxplots
x <- rnorm(100); y <- rnorm(100)
# Notice that the range of the scatterplot and boxplots have the same limits
par(mar=c(4,4,1,1),oma=c(0,0,1,0), font.axis=2, font.lab=2, cex.axis=1.5, cex.lab=1.5)   #"oma" refers to outer margin
plot(x, y, xlim=c(-3,3), ylim=c(-3,3), xlab="X", ylab="Y", pch=17, col="darkgreen", cex=1.5)
box(lwd=2)
par(mar=c(0,4,0,1))
boxplot(x, horizontal=TRUE, ylim=c(-3,3), axes=FALSE, at=.75, border="red", lwd=3)  #horizontal=TRUE refers to the one horizontal
par(mar=c(4,0,1,0))
boxplot(y, ylim=c(-3,3), axes=FALSE, at=.75, border="blue", lwd=3)      #
# Add title in outer margin
title("Scatterplot and Boxplots of X and Y", outer=TRUE, line=-2, cex.main=2) #outer=TRUE



#To add text to the plot region use text() and use mtext() to add text to
#the fiure and outer margins.
par(mfrow=c(1,2), oma=c(2,2,2,2)) # Add an outer margin to the figure
set.seed(123); x <- rnorm(10); y <- rnorm(10)
# Plot 1
plot(x, pch=19, col="red", main="Figure 1") #"adj"=0.5, the plot is placed in the center
# Label each point with its index
text(1:10, x, label=1:10, pos=rep(c(4,2), c(2,8)), font=2, cex=1.5)
# Add fancy text to the plot region
text(1, 1, "This is Fancy Plot Text", family="HersheyScript", adj=0, cex=1.5)
# Add text to the margin
mtext("This is Margin Text for Figure 1", side=2, line=4)#"line" how far to plce the margin away from the plot
# Plot 2
plot(x, pch=15, col="blue", main="Figure 2")
# Add plain text to the plot region
text(1, 1, "This is More Plot Text", family="mono", adj=0, cex=1)
# Add text to the margin
mtext("This is Margin Text for Figure 2", side=3, line=.5)
# Outer Margin, the \n can be include in character strings to add new lines
title("OUTER\nTITLE", outer=TRUE, line=-1)#[\n]serves the return function, line=-1
mtext("This is Outer Margin Text", side=1, outer=TRUE, font=3)

###Math Expressions
#R is capable of adding LATEX like expressions to R graphics
# Use expression() to add math expressions to a  figrue
# The function bquote() is used to add expressions and values. Terms inside
.() are evaluated, the remaining terms are evaluated as math expressions.
# Math expressions can be used in place of almost any text argument
(cannot be used for axis labels on persp() plots).
# See ?plotmath for a complete list of the syntax used by expression()
and bquote()
plot(1:10, type="n", xlab="X", ylab="Y")
text(5.5, 9, "expression(y==alpha[1]*x+alpha[2]*x^2)", cex=1.5)
text(5.5, 8, expression(y==alpha[1]*x+alpha[2]*x^2), cex=1.5)
theta = 3
text(5.5, 6, "theta=3; bquote(hat(theta)==.(theta))", cex=1.5)
text(5.5, 5, bquote(hat(theta)==.(theta)), cex=1.5) #"."refere to the value of data but not the symbol of the data

#adding legend
windows(width=9, height=6) # Fix window size
par(mfrow=c(1,2), oma=c(3,0,2,0)) # Add an outer margin to the figure
set.seed(789)
x1 <- rnorm(10); x2 <- rnorm(10, mean=2)
y1 <- rnorm(10); y2 <- rnorm(10, mean=2)
# PLOT 1, Use range to determine a plot region that is large enough for all the points
plot(range(x1,x2), range(y1,y2), main="Figure 1", type="n", xlab="X", ylab="Y")   #
points(x1, y1, col="red", pch=19) # Group 1
points(x2, y2, col="blue", pch=0) # Group 2
legend("topleft", c("Group 1","Group 2"), pch=c(19,0), col=c("red", "blue"),
       horiz=TRUE, bty="n")
legend(locator(1), c("Group 1","Group 2"), pch=c(19,0), col=c("red", "blue"), title="Legend") #locator serves the function as where mouse click determins where the legend is.
# PLOT 2
plot(range(x1,x2), range(y1,y2), main="Figure 2", type="n", xlab="X", ylab="Y")
lines(sort(x1), sort(y1), col="red", type="o", pch=19) # Group 1
lines(sort(x2), sort(y2), col="blue", type="o", pch=0) # Group 2
legend(-2, 2.5, c("Group 1", "Group 2"), pch=c(19,0), col=c("red", "blue"),
       horiz=TRUE, bty="n", lty=1)  #bty="n" meaning not adding the border to the legend
# Legend in figure margin
legend(1.5, -2.25, c("Group 1", "Group 2"), pch=c(19,0), col=c("red", "blue"), lty=1,
       bty="n", xpd=TRUE)
# Legend in outer margin
legend(-5.25, -3, c("Group 1", "Group 2"), pch=c(19,0), col=c("red", "blue"), lty=1,
       horiz=TRUE, xpd=NA)  #legend cannot be added outside margin unless with 

# Plot with no axes
par(mar=c(5,5,5,5))
plot(1:10, axes=FALSE, ann=FALSE)
# Add an axis on side 2 (left)
axis(2)
# Add an axis on side 3 (top), specify tick mark location, and add labels
axis(3, at=seq(1,10,by=.5), labels=format(seq(1,10,by=.5), nsmall=3))# nsmall limits the decimals of the axes parameters.  
# Add an axis on side 4 (right), specify tick mark location and rotate labels
axis(4, at=1:10, las=2)
# Add axis on side 1 (bottom), with labels rotated 45 degrees
tck <- axis(1, labels=FALSE)
text(tck, par("usr")[3]-.5, labels=paste("Label", tck), srt=45, adj=1, xpd=TRUE)# [str] degree of rotaion 
box() # Add box aroung plot region
mtext(paste("Side", 1:4), side=1:4, line=3.5, font=2) # Add axis labels


# Create a single pdf of figures, with one graph on each page
pdf("SavingExample.pdf", width=7, height=5) # Start graphics device
x <- rnorm(100)
hist(x, main="Histogram of X")
plot(x, main="Scatterplot of X")
dev.off() # Stop graphics device


system("open filename.pdf")

# Create multiple pdfs of figures, with one pdf per figure
pdf(width=7, height=5, onefile=FALSE)
x <- rnorm(100)
hist(x, main="Histogram of X")
plot(x, main="Scatterplot of X")
dev.off() # Stop graphics device

#Lattice
library(lattice)
x <- rnorm(50)
y <- rnorm(50)
sex <- gl(2,25, labels=c("Male", "Female"))
trt <- rbinom(50, size=1, prob=.5)
# Basic trellis scatterplot
# Plot y versus x, given sex
xyplot(y ~ x | sex)
# Custom trellis scatterplot
xyplot(y ~ x | sex, xlab="X-Axis", ylab="Y-Axis", pch=19, col="blue", cex=1.5,
       main="Y vs X by Sex",
       strip=strip.custom(bg="gray75", par.strip.text=list(font=2)))
# Including a grouping variable within each panel (group) and add a legend (key)
xyplot(y ~ x | sex, group=trt, xlab="X-Axis", ylab="Y-Axis",
       pch=c(19,0), col=c("blue", "red"), cex=1.5,
       main="Y vs X by Sex and Treatment",
       key=list(text=list(c("Treatment 1", "Treatment 2")),
                points=list(pch=c(19,0), col=c("blue", "red")), columns=2),
       strip=strip.custom(bg="gray75", par.strip.text=list(font=2)))


# Plot data using a custom panel function
# panel function arguments:
# x = x-coordinates
# y = y-coordinates
# subscripts = indices of x and y from the original data
# groups = grouping variable
#
# Need to use panel.points, the base graphics function points() is not
# applicable for lattice graphics
# The layout argument is used to change the arrangment of the panels
xyplot(y ~ x | sex, group=trt,
       xlab="X-Axis", ylab="Y-Axis",
       main="Y vs X by Sex and Treatment",
       layout=c(1,2),
       panel=function(x, y, subscripts, groups) {
         panel.points(x, y,
                      col=ifelse(sex[subscripts]=="Male", "blue", "palevioletred1"),
                      pch=ifelse(groups==1, 19, 0), cex=1.5)
       },
       key=list(text=list(c("Male", "Female", "Treatment 1", "Treatment 2"),
                          col=c("blue", "palevioletred1", "black", "black")),
                points=list(pch=c(NA, NA, 19,0)), columns=2),
       strip=strip.custom(bg="gray75", par.strip.text=list(font=2)))



# animation effect
wlln <- function(max.n=150) {
  par(mfrow=c(2,2), oma=c(0,0,2,0), mar=c(5.1,4.1,1.75,1))
  sample.norm <- sample.exp <- array(dim=max.n)
  for(i in 1:max.n) {
    sample.norm[i] <- mean(rnorm(i, mean=0))
    sample.exp[i] <- mean(rexp(i, rate=1/3))
      par(mfrow=c(2,2), oma=c(0,0,3,0), mar=c(5.1,4.1,1,1))
      x <- seq(-3, 3, len=100)
    plot(x, dnorm(x), type="l", axes=F, ann=F, main="Normal Population with mean 0")
    polygon(x, dnorm(x), col="blue")
    axis(1)
    plot(sample.norm[1:i], type="o", xlab="Sample Size", ylab="Sample Mean", pch=19)
    abline(h=0, col="green3")
    x <- seq(0, 20, len=100)
    plot(x, dexp(x, rate=1/3),type="l",axes=F,ann=F,main="Skewed Population with mean 3")
    polygon(c(x, 0), c(dexp(x, rate=1/3), 0), col="blue")
    axis(1)
    plot(sample.exp[1:i], type="o", xlab="Sample Size", ylab="Sample Mean", pch=19)
    abline(h=3, col="green3")
    title(paste("Sample Size =", i), outer=TRUE)
    # Brief pause between plots
    #Sys.sleep(0.5)
  }
}

library(animation)
# Create a latex file that includes the WLLN animation
saveLatex(wlln(max.n=100), img.name = "WLLN",
          latex.filename="weaklawlargenum.tex", outdir=getwd(),
          interval = 0.1, nmax = 100, ani.dev = "pdf", ani.type = "pdf",
          ani.width = 7, ani.height = 7,
          ani.opts = "controls,width=0.95\\textwidth",
          documentclass = paste("\\documentclass{article}",
                                "\\usepackage[papersize={7in,7in},margin=0.3in]{geometry}", sep = "\n"))
# Create an HTML file that includes the WLLN animation
saveHTML(wlln(max.n=100), img.name = "WLLN", htmlfile = "WLLN.html",
         outdir=getwd(), interval = 0.1, nmax = 100, ani.dev = "png",
         ani.type = "png", ani.width = 700, loop=FALSE, verbose=FALSE)


#Lecture 6
set.seed(10)
x <- rnorm(100); y <- rnorm(100)
par(mar=c(5,5,5,5))
# High-level
plot(x, y, xlab="X-axis", ylab="Y-axis", main="Y versus X",
     pch=15, col="blue")
# Low-level
abline(h=0, v=0, lty=2)
text(-2.25, 2, "This is an example of a scatterplot.", adj=0)

option(warn=2)
sqrt(-1)
option(warn=0)
sqrt(-1)

Calculate the matrix inverse of 100 random matrices. Without try(),
set.seed(100)
# Create an empty list of length 100
A.inv <- vector("list", 100)
for(i in 1:100) {
  x <- sample(1:9, replace=TRUE)  #unique values in x
  A <- matrix(x, nrow=3)
  A.inv[[i]] <- solve(A)
}

And with the try() function,
set.seed(100)
A.inv <- vector("list", 100)
for(i in 1:100) {
  x <- sample(1:9, replace=TRUE)
  A <- matrix(x, nrow=3)
  t <- try(solve(A), silent=TRUE)
  if(is(t, "try-error")) next else A.inv[[i]] <- t
}
# Number of computationally singular matrices
empty <- lapply(A.inv, is.null)  #aapply return a summarizing list for the result
do.call(sum, empty) # Call the function sum using a list of arguments

#The function fail() calculates the number of failures in data by calling
either bothFail() or eitherFail() depending on the type of failures
specified by type.
bothFail <- function(A) {
  bfail <- A[A[,1]==0 & A[,1]==A[,2],]
  numFail <- nrow(bfail)
  if(numFail==0) NA else numFail
}
eitherFail <- function(A) {
  efail <- A[A[,1]==0 | A[,2]==0,]
  numFail <- nrow(efail)
  if(numFail==0) NA else numFail
}
fail <- function(data, type) {
  if(type=="both") return(bothFail(data))
  if(type=="either") return(eitherFail(data))
}


Test the function on difference scenarios
#2 pairs where both subjects fail
set.seed(1)
(A <- matrix(rbinom(20, 1, .5), ncol=2,
             dimnames=list(Pair=1:10, c("Treatment", "Placebo"))))
fail(A, "both")
#0 pairs where both subjects fail
set.seed(15)
(A <- matrix(rbinom(20, 1, .5), ncol=2,
             dimnames=list(Pair=1:10, c("Treatment", "Placebo"))))
fail(A, "both")
#1 pair where both subjects fail
set.seed(3)
(A <- matrix(rbinom(20, 1, .5), ncol=2,
             dimnames=list(Pair=1:10, c("Treatment", "Placebo"))))
fail(A, "both")

set.seed(3)
(A <- matrix(rbinom(20, 1, .5), ncol=2,
             dimnames=list(Pair=1:10, c("Treatment", "Placebo"))))


#debug so that the "1 pair where both subjects fail" will work
#work in the global environment 
A[1,]=A[2,]=0
apply(A,1,sum)==0
sum(apply(A,1,sum)==0)

#When a function agged for debugging is entered, normal execution is suspended and the body of function is executed one statement at a time.
#At the debug prompt, Browse[2]>, the user can enter R expressions or special commands,
n Advance to the next expression
c Continue to the end of the current context
(to the end of the loop if in a loop or to the end of the function)
where Print a stack trace of all active function calls
Q Exit the browser

set.seed(1)
D <- data.frame(matrix(rnorm(1000*1000), nrow=1000))
slower <- function(D) {
  for(i in 1:ncol(D))
    D[,i] <- sqrt(abs(D[,i]))
  invisible(D)  #invisible does not return the value
}
Rprof() # Start profiling
slower(D)
Rprof(NULL) # Stop profiling
summaryRprof() # Summarize the results

set.seed(1)
D <- data.frame(matrix(rnorm(1000*1000), nrow=1000))
faster <- function(D) {
  D <- lapply(D, function(x) sqrt(abs(x)))
  invisible(D)
}
Rprof()
faster(D)
Rprof(NULL)
summaryRprof()



slow <- function(x) {
  y=0
  x.inv <- solve(x)
  d=det(x.inv)
  for(i in 1:nrow(x)) {
    y <- y + x[i,]*d
  }
  mean(y)
}
A <- matrix(rnorm(40000),nrow=200)
slow(A)

Rprof()



#The faster() function is faster not because there is no longer a loop (the functions sqrt() and abs() are called a 1000 times in both examples), but because lapply() uses C code that does not rely on the assignment function "[<-".
#lappy may or may not be fast, but apply can be slow

fail(A, "both")
traceback()

debug(bothFail)
fail(A, "both")
Browser[2]>A
Browser[2]>bfail
Browser[2]>numFail
Browser[2]>nrow(bfail)
Browser[2]>is(bfail)


# Lecture 7: object-oriented programming
head(ToothGrowth)
Is(ToothGrowth)
class(ToothGrowth)
summary(ToothGrowth)  #generic function
summary.data.frame(ToothGrowth) #


fit <- lm(len ~ dose + factor(supp), data=ToothGrowth)
class(fit)
is(fit)   #show all the classes of one method
is(ToothGrowth) #show classes

summary(fit)
summary.lm(fit)

class(fit)
methods("summary")  #to shoe different summary functions
methods(class="Date")



###application of class
odds.ratio.class <- function(X, conf.level=.95) {
  # Verify Arguments
  stopifnot(!missing(X), is.matrix(X), dim(X)==c(2,2), X>0)
  # Print warning message if any expected cell count < 5
  exp.count <- (apply(X, 1, sum) %o% apply(X, 2, sum))/sum(X)
  if(any(exp.count < 5)) warning("Expected cell count < 5")
  # Calculate odds ratio and asymptotic confidence interval
  OR <- (X[1,1]*X[2,2])/(X[1,2]*X[2,1])
  logOR.SE <- sqrt(sum(1/X))
  alpha <- 1-conf.level
  CI.lower <- exp(log(OR) - qnorm(1-alpha/2)*logOR.SE)
  CI.upper <- exp(log(OR) + qnorm(1-alpha/2)*logOR.SE)
  # Format and return results
  out <- list(OR=OR, CI=c(CI.lower, CI.upper), conf.level=conf.level)
  class(out) <- "OR"
  return(out)
}
print.OR <- function(object) {
  cat("Odds Ratio = ", object$OR, "\n", object$conf.level*100, "%
      Confidence Interval = (",object$CI[1],", ",object$CI[2],")\n", sep="")
}


X <- matrix(c(189, 104, 10845, 10933), nrow=2,
            dimnames=list("Treatment"=c("Placebo", "Aspirin"),
                          "Myocardial Infarction"=c("Yes", "No")))

odds.ratio(X)

results
names(results)
results$OR

# Matrix
diag(diag(A))   # diagonal matrix
dim(matrix)     # return dimension
nrow;ncol(matrix)
apply(m,1,mean) # 1 - row; 2 - col

# factor - with levels for model buliding

# list - combined matrix, array and factor
  * unique type of matrix with numeric and non-numeric values

# dataframe
  * all type of data


## array is 3 dimensional


#Practice
trace=function(A){
  i=1
  for (i in 1:n){
    i=i+1
    cat(i,A,"\n")
  }
  A=diag(n)
  A=A[i,i]
}

trace <- function(A) {
  out=sum(diag(A))
  class(out)=tr
  return(out)
}

print.tr=function(B){
  cat("The trace is",B,"\n")
}

#result print out the values 
#for group project, to create S3 print out functinos 

###
#Use setClass() to  define an S4 class.
setClass(Class, representation)
Class Name for the class
representation Named list of the slots and the corresponding classes;
usually a call to the representation() function

#Define the infant class,
# Define an infant class
setClass("infant", representation(sex = "character",
                                  DOB = "Date",
                                  growth = "data.frame"))
#Get information about infant,
getClass("infant")
getSlots("infant")


###New S4 object
# Infant growth "data"
# (median height and weight for boys from CDC growth charts)
age = c(0, 3, 6, 12, 18, 24, 30, 36)
wt = c( 3.53, 6.39, 8.16, 10.46, 11.80, 12.74, 13.56, 14.33)
ht = c(49.99, 62.08, 67.86, 76.11, 82.41, 87.66, 92.13, 95.45)
x <- new("infant", growth=data.frame(age, wt, ht),
         sex="male",
         DOB=as.Date("2012-01-30"))

#Use the @ sign to extract a specific slot
x@growth

#The class of the data must match the class of the corresponding slot
y <- new("infant", growth=data.frame(age, wt, ht),
         sex="male",
         DOB="2012-01-30")


###It is important that an object be valid. Slots help, but there maybe
additional requirements. To test for these requirements supply a validity
checking method to the validity argument of setClass(). This method
should return TRUE if the object is valid and a description of the
non-validity otherwise.

validity.infant <- function(object) {
  # Growth data is numeric
  if(!all(sapply(object@growth, is.numeric))) return("data not numeric")
  # Sex is "male" or "female"
  if(!(object@sex %in% c("male", "female"))) return("invalid sex")
  return(TRUE)
}
setClass("infant", representation(sex = "character",
                                  DOB = "Date",
                                  growth = "data.frame"),
         validity=validity.infant)
z <- new("infant", growth=data.frame(c("0-mon"), c(49.99), c(3.53)),
         sex="m", DOB=as.Date("2012-01-30"))


#Use setMethod() to define an S4 method.
#setMethod(f, signature, definition)
#f: Name of the generic function
#signature: Character string of the corresponding class
#definition: Function definition

##The show() method, is the default method for showing an object when its
name is typed into the console.
setMethod(f = "show", signature = "infant",
          definition = function(object) {
            cat("Sex =", object@sex,
                "\nDate-of-Birth =", format(object@DOB, "%B %d, %Y"), "\n")
            print(object@growth)
          })


#Create an infant.birthOrder class that is like infant but also includes
#the infant's birth order.
#Use the argument contains in setClass() to set a superclass, where
#contains equals a character string of the name of the class being extend.
#We only need to create a new show method for infant.BirthOrder. We
#do not need to recreate the extract or replace methods.

setClass("infant.BirthOrder", representation(BirthOrder = "numeric"),
         contains="infant")
setMethod(f = "show", signature = "infant",
          definition = function(object) {
            cat("Sex =", object@sex, "\nDate-of-Birth =",
                format(object@DOB, "%B %d, %Y"),
                "\nBirth Order =", object@BirthOrder, "\n")  
            print(object@growth)
          })
#did not specify the infant.show method, but inheretance use the method anyway

x.more <- new("infant.BirthOrder", BirthOrder=1,
              growth=data.frame(age, wt, ht),
              sex="male",
              DOB=as.Date("2012-01-30"))


#Practice for S4

x <- new("infant", growth=data.frame(wt, ht),
         sex="male",
         DOB=as.Date("2012-01-30"))

setClass("infant", representation(sex = "character",
                                  DOB = "Date",
                                  growth = "data.frame"),
         validity=validity.infant)







setMethod(f="plot", signature="infant",
          definition=function(x,y...) {
            par(mfrow=c(1,2))
            plot(x[,1],x[,2])
            plot(x[,3],x[,4])
            #plot(x@growth$age,x@growth$wt)    
            #plot(x@growth$age,x@growth$ht)
            )


matrix.n <- function(A, n) {
  A.n <- paste(rep("A", n), collapse="%*%")
  eval(parse(text=A.n)) #by using "parse" change it to unevaluated, and use eval to rerun the function
}


getMean <- function(x) {
  var <- deparse(substitute(x))
  if(!is.numeric(x)) stop(paste(var, "is not numeric"))
  cat("The mean of", var, "is", mean(x), "\n")
}



Lecture 8 Creating a package
### odds and ratio function

odds.ratio <- function(X, conf.level=.95) {
  # Verify Arguments
  stopifnot(!missing(X), is.matrix(X), dim(X)==c(2,2), X>0)
  # Print warning message if any expected cell count < 5
  exp.count <- (apply(X, 1, sum) %o% apply(X, 2, sum))/sum(X)
  if(any(exp.count < 5)) warning("Expected cell count < 5")
  # Calculate odds ratio and asymptotic confidence interval
  OR <- (X[1,1]*X[2,2])/(X[1,2]*X[2,1])
  logOR.SE <- sqrt(sum(1/X))
  alpha <- 1-conf.level
  CI.lower <- exp(log(OR) - qnorm(1-alpha/2)*logOR.SE)
  CI.upper <- exp(log(OR) + qnorm(1-alpha/2)*logOR.SE)
  # Format and return results
  out <- list(OR=OR, CI=c(CI.lower, CI.upper), conf.level=conf.level)
  class(out) <- "OR"
  return(out)
}
print.OR <- function(obj) {
  cat("Odds Ratio = ", obj$OR, "\n", obj$conf.level*100,
      "% Confidence Interval = (",obj$CI[1],", ",obj$CI[2],")\n", sep="")
}

## creating the package
package.skeleton(name="OR", code files="OR.R")

## to hide the function that are not shown to the reader bu staying in the package
.trace = function(a), sum(diag(a))

#LazyLoad, if yes delays the loading of functions until they are required.  R will not load it until otherwise specified

checking the website http://cran.r-project.org/doc/manuals/R-exts.html for assisting creating the r-package


### example 
\name{odds.ratio}
\alias{odds.ratio}    # about help file
\title{Odds Ratio}
\description{Calculate odds ratio and asymptotic  #"description section at the beginning of the package"
             confidence intervals for a 2x2 table.}
\usage{odds.ratio(X, conf.level = 0.95)}
\arguments{
  \item{X}{2x2 postive numeric matrix}
  \item{conf.level}{Numeric confidence level of the confidence interval}
}
\details{Calculate the odds ratio and asymptotic confidence intervals for a
         2x2 table. Function returns a warning message if the expected cell count in
         at least one cell is less than 5.}
\value{A list of class "OR" with the following components   # calue returned
       \item{OR}{Odds ratio}
       \item{CI}{Confidence interval}
       \item{conf.level}{Confidence level}
}
\references{Alan Agresti, Introduction to Categorical Data Analysis. Wiley, 1996.}
\author{Nicholas Christian}
\examples{          # directly executable, including the data or the R-filled dataset.  
  # Data from Agresti (1996)
  X <- matrix(c(189, 104, 10845, 10933), nrow=2,
              dimnames=list("Treatment"=c("Placebo", "Aspirin"),
                            "Myocardial Infarction"=c("Yes", "No")))
  odds.ratio(X)
}

### Building the package

Building a package on a Windows machine requires Rtools.
This software can be downloaded at,
http://cran.r-project.org/bin/windows/Rtools/
  To build the package in a computer lab you will need to follow these steps.
If you are on your own computer then you will only need the two system
commands.
# Set the library path to a writable directory (only in computer lab) - BUT DONT NEED TO USE THIS SECTION
.libPaths(new="C:/Users/njc23/R/win-library/2.15")
.libPaths()

setwd("C:/Users/njc23/Desktop")
package.skeleton(name="OR", code_files="OR.R")
# Complete description and documentation .Rd files #
# Build package, loading the already existed package
system("RCMD INSTALL --build OR")
# Check package
system("RCMD check OR")

.libPaths


### install the package from the local directory
utils:::menuInstallLocal()
library("name of the package")

methods(class='name')
OR:::trace(Y)   #For a package pkg, pkg::name returns the value of the exported variable name in namespace pkg, whereas pkg:::name returns the value of the internal variable name. The namespace will be loaded if it was not loaded before the call, but the package will not be attached to the search path.



#local varaible only exists in the function, while global varaible can be used anywhere.
##############################
# Calculate the odds ratio (OR) of a 2x2 table and the asymptotic
# confidence interval (CI) for the OR.
# Arguments: X = 2x2 matrix; conf.level = confidence level
# Output: Prints OR and CI to console and returns a list, invisibly
#------------------------------------------------------------------
odds.ratio <- function(X, conf.level=.95) {
  # Verify Arguments
  stopifnot(!missing(X), is.matrix(X), dim(X)==c(2,2), X>0)
  # Print warning message if any expected cell count < 5
  exp.count <- (apply(X, 1, sum) %o% apply(X, 2, sum))/sum(X)
  if(any(exp.count < 5)) warning("Expected cell count < 5")
  # Calculate odds ratio and asymptotic confidence interval
  OR <- (X[1,1]*X[2,2])/(X[1,2]*X[2,1])
  logOR.SE <- sqrt(sum(1/X))
  alpha <- 1-conf.level
  CI.lower <- exp(log(OR) - qnorm(1-alpha/2)*logOR.SE)
  CI.upper <- exp(log(OR) + qnorm(1-alpha/2)*logOR.SE)
  # Format and return results
  cat("Odds Ratio = ", OR, "\n",
      conf.level*100, "% Confidence Interval = (",
      CI.lower, ", ", CI.upper, ")\n", sep="")
  out <- list(OR=OR, CI=c(CI.lower, CI.upper), conf.level=conf.level)
  invisible(out)
}

#for test
odds.ratio(X=diag(2))
odds.ratio(X=matrix(10:20,3,3))




trace=function(x){
  stopifnot(is.matrix(x),nrow(x)==ncol(x))
  return(sum(diag(x)))
}


A=matrix(1:16,4)
trace(x=A)



trace=function(A){
   i=1
    for (i in 1:n){
    i=i+1
    cat(i,A,"\n")
  }
  A=diag(n)
  A=A[i,i]
}

# Verify Arguments
stopifnot(!missing(A), is.matrix(A), dim(A)==c(n,n), A>0)


#variable
#if R cannot find variable in local environment, it will look for it in global environment.


f=function(x,y=x){
  x=x+1
  return(y)
}


#create binary operators
'%x.y%' <- function(x,y) x^y
3%x.y%2


#about "..."
pass on the argument

#mapply
set mean and sd together by stating "mean=1:3, sd=1:3"

#refer to the centralized function -  switch  
apply(A,2,central,"geometic")

#set.seed() can control the range of random number


####project


#practice on dots plots on 3-dimensional level
library(rgl)

# 3D scatter plot
x <- rnorm(100)
y <- rnorm(100)
z <- 0.2*x - 0.3*y + rnorm(100, sd=0.3)
fit <- lm(z ~ x + y)
plot3d(x,y,z, type="s", col="red", size=1)
coefs <- coef(fit)
a <- coefs["x"]
b <- coefs["y"]
c <- -1
d <- coefs["(Intercept)"]
planes3d(a, b, c, d, alpha=0.5)

# heart-shaped 3D graph
library(rgl)
x <- seq(-1,1,by=0.01)
y <- seq(-1,1,by=0.01)
f <- function(x,y) {
  (-sqrt(1-x^2-(y-abs(x))^2))*cos(30*((1-x^2-(y-abs(x))^2)))
}
z <- outer(x, y, f)
persp3d(x, y, z,col='red', alpha=0.3,aspect=c(1,1,0.5))

set.seed(1)
D <- data.frame(matrix(rnorm(1000*1000), nrow=1000))
set.seed(1)
M <- matrix(rnorm(1000*1000), nrow=1000)
system.time(D[50,] <- sqrt(abs(D[50,])) + rnorm(1000))
system.time(M[50,] <- sqrt(abs(M[50,])) + rnorm(1000))



slow <- function(x) {
  y=0
  for(i in 1:nrow(x)) {
    x.inv <- solve(x)
    y <- y + x[i,]*det(x.inv)
  }
  mean(y)
}
A <- matrix(rnorm(40000),nrow=200)
slow(A)





####homework 2
1. []and loop
2. functions

4. graphs

#how to read txt with tabbed dataset

read.table(file.choose(),header=TRUE,sep="\t")
lm(Rating~B-Year)

### Hist 1
# Read values from tab-delimited autos.dat, WITHOUT DATA
autos_data <- read.table("C:/R/autos.dat", header=T, sep="\t")

# Graph autos with adjacent bars using rainbow colors
barplot(as.matrix(autos_data), main="Autos", ylab= "Total",
        beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("Mon","Tue","Wed","Thu","Fri"), cex=0.6, 
       bty="n", fill=rainbow(5))

### Hist 2
# Read values from tab-delimited autos.dat WITHOUT DATASET
autos_data <- read.table("C:/R/autos.dat", header=T, sep="\t") 

# Expand right side of clipping rect to make room for the legend
par(xpd=T, mar=par()$mar+c(0,0,0,4))

# Graph autos (transposing the matrix) using heat colors,  
# put 10% of the space between each bar, and make labels  
# smaller with horizontal y-axis labels
barplot(t(autos_data), main="Autos", ylab="Total", 
        col=heat.colors(3), space=0.1, cex.axis=0.8, las=1,
        names.arg=c("Mon","Tue","Wed","Thu","Fri"), cex=0.8) 

# Place the legend at (6,30) using heat colors
legend(6, 30, names(autos_data), cex=0.8, fill=heat.colors(3));

# Restore default clipping rect
par(mar=c(5, 4, 4, 2) + 0.1)


## Hist 3
# Read values from tab-delimited autos.dat
autos_data <- read.table("C:/R/autos.dat", header=T, sep="\t")

# Concatenate the three vectors
autos <- c(autos_data$cars, autos_data$trucks, 
           autos_data$suvs)

# Compute the largest y value used in the autos
max_num <- max(autos)

# Create a histogram for autos with fire colors, set breaks
# so each number is in its own group, make x axis range from
# 0-max_num, disable right-closing of cell intervals, set
# heading, and make y-axis labels horizontal
hist(autos, col=heat.colors(max_num), breaks=max_num, 
     xlim=c(0,max_num), right=F, main="Autos Histogram", las=1)

### Hist 4
# Read values from tab-delimited autos.dat
autos_data <- read.table("C:/R/autos.dat", header=T, sep="\t")

# Concatenate the three vectors
autos <- c(autos_data$cars, autos_data$trucks, 
           autos_data$suvs)

# Compute the largest y value used in the autos
max_num <- max(autos)

# Create uneven breaks
brk <- c(0,3,4,5,6,10,16)

# Create a histogram for autos with fire colors, set uneven
# breaks, make x axis range from 0-max_num, disable right-
# closing of cell intervals, set heading, make y-axis labels 
# horizontal, make axis labels smaller, make areas of each
# column proportional to the count
hist(autos, col=heat.colors(length(brk)), breaks=brk, 
     xlim=c(0,max_num), right=F, main="Probability Density", 
     las=1, cex.axis=0.8, freq=F)

### Pie without DATA
# Define cars vector with 5 values
cars <- c(1, 3, 6, 4, 9)

# Define some colors ideal for black & white print
colors <- c("white","grey70","grey90","grey50","black")

# Calculate the percentage for each day, rounded to one 
# decimal place
car_labels <- round(cars/sum(cars) * 100, 1)

# Concatenate a '%' char after each value
car_labels <- paste(car_labels, "%", sep="")

# Create a pie chart with defined heading and custom colors
# and labels
pie(cars, main="Cars", col=colors, labels=car_labels,
    cex=0.8)

# Create a legend at the right   
legend(1.5, 0.5, c("Mon","Tue","Wed","Thu","Fri"), cex=0.8, 
       fill=colors)

*****************************************
******R in Action by Robert Kabacoff*****
*****************************************
#generate data set table
mydata <- data.frame(age=numeric(0),
    gender=character(0), weight=numeric(0))
mydata <- edit(mydata)
  
mydataframe <- read.table("test", header=logical_value,
                          sep="delimiter", row.names="name")


# transfer values into a different variable
wtcat=ifelse(mtcars$wt<3.3,1,2)
# or
mtcars$wtcat[mtcars$wt<3.3]=1
mtcars$wtcat[mtcars$wt>=3.3]=2

# factor
wtcat=factor(wtcat,
             levels=c(1,2),
             labels=c("light","heavy"))
as.numeric(wtcat)   #show the original values

## create a pdf about the graph
pdf("mygraph.pdf")
attach(mtcars)
lines(x, z, type="b", pch=22, col="blue", lty=2)plot(wt, mpg)
abline(lm(mpg~wt))
title("Regression of MPG on Weight")
detach(mtcars)
dev.off()
  
## example of customizing axes
x <- c(1:10)
y <- x
z <- 10/x
opar <- par(no.readonly=TRUE)
par(mar=c(5, 4, 4, 8) + 0.1)
plot(x, y, type="b",
     pch=21, col="red",
     yaxt="n", lty=3, ann=FALSE)
lines(x, z, type="b", pch=22, col="blue", lty=2)
axis(2, at=x, labels=x, col.axis="red", las=2)
axis(4, at=z, labels=round(z, digits=2),
     col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
mtext("y=1/x", side=4, line=3, cex.lab=1, las=2, col="blue")
title("An Example of Creative Axes",
      xlab="X values",
      ylab="Y=X")
par(opar)

###reference lines
abline(h=yvalues, v=xvalues)
abline(h=c(1,5,7))
abline(v=seq(1, 10, 2), lty=2, col="blue")

##legend
legend(location, title, legend, ...)


#### example of graph
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
opar <- par(no.readonly=TRUE)
par(lwd=2, cex=1.5, font.lab=2)
plot(dose, drugA, type="b",
     pch=15, lty=1, col="red", ylim=c(0, 60),
     main="Drug A vs. Drug B",
     xlab="Drug Dosage", ylab="Drug Response")
lines(dose, drugB, type="b",
      pch=17, lty=2, col="blue")
abline(h=c(30), lwd=1.5, lty=2, col="gray")
library(Hmisc)
minor.tick(nx=3, ny=3, tick.ratio=0.5)
legend("topleft", inset=.05, title="Drug Type", c("A","B"),
       lty=c(1, 2), pch=c(15, 17), col=c("red", "blue"))
par(opar)

# plots
attach(mtcars)
plot(wt, mpg,
     main="Mileage vs. Car Weight",
     xlab="Weight", ylab="Mileage",
     pch=18, col="blue")
text(wt, mpg,
     row.names(mtcars),
     cex=0.6, pos=4, col="red")
detach(mtcars)


#combining graphs: hist
attach(mtcars)
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
plot(wt,mpg, main="Scatterplot of wt vs. mpg")
plot(wt,disp, main="Scatterplot of wt vs disp")
hist(wt, main="Histogram of wt")
boxplot(wt, main="Boxplot of wt")
par(opar)
detach(mtcars)

attach(mtcars)
opar <- par(no.readonly=TRUE)
par(mfrow=c(3,1))
hist(wt)
hist(mpg)
hist(disp)
par(opar)
detach(mtcars)


####data management
mortality = read.table("C:/Users/froster/Desktop/mortality.txt", header=T, sep="\t")

manager <- c(1, 2, 3, 4, 5)
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
country <- c("US", "US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)
l <- data.frame(manager, date, country, gender, age,
                         q1, q2, q3, q4, q5, stringsAsFactors=FALSE)

#recode values
l$age[l$age == 99] <- NA
l$agecat[l$age >= 35 & l$age <= 60] <- "old"
l$agecat[l$age < 35] <- "Young"

l$rating=l$q1+l$q2+l$q3+l$q4
      #or
x=c(l$q1,l$q2,l$q3,l$q4)
l$rating=apply(x,1,sum,na.rm=T)
        #or try loop 1:ncol;apply sum i;

l1=l[order(age, gender)]
  
#checking missing values
is.na(l)

#drop values
l[a,b]=NULL

#subset - selecting certain portion of data
newdata <- subset(l, age >= 35 | age < 24,
                  select=c(q1, q2, q3, q4))

subset(dataframe,sex="M") # extrapolate subset of dataset
complete.cases    # check complete cases
na.omit(values)   # erase values 
tcrossprod()      # reverse natrx


# read and write
library(ISLR)
data(dataset)
dim(dataset)
head(dataset)
View(dataset)
str(dataset)    # structure of dataset, descriptive of all levels

read.table(file = "path/file.type",header= T)

leadership = read.table("C:/Users/froster/Desktop/data1.txt", header=T, sep="")#or sep="\t"
l=as.vector(l)
l2=as.Date(l, "%m/%d/%y")
which(l2>"2008-10-25")
leadership=within(leadership,{
  agecat=NA
  agecat[Age>40]="old"
  agecat[Age<=40]="young"
})

#time difference 
difftime(today, dob, units="weeks")


###try locating the values of leadership
l2=as.matrix(leadership)
which(l2==99)
#coding missing values
l2[l2==99]=na

#selecting observations
newdata <- leadership[1:3,]
newdata <- leadership[which(leadership$gender=="M" &
                              leadership$age > 30),]
attach(leadership)
newdata <- leadership[which(gender=='M' & age > 30),]
detach(leadership)


# open "grade" table
grade=read.table("C:/Users/froster/Desktop/grade.txt", header=T, sep="")
grade[,1]
# standardized composite score z
z=apply(scale(grade[,2:4]),1,mean)
# new grade table
grade2=cbind(grade,z)
# creating quantile for score
y <- quantile(z, c(.8,.6,.4,.2))

within(grade2,{
  rank=NULL
  rank[z >= y[1]] <- "A"
  rank[z < y[1] & z >= y[2]] <- "B"
  rank[z < y[2] & z >= y[3]] <- "C"
  rank[z < y[3] & z >= y[4]] <- "D"
  rank[z < y[4]] <- "F"
})

# standardizing data
#By default, the scale() function standardizes the specified columns of a matrix or
#data frame to a mean of 0 and a standard deviation of 1

x <- pretty(c(-3,3), 30)
y <- dnorm(x)
plot(x, y,
     type = "l",
     xlab = "Normal Deviate",
     ylab = "Density",
     yaxs = "i"
)

#d = density
#p = distribution function
#q = quantile function
#r = random generation (random deviates)

#What is the value of the 90th percentile of a normaldistribution with a mean of 500 and a standard deviation of 100?
qnorm(.9, mean=500, sd=100)

#Generate 50 random normal deviates with a mean of 50 and a standard deviation of 10.
rnorm(50, mean=50, sd=10)

# creating a list of variable header and summary
vars <- c("mpg", "hp", "wt")
head(mtcars[vars])
summary(mtcars[vars])

# The generic function is.na indicates which elements are missing.

# paste function turns the vector into separate strings by comma; is.na indicates if there is null value
is.na(paste(c(1, NA))) 
is.na(c(1,NA))

mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}
sapply(mtcars[vars], mystats)

# na.omit can eliminate the observation in a dataframe, but cannot eliminate the observation from a matrix
DF <- data.frame(x = c(1, 2, 3), y = c(0, 10, NA))
na.omit(DF)     # eliminate the row with na values

m <- as.matrix(DF)
na.omit(m)
stopifnot(all(na.omit(1:3) == 1:3))  # does not affect objects with no NA's
try(na.fail(DF))#> Error: missing values in ..


within(grade{
  if (na.omit)
    q4=q4[!is.na(q4)]
  })

################################
############################The End of R in Action######################################
###############################

# a solution to the learning example
options(digits=2)
Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose",
             "David Jones", "Janice Markhammer", "Cheryl Cushing",
             "Reuven Ytzrhak", "Greg Knox", "Joel England",
             "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster <- data.frame(Student, Math, Science, English,
                     stringsAsFactors=FALSE)
z <- scale(roster[,2:4])
score <- apply(z, 1, mean)
roster <- cbind(roster, score)

y <- quantile(score, c(.8,.6,.4,.2))
roster$grade[score >= y[1]] <- "A"
roster$grade[score < y[1] & score >= y[2]] <- "B"
roster$grade[score < y[2] & score >= y[3]] <- "C"
roster$grade[score < y[3] & score >= y[4]] <- "D"
roster$grade[score < y[4]] <- "F"
name <- strsplit((roster$Student), " ")
lastname <- sapply(name, "[", 2)
firstname <- sapply(name, "[", 1)
roster <- cbind(firstname,lastname, roster[,-1])
roster <- roster[order(lastname,firstname),]


# switch
feelings <- c("sad", "afraid")
for (i in feelings)
  print(
    switch(i,
           happy = "I am glad you are happy",
           afraid = "There is nothing to fear",
           sad = "Cheer up",
           angry = "Calm down now"
    )
  )


##aggregating data
options(digits=3)
attach(mtcars)
aggdata <-aggregate(mtcars, by=list(cyl,gear), FUN=mean, na.rm=TRUE)


#### Methods#####
##PRINT
print.edfault(object)
print(unclass(object))    
str(object)
names(object)

# getting the code from generic functions
getS3method('median', 'default')
methods(print)  # for generic function
methods(class='factor') # for classes

### Object Oriented Programming
ls()    # listing current running objects
ls(pattern="am")  # searching objects with string of "am"
rm()    # for removing objects by specifying the names
rm(list=ls())
rm(list=ls(pattern="am")) # removing objects which names contain the string "am"
save();load() # for saving and loading objects
attributes(mtcars)  # providing names on rows and columns, and class regarding the object


u=matrix(c(1,2,3,1,2,4),3,2)
v=matrix(c(8,12,20,15,10,2),3,2)

for (m in c("u","v")) {    
  z <- get(m)             # get() accessing an object via strings
  print(lm(z[,2] ~ z[,1]))
}


j <- list(name="Joe", salary=55000, union=T)
class(j) <- "employee"    # the class can be changed
attributes(j) # let's check

# a generic function for printing the requested class
print.employee <- function(wrkr) {
  cat(wrkr$name,"\n")
  cat("salary",wrkr$salary,"\n")
  cat("union member",wrkr$union,"\n")
}

unclass() # show default print function regardless of the specified class- values on all levels of factors
j[1]   # show first level with name and value
j[[1]] # only show the value on the first level

#S3 function defines the class of objects for specific use of function
sum1toi <- function(i) return(i*(i+1)/2)
ut <- function(inmat) {
  nr <- nrow(inmat)
  rtrn <- list()
  class(rtrn) <- "ut"
  rtrn$mat <- vector(length=sum1toi(nr))
  rtrn$ix <- sum1toi(0:(nr-1)) + 1
  for (i in 1:nr) {
    ixi <- rtrn$ix[i]
    # copy the i-th column of inmat to mat
    rtrn$mat[ixi:(ixi+i-1)] <- inmat[1:i,i]
  }
  return(rtrn)
}

#S4 classes and methods
setClass("employee",
         representation(
           name="character",
           salary="numeric",
           union="logical")
)

# create a new object under the class "employee"
joe <- new("employee",name="Joe",salary=55000,union=T)
# use @ to call the slot's value, such as joe@salary

# "show(joe)" is equal to "joe"

setMethod("show", "employee",
          function(object) {
            inorout <- ifelse(object@union,"is","is not")
            cat(object@name,"has a salary of",object@salary,
                "and",inorout, "in the union", "\n")
          }
)

methods()   # to see all of this fmaily 
unclass()   # coverts a class object to an ordinary list


 grep(): Searches for a substring, like the Linux command of the same name.
 nchar(): Finds the length of a string.
 paste(): Assembles a string from parts.

paste(1:12) # same as as.character(1:12)
paste("A", 1:6, sep = "")     # no deliminator
stopifnot(identical(paste ("A", 1:6, sep = ""),
                    paste0("A", 1:6)))
paste("Today is", date())

 sprintf(): Assembles a string from parts.
 substr(): Extracts a substring.
 strsplit(): Splits a string into substrings.





########
#importing data from EXCEL  
install.packages("RODBC")
library(RODBC)
channel <- odbcConnectExcel("myfile.xls")
mydataframe <- sqlFetch(channel, "mysheet")
odbcClose(channel)

library(xlsx)
workbook <- "c:/myworkbook.xlsx"
mydataframe <- read.xlsx(workbook, 1)



dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
plot(dose, drugA, type="b") # type "b" means both line and dots
plot(dose, drugA, type="b", lty=2, pch=17)#pick line and spot types
data.frame(dose, drugA,drugB) #build up data set table

dim1 <- c("A1", "A2")
dim2 <- c("B1", "B2", "B3")
dim3 <- c("C1", "C2", "C3", "C4")
z <- array(1:24, c(2, 3, 4), dimnames=list(dim1, dim2, dim3))

patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(patientID, age, diabetes, status)
patientdata


attach(mtcars) #attach function avoids using $ sign
summary(mpg)
plot(mpg, disp)
plot(mpg, wt);abline(lm(mpg~wt))
detach(mtcars)

with(mtcars, {
  summary(mpg, disp, wt)
  plot(mpg, disp)
  plot(mpg, wt)
})

#If you need to create objects that will exist outside of the with() construct, use the
#special assignment operator <<- instead of the standard one (<-). It will save the object
#to the global environment outside of the with() call. This can be demonstrated with
#the following code:
  > with(mtcars, {
    nokeepstats <- summary(mpg)
    keepstats <<- summary(mpg)
  })


status <- factor(c(3,2,1,3), order=TRUE,
                 levels=c("Poor", "Improved", "Excellent"))

x <- c(1, 2, NA, 3)
y <- sum(x, na.rm=TRUE) #y=6 missing data is removed from the vector

newdata <- na.omit(leadership)#delete the row with missing data

cbind(A,B)#A and B both are dataframes
total <- merge(dataframeA, dataframeB, by=c("ID","Country")#merging dataframe by ID and country
               
subset(leadership, age >= 35 | age < 24,select=c(q1, q2, q3, q4))#select variables and observations
subset(leadership, gender=="M" & age > 25,select=gender:q4)               

               
mysample <- leadership[sample(1:nrow(leadership), 3, replace=FALSE),]# random sample
               

require(graphics)

x <- seq(-4, 4, len = 101)
y <- cbind(sin(x), cos(x))
matplot(x, y, type = "l", xaxt = "n",
       main = expression(paste(plain(sin) * phi, "  and  ",
                               plain(cos) * phi)),
       ylab = expression("sin" * phi, "cos" * phi), # only 1st is taken
       xlab = expression(paste("Phase Angle ", phi)),
       col.main = "blue")
axis(1, at = c(-pi, -pi/2, 0, pi/2, pi),
    labels = expression(-pi, -pi/2, 0, pi/2, pi))


## How to combine "math" and numeric variables :
plot(1:10, type="n", xlab="", ylab="", main = "plot math & numbers")
theta <- 1.23 ; mtext(bquote(hat(theta) == .(theta)), line= .25)
for(i in 2:9)
 text(i, i+1, substitute(list(xi, eta) == group("(",list(x,y),")"),
                         list(x = i, y = i+1)))
## note that both of these use calls rather than expressions.
##
text(1, 10,  "Derivatives:", adj = 0)
text(1, 9.6, expression(
 "             first: {f * minute}(x) " == {f * minute}(x)), adj = 0)
text(1, 9.0, expression(
 "     second: {f * second}(x) "        == {f * second}(x)), adj = 0)


plot(1:10, 1:10)
text(4, 9, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))
text(4, 8.4, "expression(hat(beta) == (X^t * X)^{-1} * X^t * y)",
    cex = .8)
text(4, 7, expression(bar(x) == sum(frac(x[i], n), i==1, n)))
text(4, 6.4, "expression(bar(x) == sum(frac(x[i], n), i==1, n))",
    cex = .8)
text(8, 5, expression(paste(frac(1, sigma*sqrt(2*pi)), " ",
                           plain(e)^{frac(-(x-mu)^2, 2*sigma^2)})),
    cex = 1.2)

## some other useful symbols
plot.new(); plot.window(c(0,4), c(15,1))
text(1, 1, "universal", adj = 0); text(2.5, 1,  "\\042")
text(3, 1, expression(symbol("\042")))
text(1, 2, "existential", adj = 0); text(2.5, 2,  "\\044")
text(3, 2, expression(symbol("\044")))
text(1, 3, "suchthat", adj = 0); text(2.5, 3,  "\\047")
text(3, 3, expression(symbol("\047")))
text(1, 4, "therefore", adj = 0); text(2.5, 4,  "\\134")
text(3, 4, expression(symbol("\134")))
text(1, 5, "perpendicular", adj = 0); text(2.5, 5,  "\\136")
text(3, 5, expression(symbol("\136")))
text(1, 6, "circlemultiply", adj = 0); text(2.5, 6,  "\\304")
text(3, 6, expression(symbol("\304")))
text(1, 7, "circleplus", adj = 0); text(2.5, 7,  "\\305")
text(3, 7, expression(symbol("\305")))
text(1, 8, "emptyset", adj = 0); text(2.5, 8,  "\\306")
text(3, 8, expression(symbol("\306")))
text(1, 9, "angle", adj = 0); text(2.5, 9,  "\\320")
text(3, 9, expression(symbol("\320")))
text(1, 10, "leftangle", adj = 0); text(2.5, 10,  "\\341")
text(3, 10, expression(symbol("\341")))
text(1, 11, "rightangle", adj = 0); text(2.5, 11,  "\\361")
text(3, 11, expression(symbol("\361")))               

)
#############################
###     The R Book   ########
#############################

#### Basics

# returns a objects that include the searched characters
apropos("lm")

#show demo regarding functions
demo(persp)

#show varaible names
names(mtcars)

# regression model
attach(mtcars)
model=lm(mpg~hp+wt)
detach(mtcars)
summary(model)

# show varaibles
objects()
# show libraries and dataframes
search()

#### Essentials of the R language
# logs to the base 10
log10(6)

#1.2e-3 = 0.0012
#3.9+4.5i

is.finite(10)

# Missing value NA
x=c(1:8,NA)

# name elements within vectors
counts=c(8:1)
names(counts)=1:8
as.vector(counts)  # show the vectors w/o names

# cumsum(x);cumprod(x);pmin(v);colMeans(x);colSums(x);rowMeams...
# cummax/cummin(x)  # replace all vectors with the maximum/minimum one
#pmin(x,y,z) -  if x, y, and z are the same length of vectors, pmin can find parallel minimum points on each column among three vectors

# geting the mean growth rate for each detergent
tapply(growthrate, detergent, mean)
# median growth rate for water type and daphnia clone
tapply(growthrate, list(water,daphnia),median)

# avoid attach(), use with()
with(OrchardSprays,boxplot(decrease~treatment))

# single square brackets are used in vectors, matrices, arrays and dataframes; double braket 

sum(x<5) # calculate vectors in which numbers are lower than 5
1*(x<5) # the first 4 numbers are 1 while the rest are 0.

sum(x*(x<5))

# sum the three biggest numbers
x=c(2,3,6,3,1,4,8,9,5,11,2,2,9,4,12,13,2,3,5,9,7)
sum(rev(sort(x))[1:3])

# count number of numbers that are larger than 8
length(x[x>8])

# to extract every nth element from a long vector we can use seq as in index.  in this case i want every 25th value in a 1000-long vector of normal random numbers with mean value 100 and standard deviation 10
xv=rnorm(1000,100,10)
xv[seq(25,length(xv),25)]   # from 25 to 1000, with interval of 25


# find the cloest one to 108
which(abs(xv-108)==min(abs(xv-108)))

# trimming vectors using negative subscripts
x[-5]
mean(sort(x)[-c(1,length(x))])  # calculate the mean without the highest and lowest scores

# obtain how many numbers between interval
floor(50/7)

# eliminate the numbers that can be divided by 7 from the vector
vec[-1:50*(1:50%%7==0)]

# logical table
x=c(NA,FALSE,TRUE)
names(x)=x
outer(x,x,"&")
outer(x,x,"|")

# regular sequence of numbers
.3:3  # interval is set default as 1
seq(0,1.5,0.2)  # from 0 to 1.5 with interval of 0.2
seq(c(5,2,4)) # seq(5);seq(2);seq(4)

# sample function
x=c(2,3,6,3,1,4,8,9,5,11,2,2,9,4,12,13,2,3,5,9,7)
rbind(sample(x),sample(x))  # random sampling function

# matrices
a=diag(3)
class(a);attributes(a)  # show class and dimension

b=matrix(1:8,4,2)
dim(b)=c(2,4) #change the matrix from 4X2 to 2X4 with the same sequence
t(b)  # transpose b


# row and column names
x=matrix(rpois(20,1.5),nrow=4)
rownames(x)=rownames(x,do.NULL=FALSE,prefix="Trial.")
drug.names=c("aspirin","paracetamol","nurofen","hedex","placebo")
colnames(x)=drug.names

dimnames(x)=list(NULL,paste("drug.",1:5,sep=""))

mean(x[,5]) # mean of the 5th column
var(x[4,])  #variance of the 4th row
rowSums(x);colSums(x);rowMeans(x);colMeans(x)
apply(x,2,mean) # all columns sums

colnames(x)=c(1:5,"variance")
rownames(x)=c(1:4,"mean")

# sweep function: use a vector to substitute the whole matrix
cols=apply(data,2,mean)
sweep(data,2,cols)  # sweep by columns, with each row the same vector

a=matrix(rep(1:10,5),10,5)
b=rep(5,5)
a-b
c=sweep(a,2,b)

#transpose
t(apply(c,1,function(x)1:10))


# array  - 3 dimentional
a=1:25
dim(a)=c(5,5)


a=letters[1:24]
dim(a)=c(4,2,3)

a[3,,,drop=F]   # drop function retains all 3 dimensions, otherwise only the values in the 1st dim
cs=c(2,1,2)
ts=c(1,2,3)
sapply(1:3,function(i)a[,cs[i],ts[i]])  # create a 4X3 table

# Character strings
a="abc";b="123"
as.numeric(a)
as.numeric(b)
mode(a) # character

pets=c("cat","dog","gerbil","terrapin")
length(pets)
nchar(pets)
class(pets) # character

c(a,b)  # two string factors
paste(a,b,sep="")  # one string 
paste(a,b)  # one string with a space between a and b

d=c(a,b,"new")
e=paste(d,"a long phrase containing blanks")

phrase="the quick brown fox jumps over the lazy dog"
# extract substringfrom a word
q=character(20)   # empty slots for letters
for (i in 1:20) q[i]=substr(phrase,1,i)

# split words into characters
strsplit(phrase,split=character(0))
# counting the occurance of each character
table(strsplit(phrase,split=character(0)))  # 8 blanks
# thus, the number of words should be 1+blanks
words=1+table(strsplit(phrase,split=character(0))) [1]
# split by some specified word
strsplit(phrase,"the")
# extract the words from the 1st list and its 2nd word string, and counting number of characters
strsplit(phrase,"the")[[1]][2]; nchar(strsplit(phrase,"the")[[1]][2])

# switch to upper or lower cases
tolower(toupper(phrase))

first=c(5,8,3,5,3,6,4,4,2,8,8,8,4,4,6)
second=c(8,6,4,2)

# match
match(first, second)  # for values only

drug=c("A","B")
drug[1+is.na(match(first,second))]

# variance: the sim of the squares of the difference between the data and the arithmetic mean
var()
# standard errors
se=function(x)sqrt(var(x)/length(x))

ci95=function(x){
  t.value<- qt(0.975,length(x)-1)
  standard.error<-se(x)
  ci<-t.value*standard.error
  cat("95% Confidence Interval = ", mean(x) -ci, "to ", mean(x) +ci,"\n") }

xv<-rnorm(30)
sem<-numeric(30)
sem[1]<-NA
for(i in 2:30) sem[i]<-se(xv[1:i])
plot(1:30,sem,ylim=c(0,0.8),
ylab="standard error of mean",xlab="sample size n",pch=16)

lines(2:30,1/sqrt(2:30))

# error bars
error.bars<-function(yv,z,nn){
xv<-
barplot(yv,ylim=c(0,(max(yv)+max(z))),names=nn,ylab=deparse(substitute(yv)
))
g=(max(xv)-min(xv))/50
for (i in 1:length(xv)) {
lines(c(xv[i],xv[i]),c(yv[i]+z[i],yv[i]-z[i]))
lines(c(xv[i]-g,xv[i]+g),c(yv[i]+z[i], yv[i]+z[i]))
lines(c(xv[i]-g,xv[i]+g),c(yv[i]-z[i], yv[i]-z[i]))
}}

se<-rep(28.75,5)
labels<-as.character(levels(clipping))
ybar<-as.vector(tapply(biomass,clipping,mean))
error.bars(ybar,se,labels)

# error bars 2
xy.error.bars<-function (x,y,xbar,ybar){
plot(x, y, pch=16, ylim=c(min(y-ybar),max(y+ybar)),
xlim=c(min(x-xbar),max(x+xbar)))
arrows(x, y-ybar, x, y+ybar, code=3, angle=90, length=0.1)
arrows(x-xbar, y, x+xbar, y, code=3, angle=90, length=0.1) }

x <- rnorm(10,25,5)
y <- rnorm(10,100,20)
xb <- runif(10)*5
yb <- runif(10)*20
xy.error.bars(x,y,xb,yb)

# loops and repeat
for (i in 1:5) print(i�?????????2) #

# factorial x
fac1<-function(x) {
  f <- 1
  if (x<2) return (1)
  for (i in 2:x) {
  f <- f*i
  f }}

sapply(0:5,fac1)

fac2<-function(x) {
f <- 1
t <- x
while(t>1) {
f <- f*t
t <- t-1 }
return(f) }

fac3<-function(x) {
f <- 1
t <- x
repeat {
if (t<2) break
f <- f*t
t <- t-1 }
return(f) }

for (i in 1:length(y)) { if(y[i] < 0) y[i] <- 0 }
y [y<0] <- 0

z <- ifelse (y < 0, -1, 1)

ifelse(Area>median(Area),"big","small")

# chartplot
charplot<-function(x,y,pc=16,co="red"){   # 3rd-font; 4th-color
plot(y~x,pch=pc,col=co)}

charplot(1:10,1:10)
charplot(1:10,1:10,17)
charplot(1:10,1:10,co="navy") # navy 16 of font size
charplot(1:10,1:10,15,"green")# green square
charplot(1:10,1:10,co="green",pc=15)  # This produces solid green squares despite the arguments being out of sequence.

##Some applications are much more straightforward if the number of arguments does not need
to be specified in advance. There is a special formal name ... (triple dot) which is used in
the argument list to specify that an arbitrary number of arguments are to be passed to the
function. Here is a function that takes any number of vectors and calculates their means and
variances:
  
many.means <- function (...)  {
  data <- list(...) 
  n<- length(data)
  means <- numeric(n)
  vars <- numeric(n)
  for (i in 1:n) {
    means[i]<-mean(data[[i]])
    vars[i]<-var(data[[i]])
  }
  print(means)
  print(vars)
  invisible(NULL)
}

x<-rnorm(100)
y<-rnorm(200)
z<-rnorm(300)
many.means(x,y,z)

# assign x=0:7 and y=1 to the function
(function(x,y){ z <- 2*x^2 + y^2; x+y+z })(0:7, 1)

# flexible handling of arguments to functions
plotx2 <- function (x, y=z^2) {
z<-1:x
plot(z,y,type="l") }

# alternative way
plot3=function(x,y=x^2){
  plot(x,y,type="l")
}

plot3(x=0:7)

par(mfrow=c(1,2))
plotx2(12)
plotx2(12,1:12)

# apply (the answer produced by apply is a vector rather than a matrix) and sapply
X<-matrix(1:24,nrow=4)
apply(X,1,sum)  # margin 1 being the rows and margin 2 the columns
apply(X,1,function(x) x^2+x) # with a function to calculate the row margin
sapply(3:7, seq)  # generate a list of sequences from 1:3 up to 1:7

sumsq <- function(a,xv=x,yv=y)
{ yf <- exp(-a*xv)
sum((yv-yf)^2) }

lm(log(y)~x)

#lapply and lists: lapply applied a specified function to each of the elements of a list in turn
a<-c("a","b","c","d")
b<-c(1,2,3,4,4,3,2,1)
c<-c(T,T,F)
list.object<-list(a,b,c)  # create a list including 3 objects
class(list.object)
lapply(list.object,class) # show class of each object in the list
lapply(list.object,mean)

# run length encoding function
poisson<-rpois(150,0.7) # 150 random numbers with mean of 0.7
# We can do our own run length encoding on the vector by eye: there is a run of two 1s,
#then a run of two 0s, then a single 2, then a single 1, then a single 0, and so on. So the run
#lengths are 2, 2, 1, 1, 1, 1,......the values associated with these runs were 1, 0, 2, 1, 0,
#1,...... Here is the output from rle:
rle(poisson)  # run length encoding - length (number of the same repeated figures), values (figures)
max(rle(poisson)[[1]])  # to show the longest run of numbers; showing the first row 'lengths'
which(rle(poisson)[[1]]==7)
rle(poisson)[[2]][55]   # show which number with the longest run

# writting an Excel readable file from R
write.table(data,"clipboard",sep="\t",col.names=NA) # the table is now in the clipboard

# union function for merging strings
setA<-c("a", "b", "c", "d", "e")
setB<-c("d", "e", "f", "g")
union(setA,setB)  # "a" "b" "c" "d" "e" "f" "g"
intersect(setA,setB) # "d" "e"
setdiff(setA,setB)  # "a" "b" "c"
setdiff(setB,setA)  # "f" "g"
all(c(setdiff(setA,setB),intersect(setA,setB),setdiff(setB,setA))==union(setA,setB))# true
setA %in% setB  # FALSE FALSE FALSE TRUE TRUE
setB %in% setA  # TRUE TRUE FALSE FALSE
setA[setA %in% setB]==intersect(setA,setB)

# Pattern matching
wf<-read.table("c:\\temp\\worldfloras.txt",header=T)
attach(wf)
names(wf)

as.vector(Country[grep("R",as.character(Country))])  
"R" # search to countries whose first name contains "R"
"^R"  # search countries whose first name begins with R
" R"  # search its second name begins with R 
" " # two words
"[C-E]" # names including words that begins with C
"^[C-E]"  # first word begins with C,D,or E
-grep("[a-t]$"...) # do not end with a letter between a and t
-grep("[A-T a-t]$"...) # exclude ending with a letter between a and t for both upper and lower case letters
"^.y"   # with y as the second letter
"^. {,4}$" # (5 ‘anythings�????????? is shown by �?????????.�????????? then curly brackets {5} then y). Which are the countries with4 or fewer letters in their names?
"^. {15, }$"  # 15 or more characters

## Substituting text within character strings
text <- c("arm","leg","head", "foot","hand", "hindleg", "elbow")
gsub("h","H",text)  # sub for anything 
sub("h","H",text)  # sub for once only
gsub("^.","O",text) # sub the first char of each word with 'O'
gsub("(\\w)(\\w*)", "\\U\\1\\L\\2",text, perl=TRUE) # first character capitalized
gsub("(\\w*)", "\\U\\1",text, perl=TRUE)  # all capitalized

text=c("arm", "leg", "head", "foot","hand", "hindleg", "elbow")
regexpr("o",text) # demonstrate the position of 'o' in each string and whether a word include the targeted letter
grep("o",text)  # which words have 'o'
text[grep("o",text)]  # what are the words having 'o'

unlist # put dataframe into a table form

freq<-as.vector(unlist (lapply(gregexpr("o",text),length)))
present<-ifelse(regexpr("o",text)<0,0,1)  # if a string hasnt a 'o', its value is 0 otherwise 1
freq*present

stock<-c(’car�?????????,’van�?????????)
requests<-c(’truck�?????????,’suv�?????????,’van�?????????,’sports�?????????,’car�?????????,’waggon�?????????,’car�?????????)
which(requests %in% stock);requests [which(requests %in% stock)]
which(sapply(requests, "%in%", stock))  # show both the wrods and their positions

#######160105 (http://v.youku.com/v_show/id_XNjYyNzczMTgw.html?from=s1.8-1-1.2) 
c(1:6)>c(6:1);c(1:6)>c(7,1) # vector comparison
exp(c(1:4)) # vector calculation
# "alt" + "-" = " <- "
y[1:3];y[-4] # take first 3 elements from vector
y>1; y[y>1] # values
people <- data.frame(city,age,gender);people[people$age,] # show the selected cases in dataframe
class(mylist,people) # output dataframe and list
attributes(people) # show attributes of an object
str(people) #show variables and values 

# visual functions: graphics, lattice, ggplot2
plot(cars$dist~cars$speed) # dist on y and speed on x axis
stripplot(~sepal.length | species, data-iris.layout=c(1,3)) # display length and categorised by species
denstityplot(~sepal.length, groups=species, data=irs.plot.points=False) # display distributions by species in one graph
xyplot(sepal.width~sepal.length, group=spcies, data=iris) 3 scatter plot with grouping variables and width on y axis
splom(iris[1:4]) # scatter plots in matrix form

p <- ggplot(data=mpg,mapping=aes(x=cty,y=hwy,colour=factor(year)))+stat_smooth()+scale_color_manual(values=c('blue2','red4'))
p <- p+geom_point(); print(p)

###   160109    ??R???????????????????????????????????????7??????#####
# Dplyr
#1.??????--??????????????????????????????
#2.??????--?????????????????????????????????
#3.??????--???????????????????????????????????????????????????
#4.??????--?????????????????????????????????
#5.??????(??????group_by)--???????????????????????????,????????????????????????????????????
library(dplyr)
data("mtcars");data('iris')
newdata <- tbl_df(mtcars) # creating a local dataframe. 
filter(newdata,cyl>4&gear>4) # to filter data
filter(irisdata, Species %in% c('setosa','virginica'))
select(newdata,cyl,mpg,hp) # select to pick column by name

newdata %>%
  select(cyl,wt,gear)%>%
  filter(wt>2)  # chaining or pipelining to perform multiple operations

...
  arrange(desc(wt))  # arrange to reorder rows in descending order

...
  mutate(newvar=mpg*cyl) # create new variable
  
newvariable <- newdata %>% mutate(newvariable=mpg@cyl) 

myirisdata%>%
  group_by(Species)%>%
  summarise(average=mean(sepal.ength,na.rm=TRUE)) # summarise data

...
  summarise_each(funs(mean,n()),Sepal.length,Sepal.width) # complex operations 
  
newdata %>% rename(miles=mpg) # rename the variable


# data.table - DT[i,j,by]
data("airquality")
mydata <- airquality
head(airquality,6)

library(data.table)
mydata <- data.table(mydata)
myiris <- data.table(myiris)

mydata[2:4,] # select2nd to 4th rows
myiris[Species=='setosa']
myiris[Species %in% c('setosa','virginica')] # select two species
mydata[,Temp] # select columns with a vector returned
mydata[,.(Temp,Month)] # return selected columns; the .(var) shows the columns, otherwise [,var] shows vector

mydata[,sum(Ozone,na.rm=TRUE)]
mydata[,(sum(Ozone,na.rm=TRUE),sd(Ozone,na.rm=TRUE))]

myiris[,{print(Sepal.Length)
  plot(Sepal.Width)
  NULL}]          # print and plot

myiris[,.(sepalsum=sum(Sepal.Length)),by=Species] # set a column and sort
myiris['setosa'];myiris[c('setosa','virginica')] # select rows associated with data point

### ggplot2 - scatter plot,bar, and histogram ###
library(ggplot2)
library(gridextra)
df <- ToothGrowth

df$dose <- as.factor(df$dose)
head(df)

bp <- ggplot(df,aes(x=dose,y=len,color=dose))+geom_boxplot()+theme(legend.position='none') # boxplot
bp+background_grid(major="xy",minor='none') # add gridlines
sp <- ggplot(mpg,aes(x=cty,y=hwy,color=factory(cyl)))+geom_point(size=2.5) # scatterplot
bp <- ggplot(diamonds,aes(clarity,fill=cut))+geom_bar()+theme(axis.text.x=element_text(angle=70,vjust=0.5)) # barplot
plot_grid(sp,bp,labels=c("A","B"),ncol=2,nrow=1) # compare two plots

ggplot(diamonds,aes(x=carat))+geom_histogram(binwidth=0.25,fill='steelblue')+scale_x_continuous(breaks=seq(0,3,by=0.5)) # histogram

################################# Machine Learning and Data Mining ###############################
* Machine Learning: 
  * Supervised learning: bi-cluster, subclassification
    [DV ~ IV (covariates, regressors)]
    1.regression: (Y being continuous)
      MLR:
      GLR: splime,LLR, Kernal
    2.classification: (Y being discrete)?
      logistic classification
      LDR (QDA): Linear Division Analysis (Quadratic Discriminant Analysis)
      decision tree
      bargining
      forest 
      boosting
      VSM
      nsural network
    [Outcome: prediction; causal relationshpi;quality of predictions and inferences]
  * Unsupervised learning:X only frequencies of keywords; logistic+classification(alipay)
    1.Clustering:
      K-means
      hierarchical clusters
    2.Dimension reduction: 
      PCA: Principal Component Analysis
    3.Networking
    4.Recommendation system
  * Semi-Supervised: Y unestimated while X being studied

################### Text Mining #####################
### File loading and reading
library(getSources,getTransformations,)
#
DirSource()
Corpus()
inspect()





################### Linear Regression
library(ElemStatLearn)
library(ISLR)
data(Wage)
View(Wage)

advert<-read.csv(file="advertising.csv")
View(advert)
par(mfrow=c(1,3))
advert$TV
attach(advert)
plot(Sales~TV,col="red")
abline(lm(Sales~TV),col="blue")
plot(Sales~Radio,col="red")
abline(lm(Sales~Radio),col="blue")
plot(Sales~Newspaper,col="red")
abline(lm(Sales~Newspaper),col="blue")
detach()
lm_sale<-lm(Sales~TV,data=advert)
slm<-summary(lm_sale)
slm
coef(lm_sale)
slm$coef
slm$sigma
slm$r.squared
##forecasting
coef(lm_sale)[1]+coef(lm_sale)[2]*300
slm$coef[,1][1]+slm$coef[,1][2]*300
predict(lm_sale,newdata=data.frame(TV=300))
round(fitted(lm_sale),2)
round(resid(lm_sale),2)
## predict intervation
predict(lm_sale,newdata=data.frame(TV=300),interval="confidence",level=0.95)
predict(lm_sale,newdata=data.frame(TV=300),interval="prediction",level=0.95)

##multivariate regression
lm_sale2<-lm(Sales~TV+Radio+Newspaper,data=advert)
slm2<-summary(lm_sale2)
slm2
coef(lm_sale2)
slm2$coef
slm2$sigma
slm2$r.squared

###predict
coef(lm_sale2)[1]+coef(lm_sale2)[2]*300+coef(lm_sale2)[3]*50+coef(lm_sale2)[4]*80
predict(lm_sale2,newdata=data.frame(TV=300,Radio=50,Newspaper=80))
## predict intervation
predict(lm_sale2,interval="confidence")
predict(lm_sale2,interval="prediction")
predict(lm_sale2,newdata=data.frame(TV=300,Radio=50,Newspaper=80),interval="confidence")
predict(lm_sale2,newdata=data.frame(TV=300,Radio=50,Newspaper=80),interval="prediction")

##stepwise model selection
step(lm_sale2,direction="forward")
step(lm_sale2,direction="both")

##qualitative predictors
credit<-read.csv(file="credit.csv")
View(credit)
str(credit)
credit=credit[,-c(1:2)]   # take out first two columns
pairs(credit)
summary(lm(Balance~Gender,data=credit))
summary(lm(Balance~Ethnicity,data=credit))

###interaction
summary(lm(Sales~TV+Radio,data=advert))
lm_sale3<-lm(Sales~TV+Radio+TV*Radio,data=advert)
summary(lm_sale3)
summary(lm(Balance~Income+Student+Income*Student,data=credit))
####nolinear
library(ISLR)
data(Auto)
View(Auto)
lm_mpg<-lm(mpg~horsepower,data=Auto)
summary(lm_mpg)
plot(mpg~horsepower,data=Auto)
abline(lm_mpg,col="orange")
lm_mpg2<-lm(mpg~horsepower+I(horsepower^2),data=Auto)
summary(lm_mpg2)
#lines(Auto$horsepower,coef(lm_mpg2)[1]+coef(lm_mpg2)[2]*Auto$horsepower+coef(lm_mpg2)[3]*I(Auto$horsepower^2))

### outlier and high leverage
x<-rnorm(100,2,2)
y<-3+2*x+rnorm(100)
lm1<-lm(y~x)
par(mfrow=c(2,2))
plot(lm1)

y<-3+2*x+x^2+rnorm(100)
lm2<-lm(y~x)
par(mfrow=c(2,2))
plot(lm2)

################### Classification
library(ISLR)
data(Default)
View(Default)
str(Default)
table(Default$default)
glm.de<-glm(default~balance,family="binomial",data=Default)
summary(glm.de)
predict(glm.de,newdata=data.frame(balance=1000))
predict(glm.de,newdata=data.frame(balance=1000),type="response")
predict(glm.de,newdata=data.frame(balance=2000),type="response")

glm.de2<-glm(default~student,family="binomial",data=Default)
summary(glm.de2)

glm.de3<-glm(default~balance+income+student,family="binomial",data=Default)
summary(glm.de3)

# south africa heart data
library(ElemStatLearn)
View(SAheart)
heartfit<-glm(chd~.,data=SAheart,family=binomial)
summary(heartfit)

# Example: The stock market data
# comparison of KNN, LDA, QDA logistic regression
library(ISLR)
names(Smarket)
View(Smarket)
dim(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,c(-1,-9)])
## Logistic regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data=Smarket ,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
glm.pred=rep("Down",1250)
glm.pred[glm.probs >.53]="Up"
table(glm.pred,Smarket$Direction)
table(Smarket$Direction)

train =( Smarket$Year <2005)
TrainD<-Smarket[train,]
TestD<-Smarket[!train,]
Smarket.2005= Smarket [!train ,]
dim(Smarket.2005)
attach(Smarket)
Direction.2005= Direction [! train ]
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data=Smarket ,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Direction.2005)


## Test
sum(default=="Yes")/length(default)     # count Yeses
n=dim(Default)[1]               # check sample size
sam = sample(n,0.7*n)           # sampleing with replacement 
T1 = Default[sam,]
V1 = Default[-sam,]
glm4 = glm(default~student,balance,income,dta=T1)

predict(glm4,newdata=data.frame(balance=2500,income=50000,student="Yes"),type="response")

* Imbalanced data: bootstrap / resampling
  validate variances of estimates
  ensemble learning

# linear discriminant analysis
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket ,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
lda.pred
lda.class=lda.pred$class
table(lda.class ,Direction.2005)
####quadratic discriminant analysis
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket ,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class 
table(qda.class ,Direction.2005)
####KNN
library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction =Direction [train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction ,k=1)
table(knn.pred,Direction.2005)
knn.pred=knn(train.X,test.X,train.Direction ,k=3)
table(knn.pred,Direction.2005)
knn.cv(train.X,train.Direction ,k=3)

# Excercise 12
#(a)
Power <- function(){
  print(2^3)
}
Power()
#(b)
Power2 <- function(x, a){
  x^a
}
Power2(3,2)
Power2(3,8)
#(c)
Power2(10,3)
Power2(8,17)
Power2(131,3)
#(d)
Power3 <- function(x, a){
  pow<-x^a
  return(power=pow)
}

#(e)
x <- 1:10
y <- Power3(x,2)
plot(x,y,type="l", xlab = "x-axis", ylab = "y-axis", main = "f(x) = x^2")

#(f)
PlotPower <- function(x,a){
  plot(x,x^a, xlab = "x-axis", ylab = "y-axis", type = "l",main=paste("f(x)=x^",a))
}
PlotPower(1:10, 3)

### comparison of classification method
## scenario 1
library(class)
library(MASS)
library(ggplot2)
Sigma <- matrix(c(2,0,0,2),2)
x1<-mvrnorm(n=20, c(0,0), Sigma)
x2<-mvrnorm(n=20, c(2,2), Sigma)
x<-rbind(x1,x2)
y <- as.factor(rep(c(0,1),each=20))
test<-data.frame(x,y)
#View(test)
plot(x[,1],x[,2],col=y)

mis=function(A){
  (A[1,2]+A[2,1])/sum(A)
}
pred=matrix(NA,nrow=100,ncol=5)
colnames(pred)=c("KNN.1","KNN.CV","LOGISTIC","LDA","QDA")
for(i in 1:100){
  Sigma <- matrix(c(2,0,0,2),2)
  x1<-cbind(mvrnorm(n=20, c(0,0), Sigma))
  x2<-cbind(mvrnorm(n=20, c(2,2), Sigma))
  x<-rbind(x1,x2)
  y <- as.factor(rep(c(0,1),each=20))
  train=data.frame(x,y)
  glmf<- glm(y~.,family=binomial,data=train)
  ldaf<-lda(y~.,data=train)
  qdaf<-qda(y~.,data=train)
  pred[i,1]=mis(table(knn(x,test[,1:2],y,k=1),test[,3]))
  pred[i,2]=mis(table(knn(x,test[,1:2],y,k=3),test[,3]))
  pglm=rep(0,40)
  pglmp<-predict.glm(glmf,test[,1:2],type="response")
  pglm[pglmp>0.5]=1
  pred[i,3]=mis(table(pglm,test[,3]))
  pred[i,4]=mis(table(predict(ldaf,newdata=test[,1:2])$class,test[,3]))
  pred[i,5]=mis(table(predict(qdaf,newdata=test[,1:2])$class,test[,3]))
}
boxplot(pred,col=2:6)




##################### Model Selection / Regulation
# best subset selection
library(ISLR)
data(Hitters)
View(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
library(leaps)
regfit.full=regsubsets(Salary~.,Hitters)      # regress subsets; maximam number of variables is 11; nbest, nvmax
summary(regfit.full)      # check result; best model selection for each step/subset, 

regfit.full=regsubsets(Salary~.,Hitters,nvmax=19)
reg.summary=summary(regfit.full)
reg.summary
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",   
     type="b")
plot(reg.summary$rsq ,xlab="Number of Variables ",ylab="Rsq",
     type="b")
plot(reg.summary$adjr2 ,xlab="Number of Variables ",
     ylab="Adjusted RSq",type="b")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)
plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type="b")
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC", type="b")
loc = which.min(reg.summary$bic)
points(6,reg.summary$bic[6], col="red",cex=2,pch=20)
plot(regfit.full,scale="bic")
which.min(reg.summary$bic)
coef(regfit.full,6)
##Forward & backward
#step()
##forward selection
regfit.fwd=regsubsets (Salary~.,data=Hitters ,nvmax=19, method ="forward")
summary(regfit.fwd)
#backward selection
regfit.bwd=regsubsets (Salary~.,data=Hitters ,nvmax=19,
                       method ="backward")
summary(regfit.bwd)

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)
##have different results
##choosing models using Validation and CV
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE) 
test=(!train)
regfit.best=regsubsets(Salary~.,data=Hitters[train,], nvmax =19)
test.mat=model.matrix(Salary~.,data=Hitters[test,])
val.errors=rep(NA,19)
for(i in 1:19){
   coefi=coef(regfit.best,id=i)
   pred=test.mat[,names(coefi)]%*%coefi
   val.errors[i]=mean((Hitters$Salary[test]-pred)^2) 
          }
val.errors
which.min(val.errors)
plot(1:19,val.errors,type="b")
coef(regfit.best,10)

### writing function predict
library(glmnet)
predict.regsubsets=function(object,newdata ,id,...){
   form=as.formula(object$call [[2]])
   mat=model.matrix(form,newdata)
   coefi=coef(object ,id=id)
   xvars=names(coefi)
   mat[,xvars]%*%coefi 
   }
regfit.best=regsubsets (Salary~.,data=Hitters ,nvmax=19)
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))

for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
    for(i in 1:19){
      pred=predict(best.fit,Hitters[folds==j,],id=i) 
      cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
    } 
}
(mean.cv.errors=apply(cv.errors ,2,mean))
plot(mean.cv.errors,type="b")
which.min(mean.cv.errors)
reg.best=regsubsets (Salary~.,data=Hitters , nvmax=19)
coef(reg.best,11)


##
library(glmnet)
###ridge
View(Hitters)
x = model.matrix(Salary~.,Hitters)[,-1]   # change categorical into dummies, may have NA values
y = Hitters$Salary
grid = 10^seq(10,-2,length=100)
grid
ridge.mod = glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
plot(ridge.mod,xvar="lambda")
cv.out<-cv.glmnet(x,y,alpha=0,lambda=grid)
plot(cv.out)
coef(cv.out)
(bestridge<-cv.out$lambda.min)
par(mfrow=c(1,1))

## Lasso - pick right side of suggested line for 5 variables
library(glmnet)
lasso.mod=glmnet(x,y,alpha=1,lambda=grid) 
par(mfrow=c(1,2))
plot(lasso.mod)
plot(lasso.mod,xvar="lambda")
par(mfrow=c(1,1))
lasso2<-cv.glmnet(x,y,alpha=1,lambda=grid)    # cross validation
lasso2
coef(lasso2)                  # show coefficients of cross validation
coef(lasso2,s="lambda.min")   # pick the smallest breakpoint, glm with slected variables
plot(lasso2)                  
## Adaptive LASSO


##SCAD MCP
library(ncvreg)
fit1<-ncvreg(x,y,family="gaussian",penalty="MCP")
plot(fit1)
fit11<-cv.ncvreg(x,y,family="gaussian",penalty="MCP")
plot(fit11)
coef(fit11)
fit2<-ncvreg(x,y,family="gaussian",penalty="SCAD")
plot(fit2)
fit22<-cv.ncvreg(x,y,family="gaussian",penalty="SCAD")
plot(fit22)

##SCAD,MCP,LASSO 
library(ncvreg)
data(heart)
View(heart)
dim(heart)
x <- as.matrix(heart[,1:9])
y <- heart$chd # 1 yes, 0 no
#lasso
fit.la<-glmnet(x,y,family="binomial")
plot(fit.la,xvar="lambda")
fit.lasso<-cv.glmnet(x,y,family="binomial")
plot(fit.lasso)
(beta.lasso<-coef(fit.lasso))
fit.lasso<-cv.ncvreg(x,y,family="binomial",penalty="lasso")
plot(fit.lasso)
#MCP
logit.mcp<-ncvreg(x,y,family="binomial",penalty="MCP")
plot(logit.mcp)
cvfit.mcp <- cv.ncvreg(x,y,family="binomial")
plot(cvfit.mcp)
coef(cvfit.mcp)
summary(cvfit.mcp)
fit.mcp<- cvfit.mcp$fit
plot(fit.mcp)
(beta.mcp<- fit.mcp$beta[,cvfit.mcp$min])
coef(cvfit.mcp)

#SCAD
fit.scad<- ncvreg(x,y,family="binomial",penalty="SCAD")
plot(fit.scad)

cvfit.scad <- cv.ncvreg(x,y,family="binomial",penalty="SCAD")
plot(cvfit.scad)
coef(cvfit.scad)
summary(cvfit.scad)
fit.scad<- cvfit.scad$fit
plot(fit.scad)
beta.scad<- fit.scad$beta[,cvfit.scad$min]

####group selection
####linear regression
library(grpreg)
data(birthwt.grpreg)
View(birthwt.grpreg)
X <- as.matrix(birthwt.grpreg[,-1:-2])
y <- birthwt.grpreg$bwt
group <- c(1,1,1,2,2,2,3,3,4,5,5,6,7,8,8,8)#±‰¡ø◊ÈΩ·ππ
colnames(X)
fit<-grpreg(X, y, group,penalty="grLasso")
plot(fit)
cvfit <- cv.grpreg(X, y, group,penalty="grLasso")
plot(cvfit)
coef(cvfit) 

##logistic regression
library(grpreg)
data(birthwt.grpreg)
View(birthwt.grpreg)
X <- as.matrix(birthwt.grpreg[,-1:-2])
y <- as.factor(birthwt.grpreg$low)
group <- c(1,1,1,2,2,2,3,3,4,5,5,6,7,8,8,8)#?????????????????????
cvfit <- grpreg(X, y, group,penalty="gLasso",family="binomial")
coef(cvfit) ## Beta at minimum Cross-Validation Error
plot(cvfit)
summary(cvfit)

cvfit <- cv.grpreg(X, y, group, penalty="grMCP",family="binomial")#L2-norm Group MCP
coef(cvfit) ## Beta at minimum Cross-Validation Error
plot(cvfit)
summary(cvfit)

cvfit <- cv.grpreg(X, y, group, penalty="grSCAD",family="binomial")#L2-norm Group SCAD
coef(cvfit) ## Beta at minimum Cross-Validation Error
plot(cvfit)
summary(cvfit)
######bi-levle selection
##composite MCP
library(grpreg)
data(birthwt.grpreg)
X <- as.matrix(birthwt.grpreg[,-1:-2])
y <- birthwt.grpreg$bwt
group <- c(1,1,1,2,2,2,3,3,4,5,5,6,7,8,8,8)
cvfit.m <- cv.grpreg(X,y,group,penalty="cMCP", family="gaussian",gama=2.5) #Composite MCP
plot(cvfit.m)
coef(cvfit.m)

## PCR 
library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters ,scale=TRUE,
            validation ="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
pcr.fit2=pcr(Salary~., data=Hitters,ncomp=1)
summary(pcr.fit2)
## PLS
set.seed(1)
pls.fit=plsr(Salary~., data=Hitters,subset=train,scale=TRUE,
             ￼￼￼validation ="CV")
summary(pls.fit)

##########


################## Decision Tree
library(tree)   # library(ISLR)

##regression tree
data(Hitters)
View(Hitters)
attach(Hitters)
Hitters<-data.frame(Salary,Years,RBI,Hits,Walks,Runs,PutOuts)
View(Hitters)

ind<-sample(322,232)
train<-Hitters[ind,]
test<-Hitters[-ind,]
tree.hit<-tree(Salary~.,data=train)
tree.hit
summary(tree.hit)
par(mfrow=c(1,1))
plot(tree.hit)
text(tree.hit)
#prune
set.seed(1)
cv.hit=cv.tree(tree.hit,FUN=prune.tree)
cv.hit
plot(cv.hit$size,cv.hit$dev ,type="b")
prune.hit=prune.tree(tree.hit,best=6)   # test number of leafe node
plot(prune.hit )
text(prune.hit,pretty =0)
detach()
#### Classification tree
library(ISLR)
attach(Carseats)
View(Carseats)
High=ifelse(Carseats$Sales <=8,"No","Yes")
High
Carseats =data.frame(Carseats,High)[,-(13:14)]    # delete column 13 and 14
View(Carseats)
dim(Carseats)
ind<-sample(400,300)    # 400 elements, 300 sample size
train<-Carseats[ind,]
test<-Carseats[-ind,]
View(train)
tree.carseats =tree(High~.-Sales,train)
summary(tree.carseats)
## deviance= -2\sum_m\sum_k n_{mk}log(\hat p_{mk})
#residual mean devicance= deviance/ (n-|T0|)
tree.carseats
plot(tree.carseats)
text(tree.carseats,pretty=0)
######prune
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
cv.carseats
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats )
text(prune.carseats ,pretty =0)
#### Predict
tree.pred=predict(prune.carseats,test,type="class") 
table(tree.pred,test$High)
prune.carseats=prune.misclass(tree.carseats,best=25)
plot(prune.carseats )
text(prune.carseats ,pretty =0)
tree.pred=predict(prune.carseats,test,type="class")
table(tree.pred,test$High)

### Bagging
library(randomForest)
set.seed(1)
dim(Boston)
View(Boston)
train=sample(nrow(Boston),0.5*nrow(Boston))
bag.boston=randomForest(medv~.,data=Boston,subset=train,ntree=200,
                       mtry=dim(Boston)[2]-1,importance =TRUE)
bag.boston
boston.test=Boston[-train ,]$medv
yhat.bag = predict(bag.boston,newdata=Boston[-train ,])
boston.test=Boston[-train ,]
plot(yhat.bag, boston.test$medv)
abline (0,1)

### Random Forest
rf.boston=randomForest(medv~.,data=Boston,subset=train,
                       mtry=3,importance =TRUE)       # mtry=p/3 or sqrt(p)
yhat.bag = predict(rf.boston,newdata=Boston[-train ,])
boston.test=Boston[-train ,]
plot(yhat.bag, boston.test$medv)      # MSE and fit line (abline) in graph
abline (0,1)
importance(rf.boston)     # importance of each variable; randomized certain variable to keep variable while in the model
varImpPlot (rf.boston)    # Dotchart of variable importance as measured by a Random Forest
####Boosting
library(gbm)
boost.boston=gbm(medv~.,data=Boston[train ,],distribution=
                   "gaussian",n.trees =5000, interaction.depth =1)
summary(boost.boston)
par(mfrow=c(1,2)) 
plot(boost.boston ,i="rm") 
plot(boost.boston ,i="lstat")

yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees=5000)

mean((yhat.boost-boston.test)^2)

## Simulation for Adaboost chi-square
set.seed(5)
library(MASS)
x=mvrnorm(n=4000,rep(0,10),diag(1,10))
y=rep(0,4000)
for(i in 1:4000){
  if(sum(x[i,]^2)>9.34){
    y[i]=1
  }
}
dat=data.frame(x,y)
train=sample(4000,2000)
library(gbm)
boost.error=NULL
for (i in 1:500){
  boost1=gbm(y~.,data=dat[train,],distribution="adaboost",n.tree=i,interaction.depth =4 )
  yhat.boost=predict(boost1,newdata=dat[-train,],n.trees =i,type="response")
  yhat=rep(0,2000)
  yhat[yhat.boost>0.5]=1
  boost.error[i]=sum(yhat!=y[-train])/2000
}
plot(boost.error,main="Boosting",type="l")
abline(h=boost.error[1])


library(randomForest)
randata=data
bag.error=NULL
for (i in 1:100){
  bag=randomForest(y~.,data=randata,subset=train,ntree=i,mtry=dim(data)[2]-1,importance=T)
  yhat.bag=predict(bag,newdata=randata[-train,])
  bag.error[i]=sum(yhat.bag!=y[-train])/2000
}
plot(tree.error1,main="bagging",type="l")
abline(h=tree.error1[1])

rf.error=NULL
for (i in 1:100){
  rf=randomForest(y~.,data=randata,subset=train,ntree=tree[i],mtry=3,importance=TRUE)
  yhat.rf=predict(rf,newdata=data[-train,])
  rf.error[i]=sum(yhat.rf!=y[-train])/2000
}
plot(rf.error,main="randomforest",type="l")
abline(h=tree.error2[1])



############### Computational Social Science  ######################
## Individual and social structure
  # social structure affect individuals in short term
  # individuals affect social strucutre in long term
  # micro-macro: Coleman s "Boat"
    micro individual behaviors to macro social phenomenon
    macro: contextual state to global outcome
    micro: indivdual response to resulting action
    macro-micro: typically contextual conditions that enable/constrain individual action
    micro-micro: adirect-action correlate of the contextuallly constrained behaviors in 1
    micro-macro: an aggregation or interaction process that can account for the new global level outcome
      observed macro-level correlation is thus accounted for by actors capable of intent and action
  # Schelling Model (1921)
    Assumption: individual decides not to move when neighbored with the same ethics
      everyone is racist, and will move once their neighbor moves
      everyone likes to stay with peers of Ethnicity
  # ABM (Agent-based Modeling)
    Heterogeneity:Equation-based Modeling. 
      Individuals rest in the relationshpi of IVs and Dvs with hetergeneity
      Individuals vary with Heterogeneity
      Characteristics being endogenous and time-invariant 
    Autonomy: 
      Actions of agents behaves without central (environmental) control
      Behaviors affected by principles, macro environment and micro structure
      Individuals evolve with interaction of others
    Explicit space: 
      Incidents occur in certain (social) space of resources
      Space defined as n-dimentional space, social network
    Local interactions: 
    Bounded ratoinality: limited information and compulational power with simple rules
    Non-equilibrium dynamics:
    * Goals
      autonomous local interactions of heterogenous boundedly rational agents
      micro interactions generate (upward from micro level) macro social phenomenon
    * Categories
      abstract model      single Characteristics
      medium model
      visionaary model    all Characteristics
    * System 
      e.g.: Sugarscape Model
  # Deffuant Bounded Confidence Model (ABM)
    Agent
      attitude
      pair interaction
      diff of attitude < miu (0.5/0.25) social tolerance
      
### CMED 6020, MMPH6117 Advanced Statistical Methods I #############################################
# SESSION 1 ##########################################
mvc <- read.csv("http://web.hku.hk/~ehylau/mvc.csv")

dim(data) # check the number of cases and variables
colname(data) # see variable names
head(data)    # specify the number of rows
tail(data)
relevel       # reset the reference group

aggregate(y ~ x1+x2, data=dataname, FUN='mean')
prop.table(table, margin)   # margin - by row/column


## ggplot help
http://www.cookbook-r.com/Graphs/index.html

# histogram
hist(mvc$MVC, axes=FALSE, xlim=c(0, 600), 
    ylim=c(0, 8), font.lab=2, cex.lab=1.2, 
    cex.main=1.5, col=grey(0.8),   
    xlab="MVC", ylab="Frequency", 
    main="Histogram of MVC")
axis(1, pos=-0.2, lwd=3.5, font=2)
axis(2, pos=-20, lwd=3.5, font=2, las=1)

# scattered plot
plot(MVC ~ age, data=mvc, xlab="Age (years)", 
    ylab="Quadriceps muscle (newtons)", 
    xlim=c(20, 70), ylim=c(0, 600), 
    cex=1.5, cex.lab=1.2, 
    font.lab=2, font.axis=2, las=1)

# line plot


# save graph to pdf
pdf("d:/figure1.pdf", width=6, height=4)
plot(MVC ~ age, data=mvc, 
    xlab = "Age (years)", 
    ylab = "Quadriceps muscle (newtons)",  
    xlim = c(20, 70), ylim = c(0, 600), 
    cex = 1.5, cex.lab = 1.2, 
    font.lab = 2,  font.axis = 2, las = 1) 
dev.off() 



# convert numeric to factor
cut(x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE, ...) 

# record categorical - setting reference group
x.f <- cut(x,c(1,3,6,8), label=c('low','med','high'), include.lowest=T, right=F) 
relevel(x.f, ref='med')                      # setting 'med' as the reference group
factor(x.f, c('high', 'med', 'low'))   # setting 'high' as reference group in such order



# exercise 1 ##########################################
# mvc <- read.csv("http://web.hku.hk/~ehylau/mvc.csv") 

n = (1:10)
f1 = -3*n + 7
f2 = 2*(-0.5)^(n-1)
plot(x=n, y =f1)
plot(x=n, y =f2, type="l")

# 1. Using R, generate the first 10 terms of the arithmetic sequence an = -3n + 7 (a1 = 4, a2 = 1, a3 = -2,...)
An = function(x){
  f = 0
  n = 1
  ns = fs = c()
  for(n in 1:x){
    f = -3*n + 7
    print(paste("a",n," = ",f,sep=''))
    ns = cbind(ns,n)
    fs = cbind(fs,f)
    n = n + 1
  }
  plot(x = ns, y = fs, xlab="n", ylab="An",
      xlim = c(1,x), ylim = c(-3*x+7, -3*1+7), type = "l")
}


# 2. Using R, generate the first 10 terms of the geometric sequence gn = 2*(-0.5)^(n-1) (g1 = 2, g2 = -1, g3 = 0.5,...)
Gn = function(x){
  f = 0
  n = 1
  ns = fs = c()
  for(n in 1:x){
    f = 2*(-0.5)^(n-1) 
    print(paste("g",n," = ",f,sep=''))
    ns = cbind(ns,n)
    fs = cbind(fs,f)
    n = n + 1
  }
  plot(x = ns, y = fs, xlab="n", ylab="An",
  xlim = c(min(ns),max(ns)), ylim = c(min(fs),max(fs)), type = "l")
}

# exercise 2 ##########################################
# 1.
set.seed(1)
x = rnorm(10000,0,2)
y = rnorm(10000,2,3)
xy = rnorm(10000,2,5)
xy2 = x + y

par(mfrow=c(2,2))

hist(x, axes=FALSE, xlim=c(-12,12), ylim=c(0, 3000), font.lab=2, cex.lab=1.2, cex.main=1.5, col=grey(0.8),  xlab="X", ylab="Frequency", main="Histogram of X") 
hist(y, axes=FALSE, xlim=c(-12,12), ylim=c(0, 3000), font.lab=2, cex.lab=1.2, cex.main=1.5, col=grey(0.8),  xlab="Y", ylab="Frequency", main="Histogram of Y") 
plot(y ~ x, xlab="X", ylab="Y", xlim=c(-12,12), ylim=c(-12,12), cex=1.5, cex.lab=1.2, font.lab=2, font.axis=2, las=1) 
hist(xy, axes=FALSE, xlim=c(-12,12), ylim=c(0, 3000), font.lab=2, cex.lab=1.2, cex.main=1.5, col=grey(0.8),  xlab="X+Y", ylab="Frequency", main="Histogram of X+Y") 
hist(xy2, axes=FALSE, xlim=c(-12,12), ylim=c(0, 3000), font.lab=2, cex.lab=1.2, cex.main=1.5, col=grey(0.8),  xlab="X+Y(simulated)", ylab="Frequency", main="Histogram of x+Y") 

mean(x); sd(x)
mean(y); sd(y)
mean(xy); sd(xy)

# 2.
mvc <- read.csv("http://web.hku.hk/~ehylau/mvc.csv") 
ht <- cut(mvc$height,c(168,173), include.lowest=T, right=F) 
lm(mvc$MVC ~ mvc$age+ht)

label=c('155-167','168-172','173-180'), 


# Tutorial 1 ##########################################
# 1. Central limit theorem
# a-c)
a = runif(1000,1,3)
b = runif(1000,1,3)
c = runif(1000,1,3)
m = (a+b+c)/3     # m = rowMeans(t) or colMeans(t)
v = (a-m)^2+(b-m)^2+(c-m)^2   
t = matrix(c(a,b,c,m,v),1000,5)
hist(m) # plot(density(m))

# d)
m1 = mean(t[,4])
s1 = var(t[,4])

shapiro.test(m)

# e) change into 30 columns

t2=matrix(runif(30*1000,1,3),ncol=30)
m2 = mean(rowMeans(t2))
v2 = var(rowMeans(t2))
hist(m2)
plot(density(m2))

shapiro.test(rowMeans(t2))


# f)
hist(rowMeans(t3),breaks=0:5/10,freq=F,main="")   # hist(sample(1:5, 1000, replace=T, prob=c(0.1,0.35,0.1,0.35,0.1)), breaks=0:5, freq=F, main="")

t3 = matrix(sample(1:5,size=300000,replace=T,prob=c(0.1,0.35,0.1,0.35,0.1)),ncol=300)
m3 = mean(rowMeans(t3))
v3 = var(rowMeans(t3))

plot(density(rowMeans(t3)))
hist(rowMeans(t3), breaks=0:50/10)

shapiro.test(rowMeans(t3))

# g) Null hypothesis: mean of 5 and sd of 3 in normally distributed data (or uniformed distribution)
??nomrality

shapiro.test(rnorm(100,mean=5,sd=3))
shapiro.test(runif(100,min=2,max=4))


# 2. MVC
mvc <- read.csv("http://web.hku.hk/~ehylau/mvc.csv") 
summary(mvc)

# a)
mvc$younger = ifelse(mvc$age <= 40,1,0)   # mvc$younger <- cut(mvc$age,c(min(mvc$age),40,max(mvc$age)), lab=c(1,0), include.lowest=T)


# b)
m1 = mean(mvc$MVC[mvc$younger==1])
m2 = mean(mvc$MVC[mvc$younger==0])  # aggregate(MVC~younger,by=list(nvc$younger),mean); aggregate(MVC~younger,data=mvc,mean)

t.test(MVC~younger,data=mvc)

# c) 
boxplot(MVC~younger, data=mvc, xlab = 'Younger adults', ylab = 'MVC', main = 'Boxplot of MVC by age group' )

# d)
plot(mvc$height, mvc$MVC, xlab = 'Height', ylab = 'MVC', main='Scatter plot between Height and MVC')
abline(lm(MVC ~ height, data = mvc))

reg$coef[1]     # the first output of the coefficient in regression model


# e)
par(mfrow=c(2,2), mar=c(4,4,1,1))
age.lower <- 2:5*10

for (i in 1:4){
  plot(mvc$height, mvc$MVC, xlab = 'Height', ylab = 'MVC', type='n', las=1)
  temp.mvc <- mvc[mvc$age>age.lower[i], ]
  points(temp.mvc$height, temp.mvc$MVC, col=gray(0.7), pch=19)
  temp.lm.mvc <- lm(MVC ~ height, data = temp.mvc)
  abline(temp.lm.mvc)
  legend("topleft", paste('age > ', age.lower[i], 'y', sep=''))
}

type = n # nothing in the graph

# f)
par(mfrow=c(2,2), mar=c(4,4,1,1))
age.lower <- 2:5*10

for (i in 1:4){
  plot(mvc$height, mvc$MVC, xlab = 'Height', ylab = 'MVC', type='n', las=1)
  temp.mvc <- mvc[mvc$age>age.lower[i], ]
  points(temp.mvc$height, temp.mvc$MVC, col=gray(0.7), pch=19)
  temp.lm.mvc <- lm(MVC ~ height, data = temp.mvc)
  abline(temp.lm.mvc)
  legend("topleft", paste('age > ', age.lower[i], 'y', sep=''))
  legend("bottomright",paste('MVC =', format(round(temp.lm.mvc$coef[1],1),nsmall=1), 
' + ', format(round(temp.lm.mvc$coef[2],1),nsmall=1), 'age', sep=''), bty='n', text.font=2)
}

# example: by 5 years age groups
#windows(width=6, height=10)
pdf('d:/figure1.pdf', width=6, height=10)
par(mfrow=c(4,2), mar=c(4,4,1,1))
age.lower <- 4:11*5

for (i in 1:8){
  plot(mvc$height, mvc$MVC, xlab = 'Height', ylab = 'MVC', type='n', las=1)
  temp.mvc <- mvc[mvc$age>age.lower[i], ]
  points(temp.mvc$height, temp.mvc$MVC, col=gray(0.7), pch=19)
  temp.lm.mvc <- lm(MVC ~ height, data = temp.mvc)
  abline(temp.lm.mvc)
  legend("topleft", paste('age > ', age.lower[i], 'y', sep=''))
  legend("bottomright",paste('MVC =', format(round(temp.lm.mvc$coef[1],1),nsmall=1), 
' + ', format(round(temp.lm.mvc$coef[2],1),nsmall=1), 'age', sep=''), bty='n', text.font=2)
}
dev.off()

# g)
lm.mvc <- lm(MVC ~ height, data=mvc)

mvc$height2 <- mvc$height^2
lm.mvc2 <- lm(MVC ~ height + height2, data=mvc)

summary(lm.mvc2)

# h)
AIC(lm.mvc, lm.mvc2) #AIC difference < 2 -> prefer the more parsimonious model

# i)
require(MASS)
lm.mvc3 <- lm(MVC ~ age + height + height2, data=mvc)
step.mvc <- stepAIC(lm.mvc3, direction="both")

summary(step.mvc)

# scope: specific the range of models to be selected
step.mvc2 <- stepAIC(lm.mvc3, direction="both", scope=list(lower=~height))
step.mvc3 <- stepAIC(lm.mvc3, direction="both", scope=list(lower=~height, upper=~age*height+age*height2))


# j)
lm.mvc4 <- lm(MVC ~ age + height, data=mvc)

new <- data.frame(age=50, height=170)
predict(lm.mvc4, new, interval="prediction")


# k): extrapolation
new2 <- data.frame(age=50, height=220)
predict(lm.mvc4, new2, interval="prediction")

new3 <- data.frame(age=50, height=150:220)
pred.mvc4 <- predict(lm.mvc4, new3, interval="prediction")

plot(new3$height, pred.mvc4[,"fit"], type='l', ylim=c(0,1000), xlab="height", ylab="predicted MVC", las=1)
lines(new3$height, pred.mvc4[,"lwr"], lty=2)
lines(new3$height, pred.mvc4[,"upr"], lty=2)
text(220, 1000, "at age 50y", adj=1, font=2)
polygon(c(rep(min(mvc$height),2),rep(max(mvc$height),2)),c(-50,1100,1100,-50), border=NA, col=rgb(0,0.5,0,0.2))

# SESSION 3 - Poisson distribution and GLM ##########################################

# Poisson distribution has only 1 parameter, the mean and variance are assumed to be the same
  # log of the mean 
    # link function: where logit(y) = log(y/(1-y))
  # rates in Poisson distribution

# Model check
  # compare the actual probability and expected ones 
  # check residual deviance/df <=1 indicates the model has good fitting; otherwise the model fails
  # or use chi square test to compare the model wit null hypothesized model
# 

dpois(x=2,lambda=5)   # density of Poisson distribution, show the xth number of incident, the probability is the corresponding one
rpois(n=10, lambda=5)
var(rpois(n=10, lambda=5))


for (i in 10^(1:5)){
  check = rbind(check,shapiro.test(rpois(n=1000,lambda=i))[2])
}


horse = read.table("C:/Users/chens/OneDrive/research/2school/PhD Courses/CMED 6020 MMPH6117 Advanced Statistical Methods I/3 GLM/GLM1/Example - horse.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)
summary(horse)
hist(horse$death, breaks=0:5)         # clustered by 1 unit of x from 0 to 5
mean(horse$death); var(horse$death)   # mean is close to variance
table(horse$death);table(horse$corps)

  # deaths ~ year
boxplot(deaths~year,data=horse, xlab="year", ylab="deaths")   # boxplot by year

horse.year <- aggregate(horse$deaths, by=list(horse$year), sum)   
colnames(horse.year) <- c('year', 'deaths') 
plot(horse.year$year, horse.year$deaths, type='l', xlab='year', ylab='deaths')    # line plot
plot(h[,1],h[,2],type='l',xlab='year',ylab='deaths')        # or, without colname function, use matrix function

  # deaths ~ corps
boxplot(deaths~corps, data=horse, xlab="corps", ylab="deaths")


# GLM - ordinal linear, logistic, poisson, negative binomial models
summary(glm(deaths ~ 1, data=horse, family=poisson))    # "~1" indicates fit in intercept only
summary(glm(deaths ~- 1, data=horse, family=poisson))    # "-1" leaves intercept out

pois.horse <- glm(deaths ~ 1, data=horse, family=poisson) 
coef(pois.horse)            # or 'pois.horse$coef'
exp(coef(pois.horse))       # to obtain the mean from the link function by taking the exp; mean(deaths)=0.61
exp(confint(pois.horse))    # obtain the confidence interval from the link function by taking the exp; CI(deaths) = [0.5,0.724]

  # model check - comparison
prop.table(table(horse$deaths))   # = table(horse$deaths)/sum(table(horse$deaths))
round(dpois(0:5,0.61),2)          # density distribution of poisson, rounded up to the second digit
nrow(horse)*dpois(0:5,exp(coef(pois.horse)))  # reverse to the expected counts of deaths based on the data volumn using the poisson distribtion

class(horse$corps)
horse$corps <- as.factor(horse$corps)     # corps is the categorical variable rather than continuous variables
summary(glm(deaths~corps, data=horse, family=poisson))    # reference group is corp 2 by default


pois.corps <- glm(deaths~corps, data=horse, family=poisson) 
round(exp(coef(pois.corps)),2)    # the exp coefficient indicate the time's relationship between each group to the reference group

cbind(coef(pois.corps),confint(pois.corps))
round(exp(cbind(coef(pois.corps), confint(pois.corps))),2)    # transformed exp coefficient of each group in relation to the reference group

class(horse$corps)
horse$corps=as.factor(horse$corps)                      # turn continuous into categorical variables
summary(glm(deaths~corps, data=horse, family= poisson)) 

pois.corps=glm(deaths~corps, data=horse, family= poisson)
round(exp(coef(pois.corps)),2)
round(exp(cbind(coef(pois.corps),confint(pois.corps))),2) # revert to the table with coefficient and confidence interval


# counts and rates
lung = read.table("C:/Users/chens/OneDrive/research/2school/PhD Courses/CMED 6020 MMPH6117 Advanced Statistical Methods I/3 GLM/GLM1/Example - lung.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)

pois.lung <- glm(count~offset(log(pop))+city+age.gp, data=lung, family=poisson)   # offset function enable in coefficient of 1 for offset log term, in consideration of count/population rates
round(exp(cbind(coef(pois.lung), confint(pois.lung))),3)    # transformation of coefficients; incidents increase as aging

plot(predict(pois.lung, type='response'), lung$count, 
    xlim=c(0,15), ylim=c(0,15), xlab='predicted', ylab='observed') 
abline(a=0, b=1)     #  Compare predicted and observed data

# observed incidences 
with(lung,round(count[city=="Fredericia"]
pop[city=="Fredericia"]*1000,2)) 
 
# predicted incidences 
new <- data.frame(city="Fredericia", age.gp=lung$age.gp[1:6], pop=lung$pop[1:6]) 
round(predict(pois.lung, newdata=new, type='response')/ lung$pop[lung$city=="Fredericia"]*1000,2)

# model checking 2
summary(pois.lung) 
deviance(pois.lung)/df.residual(pois.lung)    # model checking

# Variance is much larger than mean in poisson distribution - alpha indicates more randomness/ variance
#Example: lambda=5, gamma mean=1, variance=2
rpois(10000, 5*rgamma(10000,1/2,1/2))

epi = read.table("C:/Users/chens/OneDrive/research/2school/PhD Courses/CMED 6020 MMPH6117 Advanced Statistical Methods I/3 GLM/GLM1/Example - epilepsy.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)
mean(epi$y); var(epi$y)

require(MASS)
summary(glm(y~1,data=epi))
nb.epilepsy0 <-glm(y~1,data=epi)
nb.therapy1 <- glm.nb(y~therapy, data=epi)
nb.therapy2 <- glm.nb(y~therapy+x+age, data=epi)
epi$log10x <-  log10(epi$x)
nb.therapy3 <- glm.nb(y~therapy+log10x+age, data=epi)
round(exp(rbind(cbind(coef(nb.therapy1),confint(nb.therapy1)),
    cbind(coef(nb.therapy2),confint(nb.therapy2)),
    cbind(coef(nb.therapy3),confint(nb.therapy3)))),3)

# model comparison
AIC(nb.epilepsy0, nb.therapy1, nb.therapy2, nb.therapy3)    # AIC the lowest indicates best fit

# # SESSION 4, Session 5 ####################################################################################
 # Multicollinearity
   # Scatterplot between all predictor variables
   # Variance inflation factor (VIF) - inflated SE
     # x (to be tested) as the predictor and the rest being IV
     > 10 -> Multicollinearity
     # centering if it is polynominal model
   No direct explantation of the x2 (collineared IV) on the outcome variable
   # Strategy on Multicollinearity
     # Do nothing: because coefficients and standard errors are unbiased; however, there is inefficient estimation; overall, should follow the objectives and assumptions 
     # Increase sample size
     # Polynomial terms and interactions: centering (subtracting variable by its mean)
       squred or interaction with centered term to solve 
 # Confounding effect (C):
   # Xc affect both IV and DV
   # DAG: directed acyclic graph 
   argue the residual confounding effect in obsevational data
   # minimizing confounding effects
     # variable selection: p value, AIC
     # relative change in estiamte > 10%
     meet crtieria: associations between C with X and Y; check if C is not the mediator between X -> Y
   # Indicated impact of coufounder on outcome variable;  C has impact on both X and Y
   # Include all confounders in the model while reporting that residuals confounders do not have large impact on the result
 
  # C -> X    C -> Y    Direction   Change from unadjusted to adjusted estiamte
  # direct    direct    positive    unadjusted > adjusted 
  # direct    inverse   negative    unadjusted < adjusted 
  # direct    inverse   positive    unadjusted > adjusted 
  # inverse   direct    negative    unadjusted < adjusted 

  # Multi-level structure 
    # Violating assumption of homoscedasticity
  # Measurement error
    # Imprecise measurement in predictors will attenuate estimated coefficients toward zero
  # Interaction Effect
  # Mediation
    # Baron and Kenny criteria

## multicollinearity
mvc <- read.csv("http://web.hku.hk/~ehylau/mvc.csv")
mvc$height.sq=mvc$height^2
pairs(mvc)      # no strong patterns between variables if it is not in a linear pattened 
summary(lm(MVC~age+height+height.sq, data=mvc))   # check r^2, estimate and std. error to see if SE inflated
# mvc$height.c=mvc$height-mean(mvc$height)        # create centered term

library(car)
mvc.lm3 <- lm(MVC~age+height+height.sq, data=mvc) # 
vif(mvc.lm3)                                      # both height and height.sq have VIF>>10


mvc$ct.height <- scale(mvc$height, scale=F)       # standardizing (centering) variable; "scale" : division of standard deviation
mvc$ct.height.sq <- mvc$ct.height^2 
mvc.lm4 <- lm(MVC~age+ct.height+ct.height.sq, data=mvc) # VIF is lower than 10, multicollinearity is solved
# mvc.lm4 <- lm(MVC~age+height+ct.height.sq, data=mvc) # same result with different scale of intercepts
vif(mvc.lm4)   

## confounding, moderation, mediation
cardio = read.table("C:/Users/chens/OneDrive/research/2school/PhD Courses/CMED 6020 MMPH6117 Advanced Statistical Methods I/3 GLM/GLM3/Example - cardio.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)

  # confounder check-up 1: path analysis
  # X: phy; Y: sfrs; C: ses  
summary(lm(phy~ses, data=cardio))         # c->x
summary(lm(sfrs~ses, data=cardio))        # c->y
summary(lm(ses~phy, data=cardio))         # x->c; whether c being mediator of x->y

  # confounder check-up 2: relative change in estimate
summary(lm(sfrs~phy, data=cardio))        # obtain coefficient of phy=-0.0348
summary(lm(sfrs~phy+ses, data=cardio))    # coefficient of phy=-0.0445; relative change=(445-348)/348=27.9% > 10%, indicating confounding effect from ses
summary(lm(sfrs~phy+age, data=cardio))    # coefficient of phy=-0.03505; relative change=(3505-3480)/3480=0.7% < 10%, indicating non-confounding effect from age

  # confounder check-up 3: AIC
require(MASS) 
cf2 <- lm(sfrs~phy+ses, data=cardio) 
stepAIC(cf2)       # initial model has the lowest AIC. If suspecting SES being the confounder, the AIC will increase. ##??increase of AIC indicates SES is the confounder; testing a series of assumptions; p value here indicates significance of null hypothesis of each model in the stepping process

## interaction effect
age.int <- lm(sfrs~phy*age+ses, data=cardio)  # in format of "exposure" * "effect modifier" + "confounder", such as "phy+age+phy*age+ses"
# A*B = main effects of A and B, interaction term of A and B
summary(age.int)   # check significance level of interaction term 

  # compare model without interaction term to gain the main effect
summary(lm(sfrs~phy+age+ses, data=cardio))

## mediation
  # Approach 1
  # Steps: M ~ X; Y ~ M; Y ~ X; Y ~ X + M (for reference to evaluate direct and indirect effects)
  # 1. indirect effect of x -> m
summary(lm(bmi~phy, data=cardio)) 
  # 2. indirect effect of m -> y
summary(lm(sfrs~bmi, data=cardio)) 
  # 3. direct effect of x -> y
summary(lm(sfrs~phy, data=cardio))        # Bp=-0.035
  # 4. overall model for comparison
summary(lm(sfrs~phy+bmi, data=cardio))    # Bp=-0.041; beta is not attenuated, indicating the mediation effect does not hold

  # Approach 2.1
  # Sobel' test. alpha: X->M; beta: M->Y in Y~X+M; tau: X -> Y
alpha = coef(summary(lm(bmi~phy, data=cardio)))[2,1]        # M~X
alphaSE = coef(summary(lm(bmi~phy, data=cardio)))[2,2]      
beta = coef(summary(lm(sfrs~phy+bmi, data=cardio)))[3,1]    # Y~X+M
betaSE = coef(summary(lm(sfrs~phy+bmi, data=cardio)))[3,2]
z = alpha*beta/sqrt(alpha^2*betaSE^2+beta^2*alphaSE^2)      # Sobel's test
p = 2*pnorm(z,lower.tail=F)                                 # p value for sobel's test

  # Approach 2.2
  # Other Sobel's test
library(bda)
with(cardio,mediation.test(bmi,phy,sfrs)) # mediation.test(mv,iv,dv); significant result indicates significant mediator


# sobel's test = the mediation path is significantly from 0

library(bda)
mediation.test(mv,iv,dv)  # mv mediator
mediation.test(cardio$bmi,cardio$phy,cardio$sfrs)

with(cardio, mediation.test(bmi, phy, sfrs))

# SESSION 6 PSA ####################################################################################

## Case control study
  # types of case control study: matched case-control study, nested case-control study, risk set sampling
  # matching: balancing certain characteristics between groups to increase efficiency
  # types of matching: individually matching (paired; 1v1, 1vX), frequency matching

## Conditional logistic regression
  # logit(Pij) = Ai + B1 X1ij +... 
    # A - alpha, characteristics of each stratum (matched set)
    # B - beta, coefficient
    # i = the matched set
    # j = individuals

library(survival) # for clogit  
# case.status ~ exposure + strata(matched.set) 

mers = read.table("C:/Users/chens/OneDrive/research/2school/PhD Courses/CMED 6020 MMPH6117 Advanced Statistical Methods I/4 logistic and PSM/examplemers.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)

## conditional logistic regression
clr.mers <- clogit(case ~ dromedary + sheep + smoking + strata(strata), data=mers)  # strata: matched cases
summary(clr.mers)   # exposure to dromedary is significantly associated with MERS infection (exp(coef)OR=9.9, 95%CI=1.8-54.8); smoking is significantly associated with MERS infection (OR=14.9, 95%CI=2.6-87.1)

## PSA: the likelihood of selecting the specific options
  # To analyze quasi-experiment data
    # quasi experiment: little control on the allocation of treatment and associating factors
    # selection bias/group nonequivalence
      # treatment may tend to select patients with certain characteristics
      # patients with certain characteristics may select treatment
      # patients across different treatments may not be comparable
  # To balance observed characteristics across treatment 
    # so that more accurate estimates of the treatment effect can be estimated
  # Allow analysis on factors associated with treatment assignment
  # To adjust confounding effects

mi=read.table("C:/Users/chens/OneDrive/research/2school/PhD Courses/CMED 6020 MMPH6117 Advanced Statistical Methods I/4 logistic and PSM/examplemi.csv",sep=",", header=T, na.strings = "NA",stringsAsFactors = FALSE)

## summary 
summary(mi)
with(mi, table(trt, death)) # table of treatment options and mortality rate
round(with(mi, prop.table(table(trt, death),1)),3)  # proportion of mortality rate by treatment groups; prop.table - 1 by row, 2 by column; treatment group with 15.6% mortality rate, control with 19.2%
with(mi,t.test(death~trt))  # death rates are no different between groups

## graphics
library(ggplot2)
mi$trt <- as.factor(mi$trt) # turn continuous to categorical var
 
# death ~ age * treatment 
ggplot(mi, aes(x=age, fill=trt)) + geom_histogram(binwidth=1) + facet_grid(trt ~ .) 
# death ~ risk * treatment
ggplot(mi, aes(x=risk, fill=trt)) + geom_histogram(binwidth=1) + facet_grid(trt ~ .) 
# death ~ severity * treatment
ggplot(mi, aes(x=severity, fill=trt)) + geom_histogram(binwidth=1) + facet_grid(trt ~ .) 
ggplot(mi, aes(x=severity, fill=factor(trt))) + geom_histogram(binwidth=1) + facet_grid(trt ~ .) 

  # integrated table
h1n1 = read.table("C:/Users/chens/OneDrive/research/2school/PhD Courses/CMED 6020 MMPH6117 Advanced Statistical Methods I/Assignment/h1n1pdm.csv",header=T,sep=",",na.strings = "NA")

ftable(with(h1n1, table(et, agegp, psq)))[1:3,] 
round(prop.table(ftable(with(h1n1, table(et, agegp, psq)))[1:3,],2),3) 
ftable(with(h1n1, table(et, agegp, psq)))[4:6,] 
round(prop.table(ftable(with(h1n1, table(et, agegp, psq)))[4:6,],2),3)

ftable(with(h1n1, table(et, male, psq)))[c(2,4),] 
round(prop.table(ftable(with(h1n1, table(et, male, psq)))[1:2,],2)[2,],3) 
round(prop.table(ftable(with(h1n1, table(et, male, psq)))[3:4,],2)[2,],3)

ftable(with(h1n1, table(et, mv, psq)))[c(2,4),] 
round(prop.table(ftable(with(h1n1, table(et, mv, psq)))[1:2,],2)[2,],3) 
round(prop.table(ftable(with(h1n1, table(et, mv, psq)))[3:4,],2)[2,],3) 

ftable(with(h1n1, table(et, asthma, psq)))[c(2,4),] 
round(prop.table(ftable(with(h1n1, table(et, asthma, psq)))[1:2,],2)[2,],3) 
round(prop.table(ftable(with(h1n1, table(et, asthma, psq)))[3:4,],2)[2,],3) 

ftable(with(h1n1, table(et, copd, psq)))[c(2,4),] 
round(prop.table(ftable(with(h1n1, table(et, copd, psq)))[1:2,],2)[2,],3) 
round(prop.table(ftable(with(h1n1, table(et, copd, psq)))[3:4,],2)[2,],3) 


## Balance Approach 1 - Propensity Score
  # Functionality and rationale
    # the probability (propensity) of assigning to treatment for individuals
    # depends on the covariates / factors
    # does not depend on the outcome
    # to compare individuals with similar propensity scores (treatment vs. non-treatment)
    # any difference in the outcome should then be due to the treatment effect only
  # Assumptions
    # conditional independence / unconfoundedness: Y being independent from treatment assignment 
    # common support/overlap condition: a comparison group in each condition of treatment

  # modeling with example of MI data
ps.model <- glm(trt ~ age + risk + severity, data=mi, family=binomial) # treatment group with propensity score in consideration of age, risk and severity
mi$ps <- predict(ps.model, type='response')   # generate propensity score in each case for further matching

  # check distribution of data with propensity score 
ggplot(mi, aes(x=ps, fill=trt)) + geom_histogram(binwidth=0.01) + facet_grid(trt ~ .)   # more people choosing newer drug (treatment=1) with higher propensity score; common support, the comparison by condition of treatment emerges as the other variables are paired up

## Balance Approach 2 - Stratification - stratify by propensity scores into such as 5 groups with weighted mean of stratum-specified treatment effects 
ps.boundary <- quantile(mi$ps, 0:5/5)   # create 6 groups with even probability 
mi$psq <- cut(mi$ps, ps.boundary, right=F, include.lowest=T, label=1:5) # propensity grouping quintiles

  # death ~ age * treatment by PS strata
ggplot(mi, aes(x=trt, y=age)) + geom_boxplot(aes(fill=trt)) + facet_grid(psq ~ ., labeller=label_both) + coord_flip() # grouped by propensity score categories
  # death ~ risk (* counts) by PS strata
ggplot(mi, aes(x=risk)) + geom_histogram(binwidth=1, fill='blue') + facet_grid(trt ~ psq, labeller=label_both) # check distribution of treatment groups being similar or not
  # death ~ severity (* counts) by PS strata
ggplot(mi, aes(x=severity)) + geom_histogram(binwidth=1, fill='blue') + facet_grid(trt ~ psq, labeller=label_both) 

## Balance Approach 3 - T score for balance of treatment groups; t-test scores are modified greatly/adjusted by standard deviation
summary(lm(age~trt, data=mi))       # t score for age = 4.41 for unadjusted (in treatment==1); as well as for risk and severity
summary(lm(age~trt+psq, data=mi))   # t score for age = 0.73 as adjusted  (in treatment==1); as well as for risk and severity
  # distance (T score) shows the balanced distribution for treatment groups

## Treatment Effect Calculation 
mean.tp <- aggregate(death~trt+psq, data=mi, FUN=mean)    # Mean for each combination of treatment group and propensity groups
count.tp <- aggregate(death~trt+psq, data=mi, FUN=length) # counting numbers for each combinations of ...
cbind(mean.tp, count.tp$death) 

  # strata specific mean
str.mean <- mean.tp$death[mean.tp$et==1] - mean.tp$death[mean.tp$et==0] 

  # overall treatment effect 
n.psq <- as.numeric(table(h$psq))    # numbers of cases in each stratum
overall <- sum((mean.tp$death[mean.tp$et==1] - mean.tp$death[mean.tp$et==0])*n.psq)/sum(n.psq) 

  # overall treatment effect = (80*(0.04-0.11) + 78*(0.17-0.20) + 82*(0.17-0.15) + 80*(0.15-0.31) + 80*(0.20-0.25)) / 400 = -0.0603 ((treatment1-treatment0)* n)

  # variance of estimated treatment effect
var.tp <- aggregate(death~trt+psq, data=mi, FUN=var) 
cbind(var.tp, count.tp$death)   
sum((var.tp$death/count.tp$death)*rep(n.psq, each=2)^2)/sum(n.psq)^2

  # CI = M +/- SE = (-0.14, 0.02)
var.tp <- aggregate(death~et+psq, data=h1n1, FUN=var) 
var.over <- sum((var.tp$death/count.tp$death)*rep(n.psq, each=2)^2)/sum(n.psq)^2 
lowb <- overall - 1.96*sqrt(var.over) 
uppb <- overall + 1.96*sqrt(var.over) 
print(c(lowb, uppb)) 

## Propensity score matching function for case-control data
library(MatchIt)
m.out <- matchit(trt ~ age + risk + severity, data = mi, method = "nearest", subclass=20) # 20 classes using nearest neighbor method; interaction term can be added into the matching model

matched.mi <- match.data(m.out)       # extract cases for each class
matched.mi[matched.mi$subclass==1,]   # extract cases for certain class

  # calculation with propensity score
clr.mi <- clogit(death ~ trt + strata(subclass), data=matched.mi) 
summary(clr.mi)

## Propensity score weighting (inverse probability) to balance fair distribution 
  # treatment effect by PS weighting method
with(mi, sum(death/ps*(trt==1)-death/(1-ps)*(trt==0))/nrow(mi))   # 95%CI=(-0.163, 0.033)

  # logistic regression modeling with propensity score s the only predictor to predict outcome
lr.ps <- glm(death~trt+ps, data=mi, family=binomial) 
summary(lr.ps)  # OR for treamtent1 is 0.611 (trt1 coefficient)

exp(coef(lr.ps)) 
exp(confint(lr.ps)) # 95%CI=(0.35, 1.06)


# SESSION 7 Meta-analysis ####################################################################################

## Missing Data
  # potential reasons: dropouts, incomplete response, censored
  # measures: 
    # missing completely at random (MCAR) - missingness ~! all vars (complete case analysis; nearest neighbor imputation; mean imputation)
    # missing at random (MAR) - missingness ~! unobserved vars (regression imputation; multiple imputation; inverse probability wrighting)
    # missing not at random (MNAR) - missingness ~ unobserved vars

# Create data with missing values
set.seed(123) # unify our results
n <- 500
bmi.m <- runif(n, 25,40)
bmi.f <- runif(n, 15,30)
bmi.full <- c(bmi.m, bmi.f)	# the combined dataset
pi <- rep(c(1,0.2), each=n)	# as probability of 100% and 20%
obs <- rbinom(2*n,1,pi) 	# simulate the missing status
bmi.obs <- bmi.full[obs==1]

par(mfrow=c(1,2))
hist(bmi.obs, breaks=c(15,20,25,30,35,40))
hist(bmi.full, breaks=c(15,20,25,30,35,40))

mean(bmi.obs)   # result=30.8 this estimate is biased due to missing (completed case analysis)

# Inverse calculation with weights (## Inverse Probability Weighting (IPW))
pi.obs <- pi[obs==1]  # result = 27.1
sum(bmi.obs/pi.obs)/(2*n) # weighted calculation

## Propensity score weighting 
mi <- read.csv("examplemi.csv")
ps.model <- glm(trt ~ age + risk + severity, data=mi,
family=binomial)
mi$ps <- predict(ps.model, type='response')


## Meta Analysis
  # fixed effects model: 
    # assuming true effect of intervention is the same across studies. 
    # Homogeneous studies. 
    # outcome variable assumed to be normally distributed
  # random effects model: 
    # asumting true intervention effect of each study comes from a larger population.
    # Heterogenous studies
      # clinical diversity: Variability in participants, interventions and outcomes
      # methodological diversity: Variability in study design and risk of bias; need to control for confounders
      # statistical heterogeneity: 
        # Variability in the intervention effects across studies
        # Violate the assumption for fixed effects model
        # Random effects model allows the true effect to be different across studies
      # Clinical / methodological diversity should be addressed in the systematic review
      # Focus on statistical heterogeneity in the following slides (Cochrane Handbook for Systematic Reviews of Interventions, 2011)
      # Cochran's Q test: compare each estimate with their average. Larger Q indicates hetergeniety. Need large sample size.
      # Higgins' I^2: 0-30% low; 30-60% moderate; 50-90% substantial

  # effect size:
    # RR: relative risk - poisson regression 
    # OR: odds ratio - logistc regression
  # Inverse variance weighting
    # assign more weight to studies with higher precision (larger sample size)
    # the inverse of variance is roughly proportional to the sample size
    # the variance of the overall estiamte will be minimized

# RMA demonstration
ecig <- read.csv('d:/exampleecig.csv')

# Convert OR to log OR (beta in logistic regression; assumed to be normal)
ecig$logOR <- log(ecig$OR)

# Derive the standard error for log OR from the 95% CI
ecig$se.logOR <- (log(ecig$OR.ub)-log(ecig$OR.lb))/(2*1.96)

# Carry out meta analysis using fix effects model
require(metafor)
ecig.fe <- rma(yi=logOR, sei=se.logOR, slab=study,method="FE", data=ecig) # FE - fixed effect

  # names(ecig.fe)  # show variable names
with(ecig.fe, exp(c(b, ci.lb, ci.ub)))  # show beta and CI


summary(ecig.fe)  # show Q indicator
Q <- ecig.fe$QE
I2 <- (Q-(ecig.fe$k-1))/Q * 100  # high I2 indicates poor model; need random effect model

# random effect modeling 
ecig.re <- rma(yi=logOR, sei=se.logOR, slab=study,method="REML", data=ecig) # REML - random effect; having increased estiamtes and larger CI. 

with(ecig.re, exp(c(b, ci.lb, ci.ub)))

forest(ecig.re, transf=exp, refline=1)    # "showweights=T";"transf=exp" - transformation from logit beta to coefficient; "refline=0" - reference line

# funnel plot
funnel(ecig.re, atransf=exp)  # hetergeniety, biases
regtest(ecig.re)  # Egger's test; lowest sample size > 10; significance indicates asymmetry






# SESSION 8 Instrumental Variable ##############################################################################




###   R2wd - R to Word #############################################################################

tmp <-comGetObject("Word.Application") 
if(is.null(tmp))  
  tmp <- comCreateObject("Word.Application") 

tmp[["test"]]$Open("C:/Users/chens/Desktop/test.doc") 
.R2wd<-tmp 


###    Meta-Analysis  ##############################################################################
  
# Statistical meta-analysis with application, 7.234, 17.4.2
# ����������©��ͼ�Ļ�����R����е�ʵ��_������
# A Meta-analytic Review of Components Associated with Parent Training Program Effectiveness
# Overview of Meta-Analyses on Early Intensive Behavioral Intervention for Young Children with Autism Spectrum Disorders

160723
# p.77-85
# metacont for SMD (download metacont) ЧӦ��
# forest() (download) ɭ��ͼ
# funnel() ©��ͼ
# metabias() ƫ�з���
# metainf() (download metainf) ����Է���????????; forest(metainf())
# metainc() (download, p.105) - time-series analysis
# metagen() for year,region,lci,uci,se (download, p.109-111);
# require n1,n2,m1,sd1,m2,sd2
# SMD, cohen's d, r, effect size


######################################## SEM - path analysis

# Examples:
  Theory of planned-behavior - SEM
    moderation effect
    path analysis
  CES-D:
    model specification, correlations of items

# Book
  Bollen & Bobel, 2011: structural equations with latent variables

# Concpets
  similar concepts to SEM
  covariates: correlations of items

# software
  STATA: not useful
  Mplus: most useful

# requirement
  Observed variables (indcator)
    ordinal (up to 15 categories)
    continuous: 
      normally distributed: 
      non-normally distributed: 
    censored: above, below or both
    nominal: dummies
  latent variables (unobservable)

  # example
  SF-12/36 on health

  psychology - intelligence, depression, self-esteem, drinking behavior, feelings
  sociology - social class, well-being, ambition, discrimination
  economics - economic expectations, women eempowerment
  education - academic performance (ETS), science achievement
  business - customer satisfaction, customer loyalty
  poligical science - industrial development, political efficacy

  # graphical vocabulary
  latent variables: 
    feedback relation or reciprocal causation
    association between two variables 

  # measurement model - has causal relationships
  ksai (x being the measure)
  eta (y being the measure) 
  a construct usually needs 3 measured variables

library(lavaan)
library(lavaanPlot)
library(semPlot)
?HolzingerSwineford1939


# specify the model
HS.model = '
  visual =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed =~ x7 + x8 + x9'

fit = cfa(HS.model, data=HolzingerSwineford1939)
summary(fit,fit.measures=TRUE)

mi = modindices(fit)
mi[mi$op == "=~",]    # show the suggestion of change "=~"; each time only change one

model = '
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8
  dem60 ~ ind60
  dem65 ~ dem60 + ind60
  y1 ~~ y5
  y2 ~~ y4 + y6
  y3 ~~ y7
  y4 ~~ y8
  y6 ~~ y8
  '

fit = sem(model,data=PoliticalDemocracy)
summary(fit,fit.measure=T)
lavaanPlot(model=fit)


# chi-square: 
  # model fit test stats; should close to df
  # p value: the null hypothesis does not hold

# RMSEA: N0 < 0.06 acceptable

# Latent variable
  # p value: the item is significantly different from the 0




x3 affects both x1 and x2 and x2 affects x1 

library(lavaan) 
model1<-'x3 ~ x1 + x2 x2 ~ x1' 
fit1<-sem(model1) 
summary(fit1) #Summary of the fitted model 
coef(fit1) #check the coefficients 
parameterEstimates(fit1)  # as dataframe 


#### Output; regression model; model comparison ####
# packages = c("xlsx","outreg","plyr","psych","stargazer")
# sapply(packages,install.packages,character.only=T)
# sapply(packages,library,character.only=T)

 library(xlsx);library(outreg);library(plyr);library(psych);library(stargazer);
attach(mtcars)

# description table 0
table0=describe(mtcars)

# Regression Models
linear1=lm(mpg~hp,data=mtcars)
linear2=lm(mpg~hp+wt,data=mtcars)
linear3=lm(mpg~hp+wt+cyl,data=mtcars)
linear4=lm(mpg~hp+wt+cyl+hp*wt,data=mtcars)
linear5=lm(mpg~hp+wt+cyl+hp*wt+wt*cyl,data=mtcars)

  a1=stargazer(linear1,linear2,linear3,linear4,title="ModelResult",align=T,type="text",out="table.htm")
  a2=stargazer(attitude[1:6,],summary=T,rownames = F,title="Summary Table",align=T,type="text")
  a3=cbind(linear1,linear2,linear3,linear4)

   regt = list(
            lm(mpg~hp+wt+cyl,data=mtcars),
            lm(mpg~hp+wt+cyl,data=mtcars),
            lm(mpg~hp+wt+cyl+hp*wt,data=mtcars),
            lm(mpg~hp+wt+cyl+hp*wt+wt*cyl,data=mtcars))

# Correlation table 1
table1 = as.data.frame(cor(mtcars[,unlist(lapply(mtcars, is.numeric))]))

# Model Table 2
regs = list(linear1,linear2,linear3,linear4,linear5)
table2 = outreg(regt)

# Model comparison table 3
anova12=anova(linear1,linear2)
anova23=anova(linear2,linear3)
anova34=anova(linear3,linear4)
anova45=anova(linear4,linear4)
anova15=anova(linear1,linear5)
anova1t5= anova(linear1,linear2,linear3,linear4,linear5) 
table3 = rbind(anova12,anova23,anova34,anova45,anova1t5)

detach()

# Combine all tables
table = rbind.fill(table0,table1,table2,table3)     # require library(plyr)

# output to excel
write.xlsx(table,file="C:/Users/chens/Desktop/test.xlsx")

#anova12=anova(linear1,linear2)
#anova23=anova(linear2,linear3)
#anova34=anova(linear3,linear4)
#anova14=anova(linear1,linear4)
#stargazer(anova12,anova23,anova34,anova14,title="Model Comparison",align=T,type="text",out="table.htm")


############# Cognitive Diagnosis Modeling p.13 ##############
install.packages("GDINA")
library(GDINA)

### data reading
data1 <- read.table(file = "data1.dat", header = TRUE)
Q1 <- read.table(file = "Q1.txt")   # Q matrix

### G-DINA model fitting
fit1 <- GDINA(dat = data1, Q = Q1)
summary(fit1)   # show Log likelihood, AIC, BIC
coef(fit1)      # item success probabilities for each latent group; coef: withSE (estimate standard errors: T/F), what (what parameters to show: delta/lambda/IRF), item (which item to show)


### Bootcamp/Workshop on Linear Regression diagnostics by Clifton ###
## Notes:  y5 x5 is linearity.  y3 x3 is autocorrelation
bootcamp=read.table("C:/Users/chens/OneDrive/research/2school/PhD Courses/linear regression workshop/bootcamp.txt",header=T,sep="\t",na.strings = "NA")	# Dell
bootcamp=read.table("C:/Users/admin/OneDrive/research/2school/PhD Courses/linear regression workshop/bootcamp.txt",header=T,sep="\t",na.strings = "NA") # Lenovo


# Nonlinearity
linktest=function(y, reg) {
  fit=reg$fit
  fitsq=reg$fit^2
  link=lm(y~fit+fitsq)
  ret=summary(link)
  return(ret)
}


# Heteroskedasticity
# Breusch-Pagan.  https://en.wikipedia.org/wiki/Breusch%E2%80%93Pagan_test

library(stats)
hettest=function(reg, sampsize, predictornum) {
  fit=reg$fit
  resid=reg$res
  residsq=resid^2
  breusch=lm(residsq~fit)
  degfr=predictornum
  het=summary(breusch)
  pagan=het$r.squared*sampsize
  myhet=dchisq(pagan, degfr)
  return(myhet)
}

# Autocorrelation
arima(myreg, order=c(1,0,0))


# Outliers
install car
load car

influencePlot(myreg)


### CitNetExplorer - Scopus2CitNet package
library(Scopus2CitNet)
scopus_file = read.table(file = "test.csv", sep=",", na = "NA", header=T)
Scopus2CitNet(scopus_file)



