#page 26
mydata = data.frame(Girth=c(8.3,8.6,8.8,10.5,10.7),
                    Height=c(70,65,63,72,81),
                    Volume=c(10.3,10.3,10.2,16.4,18.8))

#page 28
A <- c("blue","blue","red","red","yellow","blue","red")
B <- c("a","b","a","a","b","b","b")
C <- c("yes","yes","yes","no","no","yes","no")


#page 30
plot(rnorm(100), type="l", col="blue")

#page 31
hist(mtcars$mpg)

hist(mtcars$mpg, breaks=12, col="red", xlab = "Miles per gallon")

#page 32
counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution
  Year 2010",xlab="Number of Gears", ylab = "Frequency", col = "yellow")

#page 33
counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution", horiz=TRUE,
        names.arg=c("3 Gears", "4 Gears", "5 Gears"), col="blue",
        xlab="Frequency")

#page 34
counts <- table(mtcars$vs, mtcars$gear)
stack <-barplot(counts, main="Car Distribution by Gears and VS",
                xlab="Number of Gears", col=c("darkblue","red"),
                legend = rownames(counts))
text(stack,8,labels=c(15,12,5))

#page 35
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)


#page 36
slices <- c(10, 12,4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie(slices, labels = lbls, main="Pie Chart of Countries",
    col = c("red","blue","yellow","green","orange"))


#page 37
slices <- c(10, 12, 4, 16, 8) 
lbls <- c("US", "UK", "Australia", "Germany", "France")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")


#page 38
boxplot(mpg~cyl,data=mtcars, main="Car Mileage Data", 
        xlab="Number of Cylinders", ylab="Miles Per Gallon")


#page 39
attach(mtcars)
plot(wt, mpg, main="Scatterplot Example",
     xlab="Car Weight ", ylab="Miles Per Gallon")


#page 40
attach(mtcars)
plot(wt, mpg, xlab="Car Weight ", ylab="Miles Per Gallon") 
abline(lm(mpg~wt))
title("Regression of MPG on Weight")


#page 41
a = c(65, 78, 88, 55, 48, 95, 66, 57, 79, 81)

t.test (a, mu=75)

#page 42
a = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
b = c(12.7, 13.6, 12.0, 15.2, 16.8, 20.0, 12.0, 15.9, 16.0, 11.1)

t.test(a,b, paired=TRUE)

#page 43
a = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
b = c(12.0, 12.2, 11.2, 13.0, 15.0, 15.8, 12.2, 13.4, 12.9, 11.0)

t.test(a,b, paired=TRUE, alt="greater")

a = c(11.9, 11.5, 10.8, 11.6, 12.2, 12.2, 13.6, 11.3, 11.4, 13.3)
b = c(12.1, 11.8, 10.2, 11.0, 13.0, 12.8, 12.2, 13.4)

t.test(a,b, alt="less")

#page 44
df <- read.csv("https://goo.gl/j6lRXD")
table(df$treatment, df$improvement)

chisq.test(df$treatment, df$improvement)

#page 45
alligator = data.frame(
  lnLength = c(3.87, 3.61, 4.33, 3.43, 3.81, 3.83, 3.46, 3.76,
               3.50, 3.58, 4.19, 3.78, 3.71, 3.73, 3.78),
  lnWeight = c(4.87, 3.93, 6.46, 3.33, 4.38, 4.70, 3.50, 4.50,
               3.58, 3.64, 5.90, 4.43, 4.38, 4.42, 4.25)
)

plot(lnWeight ~ lnLength, data = alligator,
     xlab = "Snout vent length (inches) on log scale",
     ylab = "Weight (pounds) on log scale",
     main = "Alligators in Central Florida"
)


#page 46
alli.mod1 = lm(lnWeight ~ lnLength, data = alligator)

summary(alli.mod1)
