mydata = read.csv("Red-wine.csv")
mydata

# create a list of 80% of the rows in the original dataset we can use for training
dim(mydata)
# list types for each attribute
sapply(mydata, class)
# take a peek at the first 5 rows of the data
head(mydata)
2
# summarize attribute distributions
summary(mydata)


# split input and output
x <- mydata[,1:11]
y <- mydata[,12]



# boxplot for each attribute on one image
library(ISLR)
library(ggplot2)
library(caret)
# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")