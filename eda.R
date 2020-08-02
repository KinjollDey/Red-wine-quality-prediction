
library("ggplot2")
library("dplyr")
library("gridExtra")
library(Simpsons)
library(GGally)
library(memisc)
library(pander)
library(corrplot)

directory<-"C:/Users/Md Khalid Siddique/Desktop/Red-wine"

m<-file.path(directory,"Redwine.csv")

wine <- read.csv(m)

head(wine)

tail(wine)


#Converting Wine quality into a ordered factor
wine$quality <- factor(wine$quality, ordered = T)

#Creating a new 'rating' variable into the dataframe for different quality range

wine$rating <- ifelse(wine$quality < 5, 'bad', ifelse(
  wine$quality < 7, 'average', 'good'))
wine$rating <- ordered(wine$rating,
                       levels = c('bad', 'average', 'good'))

####Univariate analysis
#Quality and rating
ggplot(data = wine, aes(x = quality)) +
  stat_count(width = 1, color = 'black',fill = I('orange'))

ggplot(data = wine, aes(x = rating)) +
  stat_count(width = 1, color = 'black',fill = I('blue'))

#Fixed acidity (Positively skewed)
p1 <- ggplot(data = wine, aes(x = fixed.acidity)) +
  geom_histogram(binwidth = 1, color = 'black',fill = I('orange'))

summary(wine$fixed.acidity)  #Median = 7.9 but some outliers dragged the mean upto 8.32


#Volatile acidity(Maybe a little bimodality)

summary(wine$volatile.acidity)


p2 <- ggplot(data = wine, aes(x = volatile.acidity)) +
  geom_histogram(binwidth = 0.05, color = 'black',fill = I('orange'))

#Citric acid(Positively skewed)
summary(wine$citric.acid)


p3 <- ggplot(data = wine, aes(x = citric.acid)) +
  geom_histogram(binwidth = 0.08, color = 'black',fill = I('orange')) +
  scale_x_continuous(breaks = seq(0,1,0.1), lim = c(0,1))


#Residual sugar (Strong freq around median with a few outliers)
summary(wine$residual.sugar)


p4 <- ggplot(data = wine, aes(x = residual.sugar)) +
  geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange'))

#Chlorides(Similar dist like Residual sugar)
summary(wine$chlorides)

p5 <- ggplot(data = wine, aes(x = chlorides)) +
  geom_histogram(binwidth = 0.01, color = 'black',fill = I('orange'))

#Free Sulphur dioxide(Large peak at 7. Positively skewed)

summary(wine$free.sulfur.dioxide)

p6 <- ggplot(data = wine, aes(x = free.sulfur.dioxide)) +
  geom_histogram(binwidth = 1, color = 'black',fill = I('orange')) +
  scale_x_continuous(breaks = seq(0,80,5))

#Total Sulphur dioxide(Similar to last one)

summary(wine$total.sulfur.dioxide)

p7 <- ggplot(data = wine, aes(x = total.sulfur.dioxide)) +
  geom_histogram(binwidth = 5, color = 'black',fill = I('orange'))

#Density(Has a very normal distribution)

summary(wine$density)

p8 <- ggplot(data = wine, aes(x = density)) +
  geom_histogram(binwidth = 0.001, color = 'black',fill = I('orange'))

#pH(Has a very normal distribution)

summary(wine$pH)

p9 <- ggplot(data = wine, aes(x = pH)) +
  geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange'))

#Sulphates(Positively skewed. Similar to Chlorides and residual sugar)

summary(wine$sulphates)

p10 <- ggplot(data = wine, aes(x = sulphates)) +
  geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange'))


#Alcohol(Positively skewed)

summary(wine$alcohol)

p11 <- ggplot(data = wine, aes(x = alcohol)) +
  geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange'))

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11, ncol = 4)


#Bivariate analysis
#Fixed acidity : Doesn't seem to have much effect
ggplot(data = wine, aes(x = quality, y = fixed.acidity)) +
  geom_boxplot()

#Volatile Acidity : Seems to have negative effect. With increase, quality seems to go down
ggplot(data=wine, aes(x = quality, y = volatile.acidity)) +
  geom_boxplot()

#Citric acid (Better wines tend to have higher citric acid)
ggplot(data=wine, aes(x=quality, y=citric.acid)) +
  geom_boxplot()

#Residual Sugar(Almost has no effect to quality. This is contrary to previous assumption)

ggplot(data=wine, aes(x=quality, y=residual.sugar)) +
  geom_boxplot()

#Chlorides

ggplot(data=wine, aes(x=quality, y=chlorides)) +
  geom_boxplot()

#Free SO2(We see too little and we get a poor wine and too much : we get an average wine)

ggplot(data=wine, aes(x=quality, y=free.sulfur.dioxide)) +
  geom_boxplot()

#Total SO2(Just like free SO2)

ggplot(data=wine, aes(x=quality, y=total.sulfur.dioxide)) +
  geom_boxplot()

#Density(Better wines tend to have lower densities but is it due to alcohol content?)

ggplot(data=wine, aes(x=quality, y=density)) +
  geom_boxplot()

#pH(Better wines seems to be more acidic. Now let's see contribution of each acid on pH)

ggplot(data=wine, aes(x=quality, y=pH)) +
  geom_boxplot()

