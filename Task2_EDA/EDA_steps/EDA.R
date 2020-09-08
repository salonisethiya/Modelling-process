setwd("C:/Users/saloni/Downloads")
# reading file
data<-read.csv('DataSet for EDA.csv',stringsAsFactors = FALSE,colClasses =c("appearances"="numeric"))
str(data)

#columns of data
names(data)

# number of records in data
nrow(data)

# calculating null values in each column
apply(data,2,function(x)sum(is.na(x)))


# summary of data
summary(data)


# visualizing the data
library(ggplot2)
library(DataExplorer)
plot_histogram(data$appearances)
boxplot(data$appearances)

library(ggplot2)
ggplot(data,aes(x=factor(id))) + geom_bar()
ggplot(data,aes(x=factor(align))) + geom_bar()
ggplot(data,aes(x=factor(hair))) + geom_bar()
ggplot(data,aes(x=factor(gender))) + geom_bar()
ggplot(data,aes(x=factor(gsm))) + geom_bar()
ggplot(data,aes(x=factor(alive))) + geom_bar()
ggplot(data,aes(x=factor(publisher))) + geom_bar()
ggplot(data,aes(x=factor(eye))) + geom_bar()

# Bar chart side by side
ggplot(data, aes(x = id, fill = align)) +
  geom_bar(position = position_dodge()) +
  theme_classic()

dataf<-data
# we can drop eye column and gsm column as 60% and 90% of the data respectively is null in it.
dataf$eye<-NULL
dataf$gsm<-NULL

# outlier treatment for continuous variable - "appearance"
boxplot(dataf$appearances)
# an outlier is considered so if it is below the first quartile - 1.5·IQR or above third quartile + 1.5·IQR.
qnt <- quantile(dataf$appearances, probs=c(.25, .75), na.rm = T)
caps <- quantile(dataf$appearances, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(dataf$appearances, na.rm = T)
H
dataf$appearances[dataf$appearances < (qnt[1] - H)] <- caps[1]
dataf$appearances[dataf$appearances > (qnt[2] + H)] <- caps[2]

#summary of appearance column
summary(dataf$appearances)


# deleting rows having null values in first_appear colum as it only has 884 missing values
# and also deleting rows having missing values in alive and gender column
data1<-dataf[!(is.na(dataf$first_appear)),]
data2<-data1[!(is.na(data1$gender)),]
data3<-data2[!(is.na(data2$alive)),]
data4<-data3
nrow(data3)
apply(data3,2,function(x)sum(is.na(x)))

mean1<-mean(data3$appearances,na.rm=TRUE)
median1<-median(data3$appearances,na.rm=TRUE)
val1 <- unique(data3$appearances[!is.na(data3$appearances)]) 
mode1 <- val[which.max(tabulate(match(data3$appearances, val)))]

# replacing missing values with median in appearance column
library(dplyr)

# imputed with median
data3<-data3 %>% 
  mutate(appearances
         =replace(appearances,is.na(appearances),median(appearances,na.rm=TRUE)))

plot_histogram(data3$appearances)
# imputed with mean
data5<-data4 %>% 
  mutate(appearances
         =replace(appearances,is.na(appearances),mean(appearances,na.rm=TRUE)))

plot_histogram(data5$appearances)
# imputed with mode
data6<-data4 %>% 
  mutate(appearances
         =replace(appearances,is.na(appearances),mode1))

plot_histogram(data6$appearances)
# mode for id variable
val2 <- unique(data3$id[!is.na(data3$id)]) 
mode2 <- val2[which.max(tabulate(match(data3$id, val2)))]
mode2

# imputing id column with mode
data3$id[is.na(data3$id)] <- mode2 

# mode for align variable
val3 <- unique(data3$align[!is.na(data3$align)]) 
mode3 <- val3[which.max(tabulate(match(data3$align, val3)))]
mode3

# imputing id column with mode
data3$align[is.na(data3$align)] <- mode3 

# mode for hair variable
val4 <- unique(data3$hair[!is.na(data3$hair)]) 
mode4 <- val4[which.max(tabulate(match(data3$hair, val4)))]
mode4

# imputing id column with mode
data3$hair[is.na(data3$hair)] <- mode4

# now has zero missing values in data
apply(data3,2,function(x)sum(is.na(x)))
nrow(data3)

# visualizing the data
library(DataExplorer)
plot_histogram(data$appearances)
plot_histogram(data3)

library(ggplot2)
ggplot(data3,aes(x=factor(id))) + geom_bar()
ggplot(data3,aes(x=factor(id))) + geom_bar()
