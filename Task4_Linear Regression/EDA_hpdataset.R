setwd("C:/Users/saloni/Downloads")
# reading file
data<-read.csv("3102_UIDs_Integrated_data_Training.csv",na.strings='NULL',stringsAsFactors = FALSE,colClasses =c("Actuals"="numeric"))
data
names(data)
summary(data)

# the number of unique values in each column
sapply(data,function(x) length(unique(x)))


# dropping below variables as 75% of them have missing values
data$NPS_Overall_Performance_HP<-NULL
data$NPS_Ease_Conducting_Business<-NULL
data$NPS_Recommend_HP<-NULL

# distribution of sales metric (PPQD_Bucket)

library(ggplot2)
ggplot(data,aes(x=(PPQD_Bucket))) + geom_bar()+
theme(axis.text.x = element_text(angle = 45, hjust = 1))

# distribution of PPQD_Bucket i.e. the buckets contribution to actual

library(dplyr)
v1<-data %>%
  group_by(PPQD_Bucket) %>%
  summarise(Actuals=sum(Actuals,na.rm=TRUE))
v2<-v1[order(-v1$Actuals),]
v2
v3<-data.frame(v2)
v3

# plot for above
library(ggplot2)
ggplot(v3,aes(x=(PPQD_Bucket),y=Actuals)) + geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90,hjust=1))

# distribution of PPID i.e. the partner id's contribution to actuals

library(dplyr)
v1<-data %>%
  group_by(PPID) %>%
  summarise(Actuals=sum(Actuals,na.rm=TRUE))
v2<-v1[order(-v1$Actuals),]
v2
v3<-data.frame(v2)
v3

# by dividing actuals of PPID by total actual sum
total_sum<-sum(data$Actuals,na.rm=TRUE)
total_sum
library(dplyr)
v1<-data %>%
  group_by(PPID) %>%
  summarise(Actuals=sum(Actuals,na.rm=TRUE)/total_sum)
v2<-v1[order(-v1$Actuals),]
v2
v3<-data.frame(v2)
v3


# distribution of countries i.e. order of countries giving sale to hp

library(dplyr)
v1<-data %>%
  group_by(World_Country_Region) %>%
  summarise(Actuals=sum(Actuals,na.rm=TRUE))
v2<-v1[order(-v1$Actuals),]
v2
v3<-data.frame(v2)
v3

# plot for countries contribution to sale for hp
library(ggplot2)
ggplot(v3,aes(x=(World_Country_Region),y=Actuals)) + geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90,hjust=1))


# order of channels giving sale to hp

library(dplyr)
v1<-data %>%
  group_by(QCV_Segment) %>%
  summarise(Actuals=sum(Actuals,na.rm=TRUE))
v2<-v1[order(-v1$Actuals),]
v2
v3<-data.frame(v2)
v3


# plot for channels contribution to sale for hp
library(ggplot2)
ggplot(v3,aes(x=(QCV_Segment),y=Actuals)) + geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90,hjust=1))


# number of distinct countries a channel belongs to
library(dplyr)
v1<-data %>%
  group_by(QCV_Segment) %>%
  summarise(n_distinct(World_Country_Region))
v1
v2<-v1[order(v1$`n_distinct(World_Country_Region)`),]
v2
v3<-data.frame(v2)
v3

# plot for distinct countries a channel belongs to
library(ggplot2)
ggplot(v3,aes(x=(QCV_Segment),y=`n_distinct.World_Country_Region.`)) + geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90,hjust=1))

# distribution on PPQD Bucket, PPID and country level
v1<-data %>%
  group_by(PPQD_Bucket,PPID,World_Country_Region) %>%
  summarise(Actuals=sum(Actuals,na.rm=TRUE))
v1
v2<-v1[order(v1$Actuals),]
v2
v3<-data.frame(v2)
v3


# distribution on PPQD Bucket, PPID,country and QCV segment level
v1<-data %>%
  group_by(PPQD_Bucket,PPID,World_Country_Region,QCV_Segment) %>%
  summarise(Actuals=sum(Actuals,na.rm=TRUE))
v1
v2<-v1[order(v1$Actuals),]
v2
v3<-data.frame(v2)
v3

# which partner is having more products

library(dplyr)
v1<-data %>%
  group_by(PPID) %>%
  summarise(n_distinct(PPQD_Bucket))
v2<-v1[order(v1$`n_distinct(PPQD_Bucket)`),]
v2
v3<-data.frame(v2)


# difference between market size units and market share units for different products
library(dplyr)
v1<-data %>%
  group_by(PPQD_Bucket) %>%
  summarise(Market_Size_Units=sum(Market_Size_Units,na.rm=TRUE),Market_Share_Units=sum(Market_Share_Units,na.rm=TRUE),difference=sum(Market_Size_Units,na.rm=TRUE)-sum(Market_Share_Units,na.rm=TRUE))
v1
v2<-v1[order(-v1$difference),]
v2
v3<-data.frame(v2)
v3

#--------------------------------------------------------------------------------------------------------------

# Program status distribution as per partners
library(dplyr)
v1<-data %>%
  group_by(Partner_Name) %>%
  count(Program_Status)
v1
v2<-v1[order(-v1$Program_Status),]
v2
v3<-data.frame(v2)
v3

#------------------------------------------------------------------------------------------------------------

data1<-data
# missing value treatment

# Country_ISO_Code has 0.13% of missing value, thus imputing it through mode
val2 <- unique(data$Country_ISO_Code[!is.na(data$Country_ISO_Code)]) 
mode2 <- val2[which.max(tabulate(match(data$Country_ISO_Code, val2)))]
mode2

data1$Country_ISO_Code[is.na(data1$Country_ISO_Code)] <- mode2 
apply(data1,2,function(x)(sum(is.na(x))))

summary(data$Actuals)


# imputing missing values in Actuals column with median
median1<-median(data$Actuals,na.rm=TRUE)
median1
# imputed with median
data1<-data %>% 
  mutate(Actuals
         =replace(Actuals,is.na(Actuals),median(Actuals,na.rm=TRUE)))


# imputed missing values in Market_Share_Units with median
data1<-data1 %>% 
  mutate(Market_Share_Units
         =replace(Market_Share_Units,is.na(Market_Share_Units),median(Market_Share_Units,na.rm=TRUE)))

# imputed missing values in Market_Share_Revenue with median
data1<-data1 %>% 
  mutate(Market_Share_Revenue
         =replace(Market_Share_Revenue,is.na(Market_Share_Revenue),median(Market_Share_Revenue,na.rm=TRUE)))

# imputed missing values in Market_Size_Units with median
data1<-data1 %>% 
  mutate(Market_Size_Units
         =replace(Market_Size_Units,is.na(Market_Size_Units),median(Market_Size_Units,na.rm=TRUE)))

# imputed missing values in Market_Size_Revenue with median
data1<-data1 %>% 
  mutate(Market_Size_Revenue
         =replace(Market_Size_Revenue,is.na(Market_Size_Revenue),median(Market_Size_Revenue,na.rm=TRUE)))

# imputed missing values in MEI_CPI_Inflation with median
data1<-data1 %>% 
  mutate(MEI_CPI_Inflation
         =replace(MEI_CPI_Inflation,is.na(MEI_CPI_Inflation),median(MEI_CPI_Inflation,na.rm=TRUE)))

# imputed missing values in MEI_CPI_Inflation with median
data1<-data1 %>% 
  group_by(World_Country_Region) %>%
  mutate(MEI_Exports
         =replace(MEI_Exports,is.na(MEI_Exports),median(MEI_Exports,na.rm=TRUE)))

# imputed missing values in MEI_Exports with median
data1<-data1 %>% 
  mutate(MEI_CPI_Inflation
         =replace(MEI_CPI_Inflation,is.na(MEI_CPI_Inflation),median(MEI_CPI_Inflation,na.rm=TRUE)))

# imputed missing values in MEI_Fixed_Investment with median
data1<-data1 %>% 
  mutate(MEI_Fixed_Investment
         =replace(MEI_Fixed_Investment,is.na(MEI_Fixed_Investment),median(MEI_Fixed_Investment,na.rm=TRUE)))

# imputed missing values in MEI_GDP with median
data1<-data1 %>% 
  mutate(MEI_GDP
         =replace(MEI_GDP,is.na(MEI_GDP),median(MEI_GDP,na.rm=TRUE)))

# imputed missing values in MEI_Government_Growth with median
data1<-data1 %>% 
  mutate(MEI_Government_Growth
         =replace(MEI_Government_Growth,is.na(MEI_Government_Growth),median(MEI_Government_Growth,na.rm=TRUE)))

# imputed missing values in MEI_Imports with median
data1<-data1 %>% 
  mutate(MEI_Imports
         =replace(MEI_Imports,is.na(MEI_Imports),median(MEI_Imports,na.rm=TRUE)))

# imputed missing values in MEI_Industrial_Production with median
data1<-data1 %>% 
  mutate(MEI_Industrial_Production
         =replace(MEI_Industrial_Production,is.na(MEI_Industrial_Production),median(MEI_Industrial_Production,na.rm=TRUE)))

# imputed missing values in MEI_Merchandise_Exports with median
data1<-data1 %>% 
  mutate(MEI_Merchandise_Exports
         =replace(MEI_Merchandise_Exports,is.na(MEI_Merchandise_Exports),median(MEI_Merchandise_Exports,na.rm=TRUE)))

# imputed missing values in MEI_Merchandise_Imports with median
data1<-data1 %>% 
  mutate(MEI_Merchandise_Imports
         =replace(MEI_Merchandise_Imports,is.na(MEI_Merchandise_Imports),median(MEI_Merchandise_Imports,na.rm=TRUE)))

# imputed missing values in MEI_Nominal_Retail_Sales with median
data1<-data1 %>% 
  mutate(MEI_Nominal_Retail_Sales
         =replace(MEI_Nominal_Retail_Sales,is.na(MEI_Nominal_Retail_Sales),median(MEI_Nominal_Retail_Sales,na.rm=TRUE)))

# imputed missing values in MEI_Private_Consumption with median
data1<-data1 %>% 
  mutate(MEI_Private_Consumption
         =replace(MEI_Private_Consumption,is.na(MEI_Private_Consumption),median(MEI_Private_Consumption,na.rm=TRUE)))

# imputed missing values in MEI_Real_Retail_Sales with median
data1<-data1 %>% 
  mutate(MEI_Real_Retail_Sales
         =replace(MEI_Real_Retail_Sales,is.na(MEI_Real_Retail_Sales),median(MEI_Real_Retail_Sales,na.rm=TRUE)))

# imputed missing values in MEI_WPI_Inflation with median
data1<-data1 %>% 
  mutate(MEI_WPI_Inflation
         =replace(MEI_WPI_Inflation,is.na(MEI_WPI_Inflation),median(MEI_WPI_Inflation,na.rm=TRUE)))

# imputed missing values in MEI_Price_Index with median
data1<-data1 %>% 
  mutate(MEI_Price_Index
         =replace(MEI_Price_Index,is.na(MEI_Price_Index),median(MEI_Price_Index,na.rm=TRUE)))

# imputed missing values in MEI_Trade_GDP_Ratio with median
data1<-data1 %>% 
  mutate(MEI_Trade_GDP_Ratio
         =replace(MEI_Trade_GDP_Ratio,is.na(MEI_Trade_GDP_Ratio),median(MEI_Trade_GDP_Ratio,na.rm=TRUE)))

# imputed missing values in MEI_Merchandise_Trade_GDP_Ratio with median
data1<-data1 %>% 
  mutate(MEI_Merchandise_Trade_GDP_Ratio
         =replace(MEI_Merchandise_Trade_GDP_Ratio,is.na(MEI_Merchandise_Trade_GDP_Ratio),median(MEI_Merchandise_Trade_GDP_Ratio,na.rm=TRUE)))

# imputed missing values in MEI_Real_Nominal_Sales with median
data1<-data1 %>% 
  mutate(MEI_Real_Nominal_Sales
         =replace(MEI_Real_Nominal_Sales,is.na(MEI_Real_Nominal_Sales),median(MEI_Real_Nominal_Sales,na.rm=TRUE)))


apply(data1,2,function(x)(sum(is.na(x))))

# correaltion between variables
mydata<-data1[,c(11:32)]
corr<-cor(mydata)
library(ggcorrplot)
ggcorrplot(corr)






