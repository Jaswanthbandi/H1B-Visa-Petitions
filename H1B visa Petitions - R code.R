## H1B Visa Petitions 2011 - 2016 ##
##Data Analytics II - Jaswanth ##

library(dplyr)
library(caTools)
library(rpart)
library(randomForest)
library(ggplot2)

## STEP 1: Reading the data

#reading the data from kaggle file
data = read.csv("h1b_kaggle.csv", header = TRUE, na.strings=c("", "NA"," "))

#making a copy of the original data for use
petitions <- data

#dimensions of the data
dim(petitions)

#lets look at the head and tail of the daa
head(petitions)
tail(petitions)
## from the tail output we see that there are NA values in the data

#looking at the column names
names(petitions)
    ## in this step we see that the variable names
    ## X refers to the case number and all other variables are relevant information about petitions
    ## we also get an understanding of the information from names

#lets see the strucutre of object petitions
str(petitions)
    ## we see that most variables are character types
    ## only the year and serial number are integer types

#a quick summary
summary(petitions)
    ## a quick summary shows is that there are atleast 107242 missing values from columns lon and lat
    


## STEP 2: Cleaning the data

#from the previous steps we see that there are NA values
#finding NA values using sapply
sapply(petitions, function(x) sum(is.na(x)))
    ## since the dataset has more than 3 million values, we can omit the 100k missing values

#removing the NA values
petitions <- na.omit(petitions)
    ## we see that missing values got removed and we are left with 2877783 observations

#checking outliers in object petitions
#since numerical values are only the default serial numbers and wages, we will work with wages
boxplot(petitions$PREVAILING_WAGE)
## this doesn't give us a clear picture of outliers, we will look at the boxplot stats

boxplot.stats(petitions$PREVAILING_WAGE)
## under $out, we see that there are 109011 outliers out of 2877783 observations

summary(petitions$PREVAILING_WAGE)
    ## however the reason for so many outlying values is the huge difference in wages of people
    ## the minimum wage from stats is $14580.80 while the maximum value is 6997606720
    ## a person's wage is an important criteria for H1B eligibility 
    ## therefore, we are not taking most values as outliers except the following cases

#calculating minimum,mean and maximum prevailing wages
min(petitions$PREVAILING_WAGE)
mean(petitions$PREVAILING_WAGE)
max(petitions$PREVAILING_WAGE)

#removing values keeping lowerbound as 60K (as per government norms)
petitions = subset(petitions, petitions$PREVAILING_WAGE > 60000)

#removing values keeping upperbound as $150K for wages
petitions = subset(petitions, petitions$PREVAILING_WAGE < 150001)

#checking the deviation
sd(petitions$PREVAILING_WAGE)


## STEP 3: Simplifying the data

#converting N and Y under FULL_TIME_POSITION to 0 and 1
levels(petitions$FULL_TIME_POSITION) <- c("0","1")
table(petitions$FULL_TIME_POSITION)

#reducing dimensionality
petitions <- subset(petitions, select = -c(SOC_NAME))

#since we will be working with two CASE_STATUS "Certified" and "Denied", we will simplify the data
subpetitions <- subset(petitions, (petitions$CASE_STATUS == "CERTIFIED" | petitions$CASE_STATUS == "DENIED" ))
subpetitions$EMPLOYER_NAME <- factor(subpetitions$EMPLOYER_NAME)

subpetitions$CASE_STATUS <- factor(subpetitions$CASE_STATUS)
table(subpetitions$CASE_STATUS)

#finding top employers
topemployeroverall <- as.data.frame(subpetitions %>% group_by(EMPLOYER_NAME) %>%
                                summarise(count = n(), percent = round(count*100/nrow(subpetitions),1)) %>% 
                                arrange(desc(count))%>% 
                                top_n(10, wt = count))

topemployeroverall

#splitting the data yearwise
table(subpetitions$YEAR)

subpetitions2011 <- subset(subpetitions, (subpetitions$YEAR == 2011))
subpetitions2012 <- subset(subpetitions, (subpetitions$YEAR == 2012))
subpetitions2013 <- subset(subpetitions, (subpetitions$YEAR == 2013))
subpetitions2014 <- subset(subpetitions, (subpetitions$YEAR == 2014))
subpetitions2015 <- subset(subpetitions, (subpetitions$YEAR == 2015))
subpetitions2016 <- subset(subpetitions, (subpetitions$YEAR == 2016))


## 2015
#tabulating variables
subpetitions2015$EMPLOYER_NAME <- factor(subpetitions2015$EMPLOYER_NAME)
subpetitions2015 <- subset(subpetitions2015, EMPLOYER_NAME == "INFOSYS LIMITED" | EMPLOYER_NAME == "DELOITTE CONSULTING LLP " | EMPLOYER_NAME == "TATA CONSULTANCY SERVICES LIMITED" | EMPLOYER_NAME == "WIPRO LIMITED" | EMPLOYER_NAME == "ACCENTURE LLP" | EMPLOYER_NAME == "IBM INDIA PRIVATE LIMITED ")


employer2015 <- as.data.frame(subpetitions2015 %>% group_by(EMPLOYER_NAME) %>%
                                summarise(count = n(), percent = round(count*100/nrow(subpetitions2015),1)) %>% 
                                arrange(desc(count))%>% 
                                top_n(10, wt = count))
employer2015


jobtitles2015 <- as.data.frame(subpetitions2015 %>% group_by(JOB_TITLE) %>%
                                    summarise(count = n(), percent = round(count*100/nrow(subpetitions2015),1)) %>% 
                                    arrange(desc(count))%>% 
                                    top_n(15, wt = count))

jobtitles2015


library(dplyr)
set.seed(134)
datasample <- subpetitions2015
datasample <- droplevels(datasample)
datasample$EMPLOYER_NAME <- factor(datasample$EMPLOYER_NAME)
datasample$JOB_TITLE <- factor(datasample$JOB_TITLE)

split = sample.split(datasample$CASE_STATUS, SplitRatio = 0.7)
sampleTrain = subset(datasample, split == TRUE)
sampleTest = subset(datasample, split == FALSE)
table(sampleTrain$CASE_STATUS)x``

sapply(sampleTrain, function(x) sum(is.na(x)))

#CART modelling
library(rpart)
set.seed(134)
modelCART = rpart(CASE_STATUS ~ EMPLOYER_NAME + FULL_TIME_POSITION + PREVAILING_WAGE , data = sampleTrain, method = "class")
prp(modelCART)
#predicting
predictCART = predict(modelCART, newdata = sampleTest, type = "class")

#tabulating
table(sampleTest$CASE_STATUS, predictCART)

#accuracy of 98.13%
accCART = (103289 + 132)/(103289 + 132 + 1506 + 463)
accCART

#randomforest modelling
library(randomForest)
modelRf = randomForest(CASE_STATUS ~ EMPLOYER_NAME + FULL_TIME_POSITION + PREVAILING_WAGE , data = sampleTrain, ntree = 500)
#predicting
predictRf = predict(modelRf, newdata = sampleTest)
#tablulating
table(sampleTest$CASE_STATUS, predictRf)

#accuracy of 99.80%
accRF = (15098)/(15098+29)
accRF


## 2016
#tabulating variables
subpetitions2016$EMPLOYER_NAME <- factor(subpetitions2016$EMPLOYER_NAME)
subpetitions2016 <- subset(subpetitions2016, EMPLOYER_NAME == "INFOSYS LIMITED" | EMPLOYER_NAME == "CAPGEMINI AMERICA INC " | EMPLOYER_NAME == "TATA CONSULTANCY SERVICES LIMITED" | EMPLOYER_NAME == "WIPRO LIMITED" | EMPLOYER_NAME == "ACCENTURE LLP" | EMPLOYER_NAME == "IBM INDIA PRIVATE LIMITED ")

employer2016 <- as.data.frame(subpetitions2016 %>% group_by(EMPLOYER_NAME) %>%
                                    summarise(count = n(), percent = round(count*100/nrow(subpetitions2016),1)) %>% 
                                    arrange(desc(count))%>% 
                                    top_n(10, wt = count))
employer2016


jobtitles2016 <- as.data.frame(subpetitions2016 %>% group_by(JOB_TITLE) %>%
                                     summarise(count = n(), percent = round(count*100/nrow(subpetitions2016),1)) %>% 
                                     arrange(desc(count))%>% 
                                     top_n(15, wt = count))

jobtitles2016



library(dplyr)
set.seed(134)
datasample <- subpetitions2016
datasample <- droplevels(datasample)
datasample$EMPLOYER_NAME <- factor(datasample$EMPLOYER_NAME)
datasample$JOB_TITLE <- factor(datasample$JOB_TITLE)

split = sample.split(datasample$CASE_STATUS, SplitRatio = 0.6)
sampleTrain = subset(datasample, split == TRUE)
sampleTest = subset(datasample, split == FALSE)
table(sampleTrain$CASE_STATUS)

sapply(sampleTrain, function(x) sum(is.na(x)))

#CART modelling
library(rpart)
set.seed(134)
modelCART = rpart(CASE_STATUS ~ EMPLOYER_NAME + FULL_TIME_POSITION + PREVAILING_WAGE , data = sampleTrain, method = "class")
prp(modelCART)
#predicting
predictCART = predict(modelCART, newdata = sampleTest, type = "class")
#tabulating
table(sampleTest$CASE_STATUS, predictCART)

#accuracy of 99.83%
accCART = (17248)/(17248+29)
accCART

#randomforest modelling
library(randomForest)
modelRf = randomForest(CASE_STATUS ~ EMPLOYER_NAME + FULL_TIME_POSITION + PREVAILING_WAGE , data = sampleTrain, ntree = 500)
#predicting
predictRf = predict(modelRf, newdata = sampleTest)
#tablulating
table(sampleTest$CASE_STATUS, predictRf)

#accuracy of 99.83%
accRF = (17248)/(17248+29)
accRF


#Visualizations

#2016
ggplot(data = employer2016, aes(x = reorder(EMPLOYER_NAME, percent),
                                y = percent, fill = EMPLOYER_NAME)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent), vjust = 1, hjust = 1) + 
  labs(x = "EMPLOYERS", y = "Petitions(in percentage)") + 
  scale_colour_gradient2() + 
  theme(legend.position = "none") +
  coord_flip()

ggplot(data = jobtitles2016, aes(x = reorder(JOB_TITLE, percent),
                                y = percent, fill = JOB_TITLE)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent), vjust = 1, hjust = 1) + 
  labs(x = "JOB ROLES", y = "Petitions(in percentage)") + 
  scale_colour_gradientn(colours = terrain.colors(10)) + 
  theme(legend.position = "none") +
  coord_flip()

#2015
ggplot(data = employer2015, aes(x = reorder(EMPLOYER_NAME, percent),
                                y = percent, fill = EMPLOYER_NAME)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent), vjust = 1, hjust = 1) + 
  labs(x = "EMPLOYERS", y = "Petitions(in percentage)") + 
  scale_colour_gradient2() + 
  theme(legend.position = "none") +
  coord_flip()

ggplot(data = jobtitles2015, aes(x = reorder(JOB_TITLE, percent),
                                 y = percent, fill = JOB_TITLE)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent), vjust = 1, hjust = 1) + 
  labs(x = "JOB ROLES", y = "Petitions(in percentage)") + 
  scale_colour_gradientn(colours = terrain.colors(10)) + 
  theme(legend.position = "none") +
  coord_flip()



#CASE_STATUS Denied Analysis
#We will explore employers and titles that have received maximum denials

deniedemployers <- petitions %>% filter(CASE_STATUS == "DENIED") %>%
  group_by(JOB_TITLE) %>% summarise(JOBS_DENIED_COUNT = n()) %>%
  arrange(desc(JOBS_DENIED_COUNT)) %>% top_n(10)

deniedemployers

deniedtitles <- petitions %>% filter(CASE_STATUS == "DENIED") %>%
  group_by(JOB_TITLE) %>% summarise(JOBS_DENIED_COUNT = n()) %>%
  arrange(desc(JOBS_DENIED_COUNT)) %>% top_n(10)

deniedtitles


## Further Exploration
#library("dplyr")
#library("tidyr")
#petitions <-separate(data = petitions, col = WORKSITE, into = c("CITY", "STATE"), sep = ",")
#pet2016 <- subset(petitions, YEAR == 2016)

#set.seed(134)
#split = sample.split(pet2016$CASE_STATUS, SplitRatio = 0.65)
#petTrain = subset(pet2016, split == TRUE)
#petTest = subset(pet2016, split == FALSE)
#petCART = rpart(CASE_STATUS ~ EMPLOYER_NAME + PREVAILING_WAGE + CITY + STATE + FULL_TIME_POSITION , data = petTrain, method = "class")

