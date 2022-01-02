
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")
if(!require(Class)) install.packages("Class", repos = "http://cran.us.r-project.org")

knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggplot2)
library(caret)
library(Metrics)
library(class)


## Our goal for this project will be to see if we can predict a games *Critic_Score* by utilizing the variables provided in the data set. We can begin by loading the data in the form of a .csv file into our working directory:
  

vgsales <- read.csv("~/Video_Games_Sales_as_at_22_Dec_2016.csv") 
## Initial Data Load, grab file from working directory
str(vgsales) ##Evaluate variable types
head(vgsales) ## View first 10 rows of data set



vgsales$Year_of_Release <- as.numeric(vgsales$Year_of_Release ) ## Coerce Numeric

vgsales$User_Score <- as.numeric(vgsales$User_Score) * 10 
## Coerce numeric and multiply by 10 to have User_Score in same format as Critic_Count



summary(vgsales$Critic_Score) ## Display summary statistics for target variable


vgsales <- vgsales[complete.cases(vgsales$Critic_Score),] 
## Remove all rows with NA values in Critic_Score
sum(is.na(vgsales$Critic_Score))


vgsales_genre <- vgsales %>% group_by(Genre) %>% summarize(avg_score = mean(Critic_Score), avg_count = mean(Critic_Count)) # Create grouped Genre Data

head(vgsales_genre[order(-vgsales_genre$avg_score),]) ##Evaluate sorted rows

vgsales_genre %>% ggplot(aes(x = reorder(Genre, -avg_score) ,y = avg_score)) + geom_col() + scale_y_continuous(limits=c(0,100)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + scale_x_discrete("Genre") ## Generate Plot


head(vgsales_genre[order(-vgsales_genre$avg_count),]) ## View top ordered rows

vgsales_genre %>% ggplot(aes(x = reorder(Genre, -avg_count) ,y = avg_count)) + geom_col() + scale_y_continuous(limits=c(0,50)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + scale_x_discrete("Genre") ## Generate Plot


vgsales_worldsales <- vgsales %>% group_by(Genre) %>% summarize(avg_score = mean(Critic_Score), avg_count = mean(Critic_Count), avg_sales = mean(Global_Sales))
head(vgsales_worldsales[order(-vgsales_worldsales$avg_sales),])

vgsales_worldsales %>% ggplot(aes(x = reorder(Genre, -avg_sales) ,y = avg_sales)) + geom_col() + scale_y_continuous(limits=c(0,1.10)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + scale_x_discrete("Genre") ## Generate Plot


vgsales_developer <- vgsales %>% group_by(Developer) %>% summarize(avg_score = mean(Critic_Score), avg_count = mean(Critic_Count), avg_sales = mean(Global_Sales))# Create grouped Developer Data

head(vgsales_developer[order(-vgsales_developer$avg_score),]) ##Evaluate sorted rows


cor(vgsales_developer$avg_score,vgsales_developer$avg_sales) ## Calculate Pearson Correlation


vgsales_platform <- vgsales %>% group_by(Platform) %>% summarize(avg_score = mean(Critic_Score), avg_count = mean(Critic_Count), avg_sales = mean(Global_Sales))# Create grouped Developer Data

head(vgsales_platform[order(-vgsales_platform$avg_score),]) ##Evaluate sorted rows

vgsales_year <- vgsales %>% group_by(Year_of_Release) %>% summarize(avg_score = mean(Critic_Score), avg_count = mean(Critic_Count), avg_sales = mean(Global_Sales))

head(vgsales_year[order(-vgsales_year$avg_score),]) ##Evaluate sorted rows


vgsales_year <- vgsales_year[complete.cases(vgsales_year$Year_of_Release),]
cor(vgsales_year$avg_count,as.numeric(vgsales_year$Year_of_Release), use = "complete.obs") 
## Calculate Pearson Correlation


## Having evaluated most of the variable of interest, we can start by fitting a basic linear equation for Critic Score in the *vgsales data set*:



lm <- lm(Critic_Score ~ Global_Sales, data = vgsales) #Create the linear regression
summary(lm) #Review the results




lm <- lm(Critic_Score ~ Critic_Count, data = vgsales) #Create the linear regression
summary(lm) #Review the results



lm <- lm(Critic_Score ~ Critic_Count + Global_Sales, data = vgsales) #Create the linear regression
summary(lm) #Review the results



set.seed(123)
training.samples <- vgsales$Critic_Score %>% createDataPartition(p = 0.9, list = FALSE)


train.data  <- vgsales[training.samples, ]
test.data <- vgsales[-training.samples, ]

lm <- lm(Critic_Score ~ Critic_Count + Global_Sales, data = train.data) #Create the linear regression

probabilities <- lm %>% predict(test.data, type = "response")
test.data$Predictions <- probabilities
rmse(test.data$Critic_Score, test.data$Predictions)

set.seed(321)
training.samples <- vgsales$Critic_Score %>% createDataPartition(p = 0.9, list = FALSE)

vgsales <- vgsales[complete.cases(vgsales$User_Score),] ## leave only rows with valid user scores
vgsales <- vgsales[complete.cases(vgsales$Year_of_Release),] ## leave only rows with valid user scores

train.data  <- vgsales[training.samples, ]
test.data <- vgsales[-training.samples, ]

lm <- lm(Critic_Score ~ User_Score + Critic_Count + Global_Sales + Genre + Year_of_Release + Platform, data = train.data) #Create the linear regression

probabilities <- lm %>% predict(test.data, type = "response") ## Predict Values with model
test.data$Predictions <- probabilities ## load predictions into test.dataframe
rmse(test.data$Critic_Score, test.data$Predictions) ## Verify RMSE for model


vgsales$favorable <- ifelse(vgsales$Critic_Score >= 80,'Favorable','Not Favorable') 
## Create binary favorable/unfavorable variable

vgsales <- vgsales %>% mutate(TopNa = ifelse(NA_Sales > JP_Sales & NA_Sales > EU_Sales,'Top NA', ifelse( JP_Sales > NA_Sales & JP_Sales > EU_Sales,'Top EU',ifelse(EU_Sales > NA_Sales & EU_Sales > JP_Sales,'Top EU','Top Other')))) ## Evaluate which country had the most sales to see if it has an effect over favorable/unfavorable

str(vgsales)


set.seed(321)

vgsales <- vgsales[complete.cases(vgsales), ] ## Remove all NA Values

knsales <- vgsales %>% select(Critic_Score, User_Score,User_Count , Critic_Count , Global_Sales, Genre, Year_of_Release , Platform,favorable ,Publisher) ## Create subset with variables of interest

training.samples <- knsales$Critic_Score %>% createDataPartition(p = 0.9, list = FALSE) 
## Create partition

train.data  <- knsales[training.samples, ] ## Subset train data
test.data <- knsales[-training.samples, ] ## Subset test Data

train.data.labels <- train.data %>% select(favorable) ## Select train labels
test.data.labels <- test.data %>% select(favorable) ## Select test labels

train.data <- train.data %>% select(User_Score, Critic_Count , Global_Sales, Year_of_Release ,User_Count) 
## RM favorable Column

test.data <- test.data %>% select(User_Score, Critic_Count , Global_Sales, Year_of_Release  ,User_Count) 
## RM favorable Column


test_pred <- knn(train = train.data, test = test.data,cl = train.data.labels$favorable, k= 79) 
## Train Model

confusionMatrix(table(test_pred ,test.data.labels$favorable)) ## Evaluate model accuracy


