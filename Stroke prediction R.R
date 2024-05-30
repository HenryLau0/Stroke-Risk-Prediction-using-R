# install and import basic libraries
install.packages('tidyverse')
library(tidyverse)
install.packages('caret')
library(caret)
install.packages('randomForest')
install.packages("randomForest", lib="C:/path/to/your/library")
library(randomForest)
install.packages('gridExtra')
library(gridExtra)
install.packages('plyr')
library(plyr)
install.packages('dplyr')
library(dplyr)
install.packages('htmlTable')
library(htmlTable)
install.packages('ggplot2')
library(ggplot2)
install.packages('rvest')
library(rvest)
install.packages('naniar')
library(naniar)
install.packages("smotefamily")
library(smotefamily)
remotes::install_github("cran/DMwR", force = TRUE)
library(DMwR)

# import the dataset
df <- read.csv("C:/Users/Acer/Downloads/healthcare-dataset-stroke-data.csv")
View(df)
str(df)
summary(df)

# convert the necessary column to factor
df$stroke <- factor(df$stroke, levels = c(0,1), labels = c("No","Yes"))
df$hypertension <- factor(df$hypertension, levels = c(0,1), labels = c("No","Yes"))
df$heart_disease <- factor(df$heart_disease, levels = c(0,1),labels = c("No","Yes"))
df$gender <- as.factor(df$gender)
df$ever_married <- as.factor(df$ever_married)
df$work_type <- as.factor(df$work_type)
df$Residence_type <- as.factor(df$Residence_type)
df$smoking_status <- as.factor(df$smoking_status)
View(df)

# Check the missing value
df$bmi[df$bmi == "N/A"] <- NA
sum(is.na(df))

# visualize the missing value
vis_miss(df)

# treat the missing value
print(summary(df))
class(df$bmi)
df$bmi <- as.numeric(df$bmi)
df$bmi[is.na(df$bmi)] <- mean(df$bmi, na.rm = TRUE)
sum(is.na(df))
df <- df[!(df$gender=="Other"),]
summary(df)

# exploratory data analysis
ggplot(df) +
  geom_bar(aes(x=df$stroke, fill=stroke),
           position = "dodge") + ggtitle("Distribution of Stroke")+ theme(
             plot.title = element_text(hjust = 0.5),
           )

gen <- ggplot(df, aes(x="",y=gender,fill=gender)) + geom_bar(stat = "identity",width = 1) + coord_polar("y",start = 0)
hyp <- ggplot(df, aes(x="",y=hypertension,fill=hypertension)) + geom_bar(stat = "identity",width = 1) + coord_polar("y",start = 0)
hd <- ggplot(df, aes(x="",y=heart_disease,fill=heart_disease)) + geom_bar(stat = "identity",width = 1) + coord_polar("y",start = 0)
em <- ggplot(df, aes(x="",y=ever_married,fill=ever_married)) + geom_bar(stat = "identity",width = 1) + coord_polar("y",start = 0)
grid.arrange(grobs = list(gen,hyp,hd,em),ncol=3,top="Distribution of Variables")

p1 <- ggplot(data =df) + geom_bar(mapping = aes(x = gender,fill=stroke))
p2 <- ggplot(data =df) + geom_bar(mapping = aes(x = hypertension,fill=stroke))
p3 <- ggplot(data =df) + geom_bar(mapping = aes(x = heart_disease,fill=stroke))
p4 <- ggplot(data =df) + geom_bar(mapping = aes(x = ever_married,fill=stroke))
grid.arrange(grobs = list(p1,p2,p3,p4),ncol=3, top="Distribution of Stroke for Each Variables")

p5 <- ggplot(data = df) + geom_bar(mapping = aes(x = work_type, fill=stroke))
p6 <- ggplot(data = df) + geom_bar(mapping = aes(x = Residence_type, fill=stroke))
p7 <- ggplot(data = df) + geom_bar(mapping = aes(x = smoking_status, fill=stroke))

grid.arrange(grobs = list(p5,p6,p7),ncol=3, top="Distribution of Stroke for Each Variables")


# data splitting
id <- sample(2,nrow(df),prob = c(0.7,0.3),replace=TRUE)
train <- df[id==1,]
test <- df[id==2,]

dim(train)
View(train)
dim(test)

# Modeling with Random Forest
bestmtry <- tuneRF(train, train$stroke, stepFactor = 1.2, improve = 0.01,trace = T, plot = T)
set.seed(123)
rf_model <- randomForest(stroke~. - id,data= train)
rf_model
confusionMatrix(predict(rf_model,train),train$stroke) 

importance(rf_model)
varImpPlot(rf_model)

# view the unbalanced data in this 2 columns
table(train$hypertension)
table(train$heart_disease)

# fix the unbalanced information in certain columns
train_balanced <- SMOTE(stroke ~ hypertension + heart_disease - id, data = train, perc.over = 85, k = 60)
rf_model1 <- randomForest(stroke~.,data= train_balanced)
rf_model1

importance(rf_model1)

test_balanced <- SMOTE(stroke ~ hypertension + heart_disease - id, data = test, perc.over = 85, k = 39)
predict <- predict(rf_model1, newdata = test_balanced, type = "class")

confusionMatrix(predict(rf_model1, test_balanced),test_balanced$stroke)




