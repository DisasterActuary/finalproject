###Final Project
###edX Data Science Capstone
###Student: Ricardo Morales
###**************************

###Setting the data sets

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
vg <- read.csv("videogames.csv") #Please load it from your working directory
class(vg)
head(vg)
nrow(vg)
ncol(vg)
summary(vg)
names(vg)
pairs(vg[, c(6, 7, 8, 9)], col = "blue",
      main = "Scatter plots - Video game sales")
#Data cleaning
vg <- vg %>% filter(!is.na(Critic_Score), !is.na(Critic_Count),
                    !is.na(User_Count)) %>%
  mutate(Critic_Score = round(Critic_Score/10),
         User_Score = round(as.numeric(User_Score))) %>%
  select(NA_Sales, EU_Sales, User_Score, Critic_Score)
vg %>%
  ggplot(aes(Critic_Score)) +
  geom_histogram(color = "black", fill = "skyblue") +
  ggtitle("Histogram of Critic Score") +
  theme(plot.title = element_text(hjust=0.5))
#Redefining variables Critic_Score & User_Score
vg<- vg %>% mutate(Critic_Score = ifelse(Critic_Score<=5,0,1),
                   User_Score = ifelse(User_Score<=5,0,1))
set.seed(1, sample.kind="Rounding")
index <- sample(nrow(vg), 0.1*nrow(vg))
length(index)
train_set <- vg[-index,] #Creating the train set
class(train_set)
nrow(train_set)
ncol(train_set)
head(train_set, 10)
train_set$Critic_Score <- factor(train_set$Critic_Score)
train_set$User_Score <- factor(train_set$User_Score)
summary(train_set$Critic_Score)
levels(train_set$Critic_Score)
summary(train_set$User_Score)
levels(train_set$User_Score)
test_set <- vg[index,] #Creating the test set
nrow(test_set)
ncol(test_set)
head(test_set, 10)
test_set$Critic_Score <- factor(test_set$Critic_Score, 
                                levels = levels(train_set$Critic_Score))
test_set$User_Score <- factor(test_set$User_Score, 
                              levels = levels(train_set$User_Score))
summary(test_set$Critic_Score)
levels(test_set$Critic_Score)
summary(test_set$User_Score)
levels(test_set$User_Score)

###Building the algorithms

#K-Nearest Neighbors

set.seed(1, sample.kind="Rounding")
train_knn <- train(Critic_Score ~ .,
                   method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3,50,2)),
                   trControl = trainControl(method = "cv", number = 10, p = 0.9))
train_knn$bestTune #Choosing parameter k
knn_Critic_Score <- predict(train_knn, test_set)
mean(knn_Critic_Score == test_set$Critic_Score) #Calculating accuracy
confusionMatrix(knn_Critic_Score, test_set$Critic_Score)$overall["Accuracy"]

##Classification Tree

set.seed(10, sample.kind="Rounding")
train_rpart <- train(Critic_Score ~ .,
                     data = train_set,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
train_rpart$bestTune #Choosing cp
rpart_Critic_Score <- predict(train_rpart, test_set)
mean(rpart_Critic_Score == test_set$Critic_Score) #Calculating accuracy
confusionMatrix(rpart_Critic_Score, test_set$Critic_Score)$overall["Accuracy"]

##Random Forest

set.seed(11, sample.kind="Rounding")
train_rf <- train(Critic_Score ~ .,
                  data = train_set,
                  method = "rf",
                  ntree = 100,
                  tuneGrid = data.frame(mtry = seq(1:10)))
train_rf$bestTune #Choosing mtry
rf_Critic_Score <- predict(train_rf, test_set)
mean(rf_Critic_Score == test_set$Critic_Score) #Calculating accuracy
confusionMatrix(rf_Critic_Score, test_set$Critic_Score)$overall["Accuracy"]

##Ensemble

ensemble <- data.frame(knn = knn_Critic_Score,
           rpart = rpart_Critic_Score,
           rf = rf_Critic_Score)
nrow(ensemble)
ncol(ensemble)
head(ensemble, 10)
ensemble_Critic_Score <- ifelse(rowSums(ensemble == 1) > 1, 1, 0)
length(ensemble_Critic_Score)
mean(ensemble_Critic_Score == test_set$Critic_Score)

###Results

knn_accuracy <- round(mean(knn_Critic_Score == test_set$Critic_Score),4)
rpart_accuracy <- round(mean(rpart_Critic_Score == test_set$Critic_Score),4)
rf_accuracy <- round(mean(rf_Critic_Score == test_set$Critic_Score),4)
ens_accuracy <- round(mean(ensemble_Critic_Score == test_set$Critic_Score),4)
data.frame(algorithm = c("knn", "classif. tree", "random forest", "ensemble"),
           accuracy = c(knn_accuracy, rpart_accuracy, rf_accuracy, ens_accuracy))

