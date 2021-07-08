### data font https://archive.ics.uci.edu/ml/datasets/Algerian+Forest+Fires+Dataset++#
##First install the packages that will be used

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(randomForest)

#This following line maybe need to be changed to mach the place where you stored the 
#data in your computer #I will provide the data in a csv file
setwd("~/Data_science/9-Capstone/Algerian_Fires") 
raw <- read_csv("Algerian_forest_fires_dataset_UPDATE.csv") #reading the file
head(raw) #checking if this was read properly

#The next line will make some changes in the data
#First it will remove the missing data (NA value), then it will convert the outcomes to factors and 
#the one variable that was read as 'char' to 'dbl'
data<- raw %>% filter(!is.na(FWI)) %>% 
  mutate(Classes = factor(Classes, levels = c("fire","not fire")), DC = as.double(DC))

head(data) #Checking the data

#The input that will be used for classification is the Fire Weather Index in the column called "FWI"
#The outcomes expected are "fire" and "not fire" in the column "Classes"
#In the next line I will make a boxplot of the FWI values (the input)  for each class
#This has the objective of get an inside in how the input variable can be used as a classifier
data %>% ggplot(aes(Classes, FWI, fill = Classes)) + 
  geom_boxplot() +
  theme(
    axis.title.x = element_text(size = 20), #change the axis name size
    axis.text.x = element_text(size = 20), #change the axis test size
    axis.title.y = element_text(size = 20), #change the axis name size
    axis.text.y = element_text(size = 20), #change the axis test size
    legend.position = "none")  #hiding the legend since there is no point in showing this since the box names are in the x axis 
#Can you see that there is a clear separation between the dispersion of FWI values for each class?


####Test and training set####
# in this part of the code I will define 20% of my data set for test the algorithms
#set the seed because this is a good practice when dealing with random function
set.seed(1, sample.kind="Rounding") #If your R version is 3.5 or less you should use set.seed(1) instead
index <- createDataPartition(data$Classes, times = 1, p = 0.2, list =  FALSE) #participating the data
train<- data[-index,] #creating the train set
test<- data[index,] #creating the test set

####Decision Tree ####
#in this section I will train my first classification algorithm: the classification tree
set.seed(10, sample.kind="Rounding")  #If your R version is 3.5 or less you should use set.seed(10) instead
fit_tree = train(Classes ~ FWI, 
                  data=train,                     #training the model
                  method="rpart", 
                  trControl = trainControl(method = "cv")) 


# the following to lines will be used to plot the tree
plot(fit_tree$finalModel, margin = 0.1)
text(fit_tree$finalModel, cex = 1.5)

predict_tree <- predict(fit_tree, test) #predicting the outcomes from the test set

confusionMatrix(predict_tree, test$Classes) #printing the confusion matrix


#### Random Forest####
#next classification algorithm is random forest
set.seed(10, sample.kind="Rounding")  #If your R version is 3.5 or less you should use set.seed(10) instead
fit_forest <- train(Classes ~ FWI, 
                    data=train,                   #training the forest
                    method="rf", 
                    trControl = trainControl(method = "cv")) 


predict_forest <- predict(fit_forest, test)  #predicting the outcomes from the test set

confusionMatrix(predict_forest, test$Classes) #printing the confusion matrix

