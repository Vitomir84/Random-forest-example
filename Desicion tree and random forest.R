
library(ISLR)

head(College)

df <- College

#Create a scatterplot of Grad.Rate versus Room.Board, colored by the Private column.

ggplot(df, aes(Room.Board, Grad.Rate), colours()) + geom_point(aes(color=Private))

#Create a histogram of full time undergrad students, color by Private.

ggplot(df, aes (F.Undergrad)) + geom_histogram(aes(fill=Private), color="black", binwidth = 300) + theme_bw()

#Create a histogram of Grad.Rate colored by Private. You should see something odd here.

ggplot(df, aes (Grad.Rate)) + geom_histogram(aes(fill=Private), color="black", binwidth = 5) + theme_bw()

#Izaberi koledz koji ima stopu zavrsavanja vecu od 100%


subset(df, Grad.Rate > 100)

#Promeniti ovu stopu u 100

df['Cazenovia College','Grad.Rate'] <- 100

subset(df, Apps == 3847)

#pravljenje trening i test baze u razmeri 70/30

library(caTools)

set.seed(101) 

sample = sample.split(df$Private, SplitRatio = .70)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)

#pravljenje modela desicion tree

#Use the rpart library to build a decision tree to predict whether or not a school is Private. 
#Remember to only build your tree off the training data


library(rpart)
tree <- rpart(Private ~.,method='class',data = train)

#kreiraj predviđene vrednosti na osnovu desicion tree modela za test bazu

tree.preds <- predict(tree,test)
head(tree.preds)

#Turn these two columns into one column to match the original Yes/No Label for a Private column.

tree.preds <- as.data.frame(tree.preds)

joiner <- function (x){
  if (x>=0.5){
    return("Yes")
    }else{
      return("No")
      }
  }


tree.preds$Private <- sapply(tree.preds$Yes, joiner)

head(tree.preds)

#Now use table() to create a confusion matrix of your tree model.

table(tree.preds$Private, test$Private)

#Use the rpart.plot library and the prp() function to plot out your tree model.

install.packages("rpart.plot")
library(rpart.plot)
prp(tree)

#Random forests improve predictive accuracy by generating a large number of bootstrapped trees 
#(based on random samples of variables), classifying a case using each tree in this new "forest", 
#and deciding a final predicted outcome by combining the results across all of the trees 
#(an average in regression, a majority vote in classification).
#We can use the randomForest library to create and build out a Random Forest.

#Call the randomForest package library

install.packages("randomForest")
library(randomForest)

#Now use randomForest() to build out a model to predict Private class. 
#Add importance=TRUE as a parameter in the model. (Use help(randomForest) to find out what this does.

help("randomForest")
forest <- randomForest(Private ~ ., data=df, importance=TRUE)
forest

#What was your model's confusion matrix on its own training set? Use model$confusion.

forest$confusion

#Grab the feature importance with model$importance. Refer to the reading for more info on what Gini[1] means.[2]

forest$importance
prp(tree)

#MeanDecreasedAccuracy - koliko uključenje prediktora smanjuje grešku klasifikacije

#MeanDecreaseGini - Gini is defined as "inequity" when used in describing a society's distribution of income, 
#or a measure of "node impurity" in tree-based classification. 
#A low Gini (i.e. higher descrease in Gini) means that a particular predictor variable plays a greater role 
#in partitioning the data into the defined classes. It's a hard one to describe without talking about the fact 
#that data in classification trees are split at individual nodes based on values of predictors. 
#I'm not so clear on how this translates into better performance.

#Now use your random forest model to predict on your test set!

p <- predict(forest,test)

table(p,test$Private)

#It should have performed better than just a single tree, 
#how much better depends on whether you are emasuring recall, precision, 
#or accuracy as the most important measure of the model.

table(p,test$Private)

#accuracy rate of randomforest model
0/(169+64)

#accuracy rate of tree desicion model

table(tree.preds$Private, test$Private)
1-((9+7)/(160+9+7+57))
#Random forest nije pogrešio uopšte dok je decision tree došao do stope pogadjanja 0,931
