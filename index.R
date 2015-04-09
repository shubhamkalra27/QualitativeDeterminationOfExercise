
usePackage <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("caret","rpart","rpart.plot","RColorBrewer","rattle",
              "grid", "randomForest", "e1071", "nnet","ggplot2","fpc")
usePackage(packages)

set.seed(333)
data <- "https://raw.githubusercontent.com/shubhamkalra27/QualitativeDeterminationOfExercise/master/data.csv"

entireSet <- read.csv(url(data), 
                      stringsAsFactors = FALSE, 
                      na.strings=c("NA","#DIV/0!",""))
############### EDA
#=========================visualizations================

library(ggplot2)
a <- which(entireSet$classe == "A")
a_only <- entireSet[a,]
ggplot(data = a_only, aes(x=roll_belt, y=yaw_belt)) + 
  geom_point(data=c_only, aes(color=user_name)) + ggtitle("Activity class A")

ch <- which(entireSet$user_name == "charles")
p1 <- qplot(roll_belt, yaw_belt, data = entireSet[ch,], colour = classe, main = 
              "Charles")
e <- which(entireSet$user_name == "eurico")
p2 <- qplot(roll_belt, yaw_belt, data = entireSet[e,], colour = classe, main = "Eurico")

pushViewport(viewport(layout = grid.layout(1,2)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col=1))
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col=2))

dim(entireSet)

summary(entireSet)

##################### k means clustering
nums <- sapply(cleanData4, is.numeric)
par(mfrow=c(2,2))

fit3 = kmeans(cleanData4[ , nums], 3)
plotcluster(cleanData4[ , nums], fit3$cluster, main = "k = 3")

fit4 = kmeans(cleanData4[ , nums], 4)
plotcluster(cleanData4[ , nums], fit4$cluster,  main = "k = 4")


fit5 = kmeans(cleanData4[ , nums], 5)
plotcluster(cleanData4[ , nums], fit5$cluster,  main = "k = 5")

fit6 = kmeans(cleanData4[ , nums], 6)
plotcluster(cleanData4[ , nums], fit6$cluster,  main = "k = 6")


fit7 = kmeans(cleanData4[ , nums], 7)
plotcluster(cleanData4[ , nums], fit7$cluster)
################ DATA CLEANING

# There are variables with Empty values. removing those variables which has too many NAs. My cutoff here is 70%
names(entireSet)

varcutoff <- 0.7
cleanData1 <- entireSet #creating another subset to iterate in loop
for(i in 1:length(entireSet)) { #for every column in the training dataset
  if( sum( is.na( entireSet[, i] ) ) /nrow(entireSet) >= varcutoff ) { #if n?? NAs > varcutoff (70%) of total observations
    for(j in 1:length(cleanData1)) {
      if( length( grep(names(entireSet[i]), names(cleanData1)[j]) ) ==1)  { #if the columns are the same:
        cleanData1 <- cleanData1[ , -j] #Remove that column
      }   
    } 
  }
}


dim(cleanData1)
names(cleanData1)
#Cleaning NearZeroVariance Variables
myDataNZV <- nearZeroVar(cleanData1, saveMetrics=FALSE)


cleanData2 <- subset(cleanData1, select = -c(myDataNZV))
dim(cleanData2)
names(cleanData2)

cleanData3 <- subset(cleanData2, select = -c( raw_timestamp_part_1,
                                              raw_timestamp_part_2, cvtd_timestamp,
                                              num_window))
temp <- cleanData3

cleanData4 <- cleanData3[-which(is.na(temp$roll_dumbbell)),]

dim(cleanData4) ; dim(cleanData3);

cleanData4$user_name <- factor(cleanData4$user_name)
cleanData4$classe <- factor(cleanData4$classe)
summary(cleanData4)

# creating data partition in training and testset. 
inTrain <- createDataPartition(y=cleanData4$classe, p=0.8, list=FALSE)

train80 <- cleanData4[inTrain, ]; train20 <- cleanData4[-inTrain, ]

dim(train80) ; dim(train20)


## check if train80 and train20 have same class types
difff <- NULL
for (i in 1:length(train80) ) {
  for(j in 1:length(train20)) {
    #if( length( grep(names(myTraining[i]), names(testing)[j]) ) ==1)  {
    difff[i] <- (class(train20[j]) == class(train80[i]))
    # }      
  }      
}

which(difff == FALSE)


############################## Fitting Decesion Trees

par(mfrow=c(1,1))
tree1 <- rpart(classe ~ ., data=train80, method="class", cp = 0.024)

fancyRpartPlot(tree1)
plotcp(tree1)
treeInSample <- predict(tree1, type = "class")
confusionMatrix(treeInSample, train80$classe)


treeOutOfSample <- predict(tree1, train20, type = "class")

confusionMatrix(treeOutOfSample, train20$classe)

################################# fitting random forest
?randomForest
names(train80)
summary(train80)
forest2 <- randomForest(classe ~. , data=train80 )

#insample
forestInsample <- predict(forest2, type = "class")
confusionMatrix(forestInsample, train80$classe)


#outtasample
forestOuttasample <- predict(forest2, train20, type= "class")
confusionMatrix(forestOuttasample, train20$classe)

####################################### fitting SVM

svmModel <- svm(classe ~ .   , data=train80) 

# test with train data
svmInSample <- predict(svmModel)
confusionMatrix(svmInSample, train80$classe)

#test outta sample
dim(train20)
length(svmOutSample)

summary(train20)
svmOutSample <- predict(svmModel, train20)

confusionMatrix(svmOutSample, train20$classe)

save.image()

#################### Neural Networks

nnetfit <- nnet(classe~., data=train80, size=6, decay=0.0001, maxit=1000)
# summarize the fit
summary(nnetfit)

# make predictions
predictionsinSample <- predict(nnetfit, type="class")
confusionMatrix(predictionsinSample, train80$classe)

predictionsoutOfSample <- predict(nnetfit, newdata = train20, type="class")
confusionMatrix(predictions, train20$classe)

nnetfit2 <- nnet(classe~., data=train80, size=4, decay=0, maxit=500)
predictions <- predict(nnetfit2, newdata = train20, type="class")
confusionMatrix(predictions, train20$classe)


#################### naive bayes

naiveBayesModel <- naiveBayes(classe ~ .   , data=train80)

naiveBayesPrediction <- predict(naiveBayesModel, newdata = train80)

confusionMatrix(naiveBayesPrediction, train80$classe)

naiveBayesPrediction <- predict(naiveBayesModel, newdata = train20)

confusionMatrix(naiveBayesPrediction, train20$classe)

table(predict(m, iris), iris[,5])

save.image()