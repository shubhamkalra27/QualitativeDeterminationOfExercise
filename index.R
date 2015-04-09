usePackage <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("caret","rpart","rpart.plot","RColorBrewer","rattle",
              "grid", "randomForest", "e1071")
usePackage(packages)


set.seed(333)
path <- "C:/Users/shubham/SkyDrive/UC study/capstoneProject/WearableComputing_weight_lifting_exercises_biceps_curl_variations.csv"
entireSet<-  read.csv(path, na.strings=c("NA","#DIV/0!","") )

############### EDA

dim(entireSet)

summary(entireSet)

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
#59 variables left
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



inTrain <- createDataPartition(y=cleanData4$classe, p=0.8, list=FALSE)

train80 <- cleanData4[inTrain, ]; train20 <- cleanData4[-inTrain, ]

dim(train80) ; dim(train20)

#we remove the classe in 20% of testing data



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



tree1 <- rpart(classe ~ ., data=train80, method="class")

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
length(forestInsample)

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
##################### k means clustering

library(fpc)

fit = kmeans(train20[,-dim(train20)[2]], 5)
plotcluster(iris[,1:4], fit$cluster)

#################### fitting logistic
install.packages("mlogit")
library(mlogit)
?mlogit
logisticModel <- mlogit(classe ~ .   , data=train80)

save.image()

#=========================visualizations================
names(traindata4)



qplot(roll_belt, yaw_belt, data = cleanData4[cleanData4$user_name == "charles",],colour = classe)

qplot(roll_belt, yaw_belt, data = cleanData4[cleanData4$user_name == "charles",],colour = classe)

cov(cleanData4$accel_belt_x,cleanData4$accel_belt_y,cleanData4$accel_belt_z)

?cor

p1 <- qplot(roll_belt, yaw_belt, data = cleanData4[ch,], colour = classe, main = 
              "Charles")
ch1 <- which(traindata4$user_name == "charles")
p2 <- qplot(roll_belt, pitch_belt, data = traindata4[ch,], colour = classe, main = 
              "Charles")

e <- which(training$user_name == "eurico")
p2 <- qplot(roll_belt, yaw_belt, data = training[e,], colour = classe, main = "Eurico")

pushViewport(viewport(layout = grid.layout(1,2)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col=1))
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col=2))
