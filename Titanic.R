
# Load new data
test <- read.csv("test.csv", header = TRUE)
train <- read.csv("train.csv", header = TRUE)

# Add a "survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("none", nrow(test)),test[,])

# Combine the data sets
data.combined <- rbind(train, test.survived)

# A bit about R data types (eg. factors,....etc.)
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$PassengerId <- as.factor(data.combined$PassengerId)

# Take a look at the gross survival rates
table(data.combined$Survived)

# Let's look at the Pclass variable in terms of Survival
table(data.combined$Pclass,data.combined$Survived)

# Load ggplot2 package for visualization
library(ggplot2)

# Hypothesis is that the rich folks "survived"!
train$Pclass <- as.factor(train$Pclass)
train$Survived <- as.factor(train$Survived)
str(train)
ggplot(train, aes(x = Pclass, fill = Survived)) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count")

# Examining the First names in the training dataset
head(as.character(train$Name))

# How many unique names are there in the Test and Train datasets
length(unique(as.character(data.combined$Name)))

# There seem to be 2 duplicate names. Let's check
# Let's get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

# Let's now take a look at the duplicated names in the data.combined dataset
data.combined[which(data.combined$Name %in% dup.names),]

# Let's now check what is up with the 'Miss.', 'Mr.' and 'Mrs.' titles to check whether there's any co-relation there
library(stringr)

# Any correlation with other variables? Like SibSp...etc.???
misses <- data.combined[which(str_detect(data.combined$Name,"Miss.")),]
misses[1:5,]

# Let's take a look at people with "Mrs." in their names or the one's who were probably married
mrses <- data.combined[which(str_detect(data.combined$Name,"Mrs.")),]
mrses[1:5,] # All of those 5 married women survived! Which is good to know!

# Now let's take a look at men on the boat in the combined data
males <- data.combined[which(data.combined$Sex == "male"),]
males[1:5,]

# As we saw that males also have "Master" and "Mr.", let's separate them out
misters <- data.combined[which(str_detect(data.combined$Name,"Mr.")),]
misters[1:5,]

# And the same for "Masters"
masters <- data.combined[which(str_detect(data.combined$Name,"Master.")),]
masters[1:5,]

# Now let's expand upon the relationship between 'Survived' and 'Pclass' by adding a new variable 'Title'
# Create a utility function to help with this extraction

extractTitle <- function(Name){
  Name <- as.character(Name)
  
  if (length(grep("Miss.",Name))>0){
    return ("Miss.")
  } else if (length(grep("Mrs.",Name))>0){
    return ("Mrs.")
  } else if (length(grep("Mr.",Name))>0){
    return ("Mr.")
  } else if (length(grep("Master.",Name))>0){
    return ("Master.")
  } else {
    return("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)){
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}
data.combined$Title <- as.factor(titles)

# Let's plot this out now. Since we have Survived values only in the first 891 rows, just use those

ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")



table(data.combined$Sex,data.combined$Survived)

ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

ggplot(data.combined[1:891,], aes(x=Age, fill=Survived)) +
  facet_wrap(~Sex+Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

ggplot(misses[misses$Survived != "none",], aes(x=Age, fill=Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") +
  xlab("Age") +
  ylab("Total Count")


boys <- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)

summary(misses$Age)

misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age >= 15 & misses.alone$Age <= 25))

summary(misses.alone)

# Move on to the variable SibSp
summary(data.combined$SibSp)

length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

str(data.combined$SibSp)

# Let's ggplot the variable SibSp in conjuction with Pclass and Title to get some interesting insights!

ggplot(data.combined[1:891,], aes(x=SibSp,fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("SibSp")+
  ylab("Total Count") +
  labs(fill = "Survived")

data.combined$Parch <- as.factor(data.combined$Parch)

ggplot(data.combined[1:891,], aes(x=Parch,fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Parch")+
  ylab("Total Count") +
  labs(fill = "Survived")


temp.SibSp <- c(train$SibSp,test$SibSp)
temp.Parch <- c(train$Parch,test$Parch)
data.combined$FamilySize <- as.factor(temp.SibSp + temp.Parch + 1)

ggplot(data.combined[1:891,], aes(x=FamilySize, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Family Size") +
  ylab("Total Count") +
  labs(fill = "Survived")


str(data.combined$Ticket)

data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

Ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket,1,1))
unique(Ticket.first.char)

data.combined$Ticket.first.char <- as.factor(Ticket.first.char)

ggplot(data.combined[1:891,],aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by Ticket.first.char") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  labs(fill = "Survived")

ggplot(data.combined[1:891,],aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  labs(fill = "Survived")

ggplot(data.combined[1:891,],aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Now Fares

summary(data.combined$Fare)
length(unique(data.combined$Fare))

ggplot(data.combined[1:891,],aes(x=Fare,fill=Survived))+
  geom_histogram(binwidth = 5)+
  ggtitle("Combined Fare Distribution")+
  xlab("Fare")+
  ylab("Total Count")

ggplot(data.combined[1:891,],aes(x=Fare,fill=Survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~Pclass + Title)+
  ggtitle("Combined Fare Distribution")+
  xlab("Fare")+
  ylab("Total Count")

ggplot(data.combined[1:891,],aes(x=Fare,fill=Survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~Pclass + Title)+
  ggtitle("Combined Fare Distribution")+
  xlab("Fare")+
  ylab("Total Count")+
  ylim(0,30)

ggplot(data.combined[1:891,],aes(x=Fare,fill=Survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~Title)+
  ggtitle("Combined Fare Distribution")+
  xlab("Fare")+
  ylab("Total Count")

# Now Cabin

str(data.combined$Cabin)

data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

data.combined[which(data.combined$Cabin == ""),"Cabin"] <- "U"
data.combined$Cabin[1:100]

Cabin.first.char <- as.factor(substr(data.combined$Cabin,1,1))
unique(Cabin.first.char)
levels(Cabin.first.char)

data.combined$Cabin.first.char <- Cabin.first.char


ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived))+
  geom_bar()+
  ggtitle("Cabin Survivability Distribution")+
  xlab("Cabin")+
  ylab("Total Count")

ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Cabin Survivability Distribution by PClass")+
  xlab("Cabin")+
  ylab("Total Count")

ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + Title)+
  ggtitle("Cabin Survivability Distribution by PClass and Title")+
  xlab("Cabin")+
  ylab("Total Count")

# Let's look at passengers with multiple cabinss

data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = Cabin.multiple, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + Title)+
  ggtitle("Pclass, Title")+
  xlab("Multiple Cabins")+
  ylab("Total Count")+
  labs(fill = "Survived")

# Ok. Lastly let's have a look at survivability based on embarkment

str(data.combined$Embarked)
levels(data.combined$Embarked)

ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + Title)+
  ggtitle("Survivability based on where the person got Onboard")+
  xlab("Port of Embarkment")+
  ylab("Total Count")+
  labs(fill = "Survived")

# Ok. We have completed working on all the variables!


#----------------------------------------------------------------------------------
#
#                        EXPLORATORY MODELING
#
#----------------------------------------------------------------------------------

# Let's use randomForest for this

library(randomForest)

#MODEL 1
# Train a Random Forest with default variables using Pclass and Title
rf.train.1 <- data.combined[1:891, c("Pclass", "Title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

#MODEL 2
# Train a Random Forest with using Pclass, Title, and SibSp
rf.train.2 <- data.combined[1:891, c("Pclass", "Title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

#MODEL 3
# Train a Random Forest with using Pclass, Title, and Parch
rf.train.3 <- data.combined[1:891, c("Pclass", "Title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

#MODEL 4
# Train a Random Forest with using Pclass, Title, SibSp and Parch
rf.train.4 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)

#MODEL 5
# Train a Random Forest with using Pclass, Title, and FamilySize
rf.train.5 <- data.combined[1:891, c("Pclass", "Title", "FamilySize")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

#MODEL 6
# Train a Random Forest with using Pclass, Title, SibSp and FamilySize
rf.train.6 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "FamilySize")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)

#MODEL 7
# Train a Random Forest with using Pclass, Title, Parch and FamilySize
rf.train.7 <- data.combined[1:891, c("Pclass", "Title", "Parch", "FamilySize")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)

#MODEL 8
# Train a Random Forest with using Pclass, Title, Parch, SibSp and FamilySize
rf.train.8 <- data.combined[1:891, c("Pclass", "Title", "Parch", "SibSp", "FamilySize")]

set.seed(1234)
rf.8 <- randomForest(x = rf.train.8, y = rf.label, importance = TRUE, ntree = 1000)
rf.8
varImpPlot(rf.8)


####################################################################################
#                                                                                  #
#                                   DATA VALIDATION                                #
#                                                                                  #
####################################################################################

# Let's extract the prediction data
test.submit.df <- data.combined[892:1309, c("Pclass","Title","FamilySize")]

# Make Predictions
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

# Write a CSV File for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df, file = "RF SUB 20160715.CSV", row.names = FALSE)


library(caret)
library(doSNOW)

set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

table(rf.label)
342/549

table(rf.label[cv.10.folds[[33]]])
308/494


ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 1000, trControl = ctrl.1)

stopCluster(cl)

rf.5.cv.1

# Try to do the same with smaller amount of data
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10, index = cv.5.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 1000, trControl = ctrl.2)
stopCluster(cl)
rf.5.cv.2

# Try to do the same with 3-fold
set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.3.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 64, trControl = ctrl.3)
stopCluster(cl)
rf.5.cv.3


###################################################################################
#                                                                                 #
#                           EXPLORATORY MODELING # 2                              #
#                                                                                 #
###################################################################################
library(rpart)
library(rpart.plot)

rpart.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  rpart.cv <- train(x = training, y = rf.label, method = "rpart", tuneLength = 30, trControl = ctrl)
  
  stopCluster(cl)
  
  return(rpart.cv)
}


features <- c("Pclass","Title","FamilySize")
rpart.train.1 <- data.combined[1:891, features]

rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

rpart.1.cv.1$finalModel

table(data.combined$Title)

data.combined[1:25, "Name"]

name.splits <- str_split(data.combined$Name, ",")
name.splits[1]
last.names <- sapply(name.splits,"[",1)
last.names[1:10]

data.combined$last.name <- last.names

# Now for titles
name.splits <- str_split(sapply(name.splits,"[",2), " ")
titles <- sapply(name.splits,"[",2)
unique(titles)

data.combined[which(titles == "the"),]

# Re-map titles to be more exact
titles[titles %in% c("Dona.","the")] <- "Lady."
titles[titles %in% c("Ms.","Mlle.")] <- "Miss."
titles[titles %in% c("Mme.")] <- "Mrs."
titles[titles %in% c("Jonkheer.","Don.")] <- "Sir."
titles[titles %in% c("Col.","Major.","Capt.")] <- "Officer"
table(titles)

data.combined$new.title <- as.factor(titles)

ggplot(data.combined[1:891,], aes(x = new.title, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Survival based on New Title")

# Lets cram the titles data a little more into existing data buckets so we don't overfit
indexes <- which(data.combined$new.title == "Lady.")
data.combined$new.title[indexes] <- "Mrs."

indexes <- which(data.combined$new.title == "Dr." |
                 data.combined$new.title == "Rev." |
                 data.combined$new.title == "Sir." |
                 data.combined$new.title == "Officer")
data.combined$new.title[indexes] <- "Mr."

ggplot(data.combined[1:891,], aes(x = new.title, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Survival based on New Title")

features <- c("Pclass","new.title","FamilySize")
rpart.train.2 <- data.combined[1:891, features]

rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)

# One female?
first.mr.df[first.mr.df$Sex == "female",]

# Update new.title feature
indexes <- which(data.combined$new.title == "Mr." &
                 data.combined$Sex == "female")
data.combined$new.title[indexes] <- "Mrs."

length(which(data.combined$Sex == "female" &
             (data.combined$new.title == "Master." |
               data.combined$new.title == "Mr.")))

indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr,]

summary(first.mr.df[first.mr.df$Survived == "1",])
View(first.mr.df[first.mr.df$Survived == "1",])


indexes <- which(data.combined$Ticket == "PC 17755" |
                 data.combined$Ticket == "PC 17611" |
                 data.combined$Ticket == "113760")
View(data.combined[indexes,])


ggplot(first.mr.df, aes(x = Fare, fill = Survived))+
  geom_density(alpha = 0.5)+
  ggtitle("1st Class 'Mr.' Survival Rates by Fare")

ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)

length(tickets)

for(i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1],"Fare"] / length(party.indexes)
  
  for(k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare


# Refresh 1st Class Mr. data frame
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)

ggplot(first.mr.df[first.mr.df$Survived != "none",], aes(x = ticket.party.size, fill = Survived))+
  geom_density(alpha = 0.5)+
  ggtitle("Survival Rates 1st Class Mr. by ticket.party.size")

ggplot(first.mr.df[first.mr.df$Survived != "none",], aes(x = avg.fare, fill = Survived))+
  geom_density(alpha = 0.5)+
  ggtitle("Survival Rates 1st Class Mr. by avg.fare")

# Hypothesis - ticket.party.size and avg.fare seem to be highly correlated
summary(data.combined$avg.fare)

# One missing value!?
data.combined[is.na(data.combined$avg.fare),]

# Get records for similar passengers and summarize avg.fares
indexes <- with(data.combined,which(Pclass == "3" & Title == "Mr." & FamilySize == 1 & Ticket != "3701"))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$avg.fare)

# Use median since it is close to mean  and a little higher than mean
data.combined[is.na(avg.fare),"avg.fare"] <- 7.84

# Now leverage Caret's preProcess function to normalize data
preproc.data.combined <- data.combined[,c("ticket.party.size","avg.fare")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)

# Hypothesis refuted for all data
cor(postproc.data.combined$ticket.party.size, postproc.data.combined$avg.fare)

# How about just for 1st class all up?
indexes <- which(data.combined$Pclass == "1")
cor(postproc.data.combined$ticket.party.size[indexes], postproc.data.combined$avg.fare[indexes])

# Hypothesis refuted again!

# OK, let's see if our feature engineering has made any difference
features <- c("Pclass","new.title","FamilySize","ticket.party.size","avg.fare")
rpart.train.3 <- data.combined[1:891, features]

# Run CV
rpart.3.cv.1 <- rpart.cv(94622, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


####################################################################################################
#                                                                                                  #
#                                       FINAL THOUGHTS                                             #
#                                                                                                  #
####################################################################################################

test.submit.df <- data.combined[892:1309, features]

rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)

submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rpart.3.preds)

write.csv(submit.df, file = "Titanic using Random Forest - 20160717 - 2.CSV", row.names = FALSE)


# Let's try another approach now

features <- c("Pclass","new.title","ticket.party.size","avg.fare")
rf.train.temp <- data.combined[1:891, features]

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp

test.submit.df <- data.combined[892:1309, features]


rf.preds <- predict(rf.temp,test.submit.df)
table(rf.preds)

submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)

write.csv(submit.df, file = "Titanic using Random Forest - 20160717 - 3.CSV", row.names = FALSE)

###################
#
# Alright...let's see how we can get things even better
#

library(infotheo)

mutinformation(rf.label,data.combined$Pclass[1:891])
mutinformation(rf.label,data.combined$Sex[1:891])
mutinformation(rf.label,discretize(data.combined$Age[1:891]))
mutinformation(rf.label,data.combined$SibSp[1:891])
mutinformation(rf.label,data.combined$Parch[1:891])
mutinformation(rf.label,discretize(data.combined$Fare[1:891]))
mutinformation(rf.label,data.combined$Embarked[1:891])
mutinformation(rf.label,data.combined$Title[1:891])
mutinformation(rf.label,data.combined$FamilySize[1:891])
mutinformation(rf.label,data.combined$Ticket.first.char[1:891])
mutinformation(rf.label,data.combined$Cabin.multiple[1:891])
mutinformation(rf.label,data.combined$new.title[1:891])
mutinformation(rf.label,data.combined$ticket.party.size[1:891])
mutinformation(rf.label,discretize(data.combined$avg.fare[1:891]))

#-----------------
#
# Now we'll leverage the 'tsne' algorithm to create 2-D representation of our data suitable
# for visualization starting with folks our model gets right very often - females and boys.

library(Rtsne)

most.correct <- data.combined[data.combined$new.title != "Mr.",]
indexes <- which(most.correct$Survived != "none")

tsne.1 <- Rtsne(most.correct[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.1$Y[indexes,1], y = tsne.1$Y[indexes,2],
                 color = most.correct$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D visualization of features for Females and Boys")

# To get a baseline, let's use conditional mutual information on the tsne x and
# Y features for females and boys in 1st and 2nd class. The intuition here is that
# the combination of these features should be higher than any individual featueres
# we looked at above.

condinformation(most.correct$Survived[indexes], discretize(tsne.1$Y[indexes,]))

# As one more comparison, we can leverage conditional mutual information using
# the top two features used in our tree plot - new.title and Pclass

condinformation(rf.label,data.combined[1:891,c("new.title","Pclass")])

# OK. Now let's take a look at adult males since our model has the biggest
# potential upside for improving (i.e., the tree predicts incorrectly for 86
# adult males). Let's visualize with tsne.
misters <- data.combined[data.combined$new.title == "Mr.",]
indexes <- which(misters$Survived != "none")

tsne.2 <- Rtsne(misters[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.2$Y[indexes,1], y = tsne.2$Y[indexes,2],
                 color = misters$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D visualization of Features for new.Title of 'Mr.'")

# Now, conditional mutual information for tsne features for adult males
condinformation(misters$Survived[indexes], discretize(tsne.2$Y[indexes,]))

#
# Idea - How about creating tsne features for all of the training data and
# using them in our model?
#

tsne.3 <- Rtsne(data.combined[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.3$Y[1:891, 1], y = tsne.3$Y[1:891, 2],
                 color = data.combined$Survived[1:891])) +
  geom_point() +
  labs(color = "Survivde") +
  ggtitle("tsne 2D visualization of Features of all training data")

# Conditional Mutual information
condinformation(data.combined$Survived[1:891], discretize(tsne.3$Y[1:891,]))

# Add the tsne features to our data frame for use in model building
data.combined$tsne.x <- tsne.3$Y[,1]
data.combined$tsne.y <- tsne.3$Y[,2]

