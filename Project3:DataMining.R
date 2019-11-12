# preprocess

setwd("/home/dell/mne/BABI/Project3/")
rawdata = read.csv("TheraBank.csv", header = TRUE)

dim(rawdata)
anyNA(rawdata)

#renaming
names = names(rawdata)
names[2] = "Age"
names[3] = "Experience"
names[4] = "Income"
head(rawdata,1)
colnames(rawdata) = names

#converting to factorss
rawdata$ID = as.factor(rawdata$ID)
rawdata$ZIP.Code = as.factor(rawdata$ZIP.Code)
rawdata$Family.members = as.factor(rawdata$Family.members)
rawdata$ID = as.factor(rawdata$CCAvg)
rawdata$Education = as.factor(rawdata$Education)
rawdata$Personal.Loan = as.factor(rawdata$Personal.Loan)
rawdata$Securities.Account = as.factor(rawdata$Securities.Account)
rawdata$CD.Account = as.factor(rawdata$CD.Account)
rawdata$Online = as.factor(rawdata$Online)
rawdata$CreditCard = as.factor(rawdata$CreditCard)

#handling missing / inaccurate values
rawdata$Family.members = ifelse(is.na(rawdata$Family.members), 0, rawdata$Family.members)
rawdata$Availed.Mortgage = ifelse(rawdata$Mortgage == 0, 0, 1)
rawdata$Experience = ifelse(rawdata$Experience < 0, 0, rawdata$Experience)
summary(rawdata)

hist(rawdata$Age, main="Frequency by Age", xlab="Age")

#standardize data
rawdata$std.age <- (rawdata$Age - mean(rawdata$Age)) / sd(rawdata$Age)
rawdata$std.experience <- (rawdata$Experience - mean(rawdata$Experience)) / sd(rawdata$Experience)
rawdata$std.income <- (rawdata$Income - mean(rawdata$Income)) / sd(rawdata$Income)
qplot(rawdata$std.age, geom="density", xlab="Age")
qplot(rawdata$Experience, geom="density", xlab="Experience")
qplot(rawdata$Income, geom="density", xlab="Income")

#form data frame
names(rawdata)
bankdata = rawdata[c( -1, -2, -3, -4, -5, -9 )]

# install.packages("ggplot2")
library(ggplot2)

qplot(rawdata$std.age, rawdata$std.income, data=rawdata)

boxplot(bankdata, col = rep(c("Red", "Green", "Blue", "Yellow", "Purple" )))

#correlation
cormatrix = cor(bankdata[10:12])

library(corrplot)
corrplot(cormatrix, method = "number")


#forming clusters
seed = 1000
set.seed(seed)

clust2 = kmeans(x = bankdata, centers = 2, nstart = 5)
clust2

library(cluster)
clusplot(bankdata, clust2$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 1)

tot.wss = rep(0,5)
for(k in 1:5) {
  set.seed(seed)
  clust = kmeans(x = bankdata, centers = k, nstart = 5)
  tot.wss[k] = clust$tot.withinss
}

tot.wss
plot(c(1:5), tot.wss, type = "b")

# install.packages("NbClust");
library(NbClust)
nc = NbClust(bankdata[9:12], min.nc = 2, max.nc = 5, method="kmeans")
nc

table(nc$Best.n[1,])

clust = kmeans(x = bankdata[10:12], centers = 3, nstart = 5)
clust
clusplot(bankdata[10:12], clust$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 1)

gower_dist = daisy(bankdata,metric = "gower")
gower_mat = as.matrix(gower_dist)

# =============================================

# Question 3
install.packages("caTools")
library(caTools)
split = sample.split(bankdata$Personal.Loan, SplitRatio = 0.7)
train<- subset(bankdata, split == TRUE)
test<- subset( bankdata, split == FALSE)

sum(train$Personal.Loan == 0)/nrow(train)

#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
set.seed(seed)
tree = rpart(formula = Personal.Loan ~ ., data = train, method = "class", minbucket = 3, cp = 0)
tree


rpart.plot(tree)
printcp(tree)
plotcp(tree)

ptree = prune(tree, cp = 0.007, "CP")
ptree
printcp(ptree)
rpart.plot(ptree)

path.rpart(ptree,c(2:6))

train$prediction = predict(ptree, data = train, type="class")
train$score = predict(ptree, data = train, type = "prob")

tbl = table(train$Personal.Loan, train$prediction)
print(tbl)
print((tbl[1,2]+tbl[2,1])/3500)
#error rate = 1.08%
#=======================================

# Question 4
# install.packages("randomForest")
library(randomForest)
set.seed(seed)
split = sample.split(bankdata$Personal.Loan, SplitRatio = 0.5)
rndtrain<- subset(bankdata, split == TRUE)
rndtest<- subset( bankdata, split == FALSE)
rndForest = randomForest(Personal.Loan ~ ., data = rndtrain, mtry = 3, ntree = 201, nodesize=10, importance = TRUE)
rndForest
plot(rndForest)
importance(rndForest)

set.seed(seed)
tunedRndForest = tuneRF(x = rndtrain[, -c(4)], y = rndtrain$Personal.Loan, mtryStart = 3, stepFactor = 1.5, ntree = 51, 
                        improve = 0.0001, trace = TRUE, plot = TRUE, doBest = TRUE, nodesize = 10 , importance = TRUE)
importance(tunedRndForest)

rndtrain$prediction =predict(tunedRndForest, data=rndtrain, type="class")
rndtrain$prob1 = predict(tunedRndForest, data=rndtrain, type="prob")[,"1"]

qs = quantile(rndtrain$prob1, prob=seq(0,1,length = 11))
qs
threshold = qs[10]
mean(rndtrain$Personal.Loan[rndtrain$prob1>threshold]=="1")

rndtest$prediction = predict(tunedRndForest, data=rndtest, type="class")
rndtest$prob1 = predict(tunedRndForest, data=rndtest, type="prob")[,"1"]

qstest = quantile(rndtest$prob1, prob=seq(0,1,length = 11))
qstest
testthreshold = qstest[10]
mean(rndtest$Personal.Loan[rndtest$prob1>testthreshold]=="1")

install.packages("knitr")
library(knitr)
knitr::stitch('Project3:DataMining.R')
