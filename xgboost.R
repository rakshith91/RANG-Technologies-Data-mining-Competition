set.seed(7)
library(xgboost)
setwd("/u/bsairamr/RANG")
#fetch data
train = read.csv("fseldata.csv", header= T)
test <- read.csv("fseltest.csv", header=T)

cl<- read.csv("classVector.csv", header=T)
cl <- cl[ , -1]


train <- as.matrix(train)
test<-as.matrix(test)

names <- read.csv("names.csv", header=T)
names <- names[, -1]

#replace NAs with 0's
class(train)<- "numeric"
class(test) <- "numeric"

#normalise data and tesr

print("normalisiing data")
for(i in 1:ncol(train)){
  max = max(train[, i])
  min = min(train[, i])
  for( j in 1: nrow(train)){
    train[j, i]= (train[j, i]-min)/(max-min)
  }

}

print("starting to normalize test")
##normalize the test
for(i in 1:ncol(test)){
  max = max(test[, i])
  min = min(test[, i])
  for( j in 1: nrow(test)){
   test[j, i]= (test[j, i]-min)/(max-min)
 }

}


print("starting xgb")
param <- list("booster"='gbtree',
              "objective" = "binary:logistic"  #for binary classification,
              "eval_metric" = "error",
              "eta"=0.02,  #step size
              "max_depth"=6 #max depth of the tree, 
		"subsample"=0.5 # a sub sample of 0.5 avoids overfitting)
#cross validation
xgb.cv <- xgb.cv(data = train, label=cl, param= param ,nfold=5, nround=15, seed=10, nthread=5)

#create the xgb model
xgb <- xgboost(data = train, label=cl, param= param , nround=25, seed=10, nthread=5)

#calculate and plot the importance of features. Default is they are ranked according to Gini imputiy.
impmat<- xgb.importance(model = xgb)
#print(impmat)
xgb.plot.importance(impmat[1:10])
#predict the class for tes data
o2 <- predict(xgb, test)

#binarize output since output is a lost of probabilities. 
custList <- read.csv("custList.csv", header=T)
custList<- custList$Cust_id

for(i in 1:11042){
  if(o2[i]>0.5){
    o2[i]=1
  }
  else{
    o2[i]=0
  }
}

#write output to file in the submission Format. 
df = data.frame(matrix(vector(), 11042, 2,
                dimnames=list(c(), c("Cust_id", "Active_Customer"))),
                stringsAsFactors=F)
df[, 1]=custList
df[, 2]=o2
write.csv(df, "output20.csv")
