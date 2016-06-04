set.seed(7)
library(xgboost)
setwd("/u/bsairamr/RANG")
#fetch data
rg = read.csv("fseldata.csv", header= T)
test <- read.csv("fseltest.csv", header=T)
#rg$Cust_id <-NULL
#rg$Active_Customer <- NULL
cl<- read.csv("classVector.csv", header=T)
cl <- cl[ , -1]
#rg$Cust_Tenure <- NULL
#test$Cust_Tenure <- NULL
#rg$Active_Customer <- cl$x
#str(rg)
rg  <- rg[ , c(1,2,3,4,5,6)]
test <- test[ , c(1,2,3,4,5,6)]
#rg  <- rg[ , 1]
#test <- test[ , 1]

rg <- as.matrix(rg)
test<-as.matrix(test)
print(length(test))
print(ncol(test))
names <- read.csv("names.csv", header=T)
names <- names[, -1]
class(rg)<- "numeric"
class(test) <- "numeric"

#generate rf model
#rf = randomForest(Active_Customer ~ ., data = rg, mtry = c(10,20,30,40, 50) , ntree = 1000)
#str(rf)
#predict RF
#o2 <- predict(rf, test)

##importance play
#imp <- importance(rf)
#normalize the data
#rg <- rg [ , -10]
#test <- test[ , -10]
#rg <- rg [ , -9]
#test <- test[ , -9]
#rg <- rg [ , c(1,2,3,4,5)]
#test <- test[ , c(1,2,3,4,5)]

print("normalisiing data")
for(i in 1:ncol(rg)){
  max = max(rg[, i])
  min = min(rg[, i])
  for( j in 1: nrow(rg)){
    rg[j, i]= (rg[j, i]-min)/(max-min)
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
print(names(rg))
print("starting xgb")
param <- list("booster"='gbtree',
              "objective" = "binary:logistic",
              "eval_metric" = "error",
              "eta"=0.02,  #step size
              "max_depth"=6, 
		"subsample"=0.5)
xgb.cv <- xgb.cv(data = rg, label=cl, param= param ,nfold=5, nround=15, seed=10, nthread=5)
xgb <- xgboost(data = rg, label=cl, param= param , nround=25, seed=10, nthread=5)

#impmat<- xgb.importance(model = xgb)
#print(impmat)
#xgb.plot.importance(impmat[1:10])
o2 <- predict(xgb, test)

#binarize output
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
#write output to file
df = data.frame(matrix(vector(), 11042, 2,
                dimnames=list(c(), c("Cust_id", "Active_Customer"))),
                stringsAsFactors=F)
df[, 1]=custList
df[, 2]=o2
write.csv(df, "output20.csv")
#print(df[, 1])

##eliminate features
#featurelist <- c(rg$Trans8, rg$Trans9, rg$Trans10, rg$Trans11, rg$Trans41)
#latestdata <- data.frame(matrix(vector(), 25766 , 1), stringsAsFactors=F)



#latesttest <- matrix(numeric(0), nrow = 11042, ncol = 15)


#rf = randomForest(V16 ~ ., data = latestdata,type="classification",  mtry = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50) , ntree = 1000)
#o2 <- predict(rf, latesttest)
#cl<- rg$Active_Customer





