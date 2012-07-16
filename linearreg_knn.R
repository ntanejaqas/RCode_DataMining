zip.train <- read.table(file="http://www.isye.gatech.edu/~ymei/7406/Handouts/zip.train.csv",sep = ",");
ziptrain23 <- subset(zip.train, zip.train[,1]==2 | zip.train[,1]==3);
zip.test <- read.table(file="http://www.isye.gatech.edu/~ymei/7406/Handouts/zip.test.csv",sep = ",");
ziptest23 <- subset(zip.test, zip.test[,1]==2 | zip.test[,1]==3);
# linear Regression
mod1 <- lm( V1 ~ . , data= ziptrain23);

# linear Regression - training error
trainpred <- predict.lm(mod1, ziptrain23[,-1]);
trainpred1 <- ifelse(trainpred > 0.5, 3, 2); 
sum(trainpred1 != ziptrain23[,1]) / nrow(ziptrain23);
#0.5262779

# linear Regression - test error
testpred <- predict.lm(mod1, ziptest23[,-1]);
testpred1 <- ifelse(testpred > 0.5, 3, 2); 
sum(testpred1 != ziptest23[,1]) / nrow(ziptest23);
#0.543956

#knn
library(class)
ks <- c(1,3,5,7,15)
nks <- length(ks);
misclass.train <- numeric(length= nks);
misclass.test  <- numeric(length= nks);

for (i in 1:nks ){
  mod.train <- knn( ziptrain23[,-1], ziptrain23[,-1], k = ks[i], cl = ziptrain23[,1]);
  mod.test  <- knn(ziptrain23[,-1], ziptest23[,-1], k = ks[i], cl= ziptrain23[,1]); 
  misclass.train[i] <- sum( mod.train != ziptrain23[,1]) / nrow(ziptrain23);
  misclass.test[i]  <- sum( mod.test != ziptest23[,1]) / nrow(ziptest23);
}

knnresult <- cbind(ks, misclass.train, misclass.test)

#      ks misclass.train misclass.test
# [1,]  1    0.000000000    0.02472527
# [2,]  3    0.005039597    0.03021978
# [3,]  5    0.005759539    0.03021978
# [4,]  7    0.006479482    0.03296703
# [5,] 15    0.009359251    0.03846154