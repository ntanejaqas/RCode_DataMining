zip.train <- read.table(file="C:\\Users\\Arvind\\Desktop\\zip.train.csv",sep = ",");
ziptrain235 <- subset(zip.train, zip.train[,1]==2 | zip.train[,1]==3 | zip.train[,1]==5);
zip.test <- read.table(file="C:\\Users\\Arvind\\Desktop\\zip.test.csv",sep = ",");
ziptest235 <- subset(zip.test, zip.test[,1]==2 | zip.test[,1]==3 | zip.test[,1]==5);

#LDA
library(MASS)
fit1 <- lda(ziptrain235[,-1], ziptrain235[,1])

ldapred1 <- predict(fit1, ziptrain235[,-1])$class
mean(ldapred1 != ziptrain235[,1])
#0.02313625

ldapred2 <- predict(fit1, ziptest235[,-1])$class
mean(ldapred2 != ziptest235[,1])
#0.09541985

#QDA
fit2 <- qda(ziptrain235[,35:257], ziptrain235[,1])

qdapred1 <- predict(fit2, ziptrain235[,35:257])$class
mean(qdapred1 != ziptrain235[,1])
#0.0005141388

qdapred2 <- predict(fit2, ziptest235[,35:257])$class
mean(qdapred2 != ziptest235[,1])
#0.06870229

#NaiveBayes
library(e1071)

ziptrain235[,1] <- as.factor(ziptrain235[,1])
fit3 <- naiveBayes( V1~., data=ziptrain235)
nbpred1 <- predict(fit3, ziptrain235[,-1]);
mean( nbpred1 != ziptrain235[,1])
#0.08997429

ziptest235[,1] <- as.factor(ziptest235[,1])
nbpred2 <- predict(fit3, ziptest235[,-1]);
mean( nbpred2 != ziptest235[,1])
#0.1183206

#Logistic Regression
#fit4 <- glm(V1 ~ ., family = binomial(link="logit"), data = ziptrain23);
fit4 <- multinom(V1~., data=ziptrain235)
logregpred1 <- predict(fit4, ziptrain235[,-1])
mean(logregpred1 != ziptrain235[,1])
#0

logregpred2 <- predict(fit4, ziptest235[,-1])
mean(logregpred2 != ziptest235[,1])
#0.1450382

#KNN
library(class)
ks <- c(1,3,5,7,15)
nks <- length(ks);
misclass.train <- numeric(length= nks);
misclass.test  <- numeric(length= nks);

for (i in 1:nks ){
  mod.train <- knn( ziptrain235[,-1], ziptrain235[,-1], k = ks[i], cl = ziptrain235[,1]);
  mod.test  <- knn(ziptrain235[,-1], ziptest235[,-1], k = ks[i], cl= ziptrain235[,1]); 
  misclass.train[i] <- sum( mod.train != ziptrain235[,1]) / nrow(ziptrain235);
  misclass.test[i]  <- sum( mod.test != ziptest235[,1]) / nrow(ziptest235);
}

knnresult <- cbind(ks, misclass.train, misclass.test)
# ks misclass.train misclass.test
# 1    0.000000000    0.04580153
# 3    0.007712082    0.05152672
# 5    0.010796915    0.04580153
# 7    0.011825193    0.04770992
# 15    0.018508997    0.06297710
