zip.train <- read.table(file="C:\\Users\\Arvind\\Desktop\\zip.train.csv",sep = ",");
ziptrain23 <- subset(zip.train, zip.train[,1]==2 | zip.train[,1]==3);
zip.test <- read.table(file="C:\\Users\\Arvind\\Desktop\\zip.test.csv",sep = ",");
ziptest23 <- subset(zip.test, zip.test[,1]==2 | zip.test[,1]==3);

#LDA
library(MASS)
fit1 <- lda(ziptrain23[,-1], ziptrain23[,1], tol=1.0e-100)

ldapred1 <- predict(fit1, ziptrain23[,-1])$class
mean(ldapred1 != ziptrain23[,1])
#0.005759539

ldapred2 <- predict(fit1, ziptest23[,-1])$class
mean(ldapred2 != ziptest23[,1])
#0.04120879

#QDA
fit2 <- qda(ziptrain23[,35:257], ziptrain23[,1])

qdapred1 <- predict(fit2, ziptrain23[,35:257])$class
mean(qdapred1 != ziptrain23[,1])
#0

qdapred2 <- predict(fit2, ziptest23[,35:257])$class
mean(qdapred2 != ziptest23[,1])
#0.03846154

#NaiveBayes
library(e1071)

ziptrain23[,1] <- as.factor(ziptrain23[,1])
fit3 <- naiveBayes( V1~., data=ziptrain23)
nbpred1 <- predict(fit3, ziptrain23[,-1]);
mean( nbpred1 != ziptrain23[,1])
#0.04391649

ziptest23[,1] <- as.factor(ziptest23[,1])
nbpred2 <- predict(fit3, ziptest23[,-1]);
mean( nbpred2 != ziptest23[,1])
#0.07142857

#Logistic Regression
#fit4 <- glm(V1 ~ ., family = binomial(link="logit"), data = ziptrain23);
fit4 <- multinom(V1~., data=ziptrain23)
logregpred1 <- predict(fit4, ziptrain23[,-1])
mean(logregpred1 != ziptrain23[,1])
#0

logregpred2 <- predict(fit4, ziptest23[,-1])
mean(logregpred2 != ziptest23[,1])
#0.05494505

