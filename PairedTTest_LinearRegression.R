mtrain <- read.table(file="C:\\Users\\Arvind\\Desktop\\mtrain.csv",sep = ",");
mtest <- read.table(file="C:\\Users\\Arvind\\Desktop\\mtest.csv",sep = ",");

MAXITERATIONS <- 1000
trainerror <- 0
testerror <- 0

mtrainmeanY <- numeric(length=nrow(mtrain))
mtrainYhat <- numeric(length=nrow(mtrain))
mtestmeanY <- numeric(length=nrow(mtest))
mtestYhat <- numeric(length=nrow(mtest))

for (i in 1:nrow(mtrain)){
  tmpU <- mtrain[i,2]/(2*sqrt(1+mtrain[i,1]))
  tmpSum <- 0
  for (n in 1:MAXITERATIONS){
    tmpSum <- tmpSum + pnorm(-sqrt(n)*tmpU/2)/n
  }
  yhat <- exp(-2*tmpSum)
  mtrainYhat[i] = yhat
  for (k in 3:ncol(mtrain)){
    trainerror <- trainerror + (mtrain[i,k] - yhat)*(mtrain[i,k] - yhat)
  }
  mtrainmeanY[i] = mean(as.numeric(mtrain[i,3:202]))
}
trainerror <- trainerror /(nrow(mtrain)*200)
#0.009883794
t.test(mtrainYhat, mtrainmeanY, paired=T)
# Paired t-test

# data:  mtrainYhat and mtrainmeanY 
# t = -30.5474, df = 2910, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
 # -0.05153846 -0.04532123 
# sample estimates:
# mean of the differences 
            # -0.04842984 

for (i in 1:nrow(mtest)){
  tmpU <- mtest[i,2]/(2*sqrt(1+mtest[i,1]))
  tmpSum <- 0
  for (n in 1:MAXITERATIONS){
    tmpSum <- tmpSum + pnorm(-sqrt(n)*tmpU/2)/n
  }
  yhat <- exp(-2*tmpSum)
  mtestYhat[i] = yhat
  for (k in 3:ncol(mtest)){
    testerror <- testerror + (mtest[i,k] - yhat)*(mtest[i,k] - yhat)
  }
  mtestmeanY[i] = mean(as.numeric(mtest[i,3:202]))
}

testerror <- testerror /(nrow(mtest)*200)
#0.008885895

t.test(mtestYhat, mtestmeanY, paired=T)

# Paired t-test

# data:  mtestYhat and mtestmeanY 
# t = -12.3855, df = 1065, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
 # -0.03845314 -0.02793541 
# sample estimates:
# mean of the differences 
            # -0.03319428 

#
#
#Fit Linear Regression model to this normalized feature set
#
#

#Subtract the mean and divide by sample covariance to get the normalized feature values
mtrain[,1:2] = scale(mtrain[,1:2], scale=TRUE, center=TRUE)
mtest[,1:2] = scale(mtest[,1:2], scale=TRUE, center=TRUE)

mtrain$Y = mtrainmeanY
mtest$Y = mtestmeanY

mod1 <- lm(Y~V1+V2, data=mtrain)
lrtrainpred <- predict.lm(mod1, mtrain[,1:2]);
lmtrainerror <- 0
for (i in 1:nrow(mtrain)){
  for(k in 3:202){
    lmtrainerror <- lmtrainerror + (mtrain[i,k] - lrtrainpred[i])*(mtrain[i,k] - lrtrainpred[i])
  }
}
lmtrainerror <- lmtrainerror / (nrow(mtrain)*200)
#0.0006400955

t.test(lrtrainpred, mtrainmeanY, paired=T)

# Paired t-test

# data:  lrtrainpred and mtrainmeanY 
# t = 0, df = 2910, p-value = 1
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
 # -0.0007413608  0.0007413608 
# sample estimates:
# mean of the differences 
          # -7.544004e-16

lmtesterror <- 0
lrtestpred <- predict.lm(mod1, mtest[,1:2]);
for (i in 1:nrow(mtest)){
  for(k in 3:202){
    lmtesterror <- lmtesterror + (mtest[i,k] - lrtestpred[i])*(mtest[i,k] - lrtestpred[i])
  }
}
lmtesterror <- lmtesterror/(nrow(mtest)*200)
#0.0006227432

t.test(lrtestpred, mtestmeanY, paired=T)

# Paired t-test

# data:  lrtestpred and mtestmeanY 
# t = -0.6972, df = 1065, p-value = 0.4858
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
# -0.0018007255  0.0008565094 
# sample estimates:
# mean of the differences 
         # -0.000472108