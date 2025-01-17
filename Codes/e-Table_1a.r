load(file="fdata_all_R2b.RData")

##

D <- cbind(y,X)
ccb <- complete.cases(D)
 
y <- y[ccb]
X <- X[ccb,]
 
N <- length(y)

##

set.seed(123456)
ord <- sample(1:N,N,replace=FALSE)

##

library(glmnet)

K <- 10			# number of split in CV

m <- floor(N/K)

A2 <- (1:K)*m; A2[K] <- N
A1 <- c(1, A2[1:(K-1)]+1)

##

predr <- rep(NA,times=N)

for(r in 1:K){

 r.i <- A1[r]:A2[r]			# index of validation data
 t.i <- setdiff(1:N,r.i)	# index of derivation data
 
 ##
 
 bs.i <- ord[t.i]		# training data

 yb <- y[bs.i]
 Xb <- X[bs.i,]
 
 lasso01b <- cv.glmnet(x=Xb, y=yb, family="gaussian", alpha=0, nfolds = 10)

 lambda01b <- lasso01b$lambda.min

 lasso02b <- glmnet(x=Xb, y=yb, family="gaussian", lambda=lambda01b, alpha=0)

 ##

 vd.i <- ord[r.i]
 
 yv <- y[vd.i]
 Xv <- X[vd.i,]
 
 pred02b <- predict(lasso02b,type="response",newx=Xv)
 predr[vd.i] <- as.numeric(pred02b)

 print(r)

}

##

lm02 <- lm(y ~ predr)
slm <- summary(lm02)

calib00 <- slm$coefficients[1,1]
calib01 <- slm$coefficients[2,1]
R2 <- slm$r.squared
R <- sqrt(R2)
bias <- mean( (y - predr) )
RMSE <- sqrt( mean( (y - predr)^2 ) )
MAE <- mean( abs(y - predr) )

Q <- c(N,R2,R,bias,MAE,RMSE,calib00,calib01)

