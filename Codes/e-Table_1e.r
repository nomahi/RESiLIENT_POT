load(file="fdata_all_R2b.RData")

##

D <- cbind(y,X)
ccb <- complete.cases(D)
 
y <- y[ccb]
X <- X[ccb,]
X0 <- X0[ccb,]
T <- T[ccb,]
 
N <- length(y)

W <- numeric(N)
for(i in 1:N){
	if(sum(T[i,])>0)	W[i] <- which(T[i,]==1)
}
W <- factor(W)

##

set.seed(123456)			
ord <- sample(1:N,N,replace=FALSE)	

##

library(grf)

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
 Xb <- X0[bs.i,]
 Wb <- W[bs.i]

 cate.forest <- multi_arm_causal_forest(Xb, yb, Wb)

 ##

 vd.i <- ord[r.i]
 
 Xv <- X0[vd.i,]
 pred02 <- predict(cate.forest, Xv,drop=TRUE)$predictions
 
 wb <- as.numeric(Wb) - 1
 
 for(i in 1:length(vd.i)){
 
	if(wb[i]>=1) predr[vd.i[i]] <- pred02[i,wb[i]]
	if(wb[i]==0) predr[vd.i[i]] <- 0

 }

 print(r)

}

##

bias <- mean( (y - predr) )
predr <- predr + bias 

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
