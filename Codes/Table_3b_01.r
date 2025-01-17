load(file="fdata_all_R2b.RData")

##

library("naniar")
miss_var_summary(data.frame(X0))

N <- length(y)

time0 <- Sys.time()

##

Xc <- X0[,c("age","sex","dem_3_marital","dem_4_cohab","education_years","working","gh_2","gh_6","phq9base")]
Xc <- data.frame(Xc,Z[,c(1:3,6)],y)

Xc$sex <- factor(Xc$sex)

##

set.seed(123456)	
ord <- order(reqday)	

##

# mice

library(glmnet)
library(mice)

B <- 100			# number of imputation

K <- 10			# number of split in CV

m <- floor(N/K)

A2 <- (1:K)*m; A2[K] <- N
A1 <- c(1, A2[1:(K-1)]+1)

##

predr <- rep(NA,times=N)

predmat1 <- make.predictorMatrix(data=Xc)

predmat1["gh_6",c("PHQ9_2","PHQ9_3","PHQ9_6","y")] <- 0
predmat1["sex",c("PHQ9_2","PHQ9_3","PHQ9_6","y")] <- 0
predmat1["dem_4_cohab",c("PHQ9_2","PHQ9_3","PHQ9_6","y")] <- 0
predmat1["education_years",c("PHQ9_2","PHQ9_3","PHQ9_6","y")] <- 0
 
predmat1["PHQ9_1",c("PHQ9_3","PHQ9_6","y")] <- 0
predmat1["PHQ9_2",c("phq9base","PHQ9_6","y")] <- 0
predmat1["PHQ9_3",c("phq9base","PHQ9_1","y")] <- 0
predmat1["PHQ9_6",c("phq9base","PHQ9_1","PHQ9_2")] <- 0
predmat1["y",c("phq9base","PHQ9_1","PHQ9_2","PHQ9_3")] <- 0

imX0 <- mice(Xc, m=B, predictorMatrix=predmat1)
 
for(r in 1:K){

 r.i <- A1[r]:A2[r]			# index of validation data
 t.i <- setdiff(1:N,r.i)	# index of derivation data
 
 ##
 
 bs.i <- ord[t.i]		# training data

 yb <- y[bs.i]
 Xb <- X0[bs.i,]
 Zb <- data.frame(Z[bs.i,])
 trtb <- trt[bs.i]

 ##
 
 vd.i <- ord[r.i]		# validation data

 yv <- y[vd.i]
 Xv <- X0[vd.i,]
 Zv <- data.frame(Z[vd.i,])
 trtv <- trt[vd.i]

 ##

 Xcr <- Xc[bs.i,]
 Xcv <- Xc[vd.i,]

 icer <- mice(Xcr, m=B, predictorMatrix=predmat1)

 ##
 
 predrb <- matrix(numeric(B*length(r.i)),length(r.i))

 for(b in 1:B){

	Db <- complete(icer,b)

	X0b <- Xb

	X0b[,"sex"] <- Db[,"sex"]
	X0b[,"dem_4_cohab"] <- Db[,"dem_4_cohab"]
	X0b[,"education_years"] <- Db[,"education_years"]
	X0b[,"gh_6"] <- Db[,"gh_6"]

	yb <- Db[,"y"]

	##

	p0 <- dim(X0b)[2]

	X1 <- X2 <- X3 <- X4 <- X5 <- X6 <- X7 <- X8 <- X9 <- X0b

	x1 <- as.numeric(trtb=="C1")
	x2 <- as.numeric(trtb=="C2")
	x3 <- as.numeric(trtb=="C3")
	x4 <- as.numeric(trtb=="C4")
	x5 <- as.numeric(trtb=="C5")
	x6 <- as.numeric(trtb=="C6")
	x7 <- as.numeric(trtb=="C7")
	x8 <- as.numeric(trtb=="C8")
	x9 <- as.numeric(trtb=="C9")

	for(k in 1:p0)	X1[,k] <- x1*X0b[,k]
	for(k in 1:p0)	X2[,k] <- x2*X0b[,k]
	for(k in 1:p0)	X3[,k] <- x3*X0b[,k]
	for(k in 1:p0)	X4[,k] <- x4*X0b[,k]
	for(k in 1:p0)	X5[,k] <- x5*X0b[,k]
	for(k in 1:p0)	X6[,k] <- x6*X0b[,k]
	for(k in 1:p0)	X7[,k] <- x7*X0b[,k]
	for(k in 1:p0)	X8[,k] <- x8*X0b[,k]
	for(k in 1:p0)	X9[,k] <- x9*X0b[,k]

	colnames(X1) <- paste0(colnames(X0b),"_C1")
	colnames(X2) <- paste0(colnames(X0b),"_C2")
	colnames(X3) <- paste0(colnames(X0b),"_C3")
	colnames(X4) <- paste0(colnames(X0b),"_C4")
	colnames(X5) <- paste0(colnames(X0b),"_C5")
	colnames(X6) <- paste0(colnames(X0b),"_C6")
	colnames(X7) <- paste0(colnames(X0b),"_C7")
	colnames(X8) <- paste0(colnames(X0b),"_C8")
	colnames(X9) <- paste0(colnames(X0b),"_C9")

	Tb <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9)
	colnames(Tb) <- c("C1","C2","C3","C4","C5","C6","C7","C8","C9")

	Xbb <- cbind(Tb,X0b,X1,X2,X3,X4,X5,X6,X7,X8,X9)		

	Xbb <- as.matrix(Xbb)

	lasso01b <- cv.glmnet(x=Xbb, y=yb, family="gaussian", alpha=1, nfolds = 10)

	lambda01b <- lasso01b$lambda.min

	lasso02b <- glmnet(x=Xbb, y=yb, family="gaussian", lambda=lambda01b, alpha=1)

	##

	Dv <- complete(imX0,b)[vd.i,]

	X0v <- Xv

	X0v[,"sex"] <- Dv[,"sex"]
	X0v[,"dem_4_cohab"] <- Dv[,"dem_4_cohab"]
	X0v[,"education_years"] <- Dv[,"education_years"]
	X0v[,"gh_6"] <- Dv[,"gh_6"]

	##

	p0 <- dim(X0v)[2]

	X1 <- X2 <- X3 <- X4 <- X5 <- X6 <- X7 <- X8 <- X9 <- X0v

	x1 <- as.numeric(trtv=="C1")
	x2 <- as.numeric(trtv=="C2")
	x3 <- as.numeric(trtv=="C3")
	x4 <- as.numeric(trtv=="C4")
	x5 <- as.numeric(trtv=="C5")
	x6 <- as.numeric(trtv=="C6")
	x7 <- as.numeric(trtv=="C7")
	x8 <- as.numeric(trtv=="C8")
	x9 <- as.numeric(trtv=="C9")

	for(k in 1:p0)	X1[,k] <- x1*X0v[,k]
	for(k in 1:p0)	X2[,k] <- x2*X0v[,k]
	for(k in 1:p0)	X3[,k] <- x3*X0v[,k]
	for(k in 1:p0)	X4[,k] <- x4*X0v[,k]
	for(k in 1:p0)	X5[,k] <- x5*X0v[,k]
	for(k in 1:p0)	X6[,k] <- x6*X0v[,k]
	for(k in 1:p0)	X7[,k] <- x7*X0v[,k]
	for(k in 1:p0)	X8[,k] <- x8*X0v[,k]
	for(k in 1:p0)	X9[,k] <- x9*X0v[,k]

	colnames(X1) <- paste0(colnames(X0v),"_C1")
	colnames(X2) <- paste0(colnames(X0v),"_C2")
	colnames(X3) <- paste0(colnames(X0v),"_C3")
	colnames(X4) <- paste0(colnames(X0v),"_C4")
	colnames(X5) <- paste0(colnames(X0v),"_C5")
	colnames(X6) <- paste0(colnames(X0v),"_C6")
	colnames(X7) <- paste0(colnames(X0v),"_C7")
	colnames(X8) <- paste0(colnames(X0v),"_C8")
	colnames(X9) <- paste0(colnames(X0v),"_C9")

	Tv <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9)
	colnames(Tv) <- c("C1","C2","C3","C4","C5","C6","C7","C8","C9")

	Xbv <- cbind(Tv,X0v,X1,X2,X3,X4,X5,X6,X7,X8,X9)		

	Xbv <- as.matrix(Xbv)

	##

	pred02b <- predict(lasso02b,type="response",newx=Xbv)	

	predrb[,b] <- as.numeric(pred02b)

	time1 <- Sys.time() - time0

	print(c(r,b))
	print(time1)
	
 }

 predr[vd.i] <- apply(predrb,1,mean)

}

##

lm02 <- lm(y ~ predr)
slm <- summary(lm02)

calib00 <- slm$coefficients[1,1]
calib01 <- slm$coefficients[2,1]
R2 <- slm$r.squared
R <- sqrt(R2)
bias <- mean( (y - predr) , na.rm=TRUE)
RMSE <- sqrt( mean( (y - predr)^2  , na.rm=TRUE) )
MAE <- mean( abs(y - predr) , na.rm=TRUE)

Q <- c(N,R2,R,bias,MAE,RMSE,calib00,calib01)

