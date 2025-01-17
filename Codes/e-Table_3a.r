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

##

beta.m <- NULL
 
for(b in 1:B){

	Db <- complete(imX0,b)

	X0b <- X0
	trtb <- trt

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

	coef02b <- coef(lasso02b)
	p02 <- coef02b@Dim[1]
	beta.b <- numeric(p02)
	beta.b[(coef02b@i+1)] <- coef02b@x
	
	beta.m <- cbind(beta.m,beta.b)

	time1 <- Sys.time() - time0

	print(b)
	print(time1)
	
}

##

beta.r <- data.frame(round(apply(beta.m,1,mean),3))
rownames(beta.r) <- c("intercept",colnames(Xbb))
colnames(beta.r) <- "beta"

