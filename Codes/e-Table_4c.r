load(file="fdata_all_R2_Mi.RData")

time0 <- Sys.time()

N <- length(y)

p0 <- dim(X0)[2]

##

Q11 <- Q12 <- Q13 <- Q14 <- Q15 <- list()
B1 <- B2 <- B3 <- B4 <- B5 <- 0

for(i in 1:2000){
	I <- 10000 + i
	filenam <- paste0("predsc_mice_",I,"_boot.csv")
	if(file.exists(filenam)==TRUE){
		Q1 <- read.csv(file=filenam)[,-1]
		Q11[[i]] <- Q1
		B1 <- B1 + 1
	}
}

for(i in 1:2000){
	I <- 20000 + i
	filenam <- paste0("predsc_mice_",I,"_boot.csv")
	if(file.exists(filenam)==TRUE){
		Q1 <- read.csv(file=filenam)[,-1]
		Q12[[i]] <- Q1
		B2 <- B2 + 1
	}
}

for(i in 1:2000){
	I <- 30000 + i
	filenam <- paste0("predsc_mice_",I,"_boot.csv")
	if(file.exists(filenam)==TRUE){
		Q1 <- read.csv(file=filenam)[,-1]
		Q13[[i]] <- Q1
		B3 <- B3 + 1
	}
}

###

B <- B1+B2+B3+B4+B5

P <- SUCRA <- SBA <- SBACR <- SBAPS <- matrix(rep(NA,times=N*10),N)

for(i in 1:N){

	ei <- NULL
	
	for(b in 1:B1){
		e1 <- Q11[[b]][i,]
		ei <- rbind(ei,e1)
	}

	for(b in 1:B2){
		e1 <- Q12[[b]][i,]
		ei <- rbind(ei,e1)
	}

	for(b in 1:B3){
		e1 <- Q13[[b]][i,]
		ei <- rbind(ei,e1)
	}
	
	pi <- numeric(10)
	ri <- numeric(10)
	si <- numeric(10)
	cri <- numeric(10)
	psi <- numeric(10)
	
	for(k in 1:B){
	
		ek <- as.numeric(ei[k,])
		pi[which.min(ek)] <- pi[which.min(ek)] + 1
		ri <- ri + (10 - rank(ek))/9
		si <- si + as.numeric(ek < ek[9])
		cri <- cri + as.numeric(ek < ek[5])
		psi <- psi + as.numeric(ek < ek[6])
		
	}
	
	P[i,] <- pi/B	
	SUCRA[i,] <- ri/B
	SBA[i,] <- si/B	
	SBACR[i,] <- cri/B	
	SBAPS[i,] <- psi/B	
	
	print(i)

}

##

trt <- numeric(N)
trt[T[,1]==1] <- 1
trt[T[,2]==1] <- 2
trt[T[,3]==1] <- 3
trt[T[,4]==1] <- 4
trt[T[,5]==1] <- 5
trt[T[,6]==1] <- 6
trt[T[,7]==1] <- 7
trt[T[,8]==1] <- 8
trt[T[,9]==1] <- 9

wm <- mm <- ym <- rep(NA,times=N)

for(i in 1:N){

	pi <- P[i,]
	
	wm[i] <- which.max(pi)
	mm[i] <- max(pi)

}

table(wm,trt)

sum(wm==trt)
mean(wm==trt)

##

load(file="lasso_mice_with_w1w2_NMID_Mi.RData")

M1 <- data.frame(id,y,predr,trt,wm,P)

colnames(M1) <- c("id","y","predr","trt","wm","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10")



