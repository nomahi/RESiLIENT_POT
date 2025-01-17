load(file="fdata_all_R2b.RData")

time0 <- Sys.time()

N <- length(y)

p0 <- dim(X0)[2]

z0 <- X0[,"phq9base"]

##

load("Model1_both_mice_nonMID.RData")
Q1 <- M1[z0<=4,]

load("Model3_both_mice_nonMID.RData")
Q2 <- M1[z0>=5,]

n1 <- dim(Q1)[1]
n2 <- dim(Q2)[1]

##

best.t <- numeric(n1)

for(i in 1:n1){

	Pi <- Q1[i,9:10]
	best.t[i] <- which.max(Pi)
	
}

table(best.t)

y1 <- Q1$y[(best.t==1)&(Q1$trt==4)]
y2 <- Q1$y[(best.t==2)&(Q1$trt==5)]
Y1 <- c(y1,y2)

m1 <- mean(Q1$y[Q1$trt==0],na.rm=TRUE)
m2 <- mean(Q1$y[Q1$trt==6],na.rm=TRUE)
m3 <- mean(c(y1,y2),na.rm=TRUE)

(m3-m2)/(m2-m1)

##

best.t <- numeric(n2)

for(i in 1:n2){

	Pi <- Q2[i,11:13]
	best.t[i] <- which.max(Pi)
	
}

table(best.t)

y1 <- Q2$y[(best.t==1)&(Q2$trt==6)]
y2 <- Q2$y[(best.t==2)&(Q2$trt==7)]
y3 <- Q2$y[(best.t==3)&(Q2$trt==8)]

Y2 <- c(y1,y2,y3)

m1 <- mean(Q2$y[Q2$trt==0],na.rm=TRUE)
m2 <- mean(Q2$y[Q2$trt==6],na.rm=TRUE)
m3 <- mean(c(y1,y2,y3),na.rm=TRUE)

(m3-m2)/(m2-m1)

##

m1 <- mean(c(Q1$y[Q1$trt==0],Q2$y[Q2$trt==0]),na.rm=TRUE)
m2 <- mean(c(Q1$y[Q1$trt==6],Q2$y[Q2$trt==6]),na.rm=TRUE)
m3 <- mean(c(Y1,Y2),na.rm=TRUE)

(m3-m2)/(m2-m1)

##

library(rqlm)

sigma <- 3.853508

ya <- c(Y1,Y2)
yb <- c(Q1$y[Q1$trt==6],Q2$y[Q2$trt==6])
yc <- c(Q1$y[Q1$trt==0],Q2$y[Q2$trt==0])

na <- length(ya)
nb <- length(yb)
nc <- length(yc)

xa <- rep(1,times=na)
xb <- rep(1,times=nb)
xc <- rep(0,times=nc)

##

yf <- c(ya,yc)
xf <- c(xa,xc)
df <- data.frame(yf,xf)

rm1 <- rqlm(yf~xf, data=df, family=gaussian)
rm1
rm1[2,c(1,3,4)]/sigma

##

yf <- c(yb,yc)
xf <- c(xb,xc)
df <- data.frame(yf,xf)

rm1 <- rqlm(yf~xf, data=df, family=gaussian)
rm1
rm1[2,c(1,3,4)]/sigma
