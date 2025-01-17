library("mmrm")
library("emmeans")

R1 <- R2 <- SR1 <- SR2 <- NULL

#########################################################################

edat <- read.csv(file="editeddata_all.csv")
load(file="presurv_edited.RData")

##

edat <- edat[is.na(edat$PHQ9)==FALSE,]

##

x1 <- factor(edat$x1)
x1 <- relevel(x1,ref="C11")
edat$x1 <- x1

answerIndex <- factor(edat$answerIndex)
answerIndex <- relevel(answerIndex,ref="26")
edat$answerIndex <- answerIndex

username <- factor(edat$username)
edat$username <- username

sex <- factor(edat$sex)
edat$sex <- sex

##

edat6 <- edat[edat$answerIndex==26,]

aov6 <- lm(PHQ9Point ~ x1,data=edat6)
sigma <- summary(aov6)$sigma		# common SD

##

presurv <- presurv[presurv$x0!="C12",]
presurv <- presurv[presurv$x0!="C10",]

aov4 <- lm(PHQ9Point ~ factor(x0),data=presurv)
sigma0 <- summary(aov4)$sigma		# common baseline SD

##

mmrm2 <- mmrm(
	formula = PHQ9 ~ x1 + PHQ9_0 + age + sex + working + answerIndex + x1*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm2)

##

coef2 <- summary(mmrm2)$coefficients

Est <- round(coef2[2:10,1],2)
CL <- round(coef2[2:10,1] - qt(0.975,df=coef2[2:10,3])*coef2[2:10,2],2)
CU <- round(coef2[2:10,1] + qt(0.975,df=coef2[2:10,3])*coef2[2:10,2],2)
P <- round(coef2[2:10,5],3)

res2 <- data.frame(Est,CL,CU,P)

Est <- round(coef2[2:10,1]/sigma,2)
CL <- round((coef2[2:10,1] - qt(0.975,df=coef2[2:10,3])*coef2[2:10,2])/sigma,2)
CU <- round((coef2[2:10,1] + qt(0.975,df=coef2[2:10,3])*coef2[2:10,2])/sigma,2)
P <- round(coef2[2:10,5],3)

sres2 <- data.frame(Est,CL,CU,P)

##

M1 <- E1 <- NULL

emm2 <- summary(emmeans(mmrm2, ~ x1 | answerIndex, infer=TRUE))

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[2,]
M1 <- rbind(M1, round(lsm,2))
E1 <- rbind(E1, round( lsm/sigma0, 2) )

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[3,]
M1 <- rbind(M1, round(lsm,2))
E1 <- rbind(E1, round( lsm/sigma0, 2) )

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[4,]
M1 <- rbind(M1, round(lsm,2))
E1 <- rbind(E1, round( lsm/sigma0, 2) )

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[5,]
M1 <- rbind(M1, round(lsm,2))
E1 <- rbind(E1, round( lsm/sigma0, 2) )

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[6,]
M1 <- rbind(M1, round(lsm,2))
E1 <- rbind(E1, round( lsm/sigma0, 2) )

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[7,]
M1 <- rbind(M1, round(lsm,2))
E1 <- rbind(E1, round( lsm/sigma0, 2) )

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[8,]
M1 <- rbind(M1, round(lsm,2))
E1 <- rbind(E1, round( lsm/sigma0, 2) )

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[9,]
M1 <- rbind(M1, round(lsm,2))
E1 <- rbind(E1, round( lsm/sigma0, 2) )

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[10,]
M1 <- rbind(M1, round(lsm,2))
E1 <- rbind(E1, round( lsm/sigma0, 2) )

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[1,]
M1 <- rbind(M1, round(lsm,2))
E1 <- rbind(E1, round( lsm/sigma0, 2) )

