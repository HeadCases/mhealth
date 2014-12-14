
# EO_Outcome

# TODO - use to benchmark against current practise

# Box transform - play with link function
# TODO - get better results from SVM
# TODO - get better results from tree
# TODO - Do test/train for ROC
# TODO - get SVD from LDA
# TODO - Kernel ridge regression

c <- crash <- read.csv(file="/home/alex/mhealth/freebird-crash-injury-emergency-bank/CRASH_dataset head Injury.csv",head=TRUE,sep=",")



# Primary outcomes - death within 2 weeks
# 1 = Death in hospital
# 2 = Transferred to other acute care hospital
# 3 = Discharged to rehabilitation centre or nursing home
# 4 = Discharged home
# 5 = Still in this hospital now

c$death <- 1*(1==c$EO_Outcome)
c[ is.na(c) ] <- 0

# library(MASS)
# r.lda <- lda( x=c[,!(names(c) == "death")], grouping=c$death )
# tmp <- r.lda$scaling
# as.matrix(tmp[order(abs(tmp)),]) 

## REMOVE TRAND and DRAND - too many categories!
## "X","DRAND","TRAND",  // Date and time of randomisation
## "IS_GCS_CURRENT","SEX" // Not useful
oe <- c[,c("death","AGE","MINS_SINCE_INJURY","GCS_EYE","GCS_MOTOR","GCS_VERBAL","PUPIL_REACT_LEFT","PUPIL_REACT_RIGHT")]
r.glm2 <- glm(death~.,data=oe,family=binomial())

# Assess strength of prediction:
plot(r.glm2)

# Several different kinds of residuals for glm (not as simple as y minus y-hat). In R, the default is deviance.
# Deviance - difference in log probability of y given theta under saturated model.
# http://pj.freefaculty.org/guides/stat/Regression-GLM/GLM2-SigTests/GLM-2-guide.pdf

# Now, how can we play with the threshold?
# mean(r.glm2$fitted.values[oe$death==0])
# [1] 0.1426009
# mean(r.glm2$fitted.values[oe$death==1])
# [1] 0.3736623

# From this, we can generate ROC curve using different thresholds:
p <- predict(r.glm2,newdata=oe,type="response")
threshold <- seq(0,1,0.001)

roc <- data.frame(t=threshold,truepos=rep(0,1001),falsepos=rep(0,1001))
for(i in 1:1001){
  t <- threshold[i]
  roc$t[i] <- t
  # roc$truepos[i] <- mean(oe$death*(p > t))
  # roc$falsepos[i] <- mean((1*!oe$death)*(p > t))
  roc$truepos[i] <- mean((p > t)[1==oe$death])
  roc$falsepos[i] <- mean((p > t)[0==oe$death])
}
plot(x=roc$falsepos,y=roc$truepos,type='l')

# Calc AUC
sum(c(0,diff(roc$falsepos))*roc$truepos) # 0.8324163

r.glm3 <- glm(death~AGE,data=oe,family=binomial())
p <- predict(r.glm3,newdata=oe,type="response")

# Try a GAM - might be able to do something smarter with age and time since injury
library(mgcv)


r.mgcv <- gam(death ~ GCS_EYE + GCS_MOTOR + GCS_VERBAL + PUPIL_REACT_LEFT + PUPIL_REACT_RIGHT + s(AGE) + s(MINS_SINCE_INJURY),
              data=oe,
              family=binomial()
              )
# No improvement in AUC

library(rpart)

r.rpart <- rpart( death ~ GCS_EYE + GCS_MOTOR + GCS_VERBAL + PUPIL_REACT_LEFT + PUPIL_REACT_RIGHT + AGE + MINS_SINCE_INJURY,
                  data=oe,
                  method="class",
                  control = rpart.control(cp = 0.01)
                  )

# Even with this very fine grained tree, its not as good at glm
p <- predict(r.rpart,newdata=oe)[,"1"]

# CART with rpart:
# http://www.statmethods.net/advstats/cart.html

# plot tree 
plot(r.rpart, uniform=TRUE, 
     main="Classification Tree")
text(r.rpart, use.n=TRUE, all=TRUE, cex=.8)

# Try and SVM, kernel based regressions, something non-parametric
library(e1071)
# TODO - choose gamma, epsilon through CV of training data
# TODO - validate all models against TEST DATASET!
# TODO - why is this SVM so shit?
r.svm <- svm(death~.,data=oe,scale=TRUE)
p <- predict(r.svm, oe)

# knn in class doesn't seem to offer return of 
knn <- function(newpt){
  
  scaled <- scale(rbind(newpt,oe)) 
  dist <- abs(scaled %*% scaled[1,])
  oe[order(dist)[1:5],]
}

library(shiny)


knn <- function(newpt){
  
  scaled <- scale(rbind(newpt,oe[,2:8])) 
  dist <- t(apply(X=scaled,MARGIN=1,FUN=function(x){x - scaled[1,]}))
  dist <- apply(dist,MARGIN=1,FUN=function(x){sqrt(sum(x^2))})
  oe[order(dist)[2:6],]
}

# As percentage
sensitivity <- function(nd,mins){
  p <- predict(r.mgcv,newdata=nd,type="response")[[1]]
  nd$MINS_SINCE_INJURY <- nd$MINS_SINCE_INJURY + mins
  p1 <- predict(r.mgcv,newdata=nd,type="response")[[1]]
  floor(10000*(p1 - p))/100
}

# Expected life years lost by delaying treatment
# (Life expectancy UK is 83)
qaly <- function(oe,sensit){
  max(1,83-oe$AGE)*oe$delta/100
}

# 1 - emprically calculate the sensitivity of each of 10 patients to additional delays
# 2 - produce a 'see in this order' - operational research / auto triage



runApp("mhealth")


