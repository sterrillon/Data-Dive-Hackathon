##### NFL #####

setwd("C:/Users/Shane/Desktop/DataDive")
library(readxl)
library(caTools)
library(scatterplot3d)

Fball <- read_excel("C:/Users/Shane/Desktop/DataDive/Fball.xlsx", sheet = "GameStats")

#View(Fball)



set.seed(69420)
split <- sample(2,nrow(Fball),prob = c(0.7,0.3),replace = T)
Fball_train <- Fball[split==1,]
Fball_test <- Fball[split==2,]

# Check
nrow(Fball_train)
nrow(Fball_test)

# Run the logistic regression on the training data
model <- glm(BinaryWinner ~ ScoreDiff1Half + Adv,
             data = Fball_train,family = "binomial")

summary(model)

# Predict our testing data
result <- predict(model,Fball_test,type="response")

# View our results
tab <- table(ActualValue=Fball_test$HorA,PredictedValue=result>0.5)

# Check the accuracy of our results
(ACC <- (tab[1,1]+tab[2,2])/sum(tab))

# store the indices of the data that we guessed incorrectly
incorrectVec <- which(abs(result-Fball_test$BinaryWinner)>0.5)

# errors contains our prediction values that were incorrect
incorrectVals <- result[incorrectVec]
hist(Fball_test$ScoreDiff1Half[incorrectVec],xlab="Point Differential at Half-Time \n (Negative: 'Away' Team, Positive: 'Home' Team)",
     main = 'Incorrect Guess Half-Time Score',col='light blue',breaks=10)

# distFromThresh contains the distance we were from making the correct guess (in form of a probability)
distFromThresh <- abs(incorrectVals-0.5)

mean(distFromThresh)
mean(abs(result-0.5))

# We can also see how far away in absolute score we were
incHomeScore <- Fball_test$`Home Points`[incorrectVec]
incAwayScore <- Fball_test$`Away Points`[incorrectVec]
absIncScoreDiff <- abs(incHomeScore-incAwayScore)
mean(absIncScoreDiff)

# incTime is the last time on the clock for the games that we guessed incorrectly
incTime <- Fball_test$`Min Time`[incorrectVec]

# We can count how many went to overtime (incTime<0), and how many games in total went to overtime
length(Fball_test$`Min Time`[which(Fball_test$`Min Time`<0)])
length(incTime[which(incTime<0)])


# Simulate random values so that we can plot a 3D contour
# Simulate x and values from our minimum to our maximums
x <- seq(min(Fball_test$Adv), max(Fball_test$Adv), by=.1)
y <- seq(min(Fball_test$ScoreDiff1Half), max(Fball_test$ScoreDiff1Half), by=0.69)
# exxp is the magnitude of the exponent in the logit equation
exxp <- matrix(0,nrow=length(y),ncol=length(x))
# Using our regression coefficients, simulate z values
b0 <- coef(model)[1]
b1 <- coef(model)[2]
b2 <- coef(model)[3]
for( i in 1:length(x)){
  for( j in 1:length(y)){
    exxp[j,i] <- b0 + b1*x[i]+b2*y[j]
  }
}
z <- 1/(1+exp(-exxp))

# Plot the countour of our logistic regression
pmat <- persp(y,x, z, col="lightblue",phi=20,theta = -35,axes=T,nticks=2,ticktype = "detailed",expand=0.5,
              xlab="\nHalf-Time Score Difference",ylab='\nScaled Team Advantage',
              main = 'Logistic Regression Prediction \n (z=1: "Home" Win, z=0:"Away" Win)')


depth3d <- function(x,y,z, pmat, minsize=0.2, maxsize=2) {
  
  # determine depth of each point from xyz and transformation matrix pmat
  tr <- as.matrix(cbind(x, y, z, 1)) %*% pmat
  tr <- tr[,3]/tr[,4]
  
  # scale depth to point sizes between minsize and maxsize
  psize <- ((tr-min(tr) ) * (maxsize-minsize)) / (max(tr)-min(tr)) + minsize
  return(psize)
}

psize = depth3d(Fball_test$ScoreDiff1Half,Fball_test$Adv,Fball_test$BinaryWinner,pmat,minsize=0.1, maxsize = 1)

# Create an object of all of the observed points
allPoints <- trans3d(Fball_test$ScoreDiff1Half,Fball_test$Adv,Fball_test$BinaryWinner, pmat=pmat)

# Create an object of all of the incorrect points
mypoints <- trans3d(Fball_test$ScoreDiff1Half[incorrectVec],Fball_test$Adv[incorrectVec],Fball_test$BinaryWinner[incorrectVec], pmat=pmat)

# Create an object of all of the predictions of the incorrect points
pred <- trans3d(Fball_test$ScoreDiff1Half[incorrectVec],Fball_test$Adv[incorrectVec],result[incorrectVec], pmat=pmat)

# Plot all of the points on top of our logistic regression contour
points(allPoints, pch=8, cex=psize, col=2)

# Draw lines from our incorrect points to the value on our contour
segments(mypoints$x, mypoints$y, pred$x, pred$y,lty=1,lw=2)

