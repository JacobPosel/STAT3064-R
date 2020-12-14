library(MASS)
library(tidyverse)
library(ggplot2)
library(GGally)
library(plot3D)
library(dplyr)
library(matrixcalc)

aircraft = read.csv("aircraft.csv", header = T)
str(aircraft)

logPower = log(aircraft$Power)
logSpan = log(aircraft$Span)
logLength = log(aircraft$Length)
logWeight = log(aircraft$Weight)
logSpeed = log(aircraft$Speed)
logRange = log(aircraft$Range)

ggplot(aircraft, aes(logWeight)) + geom_density()


aircraft = data.frame(aircraft$Year, aircraft$Period, logPower, logLength, logWeight, logSpan, logSpeed, logRange)
str(logaircraft)

aicraft.centred = transform(aircraft, logPower = logPower - mean(logPower), logLength = logLength - mean(logLength), logWeight = logWeight - mean(logWeight), logSpan = logSpan - mean(logSpan), logSpeed = logSpeed - mean(logSpeed), logRange = logRange - mean(logRange))

air.rearranged = dplyr::select (aicraft.centred, logSpan,
                                logLength, logWeight, logPower, logSpeed, logRange )

X1 = dplyr::select (aicraft.centred, logSpan,
                     logLength, logWeight )
X2 =  dplyr::select (aicraft.centred, logPower, logSpeed, logRange )

cor(X1, X2)


---------------------------------------
S1 = cov(X1)
S2 = cov(X2)
S12 = cov(X1, X2)
spectral1 = eigen(S1)
V1 = spectral1$vectors
lambda1 = spectral1$values
sqrtS1inv = V1 %*% diag(sqrt(1/lambda1)) %*% t(V1)

spectral2 = eigen(S2)
V2 = spectral2$vectors
lambda2 = spectral2$values
sqrtS2inv = V2 %*% diag(sqrt(1/lambda2)) %*% t(V2)

Chat = sqrtS2inv %*% S12 %*% sqrtS2inv
svd(Chat)$d

#a)
rm(Boston)
library(MASS) 
str(Boston)
Boston = data.frame(scale(Boston))

Boston$black = scale(Boston$black)
X1 = dplyr::select(Boston, crim, indus, nox, dis, rad, ptratio, black)
X2 = dplyr::select(Boston, rm, age, tax, medv)


S1 = cov(X1)
S2 = cov(X2)
S12 = cov(X1, X2)
spectral1 = eigen(S1)
V1 = spectral1$vectors
lambda1 = spectral1$values
sqrtS1inv = V1 %*% diag(sqrt(1/lambda1)) %*% t(V1)

spectral2 = eigen(S2)
V2 = spectral2$vectors
lambda2 = spectral2$values
sqrtS2inv = V2 %*% diag(sqrt(1/lambda2)) %*% t(V2)

Chat = sqrtS1inv %*% S12 %*% sqrtS2inv
Chat

CCA = cancor(X1, X2)
CCA
X1mat = as.matrix(X1, ncol = 7)
U = X1mat %*% CCA$xcoef
X2mat = as.matrix(X2, ncol = 4)
V = X2mat %*% CCA$ycoef
U[,1]

par(mfrow=c(2,2))
plot(U[,1], V[,1], xlab = 'U1', ylab = 'V1')
plot(U[,2], V[,2], xlab = 'U2', ylab = 'V2')
plot(U[,3], V[,3], xlab = 'U3', ylab = 'V3')
plot(U[,4], V[,4], xlab = 'U4', ylab = 'V4')

svd(Chat)$d

#b)
X1.pc = prcomp(X1,scale = TRUE)
#$rotation[,1:5]
X1.pc = X1.pc$x[,1:5]
X2
S1 = cov(X1.pc)
S2 = cov(X2)
S12 = cov(X1.pc, X2)
spectral1 = eigen(S1)
V1 = spectral1$vectors
lambda1 = spectral1$values
sqrtS1inv = V1 %*% diag(sqrt(1/lambda1)) %*% t(V1)

spectral2 = eigen(S2)
V2 = spectral2$vectors
lambda2 = spectral2$values
sqrtS2inv = V2 %*% diag(sqrt(1/lambda2)) %*% t(V2)

Chat = sqrtS1inv %*% S12 %*% sqrtS2inv
Chat

##Calculate U, V
CCA = cancor(X1.pc, X2)
CCA
X1.pcmat = as.matrix(X1.pc, ncol = 5)
X1.pcmat
U = X1.pcmat %*% CCA$xcoef

X2mat = as.matrix(X2, ncol = 4)
V = X2mat %*% CCA$ycoef
par(mfrow=c(2,2))
par(mfrow=c(2,2))
plot(U[,1], V[,1], xlab = 'U1', ylab = 'V1')
plot(U[,2], V[,2], xlab = 'U2', ylab = 'V2')
plot(U[,3], V[,3], xlab = 'U3', ylab = 'V3')
plot(U[,4], V[,4], xlab = 'U4', ylab = 'V4')
svd(Chat)$d

#c)
rm(Boston)
library(MASS) 


Boston$black = Boston$black - mean(Boston$black)
X1 = dplyr::select(Boston, crim, indus, nox, dis, rad, ptratio, black)
X2 = dplyr::select(Boston, rm, age, tax, medv, zn, lstat)

S1 = cov(X1)
S2 = cov(X2)
S12 = cov(X1, X2)
spectral1 = eigen(S1)
V1 = spectral1$vectors
lambda1 = spectral1$values
sqrtS1inv = V1 %*% diag(sqrt(1/lambda1)) %*% t(V1)

spectral2 = eigen(S2)
V2 = spectral2$vectors
lambda2 = spectral2$values
sqrtS2inv = V2 %*% diag(sqrt(1/lambda2)) %*% t(V2)

Chat = sqrtS1inv %*% S12 %*% sqrtS2inv
Chat
CCA = cancor(X1, X2) #xcenter = TRUE, ycenter = TRUE)
CCA
X1mat = as.matrix(X1, ncol = 7)
U = X1mat %*% CCA$xcoef
X2mat = as.matrix(X2, ncol = 6)
V = X2mat %*% CCA$ycoef

par(mfrow=c(1,1))
plot(U[,1], V[,1], xlab = 'U1', ylab = 'V1')
plot(U[,2], V[,2], xlab = 'U2', ylab = 'V2')
plot(U[,3], V[,3], xlab = 'U3', ylab = 'V3')
plot(U[,4], V[,4], xlab = 'U4', ylab = 'V4')
plot(U[,5], V[,5], xlab = 'U5', ylab = 'V5')
plot(U[,6], V[,6], xlab = 'U5', ylab = 'V6')

vv = svd(Chat)$d
vv
#6 upsilon variables
#could population quantity be non 0?
#look at k = 5 first
#H0:v1 =/= 0, ... v5 =/=0 and v6 = 0 vs H1: v6 =/= 0
#pval = 0.0189 < 0.02, therefore reject H0 and conclude all pairs could be correlated
#using 2% significance level

Tk = function( k, n, d1, d2, vv ){
  # vv is vector of singular values of Chat (returned by cancor in the component $cor)
  #  rr = length( vv )
  rank = length(vv)
  Tkout = - ( n - ( d1 + d2 + 3 )/2 ) * log( prod ( 1 - vv[(k+1):rank ]^2 ) ) # compare with chisq on (d1 - k ) * (d2 - k ) dof
  dof = ( d1 - k ) * ( d2 - k )
  pval = pchisq( Tkout, df = dof, lower.tail = FALSE )
  list( Tkout = Tkout, pval = pval ) }

Tk( k = 3, n = 506, d1 = 7, d2 = 6, vv = CCA$cor )

Tk( k = 5, n = 506, d1 = 7, d2 = 6, vv = CCA$cor )

CCA$cor

