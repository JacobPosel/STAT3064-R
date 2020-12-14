library(expm)
library(psych)

#PCFA
BCV2 = read.csv("BCV2.csv", header = T)
str(BCV2)
BCV2 = BCV2[2:29]
str(BCV2)
BCV2 = scale(BCV2)
summary(BCV2)
pc_BCV2 = prcomp(BCV2, scale = TRUE)

cov_B = cov(BCV2)
eig = eigen(cov_B)
gamma = eig$vectors
lambda = diag( eig$values^(1/2) ) #first k eigenvalues??

#diag = diag( eig$values[,1:5]^(1/2) )

Ahat = gamma %*% lambda #(matrix size dxk)

Ahat = Ahat[,1:2]

biplot(pc_BCV2$x, Ahat, col = c("white", "blue"))

#PRINCIPAL AXIS FACTORING 2 factor model
sigma_hat_sq = sum(eig$values[3:28])/(28-2) #eigen j>k
Omega = diag(rep(sigma_hat_sq),28)
S_A = cov_B - Omega

eig = eigen(S_A)
gamma = eig$vectors[,1:2]
lambda = diag( eig$values[1:2]^(1/2) ) #first k eigenvalues??

#diag = diag( eig$values[,1:5]^(1/2) )

Ahat = gamma %*% lambda #(matrix size dxk)

#Ahat = Ahat[,1:2]

biplot(pc_BCV2$x, Ahat, col = c("white", "blue"))

ML_norotate <- factanal(BCV2, 2, rotation = 'none', scores = 'regression')

biplot(ML_norotate$scores, 
       ML_norotate$loadings, col = c("white", "blue") )


ML_varimax <- factanal(BCV2, 2, rotation = 'varimax', scores = 'regression')
biplot(ML_varimax$scores, 
       ML_varimax$loadings, col = c("white", "blue") )

#Hypothesis Test calculating in factanal
 
