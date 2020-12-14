aircraft = read.csv("aircraft.csv", header = T) %>% mutate( Period = factor( Period ))

lda.0 = lda( Period ~ Power + Span + Length + Weight + Speed + Range, data = aircraft )
lda.0
preds.0 = predict( lda.0 )$class

compare <- xtabs( ~preds.0+aircraft$Period)

lda.1 = lda( Period ~ Power + Span + Length + Weight + Speed + Range, prior = c(1,1,1)/3  , data = aircraft )
typeof(lda.1)
preds.1 = predict(lda.1)$class
xtabs( ~preds.1+aircraft$Period)

lda.1 = lda( Period ~ Power + Span + Length + Weight + Speed + Range, prior = c(1,1,1)/3, CV=T , data = aircraft )


#The argument CV=TRUE generates leave-one-out cross-validation predictions of the class. 
air.lda = lda(formula = Period ~ Power+Span+Length+Weight+Speed+Range,
              data = aircraft, CV = TRUE)

tab <- table(aircraft$Period, air.lda$class)
conCV1 <- rbind(tab[1, ], tab[2, ],tab[3, ])

dimnames(conCV1) <- list(Actual = c("1", "2","3"), Predicted = c("1","2","3"))
print(round(conCV1, 3))

qda.0 = qda( Period ~ Power + Span + Length + Weight + Speed + Range, data = aircraft )
preds_qda.0 = predict(qda.0)$class
xtabs( ~preds_qda.0+aircraft$Period)


qda.1 = qda( Period ~ Power + Span + Length + Weight + Speed + Range, prior = c(1,1,1)/3, data = aircraft )
preds_qda.1 = predict(qda.1)$class
xtabs( ~preds_qda.1+aircraft$Period)


####Q3####

W1 = cov( ((subset(aircraft[aircraft$Period==1,],select = -c(Period,Year)) )))
W2 = cov( ((subset(aircraft[aircraft$Period==2,],select = -c(Period,Year)) )) )
W3 = cov( ((subset(aircraft[aircraft$Period==3,],select = -c(Period,Year)) )) )
W = W1 + W2 + W3
mu1 = colMeans( ((subset(aircraft[aircraft$Period==1,],select = -c(Period,Year)) )) ) 
mu2 = colMeans( ((subset(aircraft[aircraft$Period==2,],select = -c(Period,Year)) )) ) 
mu3 = colMeans( ((subset(aircraft[aircraft$Period==3,],select = -c(Period,Year)) ))) 
mu = rbind( mu1, mu2, mu3 )

mu_tot= (mu1+mu2+mu3)/3

b1=(mu1 - mu_tot) %*% t((mu1 - mu_tot))
b2=(mu2 - mu_tot) %*% t((mu2 - mu_tot))
b3=(mu3 - mu_tot) %*% t((mu3 - mu_tot))

B = (b1+b2+b3)

#or (3-1)* cov(mu)

Q=solve(W,B)
Q
eigens = eigen(Q)
eta = eigens$vectors[,1]

XX = dplyr::select( aircraft, -Year, -Period ) 
XXproj = as.matrix( XX ) %*% as.matrix( eta )
muP1 = t( as.matrix( mu1 ) ) %*% as.matrix( eta ) 
tXXproj = t( XXproj )
m1 = as.data.frame( tXXproj ) - muP1

muP2 = t( as.matrix( mu2 ) ) %*% as.matrix( eta ) 
tXXproj = t( XXproj )
m2 = as.data.frame( tXXproj ) - muP2

muP3 = t( as.matrix( mu3 ) ) %*% as.matrix( eta ) 
tXXproj = t( XXproj )
m3 = as.data.frame( tXXproj ) - muP3

mm = rbind( abs( m1 ), abs( m2 ), abs( m3 ) )
classes = sapply( mm, which.min )
classified = data.frame( assigned = classes, aircraft ) 
xtabs( ~ assigned + Period , data = classified )


#########################
###Logisitic Regressiom

laircraft = read.csv("aircraft.csv", header = T) %>% mutate( Period1 = Period == 1)
str(aircraft)
xtabs( ~ Period + Period1, laircraft)

log10Span = log10(aircraft$Span)
log10Length = log10(aircraft$Length)
log10Weight = log10(aircraft$Weight)
log10Speed = log10(aircraft$Speed)
log10Range = log10(aircraft$Range)
log10Power = log10(aircraft$Power)

Period1 = aircraft$Period == 1

laircraft = data.frame(log10Span, log10Length, log10Weight, log10Speed, log10Range, log10Power,
                     aircraft$Year, aircraft$Period, Period1)

obs(laircraft)

aero.lr1 = glm( Period1 ~ log10Power +
                  log10Span + log10Length + log10Weight + log10Speed + log10Range,
                data = laircraft,
                family = "binomial")

preds.1 = predict( aero.lr1, type = "link" ) 
hist( preds.1 )



classhat.1 = preds.1 > 0.3
#classhat.1
xtabs( ~ classhat.1 + Period1)
wrong = xtabs( ~ classhat.1 + Period1)[2,1] + xtabs( ~ classhat.1 + Period1)[1,2]
accuracy = 100 - (wrong/709 * 100)
accuracy

list = list()
nrow(laircraft)
for( ii in nrow(laircraft) ) {
  aero.lr1 = glm( Period1 ~ log10Power +
                    log10Span + log10Length + log10Weight + log10Speed + log10Range,
                  data = laircraft[-ii],
                  family = "binomial")
  preds.1 = predict( aero.lr1, newdata = laircraft[ ii, ], type = "link" ) 
  classhat.1 = preds.1 > 0.7
  append(classhat.1, list)


}
