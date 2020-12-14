aircraft = read.csv("aircraft.csv", header = T)
str(aircraft)
any(is.na(aircraft)) #returns false meaning no missing value
library(MASS)
library(ggplot2)
library(GGally)
library(tidyverse)
truehist(aircraft$Power)
logPower = log10(aircraft$Power)

summary(aircraft)
truehist(logPower)

ggplot(data = aircraft, aes(aircraft$Power)) + geom_density()

ggplot(data = aircraft, aes(logPower)) + geom_density()
logSpan = log10(aircraft$Span)
logLength = log10(aircraft$Length)
logWeight = log10(aircraft$Weight)
logSpeed = log10(aircraft$Speed)
logRange = log10(aircraft$Range)
logPower = log10(aircraft$Power)

newdata = data.frame(logSpan, logLength, logWeight, logSpeed, logRange, logPower,
                     aircraft$Year, factor(aircraft$Period))

#colour by factor
pc<-prcomp(newdata[1:6])
plot(pc$x[,1:2], col =  newdata$factor.aircraft.Period.)
str(newdata)                     

pairs(newdata)
ggpairs(newdata)


library( plot3D )
scatter3D (newdata$logWeight, newdata$logSpan, newdata$logLength,
           phi = 20, theta = 80,
           col = NULL, NAcol = "white", breaks = NULL,
           colkey = NULL, panel.first = NULL,
           clim = NULL, clab = NULL,
           bty = "b", CI = NULL, surf = NULL,
           add = FALSE, plot = TRUE)


xtabs(~factor.aircraft.Period.+ aircraft.Year, data = newdata) #frequency table

xtabs(~factor.aircraft.Period. , data = newdata)

str(newdata)

IQR(newdata$logSpan)
list1 = list("logspan" = logSpan,"loglength" = logLength,"logweight"=logWeight, "logspeed" = logSpeed, "logrange" = logRange, "Year" = newdata$aircraft.Year)
IQR = sapply(list1, IQR)
medians = sapply(list1, median)
means = sapply(list1, mean)
mediansnew = sapply(medians,"*",1.04)




for(i in 1:length(means))  {
  if(means[i]>mediansnew[i]){
    print(means[i])
  }
  
}

aircraft.per1 = data.frame(filter(newdata,newdata$factor.aircraft.Period. == "1"))
aircraft.per2 = data.frame(filter(newdata,newdata$factor.aircraft.Period == "2"))
aircraft.per3 = data.frame(filter(newdata,newdata$factor.aircraft.Period == "3"))
truehist(aircraft.per1$logPower,xlab = 'Period1LogPower',col = 'red')
truehist(aircraft.per2$logPower,xlab = 'Period2LogPower',col = 'red')
truehist(aircraft.per3$logPower,xlab = 'Period3LogPower',col = 'red')

ggpairs(aircraft.per1)

truehist(aircraft.per1$logLength,xlab = 'Period1LogLength',col = 'red')
truehist(aircraft.per2$logLength,xlab = 'Period2LogLength',col = 'red')
truehist(aircraft.per3$logLength,xlab = 'Period3LogLength',col = 'red')

ggplot( aircraft.per1, aes( logPower ) ) + geom_histogram()  + ggtitle("Period 1")
  
ggplot(data = newdata, aes(aircraft$Power)) + geom_density()


par(mfrow = c(1,1))
ggplot( aircraft.per1, aes( aircraft.per1$logRange, aircraft.per1$logWeight ))  + geom_density_2d( ) + geom_point(alpha = 0.6 ) + theme( legend.position = "bottom")
ggplot( aircraft.per2, aes( aircraft.per2$logRange, aircraft.per2$logWeight ))  + geom_density_2d( ) + geom_point(alpha = 0.6 ) + theme( legend.position = "bottom")
ggplot( aircraft.per3, aes( aircraft.per3$logRange, aircraft.per3$logWeight ))  + geom_density_2d( ) + geom_point(alpha = 0.6 ) + theme( legend.position = "bottom")

ggplot( aircraft.per1, aes( aircraft.per1$logSpeed, aircraft.per1$logLength ))  + geom_density_2d( ) + geom_point(alpha = 0.6 ) + theme( legend.position = "bottom")
ggplot( aircraft.per2, aes( aircraft.per2$logSpeed, aircraft.per2$logLength ))  + geom_density_2d( ) + geom_point(alpha = 0.6 ) + theme( legend.position = "bottom")
ggplot( aircraft.per3, aes( aircraft.per3$logSpeed, aircraft.per3$logLength ))  + geom_density_2d( ) + geom_point(alpha = 0.6 ) + theme( legend.position = "bottom")

ggplot( aircraft.per1, aes( aircraft.per1$logPower, aircraft.per1$logSpan ))  + geom_density_2d( ) + geom_point(alpha = 0.6 ) + theme( legend.position = "bottom")
ggplot( aircraft.per2, aes( aircraft.per2$logPower, aircraft.per2$logSpan ))  + geom_density_2d( ) + geom_point(alpha = 0.6 ) + theme( legend.position = "bottom")
ggplot( aircraft.per3, aes( aircraft.per3$logPower, aircraft.per3$logSpan ))  + geom_density_2d( ) + geom_point(alpha = 0.6 ) + theme( legend.position = "bottom")

ggplot( aircraft.per1, aes( aircraft.per1$logWeight, aircraft.per1$logSpeed ))  + geom_density_2d( ) + geom_point(alpha = 0.6 ) + theme( legend.position = "bottom")
ggplot( aircraft.per2, aes( aircraft.per2$logWeight, aircraft.per2$logSpeed ))  + geom_density_2d( ) + geom_point(alpha = 0.6 ) + theme( legend.position = "bottom")
ggplot( aircraft.per3, aes( aircraft.per3$logWeight, aircraft.per3$logSpeed ))  + geom_density_2d( ) + geom_point(alpha = 0.6 ) + theme( legend.position = "bottom")


ggplot( aircraft.per1, aes( aircraft.per1$logLength, aircraft.per1$logRange ))  + geom_density_2d( ) + geom_point(alpha = 0.6 ) + theme( legend.position = "bottom")
ggplot( aircraft.per2, aes( aircraft.per2$logLength, aircraft.per2$logRange ))  + geom_density_2d( ) + geom_point(alpha = 0.6 ) + theme( legend.position = "bottom")
ggplot( aircraft.per3, aes( aircraft.per3$logLength, aircraft.per3$logRange ))  + geom_density_2d( ) + geom_point(alpha = 0.6 ) + theme( legend.position = "bottom")

prc_period1 <- prcomp(aircraft.per1[1:6])
prc_period2 <- prcomp(aircraft.per2[1:6])
prc_period3 <- prcomp(aircraft.per3[1:6])
str(aircraft.per1)

prc_period1$sdev ^ 2
prc_period2$sdev ^ 2
prc_period3$sdev^2

par(mfrow=c(1,1))
        

screeplot(prc_period1 , type= ('lines'),main="Scree Plot Period 1")
screeplot(prc_period2, type = ('lines'),main = "Scree Plot Period 2")
screeplot(prc_period3, type = ('lines'), main = "Scree Plot Period 3")
plot(prc_period1,type = 'l')
plot(prc_period2,type = 'l')

plot(prc_period1$x[,1:2], main = 'Period 1', col = ("red"))
points(prc_period2$x[,1:2], main = "Period 2", col = ("blue"))
points(prc_period3$x[,1:2], main= 'Period 3', col = ("green"))

Nreps = 200 # number of replications
n = 100 # sample size
results = data.frame() # an object to store the results in
set.seed( 19501111 ) # make the process reproducible to assist with debugging if needed

for( ii in 1:Nreps ) {
  Samp = mvrnorm(n = n, mu = rep(0,6), Sigma = Sigma0 ) # generate sample
  pca.XX = prcomp( Samp )
  eigvals = pca.XX$sdev ^ 2 # prcomp gives us the standard deviations, so square them
  cond = eigvals[1]/eigvals[6]
  res.incr = data.frame( replication = ii,   # So we can find problems
                         eig1 = eigvals[1],  # We are constructing one row of a dataframe 
                         eig2 = eigvals[2],  # at a time so have to do this clunky bit
                         eig3 = eigvals[3], 
                         eig4 = eigvals[4], 
                         eig5 = eigvals[5], 
                         eig6 = eigvals[6],
                         cond.number = cond   # The condition number.
  )
  results = rbind( results, res.incr ) # add the new dataframe at the bottom of the old one
}
summary( results ) # see what we have got

# Some graphs to try to understand some relationships
ggplot( results, aes( eig1 ) ) + geom_density() 
ggplot( results, aes( eig6 ) ) + geom_density()
ggplot( results, aes( eig1, eig6 ) ) + geom_point()

ggplot( results, aes( eig1, eig6 ) ) + geom_point() + geom_density_2d()

# Density of the condition number
ggplot( results, aes( cond.number ) ) + geom_density() + 
  geom_vline( xintercept = cond.number0, colour = "grey" ) 





