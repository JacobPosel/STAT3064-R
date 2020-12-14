#Q3#
library('bloom')
coln = c("Sex", "Length", "Diameter", "Height", "Whole_weight",
         "Shucked_weight", "Viscera_weight", "Shell_weight",  "Rings") 

abalone = read_csv( file = "abalone.csv",col_names = coln ) 
summary( abalone )
summary( factor( abalone$Sex ) )

#a)
big.lm = lm(data=abalone, Rings ~ Length + Diameter + Height + Whole_weight + Shucked_weight + Viscera_weight + Shell_weight)
summary( big.lm )

#Forward Selection
fm.fwd <- fm.null <- lm(Rings ~ 1, abalone)

add1(fm.fwd, big.lm, test="F")
summary( lm( fm.fwd ) )

fm.fwd <- update(fm.fwd, . ~ . + Shell_weight)  #biggest F value
add1(fm.fwd, big.lm, test="F")

fm.fwd <- update(fm.fwd, . ~ . + Viscera_weight) 
add1(fm.fwd, big.lm, test="F")

fm.fwd <- update(fm.fwd, . ~ . + Shucked_weight) 
add1(fm.fwd, big.lm, test="F")

fm.fwd <- update(fm.fwd, . ~ . + Whole_weight) 
add1(fm.fwd, big.lm, test="F")

fm.fwd <- update(fm.fwd, . ~ . + Height) 
add1(fm.fwd, big.lm, test="F")

fm.fwd <- update(fm.fwd, . ~ . + Diameter) 
summary(fm.fwd)
add1(fm.fwd, big.lm, test="F")

fm.fwd <- update(fm.fwd, . ~ . + Diameter) 
#Alternate Forward Selection Process using glance function

glancerows = data.frame()
fm.fwd <- fm.null <- lm(Rings ~ 1, abalone)
summary( lm( fm.fwd ) )
row1 = data.frame(modelno = 0, sigma = glance( lm( fm.fwd ) )$sigma ) #$sigma is residual standard error
glancerows = rbind( glancerows, row1 )
add1(fm.fwd, big.lm, test="F")
fm.fwd <- update(fm.fwd, . ~ . + Shell_weight)
row1 = data.frame(modelno = 1, sigma = glance( lm( fm.fwd ) )$sigma ) 
glancerows = rbind( glancerows, row1 )
add1(fm.fwd, big.lm, test="F")
fm.fwd <- update(fm.fwd, . ~ . + Viscera_weight)
row1 = data.frame(modelno = 2, sigma = glance( lm( fm.fwd ) )$sigma ) 
glancerows = rbind( glancerows, row1 )
add1(fm.fwd, big.lm, test="F")
fm.fwd <- update(fm.fwd, . ~ . + Shucked_weight)
row1 = data.frame(modelno = 3, sigma = glance( lm( fm.fwd ) )$sigma ) 
glancerows = rbind( glancerows, row1 )
add1(fm.fwd, big.lm, test="F")
fm.fwd <- update(fm.fwd, . ~ . + Whole_weight)
row1 = data.frame(modelno = 4, sigma = glance( lm( fm.fwd ) )$sigma ) 
glancerows = rbind( glancerows, row1 )
add1(fm.fwd, big.lm, test="F")
fm.fwd <- update(fm.fwd, . ~ . + Height)
row1 = data.frame(modelno = 5, sigma = glance( lm( fm.fwd ) )$sigma ) 
glancerows = rbind( glancerows, row1 )
add1(fm.fwd, big.lm, test="F")
fm.fwd <- update(fm.fwd, . ~ . + Diameter)
row1 = data.frame(modelno = 6, sigma = glance( lm( fm.fwd ) )$sigma ) 
glancerows = rbind( glancerows, row1 )

plot(glancerows, xlab = "predictors", ylab= "standard deviation")
#b)

abaolne.pc = prcomp(subset(abalone, select = - Sex))

library(Compositional)
library(pls)
pcr1 = pcr(Rings~., data = subset(abalone,select = -c(Sex)), scale = F, validation = "CV")
summary(pcr1)

str(abalone[2:8])
abalone = data.frame(scale(abalone[,2:9]))
abalone_PC = prcomp(subset(abalone, select = - c(Rings))) #for scaled
abalone_PC = prcomp(subset(abalone, select = - c(Rings, Sex)))
abalone_PC
big.lm_PC = lm(data=abalone, Rings ~ abalone_PC$x[,1] + abalone_PC$x[,2] + abalone_PC$x[,3] + abalone_PC$x[,4] + abalone_PC$x[,5] + abalone_PC$x[,6] + abalone_PC$x[,7])
summary(big.lm_PC)

fm.fwd.pc <- fm.null.pc <- lm(Rings ~ 1, abalone)
add1(fm.fwd.pc, big.lm_PC, test="F")
fm.fwd.pc <- update(fm.fwd.pc, . ~ . + abalone_PC$x[, 1])

add1(fm.fwd.pc, big.lm_PC, test="F")
fm.fwd.pc <- update(fm.fwd.pc, . ~ . + abalone_PC$x[, 2])

add1(fm.fwd.pc, big.lm_PC, test="F")
fm.fwd.pc <- update(fm.fwd.pc, . ~ . + abalone_PC$x[, 3])

add1(fm.fwd.pc, big.lm_PC, test="F")
fm.fwd.pc <- update(fm.fwd.pc, . ~ . + abalone_PC$x[, 3])

add1(fm.fwd.pc, big.lm_PC, test="F")
fm.fwd.pc <- update(fm.fwd.pc, . ~ . + abalone_PC$x[, 4])

add1(fm.fwd.pc, big.lm_PC, test="F")
fm.fwd.pc <- update(fm.fwd.pc, . ~ . + abalone_PC$x[, 7])

add1(fm.fwd.pc, big.lm_PC, test="F")
fm.fwd.pc <- update(fm.fwd.pc, . ~ . + abalone_PC$x[, 5])

summary(fm.fwd.pc)
