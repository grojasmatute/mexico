setwd("/Users/gustavorojasmatute/Documents/AmericanU/AU/Dissertation")
df.foremx <- read.csv("ForecastMx.csv") ## read microdata 

## 2007 
gdp07_id2 <- subset(newdf,inft == "varpib_a07" & variable =="varpibt" &  analyst == 2 )
gdp07_id2


gy07 <- data.frame(gdp07_id2[,1])

##df for only inflation of the current period
gdpdf07 <- NULL # empty subset 
gdppon07 <- matrix() 
gdpidpoint07 <- add.col(gy07, gdppon07)
gdpidpoint07

## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf07 <- subset(newdf,inft == "varpib_a07" & variable =="varpibt" & analyst == t )
gdpidpoint07 <- add.col(gdpidpoint07,gdpdf07[,5]) } 
gdpidpoint07

gdpidpoint07_0 <- gdpidpoint07
gdpidpoint07_0[is.na(gdpidpoint07_0)] <- 0
gdpidpoint07_0

## updates ##
gdpupdate07_0 <- matrix(0, nrow = nrow(gdpidpoint07_0 ), ncol = ncol(gdpidpoint07_0 ) )
for (j in 2:ncol(gdpidpoint07_0 )) {for (i in 2:nrow(gdpidpoint07_0 )) { gdpupdate07_0[i,j] <- ifelse (gdpidpoint07_0[i,j] == gdpidpoint07_0[i-1,j],0,1) & (gdpidpoint07_0[i,j]>0)  }}
gdpupdate07_0

## create a new column with proportion of updates 
gdpupdaters07_0 <- numeric(nrow(gdpidpoint07_0))
for(t in 2:nrow(gdpidpoint07_0 )) {gdpupdaters07_0[t] <- sum(gdpupdate07_0[t,2:ncol(gdpupdate07_0)])}
gdpupdaters07_0 ## total of updaters

## participants
gdptotal07 <- numeric(11)
gdpparti07<- matrix(0, nrow = nrow(gdpidpoint07_0 ), ncol = ncol(gdpidpoint07_0 ) )
for (j in 2:ncol(gdpidpoint07_0)) {for (i in 1:nrow(gdpidpoint07_0 )) { gdpparti07[i,j] <- ifelse (gdpidpoint07_0[i,j] > 0,1,0) }}
for(t in 1:nrow(gdpidpoint07_0 )) {gdptotal07[t] <- sum(gdpparti07[t,2:ncol(gdpparti07)])}
gdptotal07

gdpupdaters07_0 <- gdpupdaters07_0[2:11]/gdptotal07[2:11]
gdpupdaters07_0

## 2008 
gdp08_id2 <- subset(newdf,inft == "varpib_a08" & variable =="varpibt" &  analyst == 4 )
gdp08_id2


gy08 <- data.frame(gdp08_id2[,1])

##df for only inflation of the current period
gdpdf08 <- NULL # empty subset 
gdppon08 <- matrix() 
gdpidpoint08 <- add.col(gy08, gdppon08)
gdpidpoint08

## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf08 <- subset(newdf,inft == "varpib_a08" & variable =="varpibt" & analyst == t )
gdpidpoint08 <- add.col(gdpidpoint08,gdpdf08[,5]) } 
gdpidpoint08

gdpidpoint08_0 <- gdpidpoint08
gdpidpoint08_0[is.na(gdpidpoint08_0)] <- 0
gdpidpoint08_0

## updates ##
gdpupdate08_0 <- matrix(0, nrow = nrow(gdpidpoint08_0 ), ncol = ncol(gdpidpoint08_0 ) )
for (j in 2:ncol(gdpidpoint08_0 )) {for (i in 2:nrow(gdpidpoint08_0 )) { gdpupdate08_0[i,j] <- ifelse (gdpidpoint08_0[i,j] == gdpidpoint08_0[i-1,j],0,1) & (gdpidpoint08_0[i,j]!=0)  }}
gdpupdate08_0

## create a new column with proportion of updates 
gdpupdaters08_0 <- numeric(nrow(gdpidpoint08_0))
for(t in 2:nrow(gdpidpoint08_0 )) {gdpupdaters08_0[t] <- sum(gdpupdate08_0[t,2:ncol(gdpupdate08_0)])}
gdpupdaters08_0 ## total of updaters

## participants
gdptotal08 <- numeric(11)
gdpparti08<- matrix(0, nrow = nrow(gdpidpoint08_0 ), ncol = ncol(gdpidpoint08_0 ) )
for (j in 2:ncol(gdpidpoint08_0)) {for (i in 1:nrow(gdpidpoint08_0 )) { gdpparti08[i,j] <- ifelse (gdpidpoint08_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint08_0 )) {gdptotal08[t] <- sum(gdpparti08[t,2:ncol(gdpparti08)])}
gdptotal08

gdpupdaters08_0 <- gdpupdaters08_0[2:10]/gdptotal08[2:10]
gdpupdaters08_0 


## 2009 
gdp09_id2 <- subset(newdf,inft == "varpib_a09" & variable =="varpibt" &  analyst == 4 )
gdp09_id2


gy09 <- data.frame(gdp09_id2[,1])

##df for only inflation of the current period
gdpdf09 <- NULL # empty subset 
gdppon09 <- matrix() 
gdpidpoint09 <- add.col(gy09, gdppon09)
gdpidpoint09

gdpdf09
## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf09 <- subset(newdf,inft == "varpib_a09" & variable =="varpibt" & analyst == t )
gdpidpoint09 <- add.col(gdpidpoint09,gdpdf09[,5]) } 
gdpidpoint09


gdpidpoint09_0 <- gdpidpoint09
gdpidpoint09_0[is.na(gdpidpoint09_0)] <- 0
gdpidpoint09_0

## updates ##


gdpupdate09_0 <- matrix(0, nrow = nrow(gdpidpoint09_0 ), ncol = ncol(gdpidpoint09_0 ) )
for (j in 2:ncol(gdpidpoint09_0 )) {for (i in 2:nrow(gdpidpoint09_0 )) { gdpupdate09_0[i,j] <- ifelse (gdpidpoint09_0[i,j] == gdpidpoint09_0[i-1,j],0,1) & (gdpidpoint09_0[i,j]!=0)  }}
gdpupdate09_0


## create a new column with proportion of updates 
gdpupdaters09_0 <- numeric(nrow(gdpidpoint09_0))
for(t in 2:nrow(gdpidpoint09_0 )) {gdpupdaters09_0[t] <- sum(gdpupdate09_0[t,2:ncol(gdpupdate09_0)])}
gdpupdaters09_0 ## total of updaters

## participants
gdptotal09 <- numeric(12)
gdpparti09 <- matrix(0, nrow = nrow(gdpidpoint09_0 ), ncol = ncol(gdpidpoint09_0 ) )
for (j in 2:ncol(gdpidpoint09_0)) {for (i in 1:nrow(gdpidpoint09_0 )) { gdpparti09[i,j] <- ifelse (gdpidpoint09_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint09_0 )) {gdptotal09[t] <- sum(gdpparti09[t,2:ncol(gdpparti09)])}
gdptotal09



gdpupdaters09_0 <- gdpupdaters09_0[2:12]/gdptotal09[2:12]
gdpupdaters09_0 


## 2010
gdp10_id2 <- subset(newdf,inft == "varpib_a10" & variable =="varpibt" &  analyst == 4 )
gdp10_id2


gy10 <- data.frame(gdp10_id2[,1])

##df for only inflation of the current period
gdpdf10 <- NULL # empty subset 
gdppon10 <- matrix() 
gdpidpoint10 <- add.col(gy10, gdppon10)
gdpidpoint10

gdpdf10
## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf10 <- subset(newdf,inft == "varpib_a10" & variable =="varpibt" & analyst == t )
gdpidpoint10 <- add.col(gdpidpoint10,gdpdf10[,5]) } 
gdpidpoint10


gdpidpoint10_0 <- gdpidpoint10
gdpidpoint10_0[is.na(gdpidpoint10_0)] <- 0
gdpidpoint10_0

## updates ##


gdpupdate10_0 <- matrix(0, nrow = nrow(gdpidpoint10_0 ), ncol = ncol(gdpidpoint10_0 ) )
for (j in 2:ncol(gdpidpoint10_0 )) {for (i in 2:nrow(gdpidpoint10_0 )) { gdpupdate10_0[i,j] <- ifelse (gdpidpoint10_0[i,j] == gdpidpoint10_0[i-1,j],0,1) & (gdpidpoint10_0[i,j]!=0)  }}
gdpupdate10_0


## 2011
gdp11_id2 <- subset(newdf,inft == "varpib_a11" & variable =="varpibt" &  analyst == 2 )
gdp11_id2


gy11 <- data.frame(gdp11_id2[,1])

##df for only inflation of the current period
gdpdf11 <- NULL # empty subset 
gdppon11 <- matrix() 
gdpidpoint11 <- add.col(gy11, gdppon11)
gdpidpoint11


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf11 <- subset(newdf,inft == "varpib_a11" & variable =="varpibt" & analyst == t )
gdpidpoint11 <- add.col(gdpidpoint11,gdpdf11[,5]) } 
gdpidpoint11


gdpidpoint11_0 <- gdpidpoint11 
gdpidpoint11_0[is.na(gdpidpoint11_0)] <- 0
gdpidpoint11_0

## updates ##


gdpupdate11_0 <- matrix(0, nrow = nrow(gdpidpoint11_0 ), ncol = ncol(gdpidpoint11_0 ) )
for (j in 2:ncol(gdpidpoint11_0 )) {for (i in 2:nrow(gdpidpoint11_0 )) { gdpupdate11_0[i,j] <- ifelse (gdpidpoint11_0[i,j] == gdpidpoint11_0[i-1,j],0,1) & (gdpidpoint11_0[i,j]!=0)  }}
gdpupdate11_0

## create a new column with proportion of updates 
gdpupdaters11_0 <- numeric(nrow(gdpidpoint11_0))
for(t in 2:nrow(gdpidpoint11_0 )) {gdpupdaters11_0[t] <- sum(gdpupdate11_0[t,2:ncol(gdpupdate11_0)])}
gdpupdaters11_0 ## total of updaters

## participants
gdptotal11 <- numeric(12)
gdpparti11 <- matrix(0, nrow = nrow(gdpidpoint11_0 ), ncol = ncol(gdpidpoint11_0 ) )
for (j in 2:ncol(gdpidpoint11_0)) {for (i in 1:nrow(gdpidpoint11_0 )) { gdpparti11[i,j] <- ifelse (gdpidpoint11_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint11_0 )) {gdptotal11[t] <- sum(gdpparti11[t,2:ncol(gdpparti11)])}
gdptotal11



gdpupdaters11_0 <- gdpupdaters11_0[2:12]/gdptotal11[2:12]
gdpupdaters11_0 


## 2012
gdp12_id2 <- subset(newdf,inft == "varpib_a12" & variable =="varpibt" &  analyst == 2 )
gdp12_id2


gy12 <- data.frame(gdp12_id2[,1])

##df for only inflation of the current period
gdpdf12 <- NULL # empty subset 
gdppon12 <- matrix() 
gdpidpoint12 <- add.col(gy12, gdppon12)
gdpidpoint12


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf12 <- subset(newdf,inft == "varpib_a12" & variable =="varpibt" & analyst == t )
gdpidpoint12 <- add.col(gdpidpoint12,gdpdf12[,5]) } 
gdpidpoint12


gdpidpoint12_0 <- gdpidpoint12 
gdpidpoint12_0[is.na(gdpidpoint12_0)] <- 0
gdpidpoint12_0

## updates ##


gdpupdate12_0 <- matrix(0, nrow = nrow(gdpidpoint12_0 ), ncol = ncol(gdpidpoint12_0 ) )
for (j in 2:ncol(gdpidpoint12_0 )) {for (i in 2:nrow(gdpidpoint12_0 )) { gdpupdate12_0[i,j] <- ifelse (gdpidpoint12_0[i,j] == gdpidpoint12_0[i-1,j],0,1) & (gdpidpoint12_0[i,j]!=0)  }}
gdpupdate12_0

## create a new column with proportion of updates 
gdpupdaters12_0 <- numeric(nrow(gdpidpoint12_0))
for(t in 2:nrow(gdpidpoint12_0 )) {gdpupdaters12_0[t] <- sum(gdpupdate12_0[t,2:ncol(gdpupdate12_0)])}
gdpupdaters12_0 ## total of updaters

## participants
gdptotal12 <- numeric(12)
gdpparti12 <- matrix(0, nrow = nrow(gdpidpoint12_0 ), ncol = ncol(gdpidpoint12_0 ) )
for (j in 2:ncol(gdpidpoint12_0)) {for (i in 1:nrow(gdpidpoint12_0 )) { gdpparti12[i,j] <- ifelse (gdpidpoint12_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint12_0 )) {gdptotal12[t] <- sum(gdpparti12[t,2:ncol(gdpparti12)])}
gdptotal12



gdpupdaters12_0 <- gdpupdaters12_0[2:12]/gdptotal12[2:12]
gdpupdaters12_0 


## 2013
gdp13_id2 <- subset(newdf,inft == "varpib_a13" & variable =="varpibt" &  analyst == 2 )
gdp13_id2


gy13 <- data.frame(gdp13_id2[,1])

##df for only inflation of the current period
gdpdf13 <- NULL # empty subset 
gdppon13 <- matrix() 
gdpidpoint13 <- add.col(gy13, gdppon13)
gdpidpoint13


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf13 <- subset(newdf,inft == "varpib_a13" & variable =="varpibt" & analyst == t )
gdpidpoint13 <- add.col(gdpidpoint13,gdpdf13[,5]) } 
gdpidpoint13


gdpidpoint13_0 <- gdpidpoint13
gdpidpoint13_0[is.na(gdpidpoint13_0)] <- 0
gdpidpoint13_0

## updates ##


gdpupdate13_0 <- matrix(0, nrow = nrow(gdpidpoint13_0 ), ncol = ncol(gdpidpoint13_0 ) )
for (j in 2:ncol(gdpidpoint13_0 )) {for (i in 2:nrow(gdpidpoint13_0 )) { gdpupdate13_0[i,j] <- ifelse (gdpidpoint13_0[i,j] == gdpidpoint13_0[i-1,j],0,1) & (gdpidpoint13_0[i,j]!=0)  }}
gdpupdate13_0

## create a new column with proportion of updates 
gdpupdaters13_0 <- numeric(nrow(gdpidpoint13_0))
for(t in 2:nrow(gdpidpoint13_0 )) {gdpupdaters13_0[t] <- sum(gdpupdate13_0[t,2:ncol(gdpupdate13_0)])}
gdpupdaters13_0 ## total of updaters

## participants
gdptotal13 <- numeric(12)
gdpparti13 <- matrix(0, nrow = nrow(gdpidpoint13_0 ), ncol = ncol(gdpidpoint13_0 ) )
for (j in 2:ncol(gdpidpoint13_0)) {for (i in 1:nrow(gdpidpoint13_0 )) { gdpparti13[i,j] <- ifelse (gdpidpoint13_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint13_0 )) {gdptotal13[t] <- sum(gdpparti13[t,2:ncol(gdpparti13)])}
gdptotal13



gdpupdaters13_0 <- gdpupdaters13_0[2:12]/gdptotal13[2:12]
gdpupdaters13_0 


## 2014
gdp14_id2 <- subset(newdf,inft == "varpib_a14" & variable =="varpibt" &  analyst == 2 )
gdp14_id2


gy14 <- data.frame(gdp14_id2[,1])

##df for only inflation of the current period
gdpdf14 <- NULL # empty subset 
gdppon14 <- matrix() 
gdpidpoint14 <- add.col(gy14, gdppon14)
gdpidpoint14


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf14 <- subset(newdf,inft == "varpib_a14" & variable =="varpibt" & analyst == t )
gdpidpoint14 <- add.col(gdpidpoint14,gdpdf14[,5]) } 
gdpidpoint14


gdpidpoint14_0 <- gdpidpoint14
gdpidpoint14_0[is.na(gdpidpoint14_0)] <- 0
gdpidpoint14_0

## updates ##


gdpupdate14_0 <- matrix(0, nrow = nrow(gdpidpoint14_0 ), ncol = ncol(gdpidpoint14_0 ) )
for (j in 2:ncol(gdpidpoint14_0 )) {for (i in 2:nrow(gdpidpoint14_0 )) { gdpupdate14_0[i,j] <- ifelse (gdpidpoint14_0[i,j] == gdpidpoint14_0[i-1,j],0,1) & (gdpidpoint14_0[i,j]!=0)  }}
gdpupdate14_0

## create a new column with proportion of updates 
gdpupdaters14_0 <- numeric(nrow(gdpidpoint14_0))
for(t in 2:nrow(gdpidpoint14_0 )) {gdpupdaters14_0[t] <- sum(gdpupdate14_0[t,2:ncol(gdpupdate14_0)])}
gdpupdaters14_0 ## total of updaters

## participants
gdptotal14 <- numeric(12)
gdpparti14 <- matrix(0, nrow = nrow(gdpidpoint14_0 ), ncol = ncol(gdpidpoint14_0 ) )
for (j in 2:ncol(gdpidpoint14_0)) {for (i in 1:nrow(gdpidpoint14_0 )) { gdpparti14[i,j] <- ifelse (gdpidpoint14_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint14_0 )) {gdptotal14[t] <- sum(gdpparti14[t,2:ncol(gdpparti14)])}
gdptotal14



gdpupdaters14_0 <- gdpupdaters14_0[2:12]/gdptotal14[2:12]
gdpupdaters14_0 


## 2015
gdp15_id2 <- subset(newdf,inft == "varpib_a15" & variable =="varpibt" &  analyst == 4 )
gdp15_id2


gy15 <- data.frame(gdp15_id2[,1])

##df for only inflation of the current period
gdpdf15 <- NULL # empty subset 
gdppon15 <- matrix() 
gdpidpoint15 <- add.col(gy15, gdppon15)
gdpidpoint15


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf15 <- subset(newdf,inft == "varpib_a15" & variable =="varpibt" & analyst == t )
gdpidpoint15 <- add.col(gdpidpoint15,gdpdf15[,5]) } 
gdpidpoint15


gdpidpoint15_0 <- gdpidpoint15
gdpidpoint15_0[is.na(gdpidpoint15_0)] <- 0
gdpidpoint15_0

## updates ##


gdpupdate15_0 <- matrix(0, nrow = nrow(gdpidpoint15_0 ), ncol = ncol(gdpidpoint15_0 ) )
for (j in 2:ncol(gdpidpoint15_0 )) {for (i in 2:nrow(gdpidpoint15_0 )) { gdpupdate15_0[i,j] <- ifelse (gdpidpoint15_0[i,j] == gdpidpoint15_0[i-1,j],0,1) & (gdpidpoint15_0[i,j]!=0)  }}
gdpupdate15_0

## create a new column with proportion of updates 
gdpupdaters15_0 <- numeric(nrow(gdpidpoint15_0))
for(t in 2:nrow(gdpidpoint15_0 )) {gdpupdaters15_0[t] <- sum(gdpupdate15_0[t,2:ncol(gdpupdate15_0)])}
gdpupdaters15_0 ## total of updaters

## participants
gdptotal15 <- numeric(12)
gdpparti15 <- matrix(0, nrow = nrow(gdpidpoint15_0 ), ncol = ncol(gdpidpoint15_0 ) )
for (j in 2:ncol(gdpidpoint15_0)) {for (i in 1:nrow(gdpidpoint15_0 )) { gdpparti15[i,j] <- ifelse (gdpidpoint15_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint15_0 )) {gdptotal15[t] <- sum(gdpparti15[t,2:ncol(gdpparti15)])}
gdptotal15



gdpupdaters15_0 <- gdpupdaters15_0[2:12]/gdptotal15[2:12]
gdpupdaters15_0 

## 2016
gdp16_id2 <- subset(newdf,inft == "varpib_a16" & variable =="varpibt" &  analyst == 4 )
gdp16_id2


gy16 <- data.frame(gdp16_id2[,1])

##df for only inflation of the current period
gdpdf16 <- NULL # empty subset 
gdppon16 <- matrix() 
gdpidpoint16 <- add.col(gy16, gdppon16)
gdpidpoint16


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf16 <- subset(newdf,inft == "varpib_a16" & variable =="varpibt" & analyst == t )
gdpidpoint16 <- add.col(gdpidpoint16,gdpdf16[,5]) } 
gdpidpoint16


gdpidpoint16_0 <- gdpidpoint16
gdpidpoint16_0[is.na(gdpidpoint16_0)] <- 0
gdpidpoint16_0

## updates ##


gdpupdate16_0 <- matrix(0, nrow = nrow(gdpidpoint16_0 ), ncol = ncol(gdpidpoint16_0 ) )
for (j in 2:ncol(gdpidpoint16_0 )) {for (i in 2:nrow(gdpidpoint16_0 )) { gdpupdate16_0[i,j] <- ifelse (gdpidpoint16_0[i,j] == gdpidpoint16_0[i-1,j],0,1) & (gdpidpoint16_0[i,j]!=0)  }}
gdpupdate16_0

## create a new column with proportion of updates 
gdpupdaters16_0 <- numeric(nrow(gdpidpoint16_0))
for(t in 2:nrow(gdpidpoint16_0 )) {gdpupdaters16_0[t] <- sum(gdpupdate16_0[t,2:ncol(gdpupdate16_0)])}
gdpupdaters16_0 ## total of updaters

## participants
gdptotal16 <- numeric(12)
gdpparti16 <- matrix(0, nrow = nrow(gdpidpoint16_0 ), ncol = ncol(gdpidpoint16_0 ) )
for (j in 2:ncol(gdpidpoint16_0)) {for (i in 1:nrow(gdpidpoint16_0 )) { gdpparti16[i,j] <- ifelse (gdpidpoint16_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint16_0 )) {gdptotal16[t] <- sum(gdpparti16[t,2:ncol(gdpparti16)])}
gdptotal16



gdpupdaters16_0 <- gdpupdaters16_0[2:12]/gdptotal16[2:12]
gdpupdaters16_0 



## 2017
gdp17_id2 <- subset(newdf,inft == "varpib_a17" & variable =="varpibt" &  analyst == 4 )
gdp17_id2


gy17 <- data.frame(gdp17_id2[,1])

##df for only inflation of the current period
gdpdf17 <- NULL # empty subset 
gdppon17 <- matrix() 
gdpidpoint17 <- add.col(gy17, gdppon17)
gdpidpoint17


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf17 <- subset(newdf,inft == "varpib_a17" & variable =="varpibt" & analyst == t )
gdpidpoint17 <- add.col(gdpidpoint17,gdpdf17[,5]) } 
gdpidpoint17


gdpidpoint17_0 <- gdpidpoint17
gdpidpoint17_0[is.na(gdpidpoint17_0)] <- 0
gdpidpoint17_0

## updates ##


gdpupdate17_0 <- matrix(0, nrow = nrow(gdpidpoint17_0 ), ncol = ncol(gdpidpoint17_0 ) )
for (j in 2:ncol(gdpidpoint17_0 )) {for (i in 2:nrow(gdpidpoint17_0 )) { gdpupdate17_0[i,j] <- ifelse (gdpidpoint17_0[i,j] == gdpidpoint17_0[i-1,j],0,1) & (gdpidpoint17_0[i,j]!=0)  }}
gdpupdate17_0

## create a new column with proportion of updates 
gdpupdaters17_0 <- numeric(nrow(gdpidpoint17_0))
for(t in 2:nrow(gdpidpoint17_0 )) {gdpupdaters17_0[t] <- sum(gdpupdate17_0[t,2:ncol(gdpupdate17_0)])}
gdpupdaters17_0 ## total of updaters

## participants
gdptotal17 <- numeric(12)
gdpparti17 <- matrix(0, nrow = nrow(gdpidpoint17_0 ), ncol = ncol(gdpidpoint17_0 ) )
for (j in 2:ncol(gdpidpoint17_0)) {for (i in 1:nrow(gdpidpoint17_0 )) { gdpparti17[i,j] <- ifelse (gdpidpoint17_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint17_0 )) {gdptotal17[t] <- sum(gdpparti17[t,2:ncol(gdpparti17)])}
gdptotal17



gdpupdaters17_0 <- gdpupdaters17_0[2:12]/gdptotal17[2:12]
gdpupdaters17_0 


## 2018
gdp18_id2 <- subset(newdf,inft == "varpib_a18" & variable =="varpibt" &  analyst == 4 )
gdp18_id2


gy18 <- data.frame(gdp18_id2[,1])

##df for only inflation of the current period
gdpdf18 <- NULL # empty subset 
gdppon18 <- matrix() 
gdpidpoint18 <- add.col(gy18, gdppon18)
gdpidpoint18


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf18 <- subset(newdf,inft == "varpib_a18" & variable =="varpibt" & analyst == t )
gdpidpoint18 <- add.col(gdpidpoint18,gdpdf18[,5]) } 
gdpidpoint18


gdpidpoint18_0 <- gdpidpoint18
gdpidpoint18_0[is.na(gdpidpoint18_0)] <- 0
gdpidpoint18_0

## updates ##


gdpupdate18_0 <- matrix(0, nrow = nrow(gdpidpoint18_0 ), ncol = ncol(gdpidpoint18_0 ) )
for (j in 2:ncol(gdpidpoint18_0 )) {for (i in 2:nrow(gdpidpoint18_0 )) { gdpupdate18_0[i,j] <- ifelse (gdpidpoint18_0[i,j] == gdpidpoint18_0[i-1,j],0,1) & (gdpidpoint18_0[i,j]!=0)  }}
gdpupdate18_0

## create a new column with proportion of updates 
gdpupdaters18_0 <- numeric(nrow(gdpidpoint18_0))
for(t in 2:nrow(gdpidpoint18_0 )) {gdpupdaters18_0[t] <- sum(gdpupdate18_0[t,2:ncol(gdpupdate18_0)])}
gdpupdaters18_0 ## total of updaters

## participants
gdptotal18 <- numeric(11)
gdpparti18 <- matrix(0, nrow = nrow(gdpidpoint18_0 ), ncol = ncol(gdpidpoint18_0 ) )
for (j in 2:ncol(gdpidpoint18_0)) {for (i in 1:nrow(gdpidpoint18_0 )) { gdpparti18[i,j] <- ifelse (gdpidpoint18_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint18_0 )) {gdptotal18[t] <- sum(gdpparti18[t,2:ncol(gdpparti18)])}
gdptotal18



gdpupdaters18_0 <- gdpupdaters18_0[2:11]/gdptotal18[2:11]
gdpupdaters18_0 


gdpdates <- c(gdpdat07, gdpdat08, gdpdat09, gdpdat10, gdpdat11, gdpdat12, gdpdat13, gdpdat14, gdpdat15, gdpdat16, gdpdat17, gdpdat18)
gdpupdaters <- c(gdpupdaters07_0, gdpupdaters08_0, gdpupdaters09_0, gdpupdaters10_0, gdpupdaters11_0, gdpupdaters12_0, gdpupdaters13_0, gdpupdaters14_0, gdpupdaters15_0, gdpupdaters16_0, gdpupdaters17_0, gdpupdaters18_0)
plot(gdpdates, gdpupdaters, type = "l", xlab = "Time", ylab = "Updaters")

## Fig 4
ggplot(data = data.frame(gdpdates, gdpupdaters), aes(x= gdpdates, y = gdpupdaters)) +
  geom_line()

mean(gdpupdaters)
gdpupdaters18_0

gdpparticipants <- c(gdptotal07, gdptotal08, gdptotal09, gdptotal10, gdptotal11, gdptotal12, gdptotal13, gdptotal14, gdptotal15, gdptotal16, gdptotal17, gdptotal18)

mean(gdpparticipants)
max(gdpparticipants)
min(gdpparticipants)

gdpdat07 <- as.Date(seq(ISOdate(2007,2,1), by = "month", length.out = 10), format = "%d/%m/%Y")
gdpdat08 <- as.Date(seq(ISOdate(2008,2,1), by = "month", length.out = 9), format = "%d/%m/%Y")
gdpdat09 <- as.Date(seq(ISOdate(2009,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
gdpdat10 <- as.Date(seq(ISOdate(2010,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
gdpdat11 <- as.Date(seq(ISOdate(2011,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
gdpdat12 <- as.Date(seq(ISOdate(2012,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
gdpdat13 <- as.Date(seq(ISOdate(2013,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
gdpdat14 <- as.Date(seq(ISOdate(2014,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
gdpdat15 <- as.Date(seq(ISOdate(2015,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
gdpdat16 <- as.Date(seq(ISOdate(2016,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
gdpdat17 <- as.Date(seq(ISOdate(2017,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
gdpdat18 <- as.Date(seq(ISOdate(2018,2,1), by = "month", length.out = 10), format = "%d/%m/%Y")
plot(gdpdat08, gdpupdaters08_0, type = 'l')




### GDP Forecast One Year Ahead 





## 2005-2006
gdp0506_id2 <- subset(newdf,inft == "varpib_a06" & (variable =="varpibtmas1") &  analyst == 2 )
gdp0506_id2


gy0506 <- data.frame(gdp0506_id2[,1])

##df for only inflation of the current period
gdpdf0506 <- NULL # empty subset 
gdppon0506 <- matrix() 
gdpidpoint0506 <- add.col(gy0506, gdppon0506)
gdpidpoint0506

## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf0506 <- subset(newdf,inft == "varpib_a06" & variable =="varpibtmas1" & analyst == t )
gdpidpoint0506 <- add.col(gdpidpoint0506,gdpdf0506[,5]) } 
gdpidpoint0506

gdpidpoint0506_0 <- gdpidpoint0506
gdpidpoint0506_0[is.na(gdpidpoint0506_0)] <- 0
gdpidpoint0506_0

## updates ##
gdpupdate0506_0 <- matrix(0, nrow = nrow(gdpidpoint0506_0 ), ncol = ncol(gdpidpoint0506_0 ) )
for (j in 2:ncol(gdpidpoint0506_0 )) {for (i in 2:nrow(gdpidpoint0506_0 )) { gdpupdate0506_0[i,j] <- ifelse (gdpidpoint0506_0[i,j] == gdpidpoint0506_0[i-1,j],0,1) & (gdpidpoint0506_0[i,j]!=0)  }}
gdpupdate0506_0

## create a new column with proportion of updates 
gdpupdaters0506_0 <- numeric(nrow(gdpidpoint0506_0))
for(t in 2:nrow(gdpidpoint0506_0 )) {gdpupdaters0506_0[t] <- sum(gdpupdate0506_0[t,2:ncol(gdpupdate0506_0)])}
gdpupdaters0506_0 ## total of updaters

## participants
gdptotal0506 <- numeric(12)
gdpparti0506<- matrix(0, nrow = nrow(gdpidpoint0506_0 ), ncol = ncol(gdpidpoint0506_0 ) )
for (j in 2:ncol(gdpidpoint0506_0)) {for (i in 1:nrow(gdpidpoint0506_0 )) { gdpparti0506[i,j] <- ifelse (gdpidpoint0506_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint0506_0 )) {gdptotal0506[t] <- sum(gdpparti0506[t,2:ncol(gdpparti0506)])}
gdptotal0506

gdpupdaters0506_0 <- gdpupdaters0506_0[2:12]/gdptotal0506[2:12]
gdpupdaters0506_0


## 2006-2007
gdp0607_id2 <- subset(newdf,inft == "varpib_a07" & (variable =="varpibtmas1") &  analyst == 2 )
gdp0607_id2


gy0607 <- data.frame(gdp0607_id2[,1])

##df for only inflation of the current period
gdpdf0607 <- NULL # empty subset 
gdppon0607 <- matrix() 
gdpidpoint0607 <- add.col(gy0607, gdppon0607)
gdpidpoint0607

## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf0607 <- subset(newdf,inft == "varpib_a07" & variable =="varpibtmas1" & analyst == t )
gdpidpoint0607 <- add.col(gdpidpoint0607,gdpdf0607[,5]) } 
gdpidpoint0607

gdpidpoint0607_0 <- gdpidpoint0607
gdpidpoint0607_0[is.na(gdpidpoint0607_0)] <- 0
gdpidpoint0607_0

## updates ##
gdpupdate0607_0 <- matrix(0, nrow = nrow(gdpidpoint0607_0 ), ncol = ncol(gdpidpoint0607_0 ) )
for (j in 2:ncol(gdpidpoint0607_0 )) {for (i in 2:nrow(gdpidpoint0607_0 )) { gdpupdate0607_0[i,j] <- ifelse (gdpidpoint0607_0[i,j] == gdpidpoint0607_0[i-1,j],0,1) & (gdpidpoint0607_0[i,j]!=0)  }}
gdpupdate0607_0

## create a new column with proportion of updates 
gdpupdaters0607_0 <- numeric(nrow(gdpidpoint0607_0))
for(t in 2:nrow(gdpidpoint0607_0 )) {gdpupdaters0607_0[t] <- sum(gdpupdate0607_0[t,2:ncol(gdpupdate0607_0)])}
gdpupdaters0607_0 ## total of updaters

## participants
gdptotal0607 <- numeric(12)
gdpparti0607<- matrix(0, nrow = nrow(gdpidpoint0607_0 ), ncol = ncol(gdpidpoint0607_0 ) )
for (j in 2:ncol(gdpidpoint0607_0)) {for (i in 1:nrow(gdpidpoint0607_0 )) { gdpparti0607[i,j] <- ifelse (gdpidpoint0607_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint0607_0 )) {gdptotal0607[t] <- sum(gdpparti0607[t,2:ncol(gdpparti0607)])}
gdptotal0607

gdpupdaters0607_0 <- gdpupdaters0607_0[2:12]/gdptotal0607[2:12]
gdpupdaters0607_0

## 2007-2008
gdp0708_id2 <- subset(newdf,inft == "varpib_a08" & (variable =="varpibtmas1") &  analyst == 2 )
gdp0708_id2


gy0708 <- data.frame(gdp0708_id2[,1])

##df for only inflation of the current period
gdpdf0708 <- NULL # empty subset 
gdppon0708 <- matrix() 
gdpidpoint0708 <- add.col(gy0708, gdppon0708)
gdpidpoint0708

## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf0708 <- subset(newdf,inft == "varpib_a08" & variable =="varpibtmas1" & analyst == t )
gdpidpoint0708 <- add.col(gdpidpoint0708,gdpdf0708[,5]) } 
gdpidpoint0708

gdpidpoint0708_0 <- gdpidpoint0708
gdpidpoint0708_0[is.na(gdpidpoint0708_0)] <- 0
gdpidpoint0708_0

## updates ##
gdpupdate0708_0 <- matrix(0, nrow = nrow(gdpidpoint0708_0 ), ncol = ncol(gdpidpoint0708_0 ) )
for (j in 2:ncol(gdpidpoint0708_0 )) {for (i in 2:nrow(gdpidpoint0708_0 )) { gdpupdate0708_0[i,j] <- ifelse (gdpidpoint0708_0[i,j] == gdpidpoint0708_0[i-1,j],0,1) & (gdpidpoint0708_0[i,j]!=0)  }}
gdpupdate0708_0

## create a new column with proportion of updates 
gdpupdaters0708_0 <- numeric(nrow(gdpidpoint0708_0))
for(t in 2:nrow(gdpidpoint0708_0 )) {gdpupdaters0708_0[t] <- sum(gdpupdate0708_0[t,2:ncol(gdpupdate0708_0)])}
gdpupdaters0708_0 ## total of updaters

## participants
gdptotal0708 <- numeric(12)
gdpparti0708<- matrix(0, nrow = nrow(gdpidpoint0708_0 ), ncol = ncol(gdpidpoint0708_0 ) )
for (j in 2:ncol(gdpidpoint0708_0)) {for (i in 1:nrow(gdpidpoint0708_0 )) { gdpparti0708[i,j] <- ifelse (gdpidpoint0708_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint0708_0 )) {gdptotal0708[t] <- sum(gdpparti0708[t,2:ncol(gdpparti0708)])}
gdptotal0708

gdpupdaters0708_0 <- gdpupdaters0708_0[2:12]/gdptotal0708[2:12]
gdpupdaters0708_0


## 2008-2009

gdp0809_id2 <- subset(newdf,inft == "varpib_a09" & (variable =="varpibtmas1") &  analyst == 2 )
gdp0809_id2


gy0809 <- data.frame(gdp0809_id2[,1])

##df for only inflation of the current period
gdpdf0809 <- NULL # empty subset 
gdppon0809 <- matrix() 
gdpidpoint0809 <- add.col(gy0809, gdppon0809)
gdpidpoint0809

## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf0809 <- subset(newdf,inft == "varpib_a09" & variable =="varpibtmas1" & analyst == t )
gdpidpoint0809 <- add.col(gdpidpoint0809,gdpdf0809[,5]) } 
gdpidpoint0809

gdpidpoint0809_0 <- gdpidpoint0809
gdpidpoint0809_0[is.na(gdpidpoint0809_0)] <- 0
gdpidpoint0809_0

## updates ##
gdpupdate0809_0 <- matrix(0, nrow = nrow(gdpidpoint0809_0 ), ncol = ncol(gdpidpoint0809_0 ) )
for (j in 2:ncol(gdpidpoint0809_0 )) {for (i in 2:nrow(gdpidpoint0809_0 )) { gdpupdate0809_0[i,j] <- ifelse (gdpidpoint0809_0[i,j] == gdpidpoint0809_0[i-1,j],0,1) & (gdpidpoint0809_0[i,j]!=0)  }}
gdpupdate0809_0

## create a new column with proportion of updates 
gdpupdaters0809_0 <- numeric(nrow(gdpidpoint0809_0))
for(t in 2:nrow(gdpidpoint0809_0 )) {gdpupdaters0809_0[t] <- sum(gdpupdate0809_0[t,2:ncol(gdpupdate0809_0)])}
gdpupdaters0809_0 ## total of updaters

## participants
gdptotal0809 <- numeric(12)
gdpparti0809<- matrix(0, nrow = nrow(gdpidpoint0809_0 ), ncol = ncol(gdpidpoint0809_0 ) )
for (j in 2:ncol(gdpidpoint0809_0)) {for (i in 1:nrow(gdpidpoint0809_0 )) { gdpparti0809[i,j] <- ifelse (gdpidpoint0809_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint0809_0 )) {gdptotal0809[t] <- sum(gdpparti0809[t,2:ncol(gdpparti0809)])}
gdptotal0809

gdpupdaters0809_0 <- gdpupdaters0809_0[2:12]/gdptotal0809[2:12]
gdpupdaters0809_0



## 2009-2010

gdp0910_id2 <- subset(newdf,inft == "varpib_a10" & (variable =="varpibtmas1") &  analyst == 2 )
gdp0910_id2


gy0910 <- data.frame(gdp0910_id2[,1])

##df for only inflation of the current period
gdpdf0910 <- NULL # empty subset 
gdppon0910 <- matrix() 
gdpidpoint0910 <- add.col(gy0910, gdppon0910)
gdpidpoint0910

## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf0910 <- subset(newdf,inft == "varpib_a10" & variable =="varpibtmas1" & analyst == t )
gdpidpoint0910 <- add.col(gdpidpoint0910,gdpdf0910[,5]) } 
gdpidpoint0910

gdpidpoint0910_0 <- gdpidpoint0910
gdpidpoint0910_0[is.na(gdpidpoint0910_0)] <- 0
gdpidpoint0910_0

## updates ##
gdpupdate0910_0 <- matrix(0, nrow = nrow(gdpidpoint0910_0 ), ncol = ncol(gdpidpoint0910_0 ) )
for (j in 2:ncol(gdpidpoint0910_0 )) {for (i in 2:nrow(gdpidpoint0910_0 )) { gdpupdate0910_0[i,j] <- ifelse (gdpidpoint0910_0[i,j] == gdpidpoint0910_0[i-1,j],0,1) & (gdpidpoint0910_0[i,j]!=0)  }}
gdpupdate0910_0

## create a new column with proportion of updates 
gdpupdaters0910_0 <- numeric(nrow(gdpidpoint0910_0))
for(t in 2:nrow(gdpidpoint0910_0 )) {gdpupdaters0910_0[t] <- sum(gdpupdate0910_0[t,2:ncol(gdpupdate0910_0)])}
gdpupdaters0910_0 ## total of updaters

## participants
gdptotal0910 <- numeric(12)
gdpparti0910<- matrix(0, nrow = nrow(gdpidpoint0910_0 ), ncol = ncol(gdpidpoint0910_0 ) )
for (j in 2:ncol(gdpidpoint0910_0)) {for (i in 1:nrow(gdpidpoint0910_0 )) { gdpparti0910[i,j] <- ifelse (gdpidpoint0910_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint0910_0 )) {gdptotal0910[t] <- sum(gdpparti0910[t,2:ncol(gdpparti0910)])}
gdptotal0910

gdpupdaters0910_0 <- gdpupdaters0910_0[2:12]/gdptotal0910[2:12]
gdpupdaters0910_0


## 2010-2011

gdp1011_id2 <- subset(newdf,inft == "varpib_a11" & (variable =="varpibtmas1") &  analyst == 4 )
gdp1011_id2


gy1011 <- data.frame(gdp1011_id2[,1])

##df for only inflation of the current period
gdpdf1011 <- NULL # empty subset 
gdppon1011 <- matrix() 
gdpidpoint1011 <- add.col(gy1011, gdppon1011)
gdpidpoint1011

## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf1011 <- subset(newdf,inft == "varpib_a11" & variable =="varpibtmas1" & analyst == t )
gdpidpoint1011 <- add.col(gdpidpoint1011,gdpdf1011[,5]) } 
gdpidpoint1011

gdpidpoint1011_0 <- gdpidpoint1011
gdpidpoint1011_0[is.na(gdpidpoint1011_0)] <- 0
gdpidpoint1011_0

## updates ##
gdpupdate1011_0 <- matrix(0, nrow = nrow(gdpidpoint1011_0 ), ncol = ncol(gdpidpoint1011_0 ) )
for (j in 2:ncol(gdpidpoint1011_0 )) {for (i in 2:nrow(gdpidpoint1011_0 )) { gdpupdate1011_0[i,j] <- ifelse (gdpidpoint1011_0[i,j] == gdpidpoint1011_0[i-1,j],0,1) & (gdpidpoint1011_0[i,j]!=0)  }}
gdpupdate1011_0

## create a new column with proportion of updates 
gdpupdaters1011_0 <- numeric(nrow(gdpidpoint1011_0))
for(t in 2:nrow(gdpidpoint1011_0 )) {gdpupdaters1011_0[t] <- sum(gdpupdate1011_0[t,2:ncol(gdpupdate1011_0)])}
gdpupdaters1011_0 ## total of updaters

## participants
gdptotal1011 <- numeric(12)
gdpparti1011<- matrix(0, nrow = nrow(gdpidpoint1011_0 ), ncol = ncol(gdpidpoint1011_0 ) )
for (j in 2:ncol(gdpidpoint1011_0)) {for (i in 1:nrow(gdpidpoint1011_0 )) { gdpparti1011[i,j] <- ifelse (gdpidpoint1011_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint1011_0 )) {gdptotal1011[t] <- sum(gdpparti1011[t,2:ncol(gdpparti1011)])}
gdptotal1011

gdpupdaters1011_0 <- gdpupdaters1011_0[2:12]/gdptotal1011[2:12]
gdpupdaters1011_0

## 2011-2012

gdp1112_id2 <- subset(newdf,inft == "varpib_a12" & (variable =="varpibtmas1") &  analyst == 2 )
gdp1112_id2


gy1112 <- data.frame(gdp1112_id2[,1])

##df for only inflation of the current period
gdpdf1112 <- NULL # empty subset 
gdppon1112 <- matrix() 
gdpidpoint1112 <- add.col(gy1112, gdppon1112)
gdpidpoint1112

## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf1112 <- subset(newdf,inft == "varpib_a12" & variable =="varpibtmas1" & analyst == t )
gdpidpoint1112 <- add.col(gdpidpoint1112,gdpdf1112[,5]) } 
gdpidpoint1112

gdpidpoint1112_0 <- gdpidpoint1112
gdpidpoint1112_0[is.na(gdpidpoint1112_0)] <- 0
gdpidpoint1112_0

## updates ##
gdpupdate1112_0 <- matrix(0, nrow = nrow(gdpidpoint1112_0 ), ncol = ncol(gdpidpoint1112_0 ) )
for (j in 2:ncol(gdpidpoint1112_0 )) {for (i in 2:nrow(gdpidpoint1112_0 )) { gdpupdate1112_0[i,j] <- ifelse (gdpidpoint1112_0[i,j] == gdpidpoint1112_0[i-1,j],0,1) & (gdpidpoint1112_0[i,j]!=0)  }}
gdpupdate1112_0

## create a new column with proportion of updates 
gdpupdaters1112_0 <- numeric(nrow(gdpidpoint1112_0))
for(t in 2:nrow(gdpidpoint1112_0 )) {gdpupdaters1112_0[t] <- sum(gdpupdate1112_0[t,2:ncol(gdpupdate1112_0)])}
gdpupdaters1112_0 ## total of updaters

## participants
gdptotal1112 <- numeric(12)
gdpparti1112<- matrix(0, nrow = nrow(gdpidpoint1112_0 ), ncol = ncol(gdpidpoint1112_0 ) )
for (j in 2:ncol(gdpidpoint1112_0)) {for (i in 1:nrow(gdpidpoint1112_0 )) { gdpparti1112[i,j] <- ifelse (gdpidpoint1112_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint1112_0 )) {gdptotal1112[t] <- sum(gdpparti1112[t,2:ncol(gdpparti1112)])}
gdptotal1112

gdpupdaters1112_0 <- gdpupdaters1112_0[2:12]/gdptotal1112[2:12]
gdpupdaters1112_0


## 2012-2013

gdp1213_id2 <- subset(newdf,inft == "varpib_a13" & (variable =="varpibtmas1") &  analyst == 4 )
gdp1213_id2


gy1213 <- data.frame(gdp1213_id2[,1])

##df for only inflation of the current period
gdpdf1213 <- NULL # empty subset 
gdppon1213 <- matrix() 
gdpidpoint1213 <- add.col(gy1213, gdppon1213)
gdpidpoint1213

## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf1213 <- subset(newdf,inft == "varpib_a13" & variable =="varpibtmas1" & analyst == t )
gdpidpoint1213 <- add.col(gdpidpoint1213,gdpdf1213[,5]) } 
gdpidpoint1213

gdpidpoint1213_0 <- gdpidpoint1213
gdpidpoint1213_0[is.na(gdpidpoint1213_0)] <- 0
gdpidpoint1213_0

## updates ##
gdpupdate1213_0 <- matrix(0, nrow = nrow(gdpidpoint1213_0 ), ncol = ncol(gdpidpoint1213_0 ) )
for (j in 2:ncol(gdpidpoint1213_0 )) {for (i in 2:nrow(gdpidpoint1213_0 )) { gdpupdate1213_0[i,j] <- ifelse (gdpidpoint1213_0[i,j] == gdpidpoint1213_0[i-1,j],0,1) & (gdpidpoint1213_0[i,j]!=0)  }}
gdpupdate1213_0

## create a new column with proportion of updates 
gdpupdaters1213_0 <- numeric(nrow(gdpidpoint1213_0))
for(t in 2:nrow(gdpidpoint1213_0 )) {gdpupdaters1213_0[t] <- sum(gdpupdate1213_0[t,2:ncol(gdpupdate1213_0)])}
gdpupdaters1213_0 ## total of updaters

## participants
gdptotal1213 <- numeric(12)
gdpparti1213<- matrix(0, nrow = nrow(gdpidpoint1213_0 ), ncol = ncol(gdpidpoint1213_0 ) )
for (j in 2:ncol(gdpidpoint1213_0)) {for (i in 1:nrow(gdpidpoint1213_0 )) { gdpparti1213[i,j] <- ifelse (gdpidpoint1213_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint1213_0 )) {gdptotal1213[t] <- sum(gdpparti1213[t,2:ncol(gdpparti1213)])}
gdptotal1213

gdpupdaters1213_0 <- gdpupdaters1213_0[2:12]/gdptotal1213[2:12]
gdpupdaters1213_0

## 2013-2014

gdp1314_id2 <- subset(newdf,inft == "varpib_a14" & (variable =="varpibtmas1") &  analyst == 4 )
gdp1314_id2


gy1314 <- data.frame(gdp1314_id2[,1])

##df for only inflation of the current period
gdpdf1314 <- NULL # empty subset 
gdppon1314 <- matrix() 
gdpidpoint1314 <- add.col(gy1314, gdppon1314)
gdpidpoint1314

## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf1314 <- subset(newdf,inft == "varpib_a14" & variable =="varpibtmas1" & analyst == t )
gdpidpoint1314 <- add.col(gdpidpoint1314,gdpdf1314[,5]) } 
gdpidpoint1314

gdpidpoint1314_0 <- gdpidpoint1314 
gdpidpoint1314_0[is.na(gdpidpoint1314_0)] <- 0
gdpidpoint1314_0

## updates ##
gdpupdate1314_0 <- matrix(0, nrow = nrow(gdpidpoint1314_0 ), ncol = ncol(gdpidpoint1314_0 ) )
for (j in 2:ncol(gdpidpoint1314_0 )) {for (i in 2:nrow(gdpidpoint1314_0 )) { gdpupdate1314_0[i,j] <- ifelse (gdpidpoint1314_0[i,j] == gdpidpoint1314_0[i-1,j],0,1) & (gdpidpoint1314_0[i,j]!=0)  }}
gdpupdate1314_0

## create a new column with proportion of updates 
gdpupdaters1314_0 <- numeric(nrow(gdpidpoint1314_0))
for(t in 2:nrow(gdpidpoint1314_0 )) {gdpupdaters1314_0[t] <- sum(gdpupdate1314_0[t,2:ncol(gdpupdate1314_0)])}
gdpupdaters1314_0 ## total of updaters

## participants
gdptotal1314 <- numeric(12)
gdpparti1314<- matrix(0, nrow = nrow(gdpidpoint1314_0 ), ncol = ncol(gdpidpoint1314_0 ) )
for (j in 2:ncol(gdpidpoint1314_0)) {for (i in 1:nrow(gdpidpoint1314_0 )) { gdpparti1314[i,j] <- ifelse (gdpidpoint1314_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint1314_0 )) {gdptotal1314[t] <- sum(gdpparti1314[t,2:ncol(gdpparti1314)])}
gdptotal1314

gdpupdaters1314_0 <- gdpupdaters1314_0[2:12]/gdptotal1314[2:12]
gdpupdaters1314_0


## 2014-2015

gdp1415_id2 <- subset(newdf,inft == "varpib_a15" & (variable =="varpibtmas1") &  analyst == 4 )
gdp1415_id2


gy1415 <- data.frame(gdp1415_id2[,1])

##df for only inflation of the current period
gdpdf1415 <- NULL # empty subset 
gdppon1415 <- matrix() 
gdpidpoint1415 <- add.col(gy1415, gdppon1415)
gdpidpoint1415

## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf1415 <- subset(newdf,inft == "varpib_a15" & variable =="varpibtmas1" & analyst == t )
gdpidpoint1415 <- add.col(gdpidpoint1415,gdpdf1415[,5]) } 
gdpidpoint1415

gdpidpoint1415_0 <- gdpidpoint1415
gdpidpoint1415_0[is.na(gdpidpoint1415_0)] <- 0
gdpidpoint1415_0

## updates ##
gdpupdate1415_0 <- matrix(0, nrow = nrow(gdpidpoint1415_0 ), ncol = ncol(gdpidpoint1415_0 ) )
for (j in 2:ncol(gdpidpoint1415_0 )) {for (i in 2:nrow(gdpidpoint1415_0 )) { gdpupdate1415_0[i,j] <- ifelse (gdpidpoint1415_0[i,j] == gdpidpoint1415_0[i-1,j],0,1) & (gdpidpoint1415_0[i,j]!=0)  }}
gdpupdate1415_0

## create a new column with proportion of updates 
gdpupdaters1415_0 <- numeric(nrow(gdpidpoint1415_0))
for(t in 2:nrow(gdpidpoint1415_0 )) {gdpupdaters1415_0[t] <- sum(gdpupdate1415_0[t,2:ncol(gdpupdate1415_0)])}
gdpupdaters1415_0 ## total of updaters

## participants
gdptotal1415 <- numeric(12)
gdpparti1415<- matrix(0, nrow = nrow(gdpidpoint1415_0 ), ncol = ncol(gdpidpoint1415_0 ) )
for (j in 2:ncol(gdpidpoint1415_0)) {for (i in 1:nrow(gdpidpoint1415_0 )) { gdpparti1415[i,j] <- ifelse (gdpidpoint1415_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint1415_0 )) {gdptotal1415[t] <- sum(gdpparti1415[t,2:ncol(gdpparti1415)])}
gdptotal1415

gdpupdaters1415_0 <- gdpupdaters1415_0[2:12]/gdptotal1415[2:12]
gdpupdaters1415_0


## 2015-2016

gdp1516_id2 <- subset(newdf,inft == "varpib_a16" & (variable =="varpibtmas1") &  analyst == 4 )
gdp1516_id2


gy1516 <- data.frame(gdp1516_id2[,1])

##df for only inflation of the current period
gdpdf1516 <- NULL # empty subset 
gdppon1516 <- matrix() 
gdpidpoint1516 <- add.col(gy1516, gdppon1516)
gdpidpoint1516

## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf1516 <- subset(newdf,inft == "varpib_a16" & variable =="varpibtmas1" & analyst == t )
gdpidpoint1516 <- add.col(gdpidpoint1516,gdpdf1516[,5]) } 
gdpidpoint1516

gdpidpoint1516_0 <- gdpidpoint1516
gdpidpoint1516_0[is.na(gdpidpoint1516_0)] <- 0
gdpidpoint1516_0

## updates ##
gdpupdate1516_0 <- matrix(0, nrow = nrow(gdpidpoint1516_0 ), ncol = ncol(gdpidpoint1516_0 ) )
for (j in 2:ncol(gdpidpoint1516_0 )) {for (i in 2:nrow(gdpidpoint1516_0 )) { gdpupdate1516_0[i,j] <- ifelse (gdpidpoint1516_0[i,j] == gdpidpoint1516_0[i-1,j],0,1) & (gdpidpoint1516_0[i,j]!=0)  }}
gdpupdate1516_0

## create a new column with proportion of updates 
gdpupdaters1516_0 <- numeric(nrow(gdpidpoint1516_0))
for(t in 2:nrow(gdpidpoint1516_0 )) {gdpupdaters1516_0[t] <- sum(gdpupdate1516_0[t,2:ncol(gdpupdate1516_0)])}
gdpupdaters1516_0 ## total of updaters

## participants
gdptotal1516 <- numeric(12)
gdpparti1516<- matrix(0, nrow = nrow(gdpidpoint1516_0 ), ncol = ncol(gdpidpoint1516_0 ) )
for (j in 2:ncol(gdpidpoint1516_0)) {for (i in 1:nrow(gdpidpoint1516_0 )) { gdpparti1516[i,j] <- ifelse (gdpidpoint1516_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint1516_0 )) {gdptotal1516[t] <- sum(gdpparti1516[t,2:ncol(gdpparti1516)])}
gdptotal1516

gdpupdaters1516_0 <- gdpupdaters1516_0[2:12]/gdptotal1516[2:12]
gdpupdaters1516_0


## 2016-2017

gdp1617_id2 <- subset(newdf,inft == "varpib_a17" & (variable =="varpibtmas1") &  analyst == 4 )
gdp1617_id2


gy1617 <- data.frame(gdp1617_id2[,1])

##df for only inflation of the current period
gdpdf1617 <- NULL # empty subset 
gdppon1617 <- matrix() 
gdpidpoint1617 <- add.col(gy1617, gdppon1617)
gdpidpoint1617

## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf1617 <- subset(newdf,inft == "varpib_a17" & variable =="varpibtmas1" & analyst == t )
gdpidpoint1617 <- add.col(gdpidpoint1617,gdpdf1617[,5]) } 
gdpidpoint1617

gdpidpoint1617_0 <- gdpidpoint1617
gdpidpoint1617_0[is.na(gdpidpoint1617_0)] <- 0
gdpidpoint1617_0

## updates ##
gdpupdate1617_0 <- matrix(0, nrow = nrow(gdpidpoint1617_0 ), ncol = ncol(gdpidpoint1617_0 ) )
for (j in 2:ncol(gdpidpoint1617_0 )) {for (i in 2:nrow(gdpidpoint1617_0 )) { gdpupdate1617_0[i,j] <- ifelse (gdpidpoint1617_0[i,j] == gdpidpoint1617_0[i-1,j],0,1) & (gdpidpoint1617_0[i,j]!=0)  }}
gdpupdate1617_0

## create a new column with proportion of updates 
gdpupdaters1617_0 <- numeric(nrow(gdpidpoint1617_0))
for(t in 2:nrow(gdpidpoint1617_0 )) {gdpupdaters1617_0[t] <- sum(gdpupdate1617_0[t,2:ncol(gdpupdate1617_0)])}
gdpupdaters1617_0 ## total of updaters

## participants
gdptotal1617 <- numeric(12)
gdpparti1617<- matrix(0, nrow = nrow(gdpidpoint1617_0 ), ncol = ncol(gdpidpoint1617_0 ) )
for (j in 2:ncol(gdpidpoint1617_0)) {for (i in 1:nrow(gdpidpoint1617_0 )) { gdpparti1617[i,j] <- ifelse (gdpidpoint1617_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint1617_0 )) {gdptotal1617[t] <- sum(gdpparti1617[t,2:ncol(gdpparti1617)])}
gdptotal1617

gdpupdaters1617_0 <- gdpupdaters1617_0[2:12]/gdptotal1617[2:12]
gdpupdaters1617_0

## 2017-2018

gdp1718_id2 <- subset(newdf,inft == "varpib_a18" & (variable =="varpibtmas1") &  analyst == 4 )
gdp1718_id2


gy1718 <- data.frame(gdp1718_id2[,1])

##df for only inflation of the current period
gdpdf1718 <- NULL # empty subset 
gdppon1718 <- matrix() 
gdpidpoint1718 <- add.col(gy1718, gdppon1718)
gdpidpoint1718

## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf1718 <- subset(newdf,inft == "varpib_a18" & variable =="varpibtmas1" & analyst == t )
gdpidpoint1718 <- add.col(gdpidpoint1718,gdpdf1718[,5]) } 
gdpidpoint1718

gdpidpoint1718_0 <- gdpidpoint1718
gdpidpoint1718_0[is.na(gdpidpoint1718_0)] <- 0
gdpidpoint1718_0

## updates ##
gdpupdate1718_0 <- matrix(0, nrow = nrow(gdpidpoint1718_0 ), ncol = ncol(gdpidpoint1718_0 ) )
for (j in 2:ncol(gdpidpoint1718_0 )) {for (i in 2:nrow(gdpidpoint1718_0 )) { gdpupdate1718_0[i,j] <- ifelse (gdpidpoint1718_0[i,j] == gdpidpoint1718_0[i-1,j],0,1) & (gdpidpoint1718_0[i,j]!=0)  }}
gdpupdate1718_0

## create a new column with proportion of updates 
gdpupdaters1718_0 <- numeric(nrow(gdpidpoint1718_0))
for(t in 2:nrow(gdpidpoint1718_0 )) {gdpupdaters1718_0[t] <- sum(gdpupdate1718_0[t,2:ncol(gdpupdate1718_0)])}
gdpupdaters1718_0 ## total of updaters

## participants
gdptotal1718 <- numeric(12)
gdpparti1718<- matrix(0, nrow = nrow(gdpidpoint1718_0 ), ncol = ncol(gdpidpoint1718_0 ) )
for (j in 2:ncol(gdpidpoint1718_0)) {for (i in 1:nrow(gdpidpoint1718_0 )) { gdpparti1718[i,j] <- ifelse (gdpidpoint1718_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint1718_0 )) {gdptotal1718[t] <- sum(gdpparti1718[t,2:ncol(gdpparti1718)])}
gdptotal1718

gdpupdaters1718_0 <- gdpupdaters1718_0[2:12]/gdptotal1718[2:12]
gdpupdaters1718_0


## 2018-2019

gdp1819_id2 <- subset(newdf,inft == "varpib_a19" & (variable =="varpibtmas1") &  analyst == 4 )
gdp1819_id2


gy1819 <- data.frame(gdp1819_id2[,1])

##df for only inflation of the current period
gdpdf1819 <- NULL # empty subset 
gdppon1819 <- matrix() 
gdpidpoint1819 <- add.col(gy1819, gdppon1819)
gdpidpoint1819

## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {gdpdf1819 <- subset(newdf,inft == "varpib_a19" & variable =="varpibtmas1" & analyst == t )
gdpidpoint1819 <- add.col(gdpidpoint1819,gdpdf1819[,5]) } 
gdpidpoint1819

gdpidpoint1819_0 <- gdpidpoint1819
gdpidpoint1819_0[is.na(gdpidpoint1819_0)] <- 0
gdpidpoint1819_0

## updates ##
gdpupdate1819_0 <- matrix(0, nrow = nrow(gdpidpoint1819_0 ), ncol = ncol(gdpidpoint1819_0 ) )
for (j in 2:ncol(gdpidpoint1819_0 )) {for (i in 2:nrow(gdpidpoint1819_0 )) { gdpupdate1819_0[i,j] <- ifelse (gdpidpoint1819_0[i,j] == gdpidpoint1819_0[i-1,j],0,1) & (gdpidpoint1819_0[i,j]!=0)  }}
gdpupdate1819_0

## create a new column with proportion of updates 
gdpupdaters1819_0 <- numeric(nrow(gdpidpoint1819_0))
for(t in 2:nrow(gdpidpoint1819_0 )) {gdpupdaters1819_0[t] <- sum(gdpupdate1819_0[t,2:ncol(gdpupdate1819_0)])}
gdpupdaters1819_0 ## total of updaters

## participants
gdptotal1819 <- numeric(11)
gdpparti1819<- matrix(0, nrow = nrow(gdpidpoint1819_0 ), ncol = ncol(gdpidpoint1819_0 ) )
for (j in 2:ncol(gdpidpoint1819_0)) {for (i in 1:nrow(gdpidpoint1819_0 )) { gdpparti1819[i,j] <- ifelse (gdpidpoint1819_0[i,j] != 0,1,0) }}
for(t in 1:nrow(gdpidpoint1819_0 )) {gdptotal1819[t] <- sum(gdpparti1819[t,2:ncol(gdpparti1819)])}
gdptotal1819

gdpupdaters1819_0 <- gdpupdaters1819_0[2:11]/gdptotal1819[2:11]
gdpupdaters1819_0

gdp2updaters <- c(gdpupdaters0506_0, gdpupdaters0607_0, gdpupdaters0708_0, gdpupdaters0809_0, gdpupdaters0910_0, gdpupdaters1011_0, gdpupdaters1112_0, gdpupdaters1213_0, gdpupdaters1314_0, gdpupdaters1415_0, gdpupdaters1516_0,  gdpupdaters1617_0, gdpupdaters1718_0, gdpupdaters1819_0)

##Fig. Updaters GPD one year ahead 
plot(dates, gdp2updaters, type = "l", xlab = "Time", ylab = "Updaters")
lines(dates, updaters, col = "blue")
lines(dates, inf2updaters, col = "red")
lines(dates, log(epu/100), col = "green")

ggplot(data = data.frame(dates, gdp2updaters, updaters, inf2updaters)) +
  geom_line(aes(x = dates, y = gdp2updaters)) + 
  geom_line(aes( x= dates, y = inf2updaters), color = "red") +
 labs( x= "Time", y = "Updaters") +
  guides( fill = guide_legend(gdp2updaters))
  
hist(gdp2cons)
hist(inf2cons)
hist(gdp2updaters)
hist(inf2updaters)
hist(updaters)
hist(gdpupdaters)


length(gdp2updaters)
length(inf2updaters)
length(dates)

mean(gdp2updaters)
mean(inf2updaters)


length(gdpupdaters0506_0)
length(infupdaters0506_0)


## Consensus GDP one year ahead 

gdp2cons05 <- numeric(12)
gdp2cons06 <- numeric(12)
gdp2cons07 <- numeric(12)
gdp2cons08 <- numeric(12)
gdp2cons09 <- numeric(12)
gdp2cons10 <- numeric(12)
gdp2cons11 <- numeric(12)
gdp2cons12 <- numeric(12)
gdp2cons13 <- numeric(12)
gdp2cons14 <- numeric(12)
gdp2cons15 <- numeric(12)
gdp2cons16 <- numeric(12)
gdp2cons17 <- numeric(12)
gdp2cons18 <- numeric(11)

gdpidpointNA0506 <- gdpidpoint0506[ , colSums(is.na(gdpidpoint0506)) == 0]
for(t in 1:nrow(gdpidpointNA0506)) {gdp2cons05[t] <- sum(gdpidpointNA0506[t,2:ncol(gdpidpointNA0506)])/(ncol(gdpidpointNA0506)-1)}
gdp2cons05

gdpidpointNA0607 <- gdpidpoint0607[ , colSums(is.na(gdpidpoint0607)) == 0]
for(t in 1:nrow(gdpidpointNA0607)) {gdp2cons06[t] <- sum(gdpidpointNA0607[t,2:ncol(gdpidpointNA0607)])/(ncol(gdpidpointNA0607)-1)}
gdp2cons06

gdpidpointNA0708 <- gdpidpoint0708[ , colSums(is.na(gdpidpoint0708)) == 0]
for(t in 1:nrow(gdpidpointNA0708)) {gdp2cons07[t] <- sum(gdpidpointNA0708[t,2:ncol(gdpidpointNA0708)])/(ncol(gdpidpointNA0708)-1)}
gdp2cons07

gdpidpointNA0809 <- gdpidpoint0809[ , colSums(is.na(gdpidpoint0809)) == 0]
for(t in 1:nrow(gdpidpointNA0809)) {gdp2cons08[t] <- sum(gdpidpointNA0809[t,2:ncol(gdpidpointNA0809)])/(ncol(gdpidpointNA0809)-1)}
gdp2cons08

gdpidpointNA0910 <- gdpidpoint0910[ , colSums(is.na(gdpidpoint0910)) == 0]
for(t in 1:nrow(gdpidpointNA0910)) {gdp2cons09[t] <- sum(gdpidpointNA0910[t,2:ncol(gdpidpointNA0910)])/(ncol(gdpidpointNA0910)-1)}
gdp2cons09

gdpidpointNA1011 <- gdpidpoint1011[ , colSums(is.na(gdpidpoint1011)) == 0]
for(t in 1:nrow(gdpidpointNA1011)) {gdp2cons10[t] <- sum(gdpidpointNA1011[t,2:ncol(gdpidpointNA1011)])/(ncol(gdpidpointNA1011)-1)}
gdp2cons10

gdpidpointNA1112 <- gdpidpoint1112[ , colSums(is.na(gdpidpoint1112)) == 0]
for(t in 1:nrow(gdpidpointNA1112)) {gdp2cons11[t] <- sum(gdpidpointNA1112[t,2:ncol(gdpidpointNA1112)])/(ncol(gdpidpointNA1112)-1)}
gdp2cons11

gdpidpointNA1213 <- gdpidpoint1213[ , colSums(is.na(gdpidpoint1213)) == 0]
for(t in 1:nrow(gdpidpointNA1213)) {gdp2cons12[t] <- sum(gdpidpointNA1213[t,2:ncol(gdpidpointNA1213)])/(ncol(gdpidpointNA1213)-1)}
gdp2cons12

gdpidpointNA1314 <- gdpidpoint1314[ , colSums(is.na(gdpidpoint1314)) == 0]
for(t in 1:nrow(gdpidpointNA1314)) {gdp2cons13[t] <- sum(gdpidpointNA1314[t,2:ncol(gdpidpointNA1314)])/(ncol(gdpidpointNA1314)-1)}
gdp2cons13

gdpidpointNA1415 <- gdpidpoint1415[ , colSums(is.na(gdpidpoint1415)) == 0]
for(t in 1:nrow(gdpidpointNA1415)) {gdp2cons14[t] <- sum(gdpidpointNA1415[t,2:ncol(gdpidpointNA1415)])/(ncol(gdpidpointNA1415)-1)}
gdp2cons14

gdpidpointNA1516 <- gdpidpoint1516[ , colSums(is.na(gdpidpoint1516)) == 0]
for(t in 1:nrow(gdpidpointNA1516)) {gdp2cons15[t] <- sum(gdpidpointNA1516[t,2:ncol(gdpidpointNA1516)])/(ncol(gdpidpointNA1516)-1)}
gdp2cons15

gdpidpointNA1617 <- gdpidpoint1617[ , colSums(is.na(gdpidpoint1617)) == 0]
for(t in 1:nrow(gdpidpointNA1617)) {gdp2cons16[t] <- sum(gdpidpointNA1617[t,2:ncol(gdpidpointNA1617)])/(ncol(gdpidpointNA1617)-1)}
gdp2cons16

gdpidpointNA1819 <- gdpidpoint1819[ , colSums(is.na(gdpidpoint1819)) == 0]
for(t in 1:nrow(gdpidpointNA1819)) {gdp2cons18[t] <- sum(gdpidpointNA1819[t,2:ncol(gdpidpointNA1819)])/(ncol(gdpidpointNA1819)-1)}
gdp2cons18


month <- c(2,3,4,5,6,7,8,8,10,11,12)
plot(month, gdpupdaters0607_0, type = "l", col = "blue")
lines(month, gdpupdaters0506_0, col = "red" )
lines(month, gdpupdaters0708_0, col = "grey" )
lines(month, gdpupdaters0809_0, col = "green" )

## read amlo2
amlo2db <- read.csv("amlo2.csv")
amlo2 <- amlo2db[,2]
amlo21 <- amlo2db[,3]
epn <- amlo2db[,4]
gdp2cons <- c(gdp2cons05, gdp2cons06, gdp2cons07, gdp2cons08, gdp2cons09, gdp2cons10, gdp2cons11, gdp2cons12, gdp2cons13, gdp2cons14, gdp2cons15, gdp2cons16, gdp2cons17, gdp2cons18 )

length(amlo2)
length(epn)
length(gdp2cons)

congdp2_amlo <- lm(gdp2cons ~ Lag(gdp2cons,1) + amlo21  )
summary(congdp2_amlo)

consensusr <- lm(consensus ~ Lag(consensus), 1)
summary(consensusr)
##
actualgdp <-
  
  

## consensus forecast error

cferror05 <- actualinf[1] - cons05
cferror05


