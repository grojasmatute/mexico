
## inflation one year ahead 


##2005

inf205_id2 <- subset(newdf,inft == "infgen_a06" & variable =="infgentmas1" &  analyst == 4 )
inf205_id2


##df for only inflation of the current period
infdf0506 <- NULL # empty subset 
infpon0506 <- matrix() 
infidpoint0506 <- add.col(gy0506, infpon0506)
infidpoint0506


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf0506 <- subset(newdf,inft == "infgen_a06" & variable =="infgentmas1" & analyst == t )
infidpoint0506 <- add.col(infidpoint0506,infdf0506[,5]) } 
infidpoint0506


infidpoint0506_0 <- infidpoint0506
infidpoint0506_0[is.na(infidpoint0506_0)] <- 0
infidpoint0506_0


infupdate0506_0 <- matrix(0, nrow = nrow(infidpoint0506_0 ), ncol = ncol(infidpoint0506_0 ) )
for (j in 2:ncol(infidpoint0506_0 )) {for (i in 2:nrow(infidpoint0506_0)) { infupdate0506_0[i,j] <- ifelse (infidpoint0506_0[i,j] == infidpoint0506_0[i-1,j],0,1) & (infidpoint0506_0[i,j]!=0)  }}
infupdate0506_0

## create a new column with proportion of updates 
infupdaters0506_0 <- numeric(nrow(infidpoint0506_0))
for(t in 2:nrow(infidpoint0506_0 )) {infupdaters0506_0[t] <- sum(infupdate0506_0[t,2:ncol(infupdate0506_0)])}
infupdaters0506_0 ## total of updaters

## participants
inftotal0506 <- numeric(12)
infparti0506<- matrix(0, nrow = nrow(infidpoint0506_0 ), ncol = ncol(infidpoint0506_0 ) )
for (j in 2:ncol(infidpoint0506_0)) {for (i in 1:nrow(infidpoint0506_0 )) { infparti0506[i,j] <- ifelse (infidpoint0506_0[i,j] != 0,1,0) }}
for(t in 1:nrow(infidpoint0506_0)) {inftotal0506[t] <- sum(infparti0506[t,2:ncol(infparti0506)])}
inftotal0506

infupdaters0506_0 <- infupdaters0506_0[2:12]/inftotal0506[2:12]
infupdaters0506_0

##2006-07

inf206_id2 <- subset(newdf,inft == "infgen_a07" & variable =="infgentmas1" &  analyst == 4 )
inf206_id2


##df for only inflation of the current period
infdf0607 <- NULL # empty subset 
infpon0607 <- matrix() 
infidpoint0607 <- add.col(gy0607, infpon0607)
infidpoint0607


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf0607 <- subset(newdf,inft == "infgen_a07" & variable =="infgentmas1" & analyst == t )
infidpoint0607 <- add.col(infidpoint0607,infdf0607[,5]) } 
infidpoint0607


infidpoint0607_0 <- infidpoint0607
infidpoint0607_0[is.na(infidpoint0607_0)] <- 0
infidpoint0607_0


infupdate0607_0 <- matrix(0, nrow = nrow(infidpoint0607_0 ), ncol = ncol(infidpoint0607_0 ) )
for (j in 2:ncol(infidpoint0607_0 )) {for (i in 2:nrow(infidpoint0607_0)) { infupdate0607_0[i,j] <- ifelse (infidpoint0607_0[i,j] == infidpoint0607_0[i-1,j],0,1) & (infidpoint0607_0[i,j]!=0)  }}
infupdate0607_0

## create a new column with proportion of updates 
infupdaters0607_0 <- numeric(nrow(infidpoint0607_0))
for(t in 2:nrow(infidpoint0607_0 )) {infupdaters0607_0[t] <- sum(infupdate0607_0[t,2:ncol(infupdate0607_0)])}
infupdaters0607_0 ## total of updaters

## participants
inftotal0607 <- numeric(12)
infparti0607<- matrix(0, nrow = nrow(infidpoint0607_0 ), ncol = ncol(infidpoint0607_0 ) )
for (j in 2:ncol(infidpoint0607_0)) {for (i in 1:nrow(infidpoint0607_0 )) { infparti0607[i,j] <- ifelse (infidpoint0607_0[i,j] != 0,1,0) }}
for(t in 1:nrow(infidpoint0607_0)) {inftotal0607[t] <- sum(infparti0607[t,2:ncol(infparti0607)])}
inftotal0607

infupdaters0607_0 <- infupdaters0607_0[2:12]/inftotal0607[2:12]
infupdaters0607_0

##2007-08

inf207_id2 <- subset(newdf,inft == "infgen_a08" & variable =="infgentmas1" &  analyst == 4 )
inf207_id2


##df for only inflation of the current period
infdf0708 <- NULL # empty subset 
infpon0708 <- matrix() 
infidpoint0708 <- add.col(gy0708, infpon0708)
infidpoint0708


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf0708 <- subset(newdf,inft == "infgen_a08" & variable =="infgentmas1" & analyst == t )
infidpoint0708 <- add.col(infidpoint0708,infdf0708[,5]) } 
infidpoint0708


infidpoint0708_0 <- infidpoint0708
infidpoint0708_0[is.na(infidpoint0708_0)] <- 0
infidpoint0708_0


infupdate0708_0 <- matrix(0, nrow = nrow(infidpoint0708_0 ), ncol = ncol(infidpoint0708_0 ) )
for (j in 2:ncol(infidpoint0708_0 )) {for (i in 2:nrow(infidpoint0708_0)) { infupdate0708_0[i,j] <- ifelse (infidpoint0708_0[i,j] == infidpoint0708_0[i-1,j],0,1) & (infidpoint0708_0[i,j]!=0)  }}
infupdate0708_0

## create a new column with proportion of updates 
infupdaters0708_0 <- numeric(nrow(infidpoint0708_0))
for(t in 2:nrow(infidpoint0708_0 )) {infupdaters0708_0[t] <- sum(infupdate0708_0[t,2:ncol(infupdate0708_0)])}
infupdaters0708_0 ## total of updaters

## participants
inftotal0708 <- numeric(12)
infparti0708<- matrix(0, nrow = nrow(infidpoint0708_0 ), ncol = ncol(infidpoint0708_0 ) )
for (j in 2:ncol(infidpoint0708_0)) {for (i in 1:nrow(infidpoint0708_0 )) { infparti0708[i,j] <- ifelse (infidpoint0708_0[i,j] != 0,1,0) }}
for(t in 1:nrow(infidpoint0708_0)) {inftotal0708[t] <- sum(infparti0708[t,2:ncol(infparti0708)])}
inftotal0708

infupdaters0708_0 <- infupdaters0708_0[2:12]/inftotal0708[2:12]
infupdaters0708_0

##2008-09

inf208_id2 <- subset(newdf,inft == "infgen_a09" & variable =="infgentmas1" &  analyst == 4 )
inf208_id2


##df for only inflation of the current period
infdf0809 <- NULL # empty subset 
infpon0809 <- matrix() 
infidpoint0809 <- add.col(gy0809, infpon0809)
infidpoint0809


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf0809 <- subset(newdf,inft == "infgen_a09" & variable =="infgentmas1" & analyst == t )
infidpoint0809 <- add.col(infidpoint0809,infdf0809[,5]) } 
infidpoint0809


infidpoint0809_0 <- infidpoint0809
infidpoint0809_0[is.na(infidpoint0809_0)] <- 0
infidpoint0809_0


infupdate0809_0 <- matrix(0, nrow = nrow(infidpoint0809_0 ), ncol = ncol(infidpoint0809_0 ) )
for (j in 2:ncol(infidpoint0809_0 )) {for (i in 2:nrow(infidpoint0809_0)) { infupdate0809_0[i,j] <- ifelse (infidpoint0809_0[i,j] == infidpoint0809_0[i-1,j],0,1) & (infidpoint0809_0[i,j]!=0)  }}
infupdate0809_0

## create a new column with proportion of updates 
infupdaters0809_0 <- numeric(nrow(infidpoint0809_0))
for(t in 2:nrow(infidpoint0809_0 )) {infupdaters0809_0[t] <- sum(infupdate0809_0[t,2:ncol(infupdate0809_0)])}
infupdaters0809_0 ## total of updaters

## participants
inftotal0809 <- numeric(12)
infparti0809<- matrix(0, nrow = nrow(infidpoint0809_0 ), ncol = ncol(infidpoint0809_0 ) )
for (j in 2:ncol(infidpoint0809_0)) {for (i in 1:nrow(infidpoint0809_0 )) { infparti0809[i,j] <- ifelse (infidpoint0809_0[i,j] != 0,1,0) }}
for(t in 1:nrow(infidpoint0809_0)) {inftotal0809[t] <- sum(infparti0809[t,2:ncol(infparti0809)])}
inftotal0809

infupdaters0809_0 <- infupdaters0809_0[2:12]/inftotal0809[2:12]
infupdaters0809_0


##2009-10

inf209_id2 <- subset(newdf,inft == "infgen_a10" & variable =="infgentmas1" &  analyst == 4 )
inf209_id2


##df for only inflation of the current period
infdf0910 <- NULL # empty subset 
infpon0910 <- matrix() 
infidpoint0910 <- add.col(gy0910, infpon0910)
infidpoint0910


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf0910 <- subset(newdf,inft == "infgen_a10" & variable =="infgentmas1" & analyst == t )
infidpoint0910 <- add.col(infidpoint0910,infdf0910[,5]) } 
infidpoint0910


infidpoint0910_0 <- infidpoint0910
infidpoint0910_0[is.na(infidpoint0910_0)] <- 0
infidpoint0910_0


infupdate0910_0 <- matrix(0, nrow = nrow(infidpoint0910_0 ), ncol = ncol(infidpoint0910_0 ) )
for (j in 2:ncol(infidpoint0910_0 )) {for (i in 2:nrow(infidpoint0910_0)) { infupdate0910_0[i,j] <- ifelse (infidpoint0910_0[i,j] == infidpoint0910_0[i-1,j],0,1) & (infidpoint0910_0[i,j]!=0)  }}
infupdate0910_0

## create a new column with proportion of updates 
infupdaters0910_0 <- numeric(nrow(infidpoint0910_0))
for(t in 2:nrow(infidpoint0910_0 )) {infupdaters0910_0[t] <- sum(infupdate0910_0[t,2:ncol(infupdate0910_0)])}
infupdaters0910_0 ## total of updaters

## participants
inftotal0910 <- numeric(12)
infparti0910<- matrix(0, nrow = nrow(infidpoint0910_0 ), ncol = ncol(infidpoint0910_0 ) )
for (j in 2:ncol(infidpoint0910_0)) {for (i in 1:nrow(infidpoint0910_0 )) { infparti0910[i,j] <- ifelse (infidpoint0910_0[i,j] != 0,1,0) }}
for(t in 1:nrow(infidpoint0910_0)) {inftotal0910[t] <- sum(infparti0910[t,2:ncol(infparti0910)])}
inftotal0910

infupdaters0910_0 <- infupdaters0910_0[2:12]/inftotal0910[2:12]
infupdaters0910_0

##2010-11

inf210_id2 <- subset(newdf,inft == "infgen_a11" & variable =="infgentmas1" &  analyst == 4 )
inf210_id2


##df for only inflation of the current period
infdf1011 <- NULL # empty subset 
infpon1011 <- matrix() 
infidpoint1011 <- add.col(gy1011, infpon1011)
infidpoint1011


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf1011 <- subset(newdf,inft == "infgen_a11" & variable =="infgentmas1" & analyst == t )
infidpoint1011 <- add.col(infidpoint1011,infdf1011[,5]) } 
infidpoint1011


infidpoint1011_0 <- infidpoint1011
infidpoint1011_0[is.na(infidpoint1011_0)] <- 0
infidpoint1011_0


infupdate1011_0 <- matrix(0, nrow = nrow(infidpoint1011_0 ), ncol = ncol(infidpoint1011_0 ) )
for (j in 2:ncol(infidpoint1011_0 )) {for (i in 2:nrow(infidpoint1011_0)) { infupdate1011_0[i,j] <- ifelse (infidpoint1011_0[i,j] == infidpoint1011_0[i-1,j],0,1) & (infidpoint1011_0[i,j]!=0)  }}
infupdate1011_0

## create a new column with proportion of updates 
infupdaters1011_0 <- numeric(nrow(infidpoint1011_0))
for(t in 2:nrow(infidpoint1011_0 )) {infupdaters1011_0[t] <- sum(infupdate1011_0[t,2:ncol(infupdate1011_0)])}
infupdaters1011_0 ## total of updaters

## participants
inftotal1011 <- numeric(12)
infparti1011<- matrix(0, nrow = nrow(infidpoint1011_0 ), ncol = ncol(infidpoint1011_0 ) )
for (j in 2:ncol(infidpoint1011_0)) {for (i in 1:nrow(infidpoint1011_0 )) { infparti1011[i,j] <- ifelse (infidpoint1011_0[i,j] != 0,1,0) }}
for(t in 1:nrow(infidpoint1011_0)) {inftotal1011[t] <- sum(infparti1011[t,2:ncol(infparti1011)])}
inftotal1011

infupdaters1011_0 <- infupdaters1011_0[2:12]/inftotal1011[2:12]
infupdaters1011_0

##2010-11

inf210_id2 <- subset(newdf,inft == "infgen_a11" & variable =="infgentmas1" &  analyst == 4 )
inf210_id2


##df for only inflation of the current period
infdf1011 <- NULL # empty subset 
infpon1011 <- matrix() 
infidpoint1011 <- add.col(gy1011, infpon1011)
infidpoint1011


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf1011 <- subset(newdf,inft == "infgen_a11" & variable =="infgentmas1" & analyst == t )
infidpoint1011 <- add.col(infidpoint1011,infdf1011[,5]) } 
infidpoint1011


infidpoint1011_0 <- infidpoint1011
infidpoint1011_0[is.na(infidpoint1011_0)] <- 0
infidpoint1011_0


infupdate1011_0 <- matrix(0, nrow = nrow(infidpoint1011_0 ), ncol = ncol(infidpoint1011_0 ) )
for (j in 2:ncol(infidpoint1011_0 )) {for (i in 2:nrow(infidpoint1011_0)) { infupdate1011_0[i,j] <- ifelse (infidpoint1011_0[i,j] == infidpoint1011_0[i-1,j],0,1) & (infidpoint1011_0[i,j]!=0)  }}
infupdate1011_0

## create a new column with proportion of updates 
infupdaters1011_0 <- numeric(nrow(infidpoint1011_0))
for(t in 2:nrow(infidpoint1011_0 )) {infupdaters1011_0[t] <- sum(infupdate1011_0[t,2:ncol(infupdate1011_0)])}
infupdaters1011_0 ## total of updaters

## participants
inftotal1011 <- numeric(12)
infparti1011<- matrix(0, nrow = nrow(infidpoint1011_0 ), ncol = ncol(infidpoint1011_0 ) )
for (j in 2:ncol(infidpoint1011_0)) {for (i in 1:nrow(infidpoint1011_0 )) { infparti1011[i,j] <- ifelse (infidpoint1011_0[i,j] != 0,1,0) }}
for(t in 1:nrow(infidpoint1011_0)) {inftotal1011[t] <- sum(infparti1011[t,2:ncol(infparti1011)])}
inftotal1011

infupdaters1011_0 <- infupdaters1011_0[2:12]/inftotal1011[2:12]
infupdaters1011_0


##2011-12

inf211_id2 <- subset(newdf,inft == "infgen_a12" & variable =="infgentmas1" &  analyst == 4 )
inf211_id2



##df for only inflation of the current period
infdf1112 <- NULL # empty subset 
infpon1112 <- matrix() 
infidpoint1112 <- add.col(gy1112, infpon1112)
infidpoint1112


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf1112 <- subset(newdf,inft == "infgen_a12" & variable =="infgentmas1" & analyst == t )
infidpoint1112 <- add.col(infidpoint1112,infdf1112[,5]) } 
infidpoint1112


infidpoint1112_0 <- infidpoint1112
infidpoint1112_0[is.na(infidpoint1112_0)] <- 0
infidpoint1112_0


infupdate1112_0 <- matrix(0, nrow = nrow(infidpoint1112_0 ), ncol = ncol(infidpoint1112_0 ) )
for (j in 2:ncol(infidpoint1112_0 )) {for (i in 2:nrow(infidpoint1112_0)) { infupdate1112_0[i,j] <- ifelse (infidpoint1112_0[i,j] == infidpoint1112_0[i-1,j],0,1) & (infidpoint1112_0[i,j]!=0)  }}
infupdate1112_0

## create a new column with proportion of updates 
infupdaters1112_0 <- numeric(nrow(infidpoint1112_0))
for(t in 2:nrow(infidpoint1112_0 )) {infupdaters1112_0[t] <- sum(infupdate1112_0[t,2:ncol(infupdate1112_0)])}
infupdaters1112_0 ## total of updaters

## participants
inftotal1112 <- numeric(12)
infparti1112<- matrix(0, nrow = nrow(infidpoint1112_0 ), ncol = ncol(infidpoint1112_0 ) )
for (j in 2:ncol(infidpoint1112_0)) {for (i in 1:nrow(infidpoint1112_0 )) { infparti1112[i,j] <- ifelse (infidpoint1112_0[i,j] != 0,1,0) }}
for(t in 1:nrow(infidpoint1112_0)) {inftotal1112[t] <- sum(infparti1112[t,2:ncol(infparti1112)])}
inftotal1112

infupdaters1112_0 <- infupdaters1112_0[2:12]/inftotal1112[2:12]
infupdaters1112_0

##2012-13

inf212_id2 <- subset(newdf,inft == "infgen_a13" & variable =="infgentmas1" &  analyst == 4 )
inf212_id2



##df for only inflation of the current period
infdf1213 <- NULL # empty subset 
infpon1213 <- matrix() 
infidpoint1213 <- add.col(gy1213, infpon1213)
infidpoint1213


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf1213 <- subset(newdf,inft == "infgen_a13" & variable =="infgentmas1" & analyst == t )
infidpoint1213 <- add.col(infidpoint1213,infdf1213[,5]) } 
infidpoint1213


infidpoint1213_0 <- infidpoint1213
infidpoint1213_0[is.na(infidpoint1213_0)] <- 0
infidpoint1213_0


infupdate1213_0 <- matrix(0, nrow = nrow(infidpoint1213_0 ), ncol = ncol(infidpoint1213_0 ) )
for (j in 2:ncol(infidpoint1213_0 )) {for (i in 2:nrow(infidpoint1213_0)) { infupdate1213_0[i,j] <- ifelse (infidpoint1213_0[i,j] == infidpoint1213_0[i-1,j],0,1) & (infidpoint1213_0[i,j]!=0)  }}
infupdate1213_0

## create a new column with proportion of updates 
infupdaters1213_0 <- numeric(nrow(infidpoint1213_0))
for(t in 2:nrow(infidpoint1213_0 )) {infupdaters1213_0[t] <- sum(infupdate1213_0[t,2:ncol(infupdate1213_0)])}
infupdaters1213_0 ## total of updaters

## participants
inftotal1213 <- numeric(12)
infparti1213<- matrix(0, nrow = nrow(infidpoint1213_0 ), ncol = ncol(infidpoint1213_0 ) )
for (j in 2:ncol(infidpoint1213_0)) {for (i in 1:nrow(infidpoint1213_0 )) { infparti1213[i,j] <- ifelse (infidpoint1213_0[i,j] != 0,1,0) }}
for(t in 1:nrow(infidpoint1213_0)) {inftotal1213[t] <- sum(infparti1213[t,2:ncol(infparti1213)])}
inftotal1213

infupdaters1213_0 <- infupdaters1213_0[2:12]/inftotal1213[2:12]
infupdaters1213_0

##2013-14

inf213_id2 <- subset(newdf,inft == "infgen_a14" & variable =="infgentmas1" &  analyst == 4 )
inf213_id2



##df for only inflation of the current period
infdf1314 <- NULL # empty subset 
infpon1314 <- matrix() 
infidpoint1314 <- add.col(gy1314, infpon1314)
infidpoint1314


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf1314 <- subset(newdf,inft == "infgen_a14" & variable =="infgentmas1" & analyst == t )
infidpoint1314 <- add.col(infidpoint1314,infdf1314[,5]) } 
infidpoint1314


infidpoint1314_0 <- infidpoint1314
infidpoint1314_0[is.na(infidpoint1314_0)] <- 0
infidpoint1314_0


infupdate1314_0 <- matrix(0, nrow = nrow(infidpoint1314_0 ), ncol = ncol(infidpoint1314_0 ) )
for (j in 2:ncol(infidpoint1314_0 )) {for (i in 2:nrow(infidpoint1314_0)) { infupdate1314_0[i,j] <- ifelse (infidpoint1314_0[i,j] == infidpoint1314_0[i-1,j],0,1) & (infidpoint1314_0[i,j]!=0)  }}
infupdate1314_0

## create a new column with proportion of updates 
infupdaters1314_0 <- numeric(nrow(infidpoint1314_0))
for(t in 2:nrow(infidpoint1314_0 )) {infupdaters1314_0[t] <- sum(infupdate1314_0[t,2:ncol(infupdate1314_0)])}
infupdaters1314_0 ## total of updaters

## participants
inftotal1314 <- numeric(12)
infparti1314<- matrix(0, nrow = nrow(infidpoint1314_0 ), ncol = ncol(infidpoint1314_0 ) )
for (j in 2:ncol(infidpoint1314_0)) {for (i in 1:nrow(infidpoint1314_0 )) { infparti1314[i,j] <- ifelse (infidpoint1314_0[i,j] != 0,1,0) }}
for(t in 1:nrow(infidpoint1314_0)) {inftotal1314[t] <- sum(infparti1314[t,2:ncol(infparti1314)])}
inftotal1314

infupdaters1314_0 <- infupdaters1314_0[2:12]/inftotal1314[2:12]
infupdaters1314_0

##2014-15

inf214_id2 <- subset(newdf,inft == "infgen_a15" & variable =="infgentmas1" &  analyst == 4 )
inf214_id2



##df for only inflation of the current period
infdf1415 <- NULL # empty subset 
infpon1415 <- matrix() 
infidpoint1415 <- add.col(gy1415, infpon1415)
infidpoint1415


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf1415 <- subset(newdf,inft == "infgen_a15" & variable =="infgentmas1" & analyst == t )
infidpoint1415 <- add.col(infidpoint1415,infdf1415[,5]) } 
infidpoint1415


infidpoint1415_0 <- infidpoint1415
infidpoint1415_0[is.na(infidpoint1415_0)] <- 0
infidpoint1415_0


infupdate1415_0 <- matrix(0, nrow = nrow(infidpoint1415_0 ), ncol = ncol(infidpoint1415_0 ) )
for (j in 2:ncol(infidpoint1415_0 )) {for (i in 2:nrow(infidpoint1415_0)) { infupdate1415_0[i,j] <- ifelse (infidpoint1415_0[i,j] == infidpoint1415_0[i-1,j],0,1) & (infidpoint1415_0[i,j]!=0)  }}
infupdate1415_0

## create a new column with proportion of updates 
infupdaters1415_0 <- numeric(nrow(infidpoint1415_0))
for(t in 2:nrow(infidpoint1415_0 )) {infupdaters1415_0[t] <- sum(infupdate1415_0[t,2:ncol(infupdate1415_0)])}
infupdaters1415_0 ## total of updaters

## participants
inftotal1415 <- numeric(12)
infparti1415<- matrix(0, nrow = nrow(infidpoint1415_0 ), ncol = ncol(infidpoint1415_0 ) )
for (j in 2:ncol(infidpoint1415_0)) {for (i in 1:nrow(infidpoint1415_0 )) { infparti1415[i,j] <- ifelse (infidpoint1415_0[i,j] != 0,1,0) }}
for(t in 1:nrow(infidpoint1415_0)) {inftotal1415[t] <- sum(infparti1415[t,2:ncol(infparti1415)])}
inftotal1415

infupdaters1415_0 <- infupdaters1415_0[2:12]/inftotal1415[2:12]
infupdaters1415_0

##2015-16

inf215_id2 <- subset(newdf,inft == "infgen_a16" & variable =="infgentmas1" &  analyst == 4 )
inf215_id2



##df for only inflation of the current period
infdf1516 <- NULL # empty subset 
infpon1516 <- matrix() 
infidpoint1516 <- add.col(gy1516, infpon1516)
infidpoint1516


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf1516 <- subset(newdf,inft == "infgen_a16" & variable =="infgentmas1" & analyst == t )
infidpoint1516 <- add.col(infidpoint1516,infdf1516[,5]) } 
infidpoint1516


infidpoint1516_0 <- infidpoint1516
infidpoint1516_0[is.na(infidpoint1516_0)] <- 0
infidpoint1516_0


infupdate1516_0 <- matrix(0, nrow = nrow(infidpoint1516_0 ), ncol = ncol(infidpoint1516_0 ) )
for (j in 2:ncol(infidpoint1516_0 )) {for (i in 2:nrow(infidpoint1516_0)) { infupdate1516_0[i,j] <- ifelse (infidpoint1516_0[i,j] == infidpoint1516_0[i-1,j],0,1) & (infidpoint1516_0[i,j]!=0)  }}
infupdate1516_0

## create a new column with proportion of updates 
infupdaters1516_0 <- numeric(nrow(infidpoint1516_0))
for(t in 2:nrow(infidpoint1516_0 )) {infupdaters1516_0[t] <- sum(infupdate1516_0[t,2:ncol(infupdate1516_0)])}
infupdaters1516_0 ## total of updaters

## participants
inftotal1516 <- numeric(12)
infparti1516<- matrix(0, nrow = nrow(infidpoint1516_0 ), ncol = ncol(infidpoint1516_0 ) )
for (j in 2:ncol(infidpoint1516_0)) {for (i in 1:nrow(infidpoint1516_0 )) { infparti1516[i,j] <- ifelse (infidpoint1516_0[i,j] != 0,1,0) }}
for(t in 1:nrow(infidpoint1516_0)) {inftotal1516[t] <- sum(infparti1516[t,2:ncol(infparti1516)])}
inftotal1516

infupdaters1516_0 <- infupdaters1516_0[2:12]/inftotal1516[2:12]
infupdaters1516_0


##2016-17

inf216_id2 <- subset(newdf,inft == "infgen_a17" & variable =="infgentmas1" &  analyst == 4 )
inf216_id2



##df for only inflation of the current period
infdf1617 <- NULL # empty subset 
infpon1617 <- matrix() 
infidpoint1617 <- add.col(gy1617, infpon1617)
infidpoint1617


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf1617 <- subset(newdf,inft == "infgen_a17" & variable =="infgentmas1" & analyst == t )
infidpoint1617 <- add.col(infidpoint1617,infdf1617[,5]) } 
infidpoint1617


infidpoint1617_0 <- infidpoint1617
infidpoint1617_0[is.na(infidpoint1617_0)] <- 0
infidpoint1617_0


infupdate1617_0 <- matrix(0, nrow = nrow(infidpoint1617_0 ), ncol = ncol(infidpoint1617_0 ) )
for (j in 2:ncol(infidpoint1617_0 )) {for (i in 2:nrow(infidpoint1617_0)) { infupdate1617_0[i,j] <- ifelse (infidpoint1617_0[i,j] == infidpoint1617_0[i-1,j],0,1) & (infidpoint1617_0[i,j]!=0)  }}
infupdate1617_0

## create a new column with proportion of updates 
infupdaters1617_0 <- numeric(nrow(infidpoint1617_0))
for(t in 2:nrow(infidpoint1617_0 )) {infupdaters1617_0[t] <- sum(infupdate1617_0[t,2:ncol(infupdate1617_0)])}
infupdaters1617_0 ## total of updaters

## participants
inftotal1617 <- numeric(12)
infparti1617<- matrix(0, nrow = nrow(infidpoint1617_0 ), ncol = ncol(infidpoint1617_0 ) )
for (j in 2:ncol(infidpoint1617_0)) {for (i in 1:nrow(infidpoint1617_0 )) { infparti1617[i,j] <- ifelse (infidpoint1617_0[i,j] != 0,1,0) }}
for(t in 1:nrow(infidpoint1617_0)) {inftotal1617[t] <- sum(infparti1617[t,2:ncol(infparti1617)])}
inftotal1617

infupdaters1617_0 <- infupdaters1617_0[2:12]/inftotal1617[2:12]
infupdaters1617_0


##2017-18

inf217_id2 <- subset(newdf,inft == "infgen_a18" & variable =="infgentmas1" &  analyst == 4 )
inf217_id2



##df for only inflation of the current period
infdf1718 <- NULL # empty subset 
infpon1718 <- matrix() 
infidpoint1718 <- add.col(gy1718, infpon1718)
infidpoint1718


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf1718 <- subset(newdf,inft == "infgen_a18" & variable =="infgentmas1" & analyst == t )
infidpoint1718 <- add.col(infidpoint1718,infdf1718[,5]) } 
infidpoint1718


infidpoint1718_0 <- infidpoint1718
infidpoint1718_0[is.na(infidpoint1718_0)] <- 0
infidpoint1718_0


infupdate1718_0 <- matrix(0, nrow = nrow(infidpoint1718_0 ), ncol = ncol(infidpoint1718_0 ) )
for (j in 2:ncol(infidpoint1718_0 )) {for (i in 2:nrow(infidpoint1718_0)) { infupdate1718_0[i,j] <- ifelse (infidpoint1718_0[i,j] == infidpoint1718_0[i-1,j],0,1) & (infidpoint1718_0[i,j]!=0)  }}
infupdate1718_0

## create a new column with proportion of updates 
infupdaters1718_0 <- numeric(nrow(infidpoint1718_0))
for(t in 2:nrow(infidpoint1718_0 )) {infupdaters1718_0[t] <- sum(infupdate1718_0[t,2:ncol(infupdate1718_0)])}
infupdaters1718_0 ## total of updaters

## participants
inftotal1718 <- numeric(12)
infparti1718<- matrix(0, nrow = nrow(infidpoint1718_0 ), ncol = ncol(infidpoint1718_0 ) )
for (j in 2:ncol(infidpoint1718_0)) {for (i in 1:nrow(infidpoint1718_0 )) { infparti1718[i,j] <- ifelse (infidpoint1718_0[i,j] != 0,1,0) }}
for(t in 1:nrow(infidpoint1718_0)) {inftotal1718[t] <- sum(infparti1718[t,2:ncol(infparti1718)])}
inftotal1718

infupdaters1718_0 <- infupdaters1718_0[2:12]/inftotal1718[2:12]
infupdaters1718_0

##2018-19

inf218_id2 <- subset(newdf,inft == "infgen_a19" & variable =="infgentmas1" &  analyst == 4 )
inf218_id2



##df for only inflation of the current period
infdf1819 <- NULL # empty subset 
infpon1819 <- matrix() 
infidpoint1819 <- add.col(gy1819, infpon1819)
infidpoint1819


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf1819 <- subset(newdf,inft == "infgen_a19" & variable =="infgentmas1" & analyst == t )
infidpoint1819 <- add.col(infidpoint1819,infdf1819[,5]) } 
infidpoint1819


infidpoint1819_0 <- infidpoint1819
infidpoint1819_0[is.na(infidpoint1819_0)] <- 0
infidpoint1819_0


infupdate1819_0 <- matrix(0, nrow = nrow(infidpoint1819_0 ), ncol = ncol(infidpoint1819_0 ) )
for (j in 2:ncol(infidpoint1819_0 )) {for (i in 2:nrow(infidpoint1819_0)) { infupdate1819_0[i,j] <- ifelse (infidpoint1819_0[i,j] == infidpoint1819_0[i-1,j],0,1) & (infidpoint1819_0[i,j]!=0)  }}
infupdate1819_0

## create a new column with proportion of updates 
infupdaters1819_0 <- numeric(nrow(infidpoint1819_0))
for(t in 2:nrow(infidpoint1819_0 )) {infupdaters1819_0[t] <- sum(infupdate1819_0[t,2:ncol(infupdate1819_0)])}
infupdaters1819_0 ## total of updaters

## participants
inftotal1819 <- numeric(11)
infparti1819<- matrix(0, nrow = nrow(infidpoint1819_0 ), ncol = ncol(infidpoint1819_0 ) )
for (j in 2:ncol(infidpoint1819_0)) {for (i in 1:nrow(infidpoint1819_0 )) { infparti1819[i,j] <- ifelse (infidpoint1718_0[i,j] != 0,1,0) }}
for(t in 1:nrow(infidpoint1819_0)) {inftotal1819[t] <- sum(infparti1819[t,2:ncol(infparti1819)])}
inftotal1819

infupdaters1819_0 <- infupdaters1819_0[2:11]/inftotal1819[2:11]
infupdaters1819_0


inf2updaters <- c(infupdaters0506_0, infupdaters0607_0, infupdaters0708_0, infupdaters0809_0, infupdaters0910_0, infupdaters1011_0, infupdaters1112_0, infupdaters1213_0, infupdaters1314_0, infupdaters1415_0, infupdaters1516_0, infupdaters1617_0, infupdaters1718_0, infupdaters1819_0 )

## Consensus 
inf2cons05 <- numeric(12)
inf2cons06 <- numeric(12)
inf2cons07 <- numeric(12)
inf2cons08 <- numeric(12)
inf2cons09 <- numeric(12)
inf2cons10 <- numeric(12)
inf2cons11 <- numeric(12)
inf2cons12 <- numeric(12)
inf2cons13 <- numeric(12)
inf2cons14 <- numeric(12)
inf2cons15 <- numeric(12)
inf2cons16 <- numeric(12)
inf2cons17 <- numeric(12)
inf2cons18 <- numeric(11)


infidpointNA0506 <- infidpoint0506[ , colSums(is.na(infidpoint0506)) == 0]
for(t in 1:nrow(infidpointNA0506)) {inf2cons05[t] <- sum(infidpointNA0506[t,2:ncol(infidpointNA0506)])/(ncol(infidpointNA0506)-1)}
inf2cons05

infidpointNA0607 <- infidpoint0607[ , colSums(is.na(infidpoint0607)) == 0]
for(t in 1:nrow(infidpointNA0607)) {inf2cons06[t] <- sum(infidpointNA0607[t,2:ncol(infidpointNA0607)])/(ncol(infidpointNA0607)-1)}
inf2cons06

infidpointNA0708 <- infidpoint0708[ , colSums(is.na(infidpoint0708)) == 0]
for(t in 1:nrow(infidpointNA0708)) {inf2cons07[t] <- sum(infidpointNA0708[t,2:ncol(infidpointNA0708)])/(ncol(infidpointNA0708)-1)}
inf2cons07

infidpointNA0809 <- infidpoint0809[ , colSums(is.na(infidpoint0809)) == 0]
for(t in 1:nrow(infidpointNA0809)) {inf2cons08[t] <- sum(infidpointNA0809[t,2:ncol(infidpointNA0809)])/(ncol(infidpointNA0809)-1)}
inf2cons08

infidpointNA0910 <- infidpoint0910[ , colSums(is.na(infidpoint0910)) == 0]
for(t in 1:nrow(infidpointNA0910)) {inf2cons09[t] <- sum(infidpointNA0910[t,2:ncol(infidpointNA0910)])/(ncol(infidpointNA0910)-1)}
inf2cons09

infidpointNA1011 <- infidpoint1011[ , colSums(is.na(infidpoint1011)) == 0]
for(t in 1:nrow(infidpointNA1011)) {inf2cons10[t] <- sum(infidpointNA1011[t,2:ncol(infidpointNA1011)])/(ncol(infidpointNA1011)-1)}
inf2cons10

infidpointNA1112 <- infidpoint1112[ , colSums(is.na(infidpoint1112)) == 0]
for(t in 1:nrow(infidpointNA1112)) {inf2cons11[t] <- sum(infidpointNA1112[t,2:ncol(infidpointNA1112)])/(ncol(infidpointNA1112)-1)}
inf2cons11

infidpointNA1213 <- infidpoint1213[ , colSums(is.na(infidpoint1213)) == 0]
for(t in 1:nrow(infidpointNA1213)) {inf2cons12[t] <- sum(infidpointNA1213[t,2:ncol(infidpointNA1213)])/(ncol(infidpointNA1213)-1)}
inf2cons12

infidpointNA1314 <- infidpoint1314[ , colSums(is.na(infidpoint1314)) == 0]
for(t in 1:nrow(infidpointNA1314)) {inf2cons13[t] <- sum(infidpointNA1314[t,2:ncol(infidpointNA1314)])/(ncol(infidpointNA1314)-1)}
inf2cons13


