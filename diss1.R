library(ggplot2)
library(forecast)
library(tseries)


library(ggfortify)
library(plotly)
library(timeDate)
library(timeSeries)
library(fpp2)
library(Hmisc)


## set directory 

setwd("/Users/gustavorojasmatute/Documents/AmericanU/AU/Dissertation")
df.foremx <- read.csv("ForecastMx.csv") ## read microdata 

fecha <- df.foremx[,1]
variable <- df.foremx[,3]
analyst <- df.foremx[,6]
pointf <- df.foremx[,7]
inft <- df.foremx[,2]

## short data frame 
newdf <- data.frame(fecha, inft, variable, analyst, pointf)
head(newdf)

inf99_id2 <- subset(newdf,inft == "infgen_a99" & variable == "infgent" & analyst == 4 )
inf99_id2
inf99_id3 <- subset(newdf,inft == "infgen_a99" & analyst == 90)
inf99_id5 <- subset(newdf,inft == "infgen_a99" & analyst == 4 )
inf99_id5

y <- as.Date(seq(ISOdate(1999,1,1), by = "month", length.out = 11), format = "%d/%m/%Y")
y <- data.frame(inf99_id2[,1])
y
x <- inf99_id3[,5]

## function to add columns 
add.col<-function(df, new.col) {n.row<-dim(df)[1]
length(new.col)<-n.row
cbind(df, new.col)
}

### 1999 

##df for only inflation of the current period
infdf <- NULL # empty subset 
ipon <- matrix() 
idpoint <- add.col(y, ipon)
idpoint


## data frame with point forecast from analyst 1 to 91
for(t in 0:91) {infdf <- subset(newdf,inft == "infgen_a99" & variable == "infgent" & analyst == t )
idpoint <- add.col(idpoint,infdf[,5]) } 
idpoint
idpointna <- idpoint[ , colSums(is.na(idpoint)) == 0]  ## eliminate NA
nrow(idpointna ) ## number of updaters
ncol(idpointna )
## updates ##
update <- matrix(0, nrow = nrow(idpointna ), ncol = ncol(idpointna ) )
for (j in 2:ncol(idpointna )) {for (i in 2:nrow(idpointna )) { update[i,j] <- ifelse (idpointna[i,j] == idpointna[i-1,j],0,1) }}
update


## create a new column with proportion of updates 
updaters <- numeric(nrow(idpointna ))
for(t in 2:nrow(idpointna )) {updaters[t] <- sum(update[t, 2:ncol(update)])/(ncol(update)-1)}
updaters
prop_upd <- add.col(y,updaters)
prop_upd <- ts(updaters, frequency = 12, start=c(1999, 1), end=c(1999, 11) )
##### ####

### 2000 ### 

inf00_id2 <- subset(newdf,inft == "infgen_a00" & variable =="infgent" &  analyst == 4 )
inf00_id2



y00 <- data.frame(inf00_id2[,1])

##df for only inflation of the current period
infdf00 <- NULL # empty subset 
ipon00 <- matrix() 
idpoint00 <- add.col(y00, ipon00)
idpoint00


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf00 <- subset(newdf,inft == "infgen_a00" & variable =="infgent"  & analyst == t )
idpoint00 <- add.col(idpoint00,infdf00[,5]) } 
idpoint00
idpointna00 <- idpoint00[ , colSums(is.na(idpoint00)) == 0]  ## eliminate NA
nrow(idpointna00 ) ## number of updaters
ncol(idpointna00 )
## updates ##
update00 <- matrix(0, nrow = nrow(idpointna00 ), ncol = ncol(idpointna00 ) )
for (j in 2:ncol(idpointna00 )) {for (i in 2:nrow(idpointna00 )) { update00[i,j] <- ifelse (idpointna00[i,j] == idpointna00[i-1,j],0,1) }}
update00


## create a new column with proportion of updates 
updaters00 <- numeric(nrow(idpointna00 ))
for(t in 2:nrow(idpointna00 )) {updaters00[t] <- sum(update00[t, 2:ncol(update00)])/(ncol(update00)-1)}
updaters00
prop_upd00 <- ts(updaters00, frequency = 12, start=c(2000, 1), end=c(2000, 10) )


### 2001 ##

inf01_id2 <- subset(newdf,inft == "infgen_a01" & variable =="infgent" &  analyst == 2 )
inf01_id2



y01 <- data.frame(inf01_id2[,1])

##df for only inflation of the current period
infdf01 <- NULL # empty subset 
ipon01 <- matrix() 
idpoint01 <- add.col(y01, ipon01)
idpoint01


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf01 <- subset(newdf,inft == "infgen_a01" & variable =="infgent" &analyst == t )
idpoint01 <- add.col(idpoint01,infdf01[,5]) } 
idpoint01
idpointna01 <- idpoint01[ , colSums(is.na(idpoint01)) == 0]  ## eliminate NA
nrow(idpointna01 ) ## number of updaters
ncol(idpointna01 )
## updates ##
update01 <- matrix(0, nrow = nrow(idpointna01 ), ncol = ncol(idpointna01 ) )
for (j in 2:ncol(idpointna01 )) {for (i in 2:nrow(idpointna01 )) { update01[i,j] <- ifelse (idpointna01[i,j] == idpointna01[i-1,j],0,1) }}
update01


## create a new column with proportion of updates 
updaters01 <- numeric(nrow(idpointna01 ))
for(t in 2:nrow(idpointna01 )) {updaters01[t] <- sum(update01[t, 2:ncol(update01)])/(ncol(update01)-1)}
updaters01
prop_upd01 <- ts(updaters01, frequency = 12, start=c(2001, 1), end=c(2001, 10) )



### 2002

inf02_id2 <- subset(newdf,inft == "infgen_a02" & variable =="infgent" &  analyst == 2 )
inf02_id2



y02 <- data.frame(inf02_id2[,1])

##df for only inflation of the current period
infdf02 <- NULL # empty subset 
ipon02 <- matrix() 
idpoint02 <- add.col(y02, ipon02)
idpoint02


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf02 <- subset(newdf,inft == "infgen_a02" & variable =="infgent" & analyst == t )
idpoint02 <- add.col(idpoint02,infdf02[,5]) } 
idpoint02
idpointna02 <- idpoint02[ , colSums(is.na(idpoint02)) == 0]  ## eliminate NA
nrow(idpointna02 ) ## number of updaters
ncol(idpointna02 )
## updates ##
update02 <- matrix(0, nrow = nrow(idpointna02 ), ncol = ncol(idpointna02 ) )
for (j in 2:ncol(idpointna02 )) {for (i in 2:nrow(idpointna02 )) { update02[i,j] <- ifelse (idpointna02[i,j] == idpointna02[i-1,j],0,1) }}
update02


## create a new column with proportion of updates 
updaters02 <- numeric(nrow(idpointna02 ))
for(t in 2:nrow(idpointna02 )) {updaters02[t] <- sum(update02[t, 2:ncol(update02)])/(ncol(update02)-1)}
updaters02
prop_upd02 <- ts(updaters02, frequency = 12, start=c(2002, 1), end=c(2002, 10) )

## 2003 ##


inf03_id2 <- subset(newdf,inft == "infgen_a03" & variable =="infgent" &  analyst == 2 )
inf03_id2



y03 <- data.frame(inf03_id2[,1])

##df for only inflation of the current period
infdf03 <- NULL # empty subset 
ipon03 <- matrix() 
idpoint03 <- add.col(y03, ipon03)
idpoint03


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf03 <- subset(newdf,inft == "infgen_a03" & variable =="infgent" & analyst == t )
idpoint03 <- add.col(idpoint03,infdf03[,5]) } 
idpoint03
idpointna03 <- idpoint03[ , colSums(is.na(idpoint03)) == 0]  ## eliminate NA
nrow(idpointna03 ) ## number of updaters
ncol(idpointna03 )
## updates ##
update03 <- matrix(0, nrow = nrow(idpointna03 ), ncol = ncol(idpointna03 ) )
for (j in 2:ncol(idpointna03 )) {for (i in 2:nrow(idpointna03 )) { update03[i,j] <- ifelse (idpointna03[i,j] == idpointna03[i-1,j],0,1) }}
update03


## create a new column with proportion of updates 
updaters03 <- numeric(nrow(idpointna03 ))
for(t in 2:nrow(idpointna03 )) {updaters03[t] <- sum(update03[t, 2:ncol(update03)])/(ncol(update03)-1)}
updaters03


## 2004 ##



inf04_id2 <- subset(newdf,inft == "infgen_a04" & variable =="infgent" &  analyst == 4 )
inf04_id2



y04 <- data.frame(inf04_id2[,1])

##df for only inflation of the current period
infdf04 <- NULL # empty subset 
ipon04 <- matrix() 
idpoint04 <- add.col(y04, ipon04)
idpoint04


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf04 <- subset(newdf,inft == "infgen_a04" & variable =="infgent" & analyst == t )
idpoint04 <- add.col(idpoint04,infdf04[,5]) } 
idpoint04
idpointna04 <- idpoint04[ , colSums(is.na(idpoint04)) == 0]  ## eliminate NA
nrow(idpointna04 ) ## number of updaters
ncol(idpointna04 )
## updates ##
update04 <- matrix(0, nrow = nrow(idpointna04 ), ncol = ncol(idpointna04 ) )
for (j in 2:ncol(idpointna04 )) {for (i in 2:nrow(idpointna04 )) { update04[i,j] <- ifelse (idpointna04[i,j] == idpointna04[i-1,j],0,1) }}
update04


## create a new column with proportion of updates 
updaters04 <- numeric(nrow(idpointna04 ))
for(t in 2:nrow(idpointna04 )) {updaters04[t] <- sum(update04[t, 2:ncol(update04)])/(ncol(update04)-1)}
updaters04

### 2005 ###





inf05_id2 <- subset(newdf,inft == "infgen_a05" & variable =="infgent" &  analyst == 2 )
inf05_id2



y05 <- data.frame(inf05_id2[,1])

##df for only inflation of the current period
infdf05 <- NULL # empty subset 
ipon05 <- matrix() 
idpoint05 <- add.col(y05, ipon05)
idpoint05


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf05 <- subset(newdf,inft == "infgen_a05" & variable =="infgent" & analyst == t )
idpoint05 <- add.col(idpoint05,infdf05[,5]) } 
idpoint05


idpointna05 <- idpoint05[ , colSums(is.na(idpoint05)) == 0]  ## eliminate NA
nrow(idpointna05 ) ## number of updaters
ncol(idpointna05 )
idpointna05[,1:5]
update05[,1:5]
## updates ##
update05 <- matrix(0, nrow = nrow(idpointna05 ), ncol = ncol(idpointna05 ) )
for (j in 2:ncol(idpointna05 )) {for (i in 2:nrow(idpointna05 )) { update05[i,j] <- ifelse (idpointna05[i,j] == idpointna05[i-1,j],0,1) }}
update05[,1:5]
update05_0

## create a new column with proportion of updates 
updaters05 <- numeric(nrow(idpointna05 ))
for(t in 2:nrow(idpointna05 )) {updaters05[t] <- sum(update05[t, 2:ncol(update05)])/(ncol(update05)-1)}
updaters05

### 2005_0
## replacing NA with 0 to count all updaters
idpoint05_0 <- idpoint05

idpoint05_0[is.na(idpoint05_0)] <- 0

## updates ##
update05_0 <- matrix(0, nrow = nrow(idpoint05_0 ), ncol = ncol(idpoint05_0 ) )
for (j in 2:ncol(idpoint05_0 )) {for (i in 2:nrow(idpoint05_0 )) { update05_0[i,j] <- ifelse (idpoint05_0[i,j] == idpoint05_0[i-1,j],0,1) }}
update05_0

## create a new column with proportion of updates 
updaters05_0 <- numeric(nrow(idpoint05_0 ))
for(t in 2:nrow(idpoint05_0 )) {updaters05_0[t] <- sum(update05[t,2:ncol(update05)]) / sum(update05_0[t, 2:ncol(update05_0)])}
updaters05_0
updaters05


## 2006 ##


inf06_id2 <- subset(newdf,inft == "infgen_a06" & variable =="infgent" &  analyst == 2 )
inf06_id2



y06 <- data.frame(inf06_id2[,1])

##df for only inflation of the current period
infdf06 <- NULL # empty subset 
ipon06 <- matrix() 
idpoint06 <- add.col(y06, ipon06)
idpoint06


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf06 <- subset(newdf,inft == "infgen_a06" & variable =="infgent" & analyst == t )
idpoint06 <- add.col(idpoint06,infdf06[,5]) } 
idpoint06
idpointna06 <- idpoint06[ , colSums(is.na(idpoint06)) == 0]  ## eliminate NA
nrow(idpointna06 ) ## number of updaters
ncol(idpointna06 )
## updates ##
update06 <- matrix(0, nrow = nrow(idpointna06 ), ncol = ncol(idpointna06 ) )
for (j in 2:ncol(idpointna06 )) {for (i in 2:nrow(idpointna06 )) { update06[i,j] <- ifelse (idpointna06[i,j] == idpointna06[i-1,j],0,1) }}
update06


## create a new column with proportion of updates 
updaters06 <- numeric(nrow(idpointna06 ))
for(t in 2:nrow(idpointna06 )) {updaters06[t] <- sum(update06[t, 2:ncol(update06)])/(ncol(update06)-1)}
updaters06

## 2007 ## 


inf07_id2 <- subset(newdf,inft == "infgen_a07" & variable =="infgent" &  analyst == 2 )
inf07_id2



y07 <- data.frame(inf07_id2[,1])

##df for only inflation of the current period
infdf07 <- NULL # empty subset 
ipon07 <- matrix() 
idpoint07 <- add.col(y07, ipon07)
idpoint07


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf07 <- subset(newdf,inft == "infgen_a07" & variable =="infgent" & analyst == t )
idpoint07 <- add.col(idpoint07,infdf07[,5]) } 
idpoint07
idpointna07 <- idpoint07[ , colSums(is.na(idpoint07)) == 0]  ## eliminate NA
nrow(idpointna07 ) ## number of updaters
ncol(idpointna07 )
## updates ##
update07 <- matrix(0, nrow = nrow(idpointna07 ), ncol = ncol(idpointna07 ) )
for (j in 2:ncol(idpointna07 )) {for (i in 2:nrow(idpointna07 )) { update07[i,j] <- ifelse (idpointna07[i,j] == idpointna07[i-1,j],0,1) }}
update07


## create a new column with proportion of updates 
updaters07 <- numeric(nrow(idpointna07 ))
for(t in 2:nrow(idpointna07 )) {updaters07[t] <- sum(update07[t, 2:ncol(update07)])/(ncol(update07)-1)}
updaters07

## 2008 ##


inf08_id2 <- subset(newdf,inft == "infgen_a08" & variable =="infgent" &  analyst == 2 )
inf08_id2



y08 <- data.frame(inf08_id2[,1])

##df for only inflation of the current period
infdf08 <- NULL # empty subset 
ipon08 <- matrix() 
idpoint08 <- add.col(y08, ipon08)
idpoint08


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf08 <- subset(newdf,inft == "infgen_a08" & variable =="infgent" & analyst == t )
idpoint08 <- add.col(idpoint08,infdf08[,5]) } 
idpoint08
idpointna08 <- idpoint08[ , colSums(is.na(idpoint08)) == 0]  ## eliminate NA
nrow(idpointna08 ) ## number of updaters
ncol(idpointna08 )
## updates ##
update08 <- matrix(0, nrow = nrow(idpointna08 ), ncol = ncol(idpointna08 ) )
for (j in 2:ncol(idpointna08 )) {for (i in 2:nrow(idpointna08 )) { update08[i,j] <- ifelse (idpointna08[i,j] == idpointna08[i-1,j],0,1) }}
update08


## create a new column with proportion of updates 
updaters08 <- numeric(nrow(idpointna08 ))
for(t in 2:nrow(idpointna08 )) {updaters08[t] <- sum(update08[t, 2:ncol(update08)])/(ncol(update08)-1)}
updaters08

## 2009 ##

inf09_id2 <- subset(newdf,inft == "infgen_a09" & variable =="infgent" &  analyst == 4 )
inf09_id2



y09 <- data.frame(inf09_id2[,1])

##df for only inflation of the current period
infdf09 <- NULL # empty subset 
ipon09 <- matrix() 
idpoint09 <- add.col(y09, ipon09)
idpoint09


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf09 <- subset(newdf,inft == "infgen_a09" & variable =="infgent" & analyst == t )
idpoint09 <- add.col(idpoint09,infdf09[,5]) } 
idpoint09
idpointna09 <- idpoint09[ , colSums(is.na(idpoint09)) == 0]  ## eliminate NA
nrow(idpointna09 ) ## number of updaters
ncol(idpointna09 )
## updates ##
update09 <- matrix(0, nrow = nrow(idpointna09 ), ncol = ncol(idpointna09 ) )
for (j in 2:ncol(idpointna09 )) {for (i in 2:nrow(idpointna09 )) { update09[i,j] <- ifelse (idpointna09[i,j] == idpointna09[i-1,j],0,1) }}
update09


## create a new column with proportion of updates 
updaters09 <- numeric(nrow(idpointna09 ))
for(t in 2:nrow(idpointna09 )) {updaters09[t] <- sum(update09[t, 2:ncol(update09)])/(ncol(update09)-1)}
updaters09


## 2010 ##

inf10_id2 <- subset(newdf,inft == "infgen_a10" & variable =="infgent" &  analyst == 4 )
inf10_id2



y10 <- data.frame(inf10_id2[,1])

##df for only inflation of the current period
infdf10 <- NULL # empty subset 
ipon10 <- matrix() 
idpoint10 <- add.col(y10, ipon10)
idpoint10


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf10 <- subset(newdf,inft == "infgen_a10" & variable =="infgent" & analyst == t )
idpoint10 <- add.col(idpoint10,infdf10[,5]) } 
idpoint10
idpointna10 <- idpoint10[ , colSums(is.na(idpoint10)) == 0]  ## eliminate NA
nrow(idpointna10 ) ## number of updaters
ncol(idpointna10 )
## updates ##
update10 <- matrix(0, nrow = nrow(idpointna10 ), ncol = ncol(idpointna10 ) )
for (j in 2:ncol(idpointna10 )) {for (i in 2:nrow(idpointna10 )) { update10[i,j] <- ifelse (idpointna10[i,j] == idpointna10[i-1,j],0,1) }}
update10


## create a new column with proportion of updates 
updaters10 <- numeric(nrow(idpointna10 ))
for(t in 2:nrow(idpointna10 )) {updaters10[t] <- sum(update10[t, 2:ncol(update10)])/(ncol(update10)-1)}
updaters10


## 2011 ##


inf11_id2 <- subset(newdf,inft == "infgen_a11" & variable =="infgent" &  analyst == 4 )
inf11_id2



y11 <- data.frame(inf11_id2[,1])

##df for only inflation of the current period
infdf11 <- NULL # empty subset 
ipon11 <- matrix() 
idpoint11 <- add.col(y11, ipon11)
idpoint11


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf11 <- subset(newdf,inft == "infgen_a11" & variable =="infgent" & analyst == t )
idpoint11 <- add.col(idpoint11,infdf11[,5]) } 
idpoint11
idpointna11 <- idpoint11[ , colSums(is.na(idpoint11)) == 0]  ## eliminate NA
nrow(idpointna11 ) ## number of updaters
ncol(idpointna11 )
## updates ##
update11 <- matrix(0, nrow = nrow(idpointna11 ), ncol = ncol(idpointna11 ) )
for (j in 2:ncol(idpointna11 )) {for (i in 2:nrow(idpointna11 )) { update11[i,j] <- ifelse (idpointna11[i,j] == idpointna11[i-1,j],0,1) }}
update11


## create a new column with proportion of updates 
updaters11 <- numeric(nrow(idpointna11 ))
for(t in 2:nrow(idpointna11 )) {updaters11[t] <- sum(update11[t, 2:ncol(update11)])/(ncol(update11)-1)}
updaters11


## 2012 ## 


inf12_id2 <- subset(newdf,inft == "infgen_a12" & variable =="infgent" &  analyst == 4 )
inf12_id2



y12 <- data.frame(inf12_id2[,1])

##df for only inflation of the current period
infdf12 <- NULL # empty subset 
ipon12 <- matrix() 
idpoint12 <- add.col(y12, ipon12)
idpoint12


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf12 <- subset(newdf,inft == "infgen_a12" & variable =="infgent" & analyst == t )
idpoint12 <- add.col(idpoint12,infdf12[,5]) } 
idpoint12
idpointna12 <- idpoint12[ , colSums(is.na(idpoint12)) == 0]  ## eliminate NA
nrow(idpointna12 ) ## number of updaters
ncol(idpointna12 )
## updates ##
update12 <- matrix(0, nrow = nrow(idpointna12 ), ncol = ncol(idpointna12 ) )
for (j in 2:ncol(idpointna12 )) {for (i in 2:nrow(idpointna12 )) { update12[i,j] <- ifelse (idpointna12[i,j] == idpointna12[i-1,j],0,1) }}
update12


## create a new column with proportion of updates 
updaters12 <- numeric(nrow(idpointna12 ))
for(t in 2:nrow(idpointna12 )) {updaters12[t] <- sum(update12[t, 2:ncol(update12)])/(ncol(update12)-1)}
updaters12

## 2013 ##

inf13_id2 <- subset(newdf,inft == "infgen_a13" & variable =="infgent" &  analyst == 4 )
inf13_id2



y13 <- data.frame(inf13_id2[,1])

##df for only inflation of the current period
infdf13 <- NULL # empty subset 
ipon13 <- matrix() 
idpoint13 <- add.col(y13, ipon13)
idpoint13


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf13 <- subset(newdf,inft == "infgen_a13" & variable =="infgent" & analyst == t )
idpoint13 <- add.col(idpoint13,infdf13[,5]) } 
idpoint13
idpointna13 <- idpoint13[ , colSums(is.na(idpoint13)) == 0]  ## eliminate NA
nrow(idpointna13 ) ## number of updaters
ncol(idpointna13 )
## updates ##
update13 <- matrix(0, nrow = nrow(idpointna13 ), ncol = ncol(idpointna13 ) )
for (j in 2:ncol(idpointna13 )) {for (i in 2:nrow(idpointna13 )) { update13[i,j] <- ifelse (idpointna13[i,j] == idpointna13[i-1,j],0,1) }}
update13


## create a new column with proportion of updates 
updaters13 <- numeric(nrow(idpointna13 ))
for(t in 2:nrow(idpointna13 )) {updaters13[t] <- sum(update13[t, 2:ncol(update13)])/(ncol(update13)-1)}
updaters13

## 2014 ##

inf14_id2 <- subset(newdf,inft == "infgen_a14" & variable =="infgent" &  analyst == 4 )
inf14_id2



y14 <- data.frame(inf14_id2[,1])

##df for only inflation of the current period
infdf14 <- NULL # empty subset 
ipon14 <- matrix() 
idpoint14 <- add.col(y14, ipon14)
idpoint14


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf14 <- subset(newdf,inft == "infgen_a14" & variable =="infgent" & analyst == t )
idpoint14 <- add.col(idpoint14,infdf14[,5]) } 
idpoint14
idpointna14 <- idpoint14[ , colSums(is.na(idpoint14)) == 0]  ## eliminate NA
nrow(idpointna14 ) ## number of updaters
ncol(idpointna14 )
## updates ##
update14 <- matrix(0, nrow = nrow(idpointna14 ), ncol = ncol(idpointna14 ) )
for (j in 2:ncol(idpointna14 )) {for (i in 2:nrow(idpointna14 )) { update14[i,j] <- ifelse (idpointna14[i,j] == idpointna14[i-1,j],0,1) }}
update14


## create a new column with proportion of updates 
updaters14 <- numeric(nrow(idpointna14 ))
for(t in 2:nrow(idpointna14 )) {updaters14[t] <- sum(update14[t, 2:ncol(update14)])/(ncol(update14)-1)}
updaters14


## 2015

inf15_id2 <- subset(newdf,inft == "infgen_a15" & variable =="infgent" &  analyst == 4 )
inf15_id2



y15 <- data.frame(inf15_id2[,1])

##df for only inflation of the current period
infdf15 <- NULL # empty subset 
ipon15 <- matrix() 
idpoint15 <- add.col(y15, ipon15)
idpoint15


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf15 <- subset(newdf,inft == "infgen_a15" & variable =="infgent" & analyst == t )
idpoint15 <- add.col(idpoint15,infdf15[,5]) } 
idpoint15
idpointna15 <- idpoint15[ , colSums(is.na(idpoint15)) == 0]  ## eliminate NA
nrow(idpointna15 ) ## number of updaters
ncol(idpointna15 )
## updates ##
update15 <- matrix(0, nrow = nrow(idpointna15 ), ncol = ncol(idpointna15 ) )
for (j in 2:ncol(idpointna15 )) {for (i in 2:nrow(idpointna15 )) { update15[i,j] <- ifelse (idpointna15[i,j] == idpointna15[i-1,j],0,1) }}
update15


## create a new column with proportion of updates 
updaters15 <- numeric(nrow(idpointna15 ))
for(t in 2:nrow(idpointna15 )) {updaters15[t] <- sum(update15[t, 2:ncol(update15)])/(ncol(update15)-1)}
updaters15

## 2016 

inf16_id2 <- subset(newdf,inft == "infgen_a16" & variable =="infgent" &  analyst == 4 )
inf16_id2



y16 <- data.frame(inf16_id2[,1])

##df for only inflation of the current period
infdf16 <- NULL # empty subset 
ipon16 <- matrix() 
idpoint16 <- add.col(y16, ipon16)
idpoint16


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf16 <- subset(newdf,inft == "infgen_a16" & variable =="infgent" & analyst == t )
idpoint16 <- add.col(idpoint16,infdf16[,5]) } 
idpoint16
idpointna16 <- idpoint16[ , colSums(is.na(idpoint16)) == 0]  ## eliminate NA
nrow(idpointna16 ) ## number of updaters
ncol(idpointna16 )
## updates ##
update16 <- matrix(0, nrow = nrow(idpointna16 ), ncol = ncol(idpointna16 ) )
for (j in 2:ncol(idpointna16 )) {for (i in 2:nrow(idpointna16 )) { update16[i,j] <- ifelse (idpointna16[i,j] == idpointna16[i-1,j],0,1) }}
update16


## create a new column with proportion of updates 
updaters16 <- numeric(nrow(idpointna16 ))
for(t in 2:nrow(idpointna16 )) {updaters16[t] <- sum(update16[t, 2:ncol(update16)])/(ncol(update16)-1)}
updaters16


## 2017 ##

inf17_id2 <- subset(newdf,inft == "infgen_a17" & variable =="infgent" &  analyst == 4 )
inf17_id2



y17 <- data.frame(inf17_id2[,1])

##df for only inflation of the current period
infdf17 <- NULL # empty subset 
ipon17 <- matrix() 
idpoint17 <- add.col(y17, ipon17)
idpoint17


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf17 <- subset(newdf,inft == "infgen_a17" & variable =="infgent" & analyst == t )
idpoint17 <- add.col(idpoint17,infdf17[,5]) } 
idpoint17
idpointna17 <- idpoint17[ , colSums(is.na(idpoint17)) == 0]  ## eliminate NA
nrow(idpointna17 ) ## number of updaters
ncol(idpointna17 )
## updates ##
update17 <- matrix(0, nrow = nrow(idpointna17 ), ncol = ncol(idpointna17 ) )
for (j in 2:ncol(idpointna17 )) {for (i in 2:nrow(idpointna17 )) { update17[i,j] <- ifelse (idpointna17[i,j] == idpointna17[i-1,j],0,1) }}
update17


## create a new column with proportion of updates 
updaters17 <- numeric(nrow(idpointna17 ))
for(t in 2:nrow(idpointna17 )) {updaters17[t] <- sum(update17[t, 2:ncol(update17)])/(ncol(update17)-1)}
updaters17

## 2018 

inf18_id2 <- subset(newdf,inft == "infgen_a18" & variable =="infgent" &  analyst == 4 )
inf18_id2



y18 <- data.frame(inf18_id2[,1])

##df for only inflation of the current period
infdf18 <- NULL # empty subset 
ipon18 <- matrix() 
idpoint18 <- add.col(y18, ipon18)
idpoint18


## data frame with point forecast from analyst 1 to 91
for(t in 1:91) {infdf18 <- subset(newdf,inft == "infgen_a18" & variable =="infgent" & analyst == t )
idpoint18 <- add.col(idpoint18,infdf18[,5]) } 
idpoint18
idpointna18 <- idpoint18[ , colSums(is.na(idpoint18)) == 0]  ## eliminate NA
nrow(idpointna18 ) ## number of updaters
ncol(idpointna18 )
## updates ##
update18 <- matrix(0, nrow = nrow(idpointna18 ), ncol = ncol(idpointna18 ) )
for (j in 2:ncol(idpointna18 )) {for (i in 2:nrow(idpointna18 )) { update18[i,j] <- ifelse (idpointna18[i,j] == idpointna18[i-1,j],0,1) }}
update18


## create a new column with proportion of updates 
updaters18 <- numeric(nrow(idpointna18 ))
for(t in 2:nrow(idpointna18 )) {updaters18[t] <- sum(update18[t, 2:ncol(update18)])/(ncol(update18)-1)}
updaters18




dat <- as.Date(seq(ISOdate(2005,1,1), by = "month", length.out = 167), format = "%d/%m/%Y")
dat



updaters <- c(updaters05_0, updaters06_0, updaters07_0, updaters08_0, updaters09_0, updaters10_0, updaters11_0, updaters12_0, updaters13_0, updaters14_0, updaters15_0, updaters16_0, updaters17_0, updaters18_0)
dat_05 <- as.Date(seq(ISOdate(2005,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
dat_06 <- as.Date(seq(ISOdate(2006,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
dat_07 <- as.Date(seq(ISOdate(2007,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
dat_08 <- as.Date(seq(ISOdate(2008,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
dat_09 <- as.Date(seq(ISOdate(2009,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
dat_10 <- as.Date(seq(ISOdate(2010,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
dat_11 <- as.Date(seq(ISOdate(2011,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
dat_12 <- as.Date(seq(ISOdate(2012,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
dat_13 <- as.Date(seq(ISOdate(2013,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
dat_14 <- as.Date(seq(ISOdate(2014,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
dat_15 <- as.Date(seq(ISOdate(2015,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
dat_16 <- as.Date(seq(ISOdate(2016,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
dat_17 <- as.Date(seq(ISOdate(2017,2,1), by = "month", length.out = 11), format = "%d/%m/%Y")
dat_18 <- as.Date(seq(ISOdate(2018,2,1), by = "month", length.out = 10), format = "%d/%m/%Y")



dates <- c(dat_05, dat_06, dat_07, dat_08, dat_09, dat_10, dat_11, dat_12, dat_13, dat_14, dat_15, dat_16, dat_17, dat_18)
data.frame(dates, updaters)
plot(dates, updaters, type = 'l')
updaters
mean(updaters)
max(updaters)
min(updaters)

dat <- c(dat05, dat06, dat07, dat08, dat09, dat10, dat11, dat12, dat13, dat14, dat15, dat16, dat17, dat18)
updatersf <- c(updaters05,updaters06, updaters07, updaters08, updaters09, updaters10, updaters11, updaters12, updaters13, updaters14, updaters15, updaters16, updaters17, updaters18  )

## Fig 1 participants 
partbmx <- read.csv("participants.csv")
bmx <- partbmx[,2]
plot(dat, total, type = "l", ylab = "Total answers", xlab = "Time")
lines(dat, bmx, col = "blue")
library(ggplot2)
ggplot(data = data.frame(dat, total), aes(x= "Time", y= "Total Answer", group=1 )) + geom_line() + geom_point()

mean(bmx)
max(bmx)
min(bmx)
data.frame(dates,updaters)

mean(updaters)
max(total)
min(updaters)

## Fig 2 Frequent updaters
data.frame(dates, updatersf[updatersf >0])
plot(dates, updatersf[updatersf >0], type = 'l', ylab = "updaters", xlab = "Time") ## I am not using this 
plot(dates, updaters, type = 'l', ylab = "updaters", xlab = "Time", col = "red")
lines(dates, updaters, col = "red")
updatersf[updatersf >0]

library(tidyverse)
library(cellranger)
library(grid)

## Figure 2 
fig2 <- data.frame(dates, updaters)
ggplot(data = fig2, aes(x= dates, y=updaters)) + 
  geom_line(color= "grey2") + labs(x = "Time", y = "Updaters")

mean(updatersf[updatersf >0])
max(updatersf[updatersf >0])
min(updatersf[updatersf >0])

ts2005 <- data.frame(dat05, updaters05, updaters05_0)
names(ts2005) <- c("Date", "prop", "prop2")

dat06 <- as.Date(seq(ISOdate(2006,1,1), by = "month", length.out = 12), format = "%d/%m/%Y")
ts2006 <- data.frame(dat06 , updaters06, updaters06_0 )
names(ts2006) <- c("Date", "prop", "prop2")

dat07 <- as.Date(seq(ISOdate(2007,1,1), by = "month", length.out = 12), format = "%d/%m/%Y")
ts2007 <- data.frame(dat07 , updaters07, updaters07_0 )
names(ts2007) <- c("Date", "prop", "prop2")

dat08 <- as.Date(seq(ISOdate(2008,1,1), by = "month", length.out = 12), format = "%d/%m/%Y")
ts2008 <- data.frame(dat08 , updaters08, updaters08_0 )
names(ts2008) <- c("Date", "prop", "prop2")

dat09 <- as.Date(seq(ISOdate(2009,1,1), by = "month", length.out = 12), format = "%d/%m/%Y")
ts2009 <- data.frame(dat09 , updaters09, updaters09_0 )
names(ts2009) <- c("Date", "prop", "prop2")

dat10 <- as.Date(seq(ISOdate(2010,1,1), by = "month", length.out = 12), format = "%d/%m/%Y")
ts2010 <- data.frame(dat10 , updaters10, updaters10_0 )
names(ts2010) <- c("Date", "prop", "prop2")

dat11 <- as.Date(seq(ISOdate(2011,1,1), by = "month", length.out = 12), format = "%d/%m/%Y")
ts2011 <- data.frame(dat11 , updaters11, updaters11_0 )
names(ts2011) <- c("Date", "prop", "prop2")

dat12 <- as.Date(seq(ISOdate(2012,1,1), by = "month", length.out = 12), format = "%d/%m/%Y")
ts2012 <- data.frame(dat12 , updaters12, updaters12_0 )
names(ts2012) <- c("Date", "prop", "prop2")

dat13 <- as.Date(seq(ISOdate(2013,1,1), by = "month", length.out = 12), format = "%d/%m/%Y")
ts2013 <- data.frame(dat13 , updaters13, updaters13_0 )
names(ts2013) <- c("Date", "prop", "prop2")

dat14 <- as.Date(seq(ISOdate(2014,1,1), by = "month", length.out = 12), format = "%d/%m/%Y")
ts2014 <- data.frame(dat14 , updaters14, updaters14_0 )
names(ts2014) <- c("Date", "prop", "prop2")

dat15 <- as.Date(seq(ISOdate(2015,1,1), by = "month", length.out = 12), format = "%d/%m/%Y")
ts2015 <- data.frame(dat15 , updaters15, updaters15_0 )
names(ts2015) <- c("Date", "prop", "prop2")

dat16 <- as.Date(seq(ISOdate(2016,1,1), by = "month", length.out = 12), format = "%d/%m/%Y")
ts2016 <- data.frame(dat16 , updaters16, updaters16_0 )
names(ts2016) <- c("Date", "prop", "prop2")

dat17 <- as.Date(seq(ISOdate(2017,1,1), by = "month", length.out = 12), format = "%d/%m/%Y")
ts2017 <- data.frame(dat17 , updaters17, updaters17_0 )
names(ts2017) <- c("Date", "prop", "prop2")

dat18 <- as.Date(seq(ISOdate(2018,1,1), by = "month", length.out = 11), format = "%d/%m/%Y")
ts2018 <- data.frame(dat18 , updaters18, updaters18_0 )
names(ts2018) <- c("Date", "prop", "prop2")

tsdataframe <- rbind(ts2005, ts2006, ts2007, ts2008, ts2009, ts2010, ts2011, ts2012, ts2013, ts2014, ts2015, ts2016, ts2017, ts2018)

## time serie
updaters_ts <- ts(tsdataframe[2:3], frequency = 12, start=c(2005, 1), end=c(2018, 11))
## fig 1
autoplot(updaters_ts)

cor(updaters_ts[,1], updaters_ts[,2])

### Rigidity  Coibion and Gorodnichenko
## vector of actual inflation 
actualinf <- c(3.33, 4.05, 3.76, 6.53, 3.57, 4.40, 3.82, 3.57, 3.97, 4.08, 2.13, 3.36, 6.77, 4.83)

## agregated innattention 
## Consensus 

mean(idpointna05[2,2:ncol(idpointna05)])
cons05 <- numeric(12)
cons06 <- numeric(12)
cons07 <- numeric(12)
cons08 <- numeric(12)
cons09 <- numeric(12)
cons10 <- numeric(12)
cons11 <- numeric(12)
cons12 <- numeric(12)
cons13 <- numeric(12)
cons14 <- numeric(12)
cons15 <- numeric(12)
cons16 <- numeric(12)
cons17 <- numeric(12)
cons18 <- numeric(11)

for(t in 1:nrow(idpointna05)) {cons05[t] <- sum(idpointna05[t,2:ncol(idpointna05)])/(ncol(idpointna05)-1)}
cons05
for(t in 1:nrow(idpointna06)) {cons06[t] <- sum(idpointna06[t,2:ncol(idpointna06)])/(ncol(idpointna06)-1)}
cons06
for(t in 1:nrow(idpointna07)) {cons07[t] <- sum(idpointna07[t,2:ncol(idpointna07)])/(ncol(idpointna07)-1)}
cons07
for(t in 1:nrow(idpointna08)) {cons08[t] <- sum(idpointna08[t,2:ncol(idpointna08)])/(ncol(idpointna08)-1)}
cons08
for(t in 1:nrow(idpointna09)) {cons09[t] <- sum(idpointna09[t,2:ncol(idpointna09)])/(ncol(idpointna09)-1)}
cons09
for(t in 1:nrow(idpointna10)) {cons10[t] <- sum(idpointna10[t,2:ncol(idpointna10)])/(ncol(idpointna10)-1)}
cons10
for(t in 1:nrow(idpointna11)) {cons11[t] <- sum(idpointna11[t,2:ncol(idpointna11)])/(ncol(idpointna11)-1)}
cons11
for(t in 1:nrow(idpointna12)) {cons12[t] <- sum(idpointna12[t,2:ncol(idpointna12)])/(ncol(idpointna12)-1)}
cons12
for(t in 1:nrow(idpointna13)) {cons13[t] <- sum(idpointna13[t,2:ncol(idpointna13)])/(ncol(idpointna13)-1)}
cons13
for(t in 1:nrow(idpointna14)) {cons14[t] <- sum(idpointna14[t,2:ncol(idpointna14)])/(ncol(idpointna14)-1)}
cons14
for(t in 1:nrow(idpointna15)) {cons15[t] <- sum(idpointna15[t,2:ncol(idpointna15)])/(ncol(idpointna15)-1)}
cons15
for(t in 1:nrow(idpointna16)) {cons16[t] <- sum(idpointna16[t,2:ncol(idpointna16)])/(ncol(idpointna16)-1)}
cons16
for(t in 1:nrow(idpointna17)) {cons17[t] <- sum(idpointna17[t,2:ncol(idpointna17)])/(ncol(idpointna17)-1)}
cons17
for(t in 1:nrow(idpointna18)) {cons18[t] <- sum(idpointna18[t,2:ncol(idpointna18)])/(ncol(idpointna18)-1)}
cons18

## consensus forecast error

cferror05 <- actualinf[1] - cons05
cferror05
cferror06 <- actualinf[2] - cons06
cferror06
cferror07 <- actualinf[3] - cons07
cferror07
cferror08 <- actualinf[4] - cons08
cferror08
cferror09 <- actualinf[5] - cons09
cferror09
cferror10 <- actualinf[6] - cons10
cferror10
cferror11 <- actualinf[7] - cons11
cferror11
cferror12 <- actualinf[8] - cons12
cferror12
cferror13 <- actualinf[9] - cons13
cferror13
cferror14 <- actualinf[10] - cons14
cferror14
cferror15 <- actualinf[11] - cons15
cferror15
cferror16 <- actualinf[12] - cons16
cferror16
cferror17 <- actualinf[13] - cons17
cferror17
cferror18 <- actualinf[14] - cons18
cferror18
cons08
actualinf
# Aggregatted forecast revision 
afrev05 <- numeric(12)
for (j in 2:12) {afrev05[j] <- cons05[j] - cons05[j-1] }
afrev05

afrev06 <- numeric(12)
for (j in 2:12) {afrev06[j] <- cons06[j] - cons06[j-1] }
afrev06

afrev07 <- numeric(12)
for (j in 2:12) {afrev07[j] <- cons07[j] - cons07[j-1] }

afrev08 <- numeric(12)
for (j in 2:12) {afrev08[j] <- cons08[j] - cons08[j-1] }
afrev08

afrev09 <- numeric(12)
for (j in 2:12) {afrev09[j] <- cons09[j] - cons09[j-1] }
afrev09

afrev10 <- numeric(12)
for (j in 2:12) {afrev10[j] <- cons10[j] - cons10[j-1] }
afrev10

afrev11 <- numeric(12)
for (j in 2:12) {afrev11[j] <- cons11[j] - cons11[j-1] }
afrev11

afrev12 <- numeric(12)
for (j in 2:12) {afrev12[j] <- cons12[j] - cons12[j-1] }
afrev12

afrev13 <- numeric(12)
for (j in 2:12) {afrev13[j] <- cons13[j] - cons13[j-1] }
afrev13

afrev14 <- numeric(12)
for (j in 2:12) {afrev14[j] <- cons14[j] - cons14[j-1] }
afrev14

afrev15 <- numeric(12)
for (j in 2:12) {afrev15[j] <- cons15[j] - cons15[j-1] }
afrev15

afrev16 <- numeric(12)
for (j in 2:12) {afrev16[j] <- cons16[j] - cons16[j-1] }
afrev16

afrev17 <- numeric(12)
for (j in 2:12) {afrev17[j] <- cons17[j] - cons17[j-1] }
afrev17

afrev18 <- numeric(11)
for (j in 2:12) {afrev18[j] <- cons18[j] - cons18[j-1] }
afrev18

ferrorT <- c(cferror05, cferror06, cferror07, cferror08, cferror09, cferror10, cferror11, cferror12, cferror13, cferror14, cferror15, cferror16, cferror17, cferror18[1:11])
ferror <- c(cferror05[2:12], cferror06[2:12], cferror07[2:12], cferror08[2:12], cferror09[2:12], cferror10[2:12], cferror11[2:12], cferror12[2:12], cferror13[2:12], cferror14[2:12], cferror15[2:12], cferror16[2:12], cferror17[2:12], cferror18[2:11])
frev <- c(afrev05[2:12], afrev06[2:12], afrev07[2:12], afrev08[2:12], afrev09[2:12], afrev10[2:12], afrev11[2:12], afrev12[2:12], afrev13[2:12], afrev14[2:12], afrev15[2:12], afrev16[2:12], afrev17[2:12], afrev18[2:11])

ferrorT
mean(ferrorT)
rmse <- sqrt(ferrorT^2)

## Fig3
plot(dat,rmse, type = "l", xlab = "time")
line(dates, updaters)

ggplot(data = data.frame(dat, rmse), aes (x= dat, y = rmse)) +
  geom_line()+
  labs(x = "Time", y = "RMSE")

library(stargazer)
aggRig <- lm(ferror ~ frev)
summary(aggRig) ## print table 
stargazer(aggRig)



consensus <- c(cons05[2:12], cons06[2:12], cons07[2:12], cons08[2:12], cons09[2:12], cons10[2:12], cons11[2:12], cons12[2:12], cons13[2:12], cons14[2:12], cons15[2:12], cons16[2:12], cons17[2:12], cons18[2:11])
consensusT <- c(cons05, cons06, cons07, cons08, cons09, cons10, cons11, cons12, cons13, cons14, cons15, cons16, cons17, cons18[1:11])
consensus_1 <- c(cons05[1:11], cons06[1:11], cons07[1:11], cons08[1:11], cons09[1:11], cons10[1:11], cons11[1:11], cons12[1:11], cons13[1:11], cons14[1:11], cons15[1:11], cons16[1:11], cons17[1:11], cons18[1:10])

aggRig2 <- lm(ferror ~ consensus + consensus_1)
summary(aggRig2) ## print table 
stargazer(aggRig2)

anualinf <- read.csv("anualinfmx.csv")
biastest <- lm(consensusT ~ Lag(anualinf$SP30578[2:168],1))
summary(biastest)
stargazer(biastest)

## interaction with AMLO 
amlodb <- read.csv("amlo.csv")
amlo <- amlodb[,2]
ele <- amlodb[,3]
epu <- amlodb$EPU
index_elect <- amlodb$eleindex
amloe <- amlodb$amloe
inf <- amlodb[,7]

addRigPol <- lm(ferror ~ frev + amlo)
summary(addRigPol)

fevamlo <- frev*amlo

addRigPol2 <- lm(ferror ~ Lag(inf,1))
summary(addRigPol2)

frevepu <- frev*Lag(epu,2)

biast <- lm(consensus ~ Lag(inf,1))
summary(biast)


plot(gdp2updaters, epu/100)

reg1 <- lm(gdp2updaters ~ epu + amlo21)
summary(reg1)

## probit 
update <- glm(updatersf[updatersf >0] ~ ferror, family=binomial(link="probit"))
summary(update)

update_epu <- glm(gdp2updaters ~ Lag(epu,2), family=binomial(link="logit"))
summary(update_epu)

update_inf <- glm(updaters ~ inf, family=binomial(link="probit"))
summary(update_inf)

actualinf[4]
idpointna05[1,2:ncol(idpointna05)]
idpointna05

# Forecast error 
f_error05 <- matrix(0, nrow(idpointna05)-1, ncol(idpointna05))
for (j in 2:ncol(idpointna05)) {f_error05[,j] <- actualinf[1] - idpointna05[2:12,j] }
f_error05

f_error06 <- matrix(0, nrow(idpointna06)-1, ncol(idpointna06))
for (j in 2:ncol(idpointna06)) {f_error06[,j] <- actualinf[2] - idpointna06[2:12,j] }
f_error06

f_error07 <- matrix(0, nrow(idpointna07)-1, ncol(idpointna07))
for (j in 2:ncol(idpointna07)) {f_error07[,j] <- actualinf[3] - idpointna07[2:12,j] }
f_error07

f_error08 <- matrix(0, nrow(idpointna08)-1, ncol(idpointna08))
for (j in 2:ncol(idpointna08)) {f_error08[,j] <- actualinf[4] - idpointna08[2:12,j] }
f_error08

f_error09 <- matrix(0, nrow(idpointna09)-1, ncol(idpointna09))
for (j in 2:ncol(idpointna09)) {f_error09[,j] <- actualinf[5] - idpointna09[2:12,j] }
f_error09

f_error10 <- matrix(0, nrow(idpointna10)-1, ncol(idpointna10))
for (j in 2:ncol(idpointna10)) {f_error10[,j] <- actualinf[6] - idpointna10[2:12,j] }
f_error10

f_error11 <- matrix(0, nrow(idpointna11)-1, ncol(idpointna11))
for (j in 2:ncol(idpointna11)) {f_error11[,j] <- actualinf[7] - idpointna11[2:12,j] }
f_error11

f_error12 <- matrix(0, nrow(idpointna12)-1, ncol(idpointna12))
for (j in 2:ncol(idpointna12)) {f_error12[,j] <- actualinf[8] - idpointna12[2:12,j] }
f_error12

f_error13 <- matrix(0, nrow(idpointna13)-1, ncol(idpointna13))
for (j in 2:ncol(idpointna13)) {f_error13[,j] <- actualinf[9] - idpointna13[2:12,j] }
f_error13

f_error14 <- matrix(0, nrow(idpointna14)-1, ncol(idpointna14))
for (j in 2:ncol(idpointna14)) {f_error14[,j] <- actualinf[10] - idpointna14[2:12,j] }
f_error14

f_error15 <- matrix(0, nrow(idpointna15)-1, ncol(idpointna15))
for (j in 2:ncol(idpointna15)) {f_error15[,j] <- actualinf[11] - idpointna15[2:12,j] }
f_error15

f_error16 <- matrix(0, nrow(idpointna16)-1, ncol(idpointna16))
for (j in 2:ncol(idpointna16)) {f_error16[,j] <- actualinf[12] - idpointna16[2:12,j] }
f_error16

f_error17 <- matrix(0, nrow(idpointna17)-1, ncol(idpointna17))
for (j in 2:ncol(idpointna17)) {f_error17[,j] <- actualinf[13] - idpointna17[2:12,j] }
f_error17

f_error18 <- matrix(0, nrow(idpointna18)-1, ncol(idpointna18))
for (j in 2:ncol(idpointna18)) {f_error18[,j] <- actualinf[14] - idpointna18[2:11,j] }
f_error18

## aqui toy 
f_error <- c(as.vector(f_error05), as.vector(f_error06), as.vector(f_error07), as.vector(f_error08), as.vector(f_error09), as.vector(f_error10), as.vector(f_error11), as.vector(f_error12), as.vector(f_error13),as.vector(f_error14), as.vector(f_error15), as.vector(f_error16), as.vector(f_error17), as.vector(f_error18))
f_error

f_revi <- c(as.vector(f_rev05[2:12,]),as.vector(f_rev06[2:12,]),as.vector(f_rev07[2:12,]),as.vector(f_rev08[2:12,]),as.vector(f_rev09[2:12,]),as.vector(f_rev10[2:12,]), as.vector(f_rev11[2:12,]),as.vector(f_rev12[2:12,]),as.vector(f_rev13[2:12,]),as.vector(f_rev14[2:12,]),as.vector(f_rev15[2:12,]),as.vector(f_rev16[2:12,]),as.vector(f_rev17[2:12,]),as.vector(f_rev18[2:11,]) )
length(f_error)

length(f_error)
length(f_revi)

indrig <- lm(f_error ~ f_revi)
summary(indrig)
stargazer(indrig)
#### ####

idpointna05[2,26] - idpointna05[1,26]
idpointna05[12,26] - idpointna05[11,26]
# Forecast Revision individual 

f_rev05 <- matrix(0, nrow(idpointna05), ncol(idpointna05))
for (t in 2:ncol(idpointna05)) {for (i in 2:nrow(idpointna05)) {f_rev05[i,t] <- idpointna05[i,t] - idpointna05[i-1,t] }}
f_rev05[2:12,]

f_rev06 <- matrix(0, nrow(idpointna06), ncol(idpointna06))
for (t in 2:ncol(idpointna06)) {for (i in 2:nrow(idpointna06)) {f_rev06[i,t] <- idpointna06[i,t] - idpointna06[i-1,t] }}
f_rev06[2:12,]

f_rev07 <- matrix(0, nrow(idpointna07), ncol(idpointna07))
for (t in 2:ncol(idpointna07)) {for (i in 2:nrow(idpointna07)) {f_rev07[i,t] <- idpointna07[i,t] - idpointna07[i-1,t] }}
f_rev07[2:12,]

f_rev08 <- matrix(0, nrow(idpointna08), ncol(idpointna08))
for (t in 2:ncol(idpointna08)) {for (i in 2:nrow(idpointna08)) {f_rev08[i,t] <- idpointna08[i,t] - idpointna08[i-1,t] }}
f_rev08[2:12,]

f_rev09 <- matrix(0, nrow(idpointna09), ncol(idpointna09))
for (t in 2:ncol(idpointna09)) {for (i in 2:nrow(idpointna09)) {f_rev09[i,t] <- idpointna09[i,t] - idpointna09[i-1,t] }}
f_rev09[2:12,]

f_rev10 <- matrix(0, nrow(idpointna10), ncol(idpointna10))
for (t in 2:ncol(idpointna10)) {for (i in 2:nrow(idpointna10)) {f_rev10[i,t] <- idpointna10[i,t] - idpointna10[i-1,t] }}
f_rev10[2:12,]

f_rev11 <- matrix(0, nrow(idpointna11), ncol(idpointna11))
for (t in 2:ncol(idpointna11)) {for (i in 2:nrow(idpointna11)) {f_rev11[i,t] <- idpointna11[i,t] - idpointna11[i-1,t] }}
f_rev11[2:12,]

f_rev12 <- matrix(0, nrow(idpointna12), ncol(idpointna12))
for (t in 2:ncol(idpointna12)) {for (i in 2:nrow(idpointna12)) {f_rev12[i,t] <- idpointna12[i,t] - idpointna12[i-1,t] }}
f_rev12[2:12,]

f_rev13 <- matrix(0, nrow(idpointna13), ncol(idpointna13))
for (t in 2:ncol(idpointna13)) {for (i in 2:nrow(idpointna13)) {f_rev13[i,t] <- idpointna13[i,t] - idpointna13[i-1,t] }}
f_rev13[2:12,]

f_rev14 <- matrix(0, nrow(idpointna14), ncol(idpointna14))
for (t in 2:ncol(idpointna14)) {for (i in 2:nrow(idpointna14)) {f_rev14[i,t] <- idpointna14[i,t] - idpointna14[i-1,t] }}
f_rev14[2:12,]

f_rev15 <- matrix(0, nrow(idpointna15), ncol(idpointna15))
for (t in 2:ncol(idpointna15)) {for (i in 2:nrow(idpointna15)) {f_rev15[i,t] <- idpointna15[i,t] - idpointna15[i-1,t] }}
f_rev15[2:12,]

f_rev16 <- matrix(0, nrow(idpointna16), ncol(idpointna16))
for (t in 2:ncol(idpointna16)) {for (i in 2:nrow(idpointna16)) {f_rev16[i,t] <- idpointna16[i,t] - idpointna16[i-1,t] }}
f_rev16[2:12,]

f_rev17 <- matrix(0, nrow(idpointna17), ncol(idpointna17))
for (t in 2:ncol(idpointna17)) {for (i in 2:nrow(idpointna17)) {f_rev17[i,t] <- idpointna17[i,t] - idpointna17[i-1,t] }}
f_rev17[2:12,]

f_rev18 <- matrix(0, nrow(idpointna18), ncol(idpointna18))
for (t in 2:ncol(idpointna18)) {for (i in 2:nrow(idpointna18)) {f_rev18[i,t] <- idpointna18[i,t] - idpointna18[i-1,t] }}
f_rev18[2:11,]

###
update05
### those who participate every month 
freqp05 <- numeric(12)
freqp06 <- numeric(12)
freqp07 <- numeric(12)
freqp08 <- numeric(12)
freqp09 <- numeric(12)
freqp10 <- numeric(12)
freqp11 <- numeric(12)
freqp12 <- numeric(12)
freqp13 <- numeric(12)
freqp14 <- numeric(12)
freqp15 <- numeric(12)
freqp16 <- numeric(12)
freqp17 <- numeric(12)
freqp18 <- numeric(11)

for(t in 1:nrow(update05)) {freqp05[t] <- sum(update05[t,2:ncol(update05)])}
freqp05

for(t in 1:nrow(update06)) {freqp06[t] <- sum(update06[t,2:ncol(update06)])}
freqp06

for(t in 1:nrow(update07)) {freqp07[t] <- sum(update07[t,2:ncol(update07)])}
freqp07

for(t in 1:nrow(update08)) {freqp08[t] <- sum(update08[t,2:ncol(update08)])}
freqp08

for(t in 1:nrow(update09)) {freqp09[t] <- sum(update09[t,2:ncol(update09)])}
freqp09

for(t in 1:nrow(update10)) {freqp10[t] <- sum(update10[t,2:ncol(update10)])}
freqp10

for(t in 1:nrow(update11)) {freqp11[t] <- sum(update11[t,2:ncol(update11)])}
freqp11

for(t in 1:nrow(update12)) {freqp12[t] <- sum(update12[t,2:ncol(update12)])}
freqp12

for(t in 1:nrow(update13)) {freqp13[t] <- sum(update13[t,2:ncol(update13)])}
freqp13

for(t in 1:nrow(update14)) {freqp14[t] <- sum(update14[t,2:ncol(update14)])}
freqp14

for(t in 1:nrow(update15)) {freqp15[t] <- sum(update15[t,2:ncol(update15)])}
freqp15

for(t in 1:nrow(update16)) {freqp16[t] <- sum(update16[t,2:ncol(update16)])}
freqp16

for(t in 1:nrow(update17)) {freqp17[t] <- sum(update17[t,2:ncol(update17)])}
freqp17

for(t in 1:nrow(update18)) {freqp18[t] <- sum(update18[t,2:ncol(update18)])}
freqp18

freqp <- c(freqp05, freqp06,freqp07, freqp08, freqp09, freqp10, freqp11, freqp12, freqp13, freqp14, freqp15, freqp16, freqp17, freqp18) 
total <- c (total05, total06, total07, total08, total09, total10, total11, total12, total13, total14, total15, total16, total17, total18)

plot(dates, freqp[freqp>0], type = 'l')
plot(dat, total[total > 0], type = 'l')
length(total[total >0]) 
length(dates)
length(freqp)
participants <- ts(data.frame(freqp, total), frequency = 12, start=c(2005, 1), end=c(2018, 11) )
autoplot(participants)
mean(total)
max(total)
min(total)

mean(freqp)
max(freqp)
min(freqp [freqp >0])

total05 <- numeric(12)
total06 <- numeric(12)
total07 <- numeric(12)
total08 <- numeric(12)
total09 <- numeric(12)
total10 <- numeric(12)
total11 <- numeric(12)
total12 <- numeric(12)
total13 <- numeric(12)
total14 <- numeric(12)
total15 <- numeric(12)
total16 <- numeric(12)
total17 <- numeric(12)
total18 <- numeric(11)


### number of participants 
parti05 <- matrix(0, nrow = nrow(idpoint05_0 ), ncol = ncol(idpoint05_0 ) )
for (j in 2:ncol(idpoint05_0 )) {for (i in 1:nrow(idpoint05_0 )) { parti05[i,j] <- ifelse (idpoint05_0[i,j] > 0,1,0) }}
for(t in 1:nrow(idpoint05_0 )) {total05[t] <- sum(parti05[t,2:ncol(parti05)])}

total05
ncol(parti05)
sum(parti05[2,])
sum(update05_0[2,])
total05
updaters05_0



parti06 <- matrix(0, nrow = nrow(idpoint06_0 ), ncol = ncol(idpoint06_0 ) )
for (j in 2:ncol(idpoint06_0 )) {for (i in 1:nrow(idpoint06_0 )) { parti06[i,j] <- ifelse (idpoint06_0[i,j] > 0,1,0) }}
for(t in 1:nrow(idpoint06_0 )) {total06[t] <- sum(parti06[t,2:ncol(parti06)])}
total06



parti07 <- matrix(0, nrow = nrow(idpoint07_0 ), ncol = ncol(idpoint07_0 ) )
for (j in 2:ncol(idpoint07_0 )) {for (i in 1:nrow(idpoint07_0 )) { parti07[i,j] <- ifelse (idpoint07_0[i,j] > 0,1,0) }}
for(t in 1:nrow(idpoint07_0 )) {total07[t] <- sum(parti07[t,2:ncol(parti07)])}
total07


parti08 <- matrix(0, nrow = nrow(idpoint08_0 ), ncol = ncol(idpoint08_0 ) )
for (j in 2:ncol(idpoint08_0 )) {for (i in 1:nrow(idpoint08_0 )) { parti08[i,j] <- ifelse (idpoint08_0[i,j] > 0,1,0) }}
for(t in 1:nrow(idpoint08_0 )) {total08[t] <- sum(parti08[t,2:ncol(parti08)])}
total08

parti09 <- matrix(0, nrow = nrow(idpoint09_0 ), ncol = ncol(idpoint09_0 ) )
for (j in 2:ncol(idpoint09_0 )) {for (i in 1:nrow(idpoint09_0 )) { parti09[i,j] <- ifelse (idpoint09_0[i,j] > 0,1,0) }}
for(t in 1:nrow(idpoint09_0 )) {total09[t] <- sum(parti09[t,2:ncol(parti09)])}
total09

parti10 <- matrix(0, nrow = nrow(idpoint10_0 ), ncol = ncol(idpoint10_0 ) )
for (j in 2:ncol(idpoint10_0 )) {for (i in 1:nrow(idpoint10_0 )) { parti10[i,j] <- ifelse (idpoint10_0[i,j] > 0,1,0) }}
for(t in 1:nrow(idpoint10_0 )) {total10[t] <- sum(parti10[t,2:ncol(parti10)])}
total10

parti11 <- matrix(0, nrow = nrow(idpoint11_0 ), ncol = ncol(idpoint11_0 ) )
for (j in 2:ncol(idpoint11_0 )) {for (i in 1:nrow(idpoint11_0 )) { parti11[i,j] <- ifelse (idpoint11_0[i,j] > 0,1,0) }}
for(t in 1:nrow(idpoint11_0 )) {total11[t] <- sum(parti11[t,2:ncol(parti11)])}
total11

parti12 <- matrix(0, nrow = nrow(idpoint12_0 ), ncol = ncol(idpoint12_0 ) )
for (j in 2:ncol(idpoint12_0 )) {for (i in 1:nrow(idpoint12_0 )) { parti12[i,j] <- ifelse (idpoint12_0[i,j] > 0,1,0) }}
for(t in 1:nrow(idpoint12_0 )) {total12[t] <- sum(parti12[t,2:ncol(parti12)])}
total12

parti13 <- matrix(0, nrow = nrow(idpoint13_0 ), ncol = ncol(idpoint13_0 ) )
for (j in 2:ncol(idpoint13_0 )) {for (i in 1:nrow(idpoint13_0 )) { parti13[i,j] <- ifelse (idpoint13_0[i,j] > 0,1,0) }}
for(t in 1:nrow(idpoint13_0 )) {total13[t] <- sum(parti13[t,2:ncol(parti13)])}
total13

parti14 <- matrix(0, nrow = nrow(idpoint14_0 ), ncol = ncol(idpoint14_0 ) )
for (j in 2:ncol(idpoint14_0 )) {for (i in 1:nrow(idpoint14_0 )) { parti14[i,j] <- ifelse (idpoint14_0[i,j] > 0,1,0) }}
for(t in 1:nrow(idpoint14_0 )) {total14[t] <- sum(parti14[t,2:ncol(parti14)])}
total14

parti15 <- matrix(0, nrow = nrow(idpoint15_0 ), ncol = ncol(idpoint15_0 ) )
for (j in 2:ncol(idpoint15_0 )) {for (i in 1:nrow(idpoint15_0 )) { parti15[i,j] <- ifelse (idpoint15_0[i,j] > 0,1,0) }}
for(t in 1:nrow(idpoint15_0 )) {total15[t] <- sum(parti15[t,2:ncol(parti15)])}
total15

parti15[11,]
parti16 <- matrix(0, nrow = nrow(idpoint16_0 ), ncol = ncol(idpoint16_0 ) )
for (j in 2:ncol(idpoint16_0 )) {for (i in 1:nrow(idpoint16_0 )) { parti16[i,j] <- ifelse (idpoint16_0[i,j] > 0,1,0) }}
for(t in 1:nrow(idpoint16_0 )) {total16[t] <- sum(parti16[t,2:ncol(parti16)])}
total16

parti17 <- matrix(0, nrow = nrow(idpoint17_0 ), ncol = ncol(idpoint17_0 ) )
for (j in 2:ncol(idpoint17_0 )) {for (i in 1:nrow(idpoint17_0 )) { parti17[i,j] <- ifelse (idpoint17_0[i,j] > 0,1,0) }}
for(t in 1:nrow(idpoint17_0 )) {total17[t] <- sum(parti17[t,2:ncol(parti17)])}
total17

parti18 <- matrix(0, nrow = nrow(idpoint18_0 ), ncol = ncol(idpoint18_0 ) )
for (j in 2:ncol(idpoint18_0 )) {for (i in 1:nrow(idpoint18_0 )) { parti18[i,j] <- ifelse (idpoint18_0[i,j] > 0,1,0) }}
for(t in 1:nrow(idpoint18_0 )) {total18[t] <- sum(parti18[t,2:ncol(parti18)])}
total18

### 2005_0 2006_0 ...

### 2005_0
## replacing NA with 0 to count all updaters
idpoint05_0 <- idpoint05

idpoint05_0[is.na(idpoint05_0)] <- 0
ncol(idpoint05_0 )
nrow(idpoint05_0)
## updates ##
update05_0 <- matrix(0, nrow = nrow(idpoint05_0 ), ncol = ncol(idpoint05_0 ) )
for (j in 2:ncol(idpoint05_0 )) {for (i in 2:nrow(idpoint05_0 )) { update05_0[i,j] <- ifelse (idpoint05_0[i,j] == idpoint05_0[i-1,j],0,1) & (idpoint05_0[i,j]>0)  }}
update05_0



idpoint05_0[,1:5]
parti05[,1:5]
update05_0[,1:5]
idpoint05[,1:5]
update05[,1:5]
### number of participants 
parti05 <- matrix(0, nrow = nrow(idpoint05_0 ), ncol = ncol(idpoint05_0 ) )
for (j in 2:ncol(idpoint05_0 )) {for (i in 2:nrow(idpoint05_0 )) { parti05[i,j] <- ifelse (idpoint05_0[i,j] > 0,1,0) }}
totalpart05 <-numeric(nrow(idpoint05_0 ))
for(t in 2:nrow(idpoint05_0 )) {totalpart05[t] <- sum(parti05[t,2:ncol(parti05)])}
totalpart05
###

## create a new column with proportion of updates 
updaters05_0 <- numeric(nrow(idpoint05_0 ))
for(t in 2:nrow(idpoint05_0 )) {updaters05_0[t] <- sum(update05_0[t,2:ncol(update05_0)])}
updaters05_0
total05
update05_0[12,]

updaters05
updaters05_0 <- updaters05_0[2:12]/total05[2:12]
updaters05_0

### 2006_0
## replacing NA with 0 to count all updaters
idpoint06_0 <- idpoint06

idpoint06_0[is.na(idpoint06_0)] <- 0

## updates ##
update06_0 <- matrix(0, nrow = nrow(idpoint06_0 ), ncol = ncol(idpoint06_0 ) )
for (j in 2:ncol(idpoint06_0 )) {for (i in 2:nrow(idpoint06_0 )) { update06_0[i,j] <- ifelse (idpoint06_0[i,j] == idpoint06_0[i-1,j],0,1) & (idpoint06_0[i,j]>0) }}
update06_0

## create a new column with proportion of updates 
updaters06_0 <- numeric(nrow(idpoint06_0 ))
for(t in 2:nrow(idpoint06_0 )) {updaters06_0[t] <- sum(update06_0[t,2:ncol(update06_0)])}
updaters06_0
total06
updaters06_0 <- updaters06_0[2:12]/total06[2:12]



### 2007_0
## replacing NA with 0 to count all updaters
idpoint07_0 <- idpoint07

idpoint07_0[is.na(idpoint07_0)] <- 0

## updates ##
update07_0 <- matrix(0, nrow = nrow(idpoint07_0 ), ncol = ncol(idpoint07_0 ) )
for (j in 2:ncol(idpoint07_0 )) {for (i in 2:nrow(idpoint07_0 )) { update07_0[i,j] <- ifelse (idpoint07_0[i,j] == idpoint07_0[i-1,j],0,1) & (idpoint07_0[i,j]>0) }}
update07_0

## create a new column with proportion of updates 
updaters07_0 <- numeric(nrow(idpoint07_0 ))
for(t in 2:nrow(idpoint07_0 )) {updaters07_0[t] <- sum(update07_0[t,2:ncol(update07_0)])}
updaters07_0 <- updaters07_0[2:12]/total07[2:12]
updaters07_0
total07

updaters07


### 2008_0
## replacing NA with 0 to count all updaters
idpoint08_0 <- idpoint08

idpoint08_0[is.na(idpoint08_0)] <- 0

## updates ##
update08_0 <- matrix(0, nrow = nrow(idpoint08_0 ), ncol = ncol(idpoint08_0 ) )
for (j in 2:ncol(idpoint08_0 )) {for (i in 2:nrow(idpoint08_0 )) { update08_0[i,j] <- ifelse (idpoint08_0[i,j] == idpoint08_0[i-1,j],0,1) & (idpoint08_0[i,j]>0) }}
update08_0

## create a new column with proportion of updates 
updaters08_0 <- numeric(nrow(idpoint08_0 ))
for(t in 2:nrow(idpoint08_0 )) {updaters08_0[t] <- sum(update08_0[t,2:ncol(update08_0)])}
updaters08_0 <- updaters08_0[2:12]/total08[2:12]
updaters08_0
updaters08

### 2009_0
## replacing NA with 0 to count all updaters
idpoint09_0 <- idpoint09

idpoint09_0[is.na(idpoint09_0)] <- 0

## updates ##
update09_0 <- matrix(0, nrow = nrow(idpoint09_0 ), ncol = ncol(idpoint09_0 ) )
for (j in 2:ncol(idpoint09_0 )) {for (i in 2:nrow(idpoint09_0 )) { update09_0[i,j] <- ifelse (idpoint09_0[i,j] == idpoint09_0[i-1,j],0,1) & (idpoint09_0[i,j]>0) }}
update09_0

## create a new column with proportion of updates 
updaters09_0 <- numeric(nrow(idpoint09_0 ))
for(t in 2:nrow(idpoint09_0 )) {updaters09_0[t] <- sum(update09_0[t,2:ncol(update09_0)])}
updaters09_0
total09
updaters09_0 <- updaters09_0[2:12]/total09[2:12]
updaters09

### 2010_0
## replacing NA with 0 to count all updaters
idpoint10_0 <- idpoint10

idpoint10_0[is.na(idpoint10_0)] <- 0

## updates ##
update10_0 <- matrix(0, nrow = nrow(idpoint10_0 ), ncol = ncol(idpoint10_0 ) )
for (j in 2:ncol(idpoint10_0 )) {for (i in 2:nrow(idpoint10_0 )) { update10_0[i,j] <- ifelse (idpoint10_0[i,j] == idpoint10_0[i-1,j],0,1) & (idpoint10_0[i,j]>0)}}
update10_0

## create a new column with proportion of updates 
updaters10_0 <- numeric(nrow(idpoint10_0 ))
for(t in 2:nrow(idpoint10_0 )) {updaters10_0[t] <- sum(update10_0[t,2:ncol(update10_0)])}
updaters10_0
updaters10_0 <- updaters10_0[2:12]/total10[2:12]
updaters10

### 2011_0
## replacing NA with 0 to count all updaters
idpoint11_0 <- idpoint11

idpoint11_0[is.na(idpoint11_0)] <- 0

## updates ##
update11_0 <- matrix(0, nrow = nrow(idpoint11_0 ), ncol = ncol(idpoint11_0 ) )
for (j in 2:ncol(idpoint11_0 )) {for (i in 2:nrow(idpoint11_0 )) { update11_0[i,j] <- ifelse (idpoint11_0[i,j] == idpoint11_0[i-1,j],0,1) & (idpoint11_0[i,j]>0) }}
update11_0

## create a new column with proportion of updates 
updaters11_0 <- numeric(nrow(idpoint11_0 ))
for(t in 2:nrow(idpoint11_0 )) {updaters11_0[t] <- sum(update11_0[t,2:ncol(update11_0)])}
updaters11_0
updaters11_0 <- updaters11_0[2:12]/total11[2:12]
updaters11

### 2012_0
## replacing NA with 0 to count all updaters
idpoint12_0 <- idpoint12

idpoint12_0[is.na(idpoint12_0)] <- 0

## updates ##
update12_0 <- matrix(0, nrow = nrow(idpoint12_0 ), ncol = ncol(idpoint12_0 ) )
for (j in 2:ncol(idpoint12_0 )) {for (i in 2:nrow(idpoint12_0 )) { update12_0[i,j] <- ifelse (idpoint12_0[i,j] == idpoint12_0[i-1,j],0,1) & (idpoint12_0[i,j]>0) }}
update12_0

## create a new column with proportion of updates 
updaters12_0 <- numeric(nrow(idpoint12_0 ))
for(t in 2:nrow(idpoint12_0 )) {updaters12_0[t] <- sum(update12_0[t,2:ncol(update12_0)])}
updaters12_0
updaters12_0 <- updaters12_0[2:12]/total12[2:12]
updaters12

### 2013_0
## replacing NA with 0 to count all updaters
idpoint13_0 <- idpoint13

idpoint13_0[is.na(idpoint13_0)] <- 0

## updates ##
update13_0 <- matrix(0, nrow = nrow(idpoint13_0 ), ncol = ncol(idpoint13_0 ) )
for (j in 2:ncol(idpoint13_0 )) {for (i in 2:nrow(idpoint13_0 )) { update13_0[i,j] <- ifelse (idpoint13_0[i,j] == idpoint13_0[i-1,j],0,1) & (idpoint13_0[i,j]>0) }}
update13_0

## create a new column with proportion of updates 
updaters13_0 <- numeric(nrow(idpoint13_0 ))
for(t in 2:nrow(idpoint13_0 )) {updaters13_0[t] <- sum(update13_0[t,2:ncol(update13_0)])}
updaters13_0
updaters13_0 <- updaters13_0[2:12]/total13[2:12]
updaters13


### 2014_0
## replacing NA with 0 to count all updaters
idpoint14_0 <- idpoint14

idpoint14_0[is.na(idpoint14_0)] <- 0

## updates ##
update14_0 <- matrix(0, nrow = nrow(idpoint14_0 ), ncol = ncol(idpoint14_0 ) )
for (j in 2:ncol(idpoint14_0 )) {for (i in 2:nrow(idpoint14_0 )) { update14_0[i,j] <- ifelse (idpoint14_0[i,j] == idpoint14_0[i-1,j],0,1) & (idpoint14_0[i,j]>0)}}
update14_0

## create a new column with proportion of updates 
updaters14_0 <- numeric(nrow(idpoint14_0 ))
for(t in 2:nrow(idpoint14_0 )) {updaters14_0[t] <- sum(update14_0[t,2:ncol(update14_0)])}
updaters14_0
updaters14_0 <- updaters14_0[2:12]/total14[2:12]
updaters14


### 2015_0
## replacing NA with 0 to count all updaters
idpoint15_0 <- idpoint15

idpoint15_0[is.na(idpoint15_0)] <- 0

## updates ##
update15_0 <- matrix(0, nrow = nrow(idpoint15_0 ), ncol = ncol(idpoint15_0 ) )
for (j in 2:ncol(idpoint15_0 )) {for (i in 2:nrow(idpoint15_0 )) { update15_0[i,j] <- ifelse (idpoint15_0[i,j] == idpoint15_0[i-1,j],0,1) & (idpoint15_0[i,j]>0) }}
update15_0

## create a new column with proportion of updates 
updaters15_0 <- numeric(nrow(idpoint15_0 ))
for(t in 2:nrow(idpoint15_0 )) {updaters15_0[t] <- sum(update15_0[t,2:ncol(update15_0)])}
updaters15_0
total15
updaters15_0 <- updaters15_0[2:12]/total15[2:12]
updaters15
update15_0[11,]
idpoint15_0[11,]
### 2016_0
## replacing NA with 0 to count all updaters
idpoint16_0 <- idpoint16

idpoint16_0[is.na(idpoint16_0)] <- 0

## updates ##
update16_0 <- matrix(0, nrow = nrow(idpoint16_0 ), ncol = ncol(idpoint16_0 ) )
for (j in 2:ncol(idpoint16_0 )) {for (i in 2:nrow(idpoint16_0 )) { update16_0[i,j] <- ifelse (idpoint16_0[i,j] == idpoint16_0[i-1,j],0,1) & (idpoint16_0[i,j]>0) }}
update16_0

## create a new column with proportion of updates 
updaters16_0 <- numeric(nrow(idpoint16_0 ))
for(t in 2:nrow(idpoint16_0 )) {updaters16_0[t] <- sum(update16_0[t,2:ncol(update16_0)])}
updaters16_0
updaters16_0 <- updaters16_0[2:12]/total16[2:12]
updaters16

### 2017_0
## replacing NA with 0 to count all updaters
idpoint17_0 <- idpoint17

idpoint17_0[is.na(idpoint17_0)] <- 0

## updates ##
update17_0 <- matrix(0, nrow = nrow(idpoint17_0 ), ncol = ncol(idpoint17_0 ) )
for (j in 2:ncol(idpoint17_0 )) {for (i in 2:nrow(idpoint17_0 )) { update17_0[i,j] <- ifelse (idpoint17_0[i,j] == idpoint17_0[i-1,j],0,1) & (idpoint17_0[i,j]>0) }}
update17_0

## create a new column with proportion of updates 
updaters17_0 <- numeric(nrow(idpoint17_0 ))
for(t in 2:nrow(idpoint17_0 )) {updaters17_0[t] <- sum(update17_0[t,2:ncol(update17_0)])}
updaters17_0
updaters17_0 <- updaters17_0[2:12]/total17[2:12]
updaters17

### 2018_0
## replacing NA with 0 to count all updaters
idpoint18_0 <- idpoint18

idpoint18_0[is.na(idpoint18_0)] <- 0

## updates ##
update18_0 <- matrix(0, nrow = nrow(idpoint18_0 ), ncol = ncol(idpoint18_0 ) )
for (j in 2:ncol(idpoint18_0 )) {for (i in 2:nrow(idpoint18_0 )) { update18_0[i,j] <- ifelse (idpoint18_0[i,j] == idpoint18_0[i-1,j],0,1) & (idpoint18_0[i,j]>0) }}
update18_0

## create a new column with proportion of updates 
updaters18_0 <- numeric(nrow(idpoint18_0 ))
for(t in 2:nrow(idpoint18_0 )) {updaters18_0[t] <- sum(update18_0[t,2:ncol(update18_0)])}
updaters18_0
updaters18_0 <- updaters18_0[2:11]/total18[2:11]
updaters18





#### olvidate de lo que esta abajo ##### 
update <- matrix(0, nrow = 10, ncol = 11)
set.seed(12345)
for (i in 2:10) {update[i] <- ifelse (inf99_id2[i,5] == inf99_id2[i-1,5],0,1) }
update





inf99_id3[,5]
merge(inf99_id2[,5],inf99_id3[,5])

rbind(inf99_id2, inf99_id3)
update <- numeric(9)
set.seed(12345)
 for (i in 2:10) {update[i] <- ifelse (inf99_id2[i,5] == inf99_id2[i-1,5],0,1) }
update

#creating empty matrix  
inf99_id <- matrix(0, nrow = 11, ncol = 91)

##df for only inflation of the current period
infdf <- NULL

for(t in 2:9) {infdf = rbind(infdf,subset(newdf,inft == "infgen_a99" & analyst == t ))}
infdf




for(t in 2:2) {infdf = subset(newdf,inft == "infgen_a99" & analyst == t )}

add.col<-function(df, new.col) {n.row<-dim(df)[1]
length(new.col)<-n.row
cbind(df, new.col)
}
y

mylist <- NULL
for(t in 2:9) { mylist = list(mylist,subset(newdf,inft == "infgen_a99" & analyst == t ))}
mylist

dat <- data.frame(mylist)
for(t in 2:9) { dat = data.frame(dat,subset(newdf,inft == "infgen_a99" & analyst == t ))}

dat



for (i in 2:10) { inf99_id[i,t] <- if (analyst == t) ifelse (infdf[i,3] == infdf[i-1,3],0,1) } 

