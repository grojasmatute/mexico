

inf205_id2 <- subset(newdf,inft == "infgen_a06" & variable =="infgentmas1" &  analyst == 2 )
inf06_id2 <- subset(newdf,inft == "infgen_a06" & variable =="infgent" &  analyst == 2 )

inf02_0506 <-data.frame(rbind(inf205_id2, inf06_id2))
ggplot(data = inf02_0506, aes( x= dat0506_02, y = inf02_0506$pointf) ) + geom_line() 

dat0506_02 <- as.Date(seq(ISOdate(2005,1,1), by = "month", length.out = 24), format = "%d/%m/%Y")


inf205_id4 <- subset(newdf,inft == "infgen_a06" & variable =="infgentmas1" &  analyst == 4 )
inf06_id4 <- subset(newdf,inft == "infgen_a06" & variable =="infgent" &  analyst == 4 )
inf04_0506 <-data.frame(rbind(inf205_id4, inf06_id4))

inf205_id8 <- subset(newdf,inft == "infgen_a06" & variable =="infgentmas1" &  analyst == 3 )
inf06_id8 <- subset(newdf,inft == "infgen_a06" & variable =="infgent" &  analyst == 3 )
inf08_0506 <-data.frame(rbind(inf205_id8, inf06_id8))
inf08_0506

inf206 <- NULL
inf_matrix06 <- matrix()
time06 <- as.Date(seq(ISOdate(2005,1,1), by = "month", length.out = 24), format = "%d/%m/%Y")


colnames(idpoint06) <- c(1:ncol(idpoint06))
colnames(infidpoint0506) <- c(1:ncol(infidpoint0506))



dfinf06 <- data.frame(rbind(idpoint06, infidpoint0506))
idpoint06
infidpoint0506

plot(time06,dfinf06[,37], col = "blue2", lwd=2, type = "l" )
plot(time06,dfinf06[,4], type = "l", col = "black", xlab = "Time Horizon (2005-2006)", ylab = "2006 Inflation Forecast")
lines(time06,dfinf06[,44], col = "red" )
lines(time06,dfinf06[,24], col = "blue" )
lines(time06,dfinf06[,34], col = "green" )
lines(time06,cons0506, col = "black", lty = 2 )
legend(2010-01-01,4.75, legend =c(4,44,24,34), col = c("black", "red", "blue", "green"), lty=c(1,1,1,1), cex=0.8)



cons0506 <- c(inf2cons05, cons06)
anualinf$SP30578[14:25]





plot(dat0506_02[2:24],inf02_0506$pointf[2:24], type = 'l')
lines(dat0506_02[2:24],inf04_0506$pointf, col = "red" )
       
    
