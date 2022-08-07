#1 başlangıç
#install.packages("readODS")
library("readODS")
data <- read_ods("/home/lv/Desktop/data2")
rownames(data)=paste(c(1:nrow(data)),data[,1]) 
data <- data[, -1]
data <- data[-39,]
data
#1 son


#2 başlangıç
new_data <- data[,1:length(data)]
cordata <- round(cor(new_data),2)
dim(cordata)
for (i in 1:18){
    for (j in 1:18){
        if (cordata[i,j] >= 0.6 | cordata[i,j] <= -0.6){
            cordata[i,j] <- cordata[i,j]}
            else {
            cordata[i,j] <- 0
            }
            }
            }

newcordata <- cordata
#2 son

#3 başlangıç
for (i in 1:18){
  for (j in 1:18){
    if (j > i){
      cordata[i,j] <- cordata[i,j]}
    else {
      cordata[i,j] <- 0
    }
  }
}

cordata
#3 son

#4 başlangıç
scalevec <- 100/colMeans(data)
dscale <- diag(scalevec)
lastofmatrix <- as.matrix(data) %*% as.matrix(dscale)
#4 son

#5 başlangıç
colnames(lastofmatrix) <- c(colnames(data))
lastofmatrix <- cbind(rownames(data), data.frame(lastofmatrix, row.names=NULL))
colnames(lastofmatrix)[1] <- "Country"
lastofmatrix
#5 son

#6 başlangıç
png("/home/lv/Desktop/DWBFCOR.png", h = 600 , w = 600)

plot(lastofmatrix$DbwF...., lastofmatrix$PE..., col=1, pch=1, xlab="Dwellings without basic facilities", ylab="DEĞERLER", frame= FALSE, main = "Dwellings without basic facilities with others", font.main = 4)

legend(500,160, legend=c("Personal earnings","Student skills ","Life expectancy") , pch = 1:3 , col = 1:3)

points(lastofmatrix$DbwF....,lastofmatrix$Sskl..Avg., pch= 2, col = 2)

points(lastofmatrix$DbwF....,lastofmatrix$L.EXP..yrs., pch= 3, col = 3)

dev.off()





png("/home/lv/Desktop/RPPCOR.png", h = 600 , w = 600)

plot(lastofmatrix$RPP..rt.. , lastofmatrix$HNADI.... , col=1, pch=1, xlab="Rooms per person", ylab="DEĞERLER", frame= FALSE, main = "Rooms per person with others", font.main = 4)

legend(60,170, legend=c("Household net adjusted disposable income"," Personal earnings ","Student skills") , pch = 1:3 , col = 1:3)

points(lastofmatrix$RPP..rt.. ,lastofmatrix$Sskl..Avg., pch= 3, col = 3)

points(lastofmatrix$RPP..rt.. ,lastofmatrix$L.EXP..yrs., pch= 2, col = 2)

dev.off()




png("/home/lv/Desktop/HNADICOR.png", h = 600 , w = 600)

plot(lastofmatrix$HNADI.... , lastofmatrix$HNW.... , col=1, pch=1, xlab="Household net adjusted disposable income", ylab="DEĞERLER", frame= FALSE, main = "Household net adjusted disposable income with others", font.main = 4)

legend(60,250, legend=c("Household net wealth"," Personal earnings ","Life satisfaction ", "Rooms per person") , pch = 1:4 , col = 1:4)

points(lastofmatrix$HNADI.... ,lastofmatrix$PE..., pch= 2, col = 2)

points(lastofmatrix$HNADI.... ,lastofmatrix$L.Sat.avg., pch= 3, col = 3)

points(lastofmatrix$HNADI.... ,lastofmatrix$RPP..rt.., pch= 4, col = 4)

dev.off()


png("/home/lv/Desktop/HNWCOR.png", h = 600 , w = 600)

plot(lastofmatrix$HNW.... , lastofmatrix$HNADI...., col=1, pch=1, xlab="Household net wealth", ylab="DEĞERLER", frame= FALSE, main = "Household net wealth with others", font.main = 4)

legend(140,70, legend=c( "Household net adj. disp. income", "Personal earnings ") , pch = 1:2 , col = 1:2)

points(lastofmatrix$HNW.... ,lastofmatrix$PE... ,, pch= 2, col = 2)

dev.off()

png("/home/lv/Desktop/HNADICOR.png", h = 600 , w = 600)

plot(lastofmatrix$LMI.... , lastofmatrix$LU.R...., col=1, pch=1, xlab="Labour market insecurity", ylab="DEĞERLER", frame= FALSE, main = "Labour market insecurity with others", font.main = 4)

legend(10,500, legend=c( "Long-term unemployment rate", "Employment rate") , pch = 1:2 , col = 1:2)

points(lastofmatrix$LMI.... ,lastofmatrix$Em.R...., pch= 2, col = 2) 

dev.off()


png("/home/lv/Desktop/EMRCOR.png", h = 600 , w = 600)

plot(lastofmatrix$Em.R.... , lastofmatrix$LMI...., col=1, pch=1, xlab="Employment rate", ylab="DEĞERLER", frame= FALSE, main = "Employment rate with others", font.main = 4)

legend(100,400, legend=c( "Labour market insecurity", "Long-term unemployment rate", "Water Quality", "Life satisfaction" ) , pch = 1:4 , col = 1:4)

points(lastofmatrix$LMI.... ,lastofmatrix$LU.R...., pch= 2, col = 2) 
points(lastofmatrix$LMI.... ,lastofmatrix$WQ...., pch= 3, col = 3)
points(lastofmatrix$LMI.... ,lastofmatrix$L.Sat.avg., pch= 4, col = 4)

dev.off()



png("/home/lv/Desktop/PECOR.png", h = 600 , w = 600)

plot(lastofmatrix$PE... , lastofmatrix$DbwF...., col=1, pch=1, xlab="Personal Earrings", ylab="DEĞERLER", frame= FALSE, main = "Personal earrings with others", font.main = 4)
 
legend(100,700, legend=c("Dwellings without basic facilities", "Quality of support network" , "Water quality" , "Life expectancy" , "Life satisfaction", "Rooms per person", "Household net wealth", "Household net adj. dis. inc.") , pch = 1:8 , col = 1:8)
 
points(lastofmatrix$PE... ,lastofmatrix$QSN...., pch= 2, col = 2) 
points(lastofmatrix$PE... ,lastofmatrix$WQ...., pch= 3, col = 3)
points(lastofmatrix$PE... ,lastofmatrix$L.EXP..yrs., pch= 4, col = 4)
points(lastofmatrix$PE... ,lastofmatrix$L.Sat.avg., pch= 5, col = 5) 
points(lastofmatrix$PE... ,lastofmatrix$RPP..rt.., pch= 6, col = 6) 
points(lastofmatrix$PE... ,lastofmatrix$HNW...., pch= 7, col = 7) 
points(lastofmatrix$PE... ,lastofmatrix$HNADI...., pch= 8, col = 8) 
 
dev.off()


png("/home/lv/Desktop/QSNCOR.png", h = 600 , w = 600)

plot(lastofmatrix$QSN.... , lastofmatrix$WQ...., col=1, pch=1, xlab="Quality of support network", ylab="DEĞERLER", frame= FALSE, main = "Quality of support network with others", font.main = 4)
 
legend(87,110, legend=c("Water Quality", "Life satisfaction" , "Personal Earrings" ) , pch = 1:3 , col = 1:3)
 
points(lastofmatrix$QSN.... ,lastofmatrix$L.Sat.avg., pch= 2, col = 2) 
points(lastofmatrix$QSN.... ,lastofmatrix$PE..., pch= 3, col = 3)

 
dev.off()



png("/home/lv/Desktop/SSKLCOR.png", h = 600 , w = 600)

plot(lastofmatrix$Sskl..Avg. , lastofmatrix$Em.WLH...., col=1, pch=1, xlab=" Student skills", ylab="DEĞERLER", frame= FALSE, main = "Student skills with others", font.main = 4)
 
legend(95,400, legend=c("Employees working very long hours","Water Quality", "Educational attainment" ) , pch = 1:3 , col = 1:3)
 
points(lastofmatrix$Sskl..Avg. ,lastofmatrix$WQ...., pch= 2, col = 2) 
points(lastofmatrix$Sskl..Avg. ,lastofmatrix$EA...., pch= 3, col = 3)

 
dev.off()



png("/home/lv/Desktop/EACOR.png", h = 600 , w = 600)

plot(lastofmatrix$EA.... , lastofmatrix$Em.WLH...., col=1, pch=1, xlab="Educational attainment", ylab="DEĞERLER", frame= FALSE, main = "Educational attainment with others", font.main = 4)
 
legend(85,400, legend=c("Emp. working very long hours","Student Skills" ) , pch = 1:2 , col = 1:2)
 
points(lastofmatrix$EA.... ,lastofmatrix$Sskl..Avg., pch= 2, col = 2) 

 
dev.off()


png("/home/lv/Desktop/WQCOR.png", h = 600 , w = 600)

plot(lastofmatrix$WQ.... , lastofmatrix$PE..., col=1, pch=1, xlab="Water Quality", ylab="DEĞERLER", frame= T, main = "Water Quality with others", font.main = 4)
 
legend(77,160, legend=c("Personal Earrings","Life satisfaction","Employment rate","Quality of support network","Student Skills" ) , pch = 1:5 , col = 1:5)
 
points(lastofmatrix$WQ.... ,lastofmatrix$L.Sat.avg., pch= 2, col = 2) 
points(lastofmatrix$WQ.... ,lastofmatrix$Em.R...., pch= 3, col = 3) 
points(lastofmatrix$WQ.... ,lastofmatrix$QSN...., pch= 4, col = 4) 
points(lastofmatrix$WQ.... ,lastofmatrix$Sskl..Avg., pch= 5, col = 5) 

 
dev.off()
#6 son

#7 başlangıç
png("/home/lv/Desktop/barplot.png", h = 600 , w = 1200)

barplot(lastofmatrix$DbwF...., names.arg = c(lastofmatrix$Country), las=2)
abline(h=mean(lastofmatrix$DbwF....), col="blue")
 
dev.off()
#7 son

#Ahmet Süheyl Kiriş
