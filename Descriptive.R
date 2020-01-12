
airquality <- datasets::airquality

head(airquality,5)

tail(airquality,3)
summary(airquality)


summary(airquality$Wind)
summary(airquality[,3])

plot(airquality$Ozone,airquality$Wind)
plot(airquality)

plot(airquality$Ozone,type="b")#p:points,l:lines,b:both
plot(airquality$Ozone, xlab='ozone concentration',ylab="No.of instances", main = 'Ozone levels in NY city',col='blue')

barplot(airquality$Ozone, xlab = "Ozone concentration",ylab = "No.of instances",main = "Ozone levels in NY city",col='green')

barplot(airquality$Ozone, xlab = "Ozone concentration",ylab = "No.of instances",main = "Ozone levels in NY city",col='green',horiz = T)

hist(airquality$Solar.R)

hist(airquality$Solar.R, xlab = "Ozone concentration",ylab = "No.of instances",main = "Ozone levels in NY city",col='blue')

boxplot(airquality[,1:4],main="Multiple box plots")
