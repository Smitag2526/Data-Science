#A F&B manager wants to determine whether there is any significant difference in the diameter of the
#cutlet b/w two units. A randomly selected sample of cutlets was collcted from both units and measured.
#analyze the data and draw inferences at 5% significance level. please state the assumption and tests that
#you are carried out to check validity of assumption"""

#research question:- is there any significance difference in diameter of ctlets of two units?
library(stats)
cutlets<-read.csv(file.choose())
attach(cutlets)

#checking for normality of data by shapiro.test
#Ho:data is not normal
#Ha:data is  normal

shapiro.test(Unit.A)

#since p-value is 0.32 imdicates that unit.A is normaly distributed

shapiro.test(Unit.B)
#since p-value is 0.5525 indicates that unit.b is normaly distributed

#to apply which type of two sample test we have to check varibles have equal varriances or not

#H0:the 2 varince are equal 
#Ha:variance are not equal

var.test(Unit.A,Unit.B,alternative="two.sided")

#since p-value is 0.31, Do not reject H0. so we can say that their varience are equal and we can apply 2-sample test for
#equal varience

#H0:diameter(unit.a cutlet)= diameter(unit.b cutlet)
#Ha:diameter(unit.a cutlet)!=diameter(unit.b cutlet)

t.test(Unit.A,Unit.B,alternative = 'two.sided',conf.level = 0.95,var.equal = TRUE)
#since p-value 0.47,do not reject H0. so we can conclude that diameter of cutlets of both units are same
