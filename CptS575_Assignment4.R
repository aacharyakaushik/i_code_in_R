library(ggplot2)
auto <- read.csv("https://scads.eecs.wsu.edu/wp-content/uploads/2017/09/Auto.csv", na.strings = "?") 
auto <- na.omit(auto)

#using sapply function to check for the types of variable in dataset 
sapply(auto,class)

Auto_mod <- transform(auto, horsepower = as.numeric(horsepower)) 

# Q 1.a
plot(Auto_mod)

# Q 1.b
Auto_mod_no_name <- (Auto_mod[,-9]) 
cor(Auto_mod_no_name)

# Q 1.c 
Linear = lm(mpg ~. ,data = Auto_mod_no_name) 
summary(Linear)
par(mfrow = c(2,2)) 
plot(Linear)

# Q 1.e
linear_lm_final = lm(mpg~ displacement*weight + weight*cylinders + displacement*cylinders +cylinders :horsepower) 
summary(linear_lm_final)

# Q 1.f
par(mfrow =c(3,2)) 
plot((auto$displacement)^3,auto$mpg, main = "Cubic displacement VS MPG", xlab="Displacement",ylab = "MPG") 
plot(log(auto$displacement),auto$mpg, main = "logaritmic Displacement VS MPG", xlab="Displacement",ylab = "MPG")
plot((auto$weight)^3,auto$mpg, main = "Cubic weight VS MPG", xlab="weight",ylab = "MPG") 
plot(log(auto$weight),auto$mpg, main = "logarithmic weight VS MPG", xlab="weight",ylab = "MPG")
plot((auto$horsepower)^3,auto$mpg, main = "Cubic horsepower VS MPG", xlab="horsepower",ylab = "MPG") 
plot(log(auto$horsepower),auto$mpg, main = "logarithmic horespower VS MPG", xlab="horsepower",ylab = "MPG")


# Q 2.a
library('ISLR') library(MASS)
Boston_lm_1 = lm(crim~zn, data = Boston)
Boston_lm_2 = lm(crim~indus, data = Boston)
Boston_lm_3 = lm(crim~chas, data = Boston)
Boston_lm_4 = lm(crim~nox, data = Boston)
Boston_lm_5 = lm(crim~rm, data = Boston)
Boston_lm_6 = lm(crim~age, data = Boston)
Boston_lm_7 = lm(crim~dis, data = Boston)
Boston_lm_8 = lm(crim~rad, data = Boston)
Boston_lm_9 = lm(crim~tax, data = Boston)
Boston_lm_10 = lm(crim~ptratio, data = Boston)
Boston_lm_11 = lm(crim~black, data = Boston)
Boston_lm_12 = lm(crim~lstat, data = Boston)
Boston_lm_13 = lm(crim~medv, data = Boston) 

# Q 2.b 
Boston_lm = lm(crim~., data=Boston) 
summary(Boston_lm)

# Q 2.c
Boston_lm_uni <- c(coefficients(Boston_lm_1)[2], coefficients(Boston_lm_2)[2], 
                   coefficients(Boston_lm_3)[2], coefficients(Boston_lm_4)[2], 
                   coefficients(Boston_lm_5)[2], coefficients(Boston_lm_6)[2], 
                   coefficients(Boston_lm_7)[2], coefficients(Boston_lm_8)[2], 
                   coefficients(Boston_lm_9)[2], coefficients(Boston_lm_10)[2], 
                   coefficients(Boston_lm_11)[2], coefficients(Boston_lm_12)[2], 
                   coefficients(Boston_lm_13)[2]) 
Boston_lm_multi <- c(coefficients(Boston_lm)[-1]) 
plot(Boston_lm_uni,Boston_lm_multi)

# Q 2.d
Boston_poly_1 = lm(crim~poly(zn,3),data = Boston)
Boston_poly_2 = lm(crim~poly(indus,3),data = Boston)
Boston_poly_4 = lm(crim~poly(nox,3),data = Boston)
Boston_poly_5 = lm(crim~poly(rm,3),data = Boston)
Boston_poly_6 = lm(crim~poly(age,3),data = Boston)
Boston_poly_7 = lm(crim~poly(dis,3),data = Boston)
Boston_poly_8 = lm(crim~poly(rad,3),data = Boston)
Boston_poly_9 = lm(crim~poly(tax,3),data = Boston)
Boston_poly_10 = lm(crim~poly(ptratio,3),data = Boston)
Boston_poly_11 = lm(crim~poly(black,3),data = Boston)
Boston_poly_12 = lm(crim~poly(lstat,3),data = Boston)
Boston_poly_13 = lm(crim~poly(zn,3),data = Boston) 


