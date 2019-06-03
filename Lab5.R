#setwd("~/LAB")
#dir.create("bigData_lab6")
setwd("/home/gehad/bigData_lab6")
getwd()
rm(list=ls())

#=============================Part(1)=====================================
x <- runif(100, 0, 10)     # 100 draws between 0 & 10
x

#(Q1) Try changing the value of standard deviation (sd) in the next command 
#How do the data points change for different values of standard deviation?
y <- 5 + 6*x + rnorm(100, sd = 2)  # default values for rnorm (mean = 0 and sigma = 1)

#Plot it
plot (x,y)

# OLS model
# OLS : Ordinary Least Squares
model1 <- lm(y ~ x)
# Learn about this object by saying ?lm and str(d)

# Compact model results
print(model1)
#(Q2) How are the coefficients of the linear model affected by changing the value
#of standard deviation in Q1?

# Regression diagnostics --
ypred <- predict(model1) # use the trained model to predict the same training data
# Learn about predict by saying ?predict.lm

par(mfrow=c(1,1))
plot(y,y, type="l", xlab="true y", ylab="predicted y") # ploting the ideal line
points(y, ypred) # plotting the predicted points
                 # the nearer to the ideal line the better

# Detailed model results
d1 <- summary(model1)
print(model1)

#(Q3) How is the value of R-squared affected by changing the value
#of standard deviation in Q1?

# Learn about this object by saying ?summary.lm and by saying str(d)
cat("OLS gave slope of ", d1$coefficients[2,1],   
    "and an R-sqr of ", d1$r.squared, "\n")

#Graphic dignostic (cont.)
par(mfrow=c(1,1)) # parameters for the next plot
plot(model1, 1) # plot 4 diagnostic graphs

#(Q4)What do you conclude about the residual plot? Is it a good residual plot?
#========================End of Part(1)==============================================

#========================Part(2)=====================================================
#Training a linear regression model
x1 <- runif(100) 
# introduce a slight nonlinearity
#(A)
y1 = 5 + 6*x1 + 20*x1*x1 + rnorm(100)
plot(x1,y1)
model <- lm(y1 ~ x1)

summary(model)

#Creating a test set (test vector)
xtest <- runif(100)
#(B)
ytest = 5 + 6*xtest  + 20*xtest*xtest + rnorm(100) # same equation of y1 but on xtest to get true y for xtest

ypred <- predict(model ,data.frame(xtest))

par(mfrow=c(1,1))
plot(ytest,ypred, type="l", xlab="true y", ylab="predicted y")
points(ytest, ypred)

# graphic dignostic (cont.)
par(mfrow=c(1,1)) # parameters for the next plot
plot(model, 1) # plot 4 diagnostic graphs

#(Q5)What do you conclude about the residual plot? Is it a good residual plot?

#(Q6)Now, change the coefficient of the non-linear term in the original model for (A) training 
#and (B) testing to a large value instead. What do you notice about the residual plot?
#===============================End of Part(2)=============================================

#=================================Part(3)==================================================
#(Q7) Import the dataset LungCapData.tsv. What are the variables in this dataset?
LungCap_df <- read.table(file = 'LungCapData.tsv', sep = '\t', header = TRUE)
View(LungCap_df)

#(Q8) Draw a scatter plot of Age (x-axis) vs. LungCap (y-axis). Label x-axis "Age" and y-axis "LungCap"
plot(LungCap_df$Age,LungCap_df$LungCap, xlab="Age", ylab="LungCap")

#(Q9) Draw a pair-wise scatter plot between Lung Capacity, Age and Height. 
#Check the slides for how to plot a pair-wise scatterplot
pairs(LungCap_df[,c(1:3)],col="dark blue")

#(Q10) Calculate correlation between Age and LungCap, and between Height and LungCap.
#Hint: You can use the function cor
cor_Age_Lung <- cor(LungCap_df$Age,LungCap_df$LungCap)
cor_Age_Lung
cor_Height_Lung <- cor(LungCap_df$Height,LungCap_df$LungCap)
cor_Height_Lung

#(Q11) Which of the two input variables (Age, Height) are more correlated to the 
#dependent variable (LungCap)?

#(Q12) Do you think the two variables (Height and LungCap) are correlated ? why ?

#(Q13) Fit a liner regression model where the dependent variable is LungCap 
#and use all other variables as the independent variables

model <- lm(LungCap ~ Age+Height+Smoke+Gender+Caesarean,data=LungCap_df)

#(Q14) Show a summary of this model
summ <- summary(model)

#(Q15) What is the R-squared value here ? What does R-squared indicate?
summ$r.squared

#(Q16) Show the coefficients of the linear model. Do they make sense?
#If not, which variables don't make sense? What should you do?

#(Q17) Redraw a scatter plot between Age and LungCap. Display/Overlay the linear model (a line) over it.
#Hint: Use the function abline(model, col="red").
#Note (1) : A warning will be displayed that this function will display only the first two 
#           coefficients in the model. It's OK.
#Note (2) : If you are working correctly, the line will not be displayed on the plot. Why?
plot(LungCap_df$Age,LungCap_df$LungCap, xlab="Age", ylab="LungCap",xlim=c(0,90),ylim=c(-15,15)); abline(model, col="red")
#plot(LungCap_df$Age,LungCap_df$LungCap, xlab="Age", ylab="LungCap"); abline(model, col="red")


#(Q18)Repeat Q13 but with these variables Age, Smoke and Cesarean as the only independent variables.

model <- lm(LungCap ~ Age+Smoke+Caesarean,data=LungCap_df)

#(Q19)Repeat Q16, Q17 for the new model. What happened?
summ <- summary(model)
summ
plot(LungCap_df$Age,LungCap_df$LungCap, xlab="Age", ylab="LungCap",xlim = c(1,20)); abline(model, col="red")

#(Q20)Predict results for this regression line on the training data.
ypred <- predict(model)

#(Q21)Calculate the mean squared error (MSE)of the training data.
 #install.packages("Metrics")
 library(Metrics)
 mse(LungCap_df$LungCap,ypred)
 
