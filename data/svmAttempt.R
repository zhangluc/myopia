install.packages("e1071")
library(e1071)
install.packages("caret")
library(caret)

# Load your dataset
data <- read.csv("myopia_og.csv", head = TRUE, sep = ";")
head(data)

mydata <- data.frame(SPHEQ= data$SPHEQ,
                     SPORTHR= data$SPORTHR,
                     MYOPIC = as.factor(data$MYOPIC))


mydata
#plot(mydata[,-3], col=(3)/2, pch = 19); abline(h=0, v=0, lty=3)
plot(mydata$SPHEQ, mydata$SPORTHR, col = as.integer(mydata$MYOPIC) + 1, pch = 19)
abline(h = 0, v = 0, lty = 3)

model <- svm(MYOPIC ~ SPHEQ + SPORTHR, 
             data= mydata,
             type= "C-classification",
             kernel = "radial",
             scale = TRUE)

summary(model)

#points(mydata[model$index, c(1,2)], col = "orange", cex = 2)

#w <- t(model$coefs)%*%model$SV
#b <-  -model$rho
#abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col = "blue", lty = 3)

points(mydata[model$index, c(1, 2)], col = "orange", cex = 1)

#w <- t(model$coefs) %*% model$SV
#b <- -model$rho
#abline(a = -b/w[2], b = -w[1]/w[2], col = "blue", lty = 3)

w <- t(model$coefs) %*% model$SV
b <- -model$rho

# Print values of w and b to ensure they are reasonable
print(w)
print(b)

# Calculate slope and intercept for abline
slope <- -w[1]/w[2]
intercept <- -b/w[2]

# Plot the decision boundary
abline(a = intercept, b = slope, col = "blue", lty = 3)


observations <- data.frame

