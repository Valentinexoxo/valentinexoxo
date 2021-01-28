#We disable warnings in advance for brevity; this isn't normally recommeded
defaultW <- getOption("warn") 
options(warn = -1) 

#You may need to install the package first before viewing the data:  install.packages("mlbench")
library(mlbench)
data(BostonHousing2)

#view the first few rows of the dataset as well as its properties
head(BostonHousing2)
str(BostonHousing2)

#below a copy of the dataset is made while converting the town and chas variables to numeric
corr_data <- BostonHousing2
corr_data$town <- as.numeric(corr_data$town)
corr_data$chas <- as.numeric(corr_data$chas)

#You may need to install the correlation package first:  install.packages("corrplot")
library(corrplot)

#Here, we round the correlation matrix to 1 decimal place only so it can fit for plotting purposes
#we also purposes "white-out" negative correlations since we are only searching for positive correlations
corr_Matrix <- round(cor(corr_data), 1)
sexy_plot <- corrplot(corr_Matrix, method = "number", 
                      title = "Sexy Correlation Plot",
                      mar=c(0,0,1,0), #this is to correct the title position in the graph
                      type="lower", order="hclust", 
                      col=c("white", "black"),
                      bg="white", tl.srt=45)

#You may need to install the ggplot2 package first:  install.packages("ggplot2")
library(ggplot2)
sexy_plot2 <- ggplot(corr_data, aes(x=medv, y=zn)) + geom_point() +
                geom_smooth(method=lm, color="red", fill="#69b3a2", se=TRUE) 
sexy_plot2

sexy_plot3 <- ggplot(corr_data, aes(x=medv, y=tract)) + geom_point() +
                geom_smooth(method=lm, color="red", fill="#69b3a2", se=TRUE) 
sexy_plot3

#create a linear regression model
linearMod <- lm(medv ~ zn, data=BostonHousing2)
print(linearMod)

#check the linear regression model
summary(linearMod)

# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(linearMod)

#create a linear regression model
linearMod2 <- lm(medv ~ chas, data=BostonHousing2)
print(linearMod2)

#check the linear regression model
summary(linearMod2)

# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(linearMod2)

#create a linear regression model
linearMod3 <- lm(medv ~ zn + chas, data=BostonHousing2)
print(linearMod3)

#check the linear regression model
summary(linearMod3)

# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(linearMod3)

#create a linear regression model
linearMod4 <- lm(medv ~ rm,, data=BostonHousing2)
print(linearMod4)

#check the linear regression model
summary(linearMod4)

# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(linearMod4)

#create a linear regression model
linearMod5 <- lm(medv ~ zn + chas + rm,, data=BostonHousing2)
print(linearMod5)

#check the linear regression model
summary(linearMod5)

# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(linearMod5)
