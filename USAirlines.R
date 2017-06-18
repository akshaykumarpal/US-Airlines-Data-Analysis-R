library("faraway")
library("ggplot2")
library("gridExtra")
library("GGally")
library("ellipse")
library("car")
library("MASS")
library("robustbase")
library("scatterplot3d")
library("quantreg")
library("DAAG")
library("mice")
library("forecast")
library("TTR")
options(scipen=999)

data(USAirlines, package="AER")
air= USAirlines

#Data Predictors and Response Variables.

#firm -> factor indicating airline firm.
#year -> factor indicating year.
#output  ->output revenue passenger miles index number.
#cost -> total cost (in USD 1000).
#price -> fuel price.
#load -> average capacity utilization of the fleet.

# Store the base plot
plot <- qplot(output,cost, data = air,xlab = "Output",  ylab = "Cost", main = "US Airlines Data")

plot1 <- plot + stat_smooth(method = "lm")
plot2 <- plot + stat_smooth(se=F)

grid.arrange(plot1,plot2)

# convert all the variables with 0 coded for missing value
air$firm[air$firm == 0] <- NA
air$year[air$year == 0] <- NA
air$output[air$putput == 0] <- NA
air$cost[air$cost == 0] <- NA
air$price[air$price == 0] <- NA
air$load[air$load==0] <- NA

# pattern of missing data
md.pattern(air)

# Removing missing values
air <- na.omit(air)

# Print structure, summary statistics and first rows of data
str((air))
g <- summary(air)
names(g)


##Understanding relationships between two continuous or more variables
# Histogram
library("ggplot2")
plot1 <- histogram(air$output, data = air, binwidth=I(5)) 
# Kernal Density Plot
plot2 <- qplot(air$output, data = air, geom="density")
# Normal QQ-Plot
plot3 <- qplot(sample=air$output, data = air) 
# Plot all three in one graph
library("gridExtra")
grid.arrange(plot1, plot2, plot3, ncol=3) 
# Overlay plots
ggplot( air, aes( x = output, y =..density..)) +  
  geom_histogram( fill =" cornsilk", colour =" grey60",  size =.2, binwidth=3) +
  geom_density() 
#  Dot Plot
p <- ggplot(data = air, aes( x = output))
p + geom_dotplot(method="histodot", binwidth = 0.75) +
  ggtitle("Dot Plot") +scale_y_continuous(NULL, breaks = NULL)
# Boxplots
boxplot(air$output, 
        ylab = "output", 
        main = "Box Plot")
## Stripcharts
numeric_data <- air[,c(1:6)]
##numeric_data <- data.frame(scale(numeric_data ))
stripchart(numeric_data, vertical = TRUE,  method = "jitter", 
  col = "green", pch=1,main="Stripcharts")
## Scatterplot
plot1 <- qplot(output, cost, data=air, alpha=I(1/2))
## Boxplot
plot2<- qplot(firm, cost, data=air, geom=c("boxplot"))
## Jitter plot
plot3 <- qplot(firm, cost, data=air, geom=c("jitter"), alpha=I(1/2))
## Opaque plot
plot4 <- qplot(output, cost, data=air, color=firm, alpha=I(1/2))
grid.arrange(plot1, plot4, plot2, plot3, nrow=2, ncol=2)


plot1 <- qplot(output, data = air, binwidth=I(5)) 
# Kernal Density Plot
plot2 <- qplot(output, data = air, geom="density")
# Normal QQ-Plot
plot3 <- qplot(sample=output, data = air) 


# Plot all three in one graph
grid.arrange(plot1, plot2, plot3, ncol=4) 

##corrleation Analysis

#Scatterplot Matrix
pairs(~ ., data=air)

#Showing Pearson Corelation
ggpairs(subset(air,select=-c(firm, year)), columns = c(2:4, 1))

air1=subset(air,select=-c(firm,year))

cor(air1, use="complete.obs", method="kendall") 
cor(air1, use="complete.obs", method="pearson") 
cor(air1, use="complete.obs", method="spearman") 

#Regression Analysis

#Fit Linear Model
#Model 1
g <- lm(cost ~ output + price + load+firm+year, data = air) 
summary(g)

#The Residual Standard error is very high so we will analyse the skewness of the predictors.
#Histograms Plot


#Fitting again.
fit <- lm(log(cost) ~ log(output) + log(price) + load+firm+year, data = air) 
summary(fit)

coef(fit)[1:4]

attach(air)
oldpar <- par(mfrow=c(1,2))
plot( density(cost), main = "normal curve overlay")
qqnorm(cost, pch=".", cex=2, main="Normal Probability QQ Plot")
qqline(cost)
par(oldpar)


#Fit Plot
p<-qplot(fitted.values(fit), log(cost), data=air)
p+geom_abline(intercept=0, slope=1)

#Residual and Deviance
# measure of error in fitting with the model is the error, deviation
Deviance <- deviance(fit)
Deviance


#R square
summary(fit)$r.square
c(summary(fit)$r.square, cor(fitted.values(fit), log(air$cost))^2)

#Interaction Model
ggpairs(air, mapping = aes(color = firm))

#Residual Plot
ggplot(fit, aes(.fitted, .resid)) +geom_point() +
  geom_hline(yintercept=0, color="dark blue", linetype="dashed") +
  ggtitle("Residual Plot")


#Exploring Model Structure
cor(fit$resid,log(air$cost))

plot1 <- qplot(load, fit$resid, geom = "boxplot", data=air) +
  geom_hline(yintercept=0, color="dark blue", linetype="dashed")
plot2 <- qplot(log(price), fit$resid, data=air) +
  geom_hline(yintercept=0, color="dark blue", linetype="dashed")
plot3 <- qplot(log(cost), fit$resid, data=air) +
  geom_hline(yintercept=0, color="dark blue", linetype="dashed")
plot4 <- qplot(log(output), fit$resid, data=air) +
  geom_hline(yintercept=0, color="dark blue", linetype="dashed")

grid.arrange(plot1, plot2, plot3, plot4, nrow=2)


#Normality of the Residuals
mod <- fortify(fit)
plot1 <- qplot(.stdresid, data=mod, geom = "histogram")
plot2 <- qplot(.stdresid, data = mod, geom = "density")
plot3 <- qplot(sample =.stdresid, data = mod, geom = "qq") + geom_abline()

grid.arrange(plot1, plot2, plot3, nrow = 1)


mod <- fortify(fit)
plot1 <- qplot(.fitted, .stdresid, data=mod) + 
  geom_hline(yintercept = c(-2, 0, 2), color = c(2,1,2), linetype="dashed")
plot2 <- qplot(sample =.stdresid, data = mod, geom = "qq") + geom_abline()

grid.arrange(plot1, plot2, nrow = 1)

##Hypothesis Testing

#Test Statistic with z-score
#Based on the data, we calculate a test statistic to judge wheather the null hypothesis is false
#To see whether the null hypothesis is false, we need the probable behavior of b1b1b1
#he standard error of the estimate b1 is ??(b1), in other words, it is by definition the conditional standard deviation of the random variable b1
x <- air$output 
SS_xx <- sum(sum((x-mean(x))^2))
SS_xx

#Decision Rule and Risk
dlm <- lm(log(cost) ~  log(output), data = air)
#We now can use the probable behavior of the t-valuet-valuet-value t(b1)t(b1)t(b1) statistic to determine if H0H0H0 is false
#We find the 95%95%95% probable interval from the 0.0250.0250.025 and 0.9750.9750.975 quantiles of the US Airlines distribution
lwr <- qt(0.025, dlm$df.residual)
upr <- qt(0.975, dlm$df.residual)
c(lwr, upr)
#So if the t(b1)t(b1)t(b1) falls outside this interval, we could decide that H0H0H0 is false

dlm.sum <- summary(dlm)
(dlm.sum$coefficients)
#We see that t(b1)t(b1)t(b1) = 29.67709, and we conclude that H0:??1=0 is false at ??=0.01 level of significance

#Comparing Models
anova(fit)
#The Residual Sum of Squares is 0.177
fit2 <- lm(log(cost) ~ log(output) + log(price) + load, data = air) 
anova(fit2)

#We see that Sum Sq value is larger than the one we get with the big model, by the amount .116+1.043

#Testing for two or more variables at once
anova(fit2,fit)

#Conclusion: The F-Ratio 23.102 is big, therefore take Model 2 (the big model) since p-value 0.0000000000000022 is less than 0.05

#Testing Subspaces
g.bg <- lm(log(cost) ~ log(output)+log(price)+load+year+firm, air) 
g.sm <- lm(log(cost) ~ I(log(output)+log(price))+load, air)
anova(g.sm, g.bg)

#Conclusion: The F-Ratio 115.18 is big, therefore accept that the coefficents for output and price are the same (Model 1) and accept the big model (Model 2) since the p-value 0.0000000000000002 is less than 0.05

#Confidence Intervals
confint(fit)[1:4,]
confint(fit,level=.90)[1:4,]


coef(fit)["log(output)"]
coef(fit)["log(price)"]

# Joint Confidence Region

s3d <- scatterplot3d(log(output),log(price),pch=16, highlight.3d=TRUE, type="h", main="3D Scatterplot")

#CI for Response Value from Output Predictor
new <- data.frame(output =log(air$output))
rbind(predict(fit, newdata = new, interval="confidence"),
      predict(fit,newdata=new, interval="prediction"))[3:6,]

#Checking for nonconstant variance
modgg <- fortify(g)

p1 <- qplot(.fitted, .resid, data = modgg) + geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") + geom_smooth(color = "red", 
                                                                                   se = F)
p2 <- qplot(.fitted, abs(.resid), data = modgg) + geom_hline(yintercept = 0, 
                                                             linetype = "dashed") + labs(title = "Scale-Location", x = "Fitted", y = "|Residuals|") + 
  geom_smooth(method = "lm", color = "red", se = F)

grid.arrange(p1, p2, nrow = 2)

#An approximate test of noncontant error variance.
summary(lm(abs(residuals(g)) ~ fitted(g)))

#An F test for nonconstant error variance between two groups defined by a predictor
modgs <- fortify(fit)
p1 <- qplot(log(output), .resid, data = modgs, color = firm)
p2 <- qplot(log(price), .resid, data = modgs, geom = "jitter")
grid.arrange(p1, p2, nrow = 2)


#Variance Stabilizing Transformation
modgg <- fortify(g)
modgs <- fortify(fit)
p1 <- qplot(.fitted, .resid, data = modgg) + geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") + geom_smooth(color = "red", 
                                                                                   se = F)
p2 <- qplot(.fitted, abs(.resid), data = modgg) + geom_hline(yintercept = 0, 
                                                             linetype = "dashed") + labs(title = "Scale-Location", x = "Fitted", y = "|Residuals|") + 
  geom_smooth(method = "lm", color = "red", se = F)
p3 <- qplot(.fitted, .resid, data = modgs) + geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") + geom_smooth(color = "red", 
                                                                                   se = F)
p4 <- qplot(.fitted, abs(.resid), data = modgs) + geom_hline(yintercept = 0, 
                                                             linetype = "dashed") + labs(title = "Scale-Location", x = "Fitted", y = "|Residuals|") + 
  geom_smooth(method = "lm", color = "red", se = F)
grid.arrange(p1, p2, p3, p4, nrow = 2)


#Approxmiate Test for Non Constant Variance
summary(lm(abs(residuals(g)) ~ fitted(g)))

summary(lm(abs(residuals(fit)) ~ fitted(fit)))


#Checking for Non Normal Errors

p1 <- qplot(sample = scale(.resid), data = modgg) + geom_abline(intercept = 0, 
                                                                slope = 1, color = "red") + labs(title = "Untransformed y", y = "Residuals")
p2 <- qplot(sample = scale(.resid), data = modgs) + geom_abline(intercept = 0, 
                                                                slope = 1, color = "red") + labs(title = "Log-Tranformed y", y = "Residuals")
grid.arrange(p1, p2, nrow = 2)


#Histogram, Kernel Density

p1 <- qplot(scale(.resid), data = modgg, geom = "blank") + geom_line(aes(y = ..density.., 
                                                                         colour = "Empirical"), stat = "density") + stat_function(fun = dnorm, aes(colour = "Normal")) + 
  geom_histogram(aes(y = ..density..), alpha = 0.4) + scale_colour_manual(name = "Density", 
                                                                          values = c("red", "blue")) + theme(legend.position = c(0.85, 0.85)) + labs(title = "Untransformed y", 
                                                                                                                                                     y = "Residuals")
p2 <- qplot(scale(.resid), data = modgs, geom = "blank") + geom_line(aes(y = ..density.., 
                                                                         colour = "Empirical"), stat = "density") + stat_function(fun = dnorm, aes(colour = "Normal")) + 
  geom_histogram(aes(y = ..density..), alpha = 0.4) + scale_colour_manual(name = "Density", 
                                                                          values = c("red", "blue")) + theme(legend.position = c(0.85, 0.85)) + labs(title = "Log-Tranformed y", 
                                                                                                                                                     y = "Residuals")
grid.arrange(p1, p2, nrow = 2)


#Wilk Test
shapiro.test(residuals(g))
shapiro.test(residuals(fit))

#Box-Cox Transform
(lambda <- powerTransform(g))

lam <- lambda$lambda
glam <- lm(log(cost)^lam ~ log(output) + log(price) + load+firm+year, data = air) 
modlam <- fortify(glam)
p1 <- qplot(sample = scale(.resid), data = modgs) + geom_abline(intercept = 0, 
                                                                slope = 1, color = "red") + labs(title = "Normal QQ-Plot", y = "Residuals log-transformed")
p2 <- qplot(sample = scale(.resid), data = modlam) + geom_abline(intercept = 0, 
                                                                 slope = 1, color = "red") + labs(title = "Normal QQ-Plot", y = "Residuals Box-Cox-Transform")
grid.arrange(p1, p2, nrow = 1)

#Influential Outliers
s3d <- scatterplot3d(log(output), log(price), log(cost), pch = 16, highlight.3d = TRUE, 
                     type = "h", main = "3D Scatterplot")
fits <- lm(log(cost) ~ log(output) + log(price))
s3d$plane3d(fits)

s3d <- scatterplot3d(log(output), log(price), log(cost)^lam, pch = 16, highlight.3d = TRUE, 
                     type = "h", main = "3D Scatterplot")
fitt <- lm(log(cost)^lam ~ log(output) + log(price))
s3d$plane3d(fitt)

#Influential Plot
influencePlot(fit)



p <- qplot(air$output, air$cost, xlab = "log(output)", ylab = "log(cost)")
ga <- lm(log(cost) ~ log(output), air)

p <- p + geom_smooth(method = "lm", se = F)
gb <- lm(log(cost) ~ log(output), air, subset = ((output) > 1))
p + geom_abline(intercept = coef(gb)[1], slope = coef(gb)[2], colour = "green")
p

plane<- row.names(fit)
halfnorm(lm.influence(glam)$hat, labs = plane, ylab = "Leverages")
cook <- cooks.distance(fit)
halfnorm(cook, 3, labs = plane, ylab = "Cook's distance")

glam1 <- lm(log(cost)^lam ~ log(output) + log(price) + load+firm+year, data = air, 
            subset = (cook < max(cook)))

compareCoefs(glam, glam1)[1:4,]


#Partial residual plot for checking model structure
ceresPlots(fit, terms = ~.)

#Checking colinearity in the model
round(cor(subset(air,select=-c(firm,year))), 1)

vif(fit)

fit2<- lm(log(cost) ~ log(output) + log(price) + load, air)

anova(fit2, fit)

#The small model is not rejected with a significance level of 10% since 
#the p-value, 0.73, is greater than 0.10.

#Auto corelation 
windows()
par(mfrow = c(2, 2))
acf(air$cost)
acf(air$output)
acf(air$price)
par(mfrow = c(1, 1))

gols <- lm(log(cost) ~ log(output) + log(price)+load, air)
summary(gols)
shapiro.test(residuals(gols))
car::durbinWatsonTest(residuals(gols))
car::vif(gols)
windows()
par(mfrow = c(2, 2))
plot(gols)


dev.off()
qplot(air$year, residuals(gols)) + geom_line() + geom_smooth(se = F) + geom_hline(yintercept = 0)

windows()
oldpar <- par(mfrow=c(2,2))
acf(residuals(g))
acf(residuals(fit))
acf(residuals(gols))
par(oldpar)


#GLS Fit using maximum likelihood.
library(nlme)
g1 <- gls(log(cost) ~ log(output) + log(price)+load)
summary(g1)
shapiro.test(residuals(g1))
intervals(g1)


plot(ACF(g1), alpha = 0.1)


#Weighted Least Squares

#Remedy for Influential Data: Robust Regression
library(car)
outlierTest(fit)
windows()
qqPlot(fit, main = "QQ Plot")

shapiro.test(residuals(fit))
# The Shapiro-Wilk test rejects normality of errors
windows()
ceresPlots(fit, terms = ~.)  # CERES plots

#Huber M-estimation
g2 <- rlm(log(cost) ~ log(output) + log(price) + load+firm+year, data = air)
#Tukey Bisquare M-estimation
g3 <- rlm(log(cost) ~ log(output) + log(price) + load+firm+year, psi = psi.bisquare, 
          init = "lts", maxit = 100, air)
#Hample M-estimation
g4 <- rlm(log(cost) ~ log(output) + log(price) + load+firm+year, psi = psi.hampel, 
          init = "lts", maxit = 100, air)
#Least Trimmed Squares (LTS)
g5 <- ltsReg(log(cost) ~ log(output) + log(price) + load+firm+year, data = air)
#Least Absolution Deviation (LAD)
g7 <- rq(log(cost) ~ log(output) + log(price) + load+firm+year, data = air)

coefs <- compareCoefs(fit, g2, g3, g4, g5, g7, se = FALSE)

windows()
plot(log(cost) ~ log(output) , data = air)
abline(lm(log(cost) ~ log(output), data=air)$coef, col = 1)  # LS
abline(rq(log(cost) ~ log(output), data = air)$coef, col = 2)  # LAD
abline(rlm(log(cost) ~ log(output), psi = psi.huber, init = "lts", data=air)$coef, col = 3)  # Huber
abline(rlm(log(cost) ~ log(output), psi = psi.bisquare, init = "lts", data=air)$coef, col = 4)  #Tukey Bi-square
abline(ltsReg(log(cost) ~ log(output), air)$coef, col = 5)  # LTS
legend("bottomleft", c("LS", "LAD", "Huber", "Bisquare", "LTS"), col = c(1, 2, 3, 4, 5), inset = 0.04, lty = 1)

#Model Selection One at a time
m1 <- update(fit, . ~ . - air)
summary(m1)

#We will eliminate Price becuase of highest p value among the output and load.
m2 <- update(m1, . ~ . - log(price))
summary(m2)

#There are no more variables with p-values higher than 0.05.
#The final model is stored in the object m2.

#Stepwise
stepfit<- step(fit)

compareCoefs(m1,m2,stepfit)[1:4,]


df <- data.frame(mse.g1=NULL, mse.g2=NULL)
seed <- round(runif(1, min=0, max=100))
oldpar <- par(mfrow=c(1,2))  
mse.g1 <- CVlm(data = air, form.lm=fit, m=3, seed=seed, printit=F,main = "g")
mse.g2 <- CVlm(data = air, form.lm=stepfit, m=3, seed=seed,printit=F, main="stepfit")
par(oldpar)
df.temp <- data.frame(mse.g1=attr(mse.g1, "ms"), mse.g2=attr(mse.g2, "ms"))
df.temp


#Normal Distribution
normal.lm <- lm(log(cost) ~ log(output)+log(price)+load+firm+year, data = air)
p + geom_line(aes(log(output), fitted(normal.lm)), color = "orange", size = 1)

#Normal Distribution with log Link
log.normal.glm <- glm(log(cost) ~ log(output)+log(price)+load+firm+year, data = air, family = "gaussian"(link = "log"))
p + geom_line(aes(log(output), fitted(log.normal.glm)), color = "red", size = 1)

#Poisson Distribution
pois.glm <- glm(log(cost) ~ log(output)+log(price)+load+firm+year, family = poisson(link = "log"), data = air)
p + geom_line(aes(log(output), fitted(pois.glm)), color = "blue", size = 1)

#Binomial Distribution

bin.glm <- glm(cbind(log(cost), log(cost)-log(output)) ~ load+log(price)+ factor(firm)+ factor(year) , data = air,family = binomial(link="logit"))
p + geom_line( aes(log(output), fitted(bin.glm)), color = "purple", size = 1)
