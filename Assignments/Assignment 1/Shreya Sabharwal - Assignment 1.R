# Assignment 2
setwd("D:/UW/Quarter 2/INFX 573/Assignment")

felix <- read.csv("FelixHernandez2015.csv")
str(felix)
felix[1:5,]
# 2(a) starts
nrow(felix[felix$W==1,])
# 2(a) ends: 18

# 2(b) starts
rMode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mean(felix$SO)
median(felix$SO)
rMode(felix$SO)
# mean: 6.16129, median: 6, mode: 5
# 2(b) ends

# 2(c) starts
plot(felix$IP ~ felix$SO)
model <- lm(felix$IP ~ felix$SO)
abline(model, col =2 )

plot(felix$IP ~ felix$BB)
model1 <- lm(felix$IP ~ felix$BB)
abline(model1, col =2 )
#  As number of innings pitched increase, strike outs also increase. 
# As number of innings pitched increase, base on balls decrease.
# 2(c) ends

# 2(d) starts
corIpSo <- cor(felix$IP, felix$SO)
# correlation between IP and SO: 0.6816081
corIpBb <- cor(felix$IP, felix$BB)
# correlation between IP and BB: -0.2638496
# Yes these align with the plots. Correlation between IP and SO is positive as graph linearly increases. 
# Correlation between IP and BB is negative as graph linearly decreases.
# 2(d) ends

#2(e) starts
meanW <- by(felix$BB,felix$Month,mean)
plot(meanW[unique(felix$Month)], col=2)
varW <- by(felix$BB,felix$Month,var)
plot(varW[unique(felix$Month)], col=3)
cor(meanW, varW)
# Since corelation between mean and variance is positive, variance increases with mean. 
#It means that the walks are very much spread out from the mean for months - Apr, May and Jun. For the next two months,
# walks are again close to the mean.

#2(e) ends

#2(f) starts
table(felix$away, felix$W)
# 7 wins away from home and 11 wins at home. Therefore, there are more wins at home.
#2(f) ends

#2(g) starts
randy <- read.csv("RandyJohnson1995.csv")
str(randy)
sum(randy$SO) > sum(felix$SO)
# Randy outperformed Felix since number of strikeouts of Randy are more.
#2(g) ends

# Question 3


# 3(c) starts
curve(dnorm, from = -5, to=5)
abline(v=0.714, col="red")
abline(v=0.52, col="blue")
text(1.285714+1, 0.3, "Verbal: 0.714",col="red")
text(0.5215124-1.5, 0.1, "Quantitative: 0.52", col="blue")
# 3(c) ends

# 3(d) starts
pnorm(0.714)
# 0.762
pnorm(0.52)
# 0.698
#3(d) ends

