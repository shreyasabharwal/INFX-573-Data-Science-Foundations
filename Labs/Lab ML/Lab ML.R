# Shreya Sabharwal

data <- read.csv("census data.csv")
data$income.g50 <-rep(0, nrow(data))
data$income.g50[data$income==" >50K"] <-1

mod <- glm(income.g50~education + age + sex + race, data = data[,!colnames(data)%in%"income"], family="binomial")
summary(mod)
# The log odds of having an income >50K  on having  a masters degree as compared to 10th class education is -3.563e-15.
# These are statistically significant.

levels(data$education)

# 3
x <-data$age
plot(data$income.g50~x, col="blue")
fits <-fitted(mod)
points(x, fits, pch = 4 , cex=0.3)
abline(0.5, 0)

# The predicted probabilities are so variable because age is not a good decider.
#People from all ages are account for salaries greater than 50K and go ahead to be more than 50K.


tab <-table(data$income.g50, fits>=0.5)
(tab[1,2]+tab[2,1])/sum(tab)

tab <-table(data$income.g50, fits>=0.25)
(tab[1,2]+tab[2,1])/sum(tab)

tab <-table(data$income.g50, fits>=0.75)
(tab[1,2]+tab[2,1])/sum(tab)

# Tab with 0.5 as cut off has the lowest percentage error.
install.packages("AUC")
library(AUC)
y <-factor(data$income.g50)
rr <-roc(fits, y)
plot(rr)
auc(rr)
    
#  By plotting the true positives against the false positive we come to know that area under the curve is about 80% for this fitted model.

mod1 <-glm(income.g50~., data=data[,!colnames(data)%in%c("income")], family="binomial")
summary(mod1)

x1 <-data$age
plot(data$income.g50~x1, col="blue")
fits1 <-fitted(mod1)
points(x1, fits1, pch = 4 , cex=0.3)
abline(0.5, 0)

# The probability values provided by the age variable are much less variable now. It clearly denotes how the probabilities of people between 35-55 are more for having a higherincome than 50K.

tab <-table(data$income.g50, fits1>=0.5)
(tab[1,2]+tab[2,1])/sum(tab)

tab <-table(data$income.g50, fits1>=0.25)
(tab[1,2]+tab[2,1])/sum(tab)

tab <-table(data$income.g50, fits1>=0.75)
(tab[1,2]+tab[2,1])/sum(tab)

# The error is least for 0.5 and much less than the previous model.
y <-factor(data$income.g50)
rr <-roc(fits1, y)
plot(rr)
auc(rr)
# AUC IS 0.889 which is much better than the previous model

#EXTRA CREDIT
install.packages("DAAG")
library("DAAG")
cv.binary(obj=mod, rand=NULL, nfolds=10, print.details=TRUE)
cv.binary(obj=mod1, rand=NULL, nfolds=10, print.details=TRUE)
# The second model gives us a better accuracy on K Fold cross validation
#Accuracy
#With Income = 79.3%
#Without Income = 83%
