#Lab1 - Shreya Sabharwal
seatbelts <- read.csv("seatbelts.csv")
class(seatbelts)

# 1. how many cases are in the observed data?
dim(seatbelts)
# 192 cases

# data frame attributes including column names
# what variables are observed for each month?
colnames(seatbelts)
summary(seatbelts)

#What was the average number of fatalities in 1970? 1978?
by(seatbelts[, "DriversKilled"], seatbelts[, "year"], mean)
# 1970: 122.4167, 1978: 125.5833

#What was the average number of rear seat fatalities in 1972? 1980?
tapply(seatbelts[,"rear"], seatbelts[,"year"], mean)
# 1972: 440.25, 1980: 380.8333

#Plot the relationship between drivers killed or seriously injured and petrol price and
#kilometers traveled. What do you see? What hypotheses might you make after seeing
#these relationships?
plot(seatbelts[,"kms"], seatbelts[,"drivers"])
# As Distance driven increases, number of fatalities decrease. We can infer that with experience, number of accidents decrease
plot(seatbelts[,"kms"], seatbelts[,"PetrolPrice"])
plot(seatbelts[,"drivers"], seatbelts[,"PetrolPrice"])

# Question: What descriptive and visual tools might we use to explore this? Examine the
#mean fatalities before and after the implementation of the law. Remember to subset your
#data (hint: there are two variables you can use to do this to do this).

#Produce a figure to get some visual intuition about the response to the seatbelt law. Was
#it a gradual decline? Sharp? Does it appear large in magnitude or small? (Hint,
#abline() is useful for marking specific dates on time series plots)
a <- subset(seatbelts, law==1)
mean(a[,"DriversKilled"])
b <- subset(seatbelts, law==0)
mean(b[,"DriversKilled"])
plot( seatbelts[,"year.month"],seatbelts[,"DriversKilled"], type="l")
abline(v=1983, col=2)
