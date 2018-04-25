# Shreya Sabharwal - Lab2

#Question 1
movies <- read.csv("movie.titles.csv")
ratings <- read.csv("ratings.csv")
str(ratings)
summary(ratings)
dim(ratings)
# number of ratings = 100004
length(table(ratings$userId))
# number of raters = 671
mean(ratings$rating)
# mean of ratings = 3.543608
median(ratings$rating)
# median = 4
sd(ratings$rating)
#standard deviation = 1.058064
hist(ratings$rating)
# Yes, people prefer round numbers

#Question 2
temp <- merge(ratings, movies, by="movieId")

#Question 3
plot(ratings$year, ratings$rating)
boxplot(ratings$rating ~ cut(ratings$year, breaks = 4), data = ratings, xaxt="n")
axis(1, at=c(1:4), labels= c("1900-1930","1930 - 1960","1960 - 1990","1993 - 2020"))

#Question 4
ratings$comedy <- rep(F, nrow = ratings)
str(ratings)
ratings$comedy[grep("comedy", ratings$genre, ignore.case="T")] <- T
t.test(ratings$rating[ratings$comedy], ratings$rating[!ratings$comedy])

# Part 2

N <- 2000
n.samp <- 30
M <- matrix(NA, N, n.samp)

for(j in 1:n.samp) M[,j] <- rexp(N)
hist(rowSums(M), freq = F)
curve(dnorm(x, mean(rowSums(M)), sd(rowSums(M))), min(rowSums(M)),max(rowSums(M)), add=T, col="red", lwd=2)

for(j in 1:n.samp) M[,j] <- runif(N)
hist(rowSums(M), freq = F)
curve(dnorm(x, mean(rowSums(M)), sd(rowSums(M))), min(rowSums(M)),max(rowSums(M)), add=T, col="red", lwd=2)

for(j in 1:n.samp) M[,j] <- rpois(N,0.5)
hist(rowSums(M), freq = F)
curve(dnorm(x, mean(rowSums(M)), sd(rowSums(M))), min(rowSums(M)),max(rowSums(M)), add=T, col="red", lwd=2)

# Normal Distribution