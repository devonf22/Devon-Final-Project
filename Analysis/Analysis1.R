plot(all$Female, all$`1998-00`) # x axis looks weird
min(all$`1998-00`, na.rm = TRUE) # these variables are working correctly 
min(all$Female, na.rm = TRUE)
max(all$Female, na.rm = TRUE)
# the min and max of Female are correct - so why cant it plot it? 
# try log transforming the x axis
all$Female <- log(all$Female)
plot(all$Female, all$`1998-00`) # this looks better

plot(all$X..female, all$`2011-13`) # this graph shows
# the percentage of female workers in the agriculture sector
# and the corresponding total food value production for that country 


# now I want to plot percentage of female workers and percentage of undernourished 
plot(both$X..female, both$`2006-08`, main = "Female Agricultural Holders and Undernourishment by Country", xlab = "Percentage of Female Agricultural Holders", ylab = "Percent Prevalance of Undernourishment", las = 1, pch = 16, col = "blue")
# now time to do analysis 
m1 <- lm(both$`2006-08` ~ 1)
m2 <- lm(both$`2006-08` ~ both$X..female)
summary(m2)
hist(m2$residuals) # how to argue for doing a lm 
# find the range of x values
x.r <- range(both$X..female)
# find the corresponding y values to the two extreme x values using the linear model
y.val <- c((9.7218 + 0.4085*x.r[1]), (9.7218 + 0.4085*x.r[2]))
# plot the line of the linear model on the transformed data 
lines(x.r, y.val, col = "green")

summary(m2)
anova(m1, m2)
anova(m2)

# anova gives an F value of 5.1462 and Pr(>F) of .02774
# significant... in the opposite direction I would have thought

hist(both$X..female)
hist(both$`2006-08`)


# now trying to match undernourishment by year ----
# ideally, I would want to make a graph showing proportion of female workers as the x
# and the total food value as the y
# with the food value taken from the column corresponding to the year the proportion data was calculated

# create an empty vector with length of countries
real <- rep(NA, length(both$Country))
# now going to change the column names to be just the middle year instead of the range
colnames(both) <- c("Country", "Year", "Total", "Female", "Per.F", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
head(both)

# now going to try to create a loop to match y values with years

for(i in 1:length(both$Country)){
  year <- both$Year[i]
  n <- which(colnames(both) == year)
  real[i] <- both[i, n]
}

# this almost works! I just need to not exclude so much data from my original data set...


