# plotting and analyzing data using a single year ---- 

# I want to plot percentage of female agricultural holders vs. percentage of undernourished for each country
plot(both$Per.F, both$`2007`, main = "Female Agricultural Holders and Undernourishment by Country", xlab = "Percentage of Female Agricultural Holders", ylab = "Percent Prevalance of Undernourishment", las = 1, pch = 16, col = "blue")
# I chose the year 2007 because it's recent enough but before the Great Recession which may have affected the data

# I'm going to try a linear regression model with this data
m1 <- lm(both$`2007` ~ 1) # linear model using null hypothesis
m2 <- lm(both$`2007` ~ both$Per.F) # linear model using explanatory variable
summary(m2)
hist(m2$residuals) # this is almost normal looking...
# so I think the lm could work

# I want to plot the line given by the intercept and slope of m2
# find the range of x values
x.r <- range(both$Per.F)
# find the corresponding y values to the two extreme x values using the linear model
y.val <- c((9.7218 + 0.4085*x.r[1]), (9.7218 + 0.4085*x.r[2]))
# plot the line of the linear model on the transformed data 
plot(both$Per.F, both$`2007`, main = "Female Agricultural Holders and Undernourishment by Country", xlab = "Percentage of Female Agricultural Holders", ylab = "Percent Prevalance of Undernourishment", las = 1, pch = 16, col = "blue")
lines(x.r, y.val, col = "green")

# now to check if the relationship is significantly different than the null hypothesis
anova(m1, m2)
anova(m2) # same results as above line 

# anova gives an F value of 5.1462 and Pr(>F) of .02774
# which is significant... in the opposite direction I would have thought





# now trying to match undernourishment by year ----
# ideally, I would want to make a graph showing percent of female agricultural holders as the x
# and the percent prevalence of undernourishment as the y
# with the undernourishment value taken from the column corresponding to the year the gender data was recorded
# let's try it 

# create an empty vector with length of countries
real <- rep(NA, length(both$Country))

# now going to try to create a loop to match y values with years

for(i in 1:length(both$Country)){
  year <- both$Year[i]
  n <- which(colnames(both) == year)
  real[i] <- both[i, n]
}

# this almost works! I think there is one year that does not have a matching column name because it's too early
# i need to find it and remove it
# my colnames with years start at 1991, so I need to remove all rows with years earlier than that
which(both$Year == "1990") # the 19th row is earlier than 1991
# re-store data without the 19th row
both <- both[-19, ]
# now re-run the loop 

for(i in 1:length(both$Country)){
  year <- both$Year[i]
  n <- which(colnames(both) == year)
  real[i] <- both[i, n]
}

# still doesn't work... are there years after 2015? 
which(both$Year == "2016") # no 
# let's look at the data
both$Year
# there are a few years that are ranges instead of singular years
# I need to change those
for(i in 1:length(both$Year)){
  if(both$Year[i] == "1998-1999"){
    both$Year[i] <- "1999"
  }
  if(both$Year[i] == "1999-2000"){
    both$Year[i] <- "2000"
  }
  if(both$Year[i] == "2000-2001"){
    both$Year[i] <- "2001"
  }
  if(both$Year[i] == "2001-2002"){
    both$Year[i] <- "2002"
  }
  if(both$Year[i] == "2004-2005"){
    both$Year[i] <- "2005"
  }
  if(both$Year[i] == "2010-2011"){
    both$Year[i] <- "2011"
  }
  if(both$Year[i] == "2011-2012"){
    both$Year[i] <- "2012"
  }
}

# that should fix it, now try loop one more time
for(i in 1:length(both$Country)){
  year <- both$Year[i]
  n <- which(colnames(both) == year)
  real[i] <- both[i, n]
}


# it worked! the real vector is filled with undernourishment data for each country
# based on what year the gender data was recorded!

# now I want to add this vector as a new column on my dataset
both$Real <- real

# and now plot the new data
# percent female agricultural holders vs percent undernourished
plot(both$Per.F, both$Real, main = "Female Agricultural Holders and Undernourishment by Country", xlab = "Percentage of Female Agricultural Holders", ylab = "Percent Prevalance of Undernourishment", las = 1, pch = 16, col = "purple")
# it looks good! 

# now run the same analyses on this data as with the first graph
m1 <- lm(both$Real ~ 1) # linear model using null hypothesis
m2 <- lm(both$Real ~ both$Per.F) # linear model using explanatory variable
summary(m2)
hist(m2$residuals) # this is kind of normal, but right-skewed...
# so I think the lm could work

x.r <- range(both$Per.F)
# find the corresponding y values to the two extreme x values using the linear model
y.val <- c((11.1259 + 0.4287*x.r[1]), (11.1259 + 0.4287*x.r[2]))
# plot the line of the linear model on the transformed data 
# and save the whole figure as a pdf in the figures folder 
pdf(paste(p.figures, "figure.pdf", sep = ""))
plot(both$Per.F, both$Real, main = "Female Agricultural Holders and Undernourishment by Country", xlab = "Percentage of Female Agricultural Holders", ylab = "Percent Prevalance of Undernourishment", las = 1, pch = 16, col = "purple")
lines(x.r, y.val, col = "green")
dev.off()

# now to check if the relationship is significantly different than the null hypothesis
anova(m1, m2)
anova(m2) # same results as above line 

# anova gives an F value of 4.9538 and Pr(>F) of 0.03067
# still significant! 

# let's try one more thing...


# running the analysis without all of the 5 values ----





