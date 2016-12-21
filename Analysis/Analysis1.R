# plotting and analyzing data using a single year ---- 

# I want to plot percentage of female agricultural holders vs. percentage of undernourished for each country
plot(both$Per.F, both$`2007`, main = "Female Agricultural Holders and Undernourishment by Country", xlab = "Percentage of Female Agricultural Holders", ylab = "Percent Prevalance of Undernourishment", las = 1, pch = 16, col = "blue")
# I chose the year 2007 because it's recent enough, but before the Great Recession which may have affected the data

# I'm going to try a linear regression model with this data
m1 <- lm(both$`2007` ~ 1) # linear model using null hypothesis
m2 <- lm(both$`2007` ~ both$Per.F) # linear model using explanatory variable
summary(m2)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)   9.7218     3.2103   3.028  0.00392 **
#   both$Per.F    0.4085     0.1801   2.269  0.02774 * 

pdf(paste(p.figures, "hist.2007.pdf", sep = "")) # save the following histogram to the figures folder
hist(m2$residuals)# this is almost normal looking...
# so I think the lm could work
dev.off()

# I want to plot the line given by the intercept and slope of m2
# find the range of x values
x.r <- range(both$Per.F)
# find the corresponding y values to the two extreme x values using the linear model
y.val <- c((9.7218 + 0.4085*x.r[1]), (9.7218 + 0.4085*x.r[2]))
# plot the line of the linear model on the transformed data and save as a pdf to the figures folder
pdf(paste(p.figures, "figure2007.pdf", sep = ""))
plot(both$Per.F, both$`2007`, main = "Female Agricultural Holders and Undernourishment by Country", xlab = "Percentage of Female Agricultural Holders", ylab = "Percent Prevalance of Undernourishment", las = 1, pch = 16, col = "blue")
lines(x.r, y.val, col = "green")
dev.off()

# now to check if the relationship is significantly different than the null hypothesis
anova(m1, m2)
anova(m2) # same results as above line 

# anova gives an F value of 5.1462 and Pr(>F) of .02774
# which is significant - in the opposite direction I would have thought

# now trying to match undernourishment by year ----
# ideally, I would want to make a graph showing percent of female agricultural holders as the x
# and the percent prevalence of undernourishment as the y
# with the undernourishment value taken from the column corresponding to the year the gender data was recorded

# to do this, I need to fix some of the values in the undernourishment data

# the undernourishment data starts in 1991, and there is a year in the women data before that
# i need to find it and remove it
which(both$Year == "1990") # the 19th row is earlier than 1991
# re-store data without the 19th row
both <- both[-19, ]

# let's look at the data to see what other values I need to fix
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

# Now I can create my loop to find the corresponding y value for each x value
# first create an empty vector with an equal length to the number of countries
real <- rep(NA, length(both$Country))

# now make the loop
for(i in 1:length(both$Country)){ # goes through country by country
  year <- both$Year[i] # stores the year that the percent female data was recorded for country i
  n <- which(colnames(both) == year) # stores the column number for undernourishment data that matches the year
  real[i] <- both[i, n] # stores the value from the ith row and the nth column into the "real" vector
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
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  11.1259     3.4338   3.240  0.00215 **
#   both$Per.F    0.4287     0.1926   2.226  0.03067 * 
pdf(paste(p.figures, "hist.real.pdf", sep = "")) # save the following histogram to the figures folder
hist(m2$residuals) # this is kind of normal, but right-skewed...
# so I think the lm could work
dev.off()

# now to plot the linear regression model line on the graph
x.r <- range(both$Per.F)
# find the corresponding y values to the two extreme x values using the linear model
y.val <- c((11.1259 + 0.4287*x.r[1]), (11.1259 + 0.4287*x.r[2]))
# plot the line of the linear model on the transformed data 
# and save the whole figure as a pdf in the figures folder 
pdf(paste(p.figures, "figurereal.pdf", sep = ""))
plot(both$Per.F, both$Real, main = "Female Agricultural Holders and Undernourishment by Country", xlab = "Percentage of Female Agricultural Holders", ylab = "Percent Prevalance of Undernourishment", las = 1, pch = 16, col = "purple")
lines(x.r, y.val, col = "green")
dev.off()

# now to check if the relationship is significantly different than the null hypothesis
anova(m1, m2)
anova(m2) # same results as above line 

# anova gives an F value of 4.9538 and Pr(>F) of 0.03067
# still significant! 

# running the analysis without all of the 5 values ----

# I want to see if all of the values of 5 (technically <5) affect the findings
# so I will remove all data points that have a value of 5
no2 <- which(both$Real == 5.0)
both.2 <- both[-no2, ]

# now make a new plot with this data 
plot(both.2$Per.F, both.2$Real, main = "Female Agricultural Holders and Undernourishment by Country", xlab = "Percentage of Female Agricultural Holders", ylab = "Percent Prevalance of Undernourishment", las = 1, pch = 16, col = "red")

# now run the same analyses on this data as with the first graph
m1 <- lm(both.2$Real ~ 1) # linear model using null hypothesis
m2 <- lm(both.2$Real ~ both.2$Per.F) # linear model using explanatory variable
summary(m2)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   15.6721     3.9296   3.988 0.000312 ***
#   both.2$Per.F   0.3929     0.2107   1.864 0.070422 . 

pdf(paste(p.figures, "hist.no5.pdf", sep = ""))
hist(m2$residuals) # this looks pretty normal
dev.off()

# now to plot the linear regression onto the graph
x.r <- range(both.2$Per.F)
# find the corresponding y values to the two extreme x values using the linear model
y.val <- c((15.6721 + 0.3929*x.r[1]), (15.6721 + 0.3929*x.r[2]))
# plot the line of the linear model on the transformed data 
# and save the whole figure as a pdf in the figures folder 
pdf(paste(p.figures, "figurewithout5s.pdf", sep = ""))
plot(both.2$Per.F, both.2$Real, main = "Female Agricultural Holders and Undernourishment by Country", xlab = "Percentage of Female Agricultural Holders", ylab = "Percent Prevalance of Undernourishment", las = 1, pch = 16, col = "red")
lines(x.r, y.val, col = "green")
dev.off()

# now to check if the relationship is significantly different than the null hypothesis
anova(m1, m2)
anova(m2) # same results as above line 

# anova gives F value 3.4763 and Pr(>F) 0.07042
# this is now no longer significant without all of the 5's! 