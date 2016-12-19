plot(all$Female, all$`1998-00`) # x axis looks weird
min(all$`1998-00`, na.rm = TRUE) # these variables are working correctly 
min(all$Female, na.rm = TRUE)
max(all$Female, na.rm = TRUE)
# the min and max of Female are correct - so why cant it plot it? 
# try log transforming the x axis
all$Female <- log(all$Female)
plot(all$Female, all$`1998-00`) # this looks better

# ideally, I would want to make a graph showing proportion of female workers as the x
# and the total food value as the y
# with the food value taken from the column corresponding to the year the proportion data was calculated
plot(all$X..female, all$`2011-13`) # this graph shows
# the percentage of female workers in the agriculture sector
# and the corresponding total food value production for that country 


# now I want to plot percentage of female workers and percentage of undernourished 
plot(both$X..female, both$`2014-16`)
