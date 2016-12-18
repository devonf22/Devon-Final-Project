# get working directory and list files
wd <- getwd()
list.files()

# list of folders I want to create in my working directory
folders <- c("Data", "Analysis", "Figures")

# create these folders if I dont have them already
for(i in 1:length(folders)){
  if(file.exists(folders[i]) == "FALSE") {
    dir.create(folders[i])
  }
}

# create paths to the new folders in the working directory
p.data <- paste(wd, "/Data/", sep = "")
p.analysis <- paste(wd, "/Analysis/", sep = "")
p.figures <- paste(wd, "/Figures/", sep = "")


# Working with the first data set ----
# list the files in my data folder
list.files(p.data)
# read the first data file I want - stats about women workers in agriculture by country
women <- read.csv(paste(p.data, "gender.agriculture.csv", sep = ""), stringsAsFactors = FALSE)
str(women)
head(women)

# I want to remove the rows with NAs and make a new data set without them 
# Go through one column at a time and ask which row contains an NA
which(is.na(women$Total)) # 4th row
which(is.na(women$Female)) # 4th row
which(is.na(women$Country)) # none
which(is.na(women$Year)) # none
which(is.na(women$X..female)) # none
which(is.na(women$Source))# none
# now I know I only need to remove the 4th row of data from the data set

women <- women[-4, ]
head(women) # now my data set has no NAs 
# I also want to remove the 6th column - source - because i have no use for it
women <- women[, -6]
head(women)

# Now I want to change total workers and female workers to numbers instead of characters
str(women$Total)


# Working with the second data set ----

list.files(p.data)
food <- read.csv(paste(p.data, "Food_Value.csv", sep = ""), stringsAsFactors = FALSE)
str(food)
head(food)
# rename the column to match the column name of the first data set I want to merge
food$Country <- food$X.table.of.contents.
# this created a new column at the end that's a replicate of the first...
# I want to remove the first two columns from the data set 
food <- food[, -c(1:2)]
head(food)
# now I want to remove all columns with dates earlier than 2000
# it will be more accurate if i only use more recent data
food <- food [, -c(1:8)]
head(food)
# I'm going to rename all of the columns with the years rather than have the years as the second row
names(food) <- c("1998-00", "1999-01", "2000-02", "2001-03", "2002-04", "2003-05", "2004-06", "2005-07", "2006-08", "2007-09", "2008-10", "2009-11", "2010-12", "2011-13", "Country")
names(food)
# now I'm ready to try to merge both data sets 

# Merging the two data sets ----

# I want to merge the two data sets to match up by country name
# the problem is that the women data set does not have all the countries that food does 

# all of the countries that are in common with both data sets
all <- merge(women, food)
str(all)
head(all)
# successful!! now to try to change almost all variables into numbers... 

as.numeric(all$`1998-00`)
plot(all$Female, all$`1999-01`)
plot(all$Female, all$`1998-00`)

sort(all$Female, decreasing = FALSE)  
