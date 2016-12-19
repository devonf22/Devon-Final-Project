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

all$`1998-00` <- as.numeric(all$`1998-00`)
# that worked, let's make a loop to turn them all into numbers
str(all)
for(i in 6:length(all)){
  name1 <- names(all[i])
  all$name1 <- as.numeric(all$names1)
}

# I cannot make a loop that works, so I'm just going to do it by hand for now
all$`1998-00` <- as.numeric(all$`1998-00`)
all$`1999-01` <- as.numeric(all$`1999-01`)
all$`2000-02` <- as.numeric(all$`2000-02`)
all$`2001-03` <- as.numeric(all$`2001-03`)
all$`2002-04` <- as.numeric(all$`2002-04`)
all$`2003-05` <- as.numeric(all$`2003-05`)
all$`2004-06` <- as.numeric(all$`2004-06`)
all$`2005-07` <- as.numeric(all$`2005-07`)
all$`2006-08` <- as.numeric(all$`2006-08`)
all$`2007-09` <- as.numeric(all$`2007-09`)
all$`2008-10` <- as.numeric(all$`2008-10`)
all$`2009-11` <- as.numeric(all$`2009-11`)
all$`2010-12` <- as.numeric(all$`2010-12`)
all$`2011-13` <- as.numeric(all$`2011-13`)
str(all)

# now I want to change the Total and Female workers into numbers, but have to take the commas out...
all$Total <- as.numeric(gsub(",","", as.character(all$Total)))
all$Female <- as.numeric(gsub(",","", as.character(all$Female)))
is.numeric(all$Total)
# that worked!
# now want to do the same thing with percentage female column, but take out % sign
all$X..female <- as.numeric(sub("%", "", all$X..female))
# that worked! 

# Working with a third data set ----
# now i want to work with a data set showing percent of undernourished by country
# I'm going to go through the same processes with this data as with the food data set 
list.files(p.data)
under <- read.csv(paste(p.data, "Undernourishment.csv", sep = ""), stringsAsFactors = FALSE)
str(under)
head(under)
under$Country <- under$X.table.of.contents.
head(under)
under <- under[, -c(1:10)]
names(under) <- c("1998-00", "1999-01", "2000-02", "2001-03", "2002-04", "2003-05", "2004-06", "2005-07", "2006-08", "2007-09", "2008-10", "2009-11", "2010-12", "2011-13", "2012-14", "2013-15", "2014-16", "Country")
head(under)

both <- merge(women, under)
head(both)
both$X..female <- as.numeric(sub("%", "", both$X..female))
str(both)
both$`1998-00` <- as.numeric(sub("<", "", both$`1998-00`))
both$`1999-01` <- as.numeric(sub("<", "", both$`1999-01`))
both$`2000-02` <- as.numeric(sub("<", "", both$`2000-02`))
both$`2001-03` <- as.numeric(sub("<", "", both$`2001-03`))
both$`2002-04` <- as.numeric(sub("<", "", both$`2002-04`))
both$`2003-05` <- as.numeric(sub("<", "", both$`2003-05`))
both$`2004-06` <- as.numeric(sub("<", "", both$`2004-06`))
both$`2005-07` <- as.numeric(sub("<", "", both$`2005-07`))
both$`2006-08` <- as.numeric(sub("<", "", both$`2006-08`))
both$`2007-09` <- as.numeric(sub("<", "", both$`2007-09`))
both$`2008-10` <- as.numeric(sub("<", "", both$`2008-10`))
both$`2009-11` <- as.numeric(sub("<", "", both$`2009-11`))
both$`2010-12` <- as.numeric(sub("<", "", both$`2010-12`))
both$`2011-13` <- as.numeric(sub("<", "", both$`2011-13`))
both$`2012-14` <- as.numeric(sub("<", "", both$`2012-14`))
both$`2013-15` <- as.numeric(sub("<", "", both$`2013-15`))
both$`2014-16` <- as.numeric(sub("<", "", both$`2014-16`))

str(both) # this is how I want the data, I'm done fixing it 
