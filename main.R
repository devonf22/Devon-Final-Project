# making paths and folders ----
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

women$Per.F <- women$X..female
women <- women [, -5]
head(women)


# Working with the second data set ----
# now i want to work with a data set showing percent prevalence of undernourishment by country
# I'm going to go through the same processes with this data as with the food data set 
list.files(p.data)
under <- read.csv(paste(p.data, "Undernourishment.csv", sep = ""), stringsAsFactors = FALSE)
str(under)
head(under)
under$Country <- under$X.table.of.contents. # rename the country column to match name to women data set
under <- under[, -c(1:2)] # remove the first two columns of useless info
# I'm going to rename the column names to be single years rather than a range to make the data easier to work with
# I will rename them to be the middle year of each range 
colnames(under) <- c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "Country")
head(under)



# merging the two data sets and cleaning up data ----
# I can now merge both data sets into one by the Country column
# which it will do automatically because they have the same column name for Country
# and it will remove all country rows not present in both data sets
both <- merge(women, under)
head(both) # it worked! 

# now to clean up the data set
# there are many values with "%", or "<" and I want to remove the symbols
both$Per.F <- as.numeric(sub("%", "", both$Per.F))

# make a loop to remove all the "<" from all year columns
for(i in 6:length(both)){
  both[, i] <- as.numeric(sub("<", "", both[, i]))
}
str(both) # it worked!
# this is how I want the data, I'm done fixing it 

# last step - write the new data set as a new csv in the data folder
write.csv(both, paste(p.data, "Merged.Data.Table.csv", sep = ""))
