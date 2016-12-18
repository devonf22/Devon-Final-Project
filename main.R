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


# Now I want to change total workers and female workers to numbers instead of characters
str(women$Total)



