# Getting and Cleaning Data Quiz 4

file.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
file.dest <- "GDP.csv"

download.file(file.url, destfile =  file.dest)

# -- load data to r
# 1st method no special argument

gdp <- read.csv("GDP.csv")
View(gdp)

# 2nd method skip bad header files, load only desired files
gdp <- read.csv("GDP.csv", skip = 4, nrows = 190)
View(gdp)

# 3nd method skip bad header files, load only desired files
# select only needed colunm

first.gdp <- read.csv("GDP.csv", skip = 4, nrows = 1)
View(first.gdp)

class.gdp <- as.character(sapply(first.gdp, class))

View(class.gdp)

class.gdp[3] <- "NULL"
class.gdp[seq(6,10)] <- "NULL"

# class.gdp <- c("factor","integer","NULL","factor","factor", rep("NULL", 5))
gdp <- read.csv("GDP.csv", skip = 4, colClasses = class.gdp, nrows = 190)

# Try data.table 
gdp2 <- read.table("GDP.csv", sep = ",", quote = "\"", skip = 5, nrow = 190)

# Try library read.table
library(data.table)
gdp3 <- fread(file.url, skip = 4, nrows = 190, drop = c(3,seq(6,10)))
