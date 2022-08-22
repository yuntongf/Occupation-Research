library(tidyverse)
library(data.table)
library(dplyr)

#### function to find the top 10 occupations with largest employment of the year ####
topEmploy <- function(data) {
  top <- data %>% 
              arrange(desc(data$Employment)) %>%
              slice(1:10)
  ggplot(top, aes(x = Occupation, y = Employment / 1000)) + 
    geom_col() + 
    labs(x="Occupation Name", y="Employment in thousands", title="Top10 Occupations By Largest Employment") +
    theme(axis.text.x = element_text(size=6, angle = 50, hjust=1))
}

#### find the top 10 occupations with smallest employment number ####
bottomEmploy <- function(data) {
bottomEmploy <- data %>% 
  arrange(Employment) %>%
  slice(1:10)

ggplot(bottomEmploy, aes(x = Occupation, y = Employment / 1000)) + 
  geom_col() + 
  labs(x="Occupation Name", y="Employment in thousands", title="Top10 Occupations With Lowest Employment") +
  theme(axis.text.x = element_text(size=6, angle = 50, hjust=1))
}

#### find the top 10 occupations with the highest median income in that year ####
topIncome <- function(data){
bottomEmploy <- data %>% 
  arrange(Median_Income) %>%
  slice(1:10)

ggplot(bottomEmploy, aes(x = Occupation, y = Median_Income)) + 
  geom_col() + 
  labs(x="Occupation Name", y="Median Income", title="Top10 Occupations With Highest Median Income") +
  theme(axis.text.x = element_text(size=6, angle = 50, hjust=1))
}


#### find the top 10 occupations with the largest income gap in that year ####
topGap <- function(data){
  income  <- data %>% 
    arrange(desc(High_Income - Low_Income)) %>%
    slice(1:10)
  
  ggplot(income, aes(x = Occupation, y = High_Income - Low_Income)) + 
    geom_col() + 
    labs(x="Occupation Name", y="Income Gap Between Top and Bottom 10 Percent", 
         title="Top10 Occupations With Largest Income Gap") +
    theme(axis.text.x = element_text(size=6, angle = 50, hjust=1))
}

#### find the top 10 occupations with the highest growth rate in that year ####
topGrowth <- function(data){
  topGrowth <- data %>% 
    arrange(desc(Growth)) %>%
    slice(1:10)
  
  ggplot(topGrowth, aes(x = Occupation, y = Growth)) + 
    geom_col() + 
    labs(x="Occupation Name", y="Growth Rate", title="Top10 Occupations With Highest Growth Rate") +
    theme(axis.text.x = element_text(size=6, angle = 50, hjust=1))
}

#### find the top 10 occupations with the lowest growth rate in that year ####
bottomGrowth <- function(data){
  topGrowth <- data %>% 
    arrange(Growth) %>%
    slice(1:10)
  
  ggplot(topGrowth, aes(x = Occupation, y = Growth)) + 
    geom_col() + 
    labs(x="Occupation Name", y="Growth Rate", title="Top10 Occupations With Lowest Growth Rate") +
    theme(axis.text.x = element_text(size=6, angle = 50, hjust=1))
}

########### read in all years ###########

## 2000 ##
data2000 <- read.csv("/Users/omgitsmonday/Desktop/FinalSpreadsheetRepoCSV/2000_data.csv", 
                     fill=TRUE)
setDT(data2000)
names(data2000)
occ <- select(data2000, "X.28")
data2000 <- select(data2000, "X.39", "X.76", "X.79", "X.82")
## check if type of values in columns are numeric ##
sapply(data2000, class)
## convert to numeric values 
data2000[] <- lapply(data2000, function(x) as.numeric(as.character(x)))
## add the occupation column
data2000 <- cbind(occ, data2000)
## check types of the columns##
sapply(data2000, class)
## rename columns
colnames(data2000) <- c("Occupation","Employment", "Median_Income", "Low_Income", 
                        "High_Income")


## 2002 ##
data2002 <- read.csv("/Users/omgitsmonday/Desktop/FinalSpreadsheetRepoCSV/2002_data.csv", 
                     fill=TRUE)
setDT(data2002)
names(data2002)
occ <- select(data2002, "Input.occupation.title")
data2002 <- select(data2002, "Answer.c1", "Answer.f1.1", "Answer.f1a.1", "Answer.f1b.1")
## convert to numeric values 
data2002[] <- lapply(data2002, function(x) as.numeric(as.character(x)))
## add the occupation column
data2002 <- cbind(occ, data2002)
## rename columns
colnames(data2002) <- c("Occupation","Employment", "Median_Income", "Low_Income", 
                        "High_Income")

## 2004 ##
data2004 <- read.csv("/Users/omgitsmonday/Desktop/FinalSpreadsheetRepoCSV/2004_data.csv", 
                     fill=TRUE)
setDT(data2004)
names(data2004)
occ <- select(data2004, "Input.occupation.title")
data2004 <- select(data2004, "Answer.c1", "Answer.f1.1", "Answer.f1a.1", "Answer.f1b.1")
## convert to numeric values 
data2004[] <- lapply(data2004, function(x) as.numeric(as.character(x)))
## add the occupation column
data2004 <- cbind(occ, data2004)
## rename columns
colnames(data2004) <- c("Occupation","Employment", "Median_Income", "Low_Income", 
                        "High_Income")

## 2006 ##
data2006 <- read.csv("/Users/omgitsmonday/Desktop/FinalSpreadsheetRepoCSV/2006_data.csv", 
                     fill=TRUE)
setDT(data2006)
names(data2006)
occ <- select(data2006, "Input.occupation.title")
data2006 <- select(data2006, "Answer.c1", "Answer.f1.1", "Answer.f1a.1", "Answer.f1b.1")
## convert to numeric values 
data2006[] <- lapply(data2006, function(x) as.numeric(as.character(x)))
## add the occupation column
data2006 <- cbind(occ, data2006)
## rename columns
colnames(data2006) <- c("Occupation","Employment", "Median_Income", "Low_Income", 
                        "High_Income")

## 2008 ##
data2008 <- read.csv("/Users/omgitsmonday/Desktop/FinalSpreadsheetRepoCSV/2008_data.csv", 
                     fill=TRUE)
setDT(data2008)
names(data2008)
occ <- select(data2008, "Input.occupation.title")
data2008 <- select(data2008, "Answer.c1", "Answer.f1.1", "Answer.f1a.1", "Answer.f1b.1")
## convert to numeric values 
data2008[] <- lapply(data2008, function(x) as.numeric(as.character(x)))
## add the occupation column
data2008 <- cbind(occ, data2008)
## rename columns
colnames(data2008) <- c("Occupation","Employment", "Median_Income", "Low_Income", 
                        "High_Income")

## 2010 ##
data2010 <- read.csv("/Users/omgitsmonday/Desktop/FinalSpreadsheetRepoCSV/2010_data.csv", 
                     fill=TRUE)
setDT(data2010)
names(data2010)
occ <- select(data2010, "Input.occupation.title")
data2010 <- select(data2010, "Answer.c1", "Answer.f1.1", "Answer.f1a.1", "Answer.f1b.1", "Answer.e2")
## convert to numeric values 
data2010[] <- lapply(data2010, function(x) as.numeric(as.character(x)))
## add the occupation column
data2010 <- cbind(occ, data2010)
## rename columns
colnames(data2010) <- c("Occupation","Employment", "Median_Income", "Low_Income", 
                        "High_Income", "Growth")

## 2012 ##
data2012 <- read.csv("/Users/omgitsmonday/Desktop/FinalSpreadsheetRepoCSV/2012_data.csv", 
                     fill=TRUE)
setDT(data2012)
names(data2012)
occ <- select(data2012, "Input.occupation.title")
data2012 <- select(data2012, "Answer.c1", "Answer.f1.1", "Answer.f1a.1", "Answer.f1b.1", "Answer.e2")
## convert to numeric values 
data2012[] <- lapply(data2012, function(x) as.numeric(as.character(x)))
## add the occupation column
data2012 <- cbind(occ, data2012)
## rename columns
colnames(data2012) <- c("Occupation","Employment", "Median_Income", "Low_Income", 
                        "High_Income", "Growth")

## 2014 ##
data2014 <- read.csv("/Users/omgitsmonday/Desktop/FinalSpreadsheetRepoCSV/2014_data.csv", 
                     fill=TRUE)
setDT(data2014)
names(data2014)
occ <- select(data2014, "Input.occupation.title")
data2014 <- select(data2014, "Answer.c1", "Answer.f1.1", "Answer.f1a.1", "Answer.f1b.1", "Answer.e2")
## convert to numeric values 
data2014[] <- lapply(data2014, function(x) as.numeric(as.character(x)))
## add the occupation column
data2014 <- cbind(occ, data2014)
## rename columns
colnames(data2014) <- c("Occupation","Employment", "Median_Income", "Low_Income", 
                        "High_Income", "Growth")

## 2016 ##
data2016 <- read.csv("/Users/omgitsmonday/Desktop/FinalSpreadsheetRepoCSV/2016_data.csv", 
                     fill=TRUE)
setDT(data2016)
names(data2016)
occ <- select(data2016, "Input.occupation.title")
data2016 <- select(data2016, "Answer.c1", "Answer.f1.1", "Answer.f1a.1", "Answer.f1b.1", "Answer.e2")
## convert to numeric values 
data2016[] <- lapply(data2016, function(x) as.numeric(as.character(x)))
## add the occupation column
data2016 <- cbind(occ, data2016)
## rename columns
colnames(data2016) <- c("Occupation","Employment", "Median_Income", "Low_Income", 
                        "High_Income", "Growth")

## 2018 ##
data2018 <- read.csv("/Users/omgitsmonday/Desktop/FinalSpreadsheetRepoCSV/2018_data.csv", 
                     fill=TRUE)
setDT(data2018)
names(data2018)
occ <- select(data2018, "Input.occupation.title")
data2018 <- select(data2018, "Answer.c1", "Answer.f1.1", "Answer.f1a.1", "Answer.f1b.1", "Answer.e2")
## convert to numeric values 
data2018[] <- lapply(data2018, function(x) as.numeric(as.character(x)))
## add the occupation column
data2018 <- cbind(occ, data2018)
## rename columns
colnames(data2018) <- c("Occupation","Employment", "Median_Income", "Low_Income", 
                        "High_Income", "Growth")

## 2020 ##
data2020 <- read.csv("/Users/omgitsmonday/Desktop/FinalSpreadsheetRepoCSV/2020_data.csv", 
                     fill=TRUE)
setDT(data2020)
names(data2020)
occ <- select(data2020, "Input.occupation.title")
data2020 <- select(data2020, "Answer.c1", "Answer.f1.1", "Answer.f1a.1", "Answer.f1b.1", "Answer.e2")
## convert to numeric values 
data2020[] <- lapply(data2020, function(x) as.numeric(as.character(x)))
## add the occupation column
data2020 <- cbind(occ, data2020)
## rename columns
colnames(data2020) <- c("Occupation","Employment", "Median_Income", "Low_Income", 
                        "High_Income", "Growth")


## call functions on all years
topEmploy(data2000)
bottomEmploy(data2000)
topIncome(data2000)
topGap(data2000)

topEmploy(data2002)
bottomEmploy(data2002)
topIncome(data2002)
topGap(data2002)

topEmploy(data2004)
bottomEmploy(data2004)
topIncome(data2004)
topGap(data2004)

topEmploy(data2006)
bottomEmploy(data2006)
topIncome(data2006)
topGap(data2006)

topEmploy(data2008)
bottomEmploy(data2008)
topIncome(data2008)
topGap(data2008)

topEmploy(data2010)
bottomEmploy(data2010)
topIncome(data2010)
topGap(data2010)
topGrowth(data2010)
bottomGrowth(data2010)

topEmploy(data2012)
bottomEmploy(data2012)
topIncome(data2012)
topGap(data2012)
topGrowth(data2012)
bottomGrowth(data2012)

topEmploy(data2014)
bottomEmploy(data2014)
topIncome(data2014)
topGap(data2014)
topGrowth(data2014)
bottomGrowth(data2014)

topEmploy(data2016)
bottomEmploy(data2016)
topIncome(data2016)
topGap(data2016)
topGrowth(data2016)
bottomGrowth(data2016)

topEmploy(data2018)
bottomEmploy(data2018)
topIncome(data2018)
topGap(data2018)
topGrowth(data2018)
bottomGrowth(data2018)

topEmploy(data2020)
bottomEmploy(data2020)
topIncome(data2020)
topGap(data2020)
topGrowth(data2020)
bottomGrowth(data2020)


## issues

# 2008 agricultural high income
# 2000 occupation title
