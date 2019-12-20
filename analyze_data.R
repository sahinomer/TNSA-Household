library(readr)
library(tidyr)
library(ggplot2)
library(DMwR2)


household <- read.table(file="household.csv", sep=";", header = TRUE, na.strings = c("" , " ","\t ", "Missing" ))

household <- na.omit(household)

household <- household[, -1]  # drop case id

household <- separate(household, region_combined, c("compass", "region", "settlement"), sep = '-')

###########################################################################

# Refactor levels of wealth index
household$wealth_index <- factor(household$wealth_index, 
                                 levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))


# Rate of related attributes
household$man_member_rate = (household$household_member - household$woman_member) / household$household_member
household$woman_member_rate = household$woman_member / household$household_member
household$child_member_rate = household$children_under_5 / household$household_member
household$bedroom_rate = household$bedroom_number / household$rooms_number

# Drop duplicated columns with rate values
household <- household[ , -which(names(household) %in% c("woman_member",  # woman_member_rate
                                                         "children_under_5", # children_member_rate
                                                         "bedroom_number"))]  # bedroom_rate

###########################################################################

# Plot distribution of data
for(colnm in colnames(household)) {
  print(ggplot(household, aes_string(x = colnm)) +
          geom_bar() + ylab("Number of Households"))
}

###########################################################################

# Split train, validation and test set
samples <- sample(1:nrow(household), nrow(household)*0.1)
train <- household[samples, ]
test <- household[-samples, ]

save(list=c("train", "test"), file = "household.data")
rm(list = ls())

###########################################################################
