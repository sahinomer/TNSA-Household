library(readr)
library(tidyr)
library(ggplot2)
library(DMwR2)
library(rpart)
library(rpart.plot)


household <- read.table(file="household.csv", sep=";", header = TRUE, na.strings = c("" , " ","\t ", "Missing" ))

household <- separate(household, region_combined, c("compass", "region", "settlement"))

household <- na.omit(household)

# Refactor levels of wealth index
household$wealth_index <- factor(household$wealth_index, 
                                 levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))

# Rate of related attributes
household$man_member_rate = (household$household_member - household$woman_member) / household$household_member
household$woman_member_rate = household$woman_member / household$household_member
household$child_member_rate = household$children_under_5 / household$household_member
household$bedroom_rate = household$bedroom_number / household$rooms_number


# Drop unused columns 
household <- household[ , -which(names(household) %in% c("case_id",  # unique ids
                                                         "woman_member",  # woman_member_rate
                                                         "children_under_5", # children_member_rate
                                                         "bedroom_number"))]  # bedroom_rate

for(colnm in colnames(household)) {
  print(ggplot(household, aes_string(x = colnm)) +
    geom_bar() + ylab("Number of Households"))
}



ggplot(household, aes_string(x = "wealth_index")) +
  geom_bar() + ggtitle("Wealth Distribution") +
  xlab("Wealth Index") + ylab("Number of Households")

ggplot(household, aes(x = refrigerator, 
                 y = owns_another_house,
                 color = wealth_index)) + 
  geom_point()

samp <- sample(1:nrow(household), 200)
train <- household[samp, ]

model <- rpartXse(wealth_index ~ ., train, se = 1, verbose = T)
prp(model, type = 0, extra = 101)
predicted <- predict(model, household, type = "class")
table(household$wealth_index, predicted)



