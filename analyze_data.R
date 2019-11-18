library(readr)
library(tidyr)
library(ggplot2)

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


ggplot(household, aes(x = wealth_index)) +
  geom_bar() + ggtitle("Wealth Distribution") +
  xlab("Wealth Index") + ylab("Number of Households")

ggplot(household, aes(x = refrigerator, 
                 y = owns_another_house,
                 color = wealth_index)) + 
  geom_point()


table(household$wealth_index, household$house_ownership)
table(household$wealth_index, household$owns_another_house)
table(household$wealth_index, household$car)
table(household$wealth_index, household$refrigerator)
table(household$wealth_index, household$heating)
table(household$wealth_index, household$mobile_phone)
table(household$wealth_index, household$motorcycle)




