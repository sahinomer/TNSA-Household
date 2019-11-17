library(readr)
library(tidyr)

household <- read_delim("household.csv",";", escape_double = FALSE, trim_ws = TRUE, na = '')
household <- separate(household, region_combined, c("compass", "region", "settlement"))

household$wealth_index <- factor(household$wealth_index)

household <- na.omit(household)

table(household$wealth_index)

barplot(table(household$wealth_index), main="Wealth Distribution",
        xlab="Number of Households")

table(household$wealth_index, household$house_ownership)
table(household$wealth_index, household$owns_another_house)
table(household$wealth_index, household$car)
table(household$wealth_index, household$refrigerator)
table(household$wealth_index, household$heating)
table(household$wealth_index, household$mobile_phone)
table(household$wealth_index, household$motorcycle)




