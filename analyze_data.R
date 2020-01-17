library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

###########################################################################

# Load household data
household <- read.table(file="household.csv", sep=";", header = TRUE, na.strings = c("" , " ","\t ", "Missing" ))
household <- household[, -1]  # drop case id

# Drop other house ownership, only one instance
household <- household[-which(household$house_ownership == "Other"), ]

# Unknown values
sum(!complete.cases(household))
household <- na.omit(household)

###########################################################################

# Seperate and factorize region
household <- separate(household, region_combined, c("cardinal_direction", "region", "settlement"), sep = '-')
household$cardinal_direction <- as.factor(household$cardinal_direction)
household$region <- as.factor(household$region)
household$settlement <- as.factor(household$settlement)

# Refactor levels of wealth index
household$wealth_index <- factor(household$wealth_index, 
                                 levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))

###########################################################################

# Rate of related attributes
household$man_member_rate = (household$household_member - household$woman_member) / household$household_member
household$woman_member_rate = household$woman_member / household$household_member
household$child_member_rate = household$children_under_5 / household$household_member
household$bedroom_rate = household$bedroom_number / household$rooms_number

# Drop duplicated columns with rate values
household <- household[ , -which(names(household) %in% c("woman_member",  # woman_member_rate
                                                         "children_under_5", # children_member_rate
                                                         "bedroom_number"))]  # bedroom_rate

summary(household)

# Too many attributes
household <- household[, -which(names(household) == "province")]

# Imbalanced attributes
household <- household[ , -which(names(household) %in% c("refrigerator", "garbage_grinder",
                                                         "washing_machine", "washer_dryer", 
                                                         "home_theather", "mobile_phone",
                                                         "taxi_minibus", "tractor", "motorcycle"))]


###########################################################################

# Plot distribution of data
# for(colnm in colnames(household)) {
#   print(ggplot(household, aes_string(x = colnm)) +
#           geom_bar() + ylab("Number of Households"))
# }
# 
# 
# for(colmn in colnames(household)) {
#   tbl <- table(household$wealth_index, household[,colmn])
#   tbl <- tbl / rowSums(tbl)
#   conf_matrix <- as.data.frame(tbl)
#   print(
#     ggplot(data =  conf_matrix, mapping = aes(x = Var1, y = Var2)) +
#       xlab("wealth_index") + ylab(colmn) + 
#       geom_tile(aes(fill = Freq)) +
#       geom_text(aes(label = sprintf("%0.4f", Freq)), vjust = 1) +
#       scale_fill_gradient(low = "blue",
#                           high = "red",
#                           trans = "log")
#   )
# }
# summary(household[, 16:41])
# 
# summary(household[, c("refrigerator", "garbage_grinder", "washing_machine", "washer_dryer", 
#                       "home_theather", "mobile_phone", "taxi_minibus", "tractor", "motorcycle")])


###########################################################################

# Split train, validation and test set
set.seed(1024)
samples <- sample(1:nrow(household), nrow(household)*0.8)
train <- household[samples, ]
test <- household[-samples, ]

save(list=c("household", "train", "test"), file = "household.data")
rm(list = ls())

###########################################################################
