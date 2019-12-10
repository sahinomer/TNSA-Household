library(readr)
library(tidyr)
library(ggplot2)
library(DMwR2)
library(rpart)
library(rpart.plot)
library(CORElearn) # feature extraction


household <- read.table(file="household.csv", sep=";", header = TRUE, na.strings = c("" , " ","\t ", "Missing" ))

household <- na.omit(household)

household <- separate(household, region_combined, c("compass", "region", "settlement"), sep = '-')

household <- household[, -1]  # drop case id

# Refactor levels of wealth index
household$wealth_index <- factor(household$wealth_index, 
                                 levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))

# Rate of related attributes
household$man_member_rate = (household$household_member - household$woman_member) / household$household_member
household$woman_member_rate = household$woman_member / household$household_member
household$child_member_rate = household$children_under_5 / household$household_member
household$bedroom_rate = household$bedroom_number / household$rooms_number


# Feature Selection
infoCore(what = "attrEval")  # classification
infoCore(what = "attrEvalReg")  # regression

#attrEval(wealth_index ~ ., household, estimator = "GainRatio")
#attrEval(wealth_index ~ ., household, estimator = "Gini")
#attrEval(wealth_index ~ ., household, estimator = "Relief")

household_backup <- household
# restore
household <- household_backup

inf_gain <- attrEval(wealth_index ~ ., household, estimator = "InfGain")

length(inf_gain[inf_gain>0.16])

features <- names(inf_gain)[inf_gain>0.16]
household <- household[,c("wealth_index", features)]


# Drop unused columns 
# household <- household[ , -which(names(household) %in% c("case_id",  # unique ids
#                                                          "woman_member",  # woman_member_rate
#                                                          "children_under_5", # children_member_rate
#                                                          "bedroom_number"))]  # bedroom_rate

for(colnm in colnames(household)) {
  print(ggplot(household, aes_string(x = colnm)) +
    geom_bar() + ylab("Number of Households"))
}

samp <- sample(1:nrow(household), nrow(household)*0.8)
train <- household[samp, ]
test <- household[-samp, ]

model <- rpartXse(wealth_index ~ ., train, se = 1, verbose = T)
prp(model, type = 0, extra = 101)
predicted <- predict(model, test, type = "class")
conf_matrix <- table(test$wealth_index, predicted)
conf_matrix
sum(diag(conf_matrix)) / sum(conf_matrix)


