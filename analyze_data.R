library(readr)
library(tidyr)
library(ggplot2)
library(DMwR2)
library(CORElearn) # feature extraction
library(rpart) # decision tree
library(rpart.plot)
library(e1071) # svm
library(performanceEstimation)

household <- read.table(file="household.csv", sep=";", header = TRUE, na.strings = c("" , " ","\t ", "Missing" ))

household <- na.omit(household)

household <- household[, -1]  # drop case id

household <- separate(household, region_combined, c("compass", "region", "settlement"), sep = '-')



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


# Split train, validation and test set
samples <- sample(1:nrow(household), nrow(household)*0.1)
train <- household[samples, ]
test <- household[-samples, ]

# samples <- sample(1:nrow(test_validation), nrow(test_validation)*0.5)
# validation <- test_validation[samples, ]
# test <- test_validation[-samples,]




# Plot distribution of data
for(colnm in colnames(household)) {
  print(ggplot(household, aes_string(x = colnm)) +
          geom_bar() + ylab("Number of Households"))
}

###########################################################################

# # Feature Selection
# infoCore(what = "attrEval")  # classification
# infoCore(what = "attrEvalReg")  # regression
# 
# #attrEval(wealth_index ~ ., household, estimator = "GainRatio")
# #attrEval(wealth_index ~ ., household, estimator = "Gini")
# #attrEval(wealth_index ~ ., household, estimator = "Relief")
# 
# household_backup <- household
# # restore
# household <- household_backup
# 
# inf_gain <- attrEval(wealth_index ~ ., household, estimator = "InfGain")
# 
# length(inf_gain[inf_gain>0.1])
# 
# features <- names(inf_gain)[inf_gain>0.1]
# household <- household[,c("wealth_index", features)]
# 



samp <- sample(1:nrow(household), nrow(household)*0.8)
train <- household[samp, ]
test <- household[-samp, ]

# decision tree
# model <- rpartXse(wealth_index ~ ., train, se = 1, verbose = T)
# prp(model, type = 0, extra = 101)

# svm
model <- svm(wealth_index ~ ., train)

predicted <- predict(model, test, type = "class")
conf_matrix <- table(test$wealth_index, predicted)
conf_matrix
sum(diag(conf_matrix)) / sum(conf_matrix)


