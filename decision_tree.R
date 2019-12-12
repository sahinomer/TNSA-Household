

parameter_set <- expand.grid(inf_gain_th = c(0.20, 0.25, 0.30, 0.40, 0.5), 
                             prune_se = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))


inf_gain <- attrEval(wealth_index ~ ., train, estimator = "InfGain")

for (row in 1:nrow(parameter_set)) {
  inf_gain_th <- parameter_set[row, "inf_gain_th"]
  prune_se  <- parameter_set[row, "prune_se"]
  
  features <- names(inf_gain)[inf_gain>inf_gain_th]
  sub_train <- train[,c("wealth_index", features)]
  
  model <- rpartXse(wealth_index ~ ., sub_train, se = prune_se)
  predicted <- predict(model, validation, type = "class")
  conf_matrix <- table(predicted, validation$wealth_index)
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  parameter_set$accuracy[row] = accuracy
  
}


# Best parameter set
sorted_parameters = parameter_set[order(parameter_set$accuracy, decreasing = TRUE), ]

inf_gain_th <- parameter_set[1, "inf_gain_th"]
prune_se  <- parameter_set[1, "prune_se"]

features <- names(inf_gain)[inf_gain>inf_gain_th]
sub_train <- train[,c("wealth_index", features)]

model <- rpartXse(wealth_index ~ ., sub_train, se = prune_se)

# test
predicted <- predict(model, test, type = "class")
test_conf_matrix <- table(predicted, test$wealth_index)
test_conf_matrix
test_accuracy <- sum(diag(test_conf_matrix)) / sum(test_conf_matrix)
test_accuracy
