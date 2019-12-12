

parameter_set <- expand.grid(scale = c(TRUE, FALSE), 
                             type = c("C-classification", "eps-regression"))

####
# Error in svm.default(x, y, scale = scale, ..., na.action = na.action) : 
# Need numeric dependent variable for regression.
####


for (row in 1:nrow(parameter_set)) {
  
  scale <- parameter_set[row, "scale"]
  type <- parameter_set[row, "type"]
  
  model <- svm(wealth_index ~ ., train, scale = scale, type = type)
  predicted <- predict(model, validation, type = "class")
  conf_matrix <- table(predicted, validation$wealth_index)
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  parameter_set$accuracy[row] = accuracy
  
}


# Best parameter set
sorted_parameters = parameter_set[order(parameter_set$accuracy, decreasing = TRUE), ]

scale <- parameter_set[1, "scale"]
type <- parameter_set[1, "type"]

model <- svm(wealth_index ~ ., train, scale = scale, type = type)

# test
predicted <- predict(model, test, type = "class")
test_conf_matrix <- table(predicted, test$wealth_index)
test_conf_matrix
test_accuracy <- sum(diag(test_conf_matrix)) / sum(test_conf_matrix)
test_accuracy
