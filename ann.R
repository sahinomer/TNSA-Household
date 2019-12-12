library(h2o)

parameter_set <- expand.grid(var1 = c("default"))

h2oInstance <- h2o.init(ip = "localhost") # start H2O instance locally



trnH <- as.h2o(train, "trnH")
valH <- as.h2o(validation, "valH")
tstH <- as.h2o(test, "tstH")


for (row in 1:nrow(parameter_set)) {
  
  # scale <- parameter_set[row, "scale"]
  
  model <- h2o.deeplearning(x=1:ncol(train)-1, y=5, training_frame=trnH,
                            hidden = c(100, 100, 100, 100), epochs = 500)
  
  predicted <- as.vector(h2o.predict(model, valH))
  
  conf_matrix <- table(predicted, validation$wealth_index)
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  parameter_set$accuracy[row] = accuracy
  
}


# Best parameter set
sorted_parameters = parameter_set[order(parameter_set$accuracy, decreasing = TRUE), ]

# scale <- parameter_set[1, "scale"]
# type <- parameter_set[1, "type"]


model <- h2o.deeplearning(x=1:ncol(train)-1, y=5, training_frame=trnH,
                          hidden = c(100, 100, 100, 100), epochs = 500)


# test
predicted <- as.vector(h2o.predict(model, tstH))
test_conf_matrix <- table(predicted, test$wealth_index)
test_conf_matrix
test_accuracy <- sum(diag(test_conf_matrix)) / sum(test_conf_matrix)
test_accuracy

h2o.shutdown(prompt = F)

