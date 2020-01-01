library(h2o)

load(file = "household.data")

h2oInstance <- h2o.init(ip = "localhost") # start H2O instance locally

train_frame <- as.h2o(train, "train_frame")
test_frame <- as.h2o(test, "test_frame")


model <- h2o.deeplearning(x=names(household[, -1]), 
                          y=c("wealth_index"), 
                          training_frame=train_frame,
                          hidden = c(100, 200, 100),
                          hidden_dropout_ratios = c(0.4, 0.4, 0.4),
                          activation = "RectifierWithDropout",
                          epochs = 500,
                          shuffle_training_data=TRUE)

predicted <- as.data.frame(h2o.predict(model, test_frame))

predicted$predict <- factor(predicted$predict, 
                            levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))

predicted$target <- test$wealth_index

conf_matrix <- table(predicted$predict, predicted$target)
conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

h2o.shutdown(prompt = F)
