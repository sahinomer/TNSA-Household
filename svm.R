library(performanceEstimation)
library(e1071) # svm

load(file = "household.data")

perfEst <- performanceEstimation(
        PredTask(wealth_index ~ ., train),
        workflowVariants(learner = "svm",
                         learner.pars = list(cost = c(1, 5, 10),
                                             gamma = c(0.01, 0.001))),
        EstimationTask(metrics = "acc",
                       method = CV(nFolds = 10, seed = 1234)))


plot(perfEst)

# Best parameter set
topPerformers(perfEst, maxs = TRUE)
work_flow <- getWorkflow(topPerformers(perfEst, maxs = TRUE)[[1]][1,1], perfEst)

cost <- work_flow@pars[["learner.pars"]][["cost"]]
gamma <- work_flow@pars[["learner.pars"]][["gamma"]]

# Test
model <- svm(wealth_index ~ ., train, cost = cost, gamma = gamma)
predicted <- predict(model, test, type = "class")
test_conf_matrix <- table(predicted, test$wealth_index)
test_conf_matrix
test_accuracy <- sum(diag(test_conf_matrix)) / sum(test_conf_matrix)
test_accuracy
