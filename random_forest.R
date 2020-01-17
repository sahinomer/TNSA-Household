library(performanceEstimation)
library(randomForest)

load(file = "household.data")

perfEst <- performanceEstimation(
  PredTask(wealth_index ~ ., train),
  workflowVariants(learner = "randomForest",
                   learner.pars = list(ntree = c(100, 200, 300, 400, 500))),
  EstimationTask(metrics = "acc",
                 method = CV(nFolds = 5, seed = 1234))
)


plot(perfEst)
save(list=c("perfEst"), file = "randomforest.perf")

# Best parameter set
topPerformers(perfEst, maxs = TRUE)
work_flow <- getWorkflow(topPerformers(perfEst, maxs = TRUE)[[1]][1,1], perfEst)

ntree <- work_flow@pars[["learner.pars"]][["ntree"]]

# Test
model <- randomForest(wealth_index ~ ., train, ntree = ntree)

predicted <- predict(model, test, type = "class")

conf_matrix <- table(predicted, test$wealth_index)
conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy
