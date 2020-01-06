library(performanceEstimation)
library(CORElearn) # feature extraction
library(adabag)

load(file = "household.data")

perfEst <- performanceEstimation(
  PredTask(wealth_index ~ ., train),
  workflowVariants(learner = "bagging",
                   learner.pars = list(mfinal = c(5, 10, 20),
                                       control = c(rpart.control(maxdepth=1),
                                                   rpart.control(maxdepth=2))
                                       )),
  EstimationTask(metrics = "acc",
                 method = CV(nFolds = 5, seed = 1234))
)


plot(perfEst)

# Best parameter set
topPerformers(perfEst, maxs = TRUE)
work_flow <- getWorkflow(topPerformers(perfEst, maxs = TRUE)[[1]][1,1], perfEst)

mfinal <- work_flow@pars[["learner.pars"]][["mfinal"]]
control <- work_flow@pars[["learner.pars"]][["control"]]

# Test
information_gain <- attrEval(wealth_index ~ ., train, estimator = "InfGain")
features <- names(information_gain)[information_gain>0.2]

model <- bagging(wealth_index ~ ., train[,c("wealth_index", features)], mfinal = mfinal,
                 control = control)

predicted <- predict(model, test, type = "class")

conf_matrix <- table(predicted, test$wealth_index)
conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy
