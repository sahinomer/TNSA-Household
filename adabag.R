library(performanceEstimation)
library(CORElearn) # feature extraction
library(adabag)

load(file = "household.data")

perfEst <- performanceEstimation(
  PredTask(wealth_index ~ ., train),
  workflowVariants(learner = "bagging",
                   learner.pars = list(mfinal = c(5, 10, 20),
                                       control = c(rpart.control(maxdepth=5))
                                       )),
  EstimationTask(metrics = "acc",
                 method = CV(nFolds = 5, seed = 1234))
)


plot(perfEst)
save(list=c("perfEst"), file = "adabag.perf")

# Best parameter set
topPerformers(perfEst, maxs = TRUE)
work_flow <- getWorkflow(topPerformers(perfEst, maxs = TRUE)[[1]][1,1], perfEst)

mfinal <- work_flow@pars[["learner.pars"]][["mfinal"]]
control <- work_flow@pars[["learner.pars"]][["control"]]

# Test
model <- bagging(wealth_index ~ ., train, mfinal = mfinal,
                 control = control)

predicted <- predict(model, test, type = "class")

predicted$confusion
sum(diag(predicted$confusion)) / sum(predicted$confusion)
