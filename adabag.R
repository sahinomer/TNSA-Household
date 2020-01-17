library(performanceEstimation)
library(adabag)

load(file = "household.data")


ada_bagging <- function(form, train, test, mfinal, control) {
  
  model <- bagging(wealth_index ~ ., train, mfinal = mfinal, control = control)
  
  predicted <- predict(model, test, type = "class")
  
  list(trues = responseValues(form, test),
       preds = predicted$class)
  
}


perfEst <- performanceEstimation(
  PredTask(wealth_index ~ ., train),
  workflowVariants(wf = "ada_bagging",
                   mfinal = c(5, 10, 20, 40, 80),
                   control = c(rpart.control(maxdepth=5), rpart.control(maxdepth=10))
                  ),
  EstimationTask(metrics = "acc",
                 method = CV(nFolds = 5, seed = 1234))
)


plot(perfEst)
save(list=c("perfEst"), file = "adabag.perf")

# Best parameter set
topPerformers(perfEst, maxs = TRUE)
work_flow <- getWorkflow(topPerformers(perfEst, maxs = TRUE)[[1]][1,1], perfEst)

mfinal <- work_flow@pars[["mfinal"]]
control <- work_flow@pars[["control"]]

# Test
model <- bagging(wealth_index ~ ., train, mfinal = mfinal, control = control)

predicted <- predict(model, test, type = "class")

predicted$confusion
sum(diag(predicted$confusion)) / sum(predicted$confusion)
