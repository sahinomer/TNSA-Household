library(performanceEstimation)
library(adabag)

load(file = "household.data")


ada_bagging <- function(form, train, test, mfinal, maxdepth) {
  
  model <- bagging(wealth_index ~ ., train, mfinal = mfinal, control = rpart.control(maxdepth=maxdepth))
  
  predicted <- predict(model, test, type = "class")
  
  list(trues = responseValues(form, test),
       preds = predicted$class)
  
}


perfEst <- performanceEstimation(
  PredTask(wealth_index ~ ., train),
  workflowVariants(wf = "ada_bagging",
                   mfinal = c(20, 80, 120, 200),
                   maxdepth = c(5, 10, 20)
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
maxdepth <- work_flow@pars[["maxdepth"]]

# Test
model <- bagging(wealth_index ~ ., train, mfinal = mfinal, control = rpart.control(maxdepth=maxdepth))

predicted <- predict(model, test, type = "class")

predicted$confusion
sum(diag(predicted$confusion)) / sum(predicted$confusion)
