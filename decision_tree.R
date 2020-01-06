library(performanceEstimation)
library(CORElearn) # feature extraction
library(DMwR2) # decision tree
library(rpart)
library(rpart.plot)

load(file = "household.data")

decision_tree <- function(form, train, test, inf_gain, prune_se) {
  
  information_gain <- attrEval(form, train, estimator = "InfGain")
  features <- names(information_gain)[information_gain>inf_gain]
  
  model <- rpartXse(form, train[,c("wealth_index", features)], se = prune_se)
  predictions <- predict(model, test, type = "class")
  
  list(trues = responseValues(form, test),
       preds = predictions)
  
}

perfEst <- performanceEstimation(
          PredTask(wealth_index ~ ., train),
          workflowVariants(wf = "decision_tree", 
                           inf_gain = c(0.20, 0.25, 0.30, 0.40, 0.5), 
                           prune_se = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)),
          EstimationTask(metrics = "acc",
                         method = CV(nFolds = 5, seed = 1234)))

plot(perfEst)
save(list=c("perfEst"), file = "decision_tree.perf")

# Best parameter set
topPerformers(perfEst, maxs = TRUE)
work_flow <- getWorkflow(topPerformers(perfEst, maxs = TRUE)[[1]][1,1], perfEst)

inf_gain <- work_flow@pars[["learner.pars"]][["inf_gain"]]
prune_se <- work_flow@pars[["learner.pars"]][["prune_se"]]

# Test
true_pred = decision_tree(wealth_index ~ ., train, test, inf_gain, prune_se)
true_pred <- as_tibble(true_pred)
test_conf_matrix <- table(true_pred$preds, true_pred$trues)
test_conf_matrix
test_accuracy <- sum(diag(test_conf_matrix)) / sum(test_conf_matrix)
test_accuracy
