#load packages
packages <- c("foreign","dplyr","here","tidyr","mlr3verse","skimr","ranger", "smotefamily","mlr3pipelines","mlr3tuning");

missing <- packages[!(packages %in% installed.packages()[,"Package"])];

if (length(missing) > 0)
{
  install.packages(missing)
}
lapply(packages, library, character.only = TRUE)

###################################
# SLIM ANALYSIS                   #
###################################

#load the prepped data
slim_clean_df <- readRDS(here("data", "processed", "clean_5th_year.rds"))
skim(slim_clean_df)

#train a benchmark regression
task <- TaskClassif$new("bankrupcyFinder",slim_clean_df,target="class")
learner_logreg <- lrn("classif.log_reg")

train_set <- sample(task$row_ids, 0.8 * task$nrow)
test_set <- setdiff(task$row_ids,train_set)

learner_logreg$train(task, row_ids = train_set)

#check performance
learner_logreg$model
summary(learner_logreg$model)

# predict
pred_logreg <- learner_logreg$predict(task,row_ids = test_set)
pred_logreg$confusion

#basic random forest
learner_rf <- lrn("classif.ranger",importance="permutation")
learner_rf$train(task,row_ids=train_set)

learner_rf$importance()

pred_rf <- learner_rf$predict(task,row_ids=test_set)

pred_rf$confusion

#random forest with weights
test_weights <- seq(5,25)
test_thresholds <- seq(0.1,0.5,by=0.05)
weights_results <- data.frame(bankrupt_weight = numeric(),bankrupt_threshold = numeric(), AUC = numeric(),bankrupcies_found = numeric(),false_alarms=numeric())
for (weight in test_weights)
{
  lrnr <- lrn("classif.ranger",predict_type="prob",class.weights=c("0"=1,"1"=weight))
  
  lrnr$train(task,row_ids=train_set)
  
  pred <- lrnr$predict(task,row_ids=test_set)
  
  auc <- pred$score(msr("classif.auc"))
  for (threshold in test_thresholds)
  {
    pred_temp <- pred$clone()
    pred_temp$set_threshold(c("0"=(1-threshold),"1"=threshold))
    weights_results <- rbind(weights_results, data.frame(
      bankrupt_weight = weight,
      bankrupt_threshold = threshold,
      AUC = auc, 
      bankrupcies_found = pred_temp$confusion["1", "1"], 
      false_alarms = pred_temp$confusion["1", "0"]
    ))
  }
  
}

weights_results <- weights_results[order(-weights_results$bankrupcies_found,weights_results$false_alarms),]
head(weights_results,15)

#final model for slim
learner_final_slim <- lrn("classif.ranger", predict_type = "prob", class.weights = c("0" = 1, "1" = 20))
learner_final_slim$train(task_fat, row_ids = train_set_fat)

pred_final_slim <- learner_final_slim$predict(task_fat, row_ids = test_set_fat)

pred_final_slim$set_threshold(c("0" = 0.9, "1" = 0.1))

pred_final_slim$confusion


#random forest with better thresholds and SMOTE
df_smote <- po("smote", dup_size=13) %>>% lrn("classif.ranger", predict_type= "prob")

learner_rf_smote <- as_learner(df_smote)

learner_rf_smote$train(task,row_ids=train_set)
pred_smote <- learner_rf_smote$predict(task,row_ids=test_set)

pred_smote$set_threshold(c("0"=0.85,"1"=0.15))
print(pred_smote$confusion)
#a basic model without noisy columns
slimmer_clean_df <- subset(slim_clean_df,select=c(ebit_to_total_assets,net_profit_to_total_assets,working_capital_to_total_assets,class))
task2 <- TaskClassif$new("bankrupcyFinder2",slimmer_clean_df,target="class")
learner_logreg2 <- lrn("classif.log_reg")

learner_logreg2$train(task2,row_ids=train_set)
pred_logreg2 <- learner_logreg2$predict(task2,row_ids=test_set)
pred_logreg2$confusion


###############################
# FAT ANALYSIS                #
###############################
fat_clean_df <- readRDS(here("data", "processed", "clean_5th_year_fat.rds"))
skim(fat_clean_df)

#train a benchmark regression
task_fat <- TaskClassif$new("bankrupcyFinderFat",fat_clean_df,target="class")
learner_logreg_fat <- lrn("classif.log_reg")

train_set_fat <- sample(task_fat$row_ids, 0.8 * task_fat$nrow)
test_set_fat <- setdiff(task_fat$row_ids,train_set_fat)

learner_logreg_fat$train(task_fat, row_ids = train_set_fat)


# predict
pred_logreg_fat <- learner_logreg_fat$predict(task_fat,row_ids = test_set_fat)
pred_logreg_fat$confusion

#basic random forest - worse performance for this many variables
learner_rf_fat <- lrn("classif.ranger",importance="permutation")
learner_rf_fat$train(task_fat,row_ids=train_set_fat)

learner_rf_fat$importance()

pred_rf_fat <- learner_rf_fat$predict(task_fat,row_ids=test_set_fat)

pred_rf_fat$confusion

#random forest with weights
learner_rf2_fat <- lrn("classif.ranger",predict_type="prob",class.weights=c("0"=1,"1"=13.5),importance="permutation")

learner_rf2_fat$train(task_fat,row_ids=train_set_fat)
learner_rf2_fat$importance()
pred_rf2_fat <- learner_rf2_fat$predict(task_fat,row_ids=test_set_fat)

pred_rf2_fat$confusion



#random forest with better thresholds
pred_rf2_fat$set_threshold(c("0"=0.85,"1"=0.15))
pred_rf2_fat$confusion
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

surrogate_tree <- rpart(class ~ ., data = fat_clean_df, method = "class", control = rpart.control(maxdepth = 3))
png("model_decision_tree.png", width = 1400, height = 900, res = 150)
rpart.plot(surrogate_tree, 
           type = 4,                     
           extra = 104,                  
           box.palette = c("skyblue", "pink"), 
           shadow.col = "gray",          
           branch.lty = 3,              
           main = "Uproszczona logika modelu Random Forest")
weights_results_fat <- data.frame(bankrupt_weight = numeric(),bankrupt_threshold = numeric(), AUC = numeric(),bankrupcies_found = numeric(),false_alarms=numeric())
for (weight in test_weights)
{
  lrnr <- lrn("classif.ranger",predict_type="prob",class.weights=c("0"=1,"1"=weight))
  
  lrnr$train(task_fat,row_ids=train_set_fat)
  
  pred <- lrnr$predict(task_fat,row_ids=test_set_fat)
  
  auc <- pred$score(msr("classif.auc"))
  for (threshold in test_thresholds)
  {
    pred_temp <- pred$clone()
    pred_temp$set_threshold(c("0"=(1-threshold),"1"=threshold))
    weights_results_fat <- rbind(weights_results_fat, data.frame(
      bankrupt_weight = weight,
      bankrupt_threshold = threshold,
      AUC = auc, 
      bankrupcies_found = pred_temp$confusion["1", "1"], 
      false_alarms = pred_temp$confusion["1", "0"]
    ))
  }
  
}

weights_results_fat <- weights_results_fat[order(-weights_results_fat$bankrupcies_found,weights_results_fat$false_alarms),]
head(weights_results_fat,15)
tail(weights_results_fat,15)
best_result_alternatives <- subset(weights_results_fat, 
                         bankrupcies_found >= 60 & bankrupcies_found <= 65 &
                           false_alarms >= 80 & false_alarms <= 105)

best_result_alternatives <- best_result_alternatives[order(-best_result_alternatives$bankrupcies_found, best_result_alternatives$false_alarms), ]

best_result_alternatives

search_space <- ps(mtry = p_int(lower=3,upper=15), min.node.size = p_int(lower = 2, upper=10))
auto_tuners <- auto_tuner(
  tuner = tnr("random_search"),
  learner = learner_rf2_fat,
  resampling = rsmp("cv",folds=3),
  measure = msr("classif.auc"),
  search_space = search_space,
  term_evals = 20
)
print("Train")
auto_tuners$train(task_fat,row_ids=train_set_fat)
print("Najlepsze parametry")
print(auto_tuners$tuning_result)

pred_tuned <- auto_tuners$predict(task_fat,row_ids=test_set_fat)

pred_tuned$set_threshold(c("0"=0.85,"1"=0.15))
pred_tuned$confusion
#random forest with better thresholds and SMOTE
#df_smote_fat <- po("smote", dup_size=13) %>>% lrn("classif.ranger", predict_type= "prob")

#learner_rf_smote_fat <- as_learner(df_smote_fat)

#learner_rf_smote_fat$train(task_fat,row_ids=train_set_fat)
#pred_smote_fat <- learner_rf_smote_fat$predict(task_fat,row_ids=test_set_fat)

#pred_smote_fat$set_threshold(c("0"=0.85,"1"=0.15))
#print(pred_smote_fat$confusion)
#a basic model without noisy columns
slimmer_clean_df <- subset(slim_clean_df,select=c(ebit_to_total_assets,net_profit_to_total_assets,working_capital_to_total_assets,class))
task2 <- TaskClassif$new("bankrupcyFinder2",slimmer_clean_df,target="class")
learner_logreg2 <- lrn("classif.log_reg")

learner_logreg2$train(task2,row_ids=train_set)
pred_logreg2 <- learner_logreg2$predict(task2,row_ids=test_set)
pred_logreg2$confusion
