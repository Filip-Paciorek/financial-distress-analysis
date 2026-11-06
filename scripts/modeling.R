#load packages
packages <- c("foreign","dplyr","here","tidyr","mlr3verse","skimr");

missing <- packages[!(packages %in% installed.packages()[,"Package"])];

if (length(missing) > 0)
{
  install.packages(missing)
}
lapply(packages, library, character.only = TRUE)


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

#a basic model without noisy columns
slimmer_clean_df <- subset(slim_clean_df,select=c(ebit_to_total_assets,net_profit_to_total_assets,working_capital_to_total_assets,class))
task2 <- TaskClassif$new("bankrupcyFinder2",slimmer_clean_df,target="class")
learner_logreg2 <- lrn("classif.log_reg")

learner_logreg2$train(task2,row_ids=train_set)
pred_logreg2 <- learner_logreg2$predict(task2,row_ids=test_set)
pred_logreg2$confusion
