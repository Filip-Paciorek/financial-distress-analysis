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
