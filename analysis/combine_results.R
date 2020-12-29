rm(list = ls())

#set directory and source results function
#insert path to analysis directory
setwd("")
source("get_results_function.R")

#########
#results#
#########

#original SVM
train_results <- read.csv("results/train_results.csv")
test_results <- read.csv("results/test_results.csv")

#modified 1
train_results2 <- read.csv("results/train_results2.csv")
test_results2 <- read.csv("results/test_results2.csv")

#modified 2
train_results3 <- read.csv("results/train_results3.csv")
test_results3 <- read.csv("results/test_results3.csv")

results <- rbind(get_results(train_results$denied,train_results$results,train_results$minority,"svm_train"),
      get_results(train_results2$denied,train_results2$results,train_results2$minority,"svm_fair_train"),
      get_results(train_results3$denied,train_results3$results,train_results3$minority,"svm_fair_train2"),
      get_results(test_results$denied,test_results$results,test_results$minority,"svm_test"),
      get_results(test_results2$denied,test_results2$results,test_results2$minority,"svm_fair_test"),
      get_results(test_results3$denied,test_results3$results,test_results3$minority,"svm_fair_test2")
)


results <- results %>% 
  #add TNR
  mutate(overall_TNR = (tn_min + tn_maj) / (tn_min + tn_maj + fn_min + fn_maj),
         #add minority denial rate
         min_den_rate = (tp_min + fp_min) / (tn_min + fn_min + tp_min + fp_min),
         #add false negative rate
         FNR_min = fn_min / (fn_min + tp_min),
         FNR_maj = fn_maj / (fn_maj + tp_maj),
         FNR_diff = FNR_min - FNR_maj
  ) %>% 
  #multiply all rates by 100
  mutate(across(c(overall_TNR,FPR_min,FPR_maj,FPR_diff,FNR_min,FNR_maj,FNR_diff,min_den_rate),
         ~ .x * 100)) %>% 
  select(method,auc,overall_TNR,FPR_min,FPR_maj,FPR_diff,FNR_min,FNR_maj,FNR_diff,min_den_rate) %>% 
  relocate(method,auc,overall_TNR,FPR_min,FPR_maj,FPR_diff,FNR_min,FNR_maj,FNR_diff,min_den_rate)

results %>% write.csv("combined_results.csv")