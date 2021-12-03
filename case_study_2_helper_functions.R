library(tidyverse)
library(ggcorrplot)
library(olsrr)
library(stringr)
library(caret)
library(CORElearn)
library(AppliedPredictiveModeling)
library(mlr)
library(randomForestSRC)
library(parallelMap)
library(utils)
library(reshape2)

####################################  PLOTTING FUNCTIONS ####################################  

get_plot_labels <- function(plot_kind, plot_type_info, extra_info=NULL){
  
  if(plot_kind == "correlation"){
    plot_title <- paste0("Heatmap of ", str_to_title(plot_type_info), " Correlation Coefficients")
    return(plot_title) 
  }else if(plot_kind == "categorical_proportions"){
    plot_title <- paste0("Proportion of employees who left the company,\nfor each level of ", plot_type_info)
    plot_ylabel <- paste0("Proportion of attrition per level of ", plot_type_info)
    plot_xlabel <- plot_type_info
    return(list(title=plot_title, xlabel=plot_xlabel, y_label=plot_ylabel))
  }
  

}

get_proportion_cis <- function(df){

  attrition_counts <- df %>%
    group_by(categorical_predictor) %>% 
    summarise(attrition_count=sum(binary_target), 
                 num_trials=length(binary_target))
  
  # Initialize new dataframe columns we want to create
  attrition_counts[,c("proportion_estimates", "lower_ci", "upper_ci", 
                      "p_values", "test_statistic")] <- -1
  
  for(row_num in 1:nrow(attrition_counts)){
    test_result <- prop.test(x=as.numeric(unname(attrition_counts[row_num,"attrition_count"])),
                             n=as.numeric(unname(attrition_counts[row_num,"num_trials"])))
    
    attrition_counts[row_num,"proportion_estimates"] <- test_result$estimate
    attrition_counts[row_num,"lower_ci"] <-  test_result$conf.int[1]
    attrition_counts[row_num,"upper_ci"] <- test_result$conf.int[2]
    attrition_counts[row_num,"p_values"] <- test_result$p.value
    attrition_counts[row_num,"statistic"] <- test_result$statistic
    
  }  
  
  attrition_counts[,"overall_proportion"] <- sum(df[,"binary_target"]) / length(df[,"binary_target"])
  
  return(attrition_counts)
  
}


run_two_sample_test <- function(df, binary_variable, continuous_variable, test_type="t", 
                                exact, equal_variance=NULL,round_digits, conf_level=0.95) {
  
  unique_values <- unique(df[,binary_variable])
  value1 <- unique_values[1]
  value2 <- unique_values[2]
  
  group1 <- df[df[binary_variable] == value1, continuous_variable]
  group2 <- df[df[binary_variable] == value2, continuous_variable]
  
  if(test_type == "t"){
    # T-test
    test <- t.test(x=group1, y=group2, var.equal=equal_variance, conf.level=conf_level)
    
    p_value <- round(test$p.value, round_digits)
    test_stat <- round(test$statistic, round_digits)
    lower_ci <- round(test$conf.int[1], round_digits)
    upper_ci <- round(test$conf.int[2], round_digits)
    est <- round(test$estimate, round_digits)
    est_diff_means <- round(est[["mean of x"]] - est[["mean of y"]], round_digits)
    
    txt <- paste0("Est. Diff Means = ", est_diff_means, "\n", 
                  "T-stat = ", test_stat, "\n", 
                  "P-value = ", p_value, "\n",
                  "CI: (", lower_ci, ", ", 
                  upper_ci, ")")
    
  }else if(test_type=="rank_sum"){
    
    test <- wilcox.test(x=group1, y=group2, conf.level=conf_level, exact=exact, conf.int=TRUE)
    
    p_value <- round(test$p.value, round_digits)
    test_stat <- round(test$statistic, round_digits)
    lower_ci <- round(test$conf.int[1], round_digits)
    upper_ci <- round(test$conf.int[2], round_digits)
    est <- round(test$estimate, round_digits)
    est_diff_means <- NULL
    
    txt <- paste0("T-stat = ", test_stat, "\n", 
                  "P-value = ", p_value, "\n",
                  "CI: (", lower_ci, ", ", 
                  upper_ci, ")")
  }
  
  return_list <- list(estimate=est, p_value=p_value, test_stat=test_stat, 
                      est_diff_means=est_diff_means, lower_ci=lower_ci, 
                      upper_ci=upper_ci, text=txt)
  
  return(return_list)
  
}

####################################  END PLOTTING FUNCTIONS ####################################  



####################################  DATA CLEANING FUNCTIONS ####################################  

categorical_ints_to_factors <- function(df){
  
  df[,"EnvironmentSatisfaction"] <- factor(df[,"EnvironmentSatisfaction"], 
                                           levels=c(1, 2, 3, 4))
  
  df[,"EnvironmentSatisfactionB"] <- factor(df[,"EnvironmentSatisfactionB"], 
                                            levels=c(0, 1))
  
  df[,"StockOptionLevel"] <- factor(df[,"StockOptionLevel"], 
                                    levels=c(0, 1, 2, 3))
  
  df[,"RelationshipSatisfaction"] <- factor(df[,"RelationshipSatisfaction"], 
                                            levels=c(1, 2, 3, 4))
  
  
  df[,"RelationshipSatisfactionB"] <- factor(df[,"RelationshipSatisfactionB"], 
                                             levels=c(0, 1))
  
  
  df[,"JobInvolvement"] <- factor(df[,"JobInvolvement"], 
                                  levels=c(1, 2, 3, 4))
  
  df[,"JobInvolvementB"] <- factor(df[,"JobInvolvementB"], 
                                   levels=c(0, 1))
  
  
  df[,"Education"] <- factor(df[,"Education"], 
                             levels=c(1, 2, 3, 4, 5))
  
  df[,"JobSatisfaction"] <- factor(df[,"JobSatisfaction"], 
                                   levels=c(1, 2, 3, 4))
  
  df[,"WorkLifeBalance"] <- factor(df[,"WorkLifeBalance"], 
                                   levels=c(1, 2, 3, 4))
  
  df[,"WorkLifeBalanceB"] <- factor(df[,"WorkLifeBalanceB"], 
                                    levels=c(0, 1))
  
  df[,"JobInvolvement"] <- factor(df[,"JobInvolvement"], 
                                  levels=c(1, 2, 3, 4))
  
  df[,"JobInvolvementB"] <- factor(df[,"JobInvolvementB"], 
                                   levels=c(0,1))
  
  df[,"JobLevel"] <- factor(df[,"JobLevel"],
                            levels=c(1, 2, 3, 4, 5))
  
  df[,"JobLevelB"] <- factor(df[,"JobLevelB"],
                             levels=c(0, 1))
  
  df[,"NumCompaniesWorkedB"] <- factor(df[,"NumCompaniesWorkedB"], 
                                       levels=c(0,1))
  
  return(df)
  
}


# VARIABLES TO TRY BINNING: RelationshipSatisfaction WorkLifeBalance NumCompaniesWorked
#                           JobLevel JobInvolvement YearsInCurrentRole EnvironmentSatisfaction
create_binned_features <- function(df){
  
  df[,"RelationshipSatisfactionB"] <- ifelse(df[,"RelationshipSatisfaction"]==1,0,1)
  df[,"WorkLifeBalanceB"] <- ifelse(df[,"WorkLifeBalance"]==1,0,1)
  df[,"NumCompaniesWorkedB"] <- ifelse(df[,"NumCompaniesWorked"] %in% c(0, 1, 2, 3, 4), 0,1)
  df[,"JobLevelB"] <- ifelse(df[,"JobLevel"]==1,0,1)
  df[,"JobInvolvementB"] <- ifelse(df[,"JobInvolvement"]==1,0,1)
  df[,"EnvironmentSatisfactionB"] <- ifelse(df[,"EnvironmentSatisfaction"]==1,0,1)
  
  return(df)
  
}

# Helper to data cleaning
convert_characters_to_factor <- function(df, additional_columns=c("Attrition"), 
                                         exclude_columns=c("Attrition_Text")){
  
  current_names <- names(df)
  
  for(name_index in 1:length(current_names)){
    
    current_name <- names(df)[name_index]
    
    if((class(df[, current_name]) == "character" & !(current_name %in% exclude_columns)) | 
       (current_name %in% additional_columns)){
      df[,current_name] <- factor(df[,current_name], levels=unique(df[,current_name]))
    }
  }
  return(df)
}


convert_characters_to_numeric <- function(df){
  
  # Convert OverTime column to numeric
  df[df["OverTime"] == "No", "OverTime"] <- 0
  df[df["OverTime"] == "Yes", "OverTime"] <- 1
  
  df[,"OverTime"] <- as.numeric(df[,"OverTime"])
  
  df[df["MaritalStatus"] == "Divorced", "MaritalStatus"] <- 0
  df[df["MaritalStatus"] == "Married", "MaritalStatus"] <- 1
  df[df["MaritalStatus"] == "Single", "MaritalStatus"] <- 2
  
  df[,"MaritalStatus"] <- as.numeric(df[,"MaritalStatus"])
  
  return(df)
}


factor_to_numeric <- function(df, col_name){
  
  df[,col_name] <- as.character(df[,col_name])
  df[,col_name] <- as.numeric(df[,col_name])
  return(df)
}

####################################  END DATA CLEANING FUNCTIONS ####################################  



####################################  FEATURE IMPORTANCE FUNCTIONS ####################################  

####### RELIEF

get_name_index <- function(df, name){
  num_names <- length(names(df))
  for(name_index in 1:num_names){
    if(names(df)[name_index] == name){
      return(name_index)
    }
  }
  return(-1)
}

relief_variable_setup <- function(df, features, binary_target, target_levels=c(0,1)){
  
  new_df <- df[,(names(df) %in% features)]
  new_df[, "binary_target"] <- df[,binary_target]
  new_df$binary_target <- factor(new_df$binary_target, levels=target_levels)
  return(new_df)
  
}

get_relief_score_df <- function(rs){
  
  wide_df <- spread(rs$permutations, Predictor, value)
  
  rs_names <- names(rs$standardized)
  
  for(name_index in 1:length(rs_names)){
    
    feature_name <- rs_names[name_index]
    
    std_score <- rs$standardized[feature_name][[1]]
    observed_score <- rs$observed[feature_name][[1]]
    
    std_name <- paste0(feature_name, "_std_score")
    obs_name <- paste0(feature_name, "_obs_score")
    
    wide_df[,std_name] <- std_score
    wide_df[,obs_name] <- observed_score
  }
  
  wide_df[,"estimator"] <- rs$options[[1]]
  wide_df[,"relief_iterations"] <- rs$options[[2]]
  
  return(wide_df)
  
}

# Filter on column name
filter_relief_df <- function(relief_df, score_type){
  
  if(score_type == "standardized"){
    relief_df <- relief_df[,str_subset(names(relief_df), "_std_score")]
  }
  else if(score_type == "observed"){
    relief_df <- relief_df[,str_subset(names(relief_df), "_obs_score")]
  }
  return(relief_df)
}

# Rename columns
rename_relief_columns <- function(relief_df){
  
  current_names <- names(relief_df)
  
  for(name_index in 1:length(current_names)){
    
    current_name <- current_names[name_index]
    new_name <- str_split(current_name, pattern="_", simplify=TRUE)[[1]]
    
    # Add the new name and remove hte old one
    relief_df[,new_name] <- relief_df[,current_name]
    relief_df <- relief_df[,!(names(relief_df) %in% current_name)]
  }
  
  return(relief_df)
}

prepare_relief_df <- function(relief_df, score_type){
  
  relief_df <- filter_relief_df(relief_df=relief_df, 
                                score_type=score_type)
  
  relief_df <- rename_relief_columns(relief_df=relief_df)
  
  relief_df <- relief_df[1,]
  
  long_df <- melt(relief_df, 
                  measure.vars=names(relief_df), 
                  variable.name="feature",
                  value.name="score")
  
  if(score_type == "standardized"){
    long_df[,"score"] <- abs(long_df[,"score"])
  }
  
  return(long_df)
  
}

####################################  END FEATURE IMPORTANCE FUNCTIONS ####################################  

## NAIVE BAYES
get_predictor_combos_manual <- function(features){
  
  # CREATES A LIST OF LISTS, WHERE EACH SUBLIST CONTAINS A COMBINATION OF FEATURES
  for(num_features in 1:length(features)){
    
    new_combos <- utils::combn(x=features,
                               m=num_features,
                               simplify=FALSE)
    
    if(num_features == 1){
      combined_combos <- new_combos
    }else{
      combined_combos <- c(combined_combos, new_combos)
    }
  }
  
  return(combined_combos)
}


combine_nb_train_test_dfs <- function(train_df, test_df){
  
  train_agg_df <- data.frame(tnr.train.mean=mean(train_df[,"tnr"]),
                             tpr.train.mean=mean(train_df[,"tpr"]),
                             acc.train.mean=mean(train_df[,"acc"]),
                             f1.train.mean=mean(train_df[,"f1"]),
                             tp.train.mean=mean(train_df[,"tp"]),
                             fp.train.mean=mean(train_df[,"fp"]),
                             tn.train.mean=mean(train_df[,"tn"]),
                             fn.train.mean=mean(train_df[,"fn"]),
                             ppv.train.mean=mean(train_df[,"ppv"]))
  
  train_test_df <- cbind(test_df, train_agg_df)
  
  return(train_test_df)
  
}
### END NAIVE BAYES

### KNN
create_knn_gs_result_df <- function(knn_gs_result){
  
  # Create the dataframe of test data metrics
  test_df <- data.frame(as.list(knn_gs_result$tuner$y))  
  
  # Create the dataframe of training metrics
  train_names <- names(knn_gs_result$metrics)
  names(knn_gs_result$metrics) <- sapply(train_names, function(x) paste(x, "train", sep="_"))
  train_df <- data.frame(as.list(knn_gs_result$metrics))
  
  # Concatenate the test and train together column wise (wider).
  gs_df <- cbind(test_df, train_df)
  
  gs_df[,"best_k"] <- knn_gs_result$tuner$x[[1]]
  
  return(gs_df)
}
### END KNN

### LINEAR REGRESSION
# Subset selection with olsrr and cross validation with caret
# 
get_caret_cv_method <- function(method, cv_folds, repeats=NULL){
  
  if(method == "cv"){
    
    train_control <- caret::trainControl(method=method,
                                         number=cv_folds)
  }else if(method == "repeatedcv"){
    
    train_control <- caret::trainControl(method=method,
                                         number=cv_folds,
                                         repeats=repeats)
    
  }else if(method == "LOOCV"){
    
    train_control <- caret::trainControl(method=method)
  }
  
  return(train_control)
}

get_best_feature_sets <- function(df, response_var, features, feature_set_type, metric){
  
  filtered_df <- df[, (names(df) %in% features)]
  filtered_df[, response_var] <- df[,c(response_var)]
  
  filtered_df <<- as.data.frame(filtered_df)
  
  lm_formula <- as.formula(paste0(response_var, "~", stringr::str_c(features, 
                                                                    collapse=" + ")))
  
  lm_model <<- stats::lm(formula=lm_formula, 
                         data=filtered_df)
  
  
  if(feature_set_type == "best_subset"){
    
    result <- olsrr::ols_step_best_subset(lm_model, 
                                          metric = metric)
    
  }else if(feature_set_type == "all_possible"){
    
    result <- olsrr::ols_step_all_possible(lm_model)
    
  }
  
  return_list <<- list(df=filtered_df,
                       lm_fit=lm_model,
                       result=result)
  
  return(return_list)
  
}

# Caret Package doesn't easily allow us to get the RMSE for the training dataset... so this function
# manually calculates its.
get_mean_training_rmse <- function(train_result, model_formula, model_df){
  
  train_indicies <- train_result$control$index
  rmses <- c()
  
  for(index in 1:length(train_indicies)){
    
    train_index <- train_indicies[[index]]
    
    train_df <- model_df[train_index,]
    
    fit <- lm(model_formula,
              data=train_df)
    
    
    rmses[[index]] <- summary(fit)$sigma
  }
  
  mean_rmse <- mean(unlist(rmses))
  
  return(mean_rmse)
}



####################################  MODELING FUNCTIONS #################################### 




##################################  END MODELING FUNCTIONS ##################################
