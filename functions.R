source("./case_study_2_helper_functions.R")

# Function to filter a dataframe by either:
# 1) col_names --> a list of column names to include
# 2) remove_columns --> a list of column names to remove
#
filter_columns <- function(df, col_names=NULL, remove_columns=NULL){
  
  # If we are filtering by a list of column names to include
  if(!is.null(col_names)){
    df <- df[, names(df) %in% col_names]
  } else if(!is.null(remove_columns)){
    df <- df[, !(names(df) %in% remove_columns)]
  }
  return(df)
}

# Overall data cleaning function, should be run each time the original dataset is read in.
#
clean_data <- function(df, columns_to_drop=c("ID", "DailyRate", "HourlyRate", "MonthlyRate"),
                       add_binned_features=TRUE, char_to_factor=TRUE, char_to_numeric=TRUE,
                       cat_ints_to_factors=TRUE){
  
  # Remove all columns with only a single unique value, as they will not be useful for finding and
  # understanding differences between employees during EDA or modeling.
  # NOTE: This removes the following columns:
  #       "EmployeeCount" --> all values = 1
  #       "Over18" --> all values = "Y"
  #       "StandardHours" --> all values = 80
  df <- select_if(df, function(column) length(unique(column)) > 1)
  
  
  # Convert the "Attrition" column from being a column of "Yes" and "No"'s to
  # a column of 1's and zeros
  df[,"Attrition_Text"] <- df[,"Attrition"]
  df[,"Attrition"] <- ifelse(df[,"Attrition"]=="Yes", 1, 0)
  df[,"Attrition_Numeric"] <- df[,"Attrition"]
  
  # Convert Gender column to binary 1's and 0's
  # 1 --> Female
  # 0 --> Male
  #df[,"Gender"] <- ifelse(df[, "Gender"]=="Female", 1, 0)
  
  
  # Encode the "Overtime" feature from "Yes"/"No" to 1 or 0
  #df[,"OverTime"] <- ifelse(df[, "OverTime"]=="Yes", 1, 0)
  
  # ID column is simply a numbering of the rows, however we also already have "Employee Number" 
  # which is a unique value for each observation, therefore these are redundant and
  # the ID column can be dropped.
  #
  # The three rate columns "DailyRate", "HourlyRate" and "MonthlyRate": There doesn't seem to be
  # any significant correlation of any type (pearson, spearman, kendall) between these features and
  # the targets we are interested in (Attrition and Salary). For that matter, they don't seem to be 
  # correlated with anything (appears they are just random noise). Since no subject matter experts on
  # this dataset were available for clarification, these features are being set aside until if/when
  # they can be better understood.
  
  # Dropping all columns in the "columns_to_drop" list, see justification for why each one
  # can be dropped above.
  df <- df[, !(names(df) %in% columns_to_drop)]
  
  # Add a column for yearly salary based on 12 months pay @ listed monthly rate.
  # Remove Monthly Rate as it now contains the same information as the new Salary column.
  df[,"Salary"] <- df[,"MonthlyIncome"] * 12
  # df <- df[, names(df) != "MonthlyIncome"]
  
  sd_salary <- sd(df[,"Salary"])
  mean_salary <- mean(df[,"Salary"])
  df[,"Salary_Scaled"] <- (df[,"Salary"] - mean_salary) / sd_salary
  
  
  if(add_binned_features){
    df <- create_binned_features(df) 
  }
  
  # All character columns (and the target) need to be factors or the mlr library throws errors
  if(char_to_factor){
    df <- convert_characters_to_factor(df)  
  } else if (char_to_numeric){
    df <- convert_characters_to_numeric(df)  
  }
  
  if(cat_ints_to_factors){
    df <- categorical_ints_to_factors(df)
  }
  
  return(df)
  
}

####################################  PLOTTING FUNCTIONS ####################################  

# Function to display a correlation matrix
# corr_type --> one of "pearson", "kendall" or "spearman"
plot_correlation <- function(df, col_names=NULL, remove_columns=NULL, round_digits=2,
                             method="square", outline_color="white", type="lower", 
                             theme="theme_minimal", show_diag=FALSE, hc_order=TRUE, 
                             annotate_values=TRUE, corr_type="pearson",
                             legend_title="Correlation Coefficient", title=NULL) {
  
  # Filter to only numeric columns (data must be numeric to include in correlation matrix).
  df <- select_if(df, is.numeric)
  
  df <- filter_columns(df, col_names=col_names, remove_columns=remove_columns)
  
  corr_matrix <- cor(df, method=corr_type)
  
  plot_title <- get_plot_labels(plot_kind="correlation", plot_type_info=corr_type)
  
  p <- ggcorrplot(corr_matrix, method=method, outline.col=outline_color, 
                  type=type, ggtheme=theme, title=plot_title, show.diag=show_diag, digits=round_digits,
                  hc.order=hc_order, lab=annotate_values, legend.title=legend_title)
  
  return(p)
  
}

# Function to plot the proportion of "Attrition" for each level of a categorical feature.
# This function also plots the confidence intervals for each proportion, which can be used
# to assess whether or not the given categorical feature may be an effective predictor of Attrition.
#
plot_categorical_proportions <- function(df, categorical_predictor, binary_target="Attrition", 
                                         hline_color="red", hline_linetype="dashed", round_digits=3, 
                                         txt_hjust=0, txt_vjust=-0.2, x_rotation=30){
  
  df[, "categorical_predictor"] <- as.factor(df[, categorical_predictor])
  df[, "binary_target"] <- df[, binary_target]
  
  df <- factor_to_numeric(df=df, col_name="binary_target")
  
  proportion_cis <- get_proportion_cis(df=df)
  
  
  overall_prop <- as.numeric(proportion_cis[1,"overall_proportion"])
  
  plot_labels <- get_plot_labels(plot_kind="categorical_proportions", plot_type_info=categorical_predictor)
  
  p <- ggplot(data=proportion_cis) +
    geom_point(mapping=aes(x=categorical_predictor, y=proportion_estimates)) + 
    geom_hline(yintercept=overall_prop, color=hline_color, linetype=hline_linetype) +
    geom_errorbar(aes(x=categorical_predictor, ymin=lower_ci, ymax=upper_ci), width=.1) +
    ggtitle(plot_labels$title) + 
    xlab(plot_labels$xlabel) + 
    ylab(plot_labels$ylabel) +
    theme(axis.text.x = element_text(angle=x_rotation, face="bold"))
  
  p <- p + geom_text(aes(x=-Inf, y=overall_prop, label=round(overall_prop, round_digits)), 
                     hjust=txt_hjust, vjust=txt_vjust, color=hline_color)
  
  
  return(p)
  
}

# Used to assess continuous features, this function will plot side by side histograms corresponding to the 
# Attrition=Yes and Attrition=No subpopulations of the given feature. 
#
# This function also will run a two sample test (t-test or rank sum) and display the test results on the plot
#
plot_histogram_by_level <- function(df, continuous_variable, binary_variable="Attrition_Text", fill_color="#bc5090", 
                                    outline_color="#003f5c", v_linetype="dashed", v_linecolor="red", v_linesize=1.4,
                                    v_line_ymax=50, xlabel_fontsize=10, title_fontsize=20, add_test=TRUE, test_type="t",
                                    num_bins=NULL, binwidth=NULL, equal_variance=FALSE, txt_yloc=Inf, txt_xloc=Inf, 
                                    round_digits=7, conf_level=0.95, exact=TRUE, annot_txt_size=7) {
  
  df[,"continuous_var"] <- df[,continuous_variable]
  df[,"categorical_var"] <- df[,binary_variable]
  
  means <- df %>% 
    group_by(categorical_var) %>%
    summarise(continuous_var = mean(continuous_var))
  
  p <- ggplot(data=df, mapping=aes(x=continuous_var)) +
    geom_histogram(binwidth=binwidth, bins=num_bins, fill=fill_color, color=outline_color) +
    facet_grid(~categorical_var) + 
    geom_segment(data=means, aes(x=continuous_var, xend=continuous_var, y=0, yend=v_line_ymax), 
                 linetype=v_linetype, color=v_linecolor, size=v_linesize)  +
    ggtitle(paste0("Distribution of ", str_to_title(continuous_variable), 
                   "\nfor individuals who stayed (Attrition=No) and left (Attrition= Yes)")) +
    ylab(paste0("Distribution of ", str_to_title(continuous_variable))) +
    xlab(str_to_title(continuous_variable)) + 
    theme(axis.text.x = element_text(size=xlabel_fontsize, face="bold"), 
          plot.title = element_text(size=title_fontsize))
  
  
  if(add_test){
    
    test_data <- run_two_sample_test(df=df, binary_variable=binary_variable, continuous_variable=continuous_variable,
                                     test_type=test_type, equal_variance=equal_variance, round_digits=round_digits,
                                     conf_level=conf_level, exact=exact)
    
    
    p <- p + 
      geom_text(aes(x=txt_xloc, y=txt_yloc), label=test_data[["text"]], vjust=1, hjust=1)
    
  }
  
  p <- p +
    geom_text(data=means, aes(x=continuous_var, y=v_line_ymax, 
                              label=round(continuous_var, round_digits)), 
              color=v_linecolor,
              size=annot_txt_size)
  
  return(p)
  
}

####################################  FEATURE IMPORTANCE FUNCTIONS ####################################  

### Calculate Relief Scores (Reference: Applied Predictive Modeling by Kuhn and Johnson)
### 
### Typically this function is run once and then a .csv file with the results is saved. This
### .csv can be used to quickly plot the relief scores via the plot_standardized_relief
### function without having to repeatedly recalculate scores.
calculate_relief_scores <- function(df, features, binary_target="Attrition", target_levels=c(0,1), 
                                    relief_iters=870, num_perms=2000, estimator="ReliefFequalK",
                                    save_path="./relief_scores.csv", save=TRUE) {
  
  
  df <- relief_variable_setup(df=df, 
                              features=features, 
                              binary_target=binary_target, 
                              target_levels=target_levels)
  
  target_index <- get_name_index(df=df, 
                                 name="binary_target")
  
  
  rs <- AppliedPredictiveModeling::permuteRelief(x=df[,-target_index], 
                                                 y=df$binary_target, 
                                                 estimator=estimator, 
                                                 nperm=num_perms, 
                                                 ReliefIterations=relief_iters)
  
  
  wide_df <- get_relief_score_df(rs)
  
  if(save){
    write.csv(wide_df, save_path)
  }
  
  return(wide_df)
  
}

# Creates a bar chart of the standardized relief scores
#
plot_standardized_relief <- function(relief_df, score_type="standardized", round_digits=2){
  
  
  relief_df <- prepare_relief_df(relief_df=relief_df, score_type=score_type)
  
  relief_df <- as.data.frame(relief_df[order(relief_df[,"score"], decreasing=TRUE),])
  
  relief_df$feature <- factor(relief_df$feature, 
                              levels=unique(relief_df$feature))

  scores_rounded <- round(relief_df[,"score"], round_digits)
    
  p <- ggplot(data=relief_df, mapping=aes(x=feature, y=score)) +
    geom_bar(stat='identity') + 
    stat_summary(fun=sum, geom="bar", fill="pink", color="navy") +
    stat_summary(fun=sum, geom="text", aes(label=scores_rounded), vjust=-0.08) + 
    theme(axis.text.x = element_text(angle=90, face="bold")) +
    ggtitle(paste0(str_to_title(score_type), " Relief Scores")) +
    ylab("Score") +
    xlab("Features")
  
  return(p)
}

# Creates a bar chart of the random forest feature importance scores using the mlr package
#
plot_rf_importances <- function(df, features, target="Attrition", round_digits=4,
                                importance_method="randomForestSRC_importance", txt_angle=45,
                                tick_label_rot=45){
  
  feature_df <- df[,features]
  feature_df[,"Attrition"] <- df[,"Attrition"]
  
  # Make the filter task
  filter_task <- makeClassifTask(data=feature_df,
                                 target=target)
  
  
  importance <- generateFilterValuesData(task=filter_task, 
                                         method="randomForestSRC_importance")
  
  importance_df <- importance$data
  
  importance_df <- as.data.frame(importance_df[order(importance_df[,"value"], 
                                                     decreasing=TRUE),])
  
  importance_df[,"feature_name"] <- importance_df[,"name"]
  
  importance_df$feature_name <- factor(importance_df$feature_name, 
                                       levels=unique(importance_df$feature_name))
  
  scores_rounded <- round(importance_df[,"value"], round_digits)
  
  p <- ggplot(data=importance_df, mapping=aes(x=feature_name, y=value)) +
    geom_bar(stat='identity') + 
    stat_summary(fun=sum, geom="bar", fill="pink", color="navy") +
    stat_summary(fun=sum, geom="text", aes(label=scores_rounded), vjust=-0.08, angle=txt_angle) + 
    theme(axis.text.x = element_text(angle=tick_label_rot, face="bold")) +
    ggtitle("Random Forest Feature Importance Scores") +
    ylab("Score") +
    xlab("Features")
  
  return(p)
}
################################  END FEATURE IMPORTANCE FUNCTIONS ################################



####################################  MODELING FUNCTIONS ####################################  

#
# FUNCTION TO REPRODUCE A SUPPORT VECTOR MACHINE MODEL THAT GETS 100% ACCURACY ON THE DATASET
# IT WAS TRAINED IN (LIKELY OVERFIT) FOUND VIA RANDOM SEARCH
#
train_support_vector_machine <- function(df, features="use_best", binary_target="Attrition"){
  
  if(features == "use_best"){
    features <- c("OverTime", "Salary", "StockOptionLevel", "JobLevelB", "YearsWithCurrentManager", 
                  "MaritalStatus", "JobRole", "YearsInCurrentRole", "Age", "YearsAtCompany", "JobSatisfaction",
                  "YearsSinceLastPromotion", "TotalWorkingYears", "JobInvolvementB", "NumCompaniesWorkedB",
                  "EnviornmentSatisfactionB")
  }
  
  
  
  # Filter to the features we want and add the target back
  svm_df <- df[,(names(df) %in% features)]
  svm_df[,"target"] <- df[,binary_target]
  
  # Create the classification task
  svm_task <- makeClassifTask(data=svm_df, target="target")
  
  # Instantiate the learner, with the previously found hyperparameters
  best_svm <- setHyperPars(makeLearner(cl="classif.svm"),
                           kernel="polynomial",
                           degree=3,
                           cost=1.48576,
                           gamma=5.97974)
  
  
  # Create the metrics we want to display
  metrics_list <- list(tnr, tpr, acc, tp, fp, tn, fn, ppv, f1, mmce)
  
  # Train the support vector machine model
  best_svm_model <- train(best_svm, svm_task)
  
  # PREDICT ON THE FULL DATASET
  predictions <- predict(best_svm_model, svm_task)
  metrics <- performance(predictions, measure=metrics_list)
  
  confusion_matrix <- calculateConfusionMatrix(predictions, 
                                               relative=TRUE)
  
  model_info <- list(preds=predictions, 
                     metrics=metrics, 
                     conf_matrix=confusion_matrix, 
                     model=best_svm_model, 
                     task=svm_task)
  
  return(model_info)
}

# Random Search best hyperparameters for support vector machine model, and optionally
# perform one initial round of feature selection
#
randomsearch_svm <- function(df, features, binary_target="Attrition", degree_lower=1,
                             degree_upper=3, kernel_types = c("polynomial", "radial", "sigmoid"),
                             cost_lower=0.1, cost_upper=10, gamma_lower=0.1, gamma_upper=10,
                             max_iterations=100, cv_folds=10, predict_sets="both", stratify=TRUE, 
                             verbose=TRUE, perform_feature_selection=TRUE){
  
  
  filtered_df <- df[, (names(df) %in% features)]
  filtered_df[,binary_target] <- df[,binary_target]
  
  # Create the metrics we want to display
  metrics_list <- list(tnr, tpr, acc, tp, fp, tn, fn, ppv, f1, mmce)
  
  if(perform_feature_selection){
    
    selection_control <- makeFeatSelControlSequential(method = "sfbs",
                                                      beta=0.001)
    
    fs_sampling <- makeResampleDesc(method="CV",
                                    iters=10,
                                    stratify=TRUE)
    
    
    selected_features <- selectFeatures(learner=makeLearner(cl="classif.svm",
                                                            par.vals=list("kernel"="polynomial",
                                                                           "degree"=2,
                                                                           "gamma"=10,
                                                                           "cost"=7.5)),
                                        task=makeClassifTask(data=filtered_df, target=binary_target),
                                        resampling=fs_sampling,
                                        control=selection_control, 
                                        measures = metrics_list, 
                                        show.info=TRUE)
    
    features <- selected_features$x
    
  }
  
  # Filter to the features we want and add the target back
  svm_df <- df[,(names(df) %in% features)]
  svm_df[,"target"] <- df[, binary_target]
  
  # Instantiate the learner
  svm_learner <- makeLearner(cl = "classif.svm")
  
  # Create the task (the data).
  svm_task <- makeClassifTask(data=svm_df, target="target")
  
  # Set up the hyperparameters to search over
  svm_params <- makeParamSet(makeDiscreteParam("kernel", values= kernel_types),
                             makeIntegerParam("degree", lower=degree_lower, upper=degree_upper),
                             makeNumericParam("cost", lower=cost_lower, upper=cost_upper),
                             makeNumericParam("gamma", lower=gamma_lower, upper=gamma_upper))
  
  # INSTANTIATE THE RANDOM SEARCH
  svm_control <- makeTuneControlRandom(maxit=max_iterations)
  

  
  svm_sampling <- makeResampleDesc(method = "CV", 
                                   iters=cv_folds, 
                                   predict=predict_sets, 
                                   stratify=stratify)
  
  # ALLOW MULTITHREADING FOR THE PARAMETER SEARCH
  parallelStartSocket()
  
  # PERFORM THE SEARCH
  tuned_svm <- tuneParams(learner = svm_learner,
                          task=svm_task,
                          resampling=svm_sampling,
                          par.set=svm_params,
                          control=svm_control, 
                          measures=metrics_list,
                          show.info=verbose)
  
  
  # Stop multithreading
  parallelStop()
  
  # USE THE HYPERPARAMS FOUND TO TRAIN ON THE ENTIRE DATASET AND PREDICT
  best_svm <- setHyperPars(learner=svm_learner,
                           par.vals=tuned_svm$x)
  
  
  # Retrain on entire dataset
  best_svm_model <- train(learner=best_svm, 
                          task=svm_task)
  
  # Predict on entire dataset
  predictions <- predict(object=best_svm_model, 
                         task=svm_task)
  
  
  metrics <- performance(predictions, 
                         measure=metrics_list)
  
  
  confusion_matrix <- calculateConfusionMatrix(predictions, 
                                               relative=TRUE)
  
  model_info <- list(preds=predictions, 
                     metrics=metrics, 
                     conf_matrix=confusion_matrix, 
                     model=best_svm_model, 
                     tuner=tuned_svm,
                     task=svm_task, 
                     metrics_list=metrics_list,
                     features=features,
                     selected_features=selected_features)
  
  return(model_info)
  
}


### Perform a GridSearch to find the best hyperparameters for an xgboost model.
#
gridsearch_xgboost <- function(df, features, binary_target="Attrition", num_trees_lower=50, 
                               num_trees_upper=200, colsample_lower=0.7, colsample_upper=1,
                               subsample_lower=0.5, subsample_upper=1, max_depth_lower=5, 
                               max_depth_upper=15, gamma_lower=0, gamma_upper=4, lr_lower=0.2,
                               lr_upper=1, short_practice=FALSE, resolution=5,
                               save_path="./best_xgb_model.rds"){
  
  # Filter to the features we want and add the target back
  xg_df <- df[,(names(df) %in% features)]
  xg_df[,"target"] <- df[, binary_target]
  
  # XGBOOST CAN'T USE FACTOR PREDICTORS, THE CODE BELOW CONVERTS ALL COLUMNS
  # EXCEPT THE TARGET COLUMN TO NUMERIC
  xgb_df <- mutate_at(xg_df, .vars = vars(-target), .funs = as.numeric)
  
  # Instantiate the learner
  xgb_learner <- makeLearner("classif.xgboost")
  
  # Create the task (the data).
  xgb_task <- makeClassifTask(data=xgb_df, target="target")
  
  # Set up the hyperparameters to search over 
  if(short_practice){
    
    xgb_params <- makeParamSet(makeNumericParam("eta", lower=0.1, upper=1))
    
  }else {
    
    xgb_params <- makeParamSet(makeNumericParam("eta", lower = lr_lower, upper = lr_upper), 
                               makeNumericParam("gamma", lower = gamma_lower, upper = gamma_upper),
                               makeIntegerParam("max_depth", lower = max_depth_lower, upper = max_depth_upper),
                               makeNumericParam("subsample", lower = subsample_lower, upper = subsample_upper),
                               makeNumericParam("colsample_bytree", lower = colsample_lower, upper = colsample_upper),
                               makeIntegerParam("nrounds", lower = num_trees_lower, upper = num_trees_upper))    
    
    
  }
  
  
  # Create the GridSearch
  xgb_control <- makeTuneControlGrid(resolution=resolution)
  
  # Create the metrics we want to display
  metrics_list <- list(tnr, tpr, acc, tp, fp, tn, fn, ppv, f1, mmce)
  
  xgb_sampling <- makeResampleDesc(method = "CV", 
                                   iters=5)
  
  # PERFORM THE SEARCH
  tuned_xgb <- tuneParams(learner = xgb_learner,
                          task=xgb_task,
                          resampling=xgb_sampling,
                          par.set=xgb_params,
                          control=xgb_control, 
                          measures=metrics_list)
  
  
  # USE THE HYPERPARAMS FOUND TO TRAIN ON THE ENTIRE DATASET AND PREDICT
  best_xgb <- setHyperPars(learner = xgb_learner,
                           par.vals=tuned_xgb$x)
  
  
  # Retrain on entire dataset
  best_xgb_model <- train(learner = best_xgb, 
                          task=xgb_task)
  
  # Predict on entire dataset
  predictions <- predict(object=best_xgb_model, 
                         task=xgb_task)
  
  
  metrics <- performance(predictions, 
                         measure=metrics_list)
  
  
  confusion_matrix <- calculateConfusionMatrix(predictions, 
                                               relative=TRUE)
  
  model_info <- list(preds=predictions, 
                     metrics=metrics, 
                     conf_matrix=confusion_matrix, 
                     model=best_xgb_model, 
                     tuner=tuned_xgb,
                     task=xgb_task, 
                     metrics_list=metrics_list)
  
  return(model_info)
  
}

### Perform a RandomSearch to find the best hyperparameters for an xgboost model.
#
# NOTE: This is much more computationally reasonable than the Gridsearch, however xgboost
#       models are still very computationally expensive to train, specially when there are
#       a large number of boosting rounds (trees).
#
randomsearch_xgboost <- function(df, features, binary_target="Attrition", num_trees_lower=50, 
                                 num_trees_upper=200, colsample_lower=0.7, colsample_upper=1,
                                 subsample_lower=0.5, subsample_upper=1, max_depth_lower=5, 
                                 max_depth_upper=15, gamma_lower=0, gamma_upper=4, lr_lower=0.2,
                                 lr_upper=1, short_practice=FALSE, resolution=5, min_child_weight_lower=1,
                                 min_child_weight_upper=10, max_iterations=1000, cv_folds=10,
                                 predict_sets="both", stratify=TRUE){
  
  # Filter to the features we want and add the target back
  xg_df <- df[,(names(df) %in% features)]
  xg_df[,"target"] <- df[, binary_target]
  
  # XGBOOST CAN'T USE FACTOR PREDICTORS, THE CODE BELOW CONVERTS ALL COLUMNS
  # EXCEPT THE TARGET COLUMN TO NUMERIC
  xgb_df <- mutate_at(xg_df, .vars = vars(-target), .funs = as.numeric)
  
  # Instantiate the learner
  xgb_learner <- makeLearner("classif.xgboost")
  
  # Create the task (the data).
  xgb_task <- makeClassifTask(data=xgb_df, 
                              target="target")

    
  xgb_params <- makeParamSet(makeNumericParam("eta", lower = lr_lower, upper = lr_upper), 
                             makeNumericParam("gamma", lower = gamma_lower, upper = gamma_upper),
                             makeIntegerParam("max_depth", lower = max_depth_lower, upper = max_depth_upper),
                             makeNumericParam("subsample", lower = subsample_lower, upper = subsample_upper),
                             makeNumericParam("colsample_bytree", lower = colsample_lower, upper = colsample_upper),
                             makeIntegerParam("nrounds", lower = num_trees_lower, upper = num_trees_upper),
                             makeNumericParam("min_child_weight", lower = min_child_weight_lower, 
                                              upper = min_child_weight_upper))    
    
  # Create the GridSearch
  xgb_control <- makeTuneControlRandom(maxit=max_iterations)
  
  # Create the metrics we want to display
  metrics_list <- list(tnr, tpr, acc, tp, fp, tn, fn, ppv, f1, mmce)
  
  xgb_sampling <- makeResampleDesc(method = "CV", 
                                   iters=cv_folds,
                                   predict=predict_sets,
                                   stratify=stratify)
  
  # PERFORM THE SEARCH
  tuned_xgb <- tuneParams(learner = xgb_learner,
                          task=xgb_task,
                          resampling=xgb_sampling,
                          par.set=xgb_params,
                          control=xgb_control, 
                          measures=metrics_list)
  
  
  # USE THE HYPERPARAMS FOUND TO TRAIN ON THE ENTIRE DATASET AND PREDICT
  best_xgb <- setHyperPars(learner = xgb_learner,
                           par.vals=tuned_xgb$x)
  
  
  # Retrain on entire dataset
  best_xgb_model <- train(learner = best_xgb, 
                          task=xgb_task)
  
  # Predict on entire dataset
  predictions <- predict(object=best_xgb_model, 
                         task=xgb_task)
  
  
  metrics <- performance(predictions, 
                         measure=metrics_list)
  
  
  confusion_matrix <- calculateConfusionMatrix(predictions, 
                                               relative=TRUE)
  
  model_info <- list(preds=predictions, 
                     metrics=metrics, 
                     conf_matrix=confusion_matrix, 
                     model=best_xgb_model, 
                     tuner=tuned_xgb,
                     task=xgb_task, 
                     metrics_list=metrics_list)
  
  return(model_info)
  
}


###################################################################################
############################### NAIVE BAYES SECTION ############################### 
###################################################################################

# This function is support by the functions train_naive_bayes_basic, combine_nb_train_test_dfs, 
# and get_predictor_combos_manual. Together this group of functions will perform the following:
#
# 1) Take in a list of features, and create all possible subset combinations of features
#    ranging from length 1 to all.
# 2) Fit a naive bayes model on each subset of features.
# 3) Cross validate each model and record the training and testing performance, then return
#    a dataframe summarizing the performance of each model.
# 
cv_nb_feat_sets_manual <- function(df, features, binary_target="Attrition", cv_method="CV",
                                   predict_sets="both", stratify=TRUE, cv_folds=10, verbose=FALSE,
                                   order_by="tnr.test.mean", order_decreasing=TRUE, save_path=NULL){
  
  # Get all the combinations of predictors we want to use for training
  # and cross validating naive bayes
  feature_sets <- get_predictor_combos_manual(features=features)
  
  
  for(set_index in 1:length(feature_sets)){
    
    # Grab the list of predictors
    feats <- c(feature_sets[[set_index]])
    
    
    # train the model using the predictors in "feats" and cross validate to get performance
    nb_info <- train_naive_bayes_basic(df=df, 
                                       features=feats, 
                                       binary_target=binary_target, 
                                       cv_method=cv_method,
                                       predict_sets=predict_sets,
                                       stratify=stratify,
                                       iterations=cv_folds,
                                       verbose=verbose)
    
    
    # Create a nice predictor string so we remember which predictors resulted
    # in this score
    pred_string <- stringr::str_c(feats, collapse=" ")
    
    # Update a dataframe that tracks the cross validation performance for all feature combinations
    if(set_index == 1){
      test_df <- data.frame(as.list(nb_info$cv_results$aggr))
      test_df[,"predictors"] <- pred_string
      test_df[,"num_predictors"] <- length(feats)
      
      all_data_df <- combine_nb_train_test_dfs(train_df=nb_info$cv_results$measures.train,
                                               test_df=test_df)
      

    }else{
      test_df <- data.frame(as.list(nb_info$cv_results$aggr))
      test_df[,"predictors"] <- pred_string
      test_df[,"num_predictors"] <- length(feats)
      
      train_test_df <- combine_nb_train_test_dfs(train_df=nb_info$cv_results$measures.train,
                                                 test_df=test_df)
      
      all_data_df <- rbind(all_data_df, train_test_df)
    }

  }
  
  all_data_df <- all_data_df[order(all_data_df[,order_by], decreasing=order_decreasing),]
  
  if(!is.null(save_path)){
    write.csv(all_data_df, file=save_path)
  }
  
  return(all_data_df)
}

train_naive_bayes_basic <- function(df, features, binary_target="Attrition", cv_method="CV",
                                    predict_sets="both", stratify=TRUE, iterations=10, verbose=FALSE){

  
  # Filter to the features we want and add the target back
  nb_df <- df[,(names(df) %in% features), drop=FALSE]
  nb_df[,"target"] <- df[, binary_target]
  
  # Create the metrics we want to display
  metrics_list <- list(tnr, tpr, acc, tp, fp, tn, fn, ppv, f1, mmce)
  
  # Create the classification task
  nb_task <- makeClassifTask(data=nb_df, 
                             target="target")
  
  # Define the sampling strategy
  nb_sampling <- makeResampleDesc(method = cv_method, 
                                  iters=iterations, 
                                  stratify = stratify,
                                  predict=predict_sets)
  
  # Perform the cross validation
  cv_results <- resample(learner = "classif.naiveBayes", 
                         task=nb_task, 
                         resampling=nb_sampling,
                         measures=metrics_list, 
                         show.info=verbose)
  
  
  info <- list(cv_results=cv_results,
               resample=nb_sampling)
  
  return(info)
  
  }

# This function was an attempt at using the mlr libraries "wrapper" style feature
# selection capabilities around a naive bayes model.
train_naive_bayes <- function(df, features, binary_target="Attrition", cv_method="CV",
                              predict_sets="both", stratify=TRUE,  iterations=10){
  
  # Filter to the features we want and add the target back
  nb_df <- df[,(names(df) %in% features)]
  nb_df[,"target"] <- df[, binary_target]
  
  # Create the metrics we want to display
  metrics_list <- list(tnr, tpr, acc, tp, fp, tn, fn, ppv, f1, mmce)
  
  # Create the classification task
  nb_task <- makeClassifTask(data=nb_df, 
                             target="target")
  
  
  # Define the sampling strategy
  feat_select_sampling <- makeResampleDesc(method = "CV", 
                                           iters=3)
  
  nb_ft_learner <- makeFeatSelWrapper(learner="classif.naiveBayes",
                                      resampling=feat_select_sampling,
                                      measures=list(tnr),
                                      control=makeFeatSelControlSequential(method="sfbs"),
                                      show.info=TRUE)
  
  
  # Define the sampling strategy
  nb_rs_sampling <- makeResampleDesc(method = cv_method, 
                                     iters=iterations, 
                                     stratify = stratify,
                                     predict=predict_sets)
  
  # Perform the cross validation
  cv_results <- resample(learner = nb_ft_learner, 
                         task=nb_task, 
                         resampling=nb_rs_sampling,
                         measures=metrics_list,
                         extract=getFeatSelResult)
  
  
  info <- list(cv_results=cv_results,
               feat_sel_learner=nb_ft_learner,
               resample=nb_rs_sampling)
  
  return(info)
  
  ###################
  
  # Refit on entire dataset
  nb_model <- train(learner=nb_ft_learner, task = nb_task)
  
  # Make Predictions
  predictions <- predict(object=nb_model, 
                         task=nb_task)
  
  # Calculate performance metrics
  metrics <- performance(predictions, 
                         measure=metrics_list)
  
  # Create confusion matrix
  confusion_matrix <- calculateConfusionMatrix(predictions, 
                                               relative=TRUE)
  
  
  # Create list of all the modeling data and metrics to return
  model_info <- list(preds=predictions, 
                     metrics=metrics, 
                     conf_matrix=confusion_matrix, 
                     model=nb_model, 
                     cv_results=cv_results,
                     task=nb_task, 
                     metrics_list=metrics_list)
  
  return(model_info)
  
}


###################################################################################
############################# END NAIVE BAYES SECTION #############################
###################################################################################


#
# Gridsearch Knn Model
#
gridsearch_knn <- function(df, features, binary_target="Attrition", gs_method="CV",
                           stratify=TRUE, cv_folds=5, start_k=1, end_k=201, step_k=2,
                           predict_sets="both", multithread_processes=8){
  
  # Filter to the features we want and add the target back
  knn_df <- df[,(names(df) %in% features), drop=FALSE]
  knn_df[,"target"] <- df[, binary_target]
  knn_df[,"target"] <- as.factor(knn_df[,"target"])
  
  # KNN doesn't support factors as inputs, so we need to encode
  Knn_df <- createDummyFeatures(obj=knn_df[, !(names(knn_df) %in% c("target")), drop=FALSE])
  
  # Create the classification task
  knn_task <- makeClassifTask(data=knn_df, target="target")
  
  # Instantiate the learner
  knn_learner <- makeLearner(cl = "classif.knn")
  
  # Create the gridsearch sampling strategy
  knn_sampling <- makeResampleDesc(method = gs_method, 
                                   iters=cv_folds, 
                                   stratify = stratify,
                                   predict=predict_sets)
  
  # Instantiate the grid search control
  knn_control <- makeTuneControlGrid()
  
  # Create the metrics we want to display
  metrics_list <- list(tnr, tpr, acc, tp, fp, tn, fn, ppv, f1, mmce)
  
  # Specify the parameters to tune over
  knn_params <- makeParamSet(makeDiscreteParam("k", values=seq(from=start_k, to=end_k, by=step_k)))
  
  # ALLOW MULTITHREADING FOR THE PARAMETER SEARCH
  parallelStartSocket(cpus=multithread_processes)
  
  # Perform the grid search
  tuned_knn <- tuneParams(learner=knn_learner,
                          task=knn_task,
                          resampling=knn_sampling,
                          par.set=knn_params,
                          control=knn_control,
                          measures=metrics_list)
  
  # Stop multithreading
  parallelStop()
  
  # USE THE HYPERPARAMS FOUND TO TRAIN ON THE ENTIRE DATASET AND PREDICT
  best_knn <- setHyperPars(learner = knn_learner,
                           par.vals=tuned_knn$x)
  
  # Refit on entire data set
  best_knn_model <- train(learner=best_knn, task = knn_task)
  
  # Make Predictions
  predictions <- predict(object=best_knn_model, 
                         task=knn_task)
  
  # Calculate performance metrics
  metrics <- performance(predictions, 
                         measure=metrics_list)
  
  # Create confusion matrix
  confusion_matrix <- calculateConfusionMatrix(predictions, 
                                               relative=TRUE)
  
  
  # Create list of all the modeling data and metrics to return
  model_info <- list(preds=predictions, 
                     metrics=metrics, 
                     conf_matrix=confusion_matrix, 
                     model=best_knn_model, 
                     tuner=tuned_knn,
                     task=knn_task, 
                     metrics_list=metrics_list)
  
  
  return(model_info)
  
}


cv_knn_feat_sets_manual <- function(df, features, binary_target="Attrition", cv_method="CV",
                                    predict_sets="both", stratify=TRUE, cv_folds=10, start_k=1,
                                    end_k=401, step_k=2, order_by="tnr.test.mean", order_decreasing=TRUE, 
                                    save_path=NULL, multithread_processes=8, update_status_every=1000){
  
  
  # Get all the combinations of predictors we want to use for training
  # and cross validating naive bayes
  feature_sets <- get_predictor_combos_manual(features=features)
  
  for(set_index in 1:length(feature_sets)){
    
    if(set_index %% update_status_every == 0){
      print("")
      print("===============================================================")
      print(paste0("Completed ", set_index, " of ", length(feature_sets), " feature set evaluations"))
      print("===============================================================")
      print("")
    }
    
    
    # Grab the list of predictors
    feats <- c(feature_sets[[set_index]])
    
    # Create a nice predictor string so we remember which predictors resulted
    # in this score
    pred_string <- stringr::str_c(feats, collapse=" ")
    
    knn_info <- gridsearch_knn(df=df, 
                               features=feats, 
                               gs_method=cv_method,
                               predict_sets=predict_sets,
                               stratify=stratify,
                               cv_folds=cv_folds,
                               start_k=start_k,
                               end_k=end_k,
                               step_k=step_k,
                               multithread_processes=multithread_processes)
    
    
    if(set_index == 1){
      final_result_df <- create_knn_gs_result_df(knn_info)
      final_result_df[,"predictors"] <- pred_string
      final_result_df[,"num_predictors"] <- length(feats)
    }else{
      intermediate_result_df <- create_knn_gs_result_df(knn_info)
      intermediate_result_df[,"predictors"] <- pred_string
      intermediate_result_df[,"num_predictors"] <- length(feats)
      final_result_df <- rbind(final_result_df, intermediate_result_df)
    }
  }
  
  final_df <- final_result_df[order(final_result_df[,order_by], 
                                    decreasing=order_decreasing),]
  
  if(!is.null(save_path)){
    write.csv(final_df, file=save_path)
  }
  
  return(final_df)
}

################### FEATURE SELECTION FOR LINEAR REGRESSION MODELS SECTION ################### 

cv_feature_sets_multiple <- function(df, feature_sets, response_var="Salary", feature_set_type="best_subset",
                                     best_subset_metric="msep", cv_folds=10, cv_type="cv", cv_repeats=NULL, cv_sortby="RMSE",
                                     save_path=NULL){
  
  
  for(index in 1:length(feature_sets)){
    
    feature_set <- feature_sets[[index]]
    
    cv_data <- cv_feature_sets(df=df,
                               features=features,
                               response_var=response_var,
                               feature_set_type=feature_set_type,
                               best_subset_metric=best_subset_metric,
                               cv_folds=cv_folds,
                               cv_type=cv_type,
                               cv_repeats=cv_repeats,
                               cv_sortby=cv_sortby)
   
    # Creating a dataframe of the cross validation results for each model
    if(index == 1){
      cv_df <- cv_data$cv_df
    }
    else{
      cv_df <- rbind(cv_df, cv_data$cv_df)
    }
  }
  
  if(!is.null(save_path)){
    write.csv(cv_df, file=save_path)
  }
  
  return(cv_df)
    
}

cv_feature_sets <- function(df, features, response_var, feature_set_type="best_subset", best_subset_metric="msep", 
                            cv_folds=10, cv_type="cv", cv_repeats=NULL, cv_sortby="RMSE", save_path=NULL){
  
  
  # GET THE FEATURE SETS THAT NEED TO BE CROSS VALIDATED
  feature_sets <- get_best_feature_sets(df=df,
                                        response_var=response_var,
                                        features=features,
                                        feature_set_type=feature_set_type,
                                        metric=metric)
  
  filtered_df <<- feature_sets$df
  feature_info <<- feature_sets$result

  # LIST OF FEATURE SETS
  predictors <- feature_info$predictors
  
  # PERFORM CROSS VALIDATION
  cv_df <- cross_validate_subsets(filtered_df=filtered_df,
                                  response_var=response_var,
                                  predictors=predictors,
                                  cv_type=cv_type,
                                  cv_folds=cv_folds,
                                  cv_repeats=cv_repeats,
                                  cv_sortby=cv_sortby)
  
  
  if(!is.null(save_path)){
    write.csv(cv_df, save_path)
  }
  
  # BUILD RETURN LIST
  info <- list(cv_df=cv_df,
               best_subsets=feature_sets,
               best_subset_features=predictors)
  
  return(info)
  
}

cross_validate_subsets <- function(filtered_df, response_var, predictors, cv_type, cv_folds, cv_repeats,
                                   cv_sortby, model_method="lm"){
  
  for(model_index in 1:length(predictors)){
    
    preds <- predictors[[model_index]]
    
    train_control <- get_caret_cv_method(method=cv_type,
                                         cv_folds=cv_folds,
                                         repeats=cv_repeats)
    
    model_formula <- as.formula(paste0(response_var, "~", stringr::str_replace_all(string=preds, 
                                                                                   pattern=" ", 
                                                                                   replacement="+")))
    
    cv_result <- caret::train(model_formula,
                              data=filtered_df,
                              method=model_method,
                              trControl=train_control)  
    
    # Creating a dataframe of the cross validation results for each model
    if(model_index == 1){
      cv_df <- as.data.frame(cv_result$results)
      cv_df[,"predictors"] <- preds
      cv_df[,"mean_training_rmse"] <- get_mean_training_rmse(train_result=cv_result,
                                                             model_formula=model_formula,
                                                             model_df=filtered_df)
      
    }
    else{
      result_df <- as.data.frame(cv_result$results)
      result_df[,"predictors"] <- preds
      result_df[,"mean_training_rmse"] <- get_mean_training_rmse(train_result=cv_result,
                                                                 model_formula=model_formula,
                                                                 model_df=filtered_df)
      cv_df <- rbind(cv_df, result_df)
    }
  }
  
  cv_df <- cv_df[order(cv_df[,cv_sortby], decreasing=FALSE),]
  

  return(cv_df)
  
}

################### END LINEAR REGRESSION SECTION ################### 