calcule_master_analysis <- function (results, mymetric = 'pv', horizon_correction = 0, forced_model = NULL) {
    source('functions_cast/get_dist.R')
    source('functions_cast/calcule_cost.R')
    source('functions_cast/select_fit.R')
    n_wt_fault <- length(results)
    n_horizons <- length(results[[1]])
    n_models <- length(results[[1]][[1]]$fit_list)
    costs <- data.frame(true_neg = 0, false_pos = 1, false_neg = 2, true_pos = 0)
    
    metric_analysis <- data.frame(WT_fault = character(n_horizons*n_models*n_wt_fault),
                                  horizon = integer(n_horizons*n_models*n_wt_fault),
                                  fitmodel = character(n_horizons*n_models*n_wt_fault),
                                  
                                  # Metrics from training process
                                  ROC = numeric(n_horizons*n_models*n_wt_fault),
                                  Sens = numeric(n_horizons*n_models*n_wt_fault),
                                  Spec = numeric(n_horizons*n_models*n_wt_fault),
                                  dist = integer(n_horizons*n_models*n_wt_fault),
                                  
                                  # Metrics from testing data
                                  Acc = numeric(n_horizons*n_models*n_wt_fault),
                                  Nir =  numeric(n_horizons*n_models*n_wt_fault),
                                  pv =  numeric(n_horizons*n_models*n_wt_fault),
                                  Kappa =  numeric(n_horizons*n_models*n_wt_fault),
                                  Sens_test =  numeric(n_horizons*n_models*n_wt_fault),
                                  Spec_test =  numeric(n_horizons*n_models*n_wt_fault),
                                  dist_test = integer(n_horizons*n_models*n_wt_fault),
                                  true_positive = integer(n_horizons*n_models*n_wt_fault),
                                  true_negative = integer(n_horizons*n_models*n_wt_fault),
                                  false_positive = integer(n_horizons*n_models*n_wt_fault),
                                  false_negative = integer(n_horizons*n_models*n_wt_fault),
                                  positives = integer(n_horizons*n_models*n_wt_fault),
                                  negatives = integer(n_horizons*n_models*n_wt_fault),
                                  
                                  # Costs
                                  cost_smart = numeric(n_horizons*n_models*n_wt_fault),
                                  cost_random = numeric(n_horizons*n_models*n_wt_fault),
                                  cost_reactive = numeric(n_horizons*n_models*n_wt_fault),
                                  
                                  stringsAsFactors = FALSE)
    
    master_analysis <- metric_analysis[1:(n_horizons*n_wt_fault),]
    all_metric_analysis <- metric_analysis
    metric_analysis <- metric_analysis[1:(n_horizons*n_models),]
    master_best_fit <- vector("list", n_horizons*n_wt_fault)
    best_fit <- vector("list", n_horizons)
    best_ma <- metric_analysis[1:n_horizons,]
    
    make_horizon_correction <- FALSE
    if( is.null(names(results[[1]])) ){
        horizon_list <- 1:n_horizons
        make_horizon_correction <- TRUE
    } else {
        horizon_list <- as.numeric(names(results[[1]]))
    }
    # Loop to analyze all WTs and faults
    for ( k in 1:n_wt_fault) {
        result_h <- results[[k]]
        # Loop to analyze specific WT and fault for all horizons
        for( i in 1:n_horizons) {
            # Loop to analyze specific WT and fault and horizon for all types of models
            for ( j in 1:n_models) {
                if(j<=length(result_h[[i]]$fit_list)){ # Fix for empty model on horitzon.
                  # Data for Fit j
                  metric_analysis$horizon[i + n_horizons*(j-1)] <- horizon_list[i]
                  metric_analysis$WT_fault[i + n_horizons*(j-1)] <- names(results[k])
                  
                  #TODO parche
                  if(is.null(result_h[[i]]$fit_list[[j]])) next
                      
                  fit <- result_h[[i]]$fit_list[[j]]
                  
                  max_id <- which.max(fit$results$ROC)
                  
                  metric_analysis$fitmodel[i + n_horizons*(j-1)] <- fit$method
                  metric_analysis$ROC[i + n_horizons*(j-1)] <- fit$results$ROC[max_id]
                  metric_analysis$Sens[i + n_horizons*(j-1)] <- fit$results$Sens[max_id]
                  metric_analysis$Spec[i + n_horizons*(j-1)] <- fit$results$Spec[max_id]
                  metric_analysis$dist[i + n_horizons*(j-1)] <- get_dist(fit$results$Sens[max_id], fit$results$Spec[max_id])
                  
                  cm <- result_h[[i]]$cm_list[[j]]
                  
                  metric_analysis$Acc[i + n_horizons*(j-1)] <- cm$overall["Accuracy"]
                  metric_analysis$Nir[i + n_horizons*(j-1)] <- cm$overall["AccuracyNull"]
                  metric_analysis$pv[i + n_horizons*(j-1)] <- cm$overall["AccuracyPValue"]
                  metric_analysis$Kappa[i + n_horizons*(j-1)] <- cm$overall["Kappa"]
                  metric_analysis$Sens_test[i + n_horizons*(j-1)] <- cm$byClass["Sensitivity"]
                  metric_analysis$Spec_test[i + n_horizons*(j-1)] <- cm$byClass["Specificity"]
                  metric_analysis$dist_test[i + n_horizons*(j-1)] <- get_dist(cm$byClass["Sensitivity"], cm$byClass["Specificity"])
                  metric_analysis$true_positive[i + n_horizons*(j-1)] <- cm$table[2,2]
                  metric_analysis$true_negative[i + n_horizons*(j-1)] <- cm$table[1,1]
                  metric_analysis$false_positive[i + n_horizons*(j-1)] <- cm$table[2,1]
                  metric_analysis$false_negative[i + n_horizons*(j-1)] <- cm$table[1,2]
                  metric_analysis$positives[i + n_horizons*(j-1)] <- sum(cm$table[,2])
                  metric_analysis$negatives[i + n_horizons*(j-1)] <- sum(cm$table[,1])  
                  
                  # Cost estimation
                  metric_analysis$cost_smart[i + n_horizons*(j-1)] <- calcule_cost(costs, cm)
                  metric_analysis$cost_random[i + n_horizons*(j-1)] <- calcule_cost(costs, cm, "random")
                  metric_analysis$cost_reactive[i + n_horizons*(j-1)] <- calcule_cost(costs, cm, "reactive")
                  # TODO: cost estimation with variable cost relation (cost_function vs. cost_relation)
              }
            }
            
            # Select fit
            sma <- subset(metric_analysis, metric_analysis$horizon==horizon_list[i]);
            best_tmp <- select_fit(metric = mymetric, sma, result_h[[i]]$fit_list, forced_model = forced_model)
            best_fit[[i]] <- best_tmp[1:3]
            best_ma[i,] <- best_tmp[[4]]
        }
        id1 <- 1 + (k-1)*n_horizons
        id2 <- k*n_horizons
        master_analysis[id1:id2,] <- best_ma 
        master_best_fit[[k]]$best_fit <- best_fit
        master_best_fit[[k]]$wt_fault <- names(results[k])
        
        id1 <- 1 + (k-1)*n_horizons*n_models
        id2 <- k*n_horizons*n_models
        all_metric_analysis[id1:id2,] <- metric_analysis
    }
    if(make_horizon_correction)
        master_analysis$horizon <- master_analysis$horizon + horizon_correction
    return(master_analysis)
}