# get_ranked_data: process wtdata to get the pvalues of each variable and then 
# selects the best ones considering correlation between the final variables.
# Warning: The returned subwtdata losses the date_time variable.
get_ranked_data <- function(wtdata, method = "theoretical", est = "mean", cor_threshold = 0.95, target=target,disp = F, file_jpeg = "", nsim = 1000) {
  iam="get_ranked_data"
  cat("\nStarting pvalue ranking selection... \n")
  #Dependencias
  if(!exists("calcule_pvalues")){
    if(!file.exists('functions_common/calcule_pvalues.R')) return(list(error=TRUE,data=NULL,msg=paste0(iam," Missing dependency function: functions_common/calcule_pvalues.R")))
    source('functions_common/calcule_pvalues.R')
  }

  p <- calcule_pvalues(wtdata, method = method, est = est,target=target, disp = disp, file_jpeg = file_jpeg, nsim = nsim)
  if(p$error) return(list(error=TRUE,data=NULL,msg=paste0(iam,": on call calcule_pvalues\n",p$msg)))
  pvalues<-p$data
  
  num_predictors = sum(pvalues < 0.05)
  # Validation of numer of pre-selected variables
  if(num_predictors == 0) return(list(error=TRUE,data=NULL,msg=paste0(iam,":Number of pre-selected variables equal to 0")))
  
  # sort out vector
  id_sort=order(pvalues)
  ## Select top ten best pvalues with lowest correlation among them
  id_selected <- numeric(length = num_predictors)
  
  id_selected[1] <- id_sort[1] # Top 1
  n_selected <- 1

  cat( n_selected, ": ", names(wtdata)[id_selected[n_selected]], 
       "\t\tpvalue: ", pvalues[id_selected[n_selected]], "\n", sep = "")
  for ( i in 2:num_predictors ) {
    tmp_y <- wtdata[,id_sort[i]]
    tmp_selected <- wtdata[,id_selected[id_selected != 0]]
    tmp_cor <- abs( cor(tmp_selected, tmp_y, use = "pairwise.complete.obs") )
    if( any(tmp_cor > cor_threshold, na.rm = T) == FALSE ) {
      n_selected <- n_selected +1
      id_selected[n_selected] <- id_sort[i]
      cat( n_selected, ": ", names(wtdata)[id_selected[n_selected]], 
           "\t\tpvalue: ", pvalues[id_selected[n_selected]], "\n", sep = "")
    }
  }
  cat("\n")
  id_selected <- id_selected[id_selected > 0]
  #df_pvalues <- data.frame(name = names(wtdata)[id_selected], pvalue = pvalues[id_selected])
  df_pvalues <- data.frame(name = names(wtdata)[which(names(wtdata)!=target)], pvalue = pvalues[which(names(wtdata)!=target)])

  subwtdata  <- wtdata[,c(id_selected,which(names(wtdata)==target))]
  result <- list(ranked_data = subwtdata, pvalues = df_pvalues, id_selected = id_selected)
  return(list(error=FALSE,data=result,msg="OK"))
}
