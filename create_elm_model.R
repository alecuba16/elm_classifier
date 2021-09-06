#custom predict
#needs the same model
predict_elm<-function(model=NULL,data=NULL,normalize=F){
  iam=match.call()[[1]]
  #Basic dependency
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg="Missing dependency function: functions_common/dependencyLoader.R"));
    source('functions_common/dependencyLoader.R')
  }
  sources<-c("pracma","functions_common/close_protocol.R")
  dep<-dependencyLoader(sources)
  if(dep$error)  return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(is.null(model)) return(list(error=T,data=NULL,msg="model cannot be null"))
  if(is.null(data)) return(list(error=T,data=NULL,msg="data cannot be null"))
  X_sigma<-model$X_sigma
  X_mu<-model$X_mu
  y_sigma<-model$y_sigma
  y_mu<-model$y_mu
  activation<-model$activation
  pseudo_inverse<-model$pseudo_inverse
  column_names_X<-model$column_names_X
  target_column_name<-model$target_column_name
  classifier<-model$classifier
  
  calcule_rmse<-F
  if(target_column_name %in% colnames(data)){#Only when it is on evaluation and target column is available
    calcule_rmse<-T
    output_y<-data[,target_column_name]
  }else{
    rmse<-NA
    norm_rmse<-NA
  }
  
  #Prepare output variables
  output_yhat<-rep(NA,nrow(data))
  
  #Prepare model variables
  complete_cases<-complete.cases(data) #Only use complete cases for model
  if(calcule_rmse) y<-data[complete_cases,target_column_name]
  X<-data[complete_cases,!(names(data) %in% target_column_name)]
  u<-rep(1,nrow(X)) 
  
  #Recover weights and model config
  k<-model$K
  W<-model$W
  b<-model$b
  B<-model$B
  # Normalize Z score (X-mean(x)/std(x))
  #Input
  if(normalize) X<-sapply(1:ncol(X), function(c) {((X[,c]-X_mu[c])/ifelse(X_sigma[c]==0,NA,X_sigma[c]))})
  if(!is.matrix(X)) X<-as.matrix(X)
  
  H<-(X%*%W)+kronecker(u,t(b)) # Matriu de pesos * entrades + bias
  if(pseudo_inverse){
    pI<-(pracma::pinv(t(H)%*%H) %*% t(H))
  }else{
    pI<-(pracma::inv(t(H)%*%H) %*% t(H)) #inv no funciona bien en todos los casos
  }
  
  if(activation) H <- 1.0 %/% ( 1.0 + exp(-H))
  yhat<-H%*%B
  #not_nan<-(!is.nan(y)&!is.nan(yhat))
  #yok<-y[not_nan]
  #yhat<-yhat[not_nan]
  #De-normalize
  if(!classifier) yhat<-(yhat*y_sigma)+y_mu
  
  if(calcule_rmse){
    #Rmse
    #rmse<-sqrt(sum((yhat-yok)^2,na.rm = T)/length(yok))
    rmse<-sqrt(sum((yhat-y[complete_cases])^2,na.rm = T)/length(y[complete_cases]))
    #Calculate normalized rmse
    min_y<-min(y,na.rm = T)
    max_y<-max(y,na.rm = T)
    norm_rmse<-rmse/(max_y-min_y)
  }
  output_yhat[complete_cases]<-yhat
  
  return(list(error=F,data=list(rmse=rmse,norm_rmse=norm_rmse,yhat=output_yhat),msg='ok'))
}

create_elm_model<-function(data=NULL,K=10,target_column_name=NULL,date_time_name='date_time',activation=F,pseudo_inverse=T,classifier=F){
  iam=match.call()[[1]]
  #Basic dependency
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg="Missing dependency function: functions_common/dependencyLoader.R"));
    source('functions_common/dependencyLoader.R')
  }
  sources<-c("pracma","functions_common/close_protocol.R")
  dep<-dependencyLoader(sources)
  if(dep$error)  return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  #Check sizes and data
  if(is.null(data) || is.null(target_column_name) || length(data)<=1  || nchar(target_column_name)<=1) 
    return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,": The data/target_column_name variable are null or empty\n")))
  
  #check target
  if(!grepl(target_column_name, paste(names(data),collapse='')))
    return(list(error=TRUE,data=NULL,msg=paste0("Target variable :",target_column_name," not in data:",names(data)))) 
  
  #Prepare output variables
  output_y<-data[,target_column_name]
  output_yhat<-rep(NA,length(output_y))
  
  #Prepare model variables
  complete_cases<-complete.cases(data) #Only use complete cases for model
  y<-data[complete_cases,target_column_name]
  X<-data[complete_cases,!(names(data) %in% target_column_name)]    
  
  #y numeric
  y<-as.numeric(y)
  
  u<-rep(1,nrow(X)) 
  #Scale
  # Normalize Z score (X-mean(x)/std(x))
  X_sigma<-apply(X,2,sd,na.rm=T)
  zero_sdv_columns<-(X_sigma==0)
  X_sigma<-X_sigma[!zero_sdv_columns]
  X<-X[,!zero_sdv_columns]
  column_names_X<-colnames(X)
  X_mu<-apply(X,2,mean,na.rm=T)
  X<-sapply(1:ncol(X),function(c){(X[,c]-X_mu[c])/X_sigma[c]})
  
  min_y<-min(y,na.rm = T)
  max_y<-max(y,na.rm = T)
  if(!classifier){
    #response
    y_sigma<-sd(y,na.rm = T)
    if(is.na(y_sigma)||is.null(y_sigma)||is.nan(y_sigma)||y_sigma==0) return(list(error=T,data=NULL,msg="sdv of target is na,null or 0"))
    y_mu<-mean(y,na.rm = T)
    y_norm<-(y-y_mu)/y_sigma
    model<-NULL
  }else{
    y_norm<-y
    y_sigma<-NULL
    y_mu<-NULL
  }
  
  if(!pseudo_inverse) K<-K[K<=ncol(X)]
  
  set.seed(1)
  b<-1*pracma::randn(K,1)  # Bias, escollit aleatoriament
  W<-matrix(pracma::randn(ncol(X)*K),ncol=K,nrow=ncol(X),byrow=T)
  H<-(X%*%W)+kronecker(u,t(b)) # Matriu de pesos * entrades + bias
  if(activation) H <- 1.0 %/% ( 1.0 + exp(-H))
  #Calcul dels pesos B de sortida
  if(pseudo_inverse){
    pI<-(pracma::pinv(t(H)%*%H) %*% t(H))
  }else{
    pI<-(pracma::inv(t(H)%*%H) %*% t(H)) #inv no funciona bien en todos los casos
  }
  
  B<-pI%*%y_norm
  # Calcul dels resultats
  yhat<-H%*%B
  #not_nan<-(!is.nan(y)&!is.nan(yhat)&!is.na(y)&!is.na(yhat))
  #yok<-y[not_nan]
  #yhat<-yhat[not_nan]
  #De-normalize
  if(!classifier) yhat<-(yhat*y_sigma)+y_mu
  #Rmse
  #rmse<-sqrt(sum((yhat-yok)^2,na.rm = T)/length(yok))
  rmse<-sqrt(sum((yhat-y)^2,na.rm = T)/length(y))
  norm_rmse<-rmse/(max_y-min_y)
  
  #Put on output prepared variables
  output_y[complete_cases]<-y
  output_yhat[complete_cases]<-yhat
  
  model<-list(classifier=classifier,K=K,b=b,W=W,B=B,pseudo_inverse=pseudo_inverse,activation=activation,column_names_X=column_names_X,target_column_name=target_column_name,X_mu=X_mu,X_sigma=X_sigma,y_mu=y_mu,y_sigma=y_sigma,predict=predict_elm)
  
  return(list(error=F,data=list(model=model,y=output_y,yhat=output_yhat,rmse=rmse,norm_rmse=norm_rmse,complete_cases=complete_cases,zero_sdv_columns=zero_sdv_columns),msg='ok'))
}
