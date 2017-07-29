normalize = function(x, mode = "min-max", new_min=0, new_max=1)
{
  if(!is.numeric(x)){x = as.numeric(x)} # convert to numeric
  
  # Square-root-normalization
  if(mode == "sqrt")
  {
    # Test for negative values
    if(TRUE %in% (x<0)){stop("Vector has negative entries. Cannot use sqrt-normalization.")}
    else{x = sqrt(x)}
  }
  # 
  else if(mode == "log")
  {
    # Test for zeroes - maybe we should add 1 if there are only positives?
    if(TRUE %in% (x==0)){stop("Vector has zeroes. Cannot use log-normalization.")}
    else{x = log(x)}
  }
  # Perform a min-max normalization 
  # (if a mode has been specified before this acts on the already normalized variable)
  normalized = (new_max-new_min) / (max(x)-min(x)) * (x - min(x)) + new_min
  return(normalized)
}


selectforoutlier<-function(x){
  
  # select all numerical variables except certain ones
  # which ones are numerical?
  numericals<-sapply(x, is.numeric)
  
  # loop over all numerical variables
  x<- x[numericals]
  x<- names(x)
  remove <- c("adFlag","availability","cPriceNA","unit","genericProduct","salesIndex","campaignIndex","X3.1.1.Unique_pids_per_group_binned.DB","X3.1.2.Unique_pids_per_group_binned_2.DB","X3.2.1.Unique_pids_per_manufacturer_binned.DB","X3.2.2.Unique_pids_per_manufacturer_binned_2.DB","X3.3.1.Unique_pids_per_category_binned.DB","X3.4.1.Unique_pids_per_day_binned.DB","day2")
  x <-x [! x %in% remove]
  return(x)
}




print_glmnet_coefs <- function(cvfit, s="lambda.min") {
  ind <- which(coef(cvfit, s=s) != 0)
  df <- data.frame(
    feature=rownames(coef(cvfit, s=s))[ind],
    coeficient=coef(cvfit, s=s)[ind]
  )
  kable(df)
}