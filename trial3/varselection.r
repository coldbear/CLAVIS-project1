##############################
#### Feature Selection########
##############################
rfe.train <- readRDS("rfetrain")
boruta.train<- readRDS("borutatrain")

Select_Features = function(method = c("RF", "Boruta_Tentative","Boruta_Final")){ 
  require(Boruta)
  require(randomForest)
  
  if(method == "RF") {
    #RF Feature Selection
    x=plot(rfe.train, type=c("g", "o"), cex = 1.0, col = 1:11, Labels = T)
    return(x)
  }
  else if(method == "Boruta_Tentative"){
    #Tentative Boruta
    plot(boruta.train, xlab = "", xaxt = "n")
    lz=lapply(1:ncol(boruta.train$ImpHistory),function(i)
      boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
    names(lz) = colnames(boruta.train$ImpHistory)
    Labels = sort(sapply(lz,median))
    axis(side = 1,las=2,labels = names(Labels),
         at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
  }
  else if(method == "Boruta_Final"){
    #Final Boruta
    final.boruta = TentativeRoughFix(boruta.train)
    boruta.df = attStats(final.boruta)
    plot(final.boruta, xlab = "", xaxt = "n")
    lz=lapply(1:ncol(final.boruta$ImpHistory),function(i)
      final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
    names(lz) = colnames(final.boruta$ImpHistory)
    Labels = sort(sapply(lz,median))
    axis(side = 1,las=2,labels = names(Labels),
         at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)
  }
  else {
    "Error: Select a valid method"  
  }
}

