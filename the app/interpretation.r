#this function returns  an  rf model
#bbbb <- rf(fold, 0.4,"order", 400, 10)

varimp <- function(model) {
  varImpPlot(model, type = 2)
}
#pdp(rf.model,data)
pdp <- function(model, data) {
 # importanceOrder=order(-random_forest$importance)
 # names <- rownames((random_forest$importance))[importanceOrder][1:55]
 # names <- as.factor(names)
 # list <- as.list(levels(names))
 # list <- unlist(list)
  par(mfrow=c(3,1))
  #par(mar=c(1,1,1,1))
  a <- partialPlot(model, data, pid)
  b <- partialPlot(model, data, X15.1.adFlag_last_1_day.Mat)
  c <-  partialPlot(model, data, X15.3.adFlag_last_1_obs.Mat)
  return(list(a,b,c))
  #partialPlot(random_forest, var_clus, data[,list[1]])
  #partialPlot(random_forest, var_clus, list[2])
  #partialPlot(random_forest, var_clus, list[3])
  #partialPlot(random_forest, var_clus, list[4])
}

#Now ICEbox  function
pidICEbox <- readRDS("icepid")
priceICEbox<- readRDS("iceprice")
#group-ICEbox <- readRDS("icegroup")

ice<- function(icemodel){
plot(icemodel, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 1, centered = TRUE)
}


#plot(group.order.ice, frac_to_plot = 1, prop_range_y = TRUE,
 #    x_quantile = T, plot_orig_pts_preds = T, color_by = "group")
#rf.ice <- randomForest(x_train, y_train)
