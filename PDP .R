####Partial dependence  plots#######
library(randomForest)
library(MASS)
library(ICEbox)
library(caret)
library(glmnet)
library(knitr)


fold<- readRDS("alt")

#Decreasingthe amonunt of variables for faster calculation and easier to interpret graphs
idx = createDataPartition(y = fold$order, p = 0.2, list = FALSE)
fold = fold[idx,]

q = fold$order
Q = fold
Q = Q[,!names(Q)=="order"]

#train default model and the most regularized model with same predictive performance


rf= randomForest(Q,q,ntree=1500,mtry=4)
varImpPlot(rf)#pid, day 2, X25.pid_prop_per_day.Mat,group

par(mfrow=c(2,2))
partialPlot(rf, Q, day2)
partialPlot(rf, Q, pid)
partialPlot(rf, Q, X25.pid_prop_per_day.Mat)
partialPlot(rf, Q, group)


#ICEbox 

rf.ice= randomForest(Q,q)
par(mfrow=c(1,1))


#for pid
pid.ice = ice(object = rf.ice, X = Q, predictor = "pid", logodds = TRUE,
               predictfcn = function(object, newdata){
                 predict(object, newdata, type = "prob")[, 2]
               } )


plot(pid.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 1)
plot(pid.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 1,
     centered = TRUE)

#also the derivative ICEbox
pid.dice <- dice(pid.ice)
plot(pid.dice)

#for X25.pid_prop_per_day.Mat
X25.pid_prop_per_day.Mat.order.ice = ice(object = rf.ice, X = Q, predictor = "X25.pid_prop_per_day.Mat", logodds = TRUE,
                predictfcn = function(object, newdata){
                  predict(object, newdata, type = "prob")[, 2]
                } )


plot(X25.pid_prop_per_day.Mat.order.ice, frac_to_plot = 1, prop_range_y = TRUE,
     x_quantile = T, plot_orig_pts_preds = T)
plot(X25.pid_prop_per_day.Mat.order.ice, frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE,
     x_quantile = T, plot_orig_pts_preds = T)

X25.pid_prop_per_day.Mat.order.dice <- dice(X25.pid_prop_per_day.Mat.order.ice)
plot(X25.pid_prop_per_day.Mat.order.dice)

#for group
group.order.ice = ice(object = rf.ice, X = Q, predictor = "group", logodds = TRUE,
                   predictfcn = function(object, newdata){
                     predict(object, newdata, type = "prob")[, 2]
                   } )

group.order.ice$Xice$group = Q$group

plot(group.order.ice, frac_to_plot = 1, prop_range_y = TRUE,
     x_quantile = T, plot_orig_pts_preds = T, color_by = "group")
plot(group.order.ice, frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE,
     x_quantile = T, plot_orig_pts_preds = T, color_by = "group")

group.dice <- dice(group.order.ice)
plot(group.dice)
#######3D example#######
library(ISLR)
library(randomForest)
library(dplyr)

# set seed for reproducibility
set.seed(42)

data(College)

rf <- randomForest(Grad.Rate ~ ., data = College)
rf.revenue
# varImpPlot(rf)

var1_vals <- seq(from = min(X$price),
                 to = max(X$price),
                 by = (max(X$price) - 
                         min(X$price))/181)

var2_vals <- seq(from = min(X$competitorPrice),
                 to = max(X$competitorPrice),
                 by = (max(X$competitorPrice) - 
                         min(X$competitorPrice))/181)

# Create a 20x20 grid
two_vals <- expand.grid(var1_vals, var2_vals)
two_vals <- arrange(two_vals, Var1, Var2)

two_rep <- X[rep(1:nrow(X), nrow(two_vals)), ]

two_rep$price <- rep(two_vals$Var1, each = nrow(X))
two_rep$competitorPrice <- rep(two_vals$Var2, each = nrow(X))

two_pred <- predict(rf.revenue, two_rep)
two_rep$pred <- two_pred

two_agg <- group_by(two_rep, price, competitorPrice) %>%
  summarise(mean_pred = mean(pred))

z <- matrix(two_agg$mean_pred, nrow = length(var1_vals), byrow = TRUE)

# Set color range (using grayscale)
jet.colors <- colorRampPalette( c("#ffffff", "#2a2a2a") ) 

# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)

# Compute the z-value at the facet centers
zfacet <- z[-1, -1] + 
  z[-1, -1 * length(var1_vals)] + 
  z[-1 * length(var2_vals), -1] + 
  z[-1 * length(var1_vals), -1 * length(var2_vals)]

# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)

# Use persp for 3D plotting
persp(x = var1_vals, y = var2_vals, z = z, theta = -45,
      xlab = "\nPrice",
      ylab = "\ncompetitorPrice",
      zlab = "\nPredicted Value",
      cex.lab = 1,
      ticktype = "detailed",
      col = color[facetcol])
