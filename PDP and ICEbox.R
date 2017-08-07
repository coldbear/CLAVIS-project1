####Partial dependence  plots#######
library(randomForest)


saveRDS(fold_1, "alt")
fold2 <- readRDS("alt")
fold <- readRDS("forPDP")

#take a stratified sample 
set.seed(222)
fold2$order <- as.factor(fold2$order)
idx = createDataPartition(y = fold2$order, p = 0.5, list = FALSE)
fold2 = fold2[idx,]
y = fold2$order
X = fold2
X = X[,!names(X)=="order"]
#train default model and the most regularized model with same predictive performance

rf.default = randomForest(X,y,ntree=800)
rf.robust = randomForest(X,y,ntree=1000,mtry=4)
varImpPlot(rf.default)#pid, day 2, X25.pid_prop_per_day.Mat
varImpPlot(rf.robust)
partialPlot(rf.default, X, day2)
partialPlot(rf.default, X, pid)
partialPlot(rf.default, X, X25.pid_prop_per_day.Mat)
partialPlot(rf.default, X, competitorPrice)
partialPlot(rf.default, X, X3.4.Unique_pids_per_day.DB)
partialPlot(rf.default, X, group)
partialPlot(rf.default, X, category)
partialPlot(rf.default, X, manufacturer)
#######3D example#######
library(ISLR)
library(randomForest)
library(dplyr)

# set seed for reproducibility
set.seed(42)

data(College)

rf <- randomForest(Grad.Rate ~ ., data = College)
# varImpPlot(rf)

var1_vals <- seq(from = min(College$Outstate),
                 to = max(College$Outstate),
                 by = (max(College$Outstate) - 
                         min(College$Outstate))/19)

var2_vals <- seq(from = min(College$perc.alumni),
                 to = max(College$perc.alumni),
                 by = (max(College$perc.alumni) - 
                         min(College$perc.alumni))/19)

# Create a 20x20 grid
two_vals <- expand.grid(var1_vals, var2_vals)
two_vals <- arrange(two_vals, Var1, Var2)

two_rep <- College[rep(1:nrow(College), nrow(two_vals)), ]

two_rep$Outstate <- rep(two_vals$Var1, each = nrow(College))
two_rep$perc.alumni <- rep(two_vals$Var2, each = nrow(College))

two_pred <- predict(rf, two_rep)
two_rep$pred <- two_pred

two_agg <- group_by(two_rep, Outstate, perc.alumni) %>%
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
      xlab = "\nOut of State Tuition",
      ylab = "\nPercentage Alumni Donating",
      zlab = "\nPredicted Value",
      cex.lab = 1,
      ticktype = "detailed",
      col = color[facetcol])