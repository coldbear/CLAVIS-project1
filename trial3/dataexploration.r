####Data Exploration#####

smp_size <- floor(0.7* nrow(data))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]



