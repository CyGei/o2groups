# Load library
library(h2o)
library(caret)
library(tidymodels)

# start h2o cluster
invisible(h2o.init())

# split data into train and test randomly sampling 80% of the data for training
set.seed(123)

#use 70% of dataset as training set and 30% as test set using tidymodels
split = initial_split(outcomes, prop = 0.7)
train = training(split)
test = testing(split)


# set label type
y = 'Species'
pred = setdiff(names(train), y)

#convert variables to factors
train[,y] = as.factor(train[,y])
test[,y] = as.factor(test[,y])

# Run AutoML for 20 base models
aml = h2o.automl(x = pred, y = y,
                 training_frame = train_h,
                 max_models = 20,
                 seed = 1,
                 max_runtime_secs = 20
)

# AutoML Leaderboard
lb = aml@leaderboard
lb

# prediction result on test data
prediction = h2o.predict(aml@leader, test_h[,-5]) %>%
  as.data.frame()

# create a confusion matrix
caret::confusionMatrix(test$Species, prediction$predict)

# close h2o connection
h2o.shutdown(prompt = F)
