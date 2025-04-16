# Observe the relevant directories

DependsOnDirectory
MethodsDirectory
InputDirectory
OutputDirectory

# Create the function vector

FunctionVector = c('data.table', 'xgboost', 'DiagrammeR', 'xgbCrossValidation', 'xgboostParameterGenerator', 'ggplot2')

# Load openxlsx so as to read the function dependency information

library(openxlsx)

# Read in the function data

DependsOn = read.xlsx(xlsxFile = paste(DependsOnDirectory, '/R Methods.xlsx', sep = ''), sheet = 'DependsOn')

# Update the function vector with the dependencies

FunctionVector = unique(c(FunctionVector, DependsOn$DependsOn[(DependsOn$Function %in% FunctionVector) & (is.na(DependsOn$DependsOn) == FALSE)]))

for (i in seq(length(FunctionVector)))
{
  
  # If the function exists in the methods directory, then load it
  
  if (file.exists(paste(MethodsDirectory, '/', FunctionVector[i], '.R', sep = '')))
  {
    
    source(paste(MethodsDirectory, '/', FunctionVector[i], '.R', sep = ''))
    
  }
  
  # If the function does not exist in the methods directory then assume it refers to a library and load the
  # library
  
  else
  {
    
    library(FunctionVector[i], character.only = TRUE)
    
  }
  
}

# Read in the data

StandardizedData = fread(paste(InputDirectory, '/StandardizedData.csv', sep = ''))
DesignMatrix = fread(paste(InputDirectory, '/DesignMatrix.csv', sep = ''))

# Read in the meta data

Target = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'Target')
Target = Target[1, 1]

OffsetVariable = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'OffsetVariable')
OffsetVariable = OffsetVariable[1, 1]

Key = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'Key')
Key = Key[, 1]

WeightVariable = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'WeightVariable')
WeightVariable = WeightVariable[, 1]

# Create the training / testing split

set.seed(666)

TrainTestVector = sample(x = c(TRUE, FALSE), size = nrow(DesignMatrix), replace = TRUE, prob = c(.7, .3))

# Create the training and testing data

Training = xgb.DMatrix(data = data.matrix(DesignMatrix[TrainTestVector, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE]), label = DesignMatrix$Target[TrainTestVector])
Testing = xgb.DMatrix(data = data.matrix(DesignMatrix[!(TrainTestVector), names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE]), label = DesignMatrix$Target[!(TrainTestVector)])

# Create the parameter set for xgb.train

xgb.trainParameters = list(
  
  # General Parameters
  
  booster = 'gbtree', # c('gbtree', 'gblinear', 'dart')
  verbosity = 1, # c(0, 1, 2, 3)
  validate_parameters = TRUE,
  nthread = 2, # detectCores()
  disable_default_eval_metric = FALSE,

  # Parameters for Tree Booster  

  eta =  1, # [0, 1]
  gamma = 0, # [0, Inf)
  max_depth = 2, # [0, Inf)
  min_child_weight = 1, # (0, Inf)
  max_delta_step = 0, # (0, Inf)
  subsample = 1, # (0, 1]
  sampling_method = 'uniform', # c('uniform', 'gradient_based')
  colsample_bynode = 1, # (0, 1]
  colsample_bylevel = 1, # (0, 1]
  colsample_bytree = 1, # (0, 1]
  lambda = 1, # [0, Inf)
  alpha = 0, # [0, Inf)
  tree_method = 'auto', # c('auto', 'exact', 'approx', 'hist')
  scale_pos_weight = 1,
  refresh_leaf = 1, # c(0, 1)
  process_type = 'default', # c('default', 'update')
  grow_policy = 'depthwise', # c('depthwise', 'lossguide')
  max_leaves = 0, # [0, Inf)
  max_bin = 256, # [1, Inf)
  num_parallel_tree = 1, # [1, Inf)
  monotone_constraints = c(0, 0, 0), # c(-1, 0, 1)^NumberOfInputOrdinalVariables
  
  # Learning task parameters
   
  objective = 'reg:squarederror', 
  eval_metric = 'rmse',
  seed_per_iteration = FALSE)

# Create the model

Model = xgb.train(data = Training, params = xgb.trainParameters, nrounds = 50, watchlist = list(train = Training, test = Testing))

# Apply the model to all of the data on the standardized data.  Observe that this only works if the standardized
# data has the same sort order as the design matrix

StandardizedData$xgboost = predict(Model, xgb.DMatrix(data = data.matrix(DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE])))

# Recover the errors

sqrt(sum((StandardizedData$Target[TrainTestVector] - StandardizedData$xgboost[TrainTestVector])^2) / sum(TrainTestVector))
sqrt(sum((StandardizedData$Target[!(TrainTestVector)] - StandardizedData$xgboost[!(TrainTestVector)])^2) / sum(!(TrainTestVector)))

# Observe the trees in the model

xgb.dump(Model, with_stats = TRUE)

# Explicitly create the indicator variables in the first tree

DesignMatrix$Indicator01 = ifelse(DesignMatrix[[Model[[8]][140]]] < 0.934589028, 1, 0)
DesignMatrix$Indicator02 = ifelse(DesignMatrix[[Model[[8]][163]]] < 0.52636987, 1, 0)
DesignMatrix$Indicator03 = ifelse(DesignMatrix[[Model[[8]][373]]] < 0.5, 1, 0)

# Explicitly create the first tree

DesignMatrix$ModelReconstruction = DesignMatrix$Indicator01 * DesignMatrix$Indicator02 + 2 * DesignMatrix$Indicator01 * (1 - DesignMatrix$Indicator02) + 3 * (1 - DesignMatrix$Indicator01) * DesignMatrix$Indicator03 + 4 * (1 - DesignMatrix$Indicator01) * (1 - DesignMatrix$Indicator03)
xgb.dump(Model, with_stats = TRUE)[seq(8)]

# Check the construction of the first tree

DesignMatrix[TrainTestVector, .(Count = .N), by = 'ModelReconstruction']

# Discard the explicit features and model

DesignMatrix = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('Indicator01', 'Indicator02', 'Indicator03', 'ModelReconstruction'))], with = FALSE]

# Calculate the number of features, i.e. nodes and splits, in the model

sum(ifelse(substr(xgb.dump(Model, with_stats = TRUE), 1, 1) == 'b', 'booster', ifelse(grepl('leaf', xgb.dump(Model, with_stats = TRUE)), 'leaf', 'split')) != 'booster')

# Determine the round at which the error on the testing data is the lowest

Model[['evaluation_log']][['iter']][Model[['evaluation_log']][['test_rmse']] == min(Model[['evaluation_log']][['test_rmse']])][1]

# Observe the results of cross - validation

xgbCrossValidation(data = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE], label = DesignMatrix$Target, params = xgb.trainParameters, nrounds = 90, NFold = 10, TrainingPercentage = .7)

# Define the fixed parameters for the hyper parameter optimization

FixedParameters = list(
  
  # General Parameters
  
  verbosity = 1, # c(0, 1, 2, 3)
  validate_parameters = TRUE,
  nthread = 7, # detectCores()
  disable_default_eval_metric = FALSE,

  # Learning task parameters
  
  objective = 'reg:squarederror', 
  eval_metric = 'rmse',
  seed_per_iteration = FALSE)

# Define the sets from which the varying parameters for the hyper parameter optimization will be pulled

ParameterBoundList = list(

  booster = c('gbtree', 'gblinear', 'dart'),
  #booster = c('gbtree'),
    
  # Parameters for Tree Booster
  
  TreeBoosterParameters = list(
    
    eta =  c(0, 1), # [0, 1], .3
    gamma = c(0, 10), # [0, Inf), 0
    max_depth = c(1, 10), # [0, Inf), 6
    min_child_weight = c(0, 10), # (0, Inf), 1
    # max_delta_step = c(0, 1), # (0, Inf), 0
    subsample = c(0, 1), # (0, 1], 1
    sampling_method = c('uniform'),
    # colsample_bynode = c(0, 1), # (0, 1], 1
    # colsample_bylevel = c(0, 1), # (0, 1], 1
    # colsample_bytree = c(0, 1), # (0, 1], 1
    lambda = c(.5, 10), # [0, Inf), 1
    alpha = c(0, 1), # [0, Inf), 0
    tree_method = c('auto', 'exact', 'approx', 'hist'), # c('auto', 'exact', 'approx', 'hist'), 'auto'
    scale_pos_weight = 1,
    refresh_leaf = 1, # 1, c(0, 1)
    process_type = 'default', # 'default', c('default', 'update')
    grow_policy = 'depthwise', # 'depthwise', c('depthwise', 'lossguide')
    max_leaves = 0, # 0, [0, Inf)
    max_bin = c(10, 250), #256, [1, Inf)
    num_parallel_tree = c(1, 10), #1, [1, Inf)
    
    # eta = c(0, 1), # [0, 1], .3
    # gamma = c(0, 0), # [0, Inf), 0
    # max_depth = c(2, 2), # [0, Inf), 6
    # min_child_weight = c(1, 1), # (0, Inf), 1
    max_delta_step = c(0, 0), # (0, Inf), 0
    # subsample = c(1, 1), # (0, 1], 1
    # sampling_method = c('uniform'),
    colsample_bynode = c(1, 1), # (0, 1], 1
    colsample_bylevel = c(1, 1), # (0, 1], 1
    colsample_bytree = c(1, 1) # (0, 1], 1
    # lambda = c(1, 1), # [0, Inf), 1
    # alpha = c(0, 0), # [0, Inf), 0
    # tree_method = c('auto'), # c('auto', 'exact', 'approx', 'hist'), 'auto'
    # scale_pos_weight = 1, # 1
    # refresh_leaf = 1, # 1, c(0, 1)
    # process_type = 'default', # 'default', c('default', 'update')
    # grow_policy = 'depthwise', # 'depthwise', c('depthwise', 'lossguide')
    # max_leaves = 0, # 0, [0, Inf)
    # max_bin = c(256, 256), #256, [1, Inf)
    # num_parallel_tree = c(1, 1) #1, [1, Inf)
    
  ),
  
  dart = list(
    
    # Additional parameters for Dart Booster
    
    sample_type = c('uniform', 'weighted'), # 'uniform', c('uniform', 'weighted')
    normalize_type = c('tree', 'forest'), # 'tree', c('tree', 'forest')
    rate_drop = c(0, 1), # 0, [0, 1]
    one_drop = c(0, 1), # 0, c(0, 1)
    skip_drop = c(0, 1) # 0, [0, 1]
    
  ),
    
  gblinear = list(
    
    # Parameters for Tree Booster  
    
    updater = 'coord_descent',
    feature_selector = c('cyclic', 'greedy', 'thrifty'), # 'cyclic', c('cyclic', 'shuffle', 'random', 'greedy', 'thrifty')
    top_k = c(1, 386) # 0
    
  )

)

# Update the number of cores being used

xgb.trainParameters[['nthread']] = detectCores() - 1

# Initialize the summary of cross validation runs

Summary = list()

StartTime = Sys.time()

for (i in seq(1, 100))
{
  
  # Generate parameters for xgboost
  
  xgb.trainParameters = xgboostParameterGenerator(FixedParameters, ParameterBoundList)
    
  # Cross validate the xgboost parameters
  
  Results = xgbCrossValidation(data = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE], label = DesignMatrix$Target, params = xgb.trainParameters, nrounds = 50, NFold = 10, TrainingPercentage = .7)

  # Update the summary

  Summary = c(Summary, list(list(Meaniter = mean(Results[['iter']]), MeanTestStatistic = mean(Results[[3]]), Parameters = xgb.trainParameters)))

}

EndTime = Sys.time()

EndTime - StartTime

# Observe the smallest test statistic value, 27570.3344463804

print(paste('Smallest test statistic value : ', min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]})), sep = ''))

# Record the parameters corresponding to the smallest test statistic value

Bestxgb.trainParameters = Summary[[seq(length(Summary))[sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}) == min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}))][1]]][['Parameters']]

# Obtain the names of the parameters used

Parameters = unique(do.call(c, lapply(seq(length(Summary)), function(i){names(Summary[[i]][['Parameters']])})))

# Restrict attention to the parameters which were allowed to vary

Parameters = Parameters[Parameters %in% c('booster', names(ParameterBoundList$TreeBoosterParameters), names(ParameterBoundList$dart), names(ParameterBoundList$gblinear))]

for (Parameter in Parameters)
{
  
  # Find iterations which contain the parameter in question
  
  Iterations = seq(length(Summary))[sapply(seq(length(Summary)), function(i){Parameter %in% names(Summary[[i]][['Parameters']])})]
  
  # Gather all parameter values and associated mean iteration and test statistics 
  
  ParameterSummary = do.call(rbind, lapply(Iterations, function(i){data.table(Parameter = Parameter, Value = Summary[[i]][['Parameters']][[Parameter]], MeanIterationStatistic = Summary[[i]][['Meaniter']], MeanTestStatistic = Summary[[i]][['MeanTestStatistic']])}))
  
  # Print the value corresponding to the smallest test statistic
  
  print(paste(Parameter, ' : ', ParameterSummary$Value[ParameterSummary$MeanTestStatistic == min(ParameterSummary$MeanTestStatistic)][1], sep = ''))
  
  # Summarize to the level of the values of the parameter
  
  ParameterSummary02 = ParameterSummary[, .(MeanIterationStatistic = mean(MeanIterationStatistic), MeanTestStatistic = mean(MeanTestStatistic)), by = 'Value']

  # Plot the iteration and test statistics
  
  if (is.character(ParameterSummary02$Value) == FALSE)
  {
    
    plot(ParameterSummary02$Value, ParameterSummary02$MeanIterationStatistic, main = Parameter, xlab = Parameter, ylab = 'Iteration', pch = 19, col = 'green')
    plot(ParameterSummary02$Value, ParameterSummary02$MeanTestStatistic, main = Parameter, xlab = Parameter, ylab = 'Test Statistic', pch = 19, col = 'green')

  }
  
  else
  {
    
    barplot(ParameterSummary02$MeanIterationStatistic, names.arg = ParameterSummary02$Value, main = Parameter, xlab = 'Values', ylab = 'Iteration', col = "green")
    barplot(ParameterSummary02$MeanTestStatistic, names.arg = ParameterSummary02$Value, main = Parameter, xlab = 'Values', ylab = 'Test Statistic', col = "green")

  }

}

#####################################

ParameterBoundList = list(
  
  booster = c('gbtree', 'dart'),
  
  # Parameters for Tree Booster
  
  TreeBoosterParameters = list(
    
    eta =  c(0, 1), # [0, 1], .3
    gamma = c(0, 10), # [0, Inf), 0
    max_depth = c(3, 9), # [0, Inf), 6
    min_child_weight = c(0, 20), # (0, Inf), 1
    # max_delta_step = c(0, 1), # (0, Inf), 0
    subsample = c(.1, 1), # (0, 1], 1
    sampling_method = c('uniform'),
    # colsample_bynode = c(0, 1), # (0, 1], 1
    # colsample_bylevel = c(0, 1), # (0, 1], 1
    # colsample_bytree = c(0, 1), # (0, 1], 1
    lambda = c(.5, 10), # [0, Inf), 1
    alpha = c(0, 2), # [0, Inf), 0
    tree_method = c('auto', 'exact', 'approx', 'hist'), # c('auto', 'exact', 'approx', 'hist'), 'auto'
    scale_pos_weight = 1, # 1
    refresh_leaf = 1, # 1, c(0, 1)
    process_type = 'default', # 'default', c('default', 'update')
    grow_policy = 'depthwise', # 'depthwise', c('depthwise', 'lossguide')
    max_leaves = 0, # 0, [0, Inf)
    max_bin = c(10, 250), #256, [1, Inf)
    num_parallel_tree = c(4, 9), #1, [1, Inf)
    
    # eta = c(0, 1), # [0, 1], .3
    # gamma = c(0, 0), # [0, Inf), 0
    # max_depth = c(2, 2), # [0, Inf), 6
    # min_child_weight = c(1, 1), # (0, Inf), 1
    max_delta_step = c(0, 0), # (0, Inf), 0
    # subsample = c(1, 1), # (0, 1], 1
    # sampling_method = c('uniform'),
    colsample_bynode = c(1, 1), # (0, 1], 1
    colsample_bylevel = c(1, 1), # (0, 1], 1
    colsample_bytree = c(1, 1) # (0, 1], 1
    # lambda = c(1, 1), # [0, Inf), 1
    # alpha = c(0, 0), # [0, Inf), 0
    # tree_method = c('auto'), # c('auto', 'exact', 'approx', 'hist'), 'auto'
    # scale_pos_weight = 1, # 1
    # refresh_leaf = 1, # 1, c(0, 1)
    # process_type = 'default', # 'default', c('default', 'update')
    # grow_policy = 'depthwise', # 'depthwise', c('depthwise', 'lossguide')
    # max_leaves = 0, # 0, [0, Inf)
    # max_bin = c(256, 256), #256, [1, Inf)
    # num_parallel_tree = c(1, 1) #1, [1, Inf)
    
  ),
  
  dart = list(
    
    # Additional parameters for Dart Booster
    
    sample_type = c('uniform', 'weighted'), # 'uniform', c('uniform', 'weighted')
    normalize_type = c('tree', 'forest'), # 'tree', c('tree', 'forest')
    rate_drop = c(0, 1), # 0, [0, 1]
    one_drop = c(0, 0), # 0, c(0, 1)
    skip_drop = c(.1, 1) # 0, [0, 1]
    
  ),
  
  gblinear = list(
    
    # Parameters for Tree Booster  
    
    updater = 'coord_descent',
    feature_selector = c('cyclic', 'greedy', 'thrifty'), # 'cyclic', c('cyclic', 'shuffle', 'random', 'greedy', 'thrifty')
    top_k = c(1, 386) # 0
    
  )
  
)

Summary = list()

StartTime = Sys.time()

for (i in seq(1, 100))
{
  
  # Generate parameters for xgboost
  
  xgb.trainParameters = xgboostParameterGenerator(FixedParameters, ParameterBoundList)
  
  # Cross validate the xgboost parameters
  
  Results = xgbCrossValidation(data = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE], label = DesignMatrix$Target, params = xgb.trainParameters, nrounds = 50, NFold = 10, TrainingPercentage = .7)
  
  # Update the summary
  
  Summary = c(Summary, list(list(Meaniter = mean(Results[['iter']]), MeanTestStatistic = mean(Results[[3]]), Parameters = xgb.trainParameters)))
  
}

EndTime = Sys.time()

EndTime - StartTime

# Observe the smallest test statistic value, 27033.329720728

print(paste('Smallest test statistic value : ', min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]})), sep = ''))

# Record the parameters corresponding to the smallest test statistic value

Bestxgb.trainParameters = Summary[[seq(length(Summary))[sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}) == min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}))][1]]][['Parameters']]

# Obtain the names of the parameters used

Parameters = unique(do.call(c, lapply(seq(length(Summary)), function(i){names(Summary[[i]][['Parameters']])})))

# Restrict attention to the parameters which were allowed to vary

Parameters = Parameters[Parameters %in% c('booster', names(ParameterBoundList$TreeBoosterParameters), names(ParameterBoundList$dart), names(ParameterBoundList$gblinear))]

# Clear all graphs

dev.off(dev.list()['RStudioGD'])

for (Parameter in Parameters)
{
  
  # Find iterations which contain the parameter in question
  
  Iterations = seq(length(Summary))[sapply(seq(length(Summary)), function(i){Parameter %in% names(Summary[[i]][['Parameters']])})]
  
  # Gather all parameter values and associated mean iteration and test statistics 
  
  ParameterSummary = do.call(rbind, lapply(Iterations, function(i){data.table(Parameter = Parameter, Value = Summary[[i]][['Parameters']][[Parameter]], MeanIterationStatistic = Summary[[i]][['Meaniter']], MeanTestStatistic = Summary[[i]][['MeanTestStatistic']])}))
  
  # Print the value corresponding to the smallest test statistic
  
  print(paste(Parameter, ' : ', ParameterSummary$Value[ParameterSummary$MeanTestStatistic == min(ParameterSummary$MeanTestStatistic)][1], sep = ''))
  
  # Summarize to the level of the values of the parameter
  
  ParameterSummary02 = ParameterSummary[, .(MeanIterationStatistic = mean(MeanIterationStatistic), MeanTestStatistic = mean(MeanTestStatistic)), by = 'Value']
  
  # Plot the iteration and test statistics
  
  if (is.character(ParameterSummary02$Value) == FALSE)
  {
    
    plot(ParameterSummary02$Value, ParameterSummary02$MeanIterationStatistic, main = Parameter, xlab = Parameter, ylab = 'Iteration', pch = 19, col = 'green')
    plot(ParameterSummary02$Value, ParameterSummary02$MeanTestStatistic, main = Parameter, xlab = Parameter, ylab = 'Test Statistic', pch = 19, col = 'green')
    
  }
  
  else
  {
    
    barplot(ParameterSummary02$MeanIterationStatistic, names.arg = ParameterSummary02$Value, main = Parameter, xlab = 'Values', ylab = 'Iteration', col = "green")
    barplot(ParameterSummary02$MeanTestStatistic, names.arg = ParameterSummary02$Value, main = Parameter, xlab = 'Values', ylab = 'Test Statistic', col = "green")
    
  }
  
}

#####################################

ParameterBoundList = list(
  
  booster = c('gbtree'),
  
  # Parameters for Tree Booster
  
  TreeBoosterParameters = list(
    
    eta =  c(.1, 1), # [0, 1], .3
    gamma = c(0, 10), # [0, Inf), 0
    max_depth = c(5, 9), # [0, Inf), 6
    min_child_weight = c(1, 20), # (0, Inf), 1
    # max_delta_step = c(0, 1), # (0, Inf), 0
    subsample = c(.1, 1), # (0, 1], 1
    sampling_method = c('uniform'),
    # colsample_bynode = c(0, 1), # (0, 1], 1
    # colsample_bylevel = c(0, 1), # (0, 1], 1
    # colsample_bytree = c(0, 1), # (0, 1], 1
    lambda = c(1, 10), # [0, Inf), 1
    alpha = c(0, 1), # [0, Inf), 0
    tree_method = c('auto', 'hist'), # c('auto', 'exact', 'approx', 'hist'), 'auto'
    scale_pos_weight = 1, # 1
    refresh_leaf = 1, # 1, c(0, 1)
    process_type = 'default', # 'default', c('default', 'update')
    grow_policy = 'depthwise', # 'depthwise', c('depthwise', 'lossguide')
    max_leaves = 0, # 0, [0, Inf)
    max_bin = c(1, 300), #256, [1, Inf)
    num_parallel_tree = c(1, 8), #1, [1, Inf)
    
    # eta = c(0, 1), # [0, 1], .3
    # gamma = c(0, 0), # [0, Inf), 0
    # max_depth = c(2, 2), # [0, Inf), 6
    # min_child_weight = c(1, 1), # (0, Inf), 1
    max_delta_step = c(0, 0), # (0, Inf), 0
    # subsample = c(1, 1), # (0, 1], 1
    # sampling_method = c('uniform'),
    colsample_bynode = c(1, 1), # (0, 1], 1
    colsample_bylevel = c(1, 1), # (0, 1], 1
    colsample_bytree = c(1, 1) # (0, 1], 1
    # lambda = c(1, 1), # [0, Inf), 1
    # alpha = c(0, 0), # [0, Inf), 0
    # tree_method = c('auto'), # c('auto', 'exact', 'approx', 'hist'), 'auto'
    # scale_pos_weight = 1, # 1
    # refresh_leaf = 1, # 1, c(0, 1)
    # process_type = 'default', # 'default', c('default', 'update')
    # grow_policy = 'depthwise', # 'depthwise', c('depthwise', 'lossguide')
    # max_leaves = 0, # 0, [0, Inf)
    # max_bin = c(256, 256), #256, [1, Inf)
    # num_parallel_tree = c(1, 1) #1, [1, Inf)
    
  ),
  
  dart = list(
    
    # Additional parameters for Dart Booster
    
    sample_type = c('uniform', 'weighted'), # 'uniform', c('uniform', 'weighted')
    normalize_type = c('tree', 'forest'), # 'tree', c('tree', 'forest')
    rate_drop = c(0, 1), # 0, [0, 1]
    one_drop = c(1, 1), # 0, c(0, 1)
    skip_drop = c(.4, 1) # 0, [0, 1]
    
  ),
  
  gblinear = list(
    
    # Parameters for Tree Booster  
    
    updater = 'coord_descent',
    feature_selector = c('cyclic', 'greedy', 'thrifty'), # 'cyclic', c('cyclic', 'shuffle', 'random', 'greedy', 'thrifty')
    top_k = c(1, 386) # 0
    
  )
  
)

Summary = list()

StartTime = Sys.time()

for (i in seq(1, 100))
{
  
  # Generate parameters for xgboost
  
  xgb.trainParameters = xgboostParameterGenerator(FixedParameters, ParameterBoundList)
  
  # Cross validate the xgboost parameters
  
  Results = xgbCrossValidation(data = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE], label = DesignMatrix$Target, params = xgb.trainParameters, nrounds = 50, NFold = 10, TrainingPercentage = .7)
  
  # Update the summary
  
  Summary = c(Summary, list(list(Meaniter = mean(Results[['iter']]), MeanTestStatistic = mean(Results[[3]]), Parameters = xgb.trainParameters)))
  
}

EndTime = Sys.time()

EndTime - StartTime

# Observe the smallest test statistic value, 26703.7778934681

print(paste('Smallest test statistic value : ', min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]})), sep = ''))

# Record the parameters corresponding to the smallest test statistic value

Bestxgb.trainParameters = Summary[[seq(length(Summary))[sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}) == min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}))][1]]][['Parameters']]

# Obtain the names of the parameters used

Parameters = unique(do.call(c, lapply(seq(length(Summary)), function(i){names(Summary[[i]][['Parameters']])})))

# Restrict attention to the parameters which were allowed to vary

Parameters = Parameters[Parameters %in% c('booster', names(ParameterBoundList$TreeBoosterParameters), names(ParameterBoundList$dart), names(ParameterBoundList$gblinear))]

# Clear all graphs

dev.off(dev.list()['RStudioGD'])

for (Parameter in Parameters)
{
  
  # Find iterations which contain the parameter in question
  
  Iterations = seq(length(Summary))[sapply(seq(length(Summary)), function(i){Parameter %in% names(Summary[[i]][['Parameters']])})]
  
  # Gather all parameter values and associated mean iteration and test statistics 
  
  ParameterSummary = do.call(rbind, lapply(Iterations, function(i){data.table(Parameter = Parameter, Value = Summary[[i]][['Parameters']][[Parameter]], MeanIterationStatistic = Summary[[i]][['Meaniter']], MeanTestStatistic = Summary[[i]][['MeanTestStatistic']])}))
  
  # Print the value corresponding to the smallest test statistic
  
  print(paste(Parameter, ' : ', ParameterSummary$Value[ParameterSummary$MeanTestStatistic == min(ParameterSummary$MeanTestStatistic)][1], sep = ''))
  
  # Summarize to the level of the values of the parameter
  
  ParameterSummary02 = ParameterSummary[, .(MeanIterationStatistic = mean(MeanIterationStatistic), MeanTestStatistic = mean(MeanTestStatistic)), by = 'Value']
  
  # Plot the iteration and test statistics
  
  if (is.character(ParameterSummary02$Value) == FALSE)
  {
    
    plot(ParameterSummary02$Value, ParameterSummary02$MeanIterationStatistic, main = Parameter, xlab = Parameter, ylab = 'Iteration', pch = 19, col = 'green')
    plot(ParameterSummary02$Value, ParameterSummary02$MeanTestStatistic, main = Parameter, xlab = Parameter, ylab = 'Test Statistic', pch = 19, col = 'green')
    
  }
  
  else
  {
    
    barplot(ParameterSummary02$MeanIterationStatistic, names.arg = ParameterSummary02$Value, main = Parameter, xlab = 'Values', ylab = 'Iteration', col = "green")
    barplot(ParameterSummary02$MeanTestStatistic, names.arg = ParameterSummary02$Value, main = Parameter, xlab = 'Values', ylab = 'Test Statistic', col = "green")
    
  }
  
}

#####################################

ParameterBoundList = list(
  
  booster = c('gbtree'),
  
  # Parameters for Tree Booster
  
  TreeBoosterParameters = list(
    
    eta =  c(.1, .4), # [0, 1], .3
    gamma = c(0, 10), # [0, Inf), 0
    max_depth = c(5, 9), # [0, Inf), 6
    min_child_weight = c(1, 30), # (0, Inf), 1
    # max_delta_step = c(0, 1), # (0, Inf), 0
    subsample = c(.1, 1), # (0, 1], 1
    sampling_method = c('uniform'),
    # colsample_bynode = c(0, 1), # (0, 1], 1
    # colsample_bylevel = c(0, 1), # (0, 1], 1
    # colsample_bytree = c(0, 1), # (0, 1], 1
    lambda = c(1, 10), # [0, Inf), 1
    alpha = c(0, 1), # [0, Inf), 0
    tree_method = c('auto', 'hist'), # c('auto', 'exact', 'approx', 'hist'), 'auto'
    scale_pos_weight = 1, # 1
    refresh_leaf = 1, # 1, c(0, 1)
    process_type = 'default', # 'default', c('default', 'update')
    grow_policy = 'depthwise', # 'depthwise', c('depthwise', 'lossguide')
    max_leaves = 0, # 0, [0, Inf)
    max_bin = c(1, 300), #256, [1, Inf)
    num_parallel_tree = c(3, 9), #1, [1, Inf)
    
    # eta = c(0, 1), # [0, 1], .3
    # gamma = c(0, 0), # [0, Inf), 0
    # max_depth = c(2, 2), # [0, Inf), 6
    # min_child_weight = c(1, 1), # (0, Inf), 1
    max_delta_step = c(0, 0), # (0, Inf), 0
    # subsample = c(1, 1), # (0, 1], 1
    # sampling_method = c('uniform'),
    colsample_bynode = c(1, 1), # (0, 1], 1
    colsample_bylevel = c(1, 1), # (0, 1], 1
    colsample_bytree = c(1, 1) # (0, 1], 1
    # lambda = c(1, 1), # [0, Inf), 1
    # alpha = c(0, 0), # [0, Inf), 0
    # tree_method = c('auto'), # c('auto', 'exact', 'approx', 'hist'), 'auto'
    # scale_pos_weight = 1, # 1
    # refresh_leaf = 1, # 1, c(0, 1)
    # process_type = 'default', # 'default', c('default', 'update')
    # grow_policy = 'depthwise', # 'depthwise', c('depthwise', 'lossguide')
    # max_leaves = 0, # 0, [0, Inf)
    # max_bin = c(256, 256), #256, [1, Inf)
    # num_parallel_tree = c(1, 1) #1, [1, Inf)
    
  ),
  
  dart = list(
    
    # Additional parameters for Dart Booster
    
    sample_type = c('uniform', 'weighted'), # 'uniform', c('uniform', 'weighted')
    normalize_type = c('tree', 'forest'), # 'tree', c('tree', 'forest')
    rate_drop = c(0, 1), # 0, [0, 1]
    one_drop = c(1, 1), # 0, c(0, 1)
    skip_drop = c(.4, 1) # 0, [0, 1]
    
  ),
  
  gblinear = list(
    
    # Parameters for Tree Booster  
    
    updater = 'coord_descent',
    feature_selector = c('cyclic', 'greedy', 'thrifty'), # 'cyclic', c('cyclic', 'shuffle', 'random', 'greedy', 'thrifty')
    top_k = c(1, 386) # 0
    
  )
  
)

Summary = list()

StartTime = Sys.time()

for (i in seq(1, 100))
{
  
  # Generate parameters for xgboost
  
  xgb.trainParameters = xgboostParameterGenerator(FixedParameters, ParameterBoundList)
  
  # Cross validate the xgboost parameters
  
  Results = xgbCrossValidation(data = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE], label = DesignMatrix$Target, params = xgb.trainParameters, nrounds = 50, NFold = 10, TrainingPercentage = .7)
  
  # Update the summary
  
  Summary = c(Summary, list(list(Meaniter = mean(Results[['iter']]), MeanTestStatistic = mean(Results[[3]]), Parameters = xgb.trainParameters)))
  
}

EndTime = Sys.time()

EndTime - StartTime

# Observe the smallest test statistic value, 26677.6845627934

print(paste('Smallest test statistic value : ', min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]})), sep = ''))

# Record the parameters corresponding to the smallest test statistic value

Bestxgb.trainParameters = Summary[[seq(length(Summary))[sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}) == min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}))][1]]][['Parameters']]

# Obtain the names of the parameters used

Parameters = unique(do.call(c, lapply(seq(length(Summary)), function(i){names(Summary[[i]][['Parameters']])})))

# Restrict attention to the parameters which were allowed to vary

Parameters = Parameters[Parameters %in% c('booster', names(ParameterBoundList$TreeBoosterParameters), names(ParameterBoundList$dart), names(ParameterBoundList$gblinear))]

# Clear all graphs

dev.off(dev.list()['RStudioGD'])

for (Parameter in Parameters)
{
  
  # Find iterations which contain the parameter in question
  
  Iterations = seq(length(Summary))[sapply(seq(length(Summary)), function(i){Parameter %in% names(Summary[[i]][['Parameters']])})]
  
  # Gather all parameter values and associated mean iteration and test statistics 
  
  ParameterSummary = do.call(rbind, lapply(Iterations, function(i){data.table(Parameter = Parameter, Value = Summary[[i]][['Parameters']][[Parameter]], MeanIterationStatistic = Summary[[i]][['Meaniter']], MeanTestStatistic = Summary[[i]][['MeanTestStatistic']])}))
  
  # Print the value corresponding to the smallest test statistic
  
  print(paste(Parameter, ' : ', ParameterSummary$Value[ParameterSummary$MeanTestStatistic == min(ParameterSummary$MeanTestStatistic)][1], sep = ''))
  
  # Summarize to the level of the values of the parameter
  
  ParameterSummary02 = ParameterSummary[, .(MeanIterationStatistic = mean(MeanIterationStatistic), MeanTestStatistic = mean(MeanTestStatistic)), by = 'Value']
  
  # Plot the iteration and test statistics
  
  if (is.character(ParameterSummary02$Value) == FALSE)
  {
    
    plot(ParameterSummary02$Value, ParameterSummary02$MeanIterationStatistic, main = Parameter, xlab = Parameter, ylab = 'Iteration', pch = 19, col = 'green')
    plot(ParameterSummary02$Value, ParameterSummary02$MeanTestStatistic, main = Parameter, xlab = Parameter, ylab = 'Test Statistic', pch = 19, col = 'green')
    
  }
  
  else
  {
    
    barplot(ParameterSummary02$MeanIterationStatistic, names.arg = ParameterSummary02$Value, main = Parameter, xlab = 'Values', ylab = 'Iteration', col = "green")
    barplot(ParameterSummary02$MeanTestStatistic, names.arg = ParameterSummary02$Value, main = Parameter, xlab = 'Values', ylab = 'Test Statistic', col = "green")
    
  }
  
}

# No new modeling will take place since there is no obvious set of parameters to attempt

# Create the model using the tuned hyperparameters

Model = xgb.train(data = Training, params = Bestxgb.trainParameters, nrounds = 50, watchlist = list(train = Training, test = Testing))

# Determine the round at which the error on the testing data is the lowest

nrounds = Model[['evaluation_log']][['iter']][Model[['evaluation_log']][['test_rmse']] == min(Model[['evaluation_log']][['test_rmse']])][1]

# Create the model

Model = xgb.train(data = Training, params = Bestxgb.trainParameters, nrounds = nrounds, watchlist = list(train = Training, test = Testing))

# Apply the model to all of the data on the standardized data.  Observe that this only works if the standardized
# data has the same sort order as the design matrix

StandardizedData$xgboost = predict(Model, xgb.DMatrix(data = data.matrix(DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE])))

# Recover the errors

sqrt(sum((StandardizedData$Target[TrainTestVector] - StandardizedData$xgboost[TrainTestVector])^2) / sum(TrainTestVector))
sqrt(sum((StandardizedData$Target[!(TrainTestVector)] - StandardizedData$xgboost[!(TrainTestVector)])^2) / sum(!(TrainTestVector)))

# Write the model to the disk

fwrite(x = StandardizedData[, c(Key, 'xgboost'), with = FALSE], file = paste(OutputDirectory, '/xgboostModel.csv', sep = ''))

# Write the model parameters to the disk

fwrite(x = as.data.table(t(sapply(seq(length(Bestxgb.trainParameters)), function(i){data.table(Parameter = names(Bestxgb.trainParameters)[i], Value = Bestxgb.trainParameters[[i]])}))), file = paste(OutputDirectory, '/xgboostModelParameters.csv', sep = ''))