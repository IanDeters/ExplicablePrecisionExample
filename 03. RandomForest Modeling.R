# Observe the relevant directories

DependsOnDirectory
MethodsDirectory
InputDirectory
OutputDirectory

# Create the function vector

FunctionVector = c('data.table', 'parallel', 'ranger', 'rangerCrossValidation', 'rangerParameterGenerator', 'Discretize', 'ggplot2')

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

# Create the random forest parameters

rangerParameters = list(
  
  num.trees = 500, # [1, Inf)
  mtry = NULL, # [1, ncol(x)] sqrt(ncol(x))
  min.node.size = NULL, # (0, Inf), 5
  min.bucket = NULL, # (0, Inf), 1
  max.depth = NULL, # [1, floor(nrow(x) / min.bucket))
  replace = TRUE, # c(FALSE, TRUE)
  sample.fraction = 1, # (0, 1), c(.632, 1)
  oob.error = TRUE,
  num.threads = NULL,
  seed = 667
  
)

# Create a forest with 500 trees

Model = ranger(
  
  y = DesignMatrix$Target, x = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE],
  case.weights = TrainTestVector,
  holdout = TRUE,
  num.trees = rangerParameters[['num.trees']],
  mtry = rangerParameters[['mtry']],
  importance = 'impurity',
  min.node.size = rangerParameters[['min.node.size']],
  min.bucket = rangerParameters[['min.bucket']],
  max.depth = rangerParameters[['max.depth']],
  replace = rangerParameters[['replace']],
  sample.fraction = rangerParameters[['sample.fraction']],
  splitrule = 'variance',
  oob.error = rangerParameters[['oob.error']],
  num.threads = rangerParameters[['num.threads']],
  seed = rangerParameters[['seed']]
  
)

# Recover the errors

Model[['prediction.error']]
sum((Model[['predictions']][!(TrainTestVector)] - DesignMatrix$Target[!(TrainTestVector)])^2) / sum(!(TrainTestVector))
sum((predict(Model, DesignMatrix[!(TrainTestVector)])[['predictions']] - DesignMatrix$Target[!(TrainTestVector)])^2) / sum(!(TrainTestVector))

Summary = data.table()

for (i in seq(1, detectCores() - 1))
{
  
  # Set the number of threads
  
  rangerParameters[['num.threads']] = i

  for (j in seq(100))
  {
    
    # Record the start time
    
    StartTime = Sys.time()
    
    Model = ranger(
      
      y = DesignMatrix$Target, x = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE],
      case.weights = TrainTestVector,
      holdout = TRUE,
      num.trees = rangerParameters[['num.trees']],
      mtry = rangerParameters[['mtry']],
      importance = 'impurity',
      min.node.size = rangerParameters[['min.node.size']],
      min.bucket = rangerParameters[['min.bucket']],
      max.depth = rangerParameters[['max.depth']],
      replace = rangerParameters[['replace']],
      sample.fraction = rangerParameters[['sample.fraction']],
      splitrule = 'variance',
      oob.error = rangerParameters[['oob.error']],
      num.threads = rangerParameters[['num.threads']],
      seed = rangerParameters[['seed']]
      
    )
    
    Summary = rbind(Summary, data.table(NumberOfCores = i, Iteration = j, Duration = Sys.time() - StartTime))

  }
  
}

Summary[order(Summary$NumberOfCores), .(Duration = mean(Duration)), by = 'NumberOfCores']

# Observe the results of cross - validation

rangerCrossValidation(data = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE], label = DesignMatrix$Target, params = rangerParameters, NFold = 10, TrainingPercentage = .7)

# Define the sets from which the varying parameters for the hyper parameter optimization will be pulled

ParameterBoundList = list(

  num.trees = c(1, 500), # [1, Inf)
  mtry = c(1, 386), # [1, ncol(x)], sqrt(ncol(x))
  min.node.size = c(3, 10), # (0, Inf), 5
  min.bucket = c(1, 100), # (0, Inf), 1
  max.depth = c(1, 10), # [1, floor(nrow(x) / min.bucket)), NULL
  replace = c(FALSE, TRUE), # c(FALSE, TRUE), TRUE
  sample.fraction = c(0, 1), # (0, 1), c(.632, 1)
  oob.error = c(TRUE), # TRUE
  num.threads = c(32, 32), # 2
  seed = c(667, 667) # NULL

)

# Initialize the summary of cross validation runs

Summary = list()

StartTime = Sys.time()

for (i in seq(1, 1000))
{
  
  set.seed(666 + i - 1)
  
  # Generate parameters for ranger
  
  rangerParameters = rangerParameterGenerator(ParameterBoundList)
  
  # Explicitly set the seed
  
  rangerParameters[['seed']] = 666 + i - 1
  
  # Ensure the sample size is sufficiently large to avoid the 
  # 'sample_fraction too small, no observations sampled. Ranger will EXIT now.' error
  
  rangerParameters[['sample.fraction']] = max(rangerParameters[['sample.fraction']], .01)

  # Cross validate the ranger parameters

  Results = rangerCrossValidation(data = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE], label = DesignMatrix$Target, params = rangerParameters, NFold = 10, TrainingPercentage = .7)

  # Update the summary

  Summary = c(Summary, list(list(MeanTestStatistic = mean(Results[[2]]), Parameters = rangerParameters)))

}

EndTime = Sys.time()

EndTime - StartTime

# Observe the smallest test statistic value, 897472557.764244

print(paste('Smallest test statistic value : ', min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]})), sep = ''))

# Record the parameters corresponding to the smallest test statistic value

BestrangerParameters = Summary[[seq(length(Summary))[sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}) == min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}))][1]]][['Parameters']]

# Obtain the names of the parameters used

Parameters = unique(do.call(c, lapply(seq(length(Summary)), function(i){names(Summary[[i]][['Parameters']])})))

# Restrict attention to the parameters which were allowed to vary

Parameters = Parameters[!(Parameters %in% c('num.threads', 'seed'))]

for (Parameter in Parameters)
{
  
  # Find iterations which contain the parameter in question
  
  Iterations = seq(length(Summary))[sapply(seq(length(Summary)), function(i){Parameter %in% names(Summary[[i]][['Parameters']])})]
  
  # Gather all parameter values and associated mean iteration and test statistics 
  
  ParameterSummary = do.call(rbind, lapply(Iterations, function(i){data.table(Parameter = Parameter, Value = Summary[[i]][['Parameters']][[Parameter]], MeanTestStatistic = Summary[[i]][['MeanTestStatistic']])}))
  
  # Print the value corresponding to the smallest test statistic
  
  print(paste(Parameter, ' : ', ParameterSummary$Value[ParameterSummary$MeanTestStatistic == min(ParameterSummary$MeanTestStatistic)][1], sep = ''))

  # Summarize to the level of the values of the parameter
  
  ParameterSummary02 = ParameterSummary[, .(Count = .N, MeanTestStatistic = mean(MeanTestStatistic)), by = 'Value']

  if (is.character(ParameterSummary02$Value) == FALSE)
  {
    
    # Discretize the values of the parameter
    
    ParameterSummary02$Group = Discretize(Weight = ParameterSummary02$Count, x = ParameterSummary02$Value, GroupNumber = 100)
    
    # Summarize to the level of the group
    
    ParameterSummary03 = ParameterSummary02[, .(Value = sum(Count * Value) / sum(Count), MeanTestStatistic = sum(Count * MeanTestStatistic) / sum(Count)), by = 'Group']
    
    # Plot the iteration and test statistics
    
    plot(ParameterSummary03$Value, ParameterSummary03$MeanTestStatistic, main = Parameter, xlab = Parameter, ylab = 'Test Statistic', pch = 19, col = 'green')

  }
  
  else
  {
    
    # Plot the iteration and test statistics
    
    barplot(ParameterSummary02$MeanTestStatistic, names.arg = ParameterSummary02$Value, main = Parameter, xlab = 'Values', ylab = 'Test Statistic', col = 'green')

  }

}

#####################################

# Define the sets from which the varying parameters for the hyper parameter optimization will be pulled

ParameterBoundList = list(
  
  num.trees = c(1, 300), # [1, Inf)
  mtry = c(100, 300), # [1, ncol(x)], sqrt(ncol(x))
  min.node.size = c(1, 20), # (0, Inf), 5
  min.bucket = c(1, 50), # (0, Inf), 1
  max.depth = c(4, 13), # [1, floor(nrow(x) / min.bucket)), NULL
  replace = c(TRUE, TRUE), # c(FALSE, TRUE), TRUE
  sample.fraction = c(.5, 1), # (0, 1), c(.632, 1)
  oob.error = c(TRUE), # TRUE
  num.threads = c(32, 32), # 2
  seed = c(667, 667) # NULL
  
)

# Initialize the summary of cross validation runs

Summary = list()

StartTime = Sys.time()

for (i in seq(1, 1000))
{
  
  # Generate parameters for ranger
  
  rangerParameters = rangerParameterGenerator(ParameterBoundList)
  
  # Explicitly set the seed
  
  rangerParameters[['seed']] = 666 + i - 1
  
  # Ensure the sample size is sufficiently large to avoid the 
  # 'sample_fraction too small, no observations sampled. Ranger will EXIT now.' error
  
  rangerParameters[['sample.fraction']] = max(rangerParameters[['sample.fraction']], .01)
  
  # Cross validate the ranger parameters
  
  Results = rangerCrossValidation(data = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE], label = DesignMatrix$Target, params = rangerParameters, NFold = 10, TrainingPercentage = .7)
  
  # Update the summary
  
  Summary = c(Summary, list(list(MeanTestStatistic = mean(Results[[2]]), Parameters = rangerParameters)))
  
}

EndTime = Sys.time()

EndTime - StartTime

# Observe the smallest test statistic value, 852332153.830964

print(paste('Smallest test statistic value : ', min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]})), sep = ''))

# Record the parameters corresponding to the smallest test statistic value

BestrangerParameters = Summary[[seq(length(Summary))[sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}) == min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}))][1]]][['Parameters']]

# Obtain the names of the parameters used

Parameters = unique(do.call(c, lapply(seq(length(Summary)), function(i){names(Summary[[i]][['Parameters']])})))

# Restrict attention to the parameters which were allowed to vary

Parameters = Parameters[!(Parameters %in% c('num.threads', 'seed'))]

# Clear all graphs

dev.off(dev.list()['RStudioGD'])

for (Parameter in Parameters)
{
  
  # Find iterations which contain the parameter in question
  
  Iterations = seq(length(Summary))[sapply(seq(length(Summary)), function(i){Parameter %in% names(Summary[[i]][['Parameters']])})]
  
  # Gather all parameter values and associated mean iteration and test statistics 
  
  ParameterSummary = do.call(rbind, lapply(Iterations, function(i){data.table(Parameter = Parameter, Value = Summary[[i]][['Parameters']][[Parameter]], MeanTestStatistic = Summary[[i]][['MeanTestStatistic']])}))
  
  # Print the value corresponding to the smallest test statistic
  
  print(paste(Parameter, ' : ', ParameterSummary$Value[ParameterSummary$MeanTestStatistic == min(ParameterSummary$MeanTestStatistic)][1], sep = ''))
  
  # Summarize to the level of the values of the parameter
  
  ParameterSummary02 = ParameterSummary[, .(Count = .N, MeanTestStatistic = mean(MeanTestStatistic)), by = 'Value']
  
  if (is.character(ParameterSummary02$Value) == FALSE)
  {
    
    # Discretize the values of the parameter
    
    ParameterSummary02$Group = Discretize(Weight = ParameterSummary02$Count, x = ParameterSummary02$Value, GroupNumber = 100)
    
    # Summarize to the level of the group
    
    ParameterSummary03 = ParameterSummary02[, .(Value = sum(Count * Value) / sum(Count), MeanTestStatistic = sum(Count * MeanTestStatistic) / sum(Count)), by = 'Group']
    
    # Plot the iteration and test statistics
    
    plot(ParameterSummary03$Value, ParameterSummary03$MeanTestStatistic, main = Parameter, xlab = Parameter, ylab = 'Test Statistic', pch = 19, col = 'green')
    
  }
  
  else
  {
    
    # Plot the iteration and test statistics
    
    barplot(ParameterSummary02$MeanTestStatistic, names.arg = ParameterSummary02$Value, main = Parameter, xlab = 'Values', ylab = 'Test Statistic', col = 'green')
    
  }
  
}

#####################################

# Define the sets from which the varying parameters for the hyper parameter optimization will be pulled

ParameterBoundList = list(
  
  num.trees = c(50, 250), # [1, Inf)
  mtry = c(100, 300), # [1, ncol(x)], sqrt(ncol(x))
  min.node.size = c(3, 15), # (0, Inf), 5
  min.bucket = c(1, 10), # (0, Inf), 1
  max.depth = c(5, 15), # [1, floor(nrow(x) / min.bucket)), NULL
  replace = c(TRUE, TRUE), # c(FALSE, TRUE), TRUE
  sample.fraction = c(.75, 1), # (0, 1), c(.632, 1)
  oob.error = c(TRUE), # TRUE
  num.threads = c(32, 32), # 2
  seed = c(667, 667) # NULL
  
)

# Initialize the summary of cross validation runs

Summary = list()

StartTime = Sys.time()

for (i in seq(1, 1000))
{
  
  # Generate parameters for ranger
  
  rangerParameters = rangerParameterGenerator(ParameterBoundList)
  
  # Explicitly set the seed
  
  rangerParameters[['seed']] = 666 + i - 1
  
  # Ensure the sample size is sufficiently large to avoid the 
  # 'sample_fraction too small, no observations sampled. Ranger will EXIT now.' error
  
  rangerParameters[['sample.fraction']] = max(rangerParameters[['sample.fraction']], .01)
  
  # Cross validate the ranger parameters
  
  Results = rangerCrossValidation(data = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE], label = DesignMatrix$Target, params = rangerParameters, NFold = 10, TrainingPercentage = .7)
  
  # Update the summary
  
  Summary = c(Summary, list(list(MeanTestStatistic = mean(Results[[2]]), Parameters = rangerParameters)))
  
}

EndTime = Sys.time()

EndTime - StartTime

# Observe the smallest test statistic value, 787811434.466339

print(paste('Smallest test statistic value : ', min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]})), sep = ''))

# Record the parameters corresponding to the smallest test statistic value

BestrangerParameters = Summary[[seq(length(Summary))[sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}) == min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}))][1]]][['Parameters']]

# Obtain the names of the parameters used

Parameters = unique(do.call(c, lapply(seq(length(Summary)), function(i){names(Summary[[i]][['Parameters']])})))

# Restrict attention to the parameters which were allowed to vary

Parameters = Parameters[!(Parameters %in% c('num.threads', 'seed'))]

# Clear all graphs

dev.off(dev.list()['RStudioGD'])

for (Parameter in Parameters)
{
  
  # Find iterations which contain the parameter in question
  
  Iterations = seq(length(Summary))[sapply(seq(length(Summary)), function(i){Parameter %in% names(Summary[[i]][['Parameters']])})]
  
  # Gather all parameter values and associated mean iteration and test statistics 
  
  ParameterSummary = do.call(rbind, lapply(Iterations, function(i){data.table(Parameter = Parameter, Value = Summary[[i]][['Parameters']][[Parameter]], MeanTestStatistic = Summary[[i]][['MeanTestStatistic']])}))
  
  # Print the value corresponding to the smallest test statistic
  
  print(paste(Parameter, ' : ', ParameterSummary$Value[ParameterSummary$MeanTestStatistic == min(ParameterSummary$MeanTestStatistic)][1], sep = ''))
  
  # Summarize to the level of the values of the parameter
  
  ParameterSummary02 = ParameterSummary[, .(Count = .N, MeanTestStatistic = mean(MeanTestStatistic)), by = 'Value']
  
  if (is.character(ParameterSummary02$Value) == FALSE)
  {
    
    # Discretize the values of the parameter
    
    ParameterSummary02$Group = Discretize(Weight = ParameterSummary02$Count, x = ParameterSummary02$Value, GroupNumber = 100)
    
    # Summarize to the level of the group
    
    ParameterSummary03 = ParameterSummary02[, .(Value = sum(Count * Value) / sum(Count), MeanTestStatistic = sum(Count * MeanTestStatistic) / sum(Count)), by = 'Group']
    
    # Plot the iteration and test statistics
    
    plot(ParameterSummary03$Value, ParameterSummary03$MeanTestStatistic, main = Parameter, xlab = Parameter, ylab = 'Test Statistic', pch = 19, col = 'green')
    
  }
  
  else
  {
    
    # Plot the iteration and test statistics
    
    barplot(ParameterSummary02$MeanTestStatistic, names.arg = ParameterSummary02$Value, main = Parameter, xlab = 'Values', ylab = 'Test Statistic', col = 'green')
    
  }
  
}

#####################################

# Define the sets from which the varying parameters for the hyper parameter optimization will be pulled

ParameterBoundList = list(
  
  num.trees = c(100, 300), # [1, Inf)
  mtry = c(50, 385), # [1, ncol(x)], sqrt(ncol(x))
  min.node.size = c(2, 10), # (0, Inf), 5
  min.bucket = c(1, 10), # (0, Inf), 1
  max.depth = c(10, 20), # [1, floor(nrow(x) / min.bucket)), NULL
  replace = c(TRUE, TRUE), # c(FALSE, TRUE), TRUE
  sample.fraction = c(.8, 1), # (0, 1), c(.632, 1)
  oob.error = c(TRUE), # TRUE
  num.threads = c(32, 32), # 2
  seed = c(667, 667) # NULL
  
)

# Initialize the summary of cross validation runs

Summary = list()

StartTime = Sys.time()

for (i in seq(1, 1000))
{
  
  # Generate parameters for ranger
  
  rangerParameters = rangerParameterGenerator(ParameterBoundList)
  
  # Explicitly set the seed
  
  rangerParameters[['seed']] = 666 + i - 1
  
  # Ensure the sample size is sufficiently large to avoid the 
  # 'sample_fraction too small, no observations sampled. Ranger will EXIT now.' error
  
  rangerParameters[['sample.fraction']] = max(rangerParameters[['sample.fraction']], .01)
  
  # Cross validate the ranger parameters
  
  Results = rangerCrossValidation(data = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE], label = DesignMatrix$Target, params = rangerParameters, NFold = 10, TrainingPercentage = .7)
  
  # Update the summary
  
  Summary = c(Summary, list(list(MeanTestStatistic = mean(Results[[2]]), Parameters = rangerParameters)))
  
}

EndTime = Sys.time()

EndTime - StartTime

# Observe the smallest test statistic value, 771382589.093395

print(paste('Smallest test statistic value : ', min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]})), sep = ''))

# Record the parameters corresponding to the smallest test statistic value

BestrangerParameters = Summary[[seq(length(Summary))[sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}) == min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}))][1]]][['Parameters']]

# Obtain the names of the parameters used

Parameters = unique(do.call(c, lapply(seq(length(Summary)), function(i){names(Summary[[i]][['Parameters']])})))

# Restrict attention to the parameters which were allowed to vary

Parameters = Parameters[!(Parameters %in% c('num.threads', 'seed'))]

# Clear all graphs

dev.off(dev.list()['RStudioGD'])

for (Parameter in Parameters)
{
  
  # Find iterations which contain the parameter in question
  
  Iterations = seq(length(Summary))[sapply(seq(length(Summary)), function(i){Parameter %in% names(Summary[[i]][['Parameters']])})]
  
  # Gather all parameter values and associated mean iteration and test statistics 
  
  ParameterSummary = do.call(rbind, lapply(Iterations, function(i){data.table(Parameter = Parameter, Value = Summary[[i]][['Parameters']][[Parameter]], MeanTestStatistic = Summary[[i]][['MeanTestStatistic']])}))
  
  # Print the value corresponding to the smallest test statistic
  
  print(paste(Parameter, ' : ', ParameterSummary$Value[ParameterSummary$MeanTestStatistic == min(ParameterSummary$MeanTestStatistic)][1], sep = ''))
  
  # Summarize to the level of the values of the parameter
  
  ParameterSummary02 = ParameterSummary[, .(Count = .N, MeanTestStatistic = mean(MeanTestStatistic)), by = 'Value']
  
  if (is.character(ParameterSummary02$Value) == FALSE)
  {
    
    # Discretize the values of the parameter
    
    ParameterSummary02$Group = Discretize(Weight = ParameterSummary02$Count, x = ParameterSummary02$Value, GroupNumber = 100)
    
    # Summarize to the level of the group
    
    ParameterSummary03 = ParameterSummary02[, .(Value = sum(Count * Value) / sum(Count), MeanTestStatistic = sum(Count * MeanTestStatistic) / sum(Count)), by = 'Group']
    
    # Plot the iteration and test statistics
    
    plot(ParameterSummary03$Value, ParameterSummary03$MeanTestStatistic, main = Parameter, xlab = Parameter, ylab = 'Test Statistic', pch = 19, col = 'green')
    
  }
  
  else
  {
    
    # Plot the iteration and test statistics
    
    barplot(ParameterSummary02$MeanTestStatistic, names.arg = ParameterSummary02$Value, main = Parameter, xlab = 'Values', ylab = 'Test Statistic', col = 'green')
    
  }
  
}

#####################################

# Define the sets from which the varying parameters for the hyper parameter optimization will be pulled

ParameterBoundList = list(
  
  num.trees = c(150, 350), # [1, Inf)
  mtry = c(100, 300), # [1, ncol(x)], sqrt(ncol(x))
  min.node.size = c(3, 10), # (0, Inf), 5
  min.bucket = c(1, 5), # (0, Inf), 1
  max.depth = c(15, 25), # [1, floor(nrow(x) / min.bucket)), NULL
  replace = c(TRUE, TRUE), # c(FALSE, TRUE), TRUE
  sample.fraction = c(.8, 1), # (0, 1), c(.632, 1)
  oob.error = c(TRUE), # TRUE
  num.threads = c(32, 32), # 2
  seed = c(667, 667) # NULL
  
)

# Initialize the summary of cross validation runs

Summary = list()

StartTime = Sys.time()

for (i in seq(1, 1000))
{
  
  # Generate parameters for ranger
  
  rangerParameters = rangerParameterGenerator(ParameterBoundList)
  
  # Explicitly set the seed
  
  rangerParameters[['seed']] = 666 + i - 1
  
  # Ensure the sample size is sufficiently large to avoid the 
  # 'sample_fraction too small, no observations sampled. Ranger will EXIT now.' error
  
  rangerParameters[['sample.fraction']] = max(rangerParameters[['sample.fraction']], .01)
  
  # Cross validate the ranger parameters
  
  Results = rangerCrossValidation(data = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE], label = DesignMatrix$Target, params = rangerParameters, NFold = 10, TrainingPercentage = .7)
  
  # Update the summary
  
  Summary = c(Summary, list(list(MeanTestStatistic = mean(Results[[2]]), Parameters = rangerParameters)))
  
}

EndTime = Sys.time()

EndTime - StartTime

# Observe the smallest test statistic value, 742430745.132503

print(paste('Smallest test statistic value : ', min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]})), sep = ''))

# Record the parameters corresponding to the smallest test statistic value

BestrangerParameters = Summary[[seq(length(Summary))[sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}) == min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}))][1]]][['Parameters']]

# Obtain the names of the parameters used

Parameters = unique(do.call(c, lapply(seq(length(Summary)), function(i){names(Summary[[i]][['Parameters']])})))

# Restrict attention to the parameters which were allowed to vary

Parameters = Parameters[!(Parameters %in% c('num.threads', 'seed'))]

# Clear all graphs

dev.off(dev.list()['RStudioGD'])

for (Parameter in Parameters)
{
  
  # Find iterations which contain the parameter in question
  
  Iterations = seq(length(Summary))[sapply(seq(length(Summary)), function(i){Parameter %in% names(Summary[[i]][['Parameters']])})]
  
  # Gather all parameter values and associated mean iteration and test statistics 
  
  ParameterSummary = do.call(rbind, lapply(Iterations, function(i){data.table(Parameter = Parameter, Value = Summary[[i]][['Parameters']][[Parameter]], MeanTestStatistic = Summary[[i]][['MeanTestStatistic']])}))
  
  # Print the value corresponding to the smallest test statistic
  
  print(paste(Parameter, ' : ', ParameterSummary$Value[ParameterSummary$MeanTestStatistic == min(ParameterSummary$MeanTestStatistic)][1], sep = ''))
  
  # Summarize to the level of the values of the parameter
  
  ParameterSummary02 = ParameterSummary[, .(Count = .N, MeanTestStatistic = mean(MeanTestStatistic)), by = 'Value']
  
  if (is.character(ParameterSummary02$Value) == FALSE)
  {
    
    # Discretize the values of the parameter
    
    ParameterSummary02$Group = Discretize(Weight = ParameterSummary02$Count, x = ParameterSummary02$Value, GroupNumber = 100)
    
    # Summarize to the level of the group
    
    ParameterSummary03 = ParameterSummary02[, .(Value = sum(Count * Value) / sum(Count), MeanTestStatistic = sum(Count * MeanTestStatistic) / sum(Count)), by = 'Group']
    
    # Plot the iteration and test statistics
    
    plot(ParameterSummary03$Value, ParameterSummary03$MeanTestStatistic, main = Parameter, xlab = Parameter, ylab = 'Test Statistic', pch = 19, col = 'green')
    
  }
  
  else
  {
    
    # Plot the iteration and test statistics
    
    barplot(ParameterSummary02$MeanTestStatistic, names.arg = ParameterSummary02$Value, main = Parameter, xlab = 'Values', ylab = 'Test Statistic', col = 'green')
    
  }
  
}

#####################################

# Define the sets from which the varying parameters for the hyper parameter optimization will be pulled

ParameterBoundList = list(
  
  num.trees = c(200, 400), # [1, Inf)
  mtry = c(50, 200), # [1, ncol(x)], sqrt(ncol(x))
  min.node.size = c(3, 10), # (0, Inf), 5
  min.bucket = c(1, 5), # (0, Inf), 1
  max.depth = c(10, 30), # [1, floor(nrow(x) / min.bucket)), NULL
  replace = c(TRUE, TRUE), # c(FALSE, TRUE), TRUE
  sample.fraction = c(.9, 1), # (0, 1), c(.632, 1)
  oob.error = c(TRUE), # TRUE
  num.threads = c(32, 32), # 2
  seed = c(667, 667) # NULL
  
)

# Initialize the summary of cross validation runs

Summary = list()

StartTime = Sys.time()

for (i in seq(1, 1000))
{
  
  # Generate parameters for ranger
  
  rangerParameters = rangerParameterGenerator(ParameterBoundList)
  
  # Explicitly set the seed
  
  rangerParameters[['seed']] = 666 + i - 1
  
  # Ensure the sample size is sufficiently large to avoid the 
  # 'sample_fraction too small, no observations sampled. Ranger will EXIT now.' error
  
  rangerParameters[['sample.fraction']] = max(rangerParameters[['sample.fraction']], .01)
  
  # Cross validate the ranger parameters
  
  Results = rangerCrossValidation(data = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE], label = DesignMatrix$Target, params = rangerParameters, NFold = 10, TrainingPercentage = .7)
  
  # Update the summary
  
  Summary = c(Summary, list(list(MeanTestStatistic = mean(Results[[2]]), Parameters = rangerParameters)))
  
}

EndTime = Sys.time()

EndTime - StartTime

# Observe the smallest test statistic value, 736374585.856561

print(paste('Smallest test statistic value : ', min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]})), sep = ''))

# Record the parameters corresponding to the smallest test statistic value

BestrangerParameters = Summary[[seq(length(Summary))[sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}) == min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}))][1]]][['Parameters']]

# Obtain the names of the parameters used

Parameters = unique(do.call(c, lapply(seq(length(Summary)), function(i){names(Summary[[i]][['Parameters']])})))

# Restrict attention to the parameters which were allowed to vary

Parameters = Parameters[!(Parameters %in% c('num.threads', 'seed'))]

# Clear all graphs

dev.off(dev.list()['RStudioGD'])

for (Parameter in Parameters)
{
  
  # Find iterations which contain the parameter in question
  
  Iterations = seq(length(Summary))[sapply(seq(length(Summary)), function(i){Parameter %in% names(Summary[[i]][['Parameters']])})]
  
  # Gather all parameter values and associated mean iteration and test statistics 
  
  ParameterSummary = do.call(rbind, lapply(Iterations, function(i){data.table(Parameter = Parameter, Value = Summary[[i]][['Parameters']][[Parameter]], MeanTestStatistic = Summary[[i]][['MeanTestStatistic']])}))
  
  # Print the value corresponding to the smallest test statistic
  
  print(paste(Parameter, ' : ', ParameterSummary$Value[ParameterSummary$MeanTestStatistic == min(ParameterSummary$MeanTestStatistic)][1], sep = ''))
  
  # Summarize to the level of the values of the parameter
  
  ParameterSummary02 = ParameterSummary[, .(Count = .N, MeanTestStatistic = mean(MeanTestStatistic)), by = 'Value']
  
  if (is.character(ParameterSummary02$Value) == FALSE)
  {
    
    # Discretize the values of the parameter
    
    ParameterSummary02$Group = Discretize(Weight = ParameterSummary02$Count, x = ParameterSummary02$Value, GroupNumber = 100)
    
    # Summarize to the level of the group
    
    ParameterSummary03 = ParameterSummary02[, .(Value = sum(Count * Value) / sum(Count), MeanTestStatistic = sum(Count * MeanTestStatistic) / sum(Count)), by = 'Group']
    
    # Plot the iteration and test statistics
    
    plot(ParameterSummary03$Value, ParameterSummary03$MeanTestStatistic, main = Parameter, xlab = Parameter, ylab = 'Test Statistic', pch = 19, col = 'green')
    
  }
  
  else
  {
    
    # Plot the iteration and test statistics
    
    barplot(ParameterSummary02$MeanTestStatistic, names.arg = ParameterSummary02$Value, main = Parameter, xlab = 'Values', ylab = 'Test Statistic', col = 'green')
    
  }
  
}

#####################################

# Define the sets from which the varying parameters for the hyper parameter optimization will be pulled

ParameterBoundList = list(
  
  num.trees = c(250, 450), # [1, Inf)
  mtry = c(25, 175), # [1, ncol(x)], sqrt(ncol(x))
  min.node.size = c(3, 10), # (0, Inf), 5
  min.bucket = c(1, 5), # (0, Inf), 1
  max.depth = c(10, 30), # [1, floor(nrow(x) / min.bucket)), NULL
  replace = c(TRUE, TRUE), # c(FALSE, TRUE), TRUE
  sample.fraction = c(.95, 1), # (0, 1), c(.632, 1)
  oob.error = c(TRUE), # TRUE
  num.threads = c(32, 32), # 2
  seed = c(667, 667) # NULL
  
)

# Initialize the summary of cross validation runs

Summary = list()

StartTime = Sys.time()

for (i in seq(1, 1000))
{
  
  # Generate parameters for ranger
  
  rangerParameters = rangerParameterGenerator(ParameterBoundList)
  
  # Explicitly set the seed
  
  rangerParameters[['seed']] = 666 + i - 1
  
  # Ensure the sample size is sufficiently large to avoid the 
  # 'sample_fraction too small, no observations sampled. Ranger will EXIT now.' error
  
  rangerParameters[['sample.fraction']] = max(rangerParameters[['sample.fraction']], .01)
  
  # Cross validate the ranger parameters
  
  Results = rangerCrossValidation(data = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE], label = DesignMatrix$Target, params = rangerParameters, NFold = 10, TrainingPercentage = .7)
  
  # Update the summary
  
  Summary = c(Summary, list(list(MeanTestStatistic = mean(Results[[2]]), Parameters = rangerParameters)))
  
}

EndTime = Sys.time()

EndTime - StartTime

# Observe the smallest test statistic value, 729190058.135476

print(paste('Smallest test statistic value : ', min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]})), sep = ''))

# Record the parameters corresponding to the smallest test statistic value

BestrangerParameters = Summary[[seq(length(Summary))[sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}) == min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}))][1]]][['Parameters']]

# Obtain the names of the parameters used

Parameters = unique(do.call(c, lapply(seq(length(Summary)), function(i){names(Summary[[i]][['Parameters']])})))

# Restrict attention to the parameters which were allowed to vary

Parameters = Parameters[!(Parameters %in% c('num.threads', 'seed'))]

# Clear all graphs

dev.off(dev.list()['RStudioGD'])

for (Parameter in Parameters)
{
  
  # Find iterations which contain the parameter in question
  
  Iterations = seq(length(Summary))[sapply(seq(length(Summary)), function(i){Parameter %in% names(Summary[[i]][['Parameters']])})]
  
  # Gather all parameter values and associated mean iteration and test statistics 
  
  ParameterSummary = do.call(rbind, lapply(Iterations, function(i){data.table(Parameter = Parameter, Value = Summary[[i]][['Parameters']][[Parameter]], MeanTestStatistic = Summary[[i]][['MeanTestStatistic']])}))
  
  # Print the value corresponding to the smallest test statistic
  
  print(paste(Parameter, ' : ', ParameterSummary$Value[ParameterSummary$MeanTestStatistic == min(ParameterSummary$MeanTestStatistic)][1], sep = ''))
  
  # Summarize to the level of the values of the parameter
  
  ParameterSummary02 = ParameterSummary[, .(Count = .N, MeanTestStatistic = mean(MeanTestStatistic)), by = 'Value']
  
  if (is.character(ParameterSummary02$Value) == FALSE)
  {
    
    # Discretize the values of the parameter
    
    ParameterSummary02$Group = Discretize(Weight = ParameterSummary02$Count, x = ParameterSummary02$Value, GroupNumber = 100)
    
    # Summarize to the level of the group
    
    ParameterSummary03 = ParameterSummary02[, .(Value = sum(Count * Value) / sum(Count), MeanTestStatistic = sum(Count * MeanTestStatistic) / sum(Count)), by = 'Group']
    
    # Plot the iteration and test statistics
    
    plot(ParameterSummary03$Value, ParameterSummary03$MeanTestStatistic, main = Parameter, xlab = Parameter, ylab = 'Test Statistic', pch = 19, col = 'green')
    
  }
  
  else
  {
    
    # Plot the iteration and test statistics
    
    barplot(ParameterSummary02$MeanTestStatistic, names.arg = ParameterSummary02$Value, main = Parameter, xlab = 'Values', ylab = 'Test Statistic', col = 'green')
    
  }
  
}

#####################################

# Define the sets from which the varying parameters for the hyper parameter optimization will be pulled

ParameterBoundList = list(
  
  num.trees = c(300, 500), # [1, Inf)
  mtry = c(75, 175), # [1, ncol(x)], sqrt(ncol(x))
  min.node.size = c(3, 10), # (0, Inf), 5
  min.bucket = c(1, 5), # (0, Inf), 1
  max.depth = c(10, 40), # [1, floor(nrow(x) / min.bucket)), NULL
  replace = c(TRUE, TRUE), # c(FALSE, TRUE), TRUE
  sample.fraction = c(.95, 1), # (0, 1), c(.632, 1)
  oob.error = c(TRUE), # TRUE
  num.threads = c(32, 32), # 2
  seed = c(667, 667) # NULL
  
)

# Initialize the summary of cross validation runs

Summary = list()

StartTime = Sys.time()

for (i in seq(1, 1000))
{
  
  # Generate parameters for ranger
  
  rangerParameters = rangerParameterGenerator(ParameterBoundList)
  
  # Explicitly set the seed
  
  rangerParameters[['seed']] = 666 + i - 1
  
  # Ensure the sample size is sufficiently large to avoid the 
  # 'sample_fraction too small, no observations sampled. Ranger will EXIT now.' error
  
  rangerParameters[['sample.fraction']] = max(rangerParameters[['sample.fraction']], .01)
  
  # Cross validate the ranger parameters
  
  Results = rangerCrossValidation(data = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE], label = DesignMatrix$Target, params = rangerParameters, NFold = 10, TrainingPercentage = .7)
  
  # Update the summary
  
  Summary = c(Summary, list(list(MeanTestStatistic = mean(Results[[2]]), Parameters = rangerParameters)))
  
}

EndTime = Sys.time()

EndTime - StartTime

# Observe the smallest test statistic value, 712371336.360716

print(paste('Smallest test statistic value : ', min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]})), sep = ''))

# Record the parameters corresponding to the smallest test statistic value

BestrangerParameters = Summary[[seq(length(Summary))[sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}) == min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}))][1]]][['Parameters']]

# Obtain the names of the parameters used

Parameters = unique(do.call(c, lapply(seq(length(Summary)), function(i){names(Summary[[i]][['Parameters']])})))

# Restrict attention to the parameters which were allowed to vary

Parameters = Parameters[!(Parameters %in% c('num.threads', 'seed'))]

# Clear all graphs

dev.off(dev.list()['RStudioGD'])

for (Parameter in Parameters)
{
  
  # Find iterations which contain the parameter in question
  
  Iterations = seq(length(Summary))[sapply(seq(length(Summary)), function(i){Parameter %in% names(Summary[[i]][['Parameters']])})]
  
  # Gather all parameter values and associated mean iteration and test statistics 
  
  ParameterSummary = do.call(rbind, lapply(Iterations, function(i){data.table(Parameter = Parameter, Value = Summary[[i]][['Parameters']][[Parameter]], MeanTestStatistic = Summary[[i]][['MeanTestStatistic']])}))
  
  # Print the value corresponding to the smallest test statistic
  
  print(paste(Parameter, ' : ', ParameterSummary$Value[ParameterSummary$MeanTestStatistic == min(ParameterSummary$MeanTestStatistic)][1], sep = ''))
  
  # Summarize to the level of the values of the parameter
  
  ParameterSummary02 = ParameterSummary[, .(Count = .N, MeanTestStatistic = mean(MeanTestStatistic)), by = 'Value']
  
  if (is.character(ParameterSummary02$Value) == FALSE)
  {
    
    # Discretize the values of the parameter
    
    ParameterSummary02$Group = Discretize(Weight = ParameterSummary02$Count, x = ParameterSummary02$Value, GroupNumber = 100)
    
    # Summarize to the level of the group
    
    ParameterSummary03 = ParameterSummary02[, .(Value = sum(Count * Value) / sum(Count), MeanTestStatistic = sum(Count * MeanTestStatistic) / sum(Count)), by = 'Group']
    
    # Plot the iteration and test statistics
    
    plot(ParameterSummary03$Value, ParameterSummary03$MeanTestStatistic, main = Parameter, xlab = Parameter, ylab = 'Test Statistic', pch = 19, col = 'green')
    
  }
  
  else
  {
    
    # Plot the iteration and test statistics
    
    barplot(ParameterSummary02$MeanTestStatistic, names.arg = ParameterSummary02$Value, main = Parameter, xlab = 'Values', ylab = 'Test Statistic', col = 'green')
    
  }
  
}

#####################################

# Define the sets from which the varying parameters for the hyper parameter optimization will be pulled

ParameterBoundList = list(
  
  num.trees = c(350, 550), # [1, Inf)
  mtry = c(100, 200), # [1, ncol(x)], sqrt(ncol(x))
  min.node.size = c(3, 10), # (0, Inf), 5
  min.bucket = c(1, 4), # (0, Inf), 1
  max.depth = c(10, 40), # [1, floor(nrow(x) / min.bucket)), NULL
  replace = c(TRUE, TRUE), # c(FALSE, TRUE), TRUE
  sample.fraction = c(.95, 1), # (0, 1), c(.632, 1)
  oob.error = c(TRUE), # TRUE
  num.threads = c(32, 32), # 2
  seed = c(667, 667) # NULL
  
)

# Initialize the summary of cross validation runs

Summary = list()

StartTime = Sys.time()

for (i in seq(1, 1000))
{
  
  # Generate parameters for ranger
  
  rangerParameters = rangerParameterGenerator(ParameterBoundList)
  
  # Explicitly set the seed
  
  rangerParameters[['seed']] = 666 + i - 1
  
  # Ensure the sample size is sufficiently large to avoid the 
  # 'sample_fraction too small, no observations sampled. Ranger will EXIT now.' error
  
  rangerParameters[['sample.fraction']] = max(rangerParameters[['sample.fraction']], .01)
  
  # Cross validate the ranger parameters
  
  Results = rangerCrossValidation(data = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE], label = DesignMatrix$Target, params = rangerParameters, NFold = 10, TrainingPercentage = .7)
  
  # Update the summary
  
  Summary = c(Summary, list(list(MeanTestStatistic = mean(Results[[2]]), Parameters = rangerParameters)))
  
}

EndTime = Sys.time()

EndTime - StartTime

# Observe the smallest test statistic value, 710427566.826557

print(paste('Smallest test statistic value : ', min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]})), sep = ''))

# Record the parameters corresponding to the smallest test statistic value

BestrangerParameters = Summary[[seq(length(Summary))[sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}) == min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]}))][1]]][['Parameters']]

# Obtain the names of the parameters used

Parameters = unique(do.call(c, lapply(seq(length(Summary)), function(i){names(Summary[[i]][['Parameters']])})))

# Restrict attention to the parameters which were allowed to vary

Parameters = Parameters[!(Parameters %in% c('num.threads', 'seed'))]

# Clear all graphs

dev.off(dev.list()['RStudioGD'])

for (Parameter in Parameters)
{
  
  # Find iterations which contain the parameter in question
  
  Iterations = seq(length(Summary))[sapply(seq(length(Summary)), function(i){Parameter %in% names(Summary[[i]][['Parameters']])})]
  
  # Gather all parameter values and associated mean iteration and test statistics 
  
  ParameterSummary = do.call(rbind, lapply(Iterations, function(i){data.table(Parameter = Parameter, Value = Summary[[i]][['Parameters']][[Parameter]], MeanTestStatistic = Summary[[i]][['MeanTestStatistic']])}))
  
  # Print the value corresponding to the smallest test statistic
  
  print(paste(Parameter, ' : ', ParameterSummary$Value[ParameterSummary$MeanTestStatistic == min(ParameterSummary$MeanTestStatistic)][1], sep = ''))
  
  # Summarize to the level of the values of the parameter
  
  ParameterSummary02 = ParameterSummary[, .(Count = .N, MeanTestStatistic = mean(MeanTestStatistic)), by = 'Value']
  
  if (is.character(ParameterSummary02$Value) == FALSE)
  {
    
    # Discretize the values of the parameter
    
    ParameterSummary02$Group = Discretize(Weight = ParameterSummary02$Count, x = ParameterSummary02$Value, GroupNumber = 100)
    
    # Summarize to the level of the group
    
    ParameterSummary03 = ParameterSummary02[, .(Value = sum(Count * Value) / sum(Count), MeanTestStatistic = sum(Count * MeanTestStatistic) / sum(Count)), by = 'Group']
    
    # Plot the iteration and test statistics
    
    plot(ParameterSummary03$Value, ParameterSummary03$MeanTestStatistic, main = Parameter, xlab = Parameter, ylab = 'Test Statistic', pch = 19, col = 'green')
    
  }
  
  else
  {
    
    # Plot the iteration and test statistics
    
    barplot(ParameterSummary02$MeanTestStatistic, names.arg = ParameterSummary02$Value, main = Parameter, xlab = 'Values', ylab = 'Test Statistic', col = 'green')
    
  }
  
}

#####################################

# Define the sets from which the varying parameters for the hyper parameter optimization will be pulled

ParameterBoundList = list(
  
  num.trees = c(400, 600), # [1, Inf)
  mtry = c(90, 190), # [1, ncol(x)], sqrt(ncol(x))
  min.node.size = c(3, 10), # (0, Inf), 5
  min.bucket = c(1, 4), # (0, Inf), 1
  max.depth = c(5, 45), # [1, floor(nrow(x) / min.bucket)), NULL
  replace = c(TRUE, TRUE), # c(FALSE, TRUE), TRUE
  sample.fraction = c(.95, 1), # (0, 1), c(.632, 1)
  oob.error = c(TRUE), # TRUE
  num.threads = c(32, 32), # 2
  seed = c(667, 667) # NULL
  
)

# Initialize the summary of cross validation runs

Summary = list()

StartTime = Sys.time()

for (i in seq(1, 1000))
{
  
  # Generate parameters for ranger
  
  rangerParameters = rangerParameterGenerator(ParameterBoundList)
  
  # Explicitly set the seed
  
  rangerParameters[['seed']] = 666 + i - 1
  
  # Ensure the sample size is sufficiently large to avoid the 
  # 'sample_fraction too small, no observations sampled. Ranger will EXIT now.' error
  
  rangerParameters[['sample.fraction']] = max(rangerParameters[['sample.fraction']], .01)
  
  # Cross validate the ranger parameters
  
  Results = rangerCrossValidation(data = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE], label = DesignMatrix$Target, params = rangerParameters, NFold = 10, TrainingPercentage = .7)
  
  # Update the summary
  
  Summary = c(Summary, list(list(MeanTestStatistic = mean(Results[[2]]), Parameters = rangerParameters)))
  
}

EndTime = Sys.time()

EndTime - StartTime

# Observe the smallest test statistic value, 713809417.235317

print(paste('Smallest test statistic value : ', min(sapply(seq(length(Summary)), function(i){Summary[[i]][['MeanTestStatistic']]})), sep = ''))

# No new modeling will take place since there is no obvious set of parameters to attempt

# Recover the mean associated to the best hyperparameters

mean(rangerCrossValidation(data = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE], label = DesignMatrix$Target, params = BestrangerParameters, NFold = 10, TrainingPercentage = .7)[['prediction.error']])

# Create the model using the tuned hyperparameters

Model = ranger(
  
  y = DesignMatrix$Target, x = DesignMatrix[, names(DesignMatrix)[!(names(DesignMatrix) %in% c('(Intercept)', 'Target'))], with = FALSE],
  case.weights = TrainTestVector,
  holdout = TRUE,
  num.trees = BestrangerParameters[['num.trees']],
  mtry = BestrangerParameters[['mtry']],
  importance = 'impurity',
  min.node.size = BestrangerParameters[['min.node.size']],
  min.bucket = BestrangerParameters[['min.bucket']],
  max.depth = BestrangerParameters[['max.depth']],
  replace = BestrangerParameters[['replace']],
  sample.fraction = BestrangerParameters[['sample.fraction']],
  splitrule = 'variance',
  oob.error = BestrangerParameters[['oob.error']],
  num.threads = BestrangerParameters[['num.threads']],
  seed = BestrangerParameters[['seed']]
  
)

# Recover the errors

Model[['prediction.error']]
sum((Model[['predictions']][!(TrainTestVector)] - DesignMatrix$Target[!(TrainTestVector)])^2) / sum(!(TrainTestVector))

# Apply the model on the design matrix

DesignMatrix$ranger = predict(Model, DesignMatrix)[['predictions']]

# Apply the model to all of the data on the standardized data.  Observe that this only works if the standardized
# data has the same sort order as the design matrix

StandardizedData$ranger = predict(Model, DesignMatrix)[['predictions']]

# Recover the errors

sum((DesignMatrix$Target[!(TrainTestVector)] - DesignMatrix$ranger[!(TrainTestVector)])^2) / sum(!(TrainTestVector))
sum((StandardizedData$Target[!(TrainTestVector)] - StandardizedData$ranger[!(TrainTestVector)])^2) / sum(!(TrainTestVector))

# Write the model to the disk

fwrite(x = StandardizedData[, c(Key, 'ranger'), with = FALSE], file = paste(OutputDirectory, '/rangerModel.csv', sep = ''))

# Write the model parameters to the disk

fwrite(x = as.data.table(t(sapply(seq(length(BestrangerParameters)), function(i){data.table(Parameter = names(BestrangerParameters)[i], Value = BestrangerParameters[[i]])}))), file = paste(OutputDirectory, '/rangerModelParameters.csv', sep = ''))