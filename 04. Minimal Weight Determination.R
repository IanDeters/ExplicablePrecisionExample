# Observe the relevant directories

DependsOnDirectory
MethodsDirectory
InputDirectory
OutputDirectory

# Create the function vector

FunctionVector = c('data.table', 'IncreasingGroupSearch', 'BreakPointApplication', 'MinimumWeightImposition', 'ProcessCompletionText')

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

# Read in the meta data

Target = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'Target')
Target = Target[1, 1]

Key = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'Key')
Key = Key[, 1]

WeightVariable = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'WeightVariable')
WeightVariable = WeightVariable[, 1]

# Read in the machine learning models

xgboostModel = fread(paste(InputDirectory, '/xgboostModel.csv', sep = ''))

nrow(StandardizedData)
ncol(StandardizedData) + 1

StandardizedData = merge(StandardizedData, xgboostModel, by = c(Key))

nrow(StandardizedData)
ncol(StandardizedData)

ModelVariables = names(xgboostModel)[!(names(xgboostModel) %in% Key)]

rm(xgboostModel)
gc()

rangerModel = fread(paste(InputDirectory, '/rangerModel.csv', sep = ''))

nrow(StandardizedData)
ncol(StandardizedData) + 1

StandardizedData = merge(StandardizedData, rangerModel, by = c(Key))

nrow(StandardizedData)
ncol(StandardizedData)

ModelVariables = c(ModelVariables, names(rangerModel)[!(names(rangerModel) %in% Key)])

rm(rangerModel)
gc()

# Create the training / testing split

set.seed(666)

TrainTestVector = sample(x = c(TRUE, FALSE), size = nrow(StandardizedData), replace = TRUE, prob = c(.7, .3))

# Create an ensemble model using the machine learning models

EnsembleModel = glm(formula = as.formula(paste('Target ~ ', paste(ModelVariables, collapse = ' + '), sep = '')), data = StandardizedData[, c('Target', ModelVariables), with = FALSE], family = gaussian(link = 'identity'), subset = TrainTestVector)

StandardizedData$EnsembleModel = predict(EnsembleModel, StandardizedData)

# Update the set of model variables

ModelVariables = c(ModelVariables, 'EnsembleModel')

# Initialize the summary

Summary = data.table()

for (ModelVariable in ModelVariables)
{
  
  # Explicitly create the training / testing data
  
  TrainingData = StandardizedData[TrainTestVector, c('Weight', 'Target', ModelVariable), with = FALSE]
  TestingData = StandardizedData[!(TrainTestVector), c('Weight', 'Target', ModelVariable), with = FALSE]
  
  # Create groups on the basis of model variables
  
  TrainingData$Group = IncreasingGroupSearch(Weight = TrainingData$Weight, Target = TrainingData$Target, OrderingVariable = TrainingData[[ModelVariable]], Direction = 1, Tolerance = .0001, MethodList = c('L-BFGS-B', 'BFGS', 'CG'), DiscretizationVector = seq(10, 1000, by = 10), RawDiscrete = TRUE, Cluster = NULL)[['Grouping']]
  
  # Create the breakpoints and average target values to impute
  
  BreakPoints = TrainingData[order(TrainingData$Group), .(Model = max(get(ModelVariable)), Weight = sum(Weight), GroupedTarget = sum(Weight * Target) / sum(Weight)), by = 'Group']

  # Initialize the looping condition
  
  LoopingCondition = TRUE
  
  while (LoopingCondition == TRUE)
  {
    
    # Create the groups on the testing data
    
    TestingData$Group = BreakPointApplication(BreakPoints[['Model']], TestingData[[ModelVariable]])
    
    # Merge in the average target values by group
    
    if (nrow(BreakPoints) == 1)
    {
      
      TestingData$GroupedTarget = sum(TrainingData$Target * TrainingData$Weight) / sum(TrainingData$Weight)
      
    }
    
    else
    {

      TestingData = merge(TestingData[, names(TestingData)[names(TestingData) != 'GroupedTarget'], with = FALSE], BreakPoints[, c('Group', 'GroupedTarget')], by = 'Group')
      
    }
    
    # True the means
    
    TestingData$GroupedTarget = TestingData$GroupedTarget * sum(TestingData$Weight * TestingData$Target) / sum(TestingData$Weight * TestingData$GroupedTarget)
    
    # Record the results
    
    Summary = rbind(Summary, data.table(Model = ModelVariable, Weight = min(BreakPoints[['Weight']]), Error = sum(TestingData$Weight * (TestingData$GroupedTarget - TestingData$Target)^2)))
    
    # Update the looping condition
    
    LoopingCondition = (1 < length(unique(BreakPoints$Weight)))
    
    if (LoopingCondition == TRUE)
    {
      
      # Create a new group to ensure a new minimum weight
      
      BreakPoints$Group = MinimumWeightImposition(BreakPoints$Weight, BreakPoints$GroupedTarget, min(BreakPoints$Weight[min(BreakPoints$Weight) < BreakPoints$Weight]))
      
      # Summarize to the new group
      
      BreakPoints = BreakPoints[order(BreakPoints$Group), .(Model = max(Model), Weight = sum(Weight), GroupedTarget = sum(Weight * GroupedTarget) / sum(Weight)), by = 'Group']
      
      
    }
  
  }

}

ProcessCompletionText()

# Sort by model and by weight

Summary = Summary[order(Summary$Model, Summary$Weight), ]

# Write the weight information to the disk

Workbook = createWorkbook()

for (ThisModel in unique(Summary$Model))
{
  
  addWorksheet(Workbook, ThisModel)
  writeData(Workbook, ThisModel, Summary[Summary$Model == ThisModel, ])
  
}

saveWorkbook(Workbook, paste(OutputDirectory, '/MetaData02.xlsx', sep = ''))