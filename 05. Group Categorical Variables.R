# Observe the relevant directories

DependsOnDirectory
MethodsDirectory
InputDirectory
OutputDirectory

MinimumWeight = 2

# Create the function vector

FunctionVector = c('data.table', 'IncreasingGroupSearch', 'MinimumWeightImposition', 'AddLeadingZeros', 'ProcessCompletionText', 'CreateTranslationTable')

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

OffsetVariable = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'OffsetVariable')
OffsetVariable = OffsetVariable[1, 1]

Key = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'Key')
Key = Key[, 1]

WeightVariable = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'WeightVariable')
WeightVariable = WeightVariable[, 1]

MetaData = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'MetaData')
TranslationTable = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'TranslationTable')

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

# Explicitly create the training / testing data

TrainingData = StandardizedData[TrainTestVector, ]

# Determine the largest variable number and add one

VariableNumber = max(nchar(TranslationTable$NewVariable))
VariableNumber = substr(TranslationTable$NewVariable, 1, VariableNumber - 7)
VariableNumber = max(as.numeric(substr(VariableNumber, 9, nchar(VariableNumber)))) + 1

# Determine the magnitude for the leading zeroes function

AddLeadingZerosMagnitude = 10^floor(log10(VariableNumber + length(unique(TranslationTable$NewVariable[TranslationTable$OldVariable %in% MetaData$Variable[MetaData$ConceptualType == 'Categorical']])) - 1))

# Initialize the translation table

TranslationTable03 = data.table()

for (ThisVariable in unique(TranslationTable$NewVariable[TranslationTable$OldVariable %in% MetaData$Variable[MetaData$ConceptualType == 'Categorical']]))
{
  
  # Summarize to the level of the variable where it is populated
  
  VariableSummary = TrainingData[TrainingData[[ThisVariable]] != 0, .(Weight = sum(Weight), Target = sum(Target) / sum(Weight), EnsembleModel = sum(EnsembleModel) / sum(Weight), ranger = sum(ranger) / sum(Weight), xgboost = sum(xgboost) / sum(Weight)), by = c(ThisVariable)]

  # Initialize the summary
  
  Summary = data.table()
    
  for (ModelVariable in ModelVariables)
  {
    
    # Create groups on the basis of model variables
    
    VariableSummary$Group = IncreasingGroupSearch(Weight = VariableSummary$Weight, Target = VariableSummary$Target, OrderingVariable = VariableSummary[[ModelVariable]], Direction = 1, Tolerance = .0001, MethodList = c('L-BFGS-B', 'BFGS', 'CG'), DiscretizationVector = seq(10, 1000, by = 10), RawDiscrete = TRUE, Cluster = NULL)[['Grouping']]
    
    # Sort by group
    
    VariableSummary = VariableSummary[order(VariableSummary$Group), ]
    
    # Apply the minimum weight
    
    VariableSummary$Group = MinimumWeightImposition(VariableSummary$Weight, VariableSummary$Target, MinimumWeight)
    
    # Merge in the average target value per group
    
    VariableSummary = merge(VariableSummary[, names(VariableSummary)[names(VariableSummary) != 'GroupedTarget'], with = FALSE], VariableSummary[, .(GroupedTarget = sum(Weight * Target) / sum(Weight)), by = 'Group'], by = 'Group')

    # Record the error
    
    Summary = rbind(Summary, data.table(ModelVariable = ModelVariable, Error = sum(VariableSummary$Weight * (VariableSummary$Target - VariableSummary$GroupedTarget)^2)))

    # Update the group name
    
    names(VariableSummary)[names(VariableSummary) == 'Group'] = paste(ModelVariable, 'Group', sep = '')

  }
  
  # Determine which model yielded the best error
  
  ModelVariable = Summary$ModelVariable[Summary$Error == min(Summary$Error)][1]

  # Update the variable name

  names(VariableSummary)[names(VariableSummary) == paste(ModelVariable, 'Group', sep = '')] = paste('Variable', AddLeadingZeros(VariableNumber, AddLeadingZerosMagnitude), sep = '')

  # Update the translation table
    
  TranslationTable03 = rbind(TranslationTable03, data.table(OldVariable = ThisVariable, NewVariable = paste('Variable', AddLeadingZeros(VariableNumber, AddLeadingZerosMagnitude), sep = '')))

  # Merge in the new variable
    
  StandardizedData = merge(StandardizedData, VariableSummary[, c(ThisVariable, paste('Variable', AddLeadingZeros(VariableNumber, AddLeadingZerosMagnitude), sep = '')), with = FALSE], by = c(ThisVariable), all.x = TRUE)

  # Attend to the missing values
  
  StandardizedData[[paste('Variable', AddLeadingZeros(VariableNumber, AddLeadingZerosMagnitude), sep = '')]][StandardizedData[[ThisVariable]] == 0] = 0
  
  # Update the variable number
  
  VariableNumber = VariableNumber + 1

}

ProcessCompletionText()

# Observe that the dimensions cohere

length(unique(TranslationTable$NewVariable[TranslationTable$OldVariable %in% MetaData$Variable[MetaData$ConceptualType == 'Categorical']]))
nrow(TranslationTable03)

# Read in the original data for purposes of manual grouping

DataSet = fread(paste(InputDirectory, '/train.csv', sep = ''))

# Observe where there might be missing values due to values present only in the testing data

for (i in seq(nrow(TranslationTable03)))
{
  
  # Check if there are missing values
  
  if (as.logical(1 - prod(is.na(StandardizedData[[TranslationTable03[['NewVariable']][i]]]) == FALSE)) == TRUE)
  {
    
    # Combine with the original data so as to apply subject matter expertise
    
    Comparison = unique(merge(DataSet[, c(Key, TranslationTable$OldVariable[TranslationTable[['NewVariable']] == TranslationTable03[['OldVariable']][i]]), with = FALSE], StandardizedData[, c(Key, TranslationTable03[['NewVariable']][i]), with = FALSE], by = c(Key))[, c(TranslationTable$OldVariable[TranslationTable[['NewVariable']] == TranslationTable03[['OldVariable']][i]], TranslationTable03[['NewVariable']][i]), with = FALSE])
    print(Comparison[order(Comparison[[2]]), ])

  }

}

# Attend to the missing cases

StandardizedData[['Variable093']][is.na(StandardizedData[['Variable093']]) == TRUE] = 1
StandardizedData[['Variable094']][is.na(StandardizedData[['Variable094']]) == TRUE] = 1
StandardizedData[['Variable101']][is.na(StandardizedData[['Variable101']]) == TRUE] = 1
StandardizedData[['Variable117']][is.na(StandardizedData[['Variable117']]) == TRUE] = 2

# Determine the number of new variables which take only one value

NumberOfVariablesToRemove = sum(sapply(seq(nrow(TranslationTable03)), function(i){length(unique(StandardizedData[[TranslationTable03$NewVariable[i]]])) == 1}))

ncol(StandardizedData) - NumberOfVariablesToRemove
nrow(TranslationTable03) - NumberOfVariablesToRemove

for (Variable in TranslationTable03$NewVariable)
{
  
  if (length(unique(StandardizedData[[Variable]])) == 1)
  {
    
    # Remove the variable from the standardized data
    
    StandardizedData = StandardizedData[, names(StandardizedData)[names(StandardizedData) != Variable], with = FALSE]
    
    # Remove the variable from the translation table
    
    TranslationTable03 = TranslationTable03[TranslationTable03$NewVariable != Variable, ]
    
  }
  
  # Otherwise, declare the variable as a factor
  
  else
  {
    
    StandardizedData[[Variable]] = as.factor(StandardizedData[[Variable]])
    
  }

}

ncol(StandardizedData)
nrow(TranslationTable03)

# Normalize the target variable

StandardizedData$NormalizedTarget = StandardizedData$Target / StandardizedData$Weight

# Sort the data before creating the design matrix

StandardizedData = StandardizedData[order(StandardizedData[[Key]]), ]

# Create a formula to create the design matrix

DesignMatrixFormula = as.formula(paste('~ NormalizedTarget + Weight + Offset + ', paste(c(TranslationTable$NewVariable[TranslationTable$OldVariable %in% MetaData$Variable[MetaData$ConceptualType == 'Continuous']], TranslationTable03$NewVariable), collapse = ' + ')))

# Create the design matrix

nrow(StandardizedData)
1 + 3 + sum(sapply(seq(nrow(TranslationTable03)), function(i){length(unique(StandardizedData[[TranslationTable03$NewVariable[i]]])) - 1})) + length(TranslationTable$NewVariable[TranslationTable$OldVariable %in% MetaData$Variable[MetaData$ConceptualType == 'Continuous']])

DesignMatrix = as.data.table(model.matrix(DesignMatrixFormula, StandardizedData))

nrow(DesignMatrix)
ncol(DesignMatrix)

# Create the additional columns in the design matrix and the translation table

DesignMatrix02 = CreateTranslationTable(StandardizedData, DesignMatrix, c(TranslationTable$NewVariable[TranslationTable$OldVariable %in% MetaData$Variable[MetaData$ConceptualType == 'Continuous']], TranslationTable03$NewVariable)[order(c(TranslationTable$NewVariable[TranslationTable$OldVariable %in% MetaData$Variable[MetaData$ConceptualType == 'Continuous']], TranslationTable03$NewVariable))])

# Add the additional columns to the design matrix

nrow(DesignMatrix)
ncol(DesignMatrix) + nrow(TranslationTable03)

DesignMatrix = cbind(DesignMatrix, DesignMatrix02[[1]])

nrow(DesignMatrix)
ncol(DesignMatrix)

# Create the translation table for the design matrix

TranslationTable04 = DesignMatrix02[[2]]

rm(DesignMatrix02)
gc()

# Export the newly created data and its attendant meta data

fwrite(x = StandardizedData[, c(Key, TranslationTable03$NewVariable), with = FALSE], file = paste(OutputDirectory, '/StandardizedData02.csv', sep = ''))
fwrite(x = DesignMatrix, file = paste(OutputDirectory, '/DesignMatrix02.csv', sep = ''))

Workbook = createWorkbook()

addWorksheet(Workbook, 'TranslationTable03')
writeData(Workbook, 'TranslationTable03', TranslationTable03)

addWorksheet(Workbook, 'TranslationTable04')
writeData(Workbook, 'TranslationTable04', TranslationTable04)

saveWorkbook(Workbook, paste(OutputDirectory, '/MetaData03.xlsx', sep = ''))