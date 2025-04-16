# Observe the relevant directories

DependsOnDirectory
MethodsDirectory
InputDirectory
OutputDirectory

NumberOfGroups = 10

# Create the function vector

FunctionVector = c('Discretize')

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

# Import the data

DataSet = fread(paste(InputDirectory, '/Train.csv', sep = ''))
StandardizedData = fread(paste(InputDirectory, '/StandardizedData.csv', sep = ''))
StandardizedData02 = fread(paste(InputDirectory, '/StandardizedData02.csv', sep = ''))
DesignMatrix = fread(paste(InputDirectory, '/DesignMatrix03.csv', sep = ''))

# Import the parameters from the model

CrossValidatedTerms = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'CrossValidatedTerms')

# Import the model information

OriginalVariableModel = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'OriginalVariableModel')

# Determine the key variable(s)

Key = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'Key')
Key = Key[1, 1]

# Determine the target

Target = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'Target')
Target = Target[1, 1]

# Determine the weight variable

WeightVariable = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'WeightVariable')
WeightVariable = WeightVariable[, 1]

# Determine the offset variable

OffsetVariable = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'OffsetVariable')
OffsetVariable = OffsetVariable[1, 1]

# Import the translation tables

TranslationTable = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'TranslationTable')
TranslationTable03 = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData03.xlsx', sep = ''), sheet = 'TranslationTable03')
TranslationTable04 = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData03.xlsx', sep = ''), sheet = 'TranslationTable04')

# Create the glm formula

glmFormula = as.formula(paste('NormalizedTarget ~ ', paste(CrossValidatedTerms$SelectedTerm, collapse = ' + '), sep = ''))

# Create the training / testing split

set.seed(666)

TrainTestVector = sample(x = c(TRUE, FALSE), size = nrow(DesignMatrix), replace = TRUE, prob = c(.7, .3))

# Create the model on the training data

Model = glm(formula = glmFormula, family = poisson(link = 'log'), data = DesignMatrix[TrainTestVector, ], weights = Weight, offset = Offset)

# Observe the correlation of the model on the testing data

cor(DesignMatrix$NormalizedTarget[!(TrainTestVector)], predict(Model, DesignMatrix[!(TrainTestVector), ], 'response'))

# Restrict attention to terms in the model

OriginalVariableModel = OriginalVariableModel[paste('V', OriginalVariableModel$Term, sep = '') %in% CrossValidatedTerms$SelectedTerm, ]

# Merge in the variable names and values for categorical variables

OriginalVariableModel = merge(OriginalVariableModel, TranslationTable04[, c('DesignMatrixName', 'Variable', 'VariableValue')], by.x = 'Variable', by.y = 'DesignMatrixName', all.x = TRUE)

# Create the vectors of categorical and continuous variables

CategoricalVariables = unique(OriginalVariableModel$Variable.y[is.na(OriginalVariableModel$Variable.y) == FALSE])
ContinuousVariables = unique(OriginalVariableModel$Variable[is.na(OriginalVariableModel$Variable.y) == TRUE])

# Apply the model to the standardized data

StandardizedData$fitted.values = predict(Model, DesignMatrix, 'response')

# Check the application of the model

cor(StandardizedData[['Target']][!(TrainTestVector)] / StandardizedData[[WeightVariable]][!(TrainTestVector)], StandardizedData$fitted.values[!(TrainTestVector)])

# Create a data set on which to create univariate summaries for the categorical variables

CategoricalDataSet = merge(StandardizedData[, c(Key, WeightVariable, OffsetVariable, 'Target', 'fitted.values'), with = FALSE], StandardizedData02[, c(Key, CategoricalVariables), with = FALSE], by = c(Key))

# Initialize the categorical variable univariate summary

CategoricalSummary = data.table()

# Initialize the translation table for all categorical variables

TotalTranslationTable = data.table()

# Initialize the Excel file

Workbook = createWorkbook()

for (ThisVariable in CategoricalVariables)
{
  
  # Find the terms corresponding to the variable
  
  TermsInQuestion = CrossValidatedTerms$SelectedTerm[CrossValidatedTerms$SelectedTerm %in% paste('V', OriginalVariableModel$Term[substr(OriginalVariableModel$Variable, 1, nchar(ThisVariable)) == ThisVariable], sep = '')]

  # Calculate the contribution coming from the variable in question
    
  CategoricalDataSet$VariableEffect = exp(as.matrix(DesignMatrix[, TermsInQuestion, with = FALSE]) %*% Model$coefficients[TermsInQuestion])

  # Summarize to the categorical variable

  Summary = CategoricalDataSet[TrainTestVector, .(Weight = sum(get(WeightVariable)), Offset = sum(get(WeightVariable) * exp(get(OffsetVariable))) / sum(get(WeightVariable)), Target = sum(Target) / sum(get(WeightVariable)), VariableEffect = sum(get(WeightVariable) * VariableEffect) / sum(get(WeightVariable)), Model = sum(get(WeightVariable) * fitted.values) / sum(get(WeightVariable))), by = c(ThisVariable)]
  
  # Mark the selected values in the model
  
  Summary$Selected = (Summary[[ThisVariable]] %in% as.numeric(unique(substr(OriginalVariableModel$Variable[substr(OriginalVariableModel$Variable, 1, nchar(ThisVariable)) == ThisVariable], nchar(ThisVariable) + 1, nchar(OriginalVariableModel$Variable[substr(OriginalVariableModel$Variable, 1, nchar(ThisVariable)) == ThisVariable])))))

  # Update the values based on whether they were selected
  
  Summary[[ThisVariable]] = Summary[[ThisVariable]] * Summary$Selected

  # Summarize to the level of the selected and dismissed values
    
  Summary = Summary[, .(Weight = sum(Weight), Offset = sum(Weight * Offset) / sum(Weight), Target = sum(Weight * Target) / sum(Weight), VariableEffect = sum(Weight * VariableEffect) / sum(Weight), Model = sum(Weight * Model) / sum(Weight)), by = c(ThisVariable)]
  
  # Sort by the current values of the groups
  
  Summary = Summary[order(Summary[[ThisVariable]]), ]
  
  # Relabel the selected groups
  
  Summary$Group = seq(0, nrow(Summary) - 1)
  
  # Record the variable in question
  
  Summary$Variable = TranslationTable$OldVariable[TranslationTable$NewVariable == TranslationTable03$OldVariable[TranslationTable03$NewVariable == ThisVariable]]

  # Update the names
  
  names(Summary)[names(Summary) == 'Weight'] = WeightVariable
  names(Summary)[names(Summary) == 'Offset'] = OffsetVariable
  names(Summary)[names(Summary) == 'Target'] = Target
  
  # Update the summary
  
  CategoricalSummary = rbind(CategoricalSummary, Summary[, c('Variable', 'Group', WeightVariable, OffsetVariable, Target, 'VariableEffect', 'Model'), with = FALSE])
  
  # Create the translation between the original variable and its labeling
  
  Translation = merge(DataSet[, c(Key, TranslationTable$OldVariable[TranslationTable$NewVariable == TranslationTable03$OldVariable[TranslationTable03$NewVariable == ThisVariable]]), with = FALSE], StandardizedData[, c(Key, TranslationTable03$OldVariable[TranslationTable03$NewVariable == ThisVariable]), with = FALSE], by = c(Key))
  
  # Create the translation between the original variable labeling and its grouping
  
  Translation = unique(merge(Translation, StandardizedData02[, c(Key, ThisVariable), with = FALSE], by = c(Key))[, seq(2, 4), with = FALSE])
  
  # Create the translation to the selected values of the grouping
  
  Translation = merge(Translation, Summary[, c(ThisVariable, 'Group'), with = FALSE], by = c(ThisVariable), all.x = TRUE)
  
  # Missing values are indicative of not being selected values
  
  Translation[[4]][is.na(Translation[[4]]) == TRUE] = 0

  # Order the values for convenient display
  
  Translation = Translation[order(Translation[[4]], Translation[[2]]), ]

  # Write the translation to the workbook
  
  addWorksheet(Workbook, substr(paste('Group', TranslationTable$OldVariable[TranslationTable$NewVariable == TranslationTable03$OldVariable[TranslationTable03$NewVariable == ThisVariable]], sep = '') ,1, 31))
  writeData(Workbook, substr(paste('Group', TranslationTable$OldVariable[TranslationTable$NewVariable == TranslationTable03$OldVariable[TranslationTable03$NewVariable == ThisVariable]], sep = ''), 1, 31), Translation[, c(2, 4)])
  
  # Record the variables in question
  
  Translation$OriginalVariable = TranslationTable$OldVariable[TranslationTable$NewVariable == TranslationTable03$OldVariable[TranslationTable03$NewVariable == ThisVariable]]
  Translation$NumericVariable = TranslationTable03$OldVariable[TranslationTable03$NewVariable == ThisVariable]
  Translation$GroupedNumericVariable = ThisVariable
  Translation$RelabeledGroupedNumericVariable = paste('Group', TranslationTable$OldVariable[TranslationTable$NewVariable == TranslationTable03$OldVariable[TranslationTable03$NewVariable == ThisVariable]], sep = '')
  
  # Update the names

  names(Translation)[names(Translation) == Translation$OriginalVariable[1]] = 'OriginalVariableValue'
  names(Translation)[names(Translation) == TranslationTable03$OldVariable[TranslationTable03$NewVariable == ThisVariable]] = 'NumericVariableValue'
  names(Translation)[names(Translation) == ThisVariable] = 'GroupedNumericVariableValue'
  names(Translation)[names(Translation) == 'Group'] = 'RelabeledGroupedNumericVariableValue'

  # Update the total translation table
    
  TotalTranslationTable = rbind(TotalTranslationTable, Translation[, c('OriginalVariable', 'NumericVariable', 'GroupedNumericVariable', 'RelabeledGroupedNumericVariable', 'OriginalVariableValue', 'NumericVariableValue', 'GroupedNumericVariableValue', 'RelabeledGroupedNumericVariableValue')])

}

addWorksheet(Workbook, 'CategoricalSummary')
writeData(Workbook, 'CategoricalSummary', CategoricalSummary)

# Obtain the potential continuous variables

ContinuousVariables = unique(OriginalVariableModel$Variable[!(substr(OriginalVariableModel$Variable, 1, max(nchar(TranslationTable03$NewVariable))) %in% TranslationTable03$NewVariable)])

# Obtain the selected continuous variables

ContinuousVariables = ContinuousVariables[ContinuousVariables %in% OriginalVariableModel$Variable[paste('V', OriginalVariableModel$Term, sep = '') %in% CrossValidatedTerms$SelectedTerm]]

# Create a data set on which to create univariate summaries for the continuous variables

ContinousDataSet = merge(StandardizedData[, c(Key, WeightVariable, OffsetVariable, 'Target', 'fitted.values'), with = FALSE], DataSet[, c(Key, ContinuousVariables), with = FALSE], by = c(Key))

# Initialize the continuous variable univariate summary

ContinuousSummary = data.table()

for (ThisVariable in ContinuousVariables)
{
  
  # Find the terms corresponding to the variable

  TermsInQuestion = CrossValidatedTerms$SelectedTerm[CrossValidatedTerms$SelectedTerm %in% paste('V', OriginalVariableModel$Term[OriginalVariableModel$Variable == ThisVariable], sep = '')]
  
  # Calculate the contribution coming from the variable in question
  
  ContinousDataSet$VariableEffect = exp(as.matrix(DesignMatrix[, TermsInQuestion, with = FALSE]) %*% Model$coefficients[TermsInQuestion])
  
  # Summarize to the continuous variable
  
  Summary = ContinousDataSet[TrainTestVector, .(Weight = sum(get(WeightVariable)), Offset = sum(get(WeightVariable) * exp(get(OffsetVariable))) / sum(get(WeightVariable)), Target = sum(Target) / sum(get(WeightVariable)), VariableEffect = sum(get(WeightVariable) * VariableEffect) / sum(get(WeightVariable)), Model = sum(get(WeightVariable) * fitted.values) / sum(get(WeightVariable))), by = c(ThisVariable)]
  
  if (NumberOfGroups < nrow(Summary))
  {
    
    # Create the decile
    
    Summary$Group = 0
    Summary$Group[is.na(Summary[[ThisVariable]]) == FALSE] = Discretize(Weight = Summary$Weight[is.na(Summary[[ThisVariable]]) == FALSE], x = Summary[[ThisVariable]][is.na(Summary[[ThisVariable]]) == FALSE], GroupNumber = NumberOfGroups)
    
    # Summarize to the level of the selected and dismissed values
    
    Summary = Summary[, .(Minimum = min(get(ThisVariable)), Mean = sum(Weight * get(ThisVariable)) / sum(Weight), Maximum = max(get(ThisVariable)), Weight = sum(Weight), Offset = sum(Weight * Offset) / sum(Weight), Target = sum(Weight * Target) / sum(Weight), VariableEffect = sum(Weight * VariableEffect) / sum(Weight), Model = sum(Weight * Model) / sum(Weight)), by = c('Group')]

  }
  
  else
  {
    
    # Update the name of the variable in question
    
    names(Summary)[names(Summary) == ThisVariable] = 'Group'
    
    # Create the summary statistics
    
    Summary$Minimum = Summary$Group
    Summary$Mean = Summary$Group
    Summary$Maximum = Summary$Group
    
  }

  # Sort the data
  
  Summary = Summary[order(Summary$Group), ]
  
  # Relabel the data

  Summary$Group = seq(ifelse(min(Summary$Group) == 0, 0, 1), ifelse(min(Summary$Group) == 0, 0, 1) + nrow(Summary) - 1)
  
  # Record the variable in question
  
  Summary$Variable = ThisVariable
  
  # Update the names
  
  names(Summary)[names(Summary) == 'Group'] = 'Value'
  names(Summary)[names(Summary) == 'Weight'] = WeightVariable
  names(Summary)[names(Summary) == 'Offset'] = OffsetVariable
  names(Summary)[names(Summary) == 'Target'] = Target
    
  # Sort by the current values of the groups
  
  #ContinuousSummary = rbind(ContinuousSummary, Summary[order(Summary$Group), c('Variable', 'Group', 'Minimum', 'Mean', 'Maximum', WeightVariable, OffsetVariable, Target, 'VariableEffect', 'Model'), with = FALSE])
  ContinuousSummary = rbind(ContinuousSummary, Summary[order(Summary$Value), c('Variable', 'Value', 'Minimum', 'Mean', 'Maximum', WeightVariable, OffsetVariable, Target, 'VariableEffect', 'Model'), with = FALSE])

}

# Write the results to the workbook

addWorksheet(Workbook, 'ContinuousSummary')
writeData(Workbook, 'ContinuousSummary', ContinuousSummary)

# Format the parameter estimates

ParameterEstimates = as.data.frame(summary(Model)[['coefficients']])

ParameterEstimates$Parameter = rownames(ParameterEstimates)
rownames(ParameterEstimates) = NULL

# Write the parameter estimates to the workbook

addWorksheet(Workbook, 'ParameterEstimates')
writeData(Workbook, 'ParameterEstimates', ParameterEstimates[, c(5, seq(4))])

# Update the variable names for the continuous variables

OriginalVariableModel$Variable.y[is.na(OriginalVariableModel$Variable.y) == TRUE] = OriginalVariableModel$OldVariable[is.na(OriginalVariableModel$Variable.y) == TRUE]

# Restrict attention to the columns of interest

OriginalVariableModel = OriginalVariableModel[, c('Term', 'Variable.y', 'Knot', 'VariableValue')]

# Update the variable names

names(OriginalVariableModel)[names(OriginalVariableModel) == 'Variable.y'] = 'Variable'

# Merge in the original categorical variable values

nrow(unique(OriginalVariableModel[, c('Term', 'Variable')]))

OriginalVariableModel = merge(OriginalVariableModel, TotalTranslationTable[, c('GroupedNumericVariable', 'GroupedNumericVariableValue', 'OriginalVariable', 'OriginalVariableValue')], by.x = c('Variable', 'VariableValue'), by.y = c('GroupedNumericVariable', 'GroupedNumericVariableValue'), all.x = TRUE)

nrow(OriginalVariableModel)

# Update the variable names

OriginalVariableModel$OriginalVariable[is.na(OriginalVariableModel$OriginalVariable) == TRUE] = OriginalVariableModel$Variable[is.na(OriginalVariableModel$OriginalVariable) == TRUE]

# Write the results to the workbook

addWorksheet(Workbook, 'ModelTerms')
writeData(Workbook, 'ModelTerms', OriginalVariableModel[order(OriginalVariableModel$Term, OriginalVariableModel$OriginalVariable, OriginalVariableModel$Knot, OriginalVariableModel$OriginalVariableValue), c('Term', 'OriginalVariable', 'Knot', 'OriginalVariableValue')])

# Write the results to the disk

saveWorkbook(Workbook, paste(OutputDirectory, '/Exhibits.xlsx', sep = ''))

# Explicitly create the terms for the model using the original data

DataSet$V1 = as.numeric(DataSet$Neighborhood == 'Crawfor')
DataSet$V2 = (DataSet$SaleCondition == 'Abnorml')
DataSet$V3 = as.numeric(DataSet$KitchenQual == 'TA')
DataSet$V4 = DataSet$TotalBsmtSF
DataSet$V5 = pmax(DataSet$KitchenAbvGr - 1, 0)
DataSet$V6 = DataSet$GarageCars
DataSet$V7 = DataSet$Fireplaces
DataSet$V8 = pmax(DataSet$YearBuilt - 2006, 0)
DataSet$V9 = DataSet$LotArea
DataSet$V10 = pmax(DataSet$LotArea - 25419, 0)
DataSet$V11 = pmax(DataSet$YearBuilt - 1951, 0)
DataSet$V12 = DataSet$BsmtFinSF1
DataSet$V13 = DataSet$FullBath

DataSet$V14 = (DataSet$Neighborhood == 'Edwards') * pmax(DataSet$TotalBsmtSF - 1626, 0)
DataSet$V15 = (DataSet$BsmtQual == 'Ex') * (DataSet$HouseStyle == '1Story')
DataSet$V15[is.na(DataSet$V15)] = 0
DataSet$V16 = pmax(DataSet$GarageCars - 2, 0) * as.numeric(DataSet$OverallQual == 9)
DataSet$V17 = DataSet$GrLivArea * DataSet$YearBuilt

# Create the formula

glmFormula = as.formula(paste('SalePrice ~ ', paste(paste('V', seq(17), sep = ''), collapse = ' + '), sep = ''))

# Create a model using the explicitly created terms

ExplicitModel = glm(formula = glmFormula, family = poisson(link = 'log'), data = DataSet[TrainTestVector, ])

# Observe the coherence of the explicit model with the one on the design matrix

max(abs(Model$fitted.values - ExplicitModel$fitted.values))
cor(Model$fitted.values, ExplicitModel$fitted.values)