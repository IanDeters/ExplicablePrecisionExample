# Observe the relevant directories

MethodsDirectory
InputDirectory
OutputDirectory

# Create the function vector

FunctionVector = c('data.table', 'DataProfiling', 'DataStandardization', 'CreateTranslationTable')

# Load openxlsx so as to read the function dependency information

library(openxlsx)

# Read in the function data

DependsOn = read.xlsx(xlsxFile = paste(MethodsDirectory, '/R Methods.xlsx', sep = ''), sheet = 'DependsOn')

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

DataSet = fread(paste(InputDirectory, '/train.csv', sep = ''))

# Set the target, weight, and offset variables

Target = 'SalePrice'

OffsetVariable = NULL
WeightVariable = NULL

# Check that target variable is always populated

print(paste('Observe that the target is always populated.  The following should evaluate as 0: ', sum(is.na(DataSet[[Target]])), sep = ''))

# If the offset is null, create a placeholder offset

if (is.null(OffsetVariable) == TRUE)
{

  # Initialize the name index
    
  i = 0
  
  while (paste('Offset', paste(rep('Z', i), collapse = ''), sep = '') %in% names(DataSet))
  {
    
    i = i + 1

  }

  # Create the placeholder offset variable name
    
  OffsetVariable = paste('Offset', paste(rep('Z', i), collapse = ''), sep = '')
  
  # Create the placeholder offset
  
  DataSet[[OffsetVariable]] = rep(0, nrow(DataSet))
  
}

# Profile the data

DataProfile = DataProfiling(DataSet, UnivariateSummarySize = 10, NumericThreshold = .99)

# Observe the duplicate flag indicator

print(paste('Observe that the duplicate flag indicator is: ', DataProfile[['DuplicateRecordFlag']], sep = ''))

# Observe the proposed key

print(paste('The proposed key is: ', DataProfile[['Key']], sep = ''))

# Manually set the key if the proposed one is not correct

Key = DataProfile[['Key']]

# Discard the target, offset, and key variables

DataProfile[['MetaData']] = DataProfile[['MetaData']][!(DataProfile[['MetaData']][['Variable']] %in% c(Target, OffsetVariable, Key)), ]

# Observe a sample for each variable initially considered to be continuous

for (ThisVariable in DataProfile[['MetaData']][['Variable']][DataProfile[['MetaData']][['ConceptualType']] == 'Continuous'])
{
  
  # Print the sample for each variable
  
  print(DataProfile[[ThisVariable]])
  
}

# Update the meta data as is deemed fit

for (ThisVariable in c('MSSubClass', 'OverallQual', 'OverallCond', 'MoSold'))
{
  
  DataProfile[['MetaData']][['ConceptualType']][DataProfile[['MetaData']][['Variable']] == ThisVariable] = 'Categorical'

}

# Create age variables

DataSet$Age = pmax(DataSet$YrSold - DataSet$YearBuilt, 0)
DataSet$RemodelAge = pmax(DataSet$YrSold - DataSet$YearRemodAdd, 0)
DataSet$GarageAge = pmax(DataSet$YrSold - DataSet$GarageYrBlt, 0)

# Update the meta data to reflect the new variables

DataProfile[['MetaData']] = rbind(DataProfile[['MetaData']], 

data.table(
  
  Variable = c('Age', 'RemodelAge', 'GarageAge'), 
  
  ConceptualType = c('Continuous', 'Continuous', 'Continuous')
  
))

# Create the meta data for the special values that the variables take beyond the missing values

NonMissingSpecialValues = data.table(
  
  Variable = c(),
  
  Value = c()
  
)

# Create the standardized data

StandardizedData = DataStandardization(DataSet, WeightVariable = WeightVariable, DataProfile[['MetaData']], NonMissingSpecialValues = NULL)

TranslationTable = StandardizedData[[2]]
StandardizedData = StandardizedData[[1]]

# Observe the coherence of the dimensions

2 * sum(DataProfile[['MetaData']][['ConceptualType']] == 'Continuous') + sum(DataProfile[['MetaData']][['ConceptualType']] != 'Continuous') + 1
nrow(TranslationTable) + 1
ncol(StandardizedData)

# If there was originally no weight variable, record that a weight variable has been created

if (is.null(WeightVariable) == TRUE)
{
  
  WeightVariable = 'Weight'
  
}

# Include the key

nrow(DataSet)
ncol(StandardizedData) + length(Key)

StandardizedData = cbind(DataSet[, Key, with = FALSE], StandardizedData)

nrow(StandardizedData)
ncol(StandardizedData)

# Include the targets and offset in the standardized data

StandardizedData$Target = DataSet[[Target]]
StandardizedData$Offset = DataSet[[OffsetVariable]]

# Include the conceptual type of the original variable in the translation table

nrow(TranslationTable)
ncol(TranslationTable) + 1

TranslationTable = merge(TranslationTable, DataProfile[['MetaData']], by.x = 'OldVariable', by.y = 'Variable')

nrow(TranslationTable)
ncol(TranslationTable)

# Declare all conceptually categorical variables as internally categorical, i.e. as factors

for (Variable in TranslationTable$NewVariable[TranslationTable$ConceptualType == 'Categorical'][!(TranslationTable$NewVariable[TranslationTable$ConceptualType == 'Categorical'] %in% c('Weight', 'Target', 'Offset'))])
{
  
  StandardizedData[[Variable]] = as.factor(StandardizedData[[Variable]])
  
}

# Create a formula to create the design matrix

DesignMatrixFormula = as.formula(paste('~ Target + Weight + Offset + ', paste(TranslationTable$NewVariable, collapse = ' + ')))

# Create the design matrix

nrow(StandardizedData)
1 + 3 + sum(sapply(seq(sum(TranslationTable$ConceptualType == 'Categorical')), function(i){length(unique(DataSet[[TranslationTable$OldVariable[TranslationTable$ConceptualType == 'Categorical'][i]]])) - 1})) + sum(TranslationTable$ConceptualType == 'Continuous')

DesignMatrix = as.data.table(model.matrix(DesignMatrixFormula, StandardizedData))

nrow(DesignMatrix)
ncol(DesignMatrix)

# Create the additional columns in the design matrix and the translation table

DesignMatrix02 = CreateTranslationTable(StandardizedData, DesignMatrix, TranslationTable$NewVariable[order(TranslationTable$NewVariable)])

# Add the additional columns to the design matrix

nrow(DesignMatrix)
ncol(DesignMatrix) + sum(TranslationTable$ConceptualType == 'Categorical')

DesignMatrix = cbind(DesignMatrix, DesignMatrix02[[1]])

nrow(DesignMatrix)
ncol(DesignMatrix)

# Create the translation table for the design matrix

TranslationTable02 = DesignMatrix02[[2]]

rm(DesignMatrix02)

# Export the newly created data and its attendant meta data

fwrite(x = DataSet, file = paste(OutputDirectory, '/ModifiedDataSet.csv', sep = ''))
fwrite(x = StandardizedData, file = paste(OutputDirectory, '/StandardizedData.csv', sep = ''))
fwrite(x = DesignMatrix, file = paste(OutputDirectory, '/DesignMatrix.csv', sep = ''))

Workbook = createWorkbook()

addWorksheet(Workbook, 'Target')
writeData(Workbook, 'Target', data.table(Target = Target))

addWorksheet(Workbook, 'OffsetVariable')
writeData(Workbook, 'OffsetVariable', data.table(OffsetVariable = OffsetVariable))

addWorksheet(Workbook, 'WeightVariable')
writeData(Workbook, 'WeightVariable', data.table(WeightVariable = WeightVariable))

addWorksheet(Workbook, 'Key')
writeData(Workbook, 'Key', data.table(Key = Key))

addWorksheet(Workbook, 'MetaData')
writeData(Workbook, 'MetaData', DataProfile[['MetaData']])

addWorksheet(Workbook, 'TranslationTable')
writeData(Workbook, 'TranslationTable', TranslationTable[, c('OldVariable', 'NewVariable')])

addWorksheet(Workbook, 'TranslationTable02')
writeData(Workbook, 'TranslationTable02', TranslationTable02)

saveWorkbook(Workbook, paste(OutputDirectory, '/MetaData.xlsx', sep = ''))