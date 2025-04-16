# Observe the relevant directories

DependsOnDirectory
MethodsDirectory
InputDirectory
OutputDirectory

# Set the maximum model degree of investigation

MaximumInvestigationDegree = 5

# Create the function vector

FunctionVector = c('data.table', 'earth', 'FormatearthOutput', 'AddLeadingZeros', 'MASS', 'doParallel', 'glmCrossValidation 02')

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
DesignMatrix02 = fread(paste(InputDirectory, '/DesignMatrix02.csv', sep = ''))

Key = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'Key')
Key = Key[, 1]

TranslationTable = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData.xlsx', sep = ''), sheet = 'TranslationTable')
TranslationTable03 = read.xlsx(xlsxFile = paste(InputDirectory, '/MetaData03.xlsx', sep = ''), sheet = 'TranslationTable03')

# Create the training / testing split

set.seed(666)

TrainTestVector = sample(x = c(TRUE, FALSE), size = nrow(DesignMatrix02), replace = TRUE, prob = c(.7, .3))

# Create the formula for the MARS routine

earthFormula = as.formula(paste('NormalizedTarget ~ ', paste(names(DesignMatrix02)[!(names(DesignMatrix02) %in% c('(Intercept)', 'NormalizedTarget', 'Weight', 'Offset'))], collapse = ' + '), ' + offset(Offset)', sep = ''))

# Initialize the current degree of the model

CurrentDegree = 0

for (i in seq(MaximumInvestigationDegree))
{
  
  # Create the MARS model
  
  Model = earth(formula = earthFormula, data = DesignMatrix02[TrainTestVector, ], weights = Weight, thresh = .001,
                pmethod = 'none', degree = i, nk = 100, nprune = 100, varmod.method = 'none')
  
  # Format the model information
  
  ModelInformation = FormatearthOutput(Model = Model)

  # Check if the maximum number of factors in the terms has increased
  
  if (CurrentDegree < max(ModelInformation[['ModelInformation']][, .(Degree = .N), by = 'Term'][['Degree']]))
  {
    
    CurrentDegree = CurrentDegree + 1
    
  }

}

# Recreate the measure of fit

Model[['rsq']]
1 - Model[['rss']] / sum(DesignMatrix02$Weight[TrainTestVector] * (DesignMatrix02$NormalizedTarget[TrainTestVector] - DesignMatrix02$Offset[TrainTestVector] - sum(DesignMatrix02$Weight[TrainTestVector] * (DesignMatrix02$NormalizedTarget[TrainTestVector] - DesignMatrix02$Offset[TrainTestVector])) / sum(DesignMatrix02$Weight[TrainTestVector]))^2)

# Recreate the fitted values

max(abs(Model[['bx']] %*% Model[['coefficients']] + DesignMatrix02$Offset[TrainTestVector] - Model[['fitted.values']]))

# Create the predictions on the data

DesignMatrix02$earth = predict(Model, DesignMatrix02)

# Observe the coherence of the predict function with the fitted values

max(abs(DesignMatrix02$earth[TrainTestVector] - Model$fitted.values))

# Create the model on the indexed data

StandardizedData$earth = DesignMatrix02$earth

# Observe the coherence of the predictions

sum(DesignMatrix02$Weight * (DesignMatrix02$NormalizedTarget - DesignMatrix02$earth)^2)
sum(StandardizedData$Weight * (StandardizedData$Target / StandardizedData$Weight - StandardizedData$earth)^2)

# Separate the model information meta data

ExpandedModelInformation = ModelInformation[['ExpandedModelInformation']]
ModelInformation = ModelInformation[['ModelInformation']]

# All knots associated with categorical variables are equivalent to the variable entering linearly
# This code updates the model information accordingly

ExpandedModelInformation$Use[(substr(ExpandedModelInformation$Variable, 1, min(nchar(TranslationTable03$NewVariable))) %in% TranslationTable03$NewVariable) == TRUE] = 2
ExpandedModelInformation$Knot[(substr(ExpandedModelInformation$Variable, 1, min(nchar(TranslationTable03$NewVariable))) %in% TranslationTable03$NewVariable) == TRUE] = NA

# Observe the number of factors in each term

ModelInformation[, .(Degree = .N), by = 'Term'][, .(Terms = .N), by = 'Degree']
ExpandedModelInformation[, .(Degree = .N), by = 'Term'][, .(Terms = .N), by = 'Degree']

# Create a function to create the terms indicated

CreateTerm = function(i)
{
  
  # Determine the variables in the term
  
  Variables = ExpandedModelInformation$Variable[ExpandedModelInformation$Term == i]
  
  # Determine the knots associated to the variables in the term
  
  Knots = ExpandedModelInformation$Knot[ExpandedModelInformation$Term == i]

  # Create the term  

  apply((DesignMatrix02[, c(Variables), with = FALSE] - matrix(data = ifelse(is.na(Knots) == TRUE, 0, Knots), nrow = nrow(DesignMatrix02), ncol = length(Variables), byrow = TRUE)) * (matrix(data = ifelse(is.na(Knots) == TRUE, 0, Knots), nrow = nrow(DesignMatrix02), ncol = length(Variables), byrow = TRUE) <= DesignMatrix02[, c(Variables), with = FALSE]), 1, prod)
  
}

# Create all possible terms

DesignMatrix03 = as.data.table(sapply(seq(length(unique(ExpandedModelInformation$Term))), CreateTerm))

# Create a function to compare columns in the MARS matrix to the third design matrix

ColumnCompare = function(i){as.numeric(apply(abs(matrix(data = DesignMatrix03[[i]][TrainTestVector], nrow = nrow(Model[['bx']]), ncol = ncol(Model[['bx']])) - Model[['bx']]), 2, max))}

# Compare columns in the MARS matrix to the third design matrix

CheckTermCreation = do.call(rbind, lapply(seq(ncol(DesignMatrix03)), function(i){data.table(names(DesignMatrix03)[i], Index = seq(ncol(Model[['bx']]))[ColumnCompare(i) == min(ColumnCompare(i))] - 1, Error = min(ColumnCompare(i)))}))

# Restrict attention columns in the third design matrix which have a match in the MARS matrix

CheckTermCreation = CheckTermCreation[CheckTermCreation$Error < 10^(-8), ]

# Observe that the number of columns in the third design matrix with a match in the MARS matrix is the same
# as the number of columns in the MARS matrix with no left hinge

nrow(CheckTermCreation)
sum(ModelInformation[, .(Use = prod(Use != -1)), by = 'Term'][['Use']])

# Confirm that the matching columns are constructed using the same components

for (i in seq(nrow(CheckTermCreation)))
{

  print(ExpandedModelInformation[ExpandedModelInformation$Term == as.numeric(substr(CheckTermCreation$V1, 2, nchar(CheckTermCreation$V1)))[i], ])
  print(ModelInformation[ModelInformation$Term == CheckTermCreation$Index[i], ])
  
}

# Create the formula for the glm function

glmFormula = as.formula(paste('NormalizedTarget ~ ', paste(names(DesignMatrix03), collapse = ' + '), sep = ''))

# Include the weight, offset, and target in the data set

DesignMatrix03$NormalizedTarget = DesignMatrix02$NormalizedTarget
DesignMatrix03$Weight = DesignMatrix02$Weight
DesignMatrix03$Offset = DesignMatrix02$Offset

# Create a model with all terms

FullModel = glm(formula = glmFormula, family = gaussian(link = 'identity'), data = DesignMatrix03[TrainTestVector, ], weights = Weight, offset = Offset)

# Compare the results and observe that the fit of the expanded model is similar to that of the original model

cor(DesignMatrix02$NormalizedTarget[TrainTestVector], DesignMatrix02$earth[TrainTestVector])
cor(DesignMatrix03$NormalizedTarget[TrainTestVector], FullModel$fitted.values)

# Import the original data

DataSet = fread(paste(InputDirectory, '/train.csv', sep = ''))

# Select all factors corresponding to continuous variables

SelectedContinuousValues = ExpandedModelInformation[(substr(ExpandedModelInformation$Variable, 1, min(nchar(TranslationTable03$NewVariable))) %in% TranslationTable03$NewVariable) == FALSE, ]

# Initialize the translation between the knots of uniform variables and the original variables

KnotTranslation = data.table()

for (i in seq(length(unique(SelectedContinuousValues$Variable))))
{
  
  # Obtain the uniform variable in question
  
  ThisVariable = unique(SelectedContinuousValues$Variable)[order(unique(SelectedContinuousValues$Variable))][i]
  
  # Find the corresponding original variable
  
  OldVariable = TranslationTable$OldVariable[TranslationTable$NewVariable == ThisVariable]
  
  # Obtain the knots pertaining to the uniform variable
  
  UniformKnots = unique(SelectedContinuousValues$Knot[SelectedContinuousValues$Variable == ThisVariable])

  # Check if one of the knot values is NA
    
  if (1 == sum(is.na(UniformKnots)))
  {
    
    # Update the knot table
    
    KnotTranslation = rbind(KnotTranslation, data.table(NewVariable = ThisVariable, Knot = NA, OldVariable = OldVariable, OriginalKnot = NA))
    
    # Remove the NA knot
    
    UniformKnots = UniformKnots[is.na(UniformKnots) == FALSE]
    
  }
  
  # Check if any knots remain
  
  if (0 < length(UniformKnots))
  {
    
    # Order the knots from the uniform variable
    
    UniformKnots = UniformKnots[order(UniformKnots)]
    
    # Create the correspondence between the original variable and the uniform variable
    
    VariableKnots = merge(DataSet[(is.na(DataSet[[OldVariable]]) == FALSE), c(Key, OldVariable), with = FALSE], StandardizedData[, c(Key, ThisVariable), with = FALSE], by = c(Key))
    
    for (j in seq(length(UniformKnots)))
    {
      
      # Update the knot table

      KnotTranslation = rbind(KnotTranslation, data.table(NewVariable = ThisVariable, Knot = UniformKnots[j], OldVariable = OldVariable, OriginalKnot = max(VariableKnots[[OldVariable]][(is.na(VariableKnots[[OldVariable]]) == FALSE) & (VariableKnots[[ThisVariable]] <= UniformKnots[j])])))
      
    }

  }
  
}

# Check the row counts

nrow(unique(SelectedContinuousValues[, c('Variable', 'Knot')]))
nrow(KnotTranslation)

# Include the categorical variables in the original data

DataSet = cbind(DataSet, DesignMatrix02[, unique(ExpandedModelInformation$Variable[(substr(ExpandedModelInformation$Variable, 1, min(nchar(TranslationTable03$NewVariable))) %in% TranslationTable03$NewVariable) == TRUE]), with = FALSE])

# Create model information for the model using the original variables

ExpandedModelInformation02 = merge(ExpandedModelInformation, KnotTranslation, by.x = c('Variable', 'Knot'), by.y = c('NewVariable', 'Knot'), all.x = TRUE)

# Update the variable and knot values to correspond to the original values

ExpandedModelInformation02$Variable[is.na(ExpandedModelInformation02$OldVariable) == FALSE] = ExpandedModelInformation02$OldVariable[is.na(ExpandedModelInformation02$OldVariable) == FALSE]
ExpandedModelInformation02$Knot[is.na(ExpandedModelInformation02$OldVariable) == FALSE] = ExpandedModelInformation02$OriginalKnot[is.na(ExpandedModelInformation02$OldVariable) == FALSE]

# Create a function to create the terms indicated

CreateTerm = function(i)
{
  
  # Determine the variables in the term
  
  Variables = ExpandedModelInformation02$Variable[ExpandedModelInformation02$Term == i]
  
  # Determine the knots associated to the variables in the term
  
  Knots = ExpandedModelInformation02$Knot[ExpandedModelInformation02$Term == i]
  
  # Create the term  
  
  apply((DataSet[, c(Variables), with = FALSE] - matrix(data = ifelse(is.na(Knots) == TRUE, 0, Knots), nrow = nrow(DataSet), ncol = length(Variables), byrow = TRUE)) * (matrix(data = ifelse(is.na(Knots) == TRUE, 0, Knots), nrow = nrow(DataSet), ncol = length(Variables), byrow = TRUE) <= DataSet[, c(Variables), with = FALSE]), 1, prod)
  
}

# Create the terms translated back to the original variables

DesignMatrix04 = sapply(seq(length(unique(ExpandedModelInformation02$Term))), CreateTerm)

# Calculate the value to be used where values are missing

MissingValue = min(sapply(seq(ncol(DesignMatrix04)), function(i){min(DesignMatrix04[[i]][is.na(DesignMatrix04[[i]]) == FALSE])})) - 1

# Apply the value to the places where there is missing data

DesignMatrix04 = as.matrix(DesignMatrix04)
DesignMatrix04[which(is.na(DesignMatrix04), arr.ind = TRUE)] = MissingValue

# Include the missing indicators

DesignMatrix04 = cbind(DesignMatrix04, DesignMatrix02[, TranslationTable$NewVariable[(grepl('Missing', TranslationTable$NewVariable)) & (TranslationTable$OldVariable %in% KnotTranslation$OldVariable)], with = FALSE])

# Merge in the term degree

ExpandedModelInformation02 = merge(ExpandedModelInformation02, ExpandedModelInformation02[, .(Degree = .N), by = 'Term'], by = 'Term')

# Create a formula using only main effects and missing indicators

glmFormula = as.formula(paste('NormalizedTarget ~', paste(c(paste('V', unique(ExpandedModelInformation02$Term[ExpandedModelInformation02$Degree == 1]), sep = ''), names(DesignMatrix04)[grepl('Missing', names(DesignMatrix04))]), collapse = ' + ')))

# Include the weight, offset, and target in the data set

DesignMatrix04$NormalizedTarget = DesignMatrix02$NormalizedTarget
DesignMatrix04$Weight = DesignMatrix02$Weight
DesignMatrix04$Offset = DesignMatrix02$Offset

# Create an intercept model

InterceptModel = glm(formula = 'NormalizedTarget ~ 1', family = poisson(link = 'log'), data = DesignMatrix04[TrainTestVector, ], weights = Weight, offset = Offset)

# Create a main effect model

MainEffectModel = stepAIC(InterceptModel, scope = glmFormula, direction = 'both', k = 4)

# Observe the correlation

cor(MainEffectModel$fitted.values, DesignMatrix04$NormalizedTarget[TrainTestVector])

# Create an offset using the main effects model

DesignMatrix04$Offset02 = predict(MainEffectModel, DesignMatrix04)

# Initialize the selection variable

ExpandedModelInformation02$Selected = FALSE

# Update the variables from the main effect model

ExpandedModelInformation02$Selected[ExpandedModelInformation02$Degree == 1] = paste('V', ExpandedModelInformation02$Term[ExpandedModelInformation02$Degree == 1], sep = '') %in% names(MainEffectModel$coefficients)

# Initialize the selected missing indicators

SelectedMissingIndicators = c()

# Update the missing indicators

SelectedMissingIndicators = unique(c(SelectedMissingIndicators, names(MainEffectModel$coefficients)[grepl('Missing', names(MainEffectModel$coefficients))]))

# Discard the main effect model

rm(MainEffectModel)

# Create a formula using only second order effects and missing indicators

glmFormula = as.formula(paste('NormalizedTarget ~', paste(c(paste('V', unique(ExpandedModelInformation02$Term[ExpandedModelInformation02$Degree == 2]), sep = ''), names(DesignMatrix04)[grepl('Missing', names(DesignMatrix04))]), collapse = ' + ')))

# Create an intercept model

InterceptModel = glm(formula = 'NormalizedTarget ~ 1', family = poisson(link = 'log'), data = DesignMatrix04[TrainTestVector, ], weights = Weight, offset = Offset02)

# Create a model with second order interactions

SecondOrderModel = stepAIC(InterceptModel, scope = glmFormula, direction = 'both', k = 4)

# Observe the correlation

cor(SecondOrderModel$fitted.values, DesignMatrix04$NormalizedTarget[TrainTestVector])

# Create an offset using the second order effects model

DesignMatrix04$Offset03 = predict(SecondOrderModel, DesignMatrix04)

# Update the variables from the second order model

ExpandedModelInformation02$Selected[ExpandedModelInformation02$Degree == 2] = paste('V', ExpandedModelInformation02$Term[ExpandedModelInformation02$Degree == 2], sep = '') %in% names(SecondOrderModel$coefficients)

# Update the missing indicators

SelectedMissingIndicators = unique(c(SelectedMissingIndicators, names(SecondOrderModel$coefficients)[grepl('Missing', names(SecondOrderModel$coefficients))]))

# Discard the second order model

rm(SecondOrderModel)

# Create a formula using only third order effects and missing indicators

glmFormula = as.formula(paste('NormalizedTarget ~', paste(c(paste('V', unique(ExpandedModelInformation02$Term[ExpandedModelInformation02$Degree == 3]), sep = ''), names(DesignMatrix04)[grepl('Missing', names(DesignMatrix04))]), collapse = ' + ')))

# Create an intercept model

InterceptModel = glm(formula = 'NormalizedTarget ~ 1', family = poisson(link = 'log'), data = DesignMatrix04[TrainTestVector, ], weights = Weight, offset = Offset03)

# Create a model with third order interactions

ThirdOrderModel = stepAIC(InterceptModel, scope = glmFormula, direction = 'both', k = 4)

# Observe the correlation

cor(ThirdOrderModel$fitted.values, DesignMatrix04$NormalizedTarget[TrainTestVector])

# Update the variables from the third order model

ExpandedModelInformation02$Selected[ExpandedModelInformation02$Degree == 3] = paste('V', ExpandedModelInformation02$Term[ExpandedModelInformation02$Degree == 3], sep = '') %in% names(ThirdOrderModel$coefficients)

# Update the missing indicators

SelectedMissingIndicators = unique(c(SelectedMissingIndicators, names(ThirdOrderModel$coefficients)[grepl('Missing', names(ThirdOrderModel$coefficients))]))

# Create the selected terms

SelectedTerms = c(paste('V', unique(ExpandedModelInformation02$Term[ExpandedModelInformation02$Selected == TRUE]), sep = ''), SelectedMissingIndicators)

# Create an intercept model

Model = glm(formula = 'NormalizedTarget ~ 1', family = poisson(link = 'log'), data = DesignMatrix04, weights = Weight, offset = Offset)

# Initialize the round

Round = 1

# Check if the cross validation file exists

if (file.exists(paste(OutputDirectory, '/CrossValidation.xlsx', sep = '')) == TRUE)
{
  
  # Obtain the sheet names
  
  SheetNames = getSheetNames(paste(OutputDirectory, '/CrossValidation.xlsx', sep = ''))
  
  # Get the sheet names corresponding to error tables
  
  SheetNames = SheetNames[substr(SheetNames, 1, pmin(nchar(SheetNames), 10)) == 'ErrorTable']
  
  # Initialize the round
  
  Round = max(as.numeric(substr(SheetNames, 11, nchar(SheetNames)))) + 1
  
}

# Initialize the cross validation history

CrossValidationHistory = list()

# Initialize the looping condition

LoopingCondition = TRUE

# Create a cluster

Cluster = makeCluster(8)
  
while (LoopingCondition)
{
  
  set.seed(666 + Round)
  
  # Run cross validation
  
  Results = glmCrossValidation(Model = Model, Variables = SelectedTerms, NFold = 10, TrainingPercentage = .7, Cluster = Cluster)
  
  # Initialize the error and parameter estimate tables
  
  ErrorTable = data.table()
  ParameterEstimateTable = data.table()
  
  for (i in seq(length(Results)))
  {
    
    # Update the error tables
    
    ErrorTable = rbind(ErrorTable, data.table(Round = Round, Variable = Results[[i]][['Variable']], Pass = Results[[i]][['Pass']], Error = Results[[i]][['Error']]))
    
    # Obtain the parameter estimates
    
    ParameterEstimate = as.data.frame(Results[[i]][['ParameterEstimate']])
    
    # Use the row names to record the parameter name
    
    ParameterEstimate$Parameter = row.names(ParameterEstimate)
    
    # Record the variable in question
    
    ParameterEstimate$Variable = Results[[i]][['Variable']]
    
    # Record the round
    
    ParameterEstimate$Round = Round
    
    # Record the pass
    
    ParameterEstimate$Pass = Results[[i]][['Pass']]
    
    # Update the parameter estimate table
    
    ParameterEstimateTable = rbind(ParameterEstimateTable, ParameterEstimate)
    
  }

  # Merge the incumbent model errors with those of the potential models
    
  ErrorTable02 = merge(ErrorTable[is.na(ErrorTable$Variable) == TRUE, ], ErrorTable[is.na(ErrorTable$Variable) == FALSE, ], by = 'Pass')
  
  # Calculate the average improvement for adding or removing the term and the number of times there was improvement
  
  ErrorTable03 = ErrorTable02[, .(ImprovementCount = sum(Error.y < Error.x), ImprovementMean = mean(Error.x^2 - Error.y^2)), by = 'Variable.y']

  # Calculate the number of passes an estimate is less than or equal to zero
    
  SignCheck = ParameterEstimateTable[(is.na(ParameterEstimateTable$Variable) == FALSE) & (ParameterEstimateTable$Parameter == ParameterEstimateTable$Variable), .(SignCount = sum(Estimate <= 0)), by = 'Variable']
  
  # Update the sign count so that 10 implies either positive or negative sign consistency
  
  SignCheck$SignCount = abs(SignCheck$SignCount - 5) + 5
  
  # Order the table by error improvement
  
  ErrorTable03 = ErrorTable03[order(ErrorTable03$ImprovementMean, decreasing = TRUE), ]
  
  # Find the term that most consistently improves the error

  ChosenTerm = ErrorTable03$Variable.y[(0 < ErrorTable03$ImprovementMean) & (ErrorTable03$Variable.y %in% SignCheck$Variable[SignCheck$SignCount == 10]) & (ErrorTable03$ImprovementCount == 10)][1]

  # If nothing is chosen, break the looping condition
  
  if (is.na(ChosenTerm) == TRUE)
  {
    
    LoopingCondition = FALSE
    
  }
  
  else
  {

    # Check if it is a forward pass
    
    if ((ChosenTerm %in% attr(terms(Model), which = 'term.labels', exact = TRUE)) == FALSE)
    {
      
      # Create the new formula
      
      Newformula = as.formula(paste(as.character(as.call(as.formula(Model$formula))[2]), ' ~ ', paste(c(as.character(as.call(as.formula(Model$formula))[3]), ChosenTerm), collapse = ' + '), sep = ''))
      
    }
    
    # Otherwise it is a backwards pass
    
    else
    {
      
      # Obtain the terms in the formula
      
      Terms = trimws(unlist(strsplit(x = as.character(as.call(as.formula(Model$formula))[3]), split = '+', fixed = TRUE)))
      
      # Create the new formula
      
      Newformula = as.formula(paste(as.character(as.call(as.formula(Model$formula))[2]), ' ~ ', paste(Terms[Terms != Variable], collapse = ' + '), sep = ''))
      
    }
    
    # Create a model with the updated formula
    
    Model = update(Model, formula = Newformula)

    # Initialize the check to see if models are being repeated
    
    LoopingCheck = c()
    
    if (0 < length(CrossValidationHistory))
    {
      
      for (i in seq(length(CrossValidationHistory)))
      {
        
        # Check if the terms of the current model are a subset of a previous model
        
        LoopingCheck = c(LoopingCheck, as.logical(prod(row.names(summary(Model)[['coefficients']]) %in% CrossValidationHistory[[i]][['ParameterEstimateTable']][['Parameter']][is.na(CrossValidationHistory[[i]][['ParameterEstimateTable']][['Variable']]) == TRUE])))
        
        # Check if the terms of a previous model are a subset of the current model
        
        LoopingCheck[length(LoopingCheck)] = LoopingCheck[length(LoopingCheck)] & as.logical(prod(CrossValidationHistory[[i]][['ParameterEstimateTable']][['Parameter']][is.na(CrossValidationHistory[[i]][['ParameterEstimateTable']][['Variable']]) == TRUE] %in% row.names(summary(Model)[['coefficients']])))
        
      }
      
    }
    
    else
    {
      
      LoopingCheck = FALSE

    }

    # If both of the above are true for a previous model, then the cross validation is looping

    if (as.logical(pmin(sum(LoopingCheck), 1)) == TRUE)
    {
      
      LoopingCondition = FALSE

    }

    # Update the cross validation history
    
    CrossValidationHistory = c(CrossValidationHistory, list(list(ErrorTable = ErrorTable, ParameterEstimateTable = ParameterEstimateTable)))        

    # If a record of cross validation exists, then load it
    
    if (file.exists(paste(OutputDirectory, '/CrossValidation.xlsx', sep = '')) == TRUE)
    {
      
      Workbook = loadWorkbook(paste(OutputDirectory, '/CrossValidation.xlsx', sep = ''))

    }
    
    # Otherwise create a new workbook
    
    else
    {
      
      Workbook = createWorkbook()
      
    }
    
    # Add the new error and parameter estimate data
    
    addWorksheet(Workbook, paste('ErrorTable', Round, sep = ''))
    addWorksheet(Workbook, paste('ParameterEstimateTable', Round, sep = ''))
    
    writeData(Workbook, paste('ErrorTable', Round, sep = ''), CrossValidationHistory[[length(CrossValidationHistory)]][['ErrorTable']])
    writeData(Workbook, paste('ParameterEstimateTable', Round, sep = ''), CrossValidationHistory[[length(CrossValidationHistory)]][['ParameterEstimateTable']])

    # Record the results thus far
    
    saveWorkbook(Workbook, paste(OutputDirectory, '/CrossValidation.xlsx', sep = ''), overwrite = TRUE)
    
    # Update the round
    
    Round = Round + 1

  }

}

# Turn off the cluster

stopCluster(Cluster)

# Confirm the cluster is gone

showConnections()

# Read in the machine learning models

xgboostModel = fread(paste(InputDirectory, '/xgboostModel.csv', sep = ''))

nrow(DataSet)
ncol(DataSet) + 1

DataSet = merge(DataSet, xgboostModel, by = c(Key))

nrow(DataSet)
ncol(DataSet)

rm(xgboostModel)
gc()

rangerModel = fread(paste(InputDirectory, '/rangerModel.csv', sep = ''))

nrow(DataSet)
ncol(DataSet) + 1

DataSet = merge(DataSet, rangerModel, by = c(Key))

nrow(DataSet)
ncol(DataSet)

rm(rangerModel)
gc()

# Observe the correlation with a model fitted on the training data and applied to the different data sets

cor(DataSet$SalePrice[TrainTestVector], DataSet$xgboost[TrainTestVector])
cor(DataSet$SalePrice[TrainTestVector], DataSet$ranger[TrainTestVector])
cor(DesignMatrix04$NormalizedTarget[TrainTestVector], predict(update(Model, data = DesignMatrix04[TrainTestVector, ]), DesignMatrix04[TrainTestVector, ], 'response'))

cor(DataSet$SalePrice[!(TrainTestVector)], DataSet$xgboost[!(TrainTestVector)])
cor(DataSet$SalePrice[!(TrainTestVector)], DataSet$ranger[!(TrainTestVector)])
cor(DesignMatrix04$NormalizedTarget[!(TrainTestVector)], predict(update(Model, data = DesignMatrix04[TrainTestVector, ]), DesignMatrix04[!(TrainTestVector), ], 'response'))

# Export the newly created data and its attendant meta data

fwrite(x = DesignMatrix04, file = paste(OutputDirectory, '/DesignMatrix03.csv', sep = ''))

Workbook = loadWorkbook(paste(OutputDirectory, '/MetaData.xlsx', sep = ''))

addWorksheet(Workbook, 'earthModel')
writeData(Workbook, 'earthModel', ModelInformation)

addWorksheet(Workbook, 'ExpandedearthModel')
writeData(Workbook, 'ExpandedearthModel', ExpandedModelInformation)

addWorksheet(Workbook, 'SelectedKnotTranslation')
writeData(Workbook, 'SelectedKnotTranslation', KnotTranslation)

addWorksheet(Workbook, 'OriginalVariableModel')
writeData(Workbook, 'OriginalVariableModel', ExpandedModelInformation02)

addWorksheet(Workbook, 'MissingValue')
writeData(Workbook, 'MissingValue', data.table(MissingValue = MissingValue))

addWorksheet(Workbook, 'StepwiseTerms')
writeData(Workbook, 'StepwiseTerms', data.table(SelectedTerm = SelectedTerms))

addWorksheet(Workbook, 'CrossValidatedTerms')
writeData(Workbook, 'CrossValidatedTerms', data.table(SelectedTerm = attr(terms(Model), which = 'term.labels', exact = TRUE)))

saveWorkbook(Workbook, paste(OutputDirectory, '/MetaData.xlsx', sep = ''), overwrite = TRUE)