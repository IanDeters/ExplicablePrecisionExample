# ExplicablePrecisionExample
This repository contains code illustrative of how to create predictive models which are simultaneously explicable and precise.

The code in this repository corresponds to the popular House Prices - Advanced Regression Techniques Kaggle competition.  Its intention is to show how to construct an easy-to-understand model (i.e. explicable) whose precision rivals that of techniques such as gradient boosting and random forest.  While I have previously given lectures on the theory behind such models, this represents a first example of implementing that theory.  Of particular importance is the minimal amount of input needed from the user.  It is not fully automated since some input is needed from the user.  For example, while the program guesses the conceptual type of the variables (i.e. categorical or continuous), the user may need to correct those guesses.  Based on the time constraints of the user, the user may wish to engage in more or fewer rounds of more or less intense machine learning (e.g. gradient boosting, random forest) model construction.  Sans the inevitable minor, periodic input, what can be automated, has been automated.  In particular, the bookkeeping, easily the most laborious component of such projects, is automated.

When examining this code, there are a few things to keep in mind.
1. This code is meant to be illustrative of how to create explicable precise models.  It is not meant as endorsing a particular machine learning technique or method of hyperparameter generation.  Those may be changed in accordance with the user’s judgment and constraints.
2. A MARS model was selected as the final model form since, in my opinion, such models are easy to understand.  However, model opacity, unlike model precision is a function of the customer’s, not the analyst’s, understanding.  The final form of the model should be selected with that in mind.
3. The flow of the code should be understood as moving from quickly produced inexplicable models to more carefully produced explicable models.  As you proceed through the code, fewer and fewer models are produced, but these models are increasingly easier to understand.
4. The final explicit model is given in the last bit of code.  While the point of this repository is to illustrate how to produce explicable, precise models, in practice, the first model is not typically what will be used by the customer.  Rather, it serves as a starting place for conversation and more assiduous, ad hoc analysis.  In general, one should never blindly trust the output of a computer.

A description of the code is as follows.  This code should be run with reference to R Methods - GitHub 03.xlsx.

1. 01. Data Profiling And Initial Transformations.R – This code does 6 things
  a. Records the target, weight, and offset variables
  b. Profiles the data
  c. Updates the meta data as the user deems fit
  d. Standardizes the data by replacing continuous variables with their empirical cumulative distribution functions, including missing indicators, and replacing all categorical variables with numeric versions of their values
  e. Creates a design matrix from the standardized data
  f. Writes all of the derived data sets and attendant meta data to the disk
2. 02. xgboost Modeling – This code does 3 things
  a. Illustrates the mechanics of the xgboost algorithm
  b. Performs repeated rounds of hyperparameter optimization
  c. Writes the model output and attendant hyperparameters to the disk
3. 03. RandomForest Modeling.R – This code does 3 things
  a. Illustrates the mechanics of the ranger algorithm
  b. Performs repeated rounds of hyperparameter optimization
  c. Writes the model output and attendant hyperparameters to the disk
4. 04. Minimal Weight Determination.R – This code does 5 things
  a. Imports the xgboost and random forest models and creates an ensemble model
  b. Groups records together based on the various models
  c. Examines the efficacy of those groups on holdout data
  d. Coarsens the groups to examine the impact of increasing the minimal amount of weight in each group
  e. Writes the results to the disk
5. 05. Group Categorical Variables.R – This code does 4 things
  a. Groups the values of the categorical variables on the basis of the machine learning models and the user selected minimum weight
  b. Attends to a few stray values present in the holdout data but not in the training data
  c. Creates a new design matrix on the basis of the newly grouped categorical variables
  d. Writes all of the derived data sets and attendant meta data to the disk
6. 06. Initial Piecewise Linear Models.R – This code does 8 things
  a. Constructs various MARS models up to a specified degree until no more precision is extracted
  b. Illustrates the mechanics of the MARS models
  c. Creates all terms indicated by the MARS algorithm
  d. Translates the MARS selections back to the original variables and creates the attendant design matrix
  e. Stepwise selects among the MARS selected original variable transformations for models of increasing interaction degree, using the previous models as offsets
  f. Cross validates among the stepwise selected terms
  g. Compares the cross validated model to the machine learning models
  h. Writes all of the derived data sets and attendant meta data to the disk
7. 07. Prepare Exhibits.R – This code does 3 things
  a. Creates univariate exhibits for the categorical variables
  b. Creates univariate exhibits for the continuous variables
  c. Explicitly creates the model on the original data
