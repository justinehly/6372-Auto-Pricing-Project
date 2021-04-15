# 6372-Auto-Pricing-Project

### Collaborators
1. Renfeng Wang
2. Allen Miller
3. Justin Ehly

### Project Scope

#### Goal: 
Build data models to understand and predict the retail price of a vehicle using various attributes.

#### Data:
[Data1](https://github.com/justinehly/6372-Auto-Pricing-Project/blob/main/DataSets/data1.csv)

#### Learning Objective 1: 
Display the ability to build regression models using the skills and discussions from Unit 1 and 2 with the purpose of identifying key relationships and interpreting those relationships.  A key question of interest that must be addressed in this analysis is the importance of the "Popularity" variable.  While the details of this variable are vague, it was created from social media, and the "higher ups" are curious how much general popularity can play a role in the retail price of a vehicle.   
.	Build a model with the main goal to identify key relationships and is highly interpretable.  Provide detailed information on summary statistics, EDA, and your model building process. 
.	Provide interpretation of the regression coefficients of your final model including hypothesis testing, interpretation of regression coefficients, and confidence intervals. It's also good to mention the Practical vs Statistical significance of the predictors.  Answer any additional questions using your model that you deem are relevant.
.	The training data set can be used for EDA and model fitting while the test set can be used to help compare models to make a final call.  There is no need to use the validation data set for this objective.

#### Learning Objective 2:
While your model from Objective 1 may be interpretable there may be some additional complexity that you could incorporate to your model so that it can predict better at the expense of interpretations.  The purpose of this objective is to go through a process to compare multiple models with the goal of developing a model that can predict the best and do well on future data.  
.	Use the training and test set to build at least one additional multiple linear regression model that attempts to find a model with additional complexity than the interpretable model of objective two.  The point here is to make sure we understand how to add complexity to a linear regression model.   Hint:  Its not just including a model with predictors that you've eliminated from Objective 1.
.	I want you to use the ISLR text book (and the googe machine) and read up on one nonparametric technique to build a regression model.  I want you to select from k-nearest neighbors' regression or regression trees. There is a chapter on trees in the ISLR book.  For information on knn regression, see Section 3.5.  It is important to know that knn can be applied to regression as well as classification problems.  Make sure your implementation in R is using the knn regression versions rather than the classification.  See me on this if you need help or reassurance.  You will use the training and test sets here to help determine the most appropriate tree or the most appropriate "k" in knn. 
.	At this point you should have at least 3 models, 2 linear regression models and 1 nonparameteric.  For each of the three models, provide measures of fit for comparisons:  test ASE and validation ASE are mandatory.  You may also include additional metrics for completeness like R squared/Adjusted Rsquared, AIC, and BIC where applicable.  This final to-do is the only point where the validation set is being used.  This is because we are truly validating our decisions that have been made using the training and test set.   It is important to do this step last when you have completed all other tasks.  Do not continue to update models to get your validation ASE to be smaller.  Just report it and offer a recap of the comparison of the results suggests.  Additional insight as to why one model is better than the other, or why they are all the same is encourage.

#### Additional Learning Objectives:
* Utilize R for deep data cleaning, EDA and visualizations
* Publish paper for final term grade
