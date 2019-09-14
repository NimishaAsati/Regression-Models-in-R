# Regression-Models-in-R
Linear and Logistic Regression on different datasets in R


## Linear Regression and Gradient Learning

#### Regression model that includes two linear features and one quadratic feature and Gaussian Noise.
##### Goal: 
		1) To develop a gradient descent learning algorithm that will estimate the best parameters theta.
		2) Find the two best input features for predicting the output mpg using algorithm developed and R's built-in function LM.
		3) Compare the difference between your full coding and R's built-in function call: lm.
		4) Detect best features using batch gradient descent algorithm by tuning learning rate.
		5) Detect best features using batch stochastic descent algorithm by tuning learning rate.

###### Data: Auto dataset --> ISLR package in R
		Gas mileage, horsepower, and other information for 392 vehicles.
		A data frame with 392 observations on the following 9 variables.
    
    
## Logistic Regression and Evaluations

###### Data: Weekly dataset in ISLR package
		It contains 1,089 weekly returns for 21 years from the beginning of 1990 to the end of 2010.
		Use its 1990-2008 as a training data and 2009-2010 as a test data.
		
##### Goal:
		1) Given the training data, we have to perform a logistic regression where 
			the input features are five of Lag variables and Volume, and the binary output is Direction. 
			Report the confusion matrix and the accuracy on both training and test data given the learned model.
		2) Run logistic regression five times with only one input features Lagj(1 <= j <= 5) for each time. 
			Compute the confusion matrix and the accuracy on both training and test data given each of the learned models. 
			Which are the best models in terms of the accuracy and F-score.
		3) Draw six ROC curves for 6 models from the part (c) and (d) with varying thresholds. 
			Determine the best model in terms of the Area Under Curve (AUC).
		4) Draw six Precision-Recall curves for 6 models from the part (c) and (d) with varying thresholds. 
			Determine the best model in terms of the Area Under Curve (AUC).
			
###### Results:

	Best Models:
		In terms of accuracy on the test data, 2nd model, where we are training on Lag2 variable alone, is giving the best result.
		In terms of FScore on the test data also, 2nd model is giving the best result.
	No, the 2nd model does not achieve the best accuracy and FScore on the training data. Both 3rd and 4th model, 
	which are trained on Lag3 and Lag4 variables respectively, gave the best accuracy and FScore on the training data.
	
	AUC Values:
	
• Overall Model = 0.554

• Model 1 = 0.536

• Model 2 = 0.535

• Model 3 = 0.507

• Model 4 = 0.510

• Model 5 = 0.521

	Based on the AUC, overall model, which was trained on 6 variables (all Lag and Volume variable), 
	is the one giving the best result.
  
  
  #### To execute the code, download the R file and run it on R Studio.
    
    
    
