This project aims to evaluate various machine learning models for predicting heart disease and provide a critical analysis 
of the field to identify gaps and recommend approaches to improve accuracy and patient outcomes. A large and unpopular medical 
dataset of “Personal key factors for Heart Disease” was selected for this study as it provides uncommon features and a high 
imbalance to the target feature. Three machine learning approaches were attempted: Naïve Bayes, Decision Tree, and SVM, with 
three alternatives for each approach namely the basic model, regularised model, and cross-validated with hyperparameter tuned 
model. The proposed models were evaluated and compared to models proposed by other researchers. The results had shown that the 
cross-validation and hyperparameter-tuning have provided minor or no improvement as compared to the baseline models. Aside from 
that, the smoothing technique has not impacted the performance of the Naïve Bayes while complexity parameter control provides 
some improvement on the decision tree method in terms of accuracy, precision, and F1-score. In terms of the machine learning 
approach, the proposed linear SVM provided the best performance across all the proposed models with a value of 0.763, 0,791, 
0,741, 0765, and 0.764 for accuracy, precision, recall, F1-score, and AUC measures. However, the linear SVM was outperformed 
by other previously proposed models that utilised oversampling approach rather than the attempted random undersampling method 
in this study. This strongly suggests that the oversampling method is more beneficial for a large and imbalanced dataset.
