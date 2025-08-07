# Machine-learning-approach-for-analyzing-factors-associated-with-global-poverty.

Poverty is a condition where people or a community lacks financial resources and essentials
to acquire basic needs, education, medicine and access to technology. There are many
researches done on poverty but mainly using economic factors. The World bank has proven
necessities to analyze poverty based on multidimensional factors not only economic factors.
Poverty headcount ratio calculated using the international poverty cut off $1.90 per day is
an important measure to determine poverty rate. In this research, percentage of population
below the poverty line or percentage of poverty headcount ratio was used as the main
variable of interest to analyze along with the relevant explanatory variables. The dataset
consists of data for poverty percentage for 163 countries given by the World bank for 13
variables.

Initially an exploratory data analysis was done to find the relationship among the response
and explanatory variables. Since the analysis is based on world prospect, data related to
some countries were not available. Those data were imputed using multiple imputation
(CART method) after identifying the nature of the missingness as Missing at Random
(MAR). In the model fitting phase, machine learning models Random forest, XGBoost and
Neural Networks were fitted on data. At the end of the research it was found that variables
Agriculture productivity index, Health care index, Environmental Performance index and
Literacy rate are the most important variables to predicting poverty.

<img width="542" height="266" alt="Screenshot 2025-08-07 at 3 16 56 PM" src="https://github.com/user-attachments/assets/521f3deb-363f-483e-ac36-23da65a3f10c" />
<img width="593" height="292" alt="Screenshot 2025-08-07 at 3 17 14 PM" src="https://github.com/user-attachments/assets/2427098d-7d41-4684-90e0-80dd947d1f0d" />
