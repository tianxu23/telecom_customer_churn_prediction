# telecom_customer_churn_prediction
to predict which customer would churn in the selected dataset of a telecommunication company.

Customer churn, also known as customer attrition, happens when customers stop using the services or
products of a business. Customer churn is prevalent in the telecommunication industry because the
market is highly saturated and the ability of telecommunication companies to differentiate their
products is disappearing . Customer attrition directly cuts into companies’ revenue and market share. 1
Therefore, telecommunication companies are relying heavily on analytics to identify customers who
have a high probability of churning so that they can introduce appropriate incentives to retain those
customers. Retaining customers is usually less costly than acquiring new ones. This is the purpose of
our project: to predict which customer would churn in the selected dataset of a telecommunication
company.

Our data is a customer dataset of a telecommunication company retrieved from Kaggle.com . The
dataset has 100,000 rows with each row representing one customer. There is no duplicate. There are
100 variables including 21 factor variables, 79 numeric or integer variables and no character variable.
Our target variable is the binary variable churn with value 0 as stay and 1 as churn. Other variables can
be categorized into four groups: personal information, usage behaviors, pricing, and phone conditions.
There are a lot of missing values for some variables such as ownrent , lor and income . The amount of
missing values ranges from 1 to 30,190 values.

To clean the data, we converted categorical variables to factors and recategorized blank values as NAs.
Afterward, we proceeded to deal with NAs in three steps:
● First, we run a simple regression with churn as the dependent variable and each of the variables
with missing values as the independent variable
● If the regression coefficient is insignificant either statistically or economically, we conclude that
those variables have little relationship with churn . Thus, we drop them from our dataset
● If the regression coefficient is significant economically and statistically, we conclude that the
variable has some relationship with churn . Those variables are hnd_price (handset price) and
avg6qty (average monthly number of calls over the past 6 months). The amounts of missing
values for hnd_price and avg6qty are 847 and 2839 respectively (less than 3% of the data)
Therefore, we delete the rows with missing values of these two variables as deletion of less
than 3% of the data does not affect the power of our analysis significantly. As a result, our clean
dataset consists of 93,931 observations and 89 variables.

Our data has 89 variables and therefore, we used Lasso Regression for dimension reduction before we
start with the classification. After running lasso regression, we are left with 40 variables, with each
categorical variable converted into a dummy variable. We then ran a logistic regression model on the
data with churn as the dependent variable and all the remaining variables from the clean dataset as
the independent variables. Further, we used backward selection to eliminate insignificant variables,
along with k-fold cross validation on the training set to get the AIC and model accuracy (in-sample
accuracy = 63% & out of sample accuracy = 59.03%). Since, the overall accuracy of the model seemed
low, we tried other classification techniques such as Decision Tree, Random Forest, Neural Networks
and Gradient Boosting Machines (GBM). In the end, we chose Gradient Boosting (gbm function in R) as
our final model as it gave us the best ‘out of sample’ accuracy (more information in Evaluation part).
Gradient Boosting: It is a machine learning algorithm that builds an ensemble of shallow and weak
successive trees, wherein, each tree learns and improves from the previous one. This proves to be a
very powerful technique as these weak successive trees eventually give a strong algorithm, with much
improved predictive capabilities.
For our GBM model on the training dataset, we used the gaussian distribution, with learning rate of
0.1, 7000 iterations and max depth of 3. In order to improve the prediction of the model, we also
added a cross validation of 5 folds within the GBM method.
Key findings from all of our models:
● Our CART model demonstrates that the only classifier of churn customers is number of days
(age) of current equipment (see Exhibit 2, Appendix)
● Based on logistic regression, the longer a customer has been with the company, the lower are
the chances of him/her churning
● A customer having a 16-17 year old kid, is less likely to churn
● Unmarried individual is more likely to churn when compared with a married or single individual
● A customer with new cell phone is less likely to churn than a customer having an old cell phone

The telecommunication company can use our model to identify the customers who are likely to churn
and take preventive measures to retain the ones that are on the verge of churning. In the long run, the
company can also use some of our insights to target the right customers by identifying the ones that
bring the most value to the company.
The accuracy with which our model predicts the customers likely to churn is extremely important as
the company will be spending a lot of resources on retaining these customers. Therefore, we have
taken the following measures to ensure maximum accuracy possible: K-fold evaluation and out of sample accuracy





