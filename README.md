# All-Machine-Learning-Algorithms-explained-in-a-project
Application of Decision Tree C5.0, Random Forest, K-NN, Artificial Neural Network, Naive-Bayes algorithms in a Project using R


An Automobile company is thinking of launching its existing products (P1, P2, P3, P4 and P5) into
a new market. Consumer behavior of new market is similar to the existing market. The company is
having consumer data of its existing customers and 500 new customers have been identified in the
new market who may buy its products. The marketing manager has clear understanding that it is the
income level which drives the purchase of different cars. Based on existing data and a recent survey
he created the following table. The cell values are probability of purchasing the products
Income Level P1 P2 P3 P4 P5
Low 90% 7% 3% 0% 0%
Medium 43% 33% 23% 1% 0%
High 0% 3% 22% 24% 51%
Price of the products are as given below:
P1 P2 P3 P4 P5
Price 160000 230000 360000 620000 900000
The manager has decided that income group 0,1,2,3 belong to “Low” category, 4,5,6 belong to
“Medium” category and 7,8 belong to “High” category. Being an MBA graduate, he could
formulate three basic strategies to design the campaign:
1. All belong to low income group (total expected revenue is 8.54 Cr)
2. All belong to high income group (total expected revenue is 34.69 Cr)
3. All belong to medium income group (total expected revenue is 11.68 Cr)
However, he knows that the disadvantage of running same campaign to different customer
segments. Hence he thought of running three different campaigns for three different income groups.
Based on his prior experience, he could estimate the cost of loss of goodwill if he runs one
campaign for a segment of customer for which the campaign is not meant for. The following table
gives the cost matrix as cost per customer.
Low Income Medium Income High Income
Campaign 1 NIL 150000 300000
Campaign 2 60000 NIL 210000
Campaign 3 70000 120000 NIL
Help the manager to target right customers with right campaign for the new customers i.e. predict
the income group of the new customers (it must be either “Low” or “Medium” or “High”). Also,
estimate the expected revenue.
Even though the variables are all integers, they are actually categorical in nature. Moreover, the
datasets contain missing values. While building the model, make sure to put na.action=NULL in
both train() function and predict() function (provided you use caret package). If you forget to put
na.action=NULL while predicting the class of new customers, not all customer will be evaluated by
the model and that is not accepted. Remember, not all models can handle missing values in data.
Your final objectives:
1. Predict income class of 500 new customers
2. Estimate expected revenue from these 500 customers
