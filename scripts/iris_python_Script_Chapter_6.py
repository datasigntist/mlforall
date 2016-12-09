
# 
#   Created : 6-Dec-2016
#

import numpy as np
import matplotlib.pyplot as plt

####Script Part 6.1
from sklearn import datasets

iris = datasets.load_iris()
print(iris.feature_names)
X = iris.data
print(iris.target_names)
y = iris.target
print('Shape of X %d rows  %d columns'%X.shape)
print(X[0],iris.target_names[y[0]])
#########################################

####Script Part 6.2
def sigmoid(z):
 return 1/(1+np.exp(-z)) 

dataSet = np.arange(-10.0,10.0,0.1)
sigmoiddataSet = sigmoid(dataSet)

plt.plot(dataSet,sigmoiddataSet)
plt.show()
#########################################

####Script Part 6.3
dataSet = np.arange(0.0,1.0,0.01)

plt.plot(dataSet,-np.log(dataSet))
plt.show()
#########################################

####Script Part 6.4
dataSet = np.arange(0.0,1.0,0.01)

plt.plot(dataSet,-np.log(1-dataSet))
plt.show()
#########################################

####Script Part 6.5
X = X[y!=2,:]
y = y[y!=2]
from sklearn.linear_model import LogisticRegression
logistic = LogisticRegression()
logistic.fit(X,y)
print('Predicted value of %s is %s'%(X[1,:],iris.target_names[logistic.predict_proba(X[1,:]).argmax()]))
#########################################