# explore.py

import pandas as pd 
from sklearn import linear_model as lm
from sklearn import svm 
from sklearn import preprocessing as prep

dat = pd.read_table('data/anthers_12_21_2016_wiiuranked_patch1.1.6.tsv')

dat2 = dat[['p1_rating', 'p2_rating', 'game_winner']].dropna()
dat2['rat_diff'] = dat2['p1_rating'] - dat2['p2_rating']
dat2 = dat2[dat2['game_winner'] != 3]

X = dat2[['p1_rating', 'p2_rating', 'rat_diff']]
y = dat2['game_winner']

X_std = prep.StandardScaler().fit_transform(X)
X_std2 = prep.StandardScaler().fit_transform(X['rat_diff'].reshape(-1,1))

# try a logistic regression model
lr_model = lm.LogisticRegression().fit(X,y)
print lr_model.score(X,y)
print lr_model.coef_

lr2 = lm.LogisticRegression().fit(X['rat_diff'].reshape(-1,1), y)
print lr2.score(X['rat_diff'].reshape(-1,1),y)
print lr2.coef_

lr3 = lm.LogisticRegression().fit(X_std, y)
print lr3.score(X_std,y)
print lr3.coef_

lr4 = lm.LogisticRegression().fit(X_std2, y)
print lr4.score(X_std2,y)
print lr4.coef_

# try an SVM
# runs really slowly
# svm_model = svm.SVC().fit(X,y)
# print svm_model.score(X,y)