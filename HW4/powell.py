import numpy as np
import pandas as pd
from scipy.optimize import minimize

data = pd.read_csv('rot.csv', names=['r', 'v'], header=0)
r = data.r.to_numpy()
v = data.v.to_numpy()

def fun(x):
    return np.sum((v - x[0]*(1 - np.exp(-r/x[1])))**2)

x0 = [0, 0]
print(minimize(fun, x0, method='Powell'))
