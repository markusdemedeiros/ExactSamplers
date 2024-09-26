import pandas as pd
from fractions import Fraction
import matplotlib.pyplot as plt
import numpy as np

def get_lb(df):
    return float(Fraction(int(df['approx']) - 1, 4 ** df['quad-digits']))

def get_ub(df):
    return float(Fraction(int(df['approx']) + 1, 4 ** df['quad-digits']))

df = pd.read_csv('./brownian_1000.csv')
df['lb'] = df.apply(get_lb, axis=1)
df['ub'] = df.apply(get_ub, axis=1)

N = df['iterate'].max() - 1

cmap = plt.get_cmap('rainbow')
colors = cmap(np.linspace(0,1,11))

df['mean'] = (df['ub'] + df['lb']) / 2.0  # Remove me, graph error bounds instead?
for i in set(df['iterate']):
    df_filtered = df.loc[df['iterate'] == i]
    data = list(df_filtered['mean'])
    # close enough. could export tn from creal but whatever
    xax = [float(x) / float(N) for x in range(len(data))]
    plt.plot(xax, data, c=colors[i], linewidth=0.5)

xax = [float(x) / float(N) for x in range(len(data))]
yax = [0.0 for _ in xax]
plt.plot(xax, yax, c='black')

# plt.savefig('brownian_1000.pdf')
plt.show()
