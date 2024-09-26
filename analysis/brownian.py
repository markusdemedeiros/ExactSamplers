import pandas as pd
from fractions import Fraction
import matplotlib.pyplot as plt

def get_lb(df):
    return float(Fraction(int(df['approx']) - 1, 4 ** df['quad-digits']))

def get_ub(df):
    return float(Fraction(int(df['approx']) + 1, 4 ** df['quad-digits']))

df = pd.read_csv('./brownian_100.csv')
df['lb'] = df.apply(get_lb, axis=1)
df['ub'] = df.apply(get_ub, axis=1)


N = df['iterate'].max() - 1

# Remove me
df['mean'] = (df['ub'] + df['lb']) / 2.0
df_filtered = df.loc[df['iterate'] == N]
data = list(df_filtered['mean'])
xax = [float(x) / float(N) for x in range(len(data))] # close enough. should export tn creal but whatever

plt.plot(xax, data)
plt.show()






# print(df.tail(10).to_string())

# iterate, quad-digits, approx
#
#
#
#
