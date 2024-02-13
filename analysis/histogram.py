#!/usr/bin/env python3
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

bins = 250
log = 'log.csv'
out = 'histo.pdf'

df = pd.read_csv(log, header=None)

# uniform bins 
# ax = df[0].plot(kind='hist', bins=bins, histtype='step')

# integer bins 
ax = df[0].plot(kind='hist', bins=np.arange(min(df[0]), max(df[0]) + 1, 1))

fig = ax.get_figure()
fig.savefig(out)
