import pandas as pd
from fractions import Fraction
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.animation as animation

df = pd.read_csv('./list_sorting_1.csv', names=['iterate','lb','ub',''])

ani_folder = "ani"


N = df['iterate'].max()
t = list(range(1, N + 1))

fig = plt.figure()
board = plt.axes(xlim=(0, 100), ylim=(0, 25))

for p in t:
    plt.axis('equal')
    ax = plt.gca()
    ax.set_xlim(0, 1)
    ax.set_xticks(range(0, 0, 1))
    ax.xaxis.set_tick_params(length=0, width=0)
    ax.get_yaxis().set_visible(False)
    ax.set_aspect('equal')
    for pos in ['right', 'top', 'bottom', 'left']:
        plt.gca().spines[pos].set_visible(False)


    df_p = df[df['iterate'] == p]
    for _, row in df_p.iterrows():
        lb = row['lb']
        ub = row['ub']
        mean = (lb + ub) / 2.0
        diff = mean - lb

        circle = plt.Circle((mean, 0), diff, fc='red', zorder=10)
        board.add_patch(circle)

    plt.plot([0, 1], [0, 0], color='black', linewidth=0.25)
    plt.text(0, 0.3, "n = {}".format(str(len(df_p))), dict(size=20))

    plt.draw()
    N = len(df_p)
    plt.savefig(f'ani/{N:05}.png', dpi=300)
    plt.cla()

# to combine:
# convert -delay 20 -loop 0 ani/*.png result.gif

print("clear the {} folder!".format(ani_folder))
