#print history
#import readline; print('\n'.join([str(readline.get_history_item(i + 1)) for i in range(readline.get_current_history_length())]))
# will read 2 column file and plot profile
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

iad=pd.read_csv('/lfs/h2/emc/da/noscrub/dagmar.merkova/python/iad.csv', delimiter =" ", names=['p','t'])

iad.plot(x="t",y="p", c='red',marker='.')
plt.yscale('log')
plt.gca().invert_yaxis()
plt.savefig('obr.png')
