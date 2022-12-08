####print history (for interactive python only: print history on python commandline) 
####import readline; print('\n'.join([str(readline.get_history_item(i + 1)) for i in range(readline.get_current_history_length())]))
##  This script  will read 2 column file and plot profile
##########################################################
#import python modules
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

myfile=pd.read_csv('/lfs/h2/emc/da/noscrub/dagmar.merkova/python/iad.csv', delimiter =" ", names=['p','t'])
myfile2=pd.read_csv('/lfs/h2/emc/da/noscrub/dagmar.merkova/python/ctr/iad.csv', delimiter =" ", names=['p','t'])

#plotting profile 
myfile.plot(x="t",y="p", c='red',marker='.')
# setting log scale  - optional
plt.yscale('log')
# inverting axis
plt.gca().invert_yaxis()
#saving figure in the file
plt.savefig('obr.png')
#if you work in interactive mode and want to see picture, use following line (- need set terminal - have problem now)
#plt.show()
