# Importer les librairies
import pandas as pd 
import numpy as np
import os
import matplotlib
import h5py
pip install pytables

# Charger la base de données
path_h5 = r"C:\Users\lajoi\Documents\0_PARCOURS UNIVERSITAIRE\MASTER 2024-2026\UPEC - M1 -  2S - MASERATI DA\Mémoire M1\5 - Programmation\0__DataBaseManagement__\__DataBaseUKDALE__\UK-DALE-disaggregated\ukdale.h5"
h5 = h5py.File("ukdale.h5")

# structure de la base de données
filename_hdf = 'ukdale.h5'

def h5_tree(val, pre=''):
    items = len(val)
    for key, val in val.items():
        items -= 1
        if items == 0:
            # the last item
            if type(val) == h5py._hl.group.Group:
                print(pre + '└── ' + key)
                h5_tree(val, pre+'    ')
            else:
                try:
                    print(pre + '└── ' + key + ' (%d)' % len(val))
                except TypeError:
                    print(pre + '└── ' + key + ' (scalar)')
        else:
            if type(val) == h5py._hl.group.Group:
                print(pre + '├── ' + key)
                h5_tree(val, pre+'│   ')
            else:
                try:
                    print(pre + '├── ' + key + ' (%d)' % len(val))
                except TypeError:
                    print(pre + '├── ' + key + ' (scalar)')

with h5py.File(filename_hdf, 'r') as hf:
    print(hf)
    h5_tree(hf)