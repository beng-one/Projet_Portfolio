# Importer les librairies
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns
from matplotlib.animation import FuncAnimation

# Fonction associée au calul des termes de la suite logistique

def fct_verhulst(Xn, p):

    '''
    Fonction mathématique appelée suite logistique << chaotique >>. 
    Elle a notamment été utilisée dans la modélisation de l'évolution des populations biologiques en temps discret et l'étude asymptotique des systèmes dynamiques.
    L'équation de récurrence de la suite s'écrit comme suit : X(n+1) = p*X(n)*(1-X(n))    
    -------------
    p : le paramètre qui varie entre 0 et 4 pour observer  de façon mathématique ssantes qui se réfèrents à l'évolution des populations. 
    Xn1 : Le terme (n+1) de la suite de logisique. A noter que le terme initial doit est compris entre 0 et 1 :: (0=< X(0) <=1).
    '''
    Xn1 = p*Xn*(1-Xn) 
    return Xn1

# Fonction associée au graph de la suite logistique

def fct_graphe_Verhulst(N, X0, p):

    '''
    Fonction pour tracer le graphe de la suite de Verhuslt.
    ----------------
    N : Le nombre de termes que la suite comporte
    X0 : Le terme initial de la suite qui doit varier entre 0 et 1.
    p : Le paramètre de l'évolution de la population qui varie entre 0 et 4.
    
    '''
    iter = 0
    vect_Xn = []
    vect_Xn.append(X0)

    while iter != N:
        Xn1 = fct_verhulst(vect_Xn[iter], p=p)
        vect_Xn.append(Xn1)
        iter += 1
    
    plt.plot(vect_Xn, 'bo-')
    plt.grid(True)
    plt.xlabel(f"{N}")
    plt.ylabel(f"Xn")
    plt.title(f"N={N}, X0={X0}, p={p}")
    plt.show()


## Fonction associée à la représentatin en escalier de la suite logistique

def fct_escalier(N, X0, p):
    '''
    Fonction pour tracer la représentation en escalier de la suite de logistique.
    ------------------
    N : Le nombre de termes que la suite comporte (256 par défaut)
    X0 : Le terme initial de la suite qui doit varier entre 0 et 1.
    p : Le paramètre de l'évolution de la population qui varie entre 0 et 4.
    '''

    # Vecteur des coordonnées pour la droite D (x ET y) et la courbe Cf (x)
    vect_abs = np.linspace(0,1,N) 

    # Vecteur des ordonnées de la courbe Cf (y)
    vect_graph_cf = [fct_verhulst(Xn=x, p=p) for x in vect_abs]
    
    # Vecteur des coordonées de la fonction escalier de la suite de Verhulst (x Et y)
    iter = 0
    vect_Xn = []
    vect_Xn.append(X0)

    while iter != N:
        Xn1 = fct_verhulst(vect_Xn[iter], p=p)
        vect_Xn.append(Xn1)
        iter += 1

    # Représentation graphique sous forme d'escalier de la suite de Verhuslt 
    plt.plot(vect_abs, vect_abs, color='blue')
    plt.plot(vect_abs, vect_graph_cf, color='blue')
    plt.step(vect_Xn, vect_Xn, color='red')
    plt.plot
    plt.grid(True)
    plt.title(f"N={N}, X0={X0}, p={p}")
    plt.show()

### Etude de la suite : (0<p<=1)
fct_graphe_Verhulst(N=100, X0=0.30,p=0.50)
fct_escalier(N=100, X0=0.30,p=0.50)
print("Conclusion : La population finira par s'éteindre")

### Etude de la suite : p = 1
fct_graphe_Verhulst(N=100, X0=0.75,p=1)
fct_escalier(N=50, X0=0.75,p=1.00)
print("Conclusion : La population s'éteindra")

### Etude de la suite : 1 < p < 3
fct_graphe_Verhulst(N=50, X0=0.3,p=2)
fct_escalier(N=50, X0=0.3,p=2)
print("Conclusion : La population se stabilisera")

### Etude de la suite : p = 3
fct_graphe_Verhulst(N=100, X0=0.3,p=3)
fct_escalier(N=100, X0=0.3,p=3)
print("Conclusion : La population se stabilisera et varira entre deux points")

### Etude de la suite : p > 3
fct_graphe_Verhulst(N=100, X0=0.3,p=3.1)
fct_escalier(N=100, X0=0.3,p=3.1)
print("Conclusion : La population se stabilisera et varira entre 2-cycles")

### Etude de la suite : p = 3.4
fct_graphe_Verhulst(N=100, X0=0.3,p=3.4)
fct_escalier(N=100, X0=0.3,p=3.4)
print("Conclusion : La population varira entre 2-cycles")

### Etude de la suite : p=3.45
fct_graphe_Verhulst(N=100, X0=0.3,p=3.45)
fct_escalier(N=100, X0=0.3,p=3.45)
print("Conclusion: 4-cyles ???")

### Etude la suite : p = 3.55
fct_graphe_Verhulst(N=100, X0=0.3,p=3.5)
fct_escalier(N=100, X0=0.3,p=3.5)
print("Conclusion: 8-cycles ???")

### Etude de la suite : p = 3.57
fct_graphe_Verhulst(N=100, X0=0.3,p=3.57)
fct_escalier(N=100, X0=0.3,p=3.57)
print("Conclusion: 16-cycles ???")

### Etude de la suite : p = 4
fct_graphe_Verhulst(N=100, X0=0.3,p=4)
fct_escalier(N=100, X0=0.3,p=4)
print("Conclusion: Observation d'un comportement chaotique")

### Courbe de bifurcation

# Fonctions associé au calcul des cycles

def cycle(a, u0):
    """Renvoie les termes distincts à 10^(-3) près parmi u(100) et u(299)"""
    u = u0
    for _ in range(100):  # On écarte les 100 premiers termes de la suite
        u = fct_verhulst(Xn=u, p=a)
    liste = []
    for _ in range(200):  # Parmi les 200 termes suivants, on ne garde que ceux qui sont distincts
        y = round(u, 3)  # (précision 10^-3)
        if y not in liste:
            liste.append(y)
        u = fct_verhulst(Xn=u, p=a)
    return liste


# Fonction associée à la courbe de bifurcation

def bifurcation(a_min, a_max, u0, prec=0.0001):  # précision initialement à 0.001
    """Pour chaque valeur de a entre a_min et a_max avec un pas prec, on affiche les valeurs distinctes 
    détectées par la fonction cycle, assimilables aux valeurs d'adhérence de la suite"""
    plt.title(f"Diagramme des bifurcations pour u0 = {round(u0, 1)}")
    X, Y = [], []
    a = a_min
    while a < a_max:
        liste = cycle(a, u0)
        for i in liste:
            X.append(a)
            Y.append(i)
        a += prec
    plt.plot(X, Y, marker=',', linestyle='', color='blue')  # marker pixel très petit sinon rendu trop gros
    plt.show()


bifurcation(0.1, 3.9,0.7)

#### référence
'''
- Ramis, J. et al (2022). Mathématiques Tout-en-un pour la Licence 1-4e éd. Dunod.
'''