# Chargement du répertoire de travail 
setwd("C:/Users/lajoi/Documents/0_PARCOURS UNIVERSITAIRE/MASTER 2024-2026/UPEC - 1S - MASERATI DA/Logiciel R/CC2")

#  Installation et chargement du package 
install.packages("wooldridge")
library(wooldridge)
library(stargazer)
library(ggplot2)
library(dplyr)

# Chargement de la base de données et création de la variable de traitement
m<- mroz
m$d <- ifelse(m$kidslt6>0,1,0)
str(m)
m$d


# --------------------------------------------------------------------------------------------
# QUESTION 1 - Créez une variable de traitement nommée "d" et vérifiez là avec les instructions suivantes :
table(m$kidslt6)
table(m$d)


# --------------------------------------------------------------------------------------------
# QUESTION 2 - Statistiques descriptives et tests statistiques pour comparer les femme en bas age ayant un enfant et les femmes n'ayant pas d'enfant

# Les variables sélectionnées pour la comparaison de ces modèles
var <- c("wage","lwage","educ","exper", "d") 



# Statistiques descriptives de la base de données intégrale <<m>>

stargazer(m[, var]
          ,type="html"
          ,digits=2
          ,title="Tableau 1 : Statistique descriptive m"
          ,out="tableau_1.html"
          ,min.max=TRUE
          ,iqr=FALSE)

# Base de données en fonction du statut des femmes
m0 <- subset(m, d==0)
m1 <- subset(m, d==1)


# Statistiques descriptives des femmes en ayant aucun enfant de moins de 6 ans
stargazer(m0[, var]
          ,type="html"
          ,digits=2
          ,title="Tableau 2 : Statistique descriptive du groupe m0"
          ,out="tableau_2.html"
          ,min.max=TRUE
          ,iqr=FALSE)


# Matrice de variance-covariance de m0
stargazer(cor(m0[, var]
              ,use="complete.obs")
              ,type="html"
              ,digits = 2
              ,title="Matrice de variance-covariance du groupe m0"
              ,out="tableau_3.html")


# Statistiques descriptives des femmes en ayant au moins un enfant de moins de 6 ans
stargazer(m1[,var]
          ,type="html"
          ,digits=2
          ,title ="Tableau 4 : Statistique descriptive du groupe m1 " 
          ,out="tableau_4.html"
          ,min.max=TRUE
          ,iqr=FALSE)

# Matrice de variance-covariance de m1
stargazer(cor(m1[,var]
          ,use="complete.obs")
          ,type="html"
          ,digits=2
          ,title = "Matrice de variance-covariance du groupe m1"
          ,out="tableau_5.html")

## TEST STATISTIQUES 

# Test non paramétrique de Kolmogorov-Smirov
ks.test(m0$lwage,m1$lwage)
ks.test(m0$educ,m1$educ)
ks.test(m0$exper,m1$exper)

# Test de Student Welch 
with(m, t.test(educ ~ d ))
with(m, t.test(exper ~ d ))

# --------------------------------------------------------------------------------------------
# QUESTION 3 - GRAPHIQUE DE COMPARAISON DES DEUX POPULATIONS


m$d <- factor(m$d,labels = c("nochildren", "children"))
levels(m$d)

# nuage de point educ - salaire (# Annexe)
ggplot(m,aes(x=educ, y=lwage, colour=d))+ 
  geom_point()
ggsave("graphique_1.png",width = 8, height = 6)


# nuage de point exper - salaire (# Annexe)
ggplot(m,aes(x=exper, y=lwage, colour=d))+ # Annexe
  geom_point()
ggsave("graphique_2.png",width = 8, height = 6)
  
# Fonction de répartition (# Annexe)
ggplot(data = m)+
  aes(x=lwage, colour = d)+
  stat_ecdf(geom="step")
ggsave("graphique_3.png",width = 8, height = 6)

# Fonction  de densité 
ggplot(data = m)+
  aes(x=lwage, fill = d)+
  geom_density(alpha=1/3)
ggsave("graphique_4.png",width = 8, height = 6)


# --------------------------------------------------------------------------------------------

# QUESTION 4 - COMPARAISON DES DEUX POPULATIONS EN UTILISANT UN TEST DE STUDENT ET LES MCO

# Test de Student pour comparer le salaire en fonction du nombre d'enfant en bas age (Tableau 6)
lwage_test <- with(m, t.test(lwage ~ d ))
lwage_test

# Régression  linéaire Multiple 
regt <- lm(data=m,lwage~d+educ+exper)
summary(regt)

# Résultats des régressions
stargazer(regt, type ="html", title="Model OLS", out="Tableau_8.html" )


# --------------------------------------------------------------------------------------------

# QUESTION 5  - ECRITURE DU MODELE DE RUBIN POUR LES MESURER LES EFFETS DU TRAITEMENT SUR LES TRAITES 
# Y=a+(X-m)*β0+γW+W(X-m)δ+μ 

# --------------------------------------------------------------------------------------------

# QUESTION 6  - MODELE DE RUBIN DE MESURER L'EFFET DE LA VARIABLE DE TRAITEMENT SUR LES 

# Y : la variable de résultat (lwage)
# W : la variable de traitement (treatement,kidslt6>=1)
# X : variables de confution c("exper", "educ", "d")

print(m$d)
m$d <- ifelse(m$d=="children",1,0)
m$d

Rubin <- data.frame(
  Y=m$lwage,
  W=m$d)

table(Rubin$W)

# variables de confusion dans les diapos
Rubin <- cbind(Rubin, m[, c("exper", "educ")])
Rubin


# Supprimer les valeurs manquantes
Rubin <- subset(Rubin,!is.na(Y))
Rubin <- Rubin[!is.na(Rubin$Y),]
stargazer(Rubin, type="html", title="Model Rublin", out="Tableau_9.html")
stargazer(Rubin, type="text")


#modèle à probabilité linéaire
liste.entree <- c("educ","exper")
liste.sortie <- c("c.educ","c.exper")

##centrage moyenne par rapport aux traités ATT
moy_tr <- function(x,w){x-sum(w*x)/sum(w)}
Rubinc1 <- Rubin
Rubinc1
Rubinc1[,liste.sortie] <- sapply(Rubin[,liste.entree],
                                 moy_tr,
                                 w=Rubinc1$W)
#vérif
summary(subset(Rubinc1,W==1))

reg0 <- lm(data=Rubinc1,Y~W)
reg1 <- lm(data=Rubinc1,Y~c.exper+c.educ)
reg2 <- lm(data=Rubinc1,Y~W+c.exper+c.educ)
reg3 <- lm(data=Rubinc1,Y~W*(c.exper+c.educ))
stargazer(reg0,reg1,reg2,reg3,type="html",
          omit.stat=c("f","ser","adj.rsq"), out="tableau_10.html")
stargazer(reg0,reg1,reg2,reg3,type="text",
          omit.stat=c("f","ser","adj.rsq"))


# Différence moyenne entre groupes : -0.06
# Model Rubin 1 : -0.074


# --------------------------------------------------------------------------------------------

# QUESTION 7 :  On ajoute le carré de l’expérience (expersq) dans les variables explicatives du salaire.
liste2.entree <- c("educ","exper","exper2")
liste2.sortie <- c("c.educ","c.exper", "c.exper2")

Rubin[, liste2.entree[3]] = Rubin[, liste2.entree[2]] **2
Rubin
Rubinc2 <- Rubin
Rubinc2
Rubinc2[,liste2.sortie] <- sapply(Rubin[,liste2.entree],
                                 moy_tr,
                                 w=Rubinc2$W)
#vérification
summary(subset(Rubinc1,W==1))


regv0 <- lm(data=Rubinc2,Y~W)
regv1 <- lm(data=Rubinc2,Y~c.exper+c.educ+c.exper2)
regv2 <- lm(data=Rubinc2,Y~W+c.exper+c.educ+c.exper2)
regv3 <- lm(data=Rubinc2,Y~W*(c.exper+c.educ+c.exper2))
stargazer(regv0,regv1,regv2,regv3,type="html",
          omit.stat=c("f","ser","adj.rsq"), out="tableau_11.html")
stargazer(regv0,regv1,regv2,regv3,type="text",
          omit.stat=c("f","ser","adj.rsq"))


# Différence moyenne entre groupes : -0.06
# Model Rubin 1 : -0.074
# Model Rubin 2 : -0.060 


