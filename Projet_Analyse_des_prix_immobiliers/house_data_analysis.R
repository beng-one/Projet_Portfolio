# Installation d'une librairiee
library(dplyr)
library(car)
library(ggplot2)
library(stargazer)

# Chargement du répertoire de travail
setwd("C:/Users/lajoi/Documents/0_PARCOURS UNIVERSITAIRE/3_MASTER_2024_2026/UPEC - M2 - S1 - MASERATI DA/Rappels de Econometrie/seance - 1/Exercice 1 - Partie 1")
data_output_dir <- "data_out/"
data_output <- ifelse(dir.exists(data_output_dir)==TRUE, print("Le dossier existe déjà"), dir.create(data_output_dir))


# Importer la base de données 
df <- read.csv("housing.csv")

# Etude de la base de données
df_N <- nrow(df)
df_K <- ncol(df)
paste("La base de données contient :", df_N,"lignes et ", df_K, "colonnes.")

# Statistiques descriptives
stargazer(df, type='html', out=paste(data_output_dir,"table1.html", sep=""))


# Histogramme de la variable cible
ggplot(df) +
  aes(x=price) +
  geom_histogram(aes(y=..density..), color="gray", fill="lightblue") +
  geom_density(alpha=.2) # params.colour.density "fill="#FF6666


# Matrice de correlation 
col_filtre <- c("prince", "lotsize", "bedrooms", "bathrms")
matrice_cor <- cor(df) # df[col_filtre.1]
stargazer(matrice_cor, type="html", out=paste(data_output_dir,"table2.html", sep=""))

# 0) regression linéaire ~ 0
lm_0 <- lm(price ~ lotsize, data=df)
stargazer(lm_0, type="html", out=paste(data_output_dir,"table3.html", sep=""))

# Nuage de point du modèle de régression 1
ggplot(df) + 
  aes(x = lotsize, y=price) +
  geom_point(color='black') +
  geom_smooth(method=lm, se=TRUE, color='red')

# 1) regression linéaire ~ 1
lm_1 <- lm(price ~ lotsize + bedrooms + bathrms, data = df)
stargazer(lm_1, type="html", out=paste(data_output_dir,"table4.html", sep=""))

# 2) Variable expliquée et residus
lm_1_residus <- resid(lm_1)
lm_1_predict <- fitted(lm_1)
lm_1_results <- setNames(cbind.data.frame(lm_1_residus, lm_1_predict), c("residus", "prediction"))
lm_1_results


# 3) Tester de deux façons différentes que β1 = 0. Conclusions
linearHypothesis(lm_1, "lotsize = 0")
table(linearHypothesis(lm_1, "lotsize = 0"))
linearHypothesis(lm_1, "lotsize = 0")
stargazer(linearHypothesis(lm_1, "lotsize = 0"), type="html", out=paste(data_output_dir,"table5.html", sep=""))



# 3) Tester de deux façons différentes que B2=0

# Test de fisher ou regarder le résutats de la régression
linearHypothesis(lm_1, "lotsize = 0")


# 4) Estimation du modèle sans la constante

lm_1_sans_constante <- lm(price ~ lotsize + bedrooms + bathrms - 1, data = df)
stargazer(lm_1_sans_constante, type="html", out=paste(data_output_dir,"table7.html", sep=""))

lm_1_results$residus2 <- resid(lm_1_sans_constante)
lm_1_results$prediction2 <- fitted(lm_1_sans_constante)


# 5) Estimation du modèle de régression linéaire multiple 3

q10 <- quantile(df$price, probs = 0.1, na.rm = FALSE)
q90 <- quantile(df$price, probs = 0.90, na.rm = FALSE)
df_q6 = df[(df$price>=q10) & (df$price<=q90), ]
nrow(df_q6)

lm_3 <- lm(price ~ lotsize + bedrooms + bathrms, data = df_q6)
stargazer(lm_3, type="html", out = paste(data_output_dir,"table8.html", sep=""))

# 6) Calculer le Ré à partir des residus, des valeurs observées et des valeurs prédites

price <- df_q6$price
residus_3 <- resid(lm_3)
predict_3 <- fitted(lm_3)
lm_3_R2 <- 1 - (sum(residus_3**2)/sum((price - mean(price))**2))
lm_3_R2

stargazer(lm_0, lm_1, lm_1_sans_constante,lm_3, type="html", out=paste(data_output_dir,"table9.html", sep=""))


# 7) Vérification des hypothèses de régression

# espérance des residus E(ui)=0
mean(lm_1_results$residus) 

# Exogénéité
mean(lm_1_results$prediction) == mean(df$price)
cov(lm_1_residus, df[,c("lotsize", "bedrooms","bathrms" )])
stargazer(cov(lm_1_residus, df[,c("lotsize", "bedrooms","bathrms" )]), type="html", out=paste(data_output_dir,"table6.html", sep="")) #, use = "completesize"

# Hétéroscédasticité

plot(lm_1)









# 8) Estimer et commenter les modèles suivants : 

modele_1 <- lm(price ~ lotsize + bathrms, data=df)
modele_2 <- lm(log(price) ~ log(lotsize) + bathrms, data=df)
modele_3 <- lm(log(price) ~ lotsize + bathrms, data=df)
modele_4 <- lm(price ~ log(lotsize) + bathrms, data=df)
modele_5 <- lm(log(price) ~ log(lotsize) + log(bathrms), data=df)

# 9) Programmation à la main le test de Breusch-Pagan
modele_bp_1 <- lm(price ~ lotsize + bedrooms + bathrms, data=df)
u <- resid(modele_bp_1)
u2 <- u**2
df$u2 <- u2

modele_bp_2 <- lm(u2 ~ lotsize + bedrooms + bathrms, data=df)
summary(modele_bp_2)
N <- length(df$price)
print(N)
R2a <- 0.1091 

BP <- N*R2a
print(BP)
pval <- 1 - pchisq(BP, df=5) # Corriger la pvaleur car elle ne donne pas les mêmes résultats
print(pval)

# Validation du test de BP
BP_tes <- bptest(modele_bp_1)
print(BP_test)
summary(modele_bp_1)$r.squared


# 9) Estimer le modèle par la méthode de White (HCO1 taille de l'échantillon et nombre de variables)
white_test <- bptest(modele_bp_1, 
                   ~ lotsize + bedrooms + bathrms +
                     I(lotsize^2) +I(bedrooms^2)+ I(bathrms^2) +
                     male*school + male*exper + school*exper+ male*exper2 + school*exper2+
                     exper*exper2,
                   data = df)
print(white_test)

# 10) 

# 11)

# 12) 

modele_12_1 <- lm(price ~ lotsize, data=df)
summary(modele_12_1)


modele_12_2 <- lm(price/1000 ~ lotsize, data=df)
summary(modele_12_2)

# Cela ne change strictement rien au modèle de base

# 13) # Division de la variable lotsize en 4 classes : L1, L2, L3, L4

ggplot(df, aes(x=lotsize))+
  geom_density(color="darkblue", fill="lightblue")

## -------- Utliser la fonction cut

q25 <- quantile(df$lotsize, probs = 0.25)

print(q75)
q50 <- quantile(df$lotsize, probs = 0.50)
q75 <- quantile(df$lotsize, probs = 0.75)

df$lotsize_discrete <- cut(
                    df$lotsize,
                    breaks = c(-Inf,q25, q50, q75, Inf),
                    labels = c("L1", "L2", "L3", "L4")
                    )

df$L1 <- ifelse(df$lotsize_discrete=="L1", 1, 0)
df$L2 <- ifelse(df$lotsize_discrete=="L2", 1, 0)
df$L3 <- ifelse(df$lotsize_discrete=="L3", 1, 0)
df$L4 <- ifelse(df$lotsize_discrete=="L4", 1, 0)

df


# 14) 

## --------- La suppression de la constante engendre un bug


# 15) # Test de Fisher


# 16) 
modele_16 <- lm(price ~ lotsize + I(lotsize**2), data=df)
summary(modele_16)

















