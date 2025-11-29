# Installation de la librairie packman
install.packages("pacman")

# Installation et importation des librairies à l'aide de packman
pacman:: p_load("dplyr", 
                "car", 
                "ggplot2", 
                "stargazer",
                "lmtest",
                "sandwich",
                "rgl",
                "plot3D",
                )


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
col_filtre <- c("price", "lotsize", "bedrooms", "bathrms")
matrice_cor <- cor(df) # df[col_filtre.1]
stargazer(matrice_cor, type="html", out=paste(data_output_dir,"table2.html", sep=""))

# 1) regression linéaire ~ 1
lm.1 <- lm(price ~ lotsize, data=df)
stargazer(lm.1, type="html", out=paste(data_output_dir,"table3.html", sep=""))

# Nuage de point du modèle de régression 1
ggplot(df) + 
  aes(x = lotsize, y=price) +
  geom_point(color='black') +
  geom_smooth(method=lm, se=TRUE, color='red')

# 2) regression linéaire ~ 2
lm.2 <- lm(price ~ lotsize + bathrms, data = df)
stargazer(lm.2, type="html", out=paste(data_output_dir,"table4.html", sep=""))

# Nuage de point et courbe éliptique en 3D

price <- df$price
lotsize <- df$lotsize
bathrms <- df$bathrms
 
plot3d(lotsize, price, bathrms, col="blue", box = FALSE,
      type ="s", radius = 0.15)
ellips <- ellipse3d(cov(cbind(lotsize, price, bathrms)), 
                     centre = c(mean(lotsize), mean(price), mean(bathrms)), level = 0.95)
plot3d(ellips, col = "blue", alpha = 0.5, add = TRUE, type = "wire")


# x, y, z variables
x <- df$lotsize
y <- df$bathrms
z <- df$price

nrow(mtcars)

# Compute the linear regression (z = ax + by + d)
fit <- lm(z ~ x + y)
# predict values on regular xy grid
grid.lines = 546
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
fitpoints <- predict(fit)
# scatter plot with regression plane
scatter3D(x, y, z, pch = 18, cex = 2, 
          theta = 20, phi = 20, ticktype = "detailed",
          xlab = "lotsize", ylab = "bathrms", zlab = "price",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints), main = "housing price")


# 2) Variable expliquée et residus
lm_2_residus <- resid(lm.2)
lm_2_predict <- fitted(lm.2)
lm_2_results <- setNames(cbind.data.frame(lm_2_residus, lm_2_predict), c("residus", "prediction"))
lm_2_results


# 3) Tester de deux façons différentes que β1 = 0. Conclusions
linearHypothesis(lm.2, "1*lotsize + 1*bathrms = 0") #, "lotsize + bathroms = 0")
stargazer(linearHypothesis(lm.2, "1*lotsize + 1*bathrms = 0"), type="html", out=paste(data_output_dir,"table5.html", sep=""))


# 4) Estimation du modèle sans la constante

lm_2_sans_constante <- lm(price ~ lotsize + bathrms - 1, data = df)
stargazer(lm_2_sans_constante, type="html", out=paste(data_output_dir,"table7.html", sep=""))
lm_2_results$residus2 <- resid(lm_2_sans_constante)
lm_2_results$prediction2 <- fitted(lm_2_sans_constante)


# 5) Estimation du modèle de régression linéaire multiple 3

q10 <- quantile(df$price, probs = 0.1, na.rm = FALSE)
q90 <- quantile(df$price, probs = 0.90, na.rm = FALSE)
df_q6 = df[(df$price>=q10) & (df$price<=q90), ]
nrow(df_q6)

lm.3 <- lm(price ~ lotsize + bathrms, data = df_q6)
stargazer(lm.3, type="html", out = paste(data_output_dir,"table8.html", sep=""))

# 6) Calculer le Ré à partir des residus, des valeurs observées et des valeurs prédites

price <- df_q6$price
residus_3 <- resid(lm.3)
predict_3 <- fitted(lm.3)
lm_3_R2 <- 1 - (sum(residus_3**2)/sum((price - mean(price))**2))
lm_3_R2

stargazer(lm.1, lm.2, lm_2_sans_constante,lm.3, type="html", out=paste(data_output_dir,"table9.html", sep=""))


# 7) Vérification des hypothèses de régression

    # 1. espérance des residus E(ui)=0
mean(lm_2_results$residus) 

    # 2. Hétéroscédasticité
plot(x=lm.2, which=1) # plot.lm function


bp_test <- bptest(lm.2) # Test d'homoscédasticité de Breusch-Pagan
print(bp_test)


# --------> Comme pvalue du Test est inférieur à 5%, H0 qui suppose que la variance des résidus est nulle est rejétée. 
# --------> Les résidus sont hétéroscédastiques

# Programmation à la main du test de Breusch-Pagan
    # Etape 1
modele_bp_scratch <- lm(price ~ lotsize + bathrms, data=df)
df$u <- resid(modele_bp_scratch)
df$u2 <-df$u ** 2

    # Etape 2
modele_bp_scratch_2 <- lm(u2 ~ lotsize + bathrms, data=df)
summary(modele_bp_scratch_2)
N <- nrow(df)
R2a <- summary(modele_bp_scratch_2)$r.squared # La fonction bptest() utilise le R2 non ajusté.
BP <- N*R2a
print(BP)
pval <- 1 - pchisq(BP, df=2) # df = K-1 avec K=variables explicatives + constante)
print(pval)


# Test de White :  la méthode de White (HCO1 taille de l'échantillon et nombre de variables)
white_test <- bptest(modele_bp_scratch, 
                     ~ lotsize + bathrms +
                       I(lotsize^2) + I(bathrms^2) +
                       lotsize*bathrms,
                     data = df)
print(white_test)
# --------> pvaleur <0.05, donc H0 qui suppose que la variance des residus est constante est rejetée.


    # Exogénéité
stargazer(cov(lm_2_residus, df[,c("lotsize", "bathrms" )]), type="html", out=paste(data_output_dir,"table6.html", sep="")) #, use = "completesize"
# --------> Au regard des résultats des test de corrélation, il apparait qu'il n'y a pas d'endogénéité dans le modèle de régression
# --------> Confirmation à l'aide du test de Haussman

cov(lm_2_residus, df[,c("lotsize", "bathrms" )])


cor_test_u_lotsize <- cor.test(df$u, df$lotsize)
cor_test_u_lotsize
cor_test_u_bathrms <- cor.test(df$u, df$bathrms)
cor_test_u_bathrms

  # Correction de l'hétéroscédasticité par la méthode de White (variance robuste)
mcqg_2 <- coeftest(modele_bp_scratch, vcov=vcovHC(modele_bp_scratch, type = "HC0"))
mcqg_2


# 8) Modèle de Spécification

write.csv(df, 'data_out/log_data.csv') # mini analyse sur excel


modele_1 <- lm(price ~ lotsize + bathrms, data=df)
modele_2 <- lm(log(price) ~ log(lotsize) + log(bathrms), data=df)
modele_3 <- lm(price ~ log(lotsize) + log(bathrms), data=df)
modele_4 <- lm(log(price) ~ lotsize + bathrms, data=df)

stargazer(modele_1, modele_2, modele_3, modele_4,
          type ="html",
          out=paste(data_output_dir,"table10.html", sep=""))

plot(modele_1, which = 1)
plot(modele_3, which = 1)
bptest(modele_3)