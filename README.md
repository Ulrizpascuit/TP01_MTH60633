# TP01 — Prévision de la Value-at-Risk avec un modèle GARCH

Ce projet estime et évalue la Value-at-Risk (VaR) des indices SP500 et FTSE100 à l’aide d’un modèle GARCH(1,1) avec erreurs normales.\
L’objectif est de modéliser la volatilité conditionnelle des rendements financiers et de produire des prévisions de risque à un pas futur.

## Structure du projet

Le dépôt est organisé pour faciliter la reproductibilité du code.

Code/ : scripts principaux (traitement des données et exécution)

Function/ : fonctions utilisées pour l’estimation et le calcul de la VaR

Data/ : données brutes et données nettoyées

Output/ : figures et résultats générés automatiquement

## Méthodologie

Les log-rendements sont calculés à partir des prix des indices.\
Un modèle GARCH(1,1) est estimé par maximum de vraisemblance sous l’hypothèse d’erreurs normales afin d’obtenir la variance conditionnelle.

La VaR à 95% est calculée à partir du quantile de la distribution conditionnelle.

Un backtesting est réalisé avec une fenêtre glissante de 1000 observations afin de produire 1000 prévisions de VaR et de comparer les violations observées avec le niveau théorique de 5%.

## Reproduction des résultats

Installer les packages requis :

install.packages(c("fs","here","zoo","xts","PerformanceAnalytics"))

Puis exécuter :

source("Code/main.R")

Les figures et résultats sont générés dans le dossier Output.
