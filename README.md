# GES816 – Travail final de synthèse (automne 2026)

**École de technologie supérieure · Montréal**
GES816 – Méthodes et systèmes d'investissement · Professeur : Edmond T. Miresco

Ce dépôt contient le code, les données et les résultats du rapport
« Étude et performance de mes transactions » : la règle V2 (AMA Variation Method « Best » + ADX(14) ≥ 17 +
tendance hebdomadaire EMA-10/40) appliquée à l'or, au pétrole, à AUDNZD et à EURGBP.

## Rapport

- [Rapport final (PDF)](rapport/GES816_rapport_final.pdf)

## Pages HTML (GitHub Pages)

| Page | Contenu |
|---|---|
| [Décisions de trading V2, 28 sept. – 2 nov. 2026](https://dzine2012.github.io/GES816/moteur_V2_R/GES816_V2_moteur.html) | Moteur R (cours téléchargés sur Yahoo Finance) : décision quotidienne justifiée pour chaque actif, trades, coûts, capital, contrôle des 5 trades au centime |
| [Backtest du portefeuille, 2003-2026](https://dzine2012.github.io/GES816/backtest_R/GES816_backtest_4_actifs.html) | Choix des actifs, backtest R avec les règles du journal (2003 – 25 sept. 2026), correction des données forex, résultats par période et par actif ; puis contrôle des décisions sur la période des trades (28 sept. – 2 nov. 2026) |

## Contenu

| Dossier | Fichiers |
|---|---|
| [`moteur_V2_R/`](moteur_V2_R) | `GES816_V2_moteur.Rmd` (moteur, code expliqué, téléchargement Yahoo Finance) et `GES816_V2_moteur.html` |
| [`backtest_R/`](backtest_R) | `GES816_backtest_4_actifs.Rmd`, `.html` et `.md` (lisible sur GitHub), `donnees_yahoo_4_actifs.csv` |
| [`python/`](python) | Moteur Python et notebooks Colab (backtest, tests pré-enregistrés n° 1 et 2), correction forex, backtest à 4 actifs |
| [`preenregistrements/`](preenregistrements) | Pré-enregistrements des deux tests, avec l'empreinte SHA-256 du code |
| [`resultats/`](resultats) | Journal des trades, classeur du backtest et de la correction forex ; `avant_correction_fx/` : résultats antérieurs à la correction |
| [`figures/`](figures) | Les 10 figures du rapport |

## Vérifier les résultats

Dans RStudio, choisir la racine du dépôt comme répertoire de travail, puis :

```r
source("verifier_resultats.R", encoding = "UTF-8")
```

Le script relance les deux codes R et compare chaque chiffre à celui du rapport (OK ou ÉCART).

## Reproduire

```r
install.packages(c("quantmod", "xts", "zoo", "rmarkdown", "knitr", "kableExtra", "ggplot2", "DT", "curl", "jsonlite"))

# Backtest 2003-2026 : données Yahoo téléchargées, ou fichier fourni (résultats identiques au rapport)
rmarkdown::render("backtest_R/GES816_backtest_4_actifs.Rmd")
rmarkdown::render("backtest_R/GES816_backtest_4_actifs.Rmd", params = list(source = "csv"))

# Décisions V2 du 28 sept. au 2 nov. 2026 (cours téléchargés sur Yahoo Finance)
rmarkdown::render("moteur_V2_R/GES816_V2_moteur.Rmd")
```

Les empreintes SHA-256 des scripts `python/ges816_confirmation.py` et `python/ges816_univers_elargi.py`
correspondent à celles des pré-enregistrements (`sha256sum python/*.py`).
