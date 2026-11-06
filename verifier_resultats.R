# =============================================================================
#  GES816 – Vérification des résultats du rapport dans RStudio
#
#  1. Ouvrir le dossier du dépôt GES816 comme répertoire de travail
#     (Session > Set Working Directory > Choose Directory...).
#  2. Lancer :  source("verifier_resultats.R", encoding = "UTF-8")
#
#  Le script relance les deux codes R du dépôt et compare chaque chiffre
#  à celui du rapport. Il affiche OK ou ÉCART pour chaque ligne.
# =============================================================================

paquets <- c("quantmod", "xts", "zoo", "rmarkdown", "knitr")
manquants <- paquets[!vapply(paquets, requireNamespace, logical(1), quietly = TRUE)]
if (length(manquants)) install.packages(manquants)
racine <- getwd()
stopifnot("Lancer ce script depuis la racine du dépôt GES816" = dir.exists("backtest_R"))

comparer <- function(obtenu, attendu, tol) ifelse(abs(obtenu - attendu) <= tol, "OK", "ÉCART")

# -----------------------------------------------------------------------------
# 1) Backtest du portefeuille 2003-2026 (chapitre 6.2), données du rapport
# -----------------------------------------------------------------------------
cat("\n=== 1. Backtest 2003 - 25 sept. 2026 (backtest_R/GES816_backtest_4_actifs.Rmd), données du rapport ===\n")
setwd(file.path(racine, "backtest_R"))
rmarkdown::render("GES816_backtest_4_actifs.Rmd", output_format = "html_document",
                  params = list(source = "csv"), quiet = TRUE, envir = new.env())
s <- read.csv("synthese_backtest.csv", check.names = FALSE, fileEncoding = "UTF-8")
setwd(racine)

# Valeurs du rapport (tableau du chapitre 6.2, backtest du 2 janvier 2003 au 25 septembre 2026)
attendu <- data.frame(
  Horizon = c("Quotidien", "Quotidien", "Quotidien", "Quotidien", "Quotidien", "Hebdomadaire", "Hebdomadaire", "Hebdomadaire"),
  Version = c("V1", "V2", "V2", "V2", "V2", "V1", "V2", "V2"),
  Periode = c("Complète", "Complète", "2003-2016", "2016-2021", "2021-2026", "Complète", "Complète", "2021-2026"),
  Trades  = c(869, 748, 430, 153, 165, 182, 165, 35),
  Sharpe  = c(-0.11, -0.05, 0.02, -0.36, 0.01, 0.01, 0.13, 0.47),
  Rendement_an = c(-0.027, -0.012, -0.004, -0.038, -0.007, -0.002, 0.006, 0.030),
  Drawdown_max = c(-0.660, -0.463, -0.289, -0.211, -0.304, -0.402, -0.275, -0.144))

res <- do.call(rbind, lapply(seq_len(nrow(attendu)), function(i) {
  a <- attendu[i, ]
  o <- s[s$Horizon == a$Horizon & s$Version == a$Version & s[[3]] == a$Periode, ]
  data.frame(Horizon = a$Horizon, Version = a$Version, Période = a$Periode,
             `Trades (rapport / R)` = sprintf("%d / %d", a$Trades, o$Trades),
             `Sharpe (rapport / R)` = sprintf("%.2f / %.3f", a$Sharpe, o$Sharpe),
             `Rend./an (rapport / R)` = sprintf("%.1f %% / %.2f %%", 100 * a$Rendement_an, 100 * o[["Rendement par an"]]),
             `DD max (rapport / R)` = sprintf("%.1f %% / %.2f %%", 100 * a$Drawdown_max, 100 * o[["Drawdown max"]]),
             Verdict = if (o$Trades == a$Trades && comparer(o$Sharpe, a$Sharpe, 0.006) == "OK" &&
                           comparer(o[["Rendement par an"]], a$Rendement_an, 0.0006) == "OK" &&
                           comparer(o[["Drawdown max"]], a$Drawdown_max, 0.0006) == "OK") "OK" else "ÉCART",
             check.names = FALSE)
}))
print(res, row.names = FALSE)
cat(if (all(res$Verdict == "OK")) "\n>>> Backtest : tous les chiffres du rapport sont retrouvés.\n" else "\n>>> Backtest : au moins un écart (voir ci-dessus).\n")

# -----------------------------------------------------------------------------
# 2) Moteur des décisions V2, 28 sept. – 2 nov. 2026 (chapitre 6.1)
# -----------------------------------------------------------------------------
cat("\n=== 2. Moteur V2 (moteur_V2_R/GES816_V2_moteur.Rmd), trades du 28 sept. au 2 nov. 2026 ===\n")
cat("Le moteur télécharge ses cours sur Yahoo Finance (connexion Internet requise).\n")
setwd(file.path(racine, "moteur_V2_R"))
rmarkdown::render("GES816_V2_moteur.Rmd", quiet = TRUE)        # écrit aussi sorties/trades.csv
TR <- read.csv("sorties/trades.csv", fileEncoding = "UTF-8"); TR$date_entree <- as.Date(TR$date_entree)
setwd(racine)
{
  attendu2 <- data.frame(actif = c("USO", "AUDNZD", "EURGBP", "XAUUSD", "AUDNZD"),
                         entree = c("2026-09-28", "2026-09-28", "2026-10-05", "2026-10-12", "2026-10-28"),
                         lots = c(46, 0.63, 0.68, 0.28, 0.53),
                         brut = c(315.56, -124.23, 1279.04, 618.80, -245.94),     # 28 sept. - 2 nov. 2026
                         net = c(313.72, -151.13, 1268.83, 614.36, -251.45))
  res2 <- do.call(rbind, lapply(seq_len(nrow(attendu2)), function(i) {
    a <- attendu2[i, ]; o <- TR[TR$actif == a$actif & format(TR$date_entree) == a$entree, ]
    data.frame(Actif = a$actif, Entrée = a$entree, `Quantité (rapport / R)` = sprintf("%.2f / %.2f", a$lots, o$lots),
               `P&L brut (rapport / R)` = sprintf("%.2f / %.2f", a$brut, o$pnl_brut),
               `P&L net (rapport / R)` = sprintf("%.2f / %.2f", a$net, o$pnl_net),
               Verdict = if (nrow(o) == 1 && abs(o$lots - a$lots) < 1e-9 && abs(o$pnl_brut - a$brut) < 0.006 &&
                             abs(o$pnl_net - a$net) < 0.006) "OK" else "ÉCART", check.names = FALSE)
  }))
  print(res2, row.names = FALSE)
  tot <- sum(TR$pnl_net)
  cat(sprintf("\nP&L net total : %.2f $ (rapport : 1 794,33 $) -> %s\n", tot, comparer(tot, 1794.33, 0.006)))
}

# -----------------------------------------------------------------------------
# 3) Facultatif : même backtest avec des données Yahoo téléchargées aujourd'hui
# -----------------------------------------------------------------------------
cat("\n=== 3. Facultatif : backtest avec données Yahoo fraîches ===\n")
cat("Décommenter les lignes ci-dessous. Les chiffres peuvent différer légèrement de ceux du rapport,\n",
    "car Yahoo révise parfois ses cours (exemple : l'or du 30 octobre 2026, 3 884,90 puis 3 898,90).\n", sep = "")
# setwd(file.path(racine, "backtest_R"))
# rmarkdown::render("GES816_backtest_4_actifs.Rmd", params = list(source = "yahoo"), output_file = "backtest_yahoo_frais.html")
# print(read.csv("synthese_backtest.csv", check.names = FALSE, fileEncoding = "UTF-8"))
# setwd(racine)
