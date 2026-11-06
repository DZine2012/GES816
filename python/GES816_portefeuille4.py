"""Backtest du portefeuille à 4 actifs (Or, Pétrole, AUDNZD, EURGBP) avec les règles exactes du journal.

Règles (identiques au journal du 28 sept. au 30 oct. 2026) :
- calcul à la clôture quotidienne ; entrée à la clôture quand les conditions sont réunies ;
- V2 : per = EMA(AMA - Ref(AMA,-2), 21) (AMA Kaufman 21/2/30, diapo 98) ; achat si per >= 0, tendance
  hebdomadaire EMA-10 > EMA-40 (semaines terminées) et ADX(14) >= 17 ; vente si l'inverse ;
- V1 : per seul (toujours en position) ;
- stop fixe à 2 x ATR(14) Wilder au moment de l'entrée ; taille = 1 % du capital / (2 x ATR) ;
- sortie à la clôture dès qu'une condition tombe ; sortie au stop (ou à l'ouverture si elle le dépasse) ;
- si les conditions tiennent encore à la clôture d'une journée de sortie, nouvelle entrée à cette clôture ;
- coûts : spread aller-retour du compte démo (hypothèse du projet) ; swaps non modélisés ;
- au plus 5 positions (jamais contraignant avec 4 actifs).
Le résultat d'un trade vaut 1 % x R, avec R = sens x (sortie - entrée) / (2 x ATR) : indépendant de la devise.
"""
import numpy as np, pandas as pd
import ges816_colab as G

ASSETS = ["XAUUSD", "WTI", "AUDNZD", "EURGBP"]
LABEL = {"XAUUSD": "Or (XAUUSD)", "WTI": "Pétrole (WTI, proxy de USO)", "AUDNZD": "AUDNZD", "EURGBP": "EURGBP"}
RISK, ATR_MULT, CAPITAL = 0.01, 2.0, 50_000.0

def atr(df, n=14):
    h, l, c = df["high"], df["low"], df["close"]
    tr = pd.concat([h - l, (h - c.shift()).abs(), (l - c.shift()).abs()], axis=1).max(axis=1)
    return tr.ewm(alpha=1 / n, adjust=False).mean()

def resample(df, tf):
    if tf == "D": return df[["open", "high", "low", "close"]]
    return df.resample("W-FRI").agg({"open": "first", "high": "max", "low": "min", "close": "last"}).dropna()

def higher_trend(c, tf):
    rule = {"D": "W-FRI", "W": "ME"}[tf]
    hi = c.resample(rule).last().dropna()
    t = np.sign(G.ema(hi, 10) - G.ema(hi, 40))
    if tf == "D":   # la semaine n'est connue qu'à sa clôture : le vendredi même, on utilise encore la précédente
        t.index = t.index + pd.Timedelta(days=1)
    return t.reindex(c.index, method="ffill").fillna(0)

def want(df, tf, version):
    c = df["close"]
    per = G.ama_variation(c, 21)
    s = pd.Series(np.where(per.isna(), 0, np.where(per >= 0, 1, -1)), index=c.index, dtype=float)
    if version == "V1": return s
    adx = G.wilder_adx(df["high"], df["low"], c, 14) >= G.ADX_MIN
    return s.where((s == higher_trend(c, tf)) & adx, 0)

def trades(df, spread, tf="D", version="V2", start="2001-01-01", end="2026-10-31"):
    b = resample(df, tf); w = want(b, tf, version); a = atr(b)
    bad = (b["close"] <= 0)
    bad = bad.rolling(11, center=True, min_periods=1).max().astype(bool)     # WTI, avril 2020
    o, h, l, c = (b[k].to_numpy() for k in ("open", "high", "low", "close"))
    W, A, B, idx = w.to_numpy(), a.to_numpy(), bad.to_numpy(), b.index
    i0 = idx.searchsorted(pd.Timestamp(start)); i1 = idx.searchsorted(pd.Timestamp(end), side="right")
    out, pos = [], None
    for i in range(i0, i1):
        if pos is not None:
            exit_px, why = None, None
            if B[i]:
                exit_px, why = c[i - 1], "Données invalides (prix négatif)"
            elif pos["dir"] > 0 and l[i] <= pos["stop"]:
                exit_px, why = min(o[i], pos["stop"]), "Stop"
            elif pos["dir"] < 0 and h[i] >= pos["stop"]:
                exit_px, why = max(o[i], pos["stop"]), "Stop"
            elif W[i] != pos["dir"]:
                exit_px, why = c[i], "Condition tombée" if W[i] == 0 else "Signal inverse"
            if exit_px is not None:
                gross = pos["dir"] * (exit_px - pos["px"]) / pos["risk_px"]
                cost = spread / pos["risk_px"]
                out.append({"Entrée": idx[pos["i"]], "Sortie": idx[i], "Sens": "Achat" if pos["dir"] > 0 else "Vente",
                            "Prix entrée": pos["px"], "Prix sortie": exit_px, "Stop": pos["stop"],
                            "R brut": gross, "R net": gross - cost, "Motif": why, "Barres": i - pos["i"]})
                pos = None
        if pos is None and W[i] != 0 and not B[i] and np.isfinite(A[i]) and A[i] > 0 and i < i1 - 1:
            rp = ATR_MULT * A[i]
            pos = {"i": i, "dir": W[i], "px": c[i], "risk_px": rp, "stop": c[i] - W[i] * rp}
    if pos is not None:   # position encore ouverte : évaluée à la dernière clôture
        i = i1 - 1; gross = pos["dir"] * (c[i] - pos["px"]) / pos["risk_px"]
        out.append({"Entrée": idx[pos["i"]], "Sortie": idx[i], "Sens": "Achat" if pos["dir"] > 0 else "Vente",
                    "Prix entrée": pos["px"], "Prix sortie": c[i], "Stop": pos["stop"], "R brut": gross,
                    "R net": gross - spread / pos["risk_px"], "Motif": "Ouverte (fin des données)", "Barres": i - pos["i"]})
    t = pd.DataFrame(out)
    # courbe en R, au jour le jour (valeur de marché des positions ouvertes)
    daily = pd.Series(0.0, index=idx[i0:i1])
    for _, r in t.iterrows():
        seg = b["close"].loc[r["Entrée"]:r["Sortie"]].copy()
        seg.iloc[-1] = r["Prix sortie"]
        d = 1 if r["Sens"] == "Achat" else -1
        rr = d * (seg - r["Prix entrée"]) / (abs(r["Prix entrée"] - r["Stop"]))
        inc = rr.diff().fillna(0)
        inc.iloc[-1] -= (r["R brut"] - r["R net"])
        daily.loc[inc.index[1:]] += inc.iloc[1:].to_numpy() if len(inc) > 1 else 0
        if len(inc) == 1: daily.loc[inc.index[-1]] += inc.iloc[-1]
    return t, daily

def portfolio(data, spreads, tf="D", version="V2", start="2001-01-01", end="2026-10-31"):
    res, cols = {}, []
    for a in ASSETS:
        t, dR = trades(data[a], spreads[a], tf, version, start, end)
        res[a] = t; cols.append(dR.rename(a))
    R = pd.concat(cols, axis=1).fillna(0)
    # capital : chaque trade risque 1 % du capital courant -> rendement quotidien = 1 % x somme des R du jour
    ret = RISK * R.sum(axis=1)
    eq = CAPITAL * (1 + ret).cumprod()
    return res, R, ret, eq

def stats(t_all, ret, eq, ppy=252):
    yrs = (ret.index[-1] - ret.index[0]).days / 365.25
    R = t_all["R net"]
    return {"Trades": len(t_all), "Trades/an": len(t_all) / yrs, "Taux de réussite": (R > 0).mean(),
            "R moyen": R.mean(), "Profit factor": R[R > 0].sum() / -R[R < 0].sum() if (R < 0).any() else np.nan,
            "Rendement total": eq.iloc[-1] / eq.iloc[0] * (1 + ret.iloc[0]) - 1 if len(eq) else np.nan,
            "Rendement annualisé": (eq.iloc[-1] / CAPITAL) ** (1 / yrs) - 1, "Volatilité": ret.std() * np.sqrt(ppy),
            "Sharpe": ret.mean() / ret.std() * np.sqrt(ppy) if ret.std() > 0 else np.nan,
            "t-stat": ret.mean() / ret.std() * np.sqrt(len(ret)) if ret.std() > 0 else np.nan,
            "Drawdown max": (eq / eq.cummax() - 1).min(), "Années": yrs}
