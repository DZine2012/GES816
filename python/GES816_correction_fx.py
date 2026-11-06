"""Correction du défaut Yahoo FX : à partir d'environ 2010, la « clôture » quotidienne des paires =X
est en fait le prix d'ouverture de la barre ; le plus haut et le plus bas de la barre sont donc postérieurs
à cette « clôture ». Correction : pendant les périodes où le défaut est détecté, la vraie clôture de la
barre t est la « clôture » Yahoo de t+1 (le cours FX est continu 24 h).
Détection, actif par actif : médiane glissante 60 j de |C-O|/(H-L) < 0,15 ET de |C-O(t+1)|/(H-L) > 0,30."""
import numpy as np, pandas as pd

def detect(df):
    rng = (df["high"] - df["low"]).replace(0, np.nan)
    a = ((df["close"] - df["open"]).abs() / rng).rolling(60, min_periods=40).median()
    b = ((df["close"] - df["open"].shift(-1)).abs() / rng).rolling(60, min_periods=40).median()
    return (a < 0.15) & (b > 0.30)

CCY = {"USD","EUR","GBP","JPY","CHF","CAD","AUD","NZD","NOK","SEK","SGD","ZAR","MXN"}
def is_fx(name):
    return len(name) == 6 and name[:3] in CCY and name[3:] in CCY

def fix(data, verbose=True):
    out, rep = {}, {}
    for k, df in data.items():
        if not is_fx(k):
            out[k] = df; continue
        m = detect(df)
        if m.sum() < 60:
            out[k] = df; continue
        d = df.copy()
        nxt = df["close"].shift(-1)
        d.loc[m, "close"] = nxt[m]
        d = d.dropna(subset=["close"])
        d["high"] = d[["high", "open", "close"]].max(axis=1)
        d["low"] = d[["low", "open", "close"]].min(axis=1)
        out[k] = d
        rep[k] = (m[m].index[0].date(), m[m].index[-1].date(), round(m.mean(), 2))
    if verbose:
        print(f"Actifs corrigés : {len(rep)} / {len(data)}")
        for k, v in rep.items(): print(f"   {k:8s} défaut détecté de {v[0]} à {v[1]} ({v[2]:.0%} des jours)")
    return out, rep

def load(p):
    raw = pd.read_csv(p, parse_dates=["date"])
    return {k: g.drop(columns="actif").set_index("date").sort_index() for k, g in raw.groupby("actif")}
