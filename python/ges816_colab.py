# =============================================================================
# GES816 – Méthodes et systèmes d'investissement (E. Miresco) – Automne 2026
# Sélection de 5 actifs indépendants + backtest des indicateurs du cours
# À exécuter dans Google Colab (Exécution > Tout exécuter). Durée : ~2-4 min.
# =============================================================================
# Méthode (à citer dans le rapport) :
#  1. Données quotidiennes OHLC Yahoo Finance, 2014 -> aujourd'hui.
#  2. SÉLECTION sur la période A (oct. 2016 - sept. 2021) uniquement :
#     - corrélation de Pearson des rendements log quotidiens, |r| max <= 0,30 ;
#     - parmi les combinaisons admissibles, on garde celle dont les actifs sont
#       les plus « tendanciels » (ratio d'efficacité de Kaufman moyen, diapo 96),
#       car les méthodes du cours sont des suiveurs de tendance.
#  3. ÉVALUATION hors échantillon sur la période B (oct. 2021 - aujourd'hui).
#  4. Paramètres = valeurs par défaut des diapos, aucune optimisation.
# =============================================================================
import itertools, os, re, shutil, sys, warnings, zipfile
import numpy as np
import pandas as pd
warnings.filterwarnings("ignore")

# ----------------------------------------------------------------- RÉGLAGES
START_DATA = "2014-01-01"
PERIOD_A = ("2016-10-01", "2021-09-30")      # sélection
PERIOD_B = ("2021-10-01", None)              # évaluation (None = jusqu'à aujourd'hui)
MAX_ABS_CORR = 0.30
N_ASSETS = 5
FORCE_ASSETS = None   # ex. ["USDJPY", "XAUUSD", "US500", "AUDNZD", "WTI"] pour imposer ton choix
ADX_MIN = 17          # diapo 253 : « niveau situé entre 17 et 30 »
VOL_TARGET, VOL_CAP = 0.10, 4.0
OUT = "resultats_GES816"

# Ticker Yahoo -> nom MT5 usuel, spread typique d'un compte démo (en prix, hypothèse)
UNIVERSE = {
    "EURUSD=X": ("EURUSD", 0.00012), "GBPUSD=X": ("GBPUSD", 0.00015), "AUDUSD=X": ("AUDUSD", 0.00012),
    "NZDUSD=X": ("NZDUSD", 0.00018), "JPY=X": ("USDJPY", 0.015), "CAD=X": ("USDCAD", 0.00020),
    "CHF=X": ("USDCHF", 0.00018), "AUDNZD=X": ("AUDNZD", 0.00035), "EURGBP=X": ("EURGBP", 0.00015),
    "EURJPY=X": ("EURJPY", 0.020), "CADJPY=X": ("CADJPY", 0.025), "AUDCAD=X": ("AUDCAD", 0.00025),
    "EURCHF=X": ("EURCHF", 0.00020), "GBPJPY=X": ("GBPJPY", 0.030), "NZDCAD=X": ("NZDCAD", 0.00030),
    "GC=F": ("XAUUSD", 0.30), "SI=F": ("XAGUSD", 0.03), "CL=F": ("WTI", 0.04), "HG=F": ("COPPER", 0.004),
    "^GSPC": ("US500", 0.6), "^GDAXI": ("GER40", 1.5), "^N225": ("JPN225", 10.0), "^FTSE": ("UK100", 1.5),
}

# ======================================================= INDICATEURS (diapos)
def ema(x, period):
    """Diapo 96 : EMA amorcée sur la 1re valeur, facteur 2/(n+1)."""
    a = 2.0 / (period + 1.0); v = np.asarray(x, float); out = np.full_like(v, np.nan); started = False
    for i, xi in enumerate(v):
        if np.isnan(xi):
            if started: out[i] = out[i - 1]
            continue
        out[i] = xi if not started else a * xi + (1 - a) * out[i - 1]; started = True
    return pd.Series(out, index=x.index)

def ama_factor(x, sc):
    v, s = x.to_numpy(float), sc.to_numpy(float); out = np.full_like(v, np.nan); prev = np.nan
    for i in range(len(v)):
        if not (np.isnan(s[i]) or np.isnan(v[i])):
            prev = v[i] if np.isnan(prev) else s[i] * v[i] + (1 - s[i]) * prev
        out[i] = prev
    return pd.Series(out, index=x.index)

def efficiency_ratio(c, n=21):
    return (c - c.shift(n)).abs() / (c - c.shift(1)).abs().rolling(n).sum()

def kaufman_ama(c, period=21, fast_sc=2, slow_sc=30):
    """Diapo 96 : AMA de Perry Kaufman."""
    fast, slow = 2 / (fast_sc + 1), 2 / (slow_sc + 1)
    return ama_factor(c, (efficiency_ratio(c, period) * (fast - slow) + slow) ** 2)

def ama_variation(c, period=21):
    """Diapo 98 « AMA Variation Method – (Best) » :
    AMA = MA(AMA,2); dif = AMA - Ref(AMA,-2); per = EMA(dif, Period); vert si per >= 0."""
    a = kaufman_ama(c, period).rolling(2).mean()
    return ema(a - a.shift(2), period)

def zlema_variation(c, period=5, smooth=10):
    """Diapos 108-110 : ZLEMA = ema1 + (ema1 - ema2) ; variation journalière lissée EMA-10."""
    e1 = ema(c, period); e2 = ema(e1, period); z = e1 + (e1 - e2)
    return ema(z - z.shift(1), smooth)

def wilder_adx(h, l, c, n=14):
    """Diapo 250 : +DI/-DI par sommation de Welles, DX, ADX_i = [(ADX_{i-1}(n-1)) + DX_i]/n."""
    h, l, cc = h.to_numpy(float), l.to_numpy(float), c.to_numpy(float); N = len(cc)
    tr = np.full(N, np.nan); pdm = np.zeros(N); mdm = np.zeros(N)
    for i in range(1, N):
        tr[i] = max(h[i] - l[i], abs(h[i] - cc[i - 1]), abs(l[i] - cc[i - 1]))
        hi, lo = h[i] - h[i - 1], l[i - 1] - l[i]
        if (hi < 0 and lo < 0) or hi == lo: continue
        if hi > lo: pdm[i] = hi
        else: mdm[i] = lo
    def wsum(x):
        o = np.full(N, np.nan)
        if N <= n: return o
        o[n] = np.nansum(x[1:n + 1])
        for i in range(n + 1, N): o[i] = x[i] + o[i - 1] * (n - 1) / n
        return o
    s_tr, s_p, s_m = wsum(tr), wsum(pdm), wsum(mdm)
    with np.errstate(all="ignore"):
        pdi, mdi = 100 * s_p / s_tr, 100 * s_m / s_tr
        dx = 100 * np.abs(pdi - mdi) / (pdi + mdi)
    adx = np.full(N, np.nan)
    if N > 2 * n:
        adx[2 * n] = np.nanmean(dx[n + 1:2 * n + 1])
        for i in range(2 * n + 1, N):
            adx[i] = (adx[i - 1] * (n - 1) + (dx[i] if not np.isnan(dx[i]) else adx[i - 1])) / n
    return pd.Series(adx, index=c.index)

def velocity_acceleration(c, a=0.1):
    """Diapos 566-568 : M = EMA des prix, V = vélocité, W = accélération."""
    v = c.to_numpy(float); M = np.full_like(v, np.nan); V = M.copy(); W = M.copy()
    M[0], V[0], W[0] = v[0], 0.0, 0.0
    for i in range(1, len(v)):
        M[i] = a * v[i] + (1 - a) * M[i - 1]
        V[i] = a * (M[i] - M[i - 1]) + (1 - a) * V[i - 1]
        W[i] = a * (V[i] - V[i - 1]) + (1 - a) * W[i - 1]
    return V, W

def _selftest():
    V, W = velocity_acceleration(pd.Series([500., 505., 510.]))
    assert abs(V[2] - 0.14) < 1e-9 and abs(W[2] - 0.0135) < 1e-9, "test diapo 567 échoué"
    print("Auto-test indicateurs : OK (exemple chiffré de la diapo 567 retrouvé)")

# ============================================================== DONNÉES
def download():
    import yfinance as yf
    data, failed = {}, []
    for tk, (name, _) in UNIVERSE.items():
        df = None
        for _try in range(3):
            try:
                df = yf.Ticker(tk).history(start=START_DATA, interval="1d", auto_adjust=False)
                if len(df): break
            except Exception:
                pass
        if df is None or len(df) < 1000:
            failed.append(tk); continue
        df.index = pd.to_datetime(df.index).tz_localize(None).normalize()
        df = df[["Open", "High", "Low", "Close"]].rename(columns=str.lower).dropna()
        df = df[(df.index.dayofweek < 5)]
        df = df[~df.index.duplicated(keep="last")].sort_index()
        df["high"] = df[["high", "open", "close"]].max(axis=1)     # corrige les ticks aberrants Yahoo
        df["low"] = df[["low", "open", "close"]].min(axis=1)
        data[name] = df
        print(f"  {tk:10s} -> {name:7s} {len(df):5d} barres  {df.index[0].date()} -> {df.index[-1].date()}")
    if failed: print("  Échec de téléchargement :", failed)
    return data

# ============================================================== SÉLECTION
def select_assets(data):
    a0, a1 = PERIOD_A
    closes = pd.DataFrame({k: v["close"] for k, v in data.items()})
    closes = closes.where(closes > 0)
    okA = [k for k in closes if closes[k].loc[a0:a1].notna().mean() > 0.9]
    lr = np.log(closes[okA]).diff().loc[a0:a1].dropna(how="all")
    corrA = lr.corr(min_periods=500)
    er = pd.Series({k: efficiency_ratio(data[k]["close"]).loc[a0:a1].mean() for k in okA})
    names = list(corrA.columns); M = corrA.abs().values
    combos = []
    for comb in itertools.combinations(range(len(names)), N_ASSETS):
        sub = M[np.ix_(comb, comb)][np.triu_indices(N_ASSETS, 1)]
        if np.isnan(sub).any(): continue
        combos.append((sub.max(), sub.mean(), er.iloc[list(comb)].mean(), [names[i] for i in comb]))
    rank = pd.DataFrame(combos, columns=["|r| max", "|r| moyen", "ER moyen", "Actifs"])
    thr = MAX_ABS_CORR
    while (rank["|r| max"] <= thr).sum() == 0: thr += 0.05
    feas = rank[rank["|r| max"] <= thr].sort_values(["ER moyen", "|r| max"], ascending=[False, True])
    chosen = FORCE_ASSETS or feas.iloc[0]["Actifs"]
    print(f"\nSeuil de corrélation utilisé : |r| <= {thr:.2f} ({len(feas)} combinaisons admissibles sur {len(rank)})")
    print("Ratio d'efficacité de Kaufman moyen (période A) :")
    print(er.sort_values(ascending=False).round(3).to_string())
    return chosen, corrA, er, feas.head(15), rank.sort_values("|r| max").head(15), thr

# ============================================================== STRATÉGIES
def weekly_trend(c):
    """Diapos 613-615 : top-down, EMA-10 vs EMA-40 sur les semaines terminées."""
    wk = c.resample("W-FRI").last().dropna()
    return np.sign(ema(wk, 10) - ema(wk, 40)).reindex(c.index, method="ffill").fillna(0)

def strategies(df):
    c = df["close"]
    per = ama_variation(c, 21)
    s1 = pd.Series(np.where(per.isna(), 0, np.where(per >= 0, 1, -1)), index=c.index, dtype=float)
    tw = weekly_trend(c)
    adx_ok = (wilder_adx(df["high"], df["low"], c, 14) >= ADX_MIN)
    zv = zlema_variation(c)
    V, W = velocity_acceleration(c)
    p = np.zeros(len(c)); st = 0
    for i in range(1, len(c)):
        if st == 1 and V[i - 1] > 0 >= V[i]: st = 0                 # EXIT à V = 0
        if st == -1 and V[i - 1] < 0 <= V[i]: st = 0
        if st == 0:
            if W[i - 1] > 0 > W[i] and V[i] > 0: st = 1              # BUY : W passe de >0 à <0
            elif W[i - 1] < 0 < W[i] and V[i] < 0: st = -1           # SELL : W passe de <0 à >0
        p[i] = st
    return {
        "V1 AMA Variation (Best)": s1,
        "AMA + ADX>=17": s1.where(adx_ok, 0),
        "AMA + top-down hebdo": s1.where(s1 == tw, 0),
        "V2 AMA + top-down + ADX>=17": s1.where((s1 == tw) & adx_ok, 0),
        "ZLEMA Variation (5, EMA-10)": pd.Series(np.where(zv.isna(), 0, np.where(zv >= 0, 1, -1)), index=c.index, dtype=float),
        "Vélocité/accélération (a=0,1)": pd.Series(p, index=c.index),
    }

# ============================================================== MOTEUR
def run_asset(name, df, spread):
    c = df["close"]
    r = c.pct_change().shift(-1).fillna(0)                 # close t -> close t+1
    bad = (c <= 0) | (c.shift(-1) <= 0) | (c.shift(1) <= 0)
    if bad.any():                                          # ex. WTI 20 avril 2020 (prix négatif)
        r[bad.rolling(11, center=True, min_periods=1).max().astype(bool)] = 0
    sp = spread / c.abs()
    vol = c.pct_change().rolling(60).std() * np.sqrt(252)
    w = (VOL_TARGET / vol).clip(upper=VOL_CAP)
    out = {}
    for sname, pos in strategies(df).items():
        cost = pos.diff().abs().fillna(pos.abs()) * sp / 2
        out[sname] = (pos, pos * r - cost, r, sp, w)
    return out

def trades_of(c, pos, sp):
    p, cc, idx, res, i = pos.to_numpy(), c.to_numpy(), c.index, [], 0
    while i < len(p):
        if p[i] == 0: i += 1; continue
        j = i
        while j < len(p) and p[j] == p[i]: j += 1
        ex = min(j, len(p) - 1)
        res.append({"Entrée": idx[i].date(), "Sortie": idx[ex].date(), "Sens": "BUY" if p[i] > 0 else "SELL",
                    "Prix entrée": cc[i], "Prix sortie": cc[ex], "Jours": (idx[ex] - idx[i]).days,
                    "Rendement net": p[i] * (cc[ex] / cc[i] - 1) - float(sp.iloc[i])})
        i = j
    return pd.DataFrame(res)

def metrics(ret, tr, pos, c):
    eq = (1 + ret).cumprod(); yrs = max((ret.index[-1] - ret.index[0]).days / 365.25, 1e-9)
    wins = tr["Rendement net"][tr["Rendement net"] > 0] if len(tr) else pd.Series(dtype=float)
    loss = tr["Rendement net"][tr["Rendement net"] < 0] if len(tr) else pd.Series(dtype=float)
    return {"Rendement annualisé": eq.iloc[-1] ** (1 / yrs) - 1, "Volatilité": ret.std() * np.sqrt(252),
            "Sharpe": ret.mean() / ret.std() * np.sqrt(252) if ret.std() > 0 else np.nan,
            "Drawdown max": (eq / eq.cummax() - 1).min(), "Nb trades": len(tr),
            "Taux de réussite": len(wins) / len(tr) if len(tr) else np.nan,
            "Profit factor": wins.sum() / -loss.sum() if loss.sum() < 0 else np.nan,
            "Gain moyen": wins.mean() if len(wins) else np.nan, "Perte moyenne": loss.mean() if len(loss) else np.nan,
            "Durée moy. (j)": tr["Jours"].mean() if len(tr) else np.nan, "Exposition": (pos != 0).mean(),
            "Buy & Hold annualisé": (c.iloc[-1] / c.iloc[0]) ** (1 / yrs) - 1}

def evaluate(data, assets, period):
    s, e = period
    runs = {a: run_asset(a, data[a], UNIVERSE_SPREAD[a]) for a in assets}
    per_asset, trades, port = [], [], {}
    for a, rr in runs.items():
        c = data[a]["close"]
        for sname, (pos, ret, r, sp, w) in rr.items():
            sl = slice(s, e)
            tr = trades_of(c.loc[sl], pos.loc[sl], sp.loc[sl])
            per_asset.append({"Actif": a, "Stratégie": sname, **metrics(ret.loc[sl], tr, pos.loc[sl], c.loc[sl])})
            if len(tr): trades.append(tr.assign(Actif=a, Stratégie=sname))
            port.setdefault(sname, {})[a] = (ret * w).loc[sl]
    rows, curves, yearly = [], {}, {}
    for sname, d in port.items():
        pr = pd.DataFrame(d).fillna(0).mean(axis=1); eq = (1 + pr).cumprod(); curves[sname] = eq
        yrs = (pr.index[-1] - pr.index[0]).days / 365.25
        yr = (1 + pr).groupby(pr.index.year).prod() - 1; yearly[sname] = yr
        rows.append({"Stratégie": sname, "Rendement annualisé": eq.iloc[-1] ** (1 / yrs) - 1,
                     "Volatilité": pr.std() * np.sqrt(252), "Sharpe": pr.mean() / pr.std() * np.sqrt(252),
                     "Drawdown max": (eq / eq.cummax() - 1).min(),
                     "Années positives": f"{(yr > 0).sum()}/{len(yr)}"})
    return pd.DataFrame(rows), pd.DataFrame(per_asset), (pd.concat(trades) if trades else pd.DataFrame()), curves, pd.DataFrame(yearly)

# ============================================================== GRAPHIQUES
def charts(curves, corrB, assets):
    import matplotlib; matplotlib.use("Agg"); import matplotlib.pyplot as plt
    from matplotlib.colors import LinearSegmentedColormap
    INK, INK2, GRID, SURF = "#0b0b0b", "#52514e", "#e6e5e0", "#fcfcfb"
    COLS = ["#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#6250d6"]
    plt.rcParams.update({"font.size": 10, "figure.facecolor": SURF, "axes.facecolor": SURF, "axes.edgecolor": GRID,
                         "xtick.color": INK2, "ytick.color": INK2, "axes.labelcolor": INK2})
    fig, ax = plt.subplots(figsize=(10, 5.4), dpi=160)
    for (n, eq), col in zip(curves.items(), COLS):
        eq = eq / eq.iloc[0] * 100; ax.plot(eq.index, eq.values, color=col, lw=2, label=f"{n} → {eq.iloc[-1]:.0f}")
    ax.axhline(100, color=INK2, lw=0.8, ls=(0, (2, 3))); ax.axvline(pd.Timestamp(PERIOD_B[0]), color=INK2, lw=0.8)
    ax.text(pd.Timestamp(PERIOD_A[0]), ax.get_ylim()[1], "  A : sélection", va="top", fontsize=9, color=INK2)
    ax.text(pd.Timestamp(PERIOD_B[0]), ax.get_ylim()[1], "  B : hors échantillon", va="top", fontsize=9, color=INK2)
    ax.grid(axis="y", color=GRID, lw=0.6); ax.spines[["top", "right"]].set_visible(False)
    ax.set_ylabel("Valeur du portefeuille (base 100)")
    ax.set_title(f"Portefeuille {', '.join(assets)} – risque égal, après spreads", loc="left", fontsize=11, color=INK, fontweight="bold")
    ax.legend(frameon=False, fontsize=8.5, loc="best"); fig.tight_layout(); fig.savefig(f"{OUT}/courbes_capital.png"); plt.close(fig)

    cmap = LinearSegmentedColormap.from_list("div", ["#2a78d6", "#f0efec", "#e34948"])
    fig, ax = plt.subplots(figsize=(6.2, 5.2), dpi=160); n = len(assets)
    ax.imshow(corrB.values, cmap=cmap, vmin=-1, vmax=1)
    ax.set_xticks(range(n), assets); ax.set_yticks(range(n), assets)
    for i in range(n):
        for j in range(n): ax.text(j, i, f"{corrB.values[i, j]:.2f}", ha="center", va="center", color=INK)
    for s_ in ax.spines.values(): s_.set_visible(False)
    ax.set_title("Corrélation de Pearson\nrendements log quotidiens, période B", loc="left", fontsize=11, color=INK, fontweight="bold")
    fig.tight_layout(); fig.savefig(f"{OUT}/correlation_5_actifs.png"); plt.close(fig)

# ============================================================== PRINCIPAL
UNIVERSE_SPREAD = {name: sp for _, (name, sp) in UNIVERSE.items()}

def main(data=None):
    pd.set_option("display.width", 220); pd.set_option("display.max_columns", 20)
    _selftest()
    if os.path.exists(OUT): shutil.rmtree(OUT)
    os.makedirs(OUT)
    if data is None:
        print("\nTéléchargement Yahoo Finance…"); data = download()
    chosen, corrA, er, feas, mincorr, thr = select_assets(data)
    print("\n>>> ACTIFS RETENUS :", ", ".join(chosen))

    closes = pd.DataFrame({k: data[k]["close"] for k in chosen}).where(lambda x: x > 0)
    corrB = np.log(closes).diff().loc[PERIOD_B[0]:PERIOD_B[1]].corr()
    end_B = min(data[a].index[-1] for a in chosen).strftime("%Y-%m-%d")
    pB = (PERIOD_B[0], PERIOD_B[1] or end_B)

    resA = evaluate(data, chosen, PERIOD_A)
    resB = evaluate(data, chosen, pB)
    full = evaluate(data, chosen, (PERIOD_A[0], pB[1]))
    charts(full[3], corrB, chosen)

    print("\n=== PORTEFEUILLE – période A (sélection) ===");      print(resA[0].round(3).to_string(index=False))
    print("\n=== PORTEFEUILLE – période B (hors échantillon) ==="); print(resB[0].round(3).to_string(index=False))
    pa = resB[1]; v = pa[pa["Stratégie"].isin(["V1 AMA Variation (Best)", "V2 AMA + top-down + ADX>=17"])]
    print("\n=== PAR ACTIF – période B (V1 et V2) ===")
    print(v[["Actif", "Stratégie", "Rendement annualisé", "Profit factor", "Taux de réussite", "Drawdown max", "Nb trades", "Buy & Hold annualisé"]].round(3).to_string(index=False))
    print("\n=== CORRÉLATION – période B ==="); print(corrB.round(2))

    raw = pd.concat([d.assign(actif=k) for k, d in data.items()]); raw.index.name = "date"
    raw.to_csv(f"{OUT}/donnees_ohlc_yahoo.csv")
    corrA.round(4).to_csv(f"{OUT}/correlation_tous_periodeA.csv"); corrB.round(4).to_csv(f"{OUT}/correlation_retenus_periodeB.csv")
    er.round(4).to_csv(f"{OUT}/ratio_efficacite_periodeA.csv", header=["ER"])
    feas.assign(Actifs=feas["Actifs"].map(", ".join)).to_csv(f"{OUT}/selection_top15.csv", index=False)
    mincorr.assign(Actifs=mincorr["Actifs"].map(", ".join)).to_csv(f"{OUT}/combinaisons_moins_correlees.csv", index=False)
    for lab, res in (("A", resA), ("B", resB)):
        res[0].to_csv(f"{OUT}/portefeuille_periode{lab}.csv", index=False)
        res[1].to_csv(f"{OUT}/par_actif_periode{lab}.csv", index=False)
        res[2].to_csv(f"{OUT}/trades_periode{lab}.csv", index=False)
    full[4].to_csv(f"{OUT}/rendements_annuels.csv")
    with open(f"{OUT}/parametres.txt", "w") as f:
        f.write(f"Actifs: {chosen}\nSeuil |r|: {thr}\nPériode A: {PERIOD_A}\nPériode B: {pB}\nADX_MIN: {ADX_MIN}\n"
                f"Vol cible: {VOL_TARGET}\nSpreads: {UNIVERSE_SPREAD}\nForcé: {FORCE_ASSETS}\n")
    zname = f"{OUT}.zip"
    with zipfile.ZipFile(zname, "w", zipfile.ZIP_DEFLATED) as z:
        for fn in os.listdir(OUT): z.write(os.path.join(OUT, fn), fn)
    print(f"\nTerminé. Fichier à m'envoyer : {zname}")
    try:
        from google.colab import files; files.download(zname)
    except Exception:
        pass
    return data

if __name__ == "__main__":
    main()
