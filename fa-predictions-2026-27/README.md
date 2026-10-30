# Free-agent contract predictions, 2026-27 (public copy)

Written 2026-10-28 10:11 (US Central), before free agency opened. Code commit: `17329ff`; private lock commit: `9c45b9d` (private repo).

## What is private, and how to check it
The graded lock also anchors some players on a published forecast from a paid source, so that file stays private. This folder holds our model's own numbers for every player.
SHA-256 of the private files:

| File | SHA-256 as stored in the repository (LF line endings) | SHA-256 of the Windows working copy (CRLF) |
|---|---|---|
| `predictions.csv` | `dff8aa10052f7277e50104ee1d2ff3b858aae0abee309e9a72ed8ed1128cdf36` | `300f4ad22d4c5cc4bd8a1e66c3fbd6fd96185ce522dee2d773d7f504c0f96a2c` |
| `README.md` | `410d6aafcda772e5f5543221fdd379d21ade65a32269af8cd813f08cbd06f552` | `1715c241f0f86054643bfb0728e389293a414df9a4eab4684418ea142f44636a` |

The hash lets anyone verify in spring that the private file we grade against is the one committed now, unchanged.

## The model
- Price model v2+b: years first (a cumulative logit), then AAV given years (a censored normal on log AAV as a share of league payroll), fit with brms on 11 winters of free-agent contracts, 2015-16 to 2025-26, with market heat as a random walk. Inputs known before the winter, including our own Marcel projection for 2027 and a 3-year track record with the 2026 season's deviation from it.
- The anchor (`model_years`, `model_aav`, `model_total`): the model's median length (7 means 7 or more, taken as 7), its AAV at that length, and total = AAV x years, with its 80% ranges (`model_*_low`, `model_*_high`).
- The slider (`aav_1y` ... `aav_10y`, `total_1y` ... `total_10y`) shows aging only: the anchor's AAV scaled to each length by the value of the player's projected WAR in those years (a survivorship-corrected aging curve; each season worth the league minimum plus a price per win). AAV never rises with length; total always rises. It leaves out the price of security and every clause (opt-outs, options, deferrals, no-trade).
- `slider_estimate_lengths`: lengths longer than the longest deal comparable players have signed (same role and tier, within 2 years of age, or 4 if fewer than 5 such deals). Those are estimates.
- `v2_*`: the model's own unconditional predictions (length drawn from the years model): years p10/median/p90, AAV median, total p10/median/p90.
- `raise_band`, `raise_flag`: a sense check of `model_aav` against what players of the same tier got over their previous salary (arbitration or an earlier free-agent contract) in 2019-20 to 2025-26: the 10th-90th percentile of that ratio, and whether the prediction falls `below band`, `inside` or `above band` (`not comparable`: no known previous salary). Flagged rows are for review; nothing is adjusted.
- Dollars are 2027 dollars (shares of league average payroll times the projected 2027 average). One row per player and branch (`no QO`, or `QO attached` where a qualifying offer is still possible).

## Known limitations
- Our Marcel projection still leans on 2026. Previous salary is a sense check, not a model input.
- The model is weakest at the top of the market.
- A lockout is expected Dec 2; the model has one lockout winter to learn from.

Derived results only. No contract records or paid-source data are in this folder.
