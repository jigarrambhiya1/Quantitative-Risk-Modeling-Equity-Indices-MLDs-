# A Parametric Approach to Quantifying Risk and Uncertainty in Indian Market-Linked Debentures

**NMIMS – Nilkamal School of Mathematics, Applied Statistics & Analytics**  
**Group 8 | Semester 1 Project | Mentor: Prof. Vaibhav Vasundekar**

---

## Overview

This project develops a parametric statistical framework to model return distributions of three major Indian equity indices — **Nifty 50**, **Bank Nifty**, and **Sensex** — and applies the fitted models to quantify risk in **Market-Linked Debentures (MLDs)**.

The core pipeline:

1. Compute log returns from daily closing prices
2. Model volatility clustering using **ARMA(1,1)-eGARCH(1,1)** with skew-t innovations
3. Fit **12 parametric distributions** to the filtered residuals via MLE
4. Select the best-fit distribution per index using AIC, BIC, KS test, and graphical left-tail tests
5. Estimate **Value-at-Risk (VaR)** and **Conditional Tail Expectation (CTE)** at 95% and 99% confidence levels
6. Apply the risk framework to MLD payoff structures (both historical data and Monte Carlo simulation)
7. Validate VaR estimates using **Kupiec's unconditional** and **Christoffersen's conditional coverage backtests**

---

## Data

Daily closing prices for the three indices were sourced from [Yahoo Finance](https://finance.yahoo.com/), covering the period **January 2, 2015 to August 30, 2024** (~10 years).

**To reproduce the analysis:**
- Download the historical data for each index from Yahoo Finance:
  - Nifty 50: `^NSEI`
  - Bank Nifty: `^NSEBANK`
  - Sensex: `^BSESN`
- Save as `.xlsx` or `.csv` and update the file path in each script accordingly.

> Raw data files are not included in this repository due to licensing constraints.

---

## Repository Structure

```
mld-risk-analysis/
├── README.md
├── scripts/
│   ├── nifty50_analysis.R        # Full analysis pipeline for Nifty 50
│   ├── banknifty_analysis.R      # Full analysis pipeline for Bank Nifty
│   └── sensex_analysis.R         # Full analysis pipeline for Sensex
├── report/
│   └── Report.pdf                # Full written report
└── presentation/
    └── PPT.pdf                   # Project presentation slides

```

---

## Methodology Summary

### 1. Volatility Modelling
Raw log returns exhibit **volatility clustering** and **autocorrelation in squared returns** (confirmed via ACF plots). To obtain approximately i.i.d. residuals, an **ARMA(1,1)-eGARCH(1,1)** model under skew-t innovations was fitted using the `rugarch` package.

The eGARCH conditional variance equation:

$$\log(\sigma_t^2) = \omega + \alpha\left(\frac{|\epsilon_{t-1}|}{\sigma_{t-1}} - E\left[\frac{|\epsilon_{t-1}|}{\sigma_{t-1}}\right]\right) + \gamma\frac{\epsilon_{t-1}}{\sigma_{t-1}} + \beta\log(\sigma_{t-1}^2)$$

Standardized residuals $z_t = \epsilon_t / \sigma_t$ are then used for distribution fitting.

### 2. Distributions Fitted (via MLE)

| # | Distribution         | Parameters |
|---|----------------------|------------|
| 1 | Normal               | 2 |
| 2 | Laplace              | 2 |
| 3 | Cauchy               | 2 |
| 4 | Student's t          | 3 |
| 5 | Skew Normal          | 3 |
| 6 | Skew t               | 4 |
| 7 | Skew Cauchy          | 3 |
| 8 | Skew Laplace         | 3 |
| 9 | Hyperbolic           | 4 |
| 10 | Normal Inverse Gaussian (NIG) | 4 |
| 11 | Variance Gamma (VG)  | 4 |
| 12 | Generalized Hyperbolic (GH) | 5 |

### 3. Best-Fit Distributions

| Index     | Best-Fit Distribution     |
|-----------|---------------------------|
| Nifty 50  | Generalized Hyperbolic    |
| Bank Nifty| Skew-t                    |
| Sensex    | Skew-t                    |

Selection based on lowest AIC/BIC, highest KS test p-value (fail to reject H₀), and best graphical left-tail fit.

### 4. Risk Measures

**Value-at-Risk (VaR):**  
$$\text{VaR}_\theta = \inf\{l \in \mathbb{R} : F_L(l) \geq \theta\}$$

**Conditional Tail Expectation (CTE):**  
$$\text{CTE}_\theta = E\{L \mid L > \text{VaR}_\theta\}$$

### 5. MLD Payoff Structures

**Principal-Protected MLD:**  
$$\text{Payoff} = \text{Principal} \times (1 + \max(0,\ P_{\text{rate}} \times r))$$

**Non-Principal-Protected MLD:**  
$$\text{Payoff} = \text{Principal} \times (1 + P_{\text{rate}} \times r)$$
$$\text{Loss} = \frac{\text{Principal} - \text{Payoff}}{\text{Principal}}$$

Participation rate used: **80%** | Principal: **₹100** | Maturity: **3 years**

---

## Key Results

### VaR and CTE — Historical Data (Direct Equity Investment)

| Measure    | Nifty 50 (%) | Bank Nifty (%) | Sensex (%) |
|------------|-------------|----------------|------------|
| VaR (99%)  | 2.70        | 4.18           | 2.70       |
| VaR (95%)  | 1.50        | 2.11           | 1.47       |
| CTE (99%)  | 4.57        | 6.69           | 4.61       |
| CTE (95%)  | 2.45        | 3.59           | 2.45       |

### VaR and CTE — Principal-Protected MLD

| Measure    | Nifty 50 | Bank Nifty | Sensex |
|------------|----------|------------|--------|
| VaR (99%)  | 0%       | 0%         | 0%     |
| CTE (99%)  | 0%       | 0%         | 0%     |

### VaR Backtesting Results

| Index      | Expected Violations | Actual Violations | LR_uc p-value | LR_cc p-value | Decision       |
|------------|--------------------|--------------------|---------------|---------------|----------------|
| Nifty 50   | 26                 | 24                 | 0.657         | 0.726         | Do not reject H₀ |
| Bank Nifty | 23                 | 22                 | 0.758         | 0.774         | Do not reject H₀ |
| Sensex     | 26                 | 24                 | 0.634         | 0.716         | Do not reject H₀ |

All models pass both coverage tests — the VaR framework is statistically reliable.

---

## R Packages Required

```r
install.packages(c(
  "readxl",    # data import
  "rugarch",   # eGARCH modelling
  "MASS",      # Normal, Cauchy, t fitting
  "VGAM",      # Laplace fitting
  "sn",        # Skew Normal, Skew t, Skew Cauchy
  "ald",       # Skew Laplace
  "ghyp",      # Hyperbolic, NIG, VG, Generalized Hyperbolic
  "moments",   # Skewness, Kurtosis
  "tseries",   # Jarque-Bera test
  "PerformanceAnalytics",  # VaR backtesting
  "xts",       # Time series handling
  "ggplot2"    # Visualizations
))
```

---

## How to Run

1. Clone the repository:
   ```bash
   git clone https://github.com/jigarrambhiya1/Quantitative-Risk-Modeling-Equity-Indices-MLDs-.git
   cd mld-risk-analysis
   ```

2. Download the data from Yahoo Finance (see Data section above).

3. Update the file path at the top of each script (line 2):
   ```r
   nifty <- read_excel("your/path/to/data.xlsx")
   ```

4. Run the script for the desired index in R or RStudio:
   ```r
   source("scripts/nifty50_analysis.R")
   ```

---

## Conclusions

- Indian equity return distributions exhibit **negative skewness, excess kurtosis, and fat tails** — normality is rejected for all three indices.
- **Bank Nifty** is the riskiest index across all measures, followed by Nifty 50 and Sensex.
- **Principal-Protected MLDs** have zero VaR and CTE — capital is fully protected by design.
- **Non-Principal-Protected MLDs** carry moderate risk: lower than direct equity but higher than zero.
- The parametric GARCH-filtered approach produces VaR estimates that pass formal backtesting, confirming the framework's reliability.

---

## References

1. Choi, S.-Y., & Yoon, J.-H. (2020). *Modeling and Risk Analysis Using Parametric Distributions with an Application in Equity-Linked Securities.* https://doi.org/10.1155/2020/9763065
2. Pokharel et al. (2024). *Probability Distributions for Modeling Stock Market Returns — An Empirical Inquiry.* https://doi.org/10.3390/ijfs12020043
3. Williams, B. (2011). *GARCH(1,1) Models.* https://math.berkeley.edu/~btw/thesis4.pdf
4. An Introduction to Market Linked Debentures. https://www.incredmoney.com/blog/an-introduction-to-market-linked-debentures/

---

## Authors

| Roll No | Name                |
|---------|---------------------|
| A011    | Tejasvini Dashaputre|
| A017    | Aakash Godave       |
| A029    | Parin Kulkarni      |
| A046    | Jigar Rambhiya      |
| A056    | Khushi Shukla       |

*NMIMS University — MSc Statistics & Data Science, Semester 1*
