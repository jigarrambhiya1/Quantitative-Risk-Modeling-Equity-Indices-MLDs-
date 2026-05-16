# Data Guide

Raw data files are **not included** in this repository. All data used in this project consists of publicly available daily closing prices downloaded from [Yahoo Finance](https://finance.yahoo.com/).

---

## Indices Used

| Index      | Yahoo Finance Ticker | Exchange         | Period Covered              |
|------------|----------------------|------------------|-----------------------------|
| Nifty 50   | `^NSEI`              | NSE (India)      | Jan 2, 2015 – Aug 30, 2024  |
| Bank Nifty | `^NSEBANK`           | NSE (India)      | Jan 2, 2015 – Aug 30, 2024  |
| Sensex     | `^BSESN`             | BSE (India)      | Jan 2, 2015 – Aug 30, 2024  |

---

## How to Download

### Option 1 — Manual Download (Yahoo Finance website)

1. Go to [https://finance.yahoo.com](https://finance.yahoo.com)
2. Search for the ticker (e.g., `^NSEI`)
3. Click on the result → go to the **Historical Data** tab
4. Set the date range: **Jan 2, 2015** to **Aug 30, 2024**
5. Set frequency to **Daily**
6. Click **Download** — this gives you a `.csv` file
7. Repeat for all three tickers

### Option 2 — Download Directly in R

You can pull the data straight into R without visiting the website using the `quantmod` package:

```r
install.packages("quantmod")
library(quantmod)

# Set date range
start_date <- "2015-01-02"
end_date   <- "2024-08-30"

# Download all three indices
getSymbols("^NSEI",    src = "yahoo", from = start_date, to = end_date)
getSymbols("^NSEBANK", src = "yahoo", from = start_date, to = end_date)
getSymbols("^BSESN",   src = "yahoo", from = start_date, to = end_date)

# Extract closing prices
nifty_close    <- Cl(NSEI)
banknifty_close <- Cl(NSEBANK)
sensex_close   <- Cl(BSESN)

# Convert to data frames
nifty_df    <- data.frame(Date = index(nifty_close),    Close = as.numeric(nifty_close))
banknifty_df <- data.frame(Date = index(banknifty_close), Close = as.numeric(banknifty_close))
sensex_df   <- data.frame(Date = index(sensex_close),   Close = as.numeric(sensex_close))

# Optional: save as CSV
write.csv(nifty_df,     "data/nifty50.csv",    row.names = FALSE)
write.csv(banknifty_df, "data/banknifty.csv",  row.names = FALSE)
write.csv(sensex_df,    "data/sensex.csv",     row.names = FALSE)
```

---

## Expected Data Format

After downloading, each file should have at minimum the following columns.  
The scripts use only **Date** and **Close**.

| Column    | Type      | Description                        |
|-----------|-----------|------------------------------------|
| Date      | Date      | Trading date (YYYY-MM-DD)          |
| Open      | Numeric   | Opening price                      |
| High      | Numeric   | Intraday high                      |
| Low       | Numeric   | Intraday low                       |
| Close     | Numeric   | Closing price ← used in analysis   |
| Adj Close | Numeric   | Adjusted closing price             |
| Volume    | Integer   | Trading volume                     |

---

## Updating the File Path in Scripts

Once your data is downloaded, update the import line at the top of each script:

```r
# If using CSV
nifty <- read.csv("data/nifty50.csv")

# If using Excel
library(readxl)
nifty <- read_excel("data/nifty50.xlsx")
```

Make sure the `Date` column is parsed correctly:

```r
nifty$Date <- as.Date(nifty$Date)
```

---

## Sample Size After Log Return Calculation

Log returns are computed as $R_t = \log(P_t / P_{t-1})$, which removes the first observation.  
The effective sample sizes used in this study were:

| Index      | Raw Observations | After Return Calculation |
|------------|-----------------|--------------------------|
| Nifty 50   | 2,624           | 2,623                    |
| Bank Nifty | 2,348           | 2,347                    |
| Sensex     | 2,641           | 2,640                    |

The slight difference in Bank Nifty observations is due to index-specific trading holidays.

---

## Notes

- Yahoo Finance occasionally has **missing values** for Indian indices on certain holidays. The scripts handle this with `na.omit()` — verify your download does not have unexpected gaps.
- If you are replicating the study for a **different time period**, re-fit the eGARCH model and all 12 distributions from scratch. Do not reuse the parameter estimates reported in the paper, as they are specific to the Jan 2015 – Aug 2024 window.
- All prices are in **Indian Rupees (INR)**.
