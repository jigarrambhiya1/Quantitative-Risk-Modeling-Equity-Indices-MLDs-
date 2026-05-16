#sensex
#install.packages("readxl")
#library(readxl)
#sensex <- read_excel("C:\\Users\\Tejaswani\\Documents\\college\\sensex rt sem1.xlsx")
library(readxl)
sensex<- read_excel("sensex rt sem1.xlsx")
View(sensex_rt_sem1)
View(sensex)
returns=diff(log(sensex$Close));returns  # log returns
sn=sensex[-1,];sn


sensex=cbind(sn,returns);sensex

plot(sensex$Close~sensex$Date,type='l',xlab="TIME INTERVAL",ylab="DAILY PRICES",col="red")
plot(sensex$returns~sensex$Date,type='l',xlab="TIME INTERVAL",ylab="DAILY LOG PRICES",col="purple")

hist(sensex$returns,
     breaks = 50,             # number of bins
     col = "darkblue",
     main = "HISTOGRAM",
     xlab = "LOG RETURNS",
     ylim = c(0, 900),        # y-axis limit from 0 to 150
     yaxt = "n")              # suppress default axis

axis(2, at = seq(0, 900, by = 50))   # add custom y-axis ticks

#ACF PLOTS
par(mfrow=c(1,2))
obj=acf(sensex$returns,plot=FALSE)   
plot(obj, main = "ACF of Returns",col="red",ylab="sample autocorrelation")
points(obj$lag,obj$acf,pch=16,col="red")


obj1=acf(sensex$returns^2,plot=FALSE);obj1
plot(obj1, main ="ACF of Squared Returns",col="red",ylab="sample autocorrelation")
points(obj1$lag,obj1$acf,pch=16,col="red")



#no.of observations
obs=nrow(sensex);obs
#mean
m=mean(sensex$returns);m

#median
med=median(sensex$returns);med
#standard deviation
SD=sd(sensex$returns);SD

#install.packages("moments")
#install.packages("e1071")
library(moments)

library(e1071)

#Skewness
skewness(sensex$returns)

#kurtosis
kurtosis(sensex$returns)



#shapiro wilk
shapiro.test((sensex$returns))
#p-value<0.05 ,data does not follow normal distribution

#jarque bera

#install.packages("tseries")
library(tseries)
jarque.bera.test(sensex$returns)
#p-value<0.05 ,data does not follow normal distribution

# GARCH(1,1)

# library(tseries)
# Test for ARCH effects (volatility clustering)
#install.packages("FinTS")
library(FinTS)
ArchTest(sensex$returns, lags = 12)
# p-value < 0.05 → you have ARCH effects → GARCH model is needed.

#install.packages("rugarch")
library(rugarch)
# Define GARCH(1,1) model
spec <-ugarchspec(  
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)), 
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE), 
  distribution.model = "sstd" 
)
# Fit model
fit <- ugarchfit(spec , data = sensex$returns)
show(fit)
# data shows volatility clusterings

# Plot Conditional Volatility
plot(sigma(fit), main="Conditional Volatility (eGARCH 1,1)")

# Check Residuals
resid <- residuals(fit, standardize=TRUE)
acf(resid, main="ACF of Standardized Residuals")
acf(resid^2, main="ACF of Squared Residuals")

# Forecast Volatility
forecast <- ugarchforecast(fit, n.ahead=10)
plot(forecast, which=3)   # 10-day ahead volatility forecast

# Extract Filtered Residuals (for distribution fitting later)
filtered_resid <- residuals(fit, standardize=TRUE)
filtered_resid <- as.numeric(filtered_resid)

# use to fit parametric distributions
#install.packages("fitdistrplus")
library(fitdistrplus)   # Normal, Laplace, etc.
#install.packages("MASS")
library(MASS)           # Student's t, Cauchy
#install.packages("sn")
library(sn)             # Skew Normal , Skew t, Skew-Cauchy
#install.packages("ghyp")
library(ghyp)           # Hyperbolic, NIG, Variance Gamma, Generalized Hyperbolic
#install.packages("gamlss")
library(gamlss)         # Skew-Laplace
#install.packages("gamlss.dist")
library(gamlss.dist)
#install.packages("VGAM") # Laplace
library(VGAM)

n <- length(filtered_resid)

norm=fitdist(filtered_resid, "norm")
summary(norm)
loglik_norm <- as.numeric(logLik(norm))
k_norm <- length(coef(norm))
aic_norm <- norm$aic 
bic_norm <- norm$bic

laplace=vglm(filtered_resid ~ 1, laplace(), trace = TRUE)
summary(laplace)
loglik_laplace <- as.numeric(logLik(laplace))
k_laplace <- length(coef(laplace))
aic_laplace <- AIC(laplace)
bic_laplace <- BIC(laplace)

t <- fitdistr(filtered_resid, "t")
summary(t)
loglik_t <- as.numeric(logLik(t))
k_t <- length(coef(t))
aic_t <- AIC(t)
bic_t <- BIC(t)

cauchy <- fitdistr(filtered_resid, "cauchy")
summary(cauchy)
loglik_cauchy <- as.numeric(logLik(cauchy))
k_cauchy <- length(coef(cauchy))
aic_cauchy <- AIC(cauchy)
bic_cauchy <- BIC(cauchy)

sn <- selm(filtered_resid ~ 1, family = "SN")
summary(sn)
loglik_sn <- as.numeric(logLik(sn))
k_sn <- length(coef(sn))
aic_sn <- AIC(sn)
bic_sn <- -2 * loglik_sn + k_sn * log(n)

st <- selm(filtered_resid ~ 1, family = "ST")
summary(st)
loglik_st <- as.numeric(logLik(st))
k_st <- length(coef(st))
aic_st <- AIC(st)
bic_st <- -2 * loglik_st + k_st * log(n)
pars_st =coef(st, param.type = "DP")
mu    = pars_st["xi"];mu
sigma = pars_st["omega"];sigma
alpha = pars_st["alpha"];alpha
nu    = pars_st["nu"];nu


sc <- selm(filtered_resid ~ 1, family = "SC")
summary(sc)
loglik_sc <- as.numeric(logLik(sc))
k_sc <- length(coef(sc))
aic_sc <- AIC(sc)
bic_sc <- -2 * loglik_sc + k_sc * log(n)

sl <- gamlss(filtered_resid ~ 1, family = SEP1)
sl <- gamlss(filtered_resid ~ 1, family = SEP1)
summary(sl)
loglik_sl <- as.numeric(logLik(sl))
k_sl <- length(coef(sl))
aic_sl <- AIC(sl)
bic_sl <- BIC(sl)

mu_hat     <- coef(sl, what = "mu")["(Intercept)"]
sigma_hat  <- exp(coef(sl, what = "sigma")["(Intercept)"])   # log link → exponentiate
nu_hat     <- coef(sl, what = "nu")["(Intercept)"]
tau_hat    <- exp(coef(sl, what = "tau")["(Intercept)"])     # log link → exponentiate



hyp <- fit.hypuv(filtered_resid)
summary(hyp)
loglik_hyp <- as.numeric(logLik(hyp))
k_hyp <- length(coef(hyp))
aic_hyp <- AIC(hyp)
bic_hyp <- -2 * loglik_hyp + k_hyp * log(n)

nig <- fit.NIGuv(filtered_resid)
summary(nig)
loglik_nig <- as.numeric(logLik(nig))
k_nig <- length(coef(nig))
aic_nig <- AIC(nig)
bic_nig <- -2 * loglik_nig + k_nig * log(n)

vg <- fit.VGuv(filtered_resid)
summary(vg)
loglik_vg <- as.numeric(logLik(vg))
k_vg <- length(coef(vg))
aic_vg <- AIC(vg)
bic_vg <- -2 * loglik_vg + k_vg * log(n)

ghyp <- fit.ghypuv(filtered_resid)
summary(ghyp)
loglik_ghyp <- as.numeric(logLik(ghyp))
k_ghyp <- length(coef(ghyp))
aic_ghyp <- AIC(ghyp)
bic_ghyp <- -2 * loglik_ghyp + k_ghyp * log(n)

# Comparison Table
results <- data.frame(
  Distribution = c("Normal","Laplace","Student-t","Cauchy",
                   "Skew-Normal","Skew-t","Skew-Cauchy","Skew-Laplace",
                   "Hyperbolic","NIG","Variance Gamma","GHyp"),
  AIC = c(aic_norm,aic_laplace,aic_t,aic_cauchy,
          aic_sn,aic_st,aic_sc,aic_sl,
          aic_hyp,aic_nig,aic_vg,aic_ghyp),
  BIC = c(bic_norm,bic_laplace,bic_t,bic_cauchy,
          bic_sn,bic_st,bic_sc,bic_sl,
          bic_hyp,bic_nig,bic_vg,bic_ghyp)
)

print(results)

#install.packages("goftest")
library(goftest)

ks_norm=ks.test(filtered_resid, "pnorm", mean = mean(filtered_resid), sd = sd(filtered_resid))
ad_norm=ad.test(filtered_resid, null = function(x) pnorm(x, mean = mean(filtered_resid), sd = sd(filtered_resid)))
cvm_norm=cvm.test(filtered_resid, null = function(x) pnorm(x, mean = mean(filtered_resid), sd = sd(filtered_resid)))

loc <- coef(laplace)[1]
scale <- exp(coef(laplace)[2])
ks_laplace=ks.test(filtered_resid, "plaplace", location = loc, scale = scale)
ad_laplace=ad.test(filtered_resid, null = function(x) plaplace(x, location = loc, scale = scale))
cvm_laplace=cvm.test(filtered_resid, null = function(x) plaplace(x, location = loc, scale = scale))

df_t <- t$estimate["df"]
ks_t=ks.test(filtered_resid, "pt", df = df_t)
ad_t=ad.test(filtered_resid, null = function(x) pt(x, df = df_t))
cvm_t=cvm.test(filtered_resid, null = function(x) pt(x, df = df_t))

loc_c <- cauchy$estimate["location"]
scale_c <- cauchy$estimate["scale"]
ks_cauchy=ks.test(filtered_resid, "pcauchy", location = loc_c, scale = scale_c)
ad_cauchy=ad.test(filtered_resid, null = function(x) pcauchy(x, location = loc_c, scale = scale_c))
cvm_cauchy=cvm.test(filtered_resid, null = function(x) pcauchy(x, location = loc_c, scale = scale_c))

dp_sn <- sn@param$dp
ks_sn=ks.test(filtered_resid, "psn", xi = dp_sn["xi"], omega = dp_sn["omega"], alpha = dp_sn["alpha"])
ad_sn=ad.test(filtered_resid, null = function(x) psn(x, xi = dp_sn["xi"], omega = dp_sn["omega"], alpha = dp_sn["alpha"]))
cvm_sn=cvm.test(filtered_resid, null = function(x) psn(x, xi = dp_sn["xi"], omega = dp_sn["omega"], alpha = dp_sn["alpha"]))

dp_st <- st@param$dp
ks_st=ks.test(filtered_resid, "pst", xi = dp_st["xi"], omega = dp_st["omega"],
              alpha = dp_st["alpha"], nu = dp_st["nu"])
ad_st=ad.test(filtered_resid, null = function(x) pst(x, xi = dp_st["xi"], omega = dp_st["omega"],
                                                     alpha = dp_st["alpha"], nu = dp_st["nu"]))
cvm_st=cvm.test(filtered_resid, null = function(x) pst(x, xi = dp_st["xi"], omega = dp_st["omega"],
                                                       alpha = dp_st["alpha"], nu = dp_st["nu"]))

dp_sc <- sc@param$dp
ks_sc=ks.test(filtered_resid, "psc", xi = dp_sc["xi"], omega = dp_sc["omega"], alpha = dp_sc["alpha"])
ad_sc=ad.test(filtered_resid, null = function(x) psc(x, xi = dp_sc["xi"], omega = dp_sc["omega"], alpha = dp_sc["alpha"]))
cvm_sc=cvm.test(filtered_resid, null = function(x) psc(x, xi = dp_sc["xi"], omega = dp_sc["omega"], alpha = dp_sc["alpha"]))

mu_sep  <- fitted(sl, "mu")[1]
sigma_sep <- fitted(sl, "sigma")[1]
nu_sep  <- fitted(sl, "nu")[1]
tau_sep <- fitted(sl, "tau")[1]
ks_sl=ks.test(filtered_resid, "pSEP1", mu = mu_sep, sigma = sigma_sep, nu = nu_sep, tau = tau_sep)
ad_sl=ad.test(filtered_resid, null = function(x) pSEP1(x, mu = mu_sep, sigma = sigma_sep, nu = nu_sep, tau = tau_sep))
cvm_sl=cvm.test(filtered_resid, null = function(x) pSEP1(x, mu = mu_sep, sigma = sigma_sep, nu = nu_sep, tau = tau_sep))

ks_hyp=ks.test(filtered_resid, "pghyp", object = hyp)
ad_hyp=ad.test(filtered_resid, null = function(x) pghyp( pghyp(x, object = hyp)))
cvm_hyp=cvm.test(filtered_resid, null = function(x) pghyp( pghyp(x, object = hyp)))

ks_nig=ks.test(filtered_resid, "pghyp", object = nig)
ad_nig=ad.test(filtered_resid, null = function(x) pghyp(x, object = nig))
cvm_nig=cvm.test(filtered_resid, null = function(x) pghyp(x, object = nig))

ks_vg=ks.test(filtered_resid, "pghyp", object = vg)
ad_vg=ad.test(filtered_resid, null = function(x) pghyp(x, object = vg))
cvm_vg=cvm.test(filtered_resid, null = function(x) pghyp(x, object = vg))

ks_ghyp=ks.test(filtered_resid, "pghyp", object = ghyp)
ad_ghyp=ad.test(filtered_resid, null = function(x) pghyp(x, object = ghyp))
cvm_ghyp=cvm.test(filtered_resid, null = function(x) pghyp(x, object = ghyp))

results1 <- data.frame(
  Distribution = c("Normal","Laplace","Student-t","Cauchy",
                   "Skew-Normal","Skew-t","Skew-Cauchy","Skew-Laplace",
                   "Hyperbolic","NIG","Variance Gamma","GHyp"),
  AIC = c(aic_norm,aic_laplace,aic_t,aic_cauchy,
          aic_sn,aic_st,aic_sc,aic_sl,
          aic_hyp,aic_nig,aic_vg,aic_ghyp),
  BIC = c(bic_norm,bic_laplace,bic_t,bic_cauchy,
          bic_sn,bic_st,bic_sc,bic_sl,
          bic_hyp,bic_nig,bic_vg,bic_ghyp),
  loglik = c(loglik_norm,loglik_laplace,loglik_t,loglik_cauchy,
             loglik_sn,loglik_st,loglik_sc,loglik_sl,
             loglik_hyp,loglik_nig,loglik_vg,loglik_ghyp),
  KS_p = c(ks_norm$p.value,ks_laplace$p.value,ks_t$p.value,ks_cauchy$p.value,
           ks_sn$p.value,ks_st$p.value,ks_sc$p.value,ks_sl$p.value,
           ks_hyp$p.value,ks_nig$p.value,ks_vg$p.value,ks_ghyp$p.value),
  KS_stat = c(ks_norm$statistic,ks_laplace$statistic,ks_t$statistic,ks_cauchy$statistic,
              ks_sn$statistic,ks_st$statistic,ks_sc$statistic,ks_sl$statistic,
              ks_hyp$statistic,ks_nig$statistic,ks_vg$statistic,ks_ghyp$statistic),
  AD_p = c(ad_norm$p.value,ad_laplace$p.value,ad_t$p.value,ad_cauchy$p.value,
           ad_sn$p.value,ad_st$p.value,ad_sc$p.value,ad_sl$p.value,
           ad_hyp$p.value,ad_nig$p.value,ad_vg$p.value,ad_ghyp$p.value),
  CvM_p = c(cvm_norm$p.value,cvm_laplace$p.value,cvm_t$p.value,cvm_cauchy$p.value,
            cvm_sn$p.value,cvm_st$p.value,cvm_sc$p.value,cvm_sl$p.value,
            cvm_hyp$p.value,cvm_nig$p.value,cvm_vg$p.value,cvm_ghyp$p.value)
)

print(results1)

results1$rank_aic <- rank(results1$AIC, ties.method = "min")
results1$rank_ks  <- rank(results1$KS_stat, ties.method = "min")
results1$total_rank <- results1$rank_aic + results1$rank_ks

best_model <- results1[which.min(results1$total_rank), ]
best_model

filtered_resid_sort = sort(filtered_resid)
empirical_log <- log(1:n / (n + 1))

# t distribution
#Fhat <- pt(filtered_resid_sort, df = df_t)
#fitted_log <- log(Fhat)

# passed distributions are 

F_st <- pst(filtered_resid_sort, xi = coef(st, param.type = "DP")["xi"],
            omega = coef(st, param.type = "DP")["omega"],
            alpha = coef(st, param.type = "DP")["alpha"],
            nu = coef(st, param.type = "DP")["nu"])
F_sl <- pSEP1(filtered_resid_sort,
              mu    = mu_hat,
              sigma = sigma_hat,
              nu    = nu_hat,
              tau   = tau_hat)

F_hyp <- pghyp(filtered_resid_sort, object = hyp)

F_nig   <- pghyp(filtered_resid_sort, object = nig)

F_vg    <- pghyp(filtered_resid_sort, object = vg)

F_ghyp  <- pghyp(filtered_resid_sort, object = ghyp)

F_list <- list(
  "Skew t" = F_st,
  "skew laplace" = F_sl,
  "Hyperbolic" = F_hyp,
  "NIG" = F_nig,
  "Variance Gamma" = F_vg,
  "Generalized Hyperbolic" = F_ghyp
)

cols <- c("blue","darkgreen","cyan","darkred","darkblue",
          "darkmagenta")
ltys <- 1:7

plot(filtered_resid_sort, empirical_log, pch = 16, col = "black",
     main = "Graphical Tail Test (Left Tail)",
     xlab = "X(t)", ylab = "log(F(x))")
for (i in 1:length(F_list)) {
  lines(filtered_resid_sort, log(F_list[[i]]), col = cols[i], lwd = 2, lty = ltys[i])
}
#legend("bottomright", legend = names(F_list), col = cols, lty = ltys,
#lwd = 2, cex = 0.7, ncol = 2, bg = "white")


F_sl <- pSEP1(filtered_resid_sort, 
              mu = mu_sep, 
              sigma = sigma_sep, 
              nu = nu_sep, 
              tau = tau_sep)
# Helper plotting function
plot_tail <- function(F_fit, dist_name, color="blue") {
  plot(filtered_resid_sort, empirical_log, pch = 16, col = "black",
       main = paste("Graphical Tail Test -", dist_name),
       xlab = "X(t)", ylab = "log(F(x))")
  lines(filtered_resid_sort, log(F_fit), col = color, lwd = 2)
}
par(mfrow=c(2,2))      

#pars_sl <- coef(sl, param.type = "DP") 
F_sl <- pSEP1(filtered_resid_sort,
              mu    = mu_hat,
              sigma = sigma_hat,
              nu    = nu_hat,
              tau   = tau_hat)
plot_tail(F_sl, "Skew laplace", "darkgreen")


F_st <- pst(filtered_resid_sort, xi = pars_st["xi"], omega = pars_st["omega"],
            alpha = pars_st["alpha"], nu = pars_st["nu"])
plot_tail(F_st, "Skew t", "blue")

F_ghyp <- pghyp(filtered_resid_sort, object = ghyp)
plot_tail(F_ghyp, "Generalized Hyperbolic", "darkmagenta")

F_hyp <- pghyp(filtered_resid_sort, object = hyp)
plot_tail(F_hyp, "Hyperbolic", "cyan")

F_nig <- pghyp(filtered_resid_sort, object = nig)
plot_tail(F_nig, "Normal Inverse Gaussian", "darkred")

F_vg <- pghyp(filtered_resid_sort, object = vg)
plot_tail(F_vg, "Variance Gamma", "darkblue")

#risk analysis

plot(fit, which = 10)  # standardized residuals
plot(fit, which = 11)  # QQ plot

sigma_values <- sigma(fit)
plot(sigma_values, type = "l", col = "blue", main = "Conditional Volatility (σt)") #confirm that volatility clustering is captured well.

# compute 1% and 5% VaR using your fitted GARCH model:
VaR_1 <- quantile(filtered_resid, probs = 0.01)
VaR_5 <- quantile(filtered_resid, probs = 0.05)
VaR_1; VaR_5