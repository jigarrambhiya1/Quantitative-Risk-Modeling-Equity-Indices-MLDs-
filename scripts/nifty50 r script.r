library(readxl)
nifty<- read_excel("RESEARCH TREATISE/nifty rt sem1 dataset.xlsx")
#View(nifty)


head(nifty)
returns=diff(log(nifty$Close))  # log returns
b=nifty[-1,]


nifty=cbind(b,returns)

plot(nifty$Close~nifty$Date,type="l",lwd=2,xlab="TIME INTERVAL",ylab="DAILY PRICES",col="red")
plot(nifty$returns~nifty$Date,type="l",lwd=2,xlab="TIME INTERVAL",ylab="DAILY LOG PRICES",
     col="purple")
hist(nifty$returns,col="darkblue",xlab="LOG RETURNS",ylab="FREQUENCY",main="HISTOGRAM")

hist(nifty$returns,
     breaks = 50,             # number of bins
     col = "darkblue",
     main = "HISTOGRAM",
     xlab = "LOG RETURNS",
     ylab="FREQUENCY",
     ylim = c(0, 900),        # y-axis limit from 0 to 150
     yaxt = "n")              # suppress default axis

axis(2, at = seq(0, 900, by = 50))   # add custom y-axis ticks


library(ggplot2)

ggplot(nifty, aes(x = Date, y = Close)) +
  geom_line(color = "red") +
  labs(x = "TIME INTERVAL", y = "DAILY PRICES") +
  theme_minimal()

library(ggplot2)
library(scales)


#ACF PLOTS
par(mfrow=c(1,2))
obj=acf(nifty$returns,plot=FALSE)   
plot(obj, main = "ACF of Returns-NIFTY50",col="red",ylab="sample autocorrelation")
points(obj$lag,obj$acf,pch=16,col="red")


obj1=acf(nifty$returns^2,plot=FALSE);obj1
plot(obj1, main ="ACF of Squared Returns-NIFTY50",col="red",ylab="sample autocorrelation")
points(obj1$lag,obj1$acf,pch=16,col="red")


#install.packages("moments")
#install.packages("e1071")
library(moments)
library(e1071)

#no.of observations
obs=nrow(nifty);obs
mean(nifty$returns)
sd(nifty$returns)
#Skewness
skewness(nifty$returns)
#kurtosis
kurtosis(nifty$returns)

#shapiro wilk
shapiro.test((nifty$returns))

#jarque bera
#install.packages("tseries")
library(tseries)
jarque.bera.test(nifty$returns)

library(rugarch)
library(PerformanceAnalytics)


spec_egarch <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(1,1), include.mean = TRUE),
  distribution.model = "sstd"   # skewed Student-t innovations
)

fit_egarch <- ugarchfit(spec_egarch, data = nifty$returns)
show(fit_egarch)


f1=residuals(fit_egarch, standardize = TRUE)

library(xts)
f= xts(f1, order.by = as.Date(nifty$Date))

length(f)
f=as.numeric(f)
#ACF PLOTS
par(mfrow=c(1,2))
obj=acf(f,plot=FALSE)   
plot(obj, main = "ACF of Returns-NIFTY 50",col="red",ylab="sample autocorrelation")
points(obj$lag,obj$acf,pch=16,col="red")


obj1=acf(na.omit((f)^2),plot=FALSE);obj1
plot(obj1, main ="ACF of Squared Returns-NIFTY 50",col="red",ylab="sample autocorrelation")
points(obj1$lag,obj1$acf,pch=16,col="red")



#to find MLE:
#1.NORMAL DISTRIBUTION
library(MASS)
fit_norm <- fitdistr(f, "normal")
fit_norm
summary(fit_norm)
k1=length(coef(fit_norm));k1
fit_norm$estimate
fit_norm$sd

LL1=fit_norm$loglik;LL1

#aic and bic
a1=AIC(fit_norm);a1
b1=BIC(fit_norm);b1

#ks test
mu_hat = fit_norm$estimate["mean"]
sd_hat = fit_norm$estimate["sd"]

ks_norm = ks.test(f, "pnorm", mean = mu_hat, sd = sd_hat)
ks_norm

# Extract statistic and p-value
s1=as.numeric(ks_norm$statistic);s1
p1=ks_norm$p.value;p1

#install.packages("nortest")
library(nortest)

#A2_norm <- anderson_darling_stat(f, pnorm, params)
#A2_norm
#ad_norm <- ad.test(f, null = pnorm, mean = mu_hat, sd = sd_hat)
#ad_norm
#A2_norm <- ad_norm$statistic; A2_norm
#pA2_norm <- ad_norm$p.value; pA2_norm


#
#2.LAPLACE DISTRIBUTION
#install.packages("VGAM")
library(VGAM)
fit_laplace <- vglm(f ~ 1, laplace, trace = TRUE)
fit_laplace
summary(fit_laplace)
coef(fit_laplace)
k2=length(coef(fit_laplace));k2
LL2=as.numeric(logLik(fit_laplace));LL2
a2=AIC(fit_laplace);a2
b2=BIC(fit_laplace);b2

# Extract parameters
pars = coef(fit_laplace)
mu    = pars["(Intercept):1"]  ;mu       # location parameter
sigma = exp(pars["(Intercept):2"]) ;sigma # scale is stored as log(scale)

# KS test
ks_laplace = ks.test(f, "plaplace", location = mu, scale = sigma)
ks_laplace

# Extract statistic and p-value
s2=as.numeric(ks_laplace$statistic);s2
p2=ks_laplace$p.value;p2


#3.CAUCHY DISTRIBUTION
library(MASS)
fit_cauchy <- fitdistr(f, "cauchy")
fit_cauchy
summary(fit_cauchy)
k3=length(coef(fit_cauchy));k3
LL3=fit_cauchy$loglik;LL3
a3=AIC(fit_cauchy);a3
b3=BIC(fit_cauchy);b3

# Extract parameters
pars_c = coef(fit_cauchy)
location = pars_c["location"]
scale    = pars_c["scale"]

# KS test
ks_cauchy = ks.test(f, "pcauchy", location = location, scale = scale)
ks_cauchy

# Extract statistic and p-value
s3=as.numeric(ks_cauchy$statistic);s3
p3=ks_cauchy$p.value;p3


#4.t DISTRIBUTION
library(MASS)
fit_t <- fitdistr(f, "t")
fit_t
summary(fit_t)
k4=length(coef(fit_t));k4

LL4=fit_t$loglik;LL4

a4=AIC(fit_t);a4   #
b4=BIC(fit_t);b4

# Extract fitted parameters
pars_t = coef(fit_t)
mu  = pars_t["m"]       # location
sigma = pars_t["s"]     # scale
nu  = pars_t["df"]      # degrees of freedom

# KS test: compare data with fitted t
ks_t <- ks.test(f, function(x) pt((x - mu)/sigma, df = nu))
ks_t

# Extract statistic and p-value
s4=as.numeric(ks_t$statistic);s4
p4=ks_t$p.value;p4

#OR

#library(fGarch)
#fit_st <- stdFit(f)   # f is your data vector
#fit_st
#fit_t$loglik
#a41=AIC(fit_st);a41
#b41=BIC(fit_st);b41

#5.SKEW NORMAL
#install.packages("sn")
library(sn)
fit_sn <- selm(f ~ 1, family = "SN")
fit_sn
summary(fit_sn)
LL5=as.numeric(logLik(fit_sn));LL5
k5=length(coef(fit_sn)) ;k5
a5=AIC(fit_sn);a5
b5=log(obs)*k5 - 2*LL5;b5   #solve

pars_sn = coef(fit_sn, param.type = "DP")
mu    = pars_sn["xi"]
sigma = pars_sn["omega"]
alpha = pars_sn["alpha"]

# KS test
ks_sn <- ks.test(f, "psn", xi = mu, omega = sigma, alpha = alpha)
ks_sn
# Results
s5=as.numeric(ks_sn$statistic);s5   # KS D statistic
p5=ks_sn$p.value;p5     # p-value


#6.SKEW t
# 1 Load required package
library(sn)


# 2 Ensure data vector is numeric
#f <- as.numeric(std_resid)   # or use as.numeric(nifty$returns)

# 3 Fit the Skew-t model (Direct Parameterization)
fit_st <- selm(f ~ 1, family = "ST", param.type = "DP")

# 4 View what’s stored inside the object
#slotNames(fit_st)


# Extract all fitted parameters (location, scale, shape, df)
fit_st@param$dp

# 7 Save them to a variable
params_st <- fit_st@param$dp
params_st

# 8️ Count number of parameters (should be 4)
k6 <- length(params_st)
k6

# 9️ Extract log-likelihood
LL6 <- as.numeric(logLik(fit_st))
LL6

# 10Extract AIC and BIC
a6=AIC(fit_st);a6
   # manually provide number of parameters
b6=log(obs)*k6 - 2*LL6;b6



pars_st =coef(fit_st, param.type = "DP")
mu    = pars_st["xi"];mu
sigma = pars_st["omega"];sigma
alpha = pars_st["alpha"];alpha
nu    = pars_st["nu"];nu

# KS test: data vs fitted skew-t
ks_st <- ks.test(f, "pst", xi = mu, omega = sigma, alpha = alpha, nu = nu)
ks_st

s6=as.numeric(ks_st$statistic);s6  # D statistic
p6=ks_st$p.value;p6     # p-value



#SKEW CAUCHY
library(sn)
fit_sc <- selm(f ~ 1, family = "SC", param.type = "DP")
summary(fit_sc, param.type = "DP")

k7=length(coef(fit_sc, param.type = "DP")) 
k7
LL7=fit_sc@logL;LL7

a7=AIC(fit_sc);a7
b7=log(obs)*k7 - 2*LL7;b7  #solve

pars = coef(fit_sc, param.type = "DP")
mu  =pars["xi"];mu
sigma = pars["omega"];sigma
alpha = pars["alpha"];alpha
# KS test for skew Cauchy fit
ks_sc <- ks.test(f, "psc", xi = mu, omega = sigma, alpha = alpha)
ks_sc

s7=as.numeric(ks_sc$statistic);s7   # D statistic
p7=ks_sc$p.value;p7     # p-value

#8. Skew Laplace distribution
#install.packages("ald")
library(ald)   
#library(ald); ls("package:ald")
fit_sl <- mleALD(f) 
fit_sl     # intercept-only model
summary(fit_sl)

k8=length(fit_sl$par);k8

LL8=likALD(f,fit_sl$par[1],fit_sl$par[2],fit_sl$par[3],loglik=TRUE);LL8
a8=2*k8 - 2*LL8;a8
b8=log(obs)*k8 - 2*LL8;b8

#ks test
# KS test: empirical vs fitted ALD
mu=fit_sl$par[1]   # location
sigma=fit_sl$par[2] # scale
p=fit_sl$par[3]     # skewness (quantile parameter in ALD)

ks_ald = ks.test(f, "pALD", mu, sigma, p)
ks_ald

s8=as.numeric(ks_ald$statistic);s8  # D statistic
p8=ks_ald$p.value;p8    # p-value


#9.hyperbolic distribution
#install.packages("ghyp")
library(ghyp)
fit_hyp <- fit.hypuv(f)
summary(fit_hyp)

LL9=logLik(fit_hyp);LL9
k9= sum(fit_hyp@fitted.params)
k9
#AIC & BIC
a9=AIC(fit_hyp);a9
b9=log(obs)*k9 - 2*LL9;b9  #solve
#KS TEST
ks_hyp=ks.test(f, function(x) pghyp(x, object = fit_hyp))
ks_hyp
s9=as.numeric(ks_hyp$statistic);s9  # KS D-statistic
p9=ks_hyp$p.value ;p9


# 10. Normal Inverse Gaussian (NIG)
library(ghyp)
fit_nig=fit.NIGuv(f)
summary(fit_nig)

LL10=logLik(fit_nig);LL10
k10=sum(fit_nig@fitted.params)
k10

a10=AIC(fit_nig);a10
b10=log(obs)*k10 - 2*LL10;b10 #solve

# KS test: data vs fitted NIG distribution
ks_nig=ks.test(f, function(x) pghyp(x, object = fit_nig))
ks_nig
s10=as.numeric(ks_nig$statistic);s10  # KS D-statistic
p10=ks_nig$p.value ;p10 

#11. Variance Gamma (VG)
library(ghyp)
fit_vg=fit.VGuv(f)
a=summary(fit_vg) ;a  # correct number of free parameters
LL11=logLik(fit_vg);LL11

k11=sum(fit_vg@fitted.params)
k11

a11=AIC(fit_vg);a11
b11=log(obs)*k11 - 2*LL11;b11  #solve

# KS test: data vs fitted VG distribution
ks_vg <- ks.test(f, function(x) pghyp(x, object = fit_vg))
ks_vg
s11=as.numeric(ks_vg$statistic);s11  # KS D-statistic
p11=ks_vg$p.value ;p11   # p-value


#12. Generalized Hyperbolic (GHYP)
library(ghyp)
fit_ghyp=fit.ghypuv(f)
summary(fit_ghyp)
LL12=logLik(fit_ghyp);LL12
k12=length(coef(fit_ghyp));k12

a12=AIC(fit_ghyp);a12
b12=log(length(f))*k12 - 2*LL12;b12  #solve


out12 <- ks.test(f, function(x) pghyp(x, object = fit_ghyp));out12

# Extract KS statistic
s12=as.numeric(out12$statistic);s12

# Extract p-value
p12=out12$p.value;p12

Distributions=c("Normal","Laplace","Cauchy","t",
                "Skew Normal","Skew t","Skew Cauchy","Skew Laplace",
                "hyperbolic distribution","Normal Inverse Gaussian",
                "Variance Gamma","Generalized Hyperbolic")
aic=c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12);aic
bic=c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12);bic
k=c(k1,k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12);k
log_likelihood=c(LL1,LL2,LL3,LL4,LL5,LL6,LL7,LL8,LL9,LL10,LL11,LL12);log_likelihood
ks_stat=c(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12);ks_stat
ks_p=c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12);ks_p
result=data.frame(Distributions,k,log_likelihood,aic,bic,ks_stat,ks_p);result
length(k)
length(ks_stat)
length(ks_p)
length(k)
length(aic)

result$rank_aic <- rank(result$aic, ties.method = "min")
result$rank_ks  <- rank(result$ks_stat, ties.method = "min")
result$total_rank <- result$rank_aic + result$rank_ks

best_model <- result[which.min(result$total_rank), ]
best_model

# Normal,laplace,cauchy,skew cauchy,skew laplace not a good fit
#so carrying graphical test only for remaining

# Example: Graphical Tail Test for fitted distribution
# f = your data vector
# Assume you already estimated parameters (e.g., for t-distribution)
N <- length(f)
f_sorted <- sort(f)

# Compute fitted CDF (replace with your chosen distribution)
Fhat <- pt((f_sorted - mu)/sigma, df = nu)  # example: fitted t-distribution

# Empirical tail (left side)
empirical_log <- log(1:(N) / (N + 1))
fitted_log <- log(Fhat)



# Sort data and empirical CDF
f_sorted <- sort(f)
n <- length(f)
empirical_log <- log(1:n / (n + 1))



# Sort data
f_sorted <- sort(f)
n <- length(f)
empirical_log <- log(1:n / (n + 1))  # empirical log cumulative probabilities

# Theoretical fitted CDFs for each model
F_t     <- pt((f_sorted - coef(fit_t)["m"]) / coef(fit_t)["s"], df = coef(fit_t)["df"])
F_st    <- pst(f_sorted, xi = coef(fit_st, param.type = "DP")["xi"],
               omega = coef(fit_st, param.type = "DP")["omega"],
               alpha = coef(fit_st, param.type = "DP")["alpha"],
               nu = coef(fit_st, param.type = "DP")["nu"])
F_hyp   <- pghyp(f_sorted, object = fit_hyp)
F_nig   <- pghyp(f_sorted, object = fit_nig)
F_vg    <- pghyp(f_sorted, object = fit_vg)
F_ghyp  <- pghyp(f_sorted, object = fit_ghyp)

# Combine all fitted CDFs into a list
F_list <- list(
  "t" = F_t,
  "Skew t" = F_st,
  "Hyperbolic" = F_hyp,
  "NIG" = F_nig,
  "Variance Gamma" = F_vg,
  "Generalized Hyperbolic" = F_ghyp
)

# Colors and line styles for clarity

cols <- c("blue","darkgreen","cyan","darkred","darkblue",
          "darkmagenta")
ltys <- 1:7
par(mar = c(5, 5, 4, 7))  # add space on the right

# PLOT
plot(f_sorted, empirical_log, pch = 16, col = "black",
     main = "Graphical Tail Test (Left Tail)",
     xlab = "X(t)", ylab = "log(F(x))")

# Add all fitted distributions
for (i in 1:length(F_list)) {
  lines(f_sorted, log(F_list[[i]]), col = cols[i], lwd = 2, lty = ltys[i])
}

# Add legend
#legend("bottomright", legend = names(F_list), col = cols, lty = ltys,
 #      lwd = 2, cex = 0.7, ncol = 2, bg = "white")

legend("topright",
       inset = c(-0.25, 0),   # moves it outside
       legend = c("NIG", "Variance Gamma", "Generalized Hyperbolic"),
       col = c("brown", "blue", "purple"),
       lty = c(2, 3, 4),
       cex = 0.8,
       xpd = TRUE,            # allows drawing outside plot
       box.lty = 0)           # optional: remove border box

# Sort data and empirical CDF
f_sorted <- sort(f)
n <- length(f)
empirical_log <- log(1:n / (n + 1))


# Helper plotting function
plot_tail <- function(F_fit, dist_name, color="blue") {
  plot(f_sorted, empirical_log, pch = 16, col = "black",
       main = paste("Graphical Tail Test -", dist_name),
       xlab = "X(t)", ylab = "log(F(x))")
  lines(f_sorted, log(F_fit), col = color, lwd = 2)
}

par(mfrow=c(2,2))      
#?plot
#?legend
#top 2
### 6. Skew t
pars_st <- coef(fit_st, param.type = "DP")
F_st <- pst(f_sorted, xi = pars_st["xi"], omega = pars_st["omega"],
            alpha = pars_st["alpha"], nu = pars_st["nu"])
plot_tail(F_st, "Skew t", "darkgreen")

### 12. Generalized Hyperbolic (GHYP)
F_ghyp <- pghyp(f_sorted, object = fit_ghyp)
plot_tail(F_ghyp, "Generalized Hyperbolic", "darkmagenta")

#Others
### 4. Student’s t
F_t <- pt((f_sorted - coef(fit_t)["m"]) / coef(fit_t)["s"], df = coef(fit_t)["df"])
plot_tail(F_t, "Student's t", "blue")

### 9. Hyperbolic
F_hyp <- pghyp(f_sorted, object = fit_hyp)
plot_tail(F_hyp, "Hyperbolic", "cyan")

### 10. Normal Inverse Gaussian (NIG)
F_nig <- pghyp(f_sorted, object = fit_nig)
plot_tail(F_nig, "Normal Inverse Gaussian", "darkred")

### 11. Variance Gamma
F_vg <- pghyp(f_sorted, object = fit_vg)
plot_tail(F_vg, "Variance Gamma", "darkblue")



#risk analysis

alpha <- 0.01  # 99% VaR level
q_ghyp <- qghyp(alpha, object = fit_ghyp)
mu_t <- fitted(fit_egarch)      # conditional mean from EGARCH
sigma_t <- sigma(fit_egarch)    # conditional SD from EGARCH

VaR_99 <- mu_t + sigma_t * q_ghyp;VaR_99
violations <- ifelse(nifty$returns < VaR_99, 1, 0)
sum(violations)  # number of violations

length(violations)  # total observations
#install.packages("VaRTest")
#library(VaRTest)
library(PerformanceAnalytics)
alpha <- 0.01
kupiec <- VaRTest(alpha, actual = nifty$returns, VaR = VaR_99)
print(kupiec)



loss=-nifty$returns
alpha <- 0.01
VaR_emp99 <- quantile(loss, 1-alpha)
CTE_emp99 <- mean(loss[loss> VaR_emp99])
VaR_emp99
CTE_emp99


alpha=0.05
VaR_emp95 <- quantile(loss, 1-alpha)
CTE_emp95 <- mean(loss[loss > VaR_emp95], na.rm = TRUE)
VaR_emp95
CTE_emp95


# Define principal and participation rate
principal <- 100
p_rate <- 0.8  # 80% participation in upside


payoff <- principal * (1 + pmax(0, p_rate * nifty$returns))

# Step 2: Convert to percentage returns relative to principal
mld_returns <- (payoff - principal) / principal


# Step 3: Define losses (for risk measures, loss = -return)
losses <- -mld_returns;losses

# Step 4: Compute VaR and CTE at 95% and 99% levels
VaR_95p<- quantile(losses, 0.95);VaR_95p
VaR_99p <- quantile(losses, 0.99);VaR_99p
VaR_95p <- quantile(mld_returns, 0.05);VaR_95p
CTE_95p <- mean(losses[losses >=VaR_95p]);CTE_95p 
CTE_99p <- mean(losses[losses >= VaR_99p]);CTE_99p


# Step 3: Define MLD parameters
principal <- 100
p_rate <- 0.8   # participation rate (80% of underlying movement)

# Step 4: Compute MLD payoffs and losses (no principal protection)
payoff <- principal * (1 + p_rate * returns)
losses <- (principal - payoff) / principal   # positive = loss, negative = gain

# Step 5: Compute historical VaR and CTE (non-parametric)
VaR_95n<- quantile(losses, 0.95, na.rm = TRUE);VaR_95n
CTE_95n <- mean(losses[losses > VaR_95n], na.rm = TRUE);CTE_95n

VaR_99n <- quantile(losses, 0.99, na.rm = TRUE);VaR_99n
CTE_99n <- mean(losses[losses > VaR_99n], na.rm = TRUE);CTE_99n




#data frame
df1=cbind(VaR_emp95,VaR_emp99,CTE_emp95,CTE_emp99);df1
rownames(df1)=c("DIRECT INVESTMENTS")
df1

df2=cbind(VaR_95p,VaR_99p,CTE_95p,CTE_99p);df2
rownames(df2)=c("INDIRECT INVESTMENTS-PRINCIPAL PROTECTED")
df2

df3=cbind(VaR_95n,VaR_99n,CTE_95n,CTE_99n);df3
rownames(df3)=c("INDIRECT INVESTMENTS-NON PRINCIPAL PROTECTED")
df3


df4=rbind(df1,df2,df3);df4
colnames(df4)=c("VaR 95%","VaR 99%","CTE 95%","CTE 99%")
df4*100


#Using Monte-Carlo Simulation
set.seed(123)
n_sim <- 100000
sim_returns <- rghyp(n_sim, object = fit_ghyp)

loss_direct <- -sim_returns;loss_direct

#1.
principal <- 100
p_rate <- 0.8

payoff_pp <- principal * (1 + pmax(0, p_rate * sim_returns))
loss_pp <- (principal - payoff_pp) / principal

#3
payoff_np <- principal * (1 + p_rate * sim_returns)
loss_np <- (principal - payoff_np) / principal

alpha <- c(0.95, 0.99)

getRisk <- function(losses, alpha = c(0.95, 0.99)) {
  VaR <- quantile(losses, alpha, na.rm = TRUE)
  CTE <- sapply(VaR, function(v) mean(losses[losses >= v], na.rm = TRUE))
  data.frame(VaR_95 = VaR[1], VaR_99 = VaR[2],
             CTE_95 = CTE[1], CTE_99 = CTE[2])
}

df_MC <- rbind(
  getRisk(loss_direct),
  getRisk(loss_pp),
  getRisk(loss_np)
)

rownames(df_MC) <- c(
  "DIRECT INVESTMENT",
  "PRINCIPAL PROTECTED",
  "NON-PRINCIPAL PROTECTED"
)

df_MC




