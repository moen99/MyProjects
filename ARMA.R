#' -----
#' title: GRA6547 Research Methodology in Finance
#' subtitle: Assignment 2
#' date: '30-03-2023'
#' output: pdf_document
#' -----

##################################################

# Preparatory steps
rm(list=ls())

library(readxl)
library(tinytex)
library(rmarkdown)
library(zoo)
library(stats)
library(base)
library(tseries)
library(latex2exp)
library(lmtest)
library(vars)
library(car)


setwd("~/Documents/Skole/Master/Second semester/Res. Meths. in Finance/Data")

# Importing and adjusting the data
data <- read_xls("CPIAUCSL.xls", skip = 10)
str(data) 
colnames(data) <- c("date", "CPI") 
data$date <- as.Date(data$date) 

###############################################################################
###############                    QUESTION 1                 #################
###############################################################################

## (a)
# Calcule the log inflation rate
data$CPI_lagged <- c(NA, data$CPI[-length(data$CPI)]) #creates a new column with
                                                      #the CPI values shifted one row down
data$CPI_log <- log(data$CPI/data$CPI_lagged) #adds a column with the log inflation rate
data <- data[-1,] #removes the first row since we only have an NA value for the log inflation rate

# Create subsample with in-sample data
N = which(data$date == "2012-12-01") #to see which row the last date is on 
insample <- data[1:N,]
tail(insample) #check if we did it right


# Plot of ACF and PACF
par(mfrow  = c(1,2),
    pty    = "s",
    las    = 0,
    mgp    = c(2.5, 1, 0),
    mar    = c(3, 4, 0, 3),
    family ="serif")

acf(insample$CPI_log,  12, ylim=c(-0.2, 0.6))
grid()

pacf(insample$CPI_log, 12, ylim=c(-0.2, 0.6))
grid()


## (b)
SBIC <- matrix(NA, nrow = 6, ncol = 6) # matrix to store the SBIC
rownames(SBIC) <- c("AR(0)", "AR(1)", "AR(2)", "AR(3)", "AR(4)", "AR(5)")
colnames(SBIC) <- c("MA(0)", "MA(1)", "MA(2)", "MA(3)", "MA(4)", "MA(5)")
for (p in 1:6){
  for (q in 1:6){
    arima_pq <- arima(insample$CPI_log, order = c(p-1, 0, q-1), 
                      include.mean=FALSE, method="ML") #ARMA model for p,q up to 6
    SBIC[p,q] <- BIC(arima_pq) #calculating the IC for each ARMA model
  }
}

# Identifying the ARMA model with the lowest IC
which( SBIC == min(SBIC), arr.ind = TRUE ) 
rownames(SBIC)[2]
colnames(SBIC)[3] 
#AR(1) MA(2) model

# ARMA(1,2)
arma <- arima(insample$CPI_log, order = c(1, 0, 2), include.mean=FALSE, method="ML")
summary(arma)
print(arma)

##Calculating t-stats of the optimal ARMA model
# Compute the estimated degrees of freedom
df <- N - sum(arma$arma[c(1, 3)])

# Compute the t-values for each coefficient
t_values <- coef(arma) / sqrt(diag(vcov(arma))) / sqrt(df)
names(t_values) <- names(coef(arma))

# Print the t-values
t_values
coeftest(arma) #to determine their significance


## (c) - Individual autocorrelation tests
# Obtain the residuals from our ARMA(1,2) model
u <- residuals(arma)

# Compute the sample ACF of the residuals and the CI
acf_u <- acf(u, lag.max = 12, plot = FALSE)
ci <- 1.96 * 1 / sqrt(N)

# Print the sample ACF values and the CI
print(acf_u$acf)
ci


## (d)
outsample <- data[N:nrow(data),] #storing out-of-sample data
K <- length(outsample$CPI_log) #length out-of-sample data

forecast <- numeric(K) 
uhat.test <- numeric(K)

for (i in 1:K){
  parameters   <- arima(data$CPI_log[1:(N+i-1)], order = c(1, 0, 2), include.mean=FALSE, method="ML")
  uhat         <- parameters$residuals[(1+max(1,2)):(N+i)] 
  uhat.test[i] <- parameters$coef[2]*uhat[(N+i-3)] + parameters$coef[3]*uhat[(N+i-4)] 
  #note that -3, -4 is because we have no forecasting from the 
  #in sample first two periods and consequently no residuals
  forecast[i]  <- parameters$coef[1] * data$CPI_log[(N+i-1)] + uhat.test[i]
}  

# Calculating the mean squared error
mse <- mean((outsample$CPI_log - forecast)^2)
mse
mean(outsample$CPI_log)

# Plot and compare the forecasting with the actual out-of-sample data
plot(outsample$date, forecast,
     type = "l", 
     col = "green", 
     ylab = "Log Inflation Rate", 
     xlab = "Year", 
     ylim = c(-0.01, 0.015))
lines(outsample$date, outsample$CPI_log, col = "blue")
grid()
legend("topleft", 
       legend=c('Forecast','Actual'),
       col=c('green','blue'), 
       lty=1, 
       cex=0.8)





###############################################################################
###############                    QUESTION 2                 #################
###############################################################################


#Importing data and merging the data sets for the respective periods of interest

income_dta      = read_xls("DPIC96.xls", range = "A168:B312",
                           col_names = c("Observation date",
                                         "RDPI"))
consumption_dta = read_xls("PCECC96.xls", range = "A168:B312", 
                           col_names = c("Observation date", 
                                         "RPCE"))

data = merge(income_dta, consumption_dta)

################################################################################
#                            ~ Question 2(a) ~                                 #
################################################################################

# Defining our y_t's
y_1t = log(data$RDPI)
y_2t = log(data$RPCE)

delta_yt = matrix(nrow = 144, ncol = 2) # Creating matrix for new values

for (i in seq_along(y_1t[-1])) {
  delta_yt[i,1] = y_1t[i+1] - y_1t[i]
} # For loop to put in the log differences

for (i in seq_along(y_2t[-1])) {
  delta_yt[i,2] = y_2t[i+1] - y_2t[i]
} # For loop to put in the log differences

# Finalising the data frame
delta_yt = as.data.frame(delta_yt)
colnames(delta_yt) = c("Delta_y1t", "Delta_y2t")

delta_y1t = delta_yt$Delta_y1t
delta_y2t = delta_yt$Delta_y2t



################################################################################
#                            ~ Question 2(b) ~                                 #
################################################################################

# First use Schwarz-Bayesian information criterion to determine optimal lag-
#length

opt_lag = VARselect(delta_yt, lag.max = 10)
print(opt_lag)

paste0("SBIC: VAR(", opt_lag$selection[3], ")")

# Suggested optimal lag based on the Schwartz-Bayesian information criterion is
# three lags. Suggesting the past three quarters of the time series is optimal 
# in explaining the effects on disposable income and expenditures.

model2 = VAR(delta_yt, p = opt_lag$selection[3]) # Running our VAR model
summary(model2) # Summary of outputs




################################################################################
#                            ~ Question 2(c) ~                                 #
################################################################################

# Granger causality tests. First create a data set with only lagged values for 
# regression
T = nrow(delta_yt) 
GC = data.frame( delta_yt$Delta_y1t, delta_yt$Delta_y2t,
                 c(NA, delta_yt$Delta_y1t[1:(T-1)]), 
                 c(NA, delta_yt$Delta_y2t[1:(T-1)]),
                 c(NA, NA, delta_yt$Delta_y1t[1:(T-2)]),
                 c(NA, NA, delta_yt$Delta_y2t[1:(T-2)]),
                 c(NA, NA, NA, delta_yt$Delta_y1t[1:(T-3)]),
                 c(NA, NA, NA, delta_yt$Delta_y2t[1:(T-3)]) )

colnames(GC) =  c( "delta_y1t", "delta_y2t", 
                   "delta_y1t_min1", "delta_y2t_min1", 
                   "delta_y1t_min2", "delta_y2t_min2",
                   "delta_y1t_min3", "delta_y2t_min3")

GC = GC[-1:-3,] # Remove rows with NA's
head(GC)
# Regressing the lags on our delta RPDI
reg_delta_y1t = lm(delta_y1t ~ delta_y1t_min1 + delta_y2t_min1 + delta_y1t_min2 +
                     delta_y2t_min2 + delta_y1t_min3 + delta_y2t_min3, data = GC)

# Regressing the lags on our delta RPCE
reg_delta_y2t = lm(delta_y2t ~ delta_y1t_min1 + delta_y2t_min1 + delta_y1t_min2 +
                     delta_y2t_min2 + delta_y1t_min3 + delta_y2t_min3, data = GC)

# H0: Lags of delta y1t do not Granger-cause current difference in RPCE

res = linearHypothesis(reg_delta_y2t, c("delta_y1t_min1 = 0", "delta_y1t_min2 = 0", 
                                        "delta_y1t_min3 = 0"))
paste0("Restrict beta, gamma and delta lags of difference in RPDI to zero: p-value = ",
       round(res$`Pr(>F)`[2], 4))
print("We can reject H0. Lagged values of difference in RPDI jointly Granger-causes current RPCE")

res = linearHypothesis(reg_delta_y2t, c("delta_y1t_min1 = 0"))
paste0("Restrict first lagged value of Delta RPDI to zero: p-value = ",
       round(res$`Pr(>F)`[2], 4))
print("We can reject H0. First lagged value Granger-causes current RPCE")

res = linearHypothesis(reg_delta_y2t, c("delta_y1t_min2 = 0"))
paste0("Restrict second lagged value of Delta RPDI to zero: p-value = ",
       round(res$`Pr(>F)`[2], 4))
print("We can reject H0. Second lagged value Granger-causes current RPCE")

res = linearHypothesis(reg_delta_y2t, c("delta_y1t_min3 = 0"))
paste0("Restrict third lagged value of Delta RPDI to zero: p-value = ",
       round(res$`Pr(>F)`[2], 4))
print("We cannot reject H0. Third lagged value do not Granger-causes current RPCE")

# H0: Lags of delta y2t do not Granger-cause current difference in RPDI

res = linearHypothesis(reg_delta_y1t, c("delta_y2t_min1 = 0", "delta_y2t_min2 = 0",
                                        "delta_y2t_min3 = 0"))
paste0("Restrict beta, gamma and delta lags of difference in RPCE to zero: p-value = ",
       round(res$`Pr(>F)`[2], 4))
print("We can reject H0. Lags of difference in RPCE jointly Granger-causes current RPDI")

res = linearHypothesis(reg_delta_y1t, c("delta_y2t_min1 = 0"))
paste0("Restrict beta, gamma and delta lags of difference in RPCE to zero: p-value = ",
       round(res$`Pr(>F)`[2], 4))
print("We cannot reject H0. First lag of difference in RPCE do not Granger-causes current RPDI")

res = linearHypothesis(reg_delta_y1t, c("delta_y2t_min2 = 0"))
paste0("Restrict beta, gamma and delta lags of difference in RPCE to zero: p-value = ",
       round(res$`Pr(>F)`[2], 4))
print("We can reject H0. Second lag of difference in RPCE Granger-causes current RPDI")

res = linearHypothesis(reg_delta_y1t, c("delta_y2t_min3 = 0"))
paste0("Restrict beta, gamma and delta lags of difference in RPCE to zero: p-value = ",
       round(res$`Pr(>F)`[2], 4))
print("We can reject H0. Third lag of difference in RPCE Granger-causes current RPDI")

# We observe that the same lags who had significant coefficients also Granger-
# cause the other variable. 

################################################################################
#                            ~ Question 2(d) ~                                 #
################################################################################

# Conducting the impulse response tests
tau = 20
imp_res = irf( model2, n.ahead = tau, ortho = FALSE)

# Shock impulse response results
round( imp_res$irf$Delta_y1t, 4 )

# Plotting the results from above analysis to compare how the shocks in one 
# variable impact the other variable over a period of 20 quarters.

par(mfrow  =  c(2,2),
    pty    = "s",
    las    = 0,
    mgp    = c(2.5, 1,0),
    mar    = c(2, 0, 2, 0),
    family = "serif")
plot(0:tau, imp_res$irf$Delta_y1t[,2], type = "l", col = "red",
     xlab = "", ylab = "", main = "Response of shock in Delta RPDI on Delta RPCE",
     xlim = c(0, tau), ylim = c(-1,1), 
     xaxp = c(0, tau, 2), yaxp = c(0,1,2))

plot(0:tau, imp_res$irf$Delta_y2t[,1], type = "l", col = "red",
     xlab = "", ylab = "", main = "Response of shock in Delta RPCE on Delta RPDI",
     xlim = c(0, tau), ylim = c(-1, 1), 
     xaxp = c(0, tau, 2), yaxp = c(-1, 1, 2))

# With orthogonalisation
imp_res = irf( model2, n.ahead = tau, ortho = TRUE)

# Shock impulse response results with orthogonalisation
round( imp_res$irf$Delta_y1t, 4 )

# Plotting results
par(mfrow  =  c(2,2),
    pty    = "s",
    las    = 0,
    mgp    = c(2.5, 1,0),
    mar    = c(2, 0, 2, 0),
    family = "serif")
plot(0:tau, imp_res$irf$Delta_y1t[,2], type = "l", col = "red",
     xlab = "", ylab = "", main = "Response of shock in Delta RPDI on Delta RPCE",
     xlim = c(0, tau), ylim = c(0,1), 
     xaxp = c(0, tau, 2), yaxp = c(0,0.5,2))

plot(0:tau, imp_res$irf$Delta_y2t[,1], type = "l", col = "red",
     xlab = "", ylab = "", main = "Response of shock in Delta RPCE on Delta RPDI",
     xlim = c(0, tau), ylim = c(-0.1, 0.1), 
     xaxp = c(0, tau, 2), yaxp = c(-0.05, 0.05, 2))

# From the plots we can see that a shock in consumption expenditure has greater 
# impact on the disposable income than vice-versa during the analysed period.
# After about 10 quarters the impact of change in consumption expenditure
# starts to converge back to zero. 


################################################################################
#                            ~ Question 2(e) ~                                 #
################################################################################

# Using variance decomposition to decompose the forecast error variance of our 
# time series.

var_dec = fevd(model2, n.ahead = tau)
round(matrix(c( var_dec$Delta_y1t[,1],
                var_dec$Delta_y1t[,2],
                var_dec$Delta_y1t[,1] + var_dec$Delta_y1t[,2]),
             ncol = 3),
      4)

# Plotting our results

par(mfrow = c(2,2),
    pty = "s",
    las = 0,
    mgp = c(2.5, 1, 0),
    mar = c(2, 0, 2, 0),
    family ="serif")
plot(1:tau, var_dec$Delta_y1t[,2], type = "l", col = "red",
     xlab = "", ylab = "", main = "% of Disp. Income variance due Consumption 
     Exp. ",
     xlim = c(0, tau), ylim = c(0, 1),
     xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
plot(1:tau, var_dec$Delta_y2t[,1], type = "l", col = "red",
     xlab = "", ylab = "", main = "% of Consumtion Exp. variance due to Disp. 
     Income",
     xlim = c(0, tau), ylim = c(0, 1),
     xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))













