#Housekeeping
rm(list=ls())

#Packages
library(xts)
library(readxl)
library(dplyr)
library(DescTools)
library(patchwork)
library(ggplot2)

#Loading in the data
setwd("~/Documents/Skole/Master/Second semester/Financial Risk")

Returns = read_excel("DataPost - Returns.xlsx",
                        col_types = c("date", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric")) 

Starting.Values = read_excel("DataPost - StartingValues.xlsx") 

# Parameters
monthsperyear = 12
reserved = monthsperyear * 7 #Use last 7 years of data

test_months = 60 #Number of months we're testing for
nTrials = 10000 #Number of simulations

# Visualizing the data
plot(Returns$Date, Returns$Electricity, type = "l", 
     col = "red", xlab = "Time", ylab = "Log
     Return", main = "Commodity Returns")
lines(Returns$Date, Returns$NaturalGas, col = "green")
lines(Returns$Date, Returns$Coal, col = "brown")
lines(Returns$Date, Returns$Carbon, col = "blue")
legend("bottom", c("Electricity", "Natural Gas", "Coal", "Carbon"),
       lty = c(1, 1, 1), ncol = 1, 
       col = c("red", "green", "brown", "blue"))

# Training set
df_training = Returns[(nrow(Returns) - reserved - test_months+1):
                         (nrow(Returns)-test_months),]


#Assuming that electricity returns is implied by risk factors of the other
#commodities. Running a regression on this

reg1 = lm(Electricity ~ WindUtilization + FossilFuelUtilization + NaturalGas
          + Coal + Carbon, data=df_training)
beta_reg1 = list()
beta_reg1['WindUtilization'] = reg1$coefficients["WindUtilization"]
beta_reg1['FossilFuelUtilization'] = reg1$coefficients["FossilFuelUtilization"]
beta_reg1['NaturalGas'] = reg1$coefficients["NaturalGas"]
beta_reg1['Coal'] = reg1$coefficients["Coal"]
beta_reg1['Carbon'] = reg1$coefficients["Carbon"]


###############################################################################
#############     Simulating Returns - Bootstrapping w/Copula      ############
###############################################################################

# Sorting the returns
sorted_rets <- list()
for (i in c("WindUtilization", "FossilFuelUtilization", "NaturalGas", "Coal", "Carbon")){
  sorted_rets[[i]] = df_training[[i]][order(df_training[[i]])]
}

#Creating noise terms
NoiseTerms = list()
NoiseTerms[['WindUtilization']]       <- matrix(rnorm(test_months * nTrials), 
                                                nrow = test_months, ncol = nTrials) 
NoiseTerms[['FossilFuelUtilization']] <- matrix(rnorm(test_months * nTrials), 
                                                nrow = test_months, ncol = nTrials)
NoiseTerms[['NaturalGas']]            <- matrix(rnorm(test_months * nTrials), 
                                                nrow = test_months, ncol = nTrials)
NoiseTerms[['Coal']]                  <- matrix(rnorm(test_months * nTrials), 
                                                nrow = test_months, ncol = nTrials)
NoiseTerms[['Carbon']]                <- matrix(rnorm(test_months * nTrials), 
                                                nrow = test_months, ncol = nTrials)
NoiseTerms[['Electricity']]           <- matrix(rnorm(test_months * nTrials), 
                                                nrow = test_months, ncol = nTrials)
                                                
U <- list()
for (i in c("WindUtilization", "FossilFuelUtilization", "NaturalGas", "Coal", "Carbon")){
  U[[i]] <- pnorm(NoiseTerms[[i]])
}

#Multivariate approach and t-distribution
df          = 20
corr_matrix = cor(df_training[c("WindUtilization", "FossilFuelUtilization", 
                                "NaturalGas", "Coal", "Carbon")])
cor(df_training$Coal, df_training$NaturalGas)
cop_param   = sin(pi/2*corr_matrix)

Z_t = mvtnorm::rmvt(test_months * nTrials, sigma = cop_param, df = df)
  #Convert to test months x Trials x 5 matrix. Rows = days, columns = sims
  Copula_Z_t <- array(Z_t, dim = c(test_months, nTrials, 5))

Copula_U_t     = list()
idx            = list()
pred_ret_cop_t = list()

for (i in c("WindUtilization", "FossilFuelUtilization", "NaturalGas", "Coal", "Carbon")) {
  Copula_U_t[[i]]     <- pt(Copula_Z_t[, , i == c("WindUtilization", "FossilFuelUtilization", 
                                                  "NaturalGas", "Coal", "Carbon")], df = df)
  idx[[i]]            <- as.numeric(ceiling(length(sorted_rets[[i]]) * Copula_U_t[[i]]))
  pred_ret_cop_t[[i]] <- matrix(sorted_rets[[i]][idx[[i]]], nrow = nrow(U[[i]]))
}

# Looking at visuals to check our bootstrapping
# Plot Coal simulation to visualize distribution (as an example)
h1         <- hist(as.vector(pred_ret_cop_t[['Coal']]), breaks = 20, plot=FALSE)
h1$density <- h1$counts/sum(h1$counts)*100
h2         <- hist(df_training$Coal, breaks = 20, plot=FALSE)
h2$density <- h2$counts/sum(h2$counts)*100
plot(h2, freq=FALSE,col = "lightblue", 
     xlab = "Simulated Returns", 
     main = "Simulated (Boot) vs. Historic Returns (Coal)")
plot(h1, freq=FALSE,  col = rgb(0,1,0,0.5), add=TRUE)
legend("topright", c("Actual", "Predicted"), fill=c("lightblue", rgb(0,1,0,0.5)))

# Using scatter plot to see whether we managed to capture the correlation
plot_df           = data.frame(as.vector(pred_ret_cop_t[['Coal']]),
                               as.vector(pred_ret_cop_t[['NaturalGas']]))
colnames(plot_df) = c("Coal","NaturalGas")
p1 <- ggplot(plot_df[sample(nrow(plot_df),1000),], aes(x = Coal, y = NaturalGas)) + 
  ggtitle("Coal vs NaturalGas (Predicted - Boot)") + 
  # Set point color and size
  geom_point(color = "blue", size = 2) + 
  # Set axis labels
  labs(x = "Coal Return", y = "NaturalGas Return")
p2 <- ggplot(Returns, aes(x = Coal, y = NaturalGas)) + 
  ggtitle("Coal vs NaturalGas (Historic)") + 
  # Set point color and size
  geom_point(color = "blue", size = 2) + 
  # Set axis labels
  labs(x = "Coal Return", y = "NaturalGas Return")
p1 + p2 + plot_layout(ncol=1) + plot_annotation(title="Predicted (Boot) vs. Historic Returns (Coal vs. NaturalGas)")

cor(pred_ret_cop_t$NaturalGas[1:10000], pred_ret_cop_t$Coal[1:10000], method = "kendall")
cor(df_training$NaturalGas, df_training$Coal, method = "kendall")


###############################################################################
##################             Estimating prices             ##################
###############################################################################

vol.ele = sd(df_training$Electricity)

# Electricity
vol.ele = sd(df_training$Electricity)
electricity.returns = (beta_reg1$WindUtilization*pred_ret_cop_t[['WindUtilization']] +
                         beta_reg1$FossilFuelUtilization*pred_ret_cop_t[['FossilFuelUtilization']] +
                         beta_reg1$NaturalGas*pred_ret_cop_t[['NaturalGas']] +
                         beta_reg1$Carbon*pred_ret_cop_t[['Carbon']] +
                         beta_reg1$Coal*pred_ret_cop_t[['Coal']] +
                         vol.ele*NoiseTerms[['Electricity']])
electricity.prices = Starting.Values$Electricity[nrow(Starting.Values) - test_months]*exp(apply(electricity.returns, 2, cumsum))

  #Distribution of ending prices
  h1 <- hist(tail(electricity.prices,1), breaks = "Sturges", 
           col  = "lightblue", 
           xlab = "Simulated Ending Price", main = "Histogram of Electricity Simulated Prices",
           plot=TRUE)
  abline(v=tail(Starting.Values$Electricity,1), col="red")

# Coal
coal.returns = pred_ret_cop_t$Coal
coal.prices  = Starting.Values$Coal[nrow(Starting.Values) - test_months]*exp(apply(coal.returns, 2, cumsum))

  #Distribution of ending prices
  h2 <- hist(tail(coal.prices,1), breaks = "Sturges", 
           col  = "yellow", 
           xlab = "Simulated Ending Price", main = "Histogram of Coal Simulated Prices",
           plot=TRUE)
  abline(v=tail(Starting.Values$Coal,1), col="red") 

mean(coal.prices)

# Carbon
carbon.returns = pred_ret_cop_t$Carbon
carbon.prices = Starting.Values$Carbon[nrow(Starting.Values) - test_months]*exp(apply(carbon.returns, 2, cumsum))

  #Distribution of ending prices
  h3 <- hist(tail(carbon.prices,1), breaks = "Sturges", 
           col  = "orange", 
           xlab = "Simulated Ending Price", main = "Histogram of Carbon Simulated Prices",
           plot=TRUE)
  abline(v=tail(Starting.Values$Carbon,1), col="red")

# Natural Gas
natgas.returns = pred_ret_cop_t$NaturalGas
natgas.prices  = Starting.Values$NaturalGas[nrow(Starting.Values) - test_months]*exp(apply(natgas.returns, 2, cumsum))

  #Distribution of ending prices
  h4 <- hist(tail(natgas.prices,1), breaks = "Sturges", 
           col  = "green", 
           xlab = "Simulated Ending Price", main = "Histogram of NaturalGas Simulated Prices",
           plot=TRUE)
  abline(v=tail(Starting.Values$NaturalGas,1), col="red")

# Fossil Fuel Utilization
ffuti.returns = pred_ret_cop_t$FossilFuelUtilization
ffuti         = Starting.Values$FossilFuelUtilization[nrow(Starting.Values) - test_months]*exp(apply(ffuti.returns, 2, cumsum))

# Wind Power Utilization
wputi.returns = pred_ret_cop_t$WindUtilization
wputi         = Starting.Values$WindUtilization[nrow(Starting.Values) - test_months]*exp(apply(wputi.returns, 2, cumsum))



###############################################################################
################          Hedging  Against Carbon Prices         ##############
###############################################################################

#Put option to hedge downside risk (option to sell at lower boundary if price goes lower)
#Call option to hedge upside risk if price goes above upper boundary (option to buy at upper boundary)
strike_call = 108.30
strike_put  = 72.20
T = 5

asian_payoff = pmax(colMeans(carbon.prices) - strike_call, 0)
carbon_call  = mean(asian_payoff)*exp(-0.02*T)

asian_payoffmin = pmax(strike_put - colMeans(carbon.prices), 0)
carbon_put      = mean(asian_payoffmin)*exp(-0.02*T)
cat("Asian Carbon Call Option value: ", carbon_call, "\n")
cat("Asian Carbon Put Option Value: ", carbon_put, "\n")
carbonhedge = carbon_call + carbon_put
cat("Total hedging cost per contract (both put & call): ", carbonhedge, "\n")

#Would benefit project 1 and 2. Benefit 2 the most since it has the most emissions

###############################################################################
##################             Cash Flows and NPV's             ###############
###############################################################################

# Portfolio 1, 100% invested in Natural Gas  
max.prod.natgas <- 200*64*30
prod.natgas     <- max.prod.natgas*ffuti

revenue.natgas      <- electricity.prices * prod.natgas
cost.natgas         <- natgas.prices * prod.natgas
  gross.natgas      <- revenue.natgas - cost.natgas
opex.natgas         <- 0.75*prod.natgas
carbonoffset.natgas <- 0.25*prod.natgas*carbon.prices
  cf.natgas         <- gross.natgas - opex.natgas - carbonoffset.natgas
  # Max/min w/95% certainty
  cat("The monthly CF's are with 95% cofidence higher than:", quantile(cf.natgas, 0.05))
  cat("The monthly CF's are with 95% cofidence lower than:", quantile(cf.natgas, 0.95))

discount.rate <- c(NA, nrow=60, ncol=1)
for (i in 1:60){
  discount.rate[i] = 1.07^(i/12)
}

discounted.cf.natgas <- apply(cf.natgas, 2, function(x) x / discount.rate)
  
PV.natgas    = colSums(discounted.cf.natgas)
  NPV.natgas = PV.natgas - 200000000
  mean(NPV.natgas)

# Portfolio 2 - 100% invested in a Coal Plant 
max.prod.coal <- 200*28*30
prod.coal     <- max.prod.coal*ffuti

revenue.coal      <- electricity.prices * prod.coal
cost.coal         <- coal.prices * prod.coal / 8
  gross.coal      <- revenue.coal - cost.coal
opex.coal         <- 1.25*prod.coal
carbonoffset.coal <- 0.5*prod.coal*carbon.prices
  cf.coal         <- gross.coal - opex.coal - carbonoffset.coal
  # Max/min w/95% certainty
  cat("The monthly CF's are with 95% cofidence higher than:", quantile(cf.coal, 0.05))
  cat("The monthly CF's are with 95% cofidence lower than:", quantile(cf.coal, 0.95))
  
discounted.cf.coal <- apply(cf.coal, 2, function(x) x / discount.rate)

PV.coal    = colSums(discounted.cf.coal)
  NPV.coal = PV.coal - 200000000
  mean(NPV.coal)

# Portfolio 3 - 100% invested in Wind Power
max.prod.wp <- 200*30*32
prod.wp     <- max.prod.wp * wputi

revenue.wp <- electricity.prices * prod.wp
opex.wp    <- 2 * prod.wp
  cf.wp    <- revenue.wp - opex.wp
  # Max/min w/95% certainty
  cat("The monthly CF's are with 95% cofidence higher than:", quantile(cf.wp, 0.05))
  cat("The monthly CF's are with 95% cofidence lower than:", quantile(cf.wp, 0.95))

discounted.cf.wp <- apply(cf.wp, 2, function(x) x / discount.rate)

PV.wp    = colSums(discounted.cf.wp)
  NPV.wp = c(PV.wp - 200000000)
  mean(NPV.wp)

# Portfolio 4 - Equal Weighted
cf.ew  <- 1/3 * (cf.natgas + cf.coal + cf.wp)
cat("The monthly CF's are with 95% cofidence higher than:", quantile(cf.ew, 0.05))
cat("The monthly CF's are with 95% cofidence lower than:", quantile(cf.ew, 0.95))
  NPV.ew <- 1/3 * (NPV.natgas + NPV.coal + NPV.wp)
  mean(NPV.ew)

# Portfolio 5 - "Optimal portfolio"
NPV.df  <- data.frame(NPV.natgas, NPV.coal, NPV.wp)
cov_mat <- cor(NPV.df)
cov_mat

h5 <- hist(tail(NPV.natgas/1000000,10000), breaks = "Sturges", 
           col  = "yellow", 
           xlab = "Simulated NPV(million EUR)", main = "Simulated NPV for Natural Gas ptf",
           plot=TRUE)

h6 <- hist(tail(NPV.coal/1000000,10000), breaks = "Sturges", 
           col  = "orange", 
           xlab = "Simulated NPV (million EUR)", main = "Simulated NPV for Coal ptf",
           plot=TRUE)

h7 <- hist(tail(NPV.wp/1000000,10000), breaks = "Sturges", 
           col  = "green", 
           xlab = "Simulated NPV (million EUR)", main = "Simulated NPV for Wind Power ptf",
           plot=TRUE)

h8 <- hist(tail(NPV.ew/1000000,10000), breaks = "Sturges", 
            col  = "blue", 
            xlab = "Simulated NPV (million EUR)", main = "Simulated NPV for Equal Weighted ptf",
            plot=TRUE)

cat("Std. dev. Natural Gas ptf:", sd(NPV.natgas))
cat("Std. dev. Coal ptf:", sd(NPV.coal))
cat("Std. dev. Wind Power ptf:", sd(NPV.wp))
cat("Std. dev. Equal Weighted ptf:", sd(NPV.ew))

cat("SR Natural Gas ptf:", SR.natgas = mean(NPV.natgas) / sd(NPV.natgas))
cat("SR Coal ptf:", SR.coal   = mean(NPV.coal) / sd(NPV.coal))
cat("SR Wind Power ptf:", SR.wp     = mean(NPV.wp) / sd(NPV.wp))
cat("SR Equal Weighted ptf:", SR.ew     = mean(NPV.ew) / sd(NPV.ew))

cf.ptf5 = 0.5 * (cf.natgas + cf.wp)
cat("The monthly CF's are with 95% cofidence higher than:", quantile(cf.ptf5, 0.05))
cat("The monthly CF's are with 95% cofidence lower than:", quantile(cf.ptf5, 0.95))

NPV.ptf5 <- 0.5*NPV.natgas + 0.5*NPV.wp
mean.npv = 0.5 * (mean(NPV.natgas + mean(NPV.wp)))
mean(NPV.ptf5)
cat("Std. dev. Test ptf:", sd(NPV.ptf5))
cat("SR Test ptf:", mean(NPV.ptf5) / sd(NPV.ptf5))

h9 <- hist(tail(NPV.ptf5/1000000,10000), breaks = "Sturges", 
           col  = "red", 
           xlab = "Simulated NPV (million EUR)", main = "Simulated NPV for portfolio 5",
           plot=TRUE)

###############################################################################
#################          CF/NPV w/regulated production        ###############
###############################################################################

# Portfolio 1, 100% invested in Natural Gas  
max.prod.natgas <- 200*64*30
prod.natgas     <- ifelse(electricity.prices >= (natgas.prices + 0.75 + 0.25*carbon.prices), 
                          max.prod.natgas*ffuti, 0)

revenue.natgas      <- electricity.prices * prod.natgas
cost.natgas         <- natgas.prices * prod.natgas
  gross.natgas      <- revenue.natgas - cost.natgas
opex.natgas         <- 0.75*prod.natgas
carbonoffset.natgas <- 0.25*prod.natgas*carbon.prices
  cf.natgas         <- gross.natgas - opex.natgas - carbonoffset.natgas
  # Max/min w/95% certainty
  cat("The monthly CF's are with 95% cofidence higher than:", quantile(cf.natgas, 0.05))
  cat("The monthly CF's are with 95% cofidence lower than:", quantile(cf.natgas, 0.95))
  
discounted.cf.natgas <- apply(cf.natgas, 2, function(x) x / discount.rate)

PV.natgas    = colSums(discounted.cf.natgas)
  NPV.natgas = PV.natgas - 200000000
  mean(NPV.natgas)

# Portfolio 2 - 100% invested in a Coal Plant 
max.prod.coal <- 200*28*30
prod.coal     <- ifelse(electricity.prices >= (coal.prices/8 + 1.25 + 0.5*carbon.prices), 
                        max.prod.coal*ffuti, 0)

revenue.coal      <- electricity.prices * prod.coal
cost.coal         <- coal.prices * prod.coal / 8
  gross.coal      <- revenue.coal - cost.coal
opex.coal         <- 1.25*prod.coal
carbonoffset.coal <- 0.5*prod.coal*carbon.prices
  cf.coal         <- gross.coal - opex.coal - carbonoffset.coal
  # Max/min w/95% certainty
  cat("The monthly CF's are with 95% cofidence higher than:", quantile(cf.coal, 0.05))
  cat("The monthly CF's are with 95% cofidence lower than:", quantile(cf.coal, 0.95))

discounted.cf.coal <- apply(cf.coal, 2, function(x) x / discount.rate)

PV.coal    = colSums(discounted.cf.coal)
  NPV.coal   = PV.coal - 200000000
  mean(NPV.coal)

# Portfolio 3 - 100% invested in Wind Power
max.prod.wp <- 200*30*32
prod.wp     <- ifelse(electricity.prices >= 2, max.prod.wp * wputi, 0)

revenue.wp <- electricity.prices * prod.wp
opex.wp    <- 2 * prod.wp
  cf.wp    <- revenue.wp - opex.wp
  # Max/min w/95% certainty
  cat("The monthly CF's are with 95% cofidence higher than:", quantile(cf.wp, 0.05))
  cat("The monthly CF's are with 95% cofidence lower than:", quantile(cf.wp, 0.95))
  
discounted.cf.wp <- apply(cf.wp, 2, function(x) x / discount.rate)

PV.wp    = colSums(discounted.cf.wp)
  NPV.wp = c(PV.wp - 200000000)
  mean(NPV.wp)

# Portfolio 4 - Equal Weighted
cf.ew  <- 1/3 * (cf.natgas + cf.coal + cf.wp)
cat("The monthly CF's are with 95% cofidence higher than:", quantile(cf.ew, 0.05))
cat("The monthly CF's are with 95% cofidence lower than:", quantile(cf.ew, 0.95))
  NPV.ew <- 1/3 * (NPV.natgas + NPV.coal + NPV.wp)
  mean(NPV.ew)

# Portfolio 5 - "Optimal portfolio"
NPV.df  <- data.frame(NPV.natgas, NPV.coal, NPV.wp)
cov_mat <- cor(NPV.df)
cov_mat

h9 <- hist(tail(NPV.natgas/1000000,10000), breaks = "Sturges", 
           col  = "yellow", 
           xlab = "Simulated NPV(million EUR)", main = "Simulated NPV for Natural Gas ptf",
           plot=TRUE)

h10 <- hist(tail(NPV.coal/1000000,10000), breaks = "Sturges", 
           col  = "orange", 
           xlab = "Simulated NPV (million EUR)", main = "Simulated NPV for Coal ptf",
           plot=TRUE)

h11 <- hist(tail(NPV.wp/1000000,10000), breaks = "Sturges", 
           col  = "green", 
           xlab = "Simulated NPV (million EUR)", main = "Simulated NPV for Wind Power ptf",
           plot=TRUE)

h12 <- hist(tail(NPV.ew/1000000,10000), breaks = "Sturges", 
           col  = "blue", 
           xlab = "Simulated NPV (million EUR)", main = "Simulated NPV for Equal Weighted ptf",
           plot=TRUE)

cat("Std. dev. Natural Gas ptf:", sd(NPV.natgas))
cat("Std. dev. Coal ptf:", sd(NPV.coal))
cat("Std. dev. Wind Power ptf:", sd(NPV.wp))
cat("Std. dev. Equal Weighted ptf:", sd(NPV.ew))

cat("SR Natural Gas ptf:", mean(NPV.natgas) / sd(NPV.natgas))
cat("SR Coal ptf:", mean(NPV.coal) / sd(NPV.coal))
cat("SR Wind Power ptf:", mean(NPV.wp) / sd(NPV.wp))
cat("SR Equal Weighted ptf:", mean(NPV.ew) / sd(NPV.ew))

cf.ptf5 = 0.5 * (cf.natgas + cf.wp)
cat("The monthly CF's are with 95% cofidence higher than:", quantile(cf.ptf5, 0.05))
cat("The monthly CF's are with 95% cofidence lower than:", quantile(cf.ptf5, 0.95))

NPV.ptf5 <- 0.5*NPV.natgas + 0.5*NPV.wp
cat("Std. dev. Test ptf:", sd(NPV.ptf5))
cat("SR Test ptf:", mean(NPV.ptf5) / sd(NPV.ptf5))

h9 <- hist(tail(NPV.ptf5/1000000,10000), breaks = "Sturges", 
           col  = "red", 
           xlab = "Simulated NPV (million EUR)", main = "Simulated NPV for portfolio 5",
           plot=TRUE)
