# Preparatory steps
rm(list=ls())

setwd("~/Documents/Skole/Master/Second semester/Sust. Finance")

arat <- read.csv("ESG rating agency A.csv")
brat <- read.csv("ESG rating agency B.csv")
crat <- read.csv("ESG rating agency C.csv")
drat <- read.csv("ESG rating agency D.csv")
erat <- read.csv("ESG rating agency E.csv")
frat <- read.csv("ESG rating agency F.csv")

mkt_ret_data <- read.csv("Market return.csv")
rf_data <- read.csv("Risk-free rate.csv")
beta <- read.csv("Stock beta.csv")
stock_ret_data <- read.csv("Stock return.csv")

# Removes the "Year"-columns
arat [,1] = NULL
brat [,1] = NULL
crat [,1] = NULL
drat [,1] = NULL
erat [,1] = NULL
frat [,1] = NULL
beta [,1] = NULL


########################################################################
##############       3 - Initial data processing       #################
########################################################################

# Excess returns from the market
excret_m <- c(mkt_ret_data$Market - rf_data$Risk.free.rate)

# Removing year column from df
r_i <- as.data.frame(stock_ret_data[,2:501])

# Excess stock returns
Re <- r_i - rf_data$Risk.free.rate

## Standardizes the ratings
std_arat <- 1-(arat-1)/4
std_brat <- brat/10
std_crat <- crat/100
std_drat <- drat/100
std_erat <- erat/100
std_frat <- frat/100

# Create single average rating
ESG <- 1/6 *(std_arat + std_brat + std_crat + std_drat + std_erat + std_frat)

# ESG uncertainty
delta <- ((std_arat - ESG)^2 + (std_brat - ESG)^2 +
            (std_crat - ESG)^2 + (std_drat - ESG)^2 +
            (std_erat - ESG)^2 + (std_frat - ESG)^2)/(6-1)


# Remove unnecessery data
rm(r_i)
rm(mkt_ret_data)
rm(rf_data)
rm(arat)
rm(brat)
rm(crat)
rm(drat)
rm(erat)
rm(frat)
rm(stock_ret_data)
rm(std_arat)
rm(std_brat)
rm(std_crat)
rm(std_drat)
rm(std_erat)
rm(std_frat)


########################################################################
#########       4 - Analysis - Univariate ptf sorts       ##############
########################################################################

# Change from dataframe to matrix
beta = as.matrix(beta)
Re = as.matrix(Re)
delta = as.matrix(delta)
ESG = as.matrix(ESG)

# Beta sorting
beta_ptfs <- matrix(0, nrow = 20, ncol = 5)

for (i in 1:20){
  beta_order <- order(beta[i,])
  beta_ordered <- Re[i,beta_order] 
  beta_ptfs[i,1] <- mean(beta_ordered[1:100]) 
  beta_ptfs[i,2] <- mean(beta_ordered[101:200]) 
  beta_ptfs[i,3] <- mean(beta_ordered[201:300]) 
  beta_ptfs[i,4] <- mean(beta_ordered[301:400]) 
  beta_ptfs[i,5] <- mean(beta_ordered[401:500]) 
}

avg_beta1 = mean(beta_ptfs [,1])
avg_beta2 = mean(beta_ptfs [,2])
avg_beta3 = mean(beta_ptfs [,3])
avg_beta4 = mean(beta_ptfs [,4])
avg_beta5 = mean(beta_ptfs [,5])


# ESG sorting
ESG_ptfs <- matrix(0, nrow = 20, ncol = 25) 

for (j in 1:20){
  ESG_order <- order(ESG[j,])
  ESG_ordered <- Re[j,ESG_order] 
  ESG_ptfs[j,1] <- mean(ESG_ordered[1:100]) 
  ESG_ptfs[j,2] <- mean(ESG_ordered[101:200]) 
  ESG_ptfs[j,3] <- mean(ESG_ordered[201:300]) 
  ESG_ptfs[j,4] <- mean(ESG_ordered[301:400]) 
  ESG_ptfs[j,5] <- mean(ESG_ordered[401:500]) 
}

avg_ESG1 = mean(ESG_ptfs [,1])
avg_ESG2 = mean(ESG_ptfs [,2])
avg_ESG3 = mean(ESG_ptfs [,3])
avg_ESG4 = mean(ESG_ptfs [,4])
avg_ESG5 = mean(ESG_ptfs [,5])


# Delta sorting
delta_ptfs <- matrix(0, nrow = 20, ncol = 25) 

for (k in 1:20){
  delta_order <- order(delta[k,])
  delta_ordered <- Re[k,ESG_order] 
  delta_ptfs[k,1] <- mean(delta_ordered[1:100]) 
  delta_ptfs[k,2] <- mean(delta_ordered[101:200]) 
  delta_ptfs[k,3] <- mean(delta_ordered[201:300]) 
  delta_ptfs[k,4] <- mean(delta_ordered[301:400]) 
  delta_ptfs[k,5] <- mean(delta_ordered[401:500]) 
}

avg_delta1 = mean(delta_ptfs [,1])
avg_delta2 = mean(delta_ptfs [,2])
avg_delta3 = mean(delta_ptfs [,3])
avg_delta4 = mean(delta_ptfs [,4])
avg_delta5 = mean(delta_ptfs [,5])


########################################################################
###########       5 - Analysis - Asset pricing models       ############
########################################################################




LS_beta = beta_ptfs [,5] - beta_ptfs [,1]
capm_beta = lm(LS_beta ~ excret_m)

LS_ESG = ESG_ptfs [,5] - ESG_ptfs [,1]
capm_ESG = lm(LS_ESG ~ excret_m)

LS_delta = delta_ptfs [,5] - delta_ptfs [,1]
capm_delta = lm(LS_delta ~ excret_m)

summary(capm_beta)
summary(capm_ESG)
summary(capm_delta)
# CAPM holds if t-stat for intercept is higher than |2|


# 5.2
reg_ESG <- matrix(0,nrow =20, ncol=2)

for (m in 1:20){
  X <- matrix(1,nrow = 2, ncol = 500)
  X[2,] <- ESG[m,]
  reg_ESG[m,] <- Re[m,]%*%t(X)%*%solve(X%*%t(X))
}

t_stat_ESG = sqrt(20) * apply(reg_ESG,2,mean) / sqrt(apply(reg_ESG,2,var))


# 5.3
reg_3F <- matrix (0, nrow=20, ncol=4)

for (n in 1:20){
  Y = matrix(1,nrow = 4, ncol = 500)
  Y[2,] = beta[n,]
  Y[3,] = ESG[n,]
  Y[4,] = delta[n,]
  reg_3F[n,] = Re[n,]%*%t(Y)%*%solve(Y%*%t(Y))
}

t_stat_3F = sqrt(20) * apply(reg_3F,2,mean) / sqrt(apply(reg_3F,2,var))

sr_beta = mean(reg_3F[,2]) / sd(reg_3F[,2])
sr_ESG = mean(reg_3F[,3]) / sd(reg_3F[,3])
sr_delta = mean(reg_3F[,4]) / sd(reg_3F[,4])

# 5.4
reg_3F_LS_beta = lm(LS_beta ~ reg_3F)
summary(reg_3F_LS_beta)

reg_3F_LS_ESG = lm(LS_ESG ~ reg_3F)
summary(reg_3F_LS_ESG)

reg_3F_LS_delta = lm(LS_delta ~ reg_3F)
summary(reg_3F_LS_delta)
# Compare t-stat of intercept with the one for CAPM in 5.1  

########################################################################
###########       6 - Impact of ESG news on returns       ##############
########################################################################

snws = diff(ESG)
unws = diff(delta)


# Residuals from 5.3 (chatgpt)
residuals_3F <- matrix(0, nrow = nrow(Re), ncol = ncol(Re))

for (p in 1:20){
  # Create a data frame of predictor variables
  Y_df <- data.frame(
    beta = beta[p, ],
    ESG = ESG[p, ],
    delta = delta[p, ],
    X1 = rep(1, 500)
  )
  
  # Fit a linear regression model and calculate the residuals
  fit <- lm(Re[p, ] ~ ., data = Y_df)
  residuals_3F[p, ] <- fit$residuals
}

# (39)
reg_u <- matrix (0, nrow=20, ncol=2)

for (q in 1:20){
  Y = matrix(1,nrow = 2, ncol = 500)
  Y[1,] = snws[q,]
  Y[2,] = unws[q,]
  reg_u[q,] = residuals_3F[q,]%*%t(Y)%*%solve(Y%*%t(Y))
}

# (40)
avg_coef_snws = 1 / 20 * sum(reg_u[,1])
avg_coef_unws = 1 / 20 * sum(reg_u[,2])

# (41)
t_stat_nws = sqrt(20) * avg_coef_snws / var(reg_u)


########################################################################
#####################       7 - Double sorts       #####################
########################################################################

ds_ptf1 <- matrix(0, nrow = 20, ncol = 25) 
ds_ptf2 <- matrix(0, nrow = 20, ncol = 25) 

for (r in 1:20){
  beta_order <- order(beta[r,])
  beta_ordered <- Re[r,beta_order]
  ESG_beta_ordered <- ESG[r,beta_order]
  delta_beta_ordered <- delta[r,beta_order]
  
  for (s in 1:5){
    N_L <- 1+(s-1)*100
    N_H <- s*100
    Re_beta_group <- beta_ordered[N_L:N_H]
    ESG_beta_group <- ESG_beta_ordered[N_L:N_H]
    delta_beta_group <- delta_beta_ordered[N_L:N_H]
    ESG_order <- order(ESG_beta_group)
    delta_order <- order(delta_beta_group)
    Re_beta_ESG_ordered <- Re_beta_group[ESG_order] # fixed line
    
    Np <- 1 + (s-1) * 5
    ds_ptf1[r,Np]   <- mean(Re_beta_ESG_ordered[1:20]) # low ESG
    ds_ptf1[r,Np+1] <- mean(Re_beta_ESG_ordered[21:40]) 
    ds_ptf1[r,Np+2] <- mean(Re_beta_ESG_ordered[41:60])
    ds_ptf1[r,Np+3] <- mean(Re_beta_ESG_ordered[61:80])
    ds_ptf1[r,Np+4] <- mean(Re_beta_ESG_ordered[81:100]) # high ESG
    
    ds_ptf2[r,Np]   <- mean(Re_beta_group[delta_order][1:20])
    ds_ptf2[r,Np+1] <- mean(Re_beta_group[delta_order][21:40])
    ds_ptf2[r,Np+2] <- mean(Re_beta_group[delta_order][41:60])
    ds_ptf2[r,Np+3] <- mean(Re_beta_group[delta_order][61:80])
    ds_ptf2[r,Np+4] <- mean(Re_beta_group[delta_order][81:100])
  }
}


# Long high ESG, short low ESG
LS_beta_ESG <- (ds_ptf1[,5] + ds_ptf1[,10] + ds_ptf1[,15] + ds_ptf1[,20] + ds_ptf1[,25]) - (ds_ptf1[,1] + ds_ptf1[,6] + ds_ptf1[,11] + ds_ptf1[,16] + ds_ptf1[,21])

# Average and standard deviation
avg_LS_beta_ESG <- mean(LS_beta_ESG)
sd_LS_beta_ESG <- sd(LS_beta_ESG)

# Sharpe ratio
sr_LS_beta_ESG <- avg_LS_beta_ESG / sd_LS_beta_ESG

# Correlation
cor_ESG <- cor(LS_beta_ESG, reg_3F[,3])

# Time series plot
plot(LS_beta_ESG, 
     type = "l",
     col  = "red",
     xlab = "Years",
     ylab = "Excess return",
     main = "Time series plot",
     ylim = c(-1.3, 1))

lines(reg_3F[,3], col="blue")


# Long high ESG uncertainty, short low ESG uncertainty
LS_beta_delta <- (ds_ptf2[,5] + ds_ptf2[,10] + ds_ptf2[,15] + ds_ptf2[,20] + ds_ptf2[,25]) - (ds_ptf2[,1] + ds_ptf2[,6] + ds_ptf2[,11] + ds_ptf2[,16] + ds_ptf2[,21])

# Average and standard deviation
avg_LS_beta_delta <- mean(LS_beta_delta)
sd_LS_beta_delta <- sd(LS_beta_delta)

# Sharpe ratio
sr_LS_beta_delta <- avg_LS_beta_delta / sd_LS_beta_delta

# Correlation 
cor_delta <- cor(LS_beta_delta, reg_3F[,4])

# Time series plot
plot(LS_beta_delta, 
     type = "l",
     col  = "red",
     xlab = "Years",
     ylab = "Excess return",
     main = "Time series plot",
     ylim = c(-0.3, 0.8))

lines(reg_3F[,4], col="blue")








