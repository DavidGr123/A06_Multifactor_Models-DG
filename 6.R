remotes::install_github("braverock/factorAnalytics",  build_vignettes = TRUE, force = TRUE)
pacman::p_load(tidyverse,tidyquant,FFdownload,FactorAnalytics,PerformanceAnalytics)
pacman::p_load(tidyverse,tidyquant,FFdownload,PortfolioAnalytics,nloptr)
pacman::p_load(tidyverse,tidyquant,PortfolioAnalytics,nloptr,tsibble,matrixcalc,Matrix,timetk,xts)
```

```{r}
## Important Function

#       fitTsfm
#   
#     fitTsfm(asset.names, factor.names, data, fit.method, variable.selection, ...):
#     
#     Fits a time series (a.k.a. macroeconomic) factor model for one or more asset returns or excess
#     returns using time series regression. Least squares (LS), discounted least squares (DLS) and
#     robust regression fitting are possible. Variable selection methods include "stepwise", "subsets" and "lars". An object of class "tsfm" containing the
#     fitted objects, estimated coefficients, R-squared and residual volatility is returned.

```


**Please** remember to put your assignment solutions in `rmd` format using **many** chunks and putting readable text in between, similar to my examples given in Research Methods and Assignment 1!
  
  For all exercises: Please use the Assignment-Forum to post your questions, I will try my best to help you along! If you follow the vignettes from `factorAnalytics`, wherever it says `z.score=T`, please exchange it for either `z.score='crossSection'` or `z.score='timeSeries'` depending on the task at hand.

## Exercise 1: Estimating the CAPM (from A05)

In this exercise we want to estimate the CAPM. Please read carefully through the two documents provided (right hand side: files). Then we start to collect the necessary data:
  
  a) From Datastream get the last 10 years of data from the 100 stocks of the S&P100 using the list `LS&P100I` (S&P 100): total return index (RI) and market cap (MV)
b) Further import the Fama-French-Factors from Kenneth Frenchs homepage (monthly, e.g. using `FFdownload`). From both datasets we select data for the last (available) 60 months, calculate returns (simple percentage) for the US-Stocks and eliminate those stocks that have NAs for this period.
c) Now subtract the risk-free rate from all the stocks. Then estimate each stocks beta with the market: Regress all stock excess returns on the market excess return and save all betas (optimally use `mutate` and `map` in combination with `lm`). Estimate the mean-return for each stock and plot the return/beta-combinations. Create the security market line and include it in the plot! What do you find?
  d) In a next step (following both documents), we sort the stocks according to their beta and build ten value-weighted portfolios (with more or less the same number of stocks). Repeat a) for the ten portfolios. What do you observe?
  e) In the third step you follow page 6-8 of the second document and estimate the second-pass regression with the market and then market & idiosyncratic risk. What do you observe? Present all your results in a similar fashion as in the document.

## Exercise 2: Calculating and checking the CAPM cont. (from A05)

```{r}
pacman::p_load(tidyverse,tidyquant,FFdownload,PortfolioAnalytics,nloptr,readxl,quantmod,FFdownload,timetk, dplyr, xts)
```


As we have seen: the CAPM for small portfolios does not work very well, and so we start using portfolios that get rid of the idiosyncratic risk!
  Go to Kenneth French's Homepage  again and download the following datasets: "Portfolios Formed on Market Beta" (where we will use 10 monthly value weighted portfolios formed on beta) and "25 Portfolios Formed on Size and Market Beta" (same thing) as well as the market factor and rf (as before). Now we are going to check the CAPM like famous researchers have done it!
We can use returns as they are in the files (simple returns)!


```{r}
inputlist<-c("F-F_Research_Data_Faktors_CSV.zip","Portfolios_Formed_on_BETA_CSV.zip")
             
#Now process only these files if they can be matched (download only)
FFdownload(output_file = "FFdata.RData", inputlist = inputlist, exclude_daily=TRUE)

load("FFdata.RData")
portf_mkt_betatest<-(FFdownload$x_Portfolios_Formed_on_BETA$monthly$value_weighted_returns)

portf_mkt_betatest

```



```{r}
#Download the Portfolios from Kenneth French's Homepage
portf_mkt_beta <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_BETA_CSV.zip"
portf_mkt_beta_csv <- "Portfolios_Formed_on_BETA.csv"
temp <- tempfile()
download.file(portf_mkt_beta, temp, quiet = TRUE)
portf_mkt_beta <- read_csv(unz(temp, portf_mkt_beta_csv), skip = 15, quote = "\",") %>%
  dplyr::rename(date = "X1") %>%
  mutate_at(vars(-date), as.numeric) %>%
  mutate(date = rollback(ymd(parse_date_time(date, "%Y%m") + months(1))))%>%
  filter(date >= first('1964-01-01') & date <= '2019-12-31')

#Download the market factor and rf (Fama/French 3 Research Factors)
mkt_factors <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"
mkt_factors_csv <- "F-F_Research_Data_Factors.CSV"
temp <- tempfile()
download.file(mkt_factors, temp, quiet = TRUE)
mkt_factors <- read_csv(unz(temp, mkt_factors_csv), skip = 3, quote = "\",") %>%
  dplyr::rename(date = X1) %>%
  mutate_at(vars(-date), as.numeric) %>%
  mutate(date = rollback(ymd(parse_date_time(date, "%Y%m") + months(1)))) %>%
  filter(date >= first('1964-01-01') & date <= '2019-12-31')


```


a)	Subtract the risk-free rate from the first set of 10 portfolios (only sorted on beta) (Lo 10,., Hi 10) and estimate each stocks beta with the market. Estimate the mean-return for each stock and plot the return/beta-combinations. Create the security market line and include it in the plot! What do you find? (You can split the file in 2-3 different time blocks and see if something changes). * Now we are done with the first-pass regression.*
  
  
  Subtract the risk-free rate from the first set of 10 portfolios (only sorted on beta) (Lo 10,., Hi 10) and estimate each stocks beta with the market.

```{r}
#join data
ten_portf <- portf_mkt_beta[1:672, -c(2:6)]
ten_portf_joined <- left_join(mkt_factors, ten_portf)

mkt_factors
ten_portf
ten_portf_joined

```
```{r, echo=FALSE}
ten_portf_joined <- ten_portf_joined <- ten_portf_joined%>% dplyr::rename("Lo10" = "Lo 10") %>% dplyr::rename("Dec2" = "Dec 2") %>% dplyr::rename("Dec3" = "Dec 3") %>% dplyr::rename("Dec4" = "Dec 4") %>% dplyr::rename("Dec5" = "Dec 5") %>% dplyr::rename("Dec6" = "Dec 6") %>% dplyr::rename("Dec7" = "Dec 7") %>% dplyr::rename("Dec8" = "Dec 8") %>% dplyr::rename("Dec9" = "Dec 9") %>% dplyr::rename("Hi10" = "Hi 10")

view(ten_portf_joined)
ten_portf_joined

```

```{r}
#substract Risk-Free-Rate
ten_portf_rf <- mutate(ten_portf_joined, Lo10rf = Lo10 - RF, Dec2rf = Dec2 - RF, Dec3rf = Dec3 - RF, Dec4rf = Dec4 - RF, Dec5rf = Dec5 -RF, Dec6rf = Dec6 - RF, Dec7rf = Dec7 - RF, Dec8rf = Dec8 - RF, De9rf = Dec9 - RF, Hi10rf = Hi10 - RF)
ten_portf_rf <- ten_portf_rf[-2:-15]

view(ten_portf_rf)
ten_portf_rf
```

```{r, echo=FALSE}
#Create XTS
mkt_factors_xts <- tk_xts(data = mkt_factors, date_var = date)
ten_portf_rf_xts <- ten_portf_rf %>%
  tk_xts(date_var = date, silent = TRUE)

```
```{r}
?lm()
#Calculate Betas for each portfolio
betas_ten_portf_lm <- lm(ten_portf_rf_xts ~ mkt_factors_xts[, 1])
betas_ten_portf_lm
betas_ten_portf <- CAPM.beta(Ra = ten_portf_rf_xts, Rb = mkt_factors_xts[, 1], Rf = 0)
betas_ten_portf
```
Estimate the mean-return for each stock and plot the return/beta-combinations.

```{r}
#Estimate Mean Return
mean_ten_portf_rf_xts <- as.data.frame(lapply(ten_portf_rf_xts, FUN=mean))
mean_ten_portf_rf_xts

#Plot the return/beta-combinations
plot.default(x = betas_ten_portf, xlim=c(0, 2),
             y = mean_ten_portf_rf_xts, ylim=c(0, 1),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations")
```
Create the security market line and include it in the plot! What do you find?
  
  ```{r}
mean_mkt <- as.data.frame(lapply(mkt_factors_xts[, 1], FUN=mean))
y_mkt <- mean_mkt[1, 1]
plot.default(x = betas_ten_portf, xlim=c(0, 2),
             y = mean_ten_portf_rf_xts, ylim=c(0, 1),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations",
             abline(0, y_mkt))
plot.default(x = betas_ten_portf, xlim=c(0, 2), 
             y = mean_ten_portf_rf_xts, ylim=c(0, 10), 
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations",
             abline(0, y_mkt))

#summary
summary_CAPM_ten_portf <- (table.CAPM(Ra = ten_portf_rf_xts, Rb = mkt_factors_xts[, 1], Rf = 0)[1:9, ])
```
(You can split the file in 2-3 different time blocks and see if something changes). * Now we are done with the first-pass regression.*
  
  ```{r}
#look for first 10 years
ten_portf_rf_10yrs_xts <- ten_portf_rf[1:120, ] %>%
  tk_xts(date_var = date, silent = TRUE)
betas_ten_portf_rf_10yrs <- CAPM.beta(Ra = ten_portf_rf_10yrs_xts, Rb = mkt_factors_xts[1:120, 1], Rf = 0)
mean_ten_portf_rf_10yrs_xts <- as.data.frame(lapply(ten_portf_rf_10yrs_xts, FUN=mean))
mean_mkt_10yrs <- as.data.frame(lapply(mkt_factors_xts[1:120, 1], FUN=mean))
y_mkt_10yrs <- mean_mkt_10yrs[1, 1]
plot.default(x = betas_ten_portf_rf_10yrs, xlim=c(0, 2),
             y = mean_ten_portf_rf_10yrs_xts, ylim=c(0, 1),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations 1964-1974",
             abline(0, y_mkt_10yrs))
summary_CAPM_ten_portf_10yrs <- (table.CAPM(Ra = ten_portf_rf_xts[1:120, ], Rb = mkt_factors_xts[1:120, 1], Rf = 0)[1:9, ])
summary_CAPM_ten_portf_10yrs
```
```{r, echo=FALSE}

#look for 2000-2019
ten_portf_rf_2000_xts <- ten_portf_rf[433:672, ] %>%
  tk_xts(date_var = date, silent = TRUE)
betas_ten_portf_rf_2000 <- CAPM.beta(Ra = ten_portf_rf_2000_xts, Rb = mkt_factors_xts[433:672, 1], Rf = 0)
mean_ten_portf_rf_2000_xts <- lapply(ten_portf_rf_2000_xts, FUN=mean)
mean_ten_portf_rf_2000_xts <- as.data.frame(mean_ten_portf_rf_2000_xts)
mean_mkt_2000 <- lapply(mkt_factors_xts[433:672, 1], FUN=mean)
mean_mkt_2000 <- as.data.frame(mean_mkt_2000)
y_mkt_2000 <- mean_mkt_2000[1, 1]
plot.default(x = betas_ten_portf_rf_2000, xlim=c(0, 2),
             y = mean_ten_portf_rf_2000_xts, ylim=c(0, 1),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations 2000-2019",
             abline(0, y_mkt_2000))
summary_CAPM_ten_portf_2000 <- (table.CAPM(Ra = ten_portf_rf_xts[433:672, ], Rb = mkt_factors_xts[433:672, 1], Rf = 0)[1:9, ])
summary_CAPM_ten_portf_2000

plot.default(x = betas_ten_portf, xlim=c(0, 2),
             y = mean_ten_portf_rf_xts, ylim=c(0, 1),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations 1964-2019",
             abline(0, y_mkt))
summary_CAPM_ten_portf


```


b)	In the second-pass regression we now regress the average stock returns on the betas estimated before. What do you find in the coefficients and does this contradict the CAPM? Try different time periods again and see what you find. (all of the interpretations are in BKM pp.416f). 


```{r}
betas_ten_portf


```

```{r}
#There are a number of reasons we expect might the CAPM to fail:
#1. Imperfect measures of the market portfolio
#2. Beta is an incomplete measure of risk
#3. Tax effects
#4. Non - normality of returns
#5. No riskless asset
#6. Divergent borrowing and lending rates
```

There are a number of reasons we expect might the CAPM to
fail:
  1. Imperfect measures of the market portfolio
2. Beta is an incomplete measure of risk
3. Tax effects
4. Non - normality of returns
5. No riskless asset
6. Divergent borrowing and lending rates

c)	Now do the extended second pass regression (regress on betas and residual-sds that you can extract from the regression) and see what you find for different periods. Interpret according to concept check 13.2. One of the (many) problems of the CAPM can be the correlation between residual variances and betas. Calculate and interpret.

```{r}
#Look at a) -> We now do it with the mean return of every portfolio combined... 

#1964-2019
com_mean_ten_portf_rf <- sum(mean_ten_portf_rf_xts)/10
mean_betas_ten_portf <- sum(betas_ten_portf)/10
plot.default(x = mean_betas_ten_portf, xlim=c(0, 2),
             y = com_mean_ten_portf_rf, ylim=c(0, 2),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations 10 Portfolios 1964-2019",
             abline(0, y_mkt))


```

```{r, echo=FALSE}

#1964-1974
com_mean_ten_portf_rf_10yrs <- sum(mean_ten_portf_rf_10yrs_xts)/10
mean_betas_ten_portf_10yrs <- sum(betas_ten_portf_rf_10yrs)/10
plot.default(x = mean_betas_ten_portf_10yrs, xlim=c(0, 2),
             y = com_mean_ten_portf_rf_10yrs, ylim=c(0, 2),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations 10 Portfolios 1964-1974",
             abline(0, y_mkt_10yrs))

#2000-2019
com_mean_ten_portf_rf_2000 <- sum(mean_ten_portf_rf_2000_xts)/10
mean_betas_ten_portf_2000 <- sum(betas_ten_portf_rf_2000)/10
plot.default(x = mean_betas_ten_portf_2000, xlim=c(0, 2),
             y = com_mean_ten_portf_rf_2000, ylim=c(0, 2),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations 10 Portfolios 2000-2019",
             abline(0, y_mkt_2000))

```

```{r, echo=FALSE}

#SML-Function
calc_residual <- function(x) {y <- y_mkt*x}
calc_residual_10yrs <- function(x) {y <- y_mkt_10yrs*x}
calc_residual_2000 <- function(x) {y <- y_mkt_2000*x}
residual_1964_2019 <- as.data.frame((com_mean_ten_portf_rf - calc_residual(mean_betas_ten_portf))^2)
residual_1964_1974 <- as.data.frame((com_mean_ten_portf_rf_10yrs - calc_residual_10yrs(mean_betas_ten_portf_10yrs))^2)
residual_2000_2019 <- as.data.frame((com_mean_ten_portf_rf_2000 - calc_residual_2000(mean_betas_ten_portf_2000))^2)
joined_residuals <- merge(residual_1964_2019[1, 1], residual_1964_1974[1, 1])
joined_residuals <- merge(joined_residuals, residual_2000_2019)
Residuals_different_timeperiods <- joined_residuals %>% 
  dplyr::rename("Residual 2000-2019" = "(com_mean_ten_portf_rf_2000 - calc_residual_2000(mean_betas_ten_portf_2000))^2") %>% dplyr::rename("Residual 1964-2008" = "x") %>% dplyr::rename("Residual 1964-1974" = "y")
Residuals_different_timeperiods
```


d)	Try again with 25 portfolios sorted on size and beta. What do you find? Is that interesting? 
  
  ```{r}
inputlist1<-c("F-F_Research_Data_Faktors_CSV.zip","25_Portfolios_Formed_on_Size_and Market_Beta_CSV.zip")

#Now process only these files if they can be matched (download only)
FFdownload(output_file = "FFdata.RData", inputlist = inputlist1, exclude_daily=TRUE)

load("FFdata.RData")
twentyfive_portf<-(FFdownload$x_25_Portfolios_Formed_on_Size_and Market_Beta$monthly$value_weighted_returns)

twentyfive_portf

```

download.file(portf_mkt_beta, temp, quiet = TRUE)
portf_mkt_beta <- read_csv(unz(temp, portf_mkt_beta_csv), skip = 15, quote = "\",") %>%
  dplyr::rename(date = "X1") %>%
  mutate_at(vars(-date), as.numeric) %>%
  mutate(date = rollback(ymd(parse_date_time(date, "%Y%m") + months(1))))%>%
  filter(date >= first('1964-01-01') & date <= '2019-12-31')
```



```{r}
twentyfive_portf
```


```{r}
#join data
twentyfive_portf <- portf_mkt_beta[1:672, -c(7:16)]
twentyfive_portf_joined <- left_join(mkt_factors, twentyfive_portf)
```

```{r, echo=FALSE}

twentyfive_portf_joined <- twentyfive_portf_joined <- twentyfive_portf_joined%>%
  dplyr::rename("Lo20" = "Lo 20") %>%
  dplyr::rename("Qnt2" = "Qnt 2") %>%
  dplyr::rename("Qnt3" = "Qnt 3") %>%
  dplyr::rename("Qnt4" = "Qnt 4") %>%
  dplyr::rename("Hi20" = "Hi 20")
````

```{r}
#substract Risk-Free-Rate
twentyfive_portf_rf <- mutate(twentyfive_portf_joined, Lo20rf = Lo20 - RF, Qnt2rf = Qnt2 - RF, Qnt3rf = Qnt3 - RF, Qnt4rf = Qnt4 - RF, Hi20rf = Hi20 - RF)
twentyfive_portf_rf <- twentyfive_portf_rf[-2:-10]

```

```{r, echo=FALSE}
#substract Risk-Free-Rate
twentyfive_portf_rf <- mutate(twentyfive_portf_joined, Lo20rf = Lo20 - RF, Qnt2rf = Qnt2 - RF, Qnt3rf = Qnt3 - RF, Qnt4rf = Qnt4 - RF, Hi20rf = Hi20 - RF)
twentyfive_portf_rf <- twentyfive_portf_rf[-2:-10]


#Create XTS
twentyfive_portf_rf_xts <- twentyfive_portf_rf %>%
  tk_xts(date_var = date, silent = TRUE)

#Calculate Betas for each portfolio
betas_twentyfive_portf <- CAPM.beta(Ra = twentyfive_portf_rf_xts, Rb = mkt_factors_xts[, 1], Rf = 0)

#Estimate Mean Return
mean_twentyfive_portf_rf_xts <- as.data.frame(lapply(twentyfive_portf_rf_xts, FUN=mean))

#Plot the return/beta-combinations
plot.default(x = betas_twentyfive_portf, xlim=c(0, 2),
             y = mean_twentyfive_portf_rf_xts, ylim=c(0, 1),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations 25",
             abline(0, y_mkt))

#We now do it with the mean return of every portfolio combined...
com_mean_twentyfive_portf_rf <- sum(mean_twentyfive_portf_rf_xts)/5
# and the beta
mean_betas_twentyfive_portf <- sum(betas_twentyfive_portf)/5

plot.default(x = mean_betas_ten_portf, xlim=c(0, 2),
             y = com_mean_ten_portf_rf, ylim=c(0, 1),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations Portfolio Summary 25",
             abline(0, y_mkt))
plot.default(x = mean_betas_ten_portf, xlim=c(0, 2),
             y = com_mean_ten_portf_rf, ylim=c(0, 1),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations Portfolio Summary 10",
             abline(0, y_mkt))

```


```{r}
#substract Risk-Free-Rate
ten_portf_rf <- mutate(ten_portf_joined, Lo10rf = Lo10 - RF, Dec2rf = Dec2 - RF, Dec3rf = Dec3 - RF, Dec4rf = Dec4 - RF, Dec5rf = Dec5 -RF, Dec6rf = Dec6 - RF, Dec7rf = Dec7 - RF, Dec8rf = Dec8 - RF, De9rf = Dec9 - RF, Hi10rf = Hi10 - RF)
ten_portf_rf <- ten_portf_rf[-2:-15]

view(ten_portf_rf)
ten_portf_rf
```

```{r, echo=FALSE}
#Create XTS
mkt_factors_xts <- tk_xts(data = mkt_factors, date_var = date)
ten_portf_rf_xts <- ten_portf_rf %>%
  tk_xts(date_var = date, silent = TRUE)

```
```{r}
?lm()
#Calculate Betas for each portfolio
betas_ten_portf_lm <- lm(ten_portf_rf_xts ~ mkt_factors_xts[, 1])
betas_ten_portf_lm
betas_ten_portf <- CAPM.beta(Ra = ten_portf_rf_xts, Rb = mkt_factors_xts[, 1], Rf = 0)
betas_ten_portf
```
Estimate the mean-return for each stock and plot the return/beta-combinations.

```{r}
#Estimate Mean Return
mean_ten_portf_rf_xts <- as.data.frame(lapply(ten_portf_rf_xts, FUN=mean))
mean_ten_portf_rf_xts

#Plot the return/beta-combinations
plot.default(x = betas_ten_portf, xlim=c(0, 2),
             y = mean_ten_portf_rf_xts, ylim=c(0, 1),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations")
```
Create the security market line and include it in the plot! What do you find?
  
  ```{r}
mean_mkt <- as.data.frame(lapply(mkt_factors_xts[, 1], FUN=mean))
y_mkt <- mean_mkt[1, 1]
plot.default(x = betas_ten_portf, xlim=c(0, 2),
             y = mean_ten_portf_rf_xts, ylim=c(0, 1),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations",
             abline(0, y_mkt))


#
```
(You can split the file in 2-3 different time blocks and see if something changes). * Now we are done with the first-pass regression.*
  
  ```{r}
#look for first 10 years
ten_portf_rf_10yrs_xts <- ten_portf_rf[1:120, ] %>%
  tk_xts(date_var = date, silent = TRUE)
betas_ten_portf_rf_10yrs <- CAPM.beta(Ra = ten_portf_rf_10yrs_xts, Rb = mkt_factors_xts[1:120, 1], Rf = 0)
mean_ten_portf_rf_10yrs_xts <- as.data.frame(lapply(ten_portf_rf_10yrs_xts, FUN=mean))
mean_mkt_10yrs <- as.data.frame(lapply(mkt_factors_xts[1:120, 1], FUN=mean))
y_mkt_10yrs <- mean_mkt_10yrs[1, 1]
plot.default(x = betas_ten_portf_rf_10yrs, xlim=c(0, 2),
             y = mean_ten_portf_rf_10yrs_xts, ylim=c(0, 1),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations 1964-1974",
             abline(0, y_mkt_10yrs))
summary_CAPM_ten_portf_10yrs <- (table.CAPM(Ra = ten_portf_rf_xts[1:120, ], Rb = mkt_factors_xts[1:120, 1], Rf = 0)[1:9, ])
summary_CAPM_ten_portf_10yrs
```
```{r, echo=FALSE}

#look for 2000-2019
ten_portf_rf_2000_xts <- ten_portf_rf[433:672, ] %>%
  tk_xts(date_var = date, silent = TRUE)
betas_ten_portf_rf_2000 <- CAPM.beta(Ra = ten_portf_rf_2000_xts, Rb = mkt_factors_xts[433:672, 1], Rf = 0)
mean_ten_portf_rf_2000_xts <- lapply(ten_portf_rf_2000_xts, FUN=mean)
mean_ten_portf_rf_2000_xts <- as.data.frame(mean_ten_portf_rf_2000_xts)
mean_mkt_2000 <- lapply(mkt_factors_xts[433:672, 1], FUN=mean)
mean_mkt_2000 <- as.data.frame(mean_mkt_2000)
y_mkt_2000 <- mean_mkt_2000[1, 1]
plot.default(x = betas_ten_portf_rf_2000, xlim=c(0, 2),
             y = mean_ten_portf_rf_2000_xts, ylim=c(0, 1),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations 2000-2019",
             abline(0, y_mkt_2000))
summary_CAPM_ten_portf_2000 <- (table.CAPM(Ra = ten_portf_rf_xts[433:672, ], Rb = mkt_factors_xts[433:672, 1], Rf = 0)[1:9, ])
summary_CAPM_ten_portf_2000

plot.default(x = betas_ten_portf, xlim=c(0, 2),
             y = mean_ten_portf_rf_xts, ylim=c(0, 1),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations 1964-2019",
             abline(0, y_mkt))
summary_CAPM_ten_portf


```


b)	In the second-pass regression we now regress the average stock returns on the betas estimated before. What do you find in the coefficients and does this contradict the CAPM? Try different time periods again and see what you find. (all of the interpretations are in BKM pp.416f). 

There are a number of reasons we expect might the CAPM to
fail:
  1. Imperfect measures of the market portfolio
2. Beta is an incomplete measure of risk
3. Tax effects
4. Non - normality of returns
5. No riskless asset
6. Divergent borrowing and lending rates

c)	Now do the extended second pass regression (regress on betas and residual-sds that you can extract from the regression) and see what you find for different periods. Interpret according to concept check 13.2. One of the (many) problems of the CAPM can be the correlation between residual variances and betas. Calculate and interpret.

```{r}
#Look at a) -> We now do it with the mean return of every portfolio combined... 

#1964-2019
com_mean_ten_portf_rf <- sum(mean_ten_portf_rf_xts)/10
mean_betas_ten_portf <- sum(betas_ten_portf)/10
plot.default(x = mean_betas_ten_portf, xlim=c(0, 2),
             y = com_mean_ten_portf_rf, ylim=c(0, 2),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations 10 Portfolios 1964-2019",
             abline(0, y_mkt))


```

```{r, echo=FALSE}

#1964-1974
com_mean_ten_portf_rf_10yrs <- sum(mean_ten_portf_rf_10yrs_xts)/10
mean_betas_ten_portf_10yrs <- sum(betas_ten_portf_rf_10yrs)/10
plot.default(x = mean_betas_ten_portf_10yrs, xlim=c(0, 2),
             y = com_mean_ten_portf_rf_10yrs, ylim=c(0, 2),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations 10 Portfolios 1964-1974",
             abline(0, y_mkt_10yrs))

#2000-2019
com_mean_ten_portf_rf_2000 <- sum(mean_ten_portf_rf_2000_xts)/10
mean_betas_ten_portf_2000 <- sum(betas_ten_portf_rf_2000)/10
plot.default(x = mean_betas_ten_portf_2000, xlim=c(0, 2),
             y = com_mean_ten_portf_rf_2000, ylim=c(0, 2),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations 10 Portfolios 2000-2019",
             abline(0, y_mkt_2000))

```

```{r, echo=FALSE}

#SML-Function
calc_residual <- function(x) {y <- y_mkt*x}
calc_residual_10yrs <- function(x) {y <- y_mkt_10yrs*x}
calc_residual_2000 <- function(x) {y <- y_mkt_2000*x}
residual_1964_2019 <- as.data.frame((com_mean_ten_portf_rf - calc_residual(mean_betas_ten_portf))^2)
residual_1964_1974 <- as.data.frame((com_mean_ten_portf_rf_10yrs - calc_residual_10yrs(mean_betas_ten_portf_10yrs))^2)
residual_2000_2019 <- as.data.frame((com_mean_ten_portf_rf_2000 - calc_residual_2000(mean_betas_ten_portf_2000))^2)
joined_residuals <- merge(residual_1964_2018[1, 1], residual_1964_1974[1, 1])
joined_residuals <- merge(joined_residuals, residual_2000_2018)
Residuals_different_timeperiods <- joined_residuals %>% 
  dplyr::rename("Residual 2000-2019" = "(com_mean_ten_portf_rf_2000 - calc_residual_2000(mean_betas_ten_portf_2000))^2") %>% dplyr::rename("Residual 1964-2008" = "x") %>% dplyr::rename("Residual 1964-1974" = "y")
Residuals_different_timeperiods
```


d)	Try again with 25 portfolios sorted on size and beta. What do you find? Is that interesting? 
  
  ```{r}
#join data
twentyfive_portf <- portf_mkt_beta[1:672, -c(7:16)]
twentyfive_portf_joined <- left_join(mkt_factors, twentyfive_portf)
```

```{r, echo=FALSE}

twentyfive_portf_joined <- twentyfive_portf_joined <- twentyfive_portf_joined%>%
  dplyr::rename("Lo20" = "Lo 20") %>%
  dplyr::rename("Qnt2" = "Qnt 2") %>%
  dplyr::rename("Qnt3" = "Qnt 3") %>%
  dplyr::rename("Qnt4" = "Qnt 4") %>%
  dplyr::rename("Hi20" = "Hi 20")
````

```{r}
#substract Risk-Free-Rate
twentyfive_portf_rf <- mutate(twentyfive_portf_joined, Lo20rf = Lo20 - RF, Qnt2rf = Qnt2 - RF, Qnt3rf = Qnt3 - RF, Qnt4rf = Qnt4 - RF, Hi20rf = Hi20 - RF)
twentyfive_portf_rf <- twentyfive_portf_rf[-2:-10]

```

```{r, echo=FALSE}
#substract Risk-Free-Rate
twentyfive_portf_rf <- mutate(twentyfive_portf_joined, Lo20rf = Lo20 - RF, Qnt2rf = Qnt2 - RF, Qnt3rf = Qnt3 - RF, Qnt4rf = Qnt4 - RF, Hi20rf = Hi20 - RF)
twentyfive_portf_rf <- twentyfive_portf_rf[-2:-10]


#Create XTS
twentyfive_portf_rf_xts <- twentyfive_portf_rf %>%
  tk_xts(date_var = date, silent = TRUE)

#Calculate Betas for each portfolio
betas_twentyfive_portf <- CAPM.beta(Ra = twentyfive_portf_rf_xts, Rb = mkt_factors_xts[, 1], Rf = 0)

#Estimate Mean Return
mean_twentyfive_portf_rf_xts <- as.data.frame(lapply(twentyfive_portf_rf_xts, FUN=mean))

#Plot the return/beta-combinations
plot.default(x = betas_twentyfive_portf, xlim=c(0, 2),
             y = mean_twentyfive_portf_rf_xts, ylim=c(0, 1),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations 25",
             abline(0, y_mkt))

#We now do it with the mean return of every portfolio combined...
com_mean_twentyfive_portf_rf <- sum(mean_twentyfive_portf_rf_xts)/5
# and the beta
mean_betas_twentyfive_portf <- sum(betas_twentyfive_portf)/5

plot.default(x = mean_betas_ten_portf, xlim=c(0, 2),
             y = com_mean_ten_portf_rf, ylim=c(0, 1),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations Portfolio Summary 25",
             abline(0, y_mkt))
plot.default(x = mean_betas_ten_portf, xlim=c(0, 2),
             y = com_mean_ten_portf_rf, ylim=c(0, 1),
             xlab = "Beta", ylab = "Mean Return",
             main = "Return/Beta-combinations Portfolio Summary 10",
             abline(0, y_mkt))

```








# Exercise 3: Statistical Factor Models

Follow the file [sfmVignette.pdf](https://github.com/braverock/FactorAnalytics/blob/master/vignettes/sfmVignette.pdf) and interpret your results.

```{r}
SP500 <- tq_index("SP500")
NASDAQ <- tq_exchange("NASDAQ")
NYSE <- tq_exchange("NYSE") 
stocks.selection <- SP500 %>% 
  inner_join(rbind(NYSE,NASDAQ) %>% select(symbol,last.sale.price,market.cap,ipo.year),by=c("symbol")) %>%
  filter(ipo.year<2000&!is.na(market.cap)) %>% 
  arrange(desc(weight)) %>% 
  slice(1:10)
stocks.selection
```

These are the returns of the selected stocks.

```{r}
stocks.returns <- stocks.selection$symbol %>%
  tq_get(get  = "stock.prices",
         from = "2000-01-01",
         to   = "2019-12-31") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly")
stocks.returns
```

These are the stocks return in the xts format and also in a wide format
````{r}
stocks.returns.xts <- stocks.returns%>%
  subset( select = c(symbol,date, monthly.returns)) %>%
  pivot_wider(names_from = symbol, 
              values_from = monthly.returns) %>% 
  tk_xts(date_var = date, silent = TRUE)
colnames(stocks.returns.xts)
```

### Fit a statistical factor model fitSfm

Principal Component Analysis (PCA): uses the eigen decomposition of the covariance matrix of asset returns to find the first K principal components that explain the largest portion of the sample covariance matrix returns. Factor loading are then estimated using time series regression. Foctor analysis involves maximum likelihood optimization to estimate the factor loadings and the residual covarince matrix, constructing the factor realization and choosing a rotation of the coordinate system for a more meeaningful interpretion of factors

used when T>N
T: Number of observations
N: Number of assets

if N>T then Aymptotic Principal Component Analysis (APCA)

#### Principal Component Analysis
Fitting a statistical factor model with two principal components (k=2)
```{r}
fit.pca <- fitSfm(stocks.returns.xts, k=2)
fit.pca 
```
Screenplot of eigenvalues
An eigenvector of a linear transformation is a nonzero vector that changes by a scalar factor when that linear transformation is applied to it. Eigenvalues and eigenvectors allow us to "reduce" a linear operation to separate, simpler, problems.
```{r}
plot(fit.pca, which=1, eig.max = 0.9)

```

First principal component explains about 48% of total variance. The first two components explain about 61% of total variance.

Now plotting the estimated factor returns
```{r}
plot(fit.pca, which=2)
```

Estimated factor loadings for all assets
Factor loading is the correlation coefficient for the variable and factor.
```{r}

plot(fit.pca, which=3, a.sub=1:10)

```
First factor has all positive loadings, whereas the second factor has both positive and negative loadings.


```{r}
t(fit.pca$mimic)

plot(fit.pca, which=12, n.top=3)
```
This figure displays the top three assets with the largest and smalles weights in each factor mimicking portfolio. For the first factor, NVIDA, Amazone and Adobe have the highest weights and Amgen, UPS and Microsoft have the lowest weights. Since all weights are positive this might be construed as a market-wide factor. For the second factor, Amazon, Qualcom and Cisco have the highest weights and NVIDA, Apple and Adobe have the lowest weights.

Now plotting the correlations between assets with the top 3 largest and smallest positions in the F.1 factor mimicking portfolio

```{r}
plot(fit.pca, which=13, f.sub=1, n.top=3)
```
Here we can see the correlations between assets with top 3 largest and smallest weight in the factor mimicking portfolio for the first principal component. Correlations are very different.


```{r}
plot(fit.pca, which=13, f.sub=2, n.top=3)
```
Here we can see the correlations between assets with top 3 largest and smallest weight in the factor mimicking portfolio for the first principal component. Pretty high correlations overall.





#### S3 generic methods
all estimaded coefficients from PCA including intercept
```{r}
coef(fit.pca)
```

compare returns data with fitted and residual values for AAPL: fit.pca
```{r}
AAPL.ts <- merge(fit.pca$data[,1], fitted(fit.pca)[,1], residuals(fit.pca)[,1])

colnames(AAPL.ts) <- c("AAPL.return", "AAPL.fitted", "AAPL.residual")

tail(AAPL.ts)
```

fitted(): returns an xts data object of the component of asset returns explained by the factor model

residuals(): returns xts data object with the component of asset returns not explained by the factor model

Summary for fit.pca with HAC standard errors (allows for heteroskedasticity and autocorrelation consistent estimates and standard errors)
```{r}
sum.pca <- summary(fit.pca, se.type="HAC", n.top=3)

sum.pca$sum.list[[1]]
```
factor mimicking portfolio weights
```{r}
sum.pca$mimic.sum
```

### Factor Model Covariance and Risk Decomposition

#### Factor model covariance

```{r}
Omega <- fmCov(fit.pca)
# return correlation plot for all assets
plot(fit.pca, which=8, a.sub=1:10)
```

#### Standard deviation decomposition
```{r}
decomp <- fmSdDecomp(fit.pca)

#get the factor model standard deviation for all assets
decomp$Sd.fm

#get the component contribution to Sd
head(decomp$cSd)

#plotting
plot(fit.pca, which=9, f.sub=1:2, a.sub=1:10)
```

#### Value-at-Risk decomposition
```{r}
decomp1 <- fmVaRDecomp(fit.pca)

#factor model Value-at-Risk
head(decomp1$VaR.fm)

#Marginal factor contributions to VAR
head(decomp1$mVaR)

# plotting
plot(fit.pca, which=11, f.sub=1:2, a.sub=1:10)
```


####Expected Shorfall decomposition
```{r}
decomp2 <- fmEsDecomp(fit.pca)

# factor model Expected Shortfall
head(decomp2$ES.fm)

# percentage component contribution to ES
head(decomp2$pcES)

# plotting
plot(fit.pca, which = 10, f.sub=1:2, a.sub=1:10)
```


### 4. Market plus Industry plus Country Model 

In this discussion we treat the terms “Industry” and “Sector” interchangeably, noting that for some models, e.g., a U.S. equity model, one may prefer to just use sector factors but may also wish to use industry factors, and a global model with countries may also contain industry factors. Our current examples use sector factors but we refer to them in our mathematical models loosely as industry factors. 


We will illustrate use of fitFfm to fit a market plus sector model to the DJIA stock returns and sector data. But first we fit a pure sector model without a market component and examine the factor return coefficients for the first month of the five-year fitting window as a reference point. 

```{r} 

dat = factorDataSetDjia5Yrs 
fitSec = fitFfm(dat,
                asset.var = "TICKER",
                ret.var = "RETURN", 
                date.var = "DATE",
                exposure.vars = "SECTOR") 

``` 

```{r} 

round(coef(summary(fitSec)$sum.list[[1]])[, 1], 3) 
round(fitSec$factor.returns[1, ], 3) 

``` 

Note that the last two lines of code produce identical results. This is because without any constraints such as those discussed above, the coefficients of the cross-section regression at each time period are extracted to form the time series of factor returns in the factor.returns component of the ffm object. Now we fit a market plus sector model by adding the fitF argument addIntercept = T,and examine the coefficients gˆmi,1 and the resulting factor returns ˆfmi,1 for the first month of the fiveyear fitting window. 

```{r} 

fitSecInt = fitFfm(dat,
                   asset.var = "TICKER",
                   ret.var = "RETURN", 
                   date.var = "DATE",
                   exposure.vars = "SECTOR",
                   addIntercept = T) 

round(coef(summary(fitSecInt)$sum.list[[1]])[, 1], 2) 
#excactly the same as before, but we add intercept t; therefore we add the market component
``` 

```{r} 

round(fitSecInt$factor.returns[1, ], 2) 

``` 

```{r} 

round(sum(fitSecInt$factor.returns[1, -1]), 2) 
# Summed returns of all sector should equal to zero, due to the relationship of the sectors
``` 

Note that the next to last line of code above prints the unique least squares model coefficients vector gˆmi,1 for month 1 (9 of them since there are 9 sectors) 


### 5. A Simultated Data Example 

We have created an artificial example of a market+sector+country model (where sector plays the role of industry) consisting of random returns of 30 stocks with three sectors for the sector factor and two countries for the countries factor, for each of five months. The normally distributed returns for the three sectors alone have means of 1, 2, 3, with standard deviations .2. The two countries contribute additional normally distributed returns having means 4 and 5 with standard deviations .2. So returns associated with the first country have means 5, 6, 7 and means associated with the second 
country have means 6, 7, 8. Thus the overall mean of 6.5. The code for creating the returns is as follows: 
  
  ```{r} 

# Country Incremental Components of Asset Returns 
set.seed(10000)

Bind = cbind(rep(1, 30), 
             c(rep(1, 10), rep(0, 20)), 
             c(rep(0, 10), rep(1, 10), rep(0, 10)), 
             c(rep(0, 20), rep(1, 10))) 

cty1 = matrix(rep(c(0, 1), 15)) 

cty2 = matrix(rep(c(1, 0), 15)) 

Bmic = cbind(Bind, cty1, cty2) 

dimnames(Bmic)[[2]] = c("mkt", "sec1", "sec2", "sec3", "cty1", "cty2") 

r.add = rnorm(30, 4, 0.2) 
r.cty1 = rep(0, 30) 
r.cty2 = rep(0, 30) 

for (i in 1:30) 
{
  if (Bmic[i, "cty1"] == 1) 
  { 
    r.cty1[i] = r.add[i] 
    r.cty2[i] = 0 
  } 
  else 
  { 
    r.cty1[i] = 0
    r.cty2[i] = r.add[i] + 1
  } 
} 


# Asset Returns for Market+Industry+Country Model 
mu = c(1, 2, 3) 
sd = c(0.2, 0.2, 0.2) 
r = list() 
r.mic = list() 
fitMic = list() 
fitMic1 = list() 

for (i in 1:5) 
{
  set.seed(1099923 + (i - 1)) 
  r[[i]] = c(rnorm(10, mu[1], sd[1]), 
             rnorm(10, mu[2], sd[2]), 
             rnorm(10, mu[3], sd[3])) 
  r.mic[[i]] = r[[i]] + r.cty1 + r.cty2 
} 

``` 

```{r} 

#qq-plot of the 30 asset returns for the first of the 5 time periods 
#Die Beobachtungswerte zweier Merkmale, deren Verteilung man vergleichen will, werden jeweils der Größe nach geordnet. Diese geordneten Daten werden zu Wertepaaren zusammengefasst und in einem Koordinatensystem abgetragen. Ergeben die Punkte (annähernd) eine Gerade, kann man vermuten, dass den beiden Merkmalen die gleiche Verteilung zu Grunde liegt. Problematisch ist das Verfahren, wenn von den beiden Merkmalen unterschiedlich viele Beobachtungen vorliegen. Hier kann mit Interpolationsverfahren abgeholfen werden.
qqnorm(r.mic[[1]],
       main = "MIC Model Equity Returns for First Period", 
       xlab = "NORMAL QQ-PLOT",
       ylab = "RETURNS") 

``` 

Now we build the data frame required by fitFfm, fit the MIC model and display the factor returns for each of the five months. What we have been calling the Industry factor is called Sector for this example 


```{r} 

Returns = unlist(r.mic) 

COUNTRY = rep(rep(c("US", "India"), 15), 5) 

SECTOR = rep(rep(c("SEC1", "SEC2", "SEC3"), each = 10), 5) 

TICKER = rep(c(LETTERS[1:26], paste0("A", LETTERS[1:4])), 5) 

DATE = rep(seq(as.Date("2000/1/1"), by = "month", length.out = 5), each = 30) 

data.mic = data.frame(DATE = as.character(DATE),
                      TICKER, 
                      Returns, 
                      SECTOR, 
                      COUNTRY) 

exposure.vars = c("SECTOR", "COUNTRY") 

fit = fitFfm(data = data.mic,
             asset.var = "TICKER", 
             ret.var = "Returns",
             date.var = "DATE",
             exposure.vars = exposure.vars, 
             addIntercept = T) 

fit$factor.returns 


``` 

We see that the Market values of the factor have values clustering around 6.5 as expected. We can also see that the three sector factor returns sum to zero and the two country factor returns sum to zero, as expected due to the constraints that they sum to zero.

