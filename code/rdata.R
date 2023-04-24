#libraries
install.packages("forecast")
library(forecast)
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("corrplot")
library(corrplot)
install.packages("lubridate")
library(lubridate)
install.packages("car")
library(car)
install.packages("nlme")
library(nlme)
install.packages("seasonal")
library(seasonal)
install.packages("zoo")
library(zoo)
install.packages("plotly")
library(plotly)
install.packages("ggplot2")
library(ggplot2)
install.packages("mediation")
library(mediation)
install.packages("gvlma")
library(gvlma)
install.packages("gridExtra")
library(gridExtra)
install.packages("stats")
library(stats)
install.packages("xgboost")
library(xgboost)
install.packages("ncvreg")
library(ncvreg)
install.packages("Metrics")
library(Metrics)


#data
m2sl <- read.csv("D:/Data science/ADE_sem_I/analiza regresji/data/M2SL.csv")
sp500daily <- read.csv("D:/Data science/ADE_sem_I/analiza regresji/data/SP500_daily.csv", sep=";")
sp500monthly <- read.csv("D:/Data science/ADE_sem_I/analiza regresji/data/SP500_monthly.csv")
wm2ns <- read.csv("D:/Data science/ADE_sem_I/analiza regresji/data/WM2NS.csv", header=TRUE)  
m2ns <- read.csv("D:/Data science/ADE_sem_I/analiza regresji/data/M2NS.csv")
indpro <- read.csv("D:/Data science/ADE_sem_I/analiza regresji/data/indpor.csv")
gdp <- read.csv("D:/Data science/ADE_sem_I/analiza regresji/data/gdp.csv")
gasregw <- read.csv("D:/Data science/ADE_sem_I/analiza regresji/data/gasregw.csv")
gasregm <- read.csv("D:/Data science/ADE_sem_I/analiza regresji/data/mgasreg.csv")
fedfunds <- read.csv("D:/Data science/ADE_sem_I/analiza regresji/data/fedfunds.csv")
gasgulfm <- read.csv("D:/Data science/ADE_sem_I/analiza regresji/data/mgasgulf.csv")
gasgulfw <- read.csv("D:/Data science/ADE_sem_I/analiza regresji/data/wgasgulf.csv")
usdxm <- read.csv("D:/Data science/ADE_sem_I/analiza regresji/data/usdxm.csv")
usdxw <- read.csv("D:/Data science/ADE_sem_I/analiza regresji/data/usdxw.csv")

#transformations
names(m2sl) <- tolower(names(m2sl))
names(sp500daily) <- tolower(names(sp500daily))
names(sp500monthly) <- tolower(names(sp500monthly))
names(wm2ns) <- tolower(names(wm2ns))
names(fedfunds) <- tolower(names(fedfunds))
names(gasregw) <- tolower(names(gasregw))
names(gasregm) <- tolower(names(gasregm))
names(gdp) <- tolower(names(gdp))
names(indpro) <- tolower(names(indpro))
names(usdxm) <- tolower(names(usdxm))
names(usdxw) <- tolower(names(usdxw))
usdxm <- usdxm  |> transmute(date=date,usdxm_close=usdxm$adj.close)
usdxw <- usdxw |> transmute(date=date,usdxw_close=usdxw$adj.close)
names(gasgulfm) <- tolower(names(gasgulfm))
names(gasgulfw) <- tolower(names(gasgulfw))
names(m2ns) <- tolower(names(m2ns))
gasgulfw <- gasgulfw |> transmute(gasgulfw=wgasusgulf,date)

indpro$date <- as_date(indpro$date)
usdxm$date <- as_date(usdxm$date)
usdxw$date <- as_date(usdxw$date)
gasgulfm$date <- as_date(gasgulfm$date)
gasgulfw$date <- as_date(gasgulfw$date)
fedfunds$date <- as_date(fedfunds$date)
gdp$date <- as_date(gdp$date)
gasregw$date <- as_date(gasregw$date)
gasregm$date <- as_date(gasregm$date)
gasregw$gasregw <- as.numeric(gasregw$gasregw)
wm2ns$wm2ns <- as_date(wm2ns$wm2ns)

#creating list of dataframes and new names


list_df <- list(fedfunds,gasgulfm,gasgulfw,gasregm,gasregw,gdp,indpro,usdxm,m2ns,m2sl,usdxw,wm2ns)
nazwy <- c("xfedfunds","xgasgulfm","xgasgulfw","xgasregm","xgasregw","xgdp","xindpro","xusdxm","xm2ns","xm2sl","xusdxw","xwm2ns")

df_datefilter <- lapply(list_df,datefilter)
names(df_datefilter) <- nazwy
list2env(df_datefilter, envir = .GlobalEnv)
xgasregm$gasregm <- as.numeric(xgasregm$gasregm)
xgasregw$gasregw <- as.numeric(xgasregw$gasregw)



fedfunds_adj_object <- Deseasonal(data = xfedfunds,
                                  col_date = "date",
                                  col_value = "fedfunds",
                                  frequency = 12,
                                  start = c(2000, 1),
                                  ylab = "FFER",
                                  title = "Federal Funds Effective Rate over time monthly ")
gasgulfm_adj_object <- Deseasonal(data = xgasgulfm,
                                  col_date = "date",
                                  col_value = "mgasusgulf",
                                  frequency = 12,
                                  start = c(2000, 1),
                                  ylab = "Gasoline",
                                  title = "Conventional Gasoline Prices: Dollars per Gallon monthly over time")
gasgulfw_adj_object <- Deseasonal(data = xgasgulfw,
                                  col_date = "date",
                                  col_value = "gasgulfw",
                                  frequency = 52,
                                  start = c(2000, 1),
                                  ylab = "Gasoline",
                                  title = "Conventional Gasoline Prices: Dollars per Gallon weekly over time")


gasregm_adj_object <- Deseasonal(data = xgasregm,
                                 col_date = "date",
                                 col_value = "gasregm",
                                 frequency = 12,
                                 start = c(2000, 1),
                                 ylab = "Gas",
                                 title = "US Regular Gas Price: Dollars per Gallon monthly over time")


gasregw_adj_object <- Deseasonal(data = xgasregw,
                                 col_date = "date",
                                 col_value = "gasregw",
                                 frequency = 52,
                                 start = c(2000, 1),
                                 ylab = "Gas",
                                 title = "US RegularGas Price: Dollars per Gallon weekly over time")
wm2ns_adj_object <- Deseasonal(data = xwm2ns,
                               col_date = "date",
                               col_value = "wm2ns",
                               frequency = 52,
                               start = c(2000, 1),
                               ylab = "M2",
                               title = "M2 indicator: Billions of Dollars weekly over time")

fedfundsplot <- fedfunds_adj_object$after_plot
gasgulfmplot <- gasgulfm_adj_object$after_plot
gasgulfwplot <- gasgulfw_adj_object$after_plot
gasregwplot <- gasregw_adj_object$after_plot
gasregmplot <- gasregm_adj_object$after_plot
wm2nsplot <- wm2ns_adj_object$after_plot
grid.arrange(fedfundsplot,wm2nsplot,gasgulfmplot,gasgulfwplot,gasregwplot,gasregmplot)


asfedfunds <- fedfunds_adj_object$adjusted_data
gasgulfm <- gasgulfm_adj_object$adjusted_data
gasgulfw <- gasgulfw_adj_object$adjusted_data
gasregw <- gasregw_adj_object$adjusted_data
gasregm <- gasregm_adj_object$adjusted_data
wm2ns <- wm2ns_adj_object$adjusted_data

#creating monthly data frame with all indicators
sp500monthly$date <- as_date(sp500monthly$date)
indpro$date <- as_date(indpro$date)
usdxm$date <- as.Date(usdxm$date)
usdxw$date <- as.Date(usdxw$date)
gasgulfm$date <- as_date(gasgulfm$date)
gasgulfm <- gasgulfm |> transmute(date=as_date(date),gasgulfm=as.numeric(col_value_adj))
gasgulfw$date <- as_date(gasgulfw$date)
fedfunds$date <- as_date(fedfunds$date)
gdp$date <- as_date(gdp$date)
gasregw$date <- as_date(gasregw$date)
gasregm$date <- as_date(gasregm$date)
gasregw$gasregw <- as.numeric(gasregw$col_value_adj)
wm2ns$date <- as_date(wm2ns$date)
m2sl$date <- as_date(m2sl$date)

spm <- merge(sp500monthly,m2sl,by="date",all.x = TRUE)
spm <- merge(spm,gasgulfm,by="date",all.x = TRUE)
spm <- merge(spm,gasregm,by="date",all.x = TRUE)
spm <- merge(spm,usdxm,by="date",all.x = TRUE)
spm <- merge(spm,indpro,by="date",all.x = TRUE)
spm <- merge(spm,fedfunds,by="date",all.x = TRUE)



spm <- spm |> mutate(quarter=as.numeric(quarter(date)),year=year(date),date=as_date(date))
gdp <- gdp|> mutate(date=as_date(date),quarter=as.numeric(quarter(date)),year=year(date))
spm <- left_join(by = c("quarter","year"),x = spm,y=gdp)

spm <- spm |> transmute(date=as_date(date.x),
                        adj_close,
                        volume_in_mln,
                        m2sl,
                        gasgulfm,
                        gasregm=col_value_adj,
                        usdxm_close,
                        indpro,
                        fedfunds,
                        gasregm,
                        gdp)

#creating weekly data frame with all indicators
sp500daily <- sp500daily |> mutate(date=as_date(date))
sp500weekly <- aggregate(sp500daily[, -1], by = list(date = cut(as.Date(sp500daily$date), breaks = "1 week")), mean) |> mutate(quarter=as.numeric(quarter(date)),year=year(date),date=as_date(date),week=week(date),month=month(date))

sp500daily <- sp500daily |> mutate(year=year(date),week=week(date))

wm2ns <- wm2ns |> transmute(wm2ns,date=as_date(date))
spw <- merge(sp500weekly,wm2ns,by="date",all.x = TRUE)

gasregw <- gasregw|> transmute(gasregw=as.numeric(gasregw),date=as_date(date))
spw <- merge(spw,gasregw,by="date",all.x = TRUE)




gasgulfw <- gasgulfw |> mutate(quarter=as.numeric(quarter(date)),year=year(date),date=as_date(date),week=week(date),month=month(date))
spw <- left_join(by = c("year","week","quarter"),x = spw,y=gasgulfw)

usdxw <- usdxw |> mutate(quarter=as.numeric(quarter(date)),year=year(date),date=as_date(date),week=week(date))
spw <- left_join(by = c("quarter","year","week"),x = spw,y=usdxw)

spw <- spw |> transmute(date=date.x,
                        adj_close,
                        volume_in_mln,
                        wm2ns=col_value_adj.x,
                        gasregw=gasregw,
                        gasgulfw=col_value_adj.y,
                        usdxw_close,
                        quarter,year,
                        month=month.x)

fedfunds <- fedfunds |> mutate(quarter=as.numeric(quarter(date)),year=year(date),date=as_date(date),week=week(date),month=month(date)) |> filter(year>=2000)
fedfunds 
spw <- left_join(by = c("quarter","year","month"),x = spw,y=fedfunds)

indpro <- indpro |> mutate(quarter=as.numeric(quarter(date)),year=year(date),date=as_date(date),week=week(date),month=month(date)) |> filter(year>=2000)
fedfunds 
spw <- left_join(by = c("quarter","year","month"),x = spw,y=indpro)


gdp <- gdp |> mutate(quarter=as.numeric(quarter(date)),year=year(date),date=as_date(date),week=week(date),month=month(date)) |> filter(year>=2000)
fedfunds 
spw <- left_join(by = c("quarter","year"),x = spw,y=gdp)

spw <- spw |> transmute(date=date.x,adj_close,volume_in_mln,wm2ns,gasregw,gasgulfw,usdxw_close,fedfunds,gdp,indpro)



save(spw, file = "spw.Rdata")
save(spm, file = "spm.Rdata")
histogram_f(spm)

lmmlog <- lm(adj_close~volume_in_mln+m2sl+fedfunds+indpro+gasregm+gdp+usdxm_close+gasgulfm,
             data=spm)
summary(lmmlog)
vif(lmmlog)

lmmlog <- lm(adj_close~volume_in_mln+m2sl+fedfunds+gasregm+gdp+usdxm_close+gasgulfm,
             data=spm)
summary(lmmlog)
vif(lmmlog)

lmmlog <- lm(adj_close~volume_in_mln+m2sl+fedfunds+gdp+usdxm_close+gasgulfm,
             data=spm)
summary(lmmlog)
vif(lmmlog)

lmmlog <- lm(adj_close~volume_in_mln+m2sl+fedfunds+indpro+usdxm_close+gasgulfm,
             data=spm)
summary(lmmlog)
vif(lmmlog)

lmmlog <- lm(adj_close~volume_in_mln+m2sl+fedfunds+indpro+usdxm_close+gasregm,
             data=spm)
summary(lmmlog)
vif(lmmlog)

lmmlog <- lm(adj_close~volume_in_mln+m2sl+fedfunds+indpro+gasregm,
             data=spm)
summary(lmmlog)
vif(lmmlog)

lmmlog <- lm(log(adj_close)~log(volume_in_mln)+log(m2sl)+log(fedfunds)+gasregm,
             data=spm)
summary(lmmlog)
vif(lmmlog)


spm1 <- spm |> dplyr::select(-date)
spm1 <- na.omit(spm1)
pca <- prcomp(spm1,center = TRUE,scale. = TRUE)
summary(pca)


spw1 <- spw |> dplyr::select(-date)
spw1 <- na.omit(spw1)
pca <- prcomp(spw1,center = TRUE,scale. = TRUE)
summary(pca)


#fitting
set.seed(123)
lm <- modelling(spm, spw, "lm")
xgboost <- modelling(spm, spw, "xgboost")
lasso <- modelling(spm, spw, "lasso")

#data frame that shows what rmse for what model
data.frame(lm = c(lm$rmse_monthly, lm$rmse_weekly),
           xgboost = c(xgboost$rmse_monthly, xgboost$rmse_weekly),
           lasso = c(lasso$rmse_monthly, lasso$rmse_weekly))


spm <- na.omit(spm)
spw <- na.omit(spw)
#arima for spm
spm_arima <- spm |> dplyr::select(adj_close) |>ts(frequency = 12)

xregspm <- spm |> dplyr::select(-date,-adj_close,-gdp,-gasgulfm,-indpro,usdxm_close)
xregspm <- as.matrix(xregspm)

model <- auto.arima(spm_arima,xreg = xregspm,d=1,D=1,
                    stepwise = FALSE,
                    approximation = FALSE,
                    trace = TRUE)

arimasum <- summary(model)
arimasum

#linear regression
model_lm <- lm(adj_close~volume_in_mln + m2sl + gasregm + fedfunds + usdxm_close + indpro, data = spm)
y_predicted <- predict(model_lm, newdata = spm %>% dplyr::select(c(-adj_close, -gasgulfm, -gdp, -date)))
y_expected <-spm$adj_close
#blad sredniej kwadratowej dla regresji liniowej
rmse_m <- rmse(y_expected, y_predicted)
rmse_m
summary(model)