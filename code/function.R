## Internal functions used during the analysis

Deseasonal <- function(data,
                       col_date,
                       col_value,
                       frequency,
                       start,
                       ylab,
                       title) {
  data$date <- as_date(data$date)
  data <- data %>% filter(date >= '2000-01-01')
  
  plot_before_removing <- ggplot(data = data, aes_string(x = col_date, y = col_value)) +
    geom_area(fill="#69b3a1", alpha=0.5) +
    geom_line(color = "#69b3a1") +
    xlab("Date (monthly)") +
    ylab(ylab) +
    ggtitle(title)
  
  # create a ts object
  ts_data <- ts(data[,col_value], frequency = frequency, start = start)
  
  # decompose the time series into its trend, seasonal, and random components using stl()
  ts_decomp <- decompose(ts_data)
  
  # plot the seasonal component
  #plot(ts_decomp$time.series[, "seasonal"], xlab = "Year", ylab = "Seasonal",
  #    main = "Seasonal component")
  
  
  # remove the seasonal component from the original time series
  #indpro_adj <- ts_data - ts_decomp$time.series[, "seasonal"]
  col_value_adj <- seasadj(ts_decomp)
  
  data_adj <- data_frame(date = data[,col_date], col_value_adj = col_value_adj)
  
  # Time series plot after removing seasonality
  plot_after_removing <- ggplot(data = data_adj, aes(x = date, y = col_value_adj)) +
    geom_area(fill="#69b3a2", alpha=0.5) +
    geom_line(color = "#69b3a2") +
    xlab("year") +
    ylab(ylab) +
    ggtitle(title)
  
  list(before_plot = plot_before_removing,
       after_plot = plot_after_removing,
       ts_decomp = ts_decomp,
       adjusted_data = data_adj)
  
}


datefilter <- function(data){
  data |> mutate(date=as_date(date),week=week(date),
                 month=month(date),
                 quarter=quarter(date),
                 year=year(date)) |> 
    filter(year>=2000)
  return(data)
}


#histogram functions for checking indicator's distributions
histogram_f <- function(dataframe) {
  for (col in names(dataframe)) {
    p <- ggplot(dataframe, aes_string(col)) + geom_histogram()
    print(p)
  }
}

histogram_flog <- function(dataframe) {
  for (col in names(dataframe)) {
    p <- ggplot(dataframe, aes_string(x = col)) +
      scale_x_continuous(trans = "log10") +  # add log transformation to x-axis
      geom_histogram()
    print(p)
  }
}

potezna_analiza <- function(
    data, #ramka danych
    num, #liczba okresow dla ktorych chemy prognozowac
    prognosed,#wartosci chcemy prognozwac zapis"nazwa_kolu
    use_reg,
    NAremoval,
    year,
    freq
){
  
  
  
  
  #orka
  NAremoval
  if (NAremoval==TRUE) {data <- data |> na.omit()
  }
  
  col <- as.data.frame(data[, (names(data) %in% prognosed)]) |>
    ts(start = year,frequency = freq)
  
  #msdse=mean,st
  funkcje <- list(
    function(data) {cbind(mean = colMeans(data),
                          SE = unlist(sapply(data, function(x) {
                            sd(x) / sqrt(length(x))
                          })),
                          sd = unlist(sapply(data, sd)))}
  )
  msdse <- lapply(funkcje, function(f) { f(data) })
  msdse[] <- lapply(msdse, format, scientific=FALSE)
  
  #conditional regx
  use_reg
  if(use_reg==TRUE){
    regx <- as.data.frame(data[, (!names(data) %in% prognosed)],rownames.force=NA)
    regx <- regx |> as.matrix(rownames.force = NA)
    
    #tworzenie modelu i prognozy    
    arima <- auto.arima(col,
                        xreg=regx,
                        d=1,D=1,
                        stepwise = FALSE,
                        approximation = FALSE,
                        trace = TRUE
    )
    fcst <- forecast(arima,
                     xreg=regx,
                     h=num)}
  else{
    arima <- auto.arima(col,
                        d=1,D=1,
                        stepwise = FALSE,
                        approximation = FALSE,
                        trace = TRUE
    )
    fcst <- forecast(arima,
                     h=num)
    
  }
  
  
  
  
  
  #wykres w plotly
  plot <- plot_ly()
  plot <- add_lines(plot, x = time(fcst$mean), y = fcst$mean,
                    name = "Forecast")
  plot <- add_lines(plot,
                    x = time(col), y = col,
                    name = "Actual")
  
  #korelacja wartosci liczbowe plus wykres
  cor <- cor(data) |> corrplot(
    method = "square", order = "hclust",
    addCoef.col = "black",
    type = "upper",
    diag = FALSE,
    tl.col = "black")
  
  #wykresy plotly dla poszczególnych kolumn
  plotly <- for (col_name in colnames(data)) {
    fig <- plot_ly(data,
                   x = ~seq_along(data[[col_name]]),
                   y = ~data[[col_name]],
                   type = "scatter",
                   mode = "lines",
                   name = col_name)
    fig <- fig %>% layout(title = col_name)
    fig %>% print()
  }
  
  #wykres plus zsumowanie elementow prognozy
  autoplot <- autoplot(fcst)
  summary <- summary(fcst)
  
  #tutaj wpisuje jakie dane ma mi zwrocic
  result_list <- list(
    plot=plot,
    plotly=plotly,
    cor=cor,
    autoplot=autoplot,
    summary=summary,
    arima=arima,
    fcst=fcst,
    msdse=msdse
    
    
    
  )
  return(result_list)
  
}

modelling <- function(data_monthly, data_weekly, model) {
  
  data_monthly <- data_monthly %>% na.omit(data_monthly)
  data_weekly <- data_weekly %>% na.omit(data_weekly)
  
  n_data <- nrow(data_monthly)
  
  #train_idxm <- sample.int(n = .8 * n_data, size = .8 * n_data)
  spm_train <- data_monthly %>% filter(date < '2019-01-01')
  spm_test <- data_monthly %>% filter(date >= '2019-01-01')
  
  ### SPLITTING DATA - weekly
  n_data <- nrow(data_weekly)
  
  #train_idxw <- sample.int(n = .8 * n_data, size = .8 * n_data)
  spw_train <- data_weekly %>% filter(date < '2019-01-01')
  spw_test <- data_weekly %>% filter(date >= '2019-01-01') #%>% na.omit()
  
  df_train_yw <- spw_train %>% dplyr::select("adj_close")
  df_test_yw <- spw_test %>% dplyr::select("adj_close")
  
  df_train_w <- model.matrix(adj_close ~ . - 1 - date - gasgulfw - gdp, spw_train)
  df_test_w <- model.matrix(adj_close ~ . - 1 - date - gasgulfw - gdp, spw_test)
  df_train_ym <- spm_train %>% dplyr::select("adj_close")
  df_test_ym <- spm_test %>% dplyr::select("adj_close")
  
  df_train_m <- model.matrix(adj_close ~ . - 1 - date - gasgulfm - gdp, spm_train)
  df_test_m <- model.matrix(adj_close ~ . - 1 - date - gasgulfm - gdp, spm_test)
  
  if (model == "xgboost") {
    
    train_pool <- xgb.DMatrix(data = df_train_w, label = df_train_yw$adj_close)
    test_pool <- xgb.DMatrix(data = df_test_w, label = df_test_yw$adj_close)
    
    watchlist <- list(train = train_pool, test = test_pool)
    
    model_weekly <- xgb.train(
      watchlist = watchlist, 
      nrounds = 50000, 
      data = train_pool,
      params = list(
        #objective = "multi:softmax", 
        eval_metric = "rmse", 
        eta = .001,
        max_depth = 5,
        lambda = 0,
        subsample  = .5
      )
    )
    
    df_train_ym <- spm_train %>% dplyr::select("adj_close")
    df_test_ym <- spm_test %>% dplyr::select("adj_close")
    
    df_train_m <- model.matrix(adj_close ~ . - 1 - date - gasgulfm - gdp, spm_train)
    df_test_m <- model.matrix(adj_close ~ . - 1 - date - gasgulfm - gdp, spm_test)
    
    train_pool <- xgb.DMatrix(data = df_train_m, label = df_train_ym$adj_close)
    test_pool <- xgb.DMatrix(data = df_test_m, label = df_test_ym$adj_close)
    
    watchlist <- list(train = train_pool, test = test_pool)
    
    model_monthly <- xgb.train(
      watchlist = watchlist, 
      nrounds = 50000,
      data = train_pool,
      params = list(
        #objective = "multi:softmax", 
        eval_metric = "rmse", 
        eta = .001,
        max_depth = 5,
        lambda = 0,
        subsample  = .5
      )
    )
    
    rmse_m <- model_monthly$evaluation_log$test_rmse %>% min
    rmse_w <- model_weekly$evaluation_log$test_rmse %>% min
    
  } else if (model == "lm") {
    model_weekly <- lm(adj_close~volume_in_mln + wm2ns + gasregw + fedfunds + usdxw_close + indpro,
                       data=spw_train)
    #summary(lmw)
    
    y_predicted1 <- predict(model_weekly, newdata = as.data.frame(df_test_w))
    y_expected1 <- df_test_yw$adj_close
    rmse_w <- rmse(y_expected1, y_predicted1)
    
    model_monthly <- lm(adj_close~volume_in_mln + m2sl + gasregm + fedfunds + usdxm_close + indpro,
                        data=spm_train)
    #summary(lmw)
    
    y_predicted <- predict(model_monthly, newdata = as.data.frame(df_test_m))
    y_expected <- df_test_ym$adj_close
    rmse_m <- rmse(y_expected, y_predicted)
    
  } else if (model == "lasso") {
    formula_m <- adj_close ~ -1 + volume_in_mln + m2sl + gasregm + fedfunds + usdxm_close + indpro
    Xm <- model.frame(formula_m, spm_train)
    XM <- model.matrix(Xm, spm_train)
    model_monthly <- cv.ncvreg(X = XM, y = Xm$adj_close, penalty = "lasso", family = "gaussian", nlambda = 50)
    
    beta <- model_monthly$fit$beta[,model_monthly$min]
    Xm <- model.frame(formula_m, spm_test)
    XM <- model.matrix(Xm, spm_test)
    
    y_predicted1 <- predict(model_monthly, XM)
    y_expected1 <- spm_test$adj_close
    rmse_m <- rmse(y_expected1, y_predicted1)
    
    formula_w <- adj_close ~ -1 + volume_in_mln + wm2ns + gasregw + fedfunds + usdxw_close + indpro
    Xw <- model.frame(formula_w, spw_train)
    XW <- model.matrix(Xw, spw_train)
    model_weekly <- cv.ncvreg(X = XW, y = Xw$adj_close, penalty = "lasso", family = "gaussian", nlambda = 50)
    
    beta <- model_weekly$fit$beta[,model_weekly$min]
    Xw <- model.frame(formula_w, spw_test)
    XW <- model.matrix(Xw, spw_test)
    
    y_predicted <- predict(model_weekly, XW)
    y_expected <- spw_test$adj_close
    rmse_w <- rmse(y_expected, y_predicted)
    
  } else if (model == "arima") {
    
    spw_arima_train <- spw_train |> dplyr::select(-date,-gdp)
    
    model_weekly <- potezna_analiza(
      data = spw_arima_train,
      num = 4,
      prognosed = "adj_close",
      use_reg = FALSE,
      NAremoval = TRUE,
      year = c(2000,1),
      freq = 52)
    
    spm_arima_train <- spm_train |> dplyr::select(-date,-gdp)
    
    model_monthly <- potezna_analiza(
      data = spw_arima_train,
      num = 4,
      prognosed = "adj_close",
      use_reg = FALSE,
      NAremoval = TRUE,
      year = c(2000,1),
      freq = 12)
    
  }
  
  list(model_monthly = model_monthly,
       model_weekly = model_weekly,
       rmse_monthly = rmse_m,
       rmse_weekly = rmse_w)
}