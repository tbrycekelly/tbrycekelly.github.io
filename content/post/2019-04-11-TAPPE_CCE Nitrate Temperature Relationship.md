---
title: "TAPPE_CCE Nitrate Temperature Relationship"
author: "Thomas Kelly"
date: 2019-04-11T21:13:14-05:00
categories: ["R"]
tags: ["R Markdown", "plot", "regression"]
---



```R
source('source.r')
source('source.maps.r')
source('source.physics.r')

```


```R
data = read.xlsx('../Data/CalCOFI Nitrate.xlsx')
str(data)
```

    'data.frame':	342041 obs. of  19 variables:
     $ studyName : chr  "1959-08-13-C-31HS" "1959-08-13-C-31HS" "1959-08-13-C-31HS" "1959-08-13-C-31HS" ...
     $ Cast      : num  11129 11129 11129 11129 11129 ...
     $ Bottle    : num  291243 291245 291248 291252 291254 ...
     $ Stn       : chr  "096.7 032.0" "096.7 032.0" "096.7 032.0" "096.7 032.0" ...
     $ Depth     : num  374 400 407 462 468 500 523 0 1 10 ...
     $ T         : num  7.79 7.51 7.55 6.91 6.82 ...
     $ S         : num  34.2 34.3 34.3 34.3 34.3 ...
     $ Nitrate   : num  24.8 30.2 31.7 26.6 28.8 28.7 28.6 0.9 0.9 0.8 ...
     $ Oxygen.ml : num  1.7 1.06 1 0.34 0.58 0.41 0.41 4.72 4.72 4.14 ...
     $ Sigma     : num  26.7 26.8 26.8 26.9 26.9 ...
     $ Oxygen.sat: num  25.5 15.8 14.9 5 8.5 6 5.9 100 100 87.7 ...
     $ Oxygen.uM : num  73.9 46.1 43.5 14.8 25.2 ...
     $ Chl.mg    : num  NA NA NA NA NA NA NA NA NA NA ...
     $ Phaeo     : num  NA NA NA NA NA NA NA NA NA NA ...
     $ Phosphate : num  2.89 2.93 2.85 3.12 3.13 3.18 3.25 0.36 0.36 0.41 ...
     $ Silica    : num  52 54.9 55 65 66 71.2 74 1 1 1 ...
     $ Nitrite   : num  NA NA NA NA NA NA NA 0 0 0 ...
     $ Ammonia   : num  NA NA NA NA NA NA NA NA NA NA ...
     $ Light     : num  NA NA NA NA NA NA NA NA NA NA ...
    


```R
data = data[data$Cast > 22000,]
data$Dist = as.numeric(substr(data$Stn, 1, 5))
data$Line = as.numeric(substr(data$Stn, 7, 11))
```


```R
str(data)
```

    'data.frame':	294817 obs. of  22 variables:
     $ studyName : chr  "1981-05-19-C-31JD" "1981-05-19-C-31JD" "1981-05-19-C-31JD" "1981-05-19-C-31JD" ...
     $ Cast      : num  22031 22031 22031 22031 22031 ...
     $ Bottle    : num  543653 543654 543655 543656 543657 ...
     $ Stn       : chr  "090.0 030.0" "090.0 030.0" "090.0 030.0" "090.0 030.0" ...
     $ Depth     : num  0 1 8 10 17 20 28 30 48 50 ...
     $ T         : num  19.2 19.2 19.2 18.4 14.9 ...
     $ S         : num  33.7 33.7 33.7 33.6 33.6 ...
     $ Nitrate   : num  0.2 0.2 0.2 0.3 0.6 3.1 11.1 12.4 19.1 19.3 ...
     $ Oxygen.ml : num  6.03 6.03 6.12 5.74 4.23 3.96 3.65 3.58 3.27 3.28 ...
     $ Sigma     : num  23.9 23.9 24 24.1 24.9 ...
     $ Oxygen.sat: num  114.2 114.2 115.8 107 73.6 ...
     $ Oxygen.uM : num  263 263 267 250 184 ...
     $ Chl.mg    : num  0.35 0.35 0.4 1.17 3.55 3.01 0.56 0.52 0.15 0.14 ...
     $ Phaeo     : num  0.07 0.07 0.08 0.18 0.55 0.54 0.5 0.47 0.17 0.17 ...
     $ Phosphate : num  0.18 0.18 0.2 0.31 0.79 0.93 1.23 1.28 1.57 1.58 ...
     $ Silica    : num  3.4 3.4 3.2 4.6 10.5 11.8 14.2 14.9 20.7 21 ...
     $ Nitrite   : num  0.04 0.04 0.04 0.06 0.19 0.3 0.55 0.53 0.07 0.07 ...
     $ Ammonia   : num  NA NA NA NA NA NA NA NA NA NA ...
     $ Light     : num  NA NA NA NA NA NA NA NA NA NA ...
     $ Predict   : num  NA NA NA NA NA NA NA NA NA NA ...
     $ Dist      : num  90 90 90 90 90 90 90 90 90 90 ...
     $ Line      : num  30 30 30 30 30 30 30 30 30 30 ...
    


```R
par(mfrow=c(2,2))

l = which(data$Depth > 100)

plot(data$T[l], data$Nitrate[l] + data$Nitrite[l], pch = 20, cex = 0.4, ylim = c(0, 55), yaxs='i',
     xaxs='i', xlab = 'Temperature', ylab = 'Nitrate + Nitrite',
     col = make.pal(data$Depth[l], n = 200, min = 50, max = 500, pal = 'ocean.deep', rev = TRUE))


plot(calc.sigma.theta(S = data$S[l], T = data$T[l], P = data$Depth[l]), data$Nitrate[l] + data$Nitrite[l], pch = 20, cex = 0.4, ylim = c(0, 55), yaxs='i', xaxs='i',
     xlab = 'Sigma Theta', ylab = 'Nitrate + Nitrite',
     col = make.pal(data$Depth[l], n = 200, min = 50, max = 500, pal = 'ocean.deep', rev = TRUE))


plot.colorbar(50, 500, pal = 'ocean.deep', rev = TRUE)


plot(data$S[l], data$Nitrate[l] + data$Nitrite[l], pch = 20, cex = 0.4, ylim = c(0, 55), yaxs='i', xaxs='i',
     xlab = 'Salinity', ylab = 'Nitrate + Nitrite',
     col = make.pal(data$Depth[l], n = 200, min = 50, max = 500, pal = 'ocean.deep', rev = TRUE))
```


![png](output_4_0.png)



```R
par(mfrow=c(2,2))

x = calc.sigma.theta(S = data$S[l], T = data$T[l], P = data$Depth[l])
y = data$Nitrate[l] + data$Nitrite[l]

plot(x, y, pch = 20, cex = 0.4, ylim = c(0, 55), yaxs='i', xaxs='i',
     xlab = 'Sigma Theta', ylab = 'Nitrate + Nitrite',
     col = make.pal(data$Depth[l], n = 200, min = 50, max = 500, pal = 'ocean.deep', rev = TRUE))

ols = lm(y ~ x)

data$Predict = NA
data$Predict[l] = x * ols$coefficients[[2]] + ols$coefficients[[1]]

lines(c(0, 100), predict(ols, newdata = data.frame(x = c(0,100))), lwd = 2, col = 'red')


plot(ols$model$x, ols$residuals, xlab = 'Sigma Theta', ylab = 'Residual', pch = 20,
     col = make.pal(data$Cast[l], pal = 'ocean.phase'), cex = 0.4)

add.one.to.one(slope = 0)


plot(data$Cast[l], data$Predict[l] - y, pch = 20, cex = 0.5)

plot(data$Predict[l] - y, data$Nitrite[l] / x,  pch = 20, cex = 0.5, ylab = 'Proportion of nitrite')

plot(data$Depth[l], data$Predict[l] - y, pch = 20, cex = 0.5)

plot(data$Dist[l], data$Predict[l] - y, pch = 20, cex = 0.5)
plot(data$Line[l], data$Predict[l] - y, pch = 20, cex = 0.5)
```


![png](output_5_0.png)



![png](output_5_1.png)

