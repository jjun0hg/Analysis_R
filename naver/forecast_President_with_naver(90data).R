# 3달 동안의 검색어 빅데이터를 이용한 분석
# 네이버 dataLab 이용
# '우물 안 개구리' 님의 블로그 글 인용하여 작성
# ARIMA 분석
# Improt Dataset 이용하여 불러오기

library(readxl)
forecast_President_naver_ <- read_excel("forecast_President(naver).xlsx", 
                                        +     col_types = c("skip", "text", "text"))
View(forecast_President_naver_)
na.omit(forecast_President_naver_)
FPN <- na.omit(forecast_President_naver_)

# 시계열 자료로 변환
ts <- ts(FPN)

# 자기상관(acf 이용)
acf(FPN)

# 정상범위를 벗어나는 것이 있어 적절한 차분 이용
diff(ts)

# ARIMA
library(forecast)
auto_arima_1 <- auto.arima(ts[,1])
auto_arima_2 <- auto.arima(ts[,2])

# auto_arima_1 = c(0,1,0) , auto_arima_2 = c(0,1,2)
arima(ts[,1], order = c(0,1,0))
arima(ts[,2], order = c(1,1,2))
model_1 <- arima(ts[,1], order = c(0,1,0))
model_2 <- arima(ts[,2], order = c(1,1,2))

# 검증
tsdiag(model_1)
Box.test(model_1$residuals, lag =1, type = "Ljung")
# p-value = 0.2559 이므로 사용가능
tsdiag(model_2)
Box.test(model_2$residuals, lag =1, type = "Ljung")
# p-value = 0.9437 이므로 사용가능


# 추세선 그어보기
plot(arima_1$x, lty=1)
lines(fitted(arima_1), lty=2, col ="red")
lines(fitted(arima_1), lty = 2 , lwd =1, col = "blue")

plot(arima_2$x, lty=1)
lines(fitted(arima_2), lty=2, col ="red")
lines(fitted(arima_2), lty = 2 , lwd =1, col = "red")

# 예측
fore_1 <- forecast(arima_1)
fore_2 <- forecast(arima_2)

plot(fore_1, ylim = c(0,100))
plot(fore_2, ylim = c(0,100))

