# 한달간의 검색어 빅데이터를 이용한 분석
# 카카오 트렌드 데이터 이용
# '우물 안 개구리' 님의 블로그 글 인용하여 작성
# ARIMA 분석
# improt dataset 이용하여 불러오기
library(readxl)
forecast_President <- read_excel("forecast_President.xlsx", 
                                   +     col_types = c("skip", "numeric", "numeric"))
View(forecast_President)
# 결측값 제거
na.omit(forecast_President)
FP <- na.omit(forecast_President)
ts <- ts(FP)
# 자기상관(acf 이용)
acf(FP)
# 정상범위를 벗어나는것이 많지 않으므로 차분X
library(forecast)
auto_arima_1 <- auto.arima(ts[,1])
auto_arima_2 <- auto.arima(ts[,2])
# auto_arima_1 = c(2,1,1) , auto_arima_2 = c(1,1,1)
arima(ts[,1], order = c(2,1,1))
arima(ts[,2], order = c(1,1,1))
model_1 <- arima(ts[,1], order = c(2,1,1))
model_2 <- arima(ts[,2], order = c(1,1,1))
# 검증
tsdiag(model_1)
Box.test(model_1$residuals, lag =1, type = "Ljung")
# p-value = 0.8273 이므로 사용가능
tsdiag(model_2)
Box.test(model_2$residuals, lag =1, type = "Ljung")
# p-value = 0.7524 이므로 사용가능


# 추세선 그어보기
plot(auto_arima_1$x, lty=1)
lines(fitted(auto_arima_1), lty=2, col ="red")
lines(fitted(auto_arima_1), lty = 2 , lwd =1, col = "red")

plot(auto_arima_2$x, lty=1)
lines(fitted(auto_arima_2), lty=2, col ="red")
lines(fitted(auto_arima_2), lty = 2 , lwd =1, col = "red")
# 예측하기
fore_1 <- forecast(auto_arima_1)
fore_2 <- forecast(auto_arima_2)
# 예측 시각화
plot(fore_1)
plot(fore_2)

# fore_1이 근소하게 앞서있지만 명확하지 않아, 더 많은 데이터를 이용




