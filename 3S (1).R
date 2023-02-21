#### 0. 분석 환경  ####
## 분석환경을 명시합니다. 코드 실행시 오류가 나면 먼저 패키지 버전부터 확인해주세요.
## Rstudio에서 구동하시길 추천합니다. Jupyter사용시 패키지의 버전이 차이나서 시각화 과정에서 오류메시지가 발생합니다.(결과생성에 문제는 없음)

### OS, R, Rstudio Version ###
## Windows 10 pro, 10.0.19044 빌드 19044
## R 4.2.2 / RStudio 2022.07.2+576 "Spotted Wakerobin" Release (e7373ef832b49b2a9b88162cfe7eac5f22c40b34, 2022-09-06) for Windows

### 패키지 버전은 아래와 같습니다.###
## tidyverse 1.3.2, data.table 1.14.6, lubridate 1.9.0, forecast 8.19, zoo 1.8-11, stringr 1.4.1, tictoc 1.1, reshape2 1.4.4, dplyr 1.0.10

#### 1. 패키지 설치, 로딩 ####
## p_load로 필요한 패키지가 자동설치,로딩되니 기다려주세요.
## R, 패키지 버전에 따라서 오류가 발생할 수 있습니다.
## 외부데이터는 사용하지 않았습니다.
## pacman::p_load 오류시 install.package('패키지명');library(패키지명)로 패키지 설치,로딩하시길 추천합니다.
if(!(any(installed.packages()[,1] == 'pacman')) ){
  install.packages('pacman')
}
pacman::p_load(tidyverse, data.table, lubridate, forecast, zoo, stringr,tictoc,reshape2,dplyr)
list.files(pattern = '*csv')
#### 2. EDA ####
## dt_address변수에 데이터가 있는 폴더 경로를 넣어주세요. 데이터 입출력을 위한 작업입니다.
getwd()
dt_address <- '/Users/park/Downloads'
train_dt <- fread( paste0(dt_address,'/train.csv') , encoding = 'UTF-8')

train_dt$일시 <- ymd(train_dt$일시)

## 18~21년 구역별 시각화
par(mfrow=c(2,2))
for (i in 1:4){
  plot( as.data.frame(train_dt)[,c(1,i+1)] , type='l',col='lightskyblue2' )
}
par(mfrow=c(1,1))

## 18~21년 통합
# Warning message: "Ignoring unknown parameters: linewidth"가 발생시 ggplot2 버전차이때문입니다. ggplot2 3.4.0을 추천합니다.
train_dt %>%  reshape2::melt( id='일시', variable.name = 'region') %>% ggplot( aes(x=일시,y=value,col=region)) + geom_line(linewidth=0.6)

## 주말 영향력 분석
for( year_EDA in 2018:2021){
  temp <- train_dt %>% filter(year(일시)==year_EDA)
  wday <-  temp %>% .$일시 %>% wday() # 7:sunday, 1:saturday
  weekend <- ifelse( (wday==7) | (wday==1), T , F ) # 주말=1, 평일=0
  
  weekend_df <- temp[weekend,]
  cat('광진구, 동대문구, 성동구, 중랑구 순\n')
  cat(year_EDA,'년 주말 평균 : ',  weekend_df[,-1] %>% colMeans() ,'\n')
  
  week_df <- temp[!weekend, ]
  cat(year_EDA,'년 평일 평균 : ',  week_df[,-1] %>% colMeans() ,'\n\n')
}

## 2021년 주말 시각화 / 주말 = red & circle, 평일 = blue & triangle
wday <- train_dt %>% filter(year(일시)==2021) %>% .$일시 %>% wday() # 7:sunday, 1:saturday
weekend <- ifelse( (wday==7) | (wday==1), 1 , 0 ) # 주말=1, 평일=0
par(mfrow=c(2,2))
for( i in 2:5){
  as.data.frame(train_dt)[,c(1,i)] %>% filter(year(일시)==2021) %>% plot(col=ifelse(weekend==1,'red','blue'),pch=ifelse(weekend==1,16,17),main='2021년')
}
par(mfrow=c(1,1))

## 년도에 따른 6~9월 시각화
  train_dt %>% filter(year(일시) == 2018 & (month(일시)==6|month(일시)==7|month(일시)==8| month(일시)==9)) %>%  reshape2::melt( id='일시', variable.name = 'region') %>% ggplot( aes(x=일시,y=value,col=region)) + geom_line(linewidth=0.8)+ ggtitle(paste0(2018,'년')) 
  train_dt %>% filter(year(일시) == 2019 & (month(일시)==6|month(일시)==7|month(일시)==8| month(일시)==9)) %>%  reshape2::melt( id='일시', variable.name = 'region') %>% ggplot( aes(x=일시,y=value,col=region)) + geom_line(linewidth=0.8)+ ggtitle(paste0(2019,'년')) 
  train_dt %>% filter(year(일시) == 2020 & (month(일시)==6|month(일시)==7|month(일시)==8| month(일시)==9)) %>%  reshape2::melt( id='일시', variable.name = 'region') %>% ggplot( aes(x=일시,y=value,col=region)) + geom_line(linewidth=0.8)+ ggtitle(paste0(2020,'년')) 
  train_dt %>% filter(year(일시) == 2021 & (month(일시)==6|month(일시)==7|month(일시)==8| month(일시)==9)) %>%  reshape2::melt( id='일시', variable.name = 'region') %>% ggplot( aes(x=일시,y=value,col=region)) + geom_line(linewidth=0.8)+ ggtitle(paste0(2021,'년')) 
 
#### 3. Validation ####
### 3.1 2021's 1~12을 검증데이터로 사용 ###
  train_dt <- as.data.frame(fread( paste0(dt_address,'/train.csv') , encoding = 'UTF-8'))
  train_dt$일시 <- ymd(train_dt$일시) # 일시를 날짜형 데이터로 변경
  
  valid_real <- train_dt %>% filter( year(ymd(train_dt$일시)) == 2021 )
  valid_pred <- matrix(c(0),nrow=365,ncol = 5); colnames(valid_pred) <- c('일시','광진구','동대문구','성동구','중랑구'); valid_pred <-as.data.frame(valid_pred)
  valid_pred[,1] <- valid_real$일시

### 3.2 validation 데이터 단위 변경(일->월) ###
  train_month <- train_dt %>%  group_by(년 = year(train_dt$일시), 월 = month(train_dt$일시))  %>% 
    summarise(광진구= median(광진구, na.rm = T), 
              동대문구 = median(동대문구, na.rm = T),
              성동구= median(성동구, na.rm = T),
              중랑구= median(중랑구, na.rm = T)
    )
  ts_dt <- ts(train_month[,c(3:6)], start = c(2018, 1), end = c(2021, 12), freq =12)
  train.ts <- window(ts_dt, start = c(2018, 1), end = c(2020, 12))
  valid.ts <- window(ts_dt, start = c(2021, 1), end = c(2021, 12))

### 3.3 Cal MAE function (validation MAE계산 함수) ###
Cal_MAE_month <- function(predict = valid_pred){
    valid_MAE_table <- as.data.frame(matrix(c(0),nrow=365,ncol = 5))
    colnames(valid_MAE_table) <- c('일시','광진구','동대문구','성동구','중랑구')
    valid_MAE_table[,1] <- valid_real$일시
    for (i in 1:365){
      for (j in 2:5){
        valid_MAE_table[i,j]<- abs(valid_real[i,j]-predict[i,j])
      }
    }
    return(valid_MAE_table[,-1] %>% rowMeans() %>% mean)
  }

### 3.4 validation Double_Exponential_Smoothing (error = M, trend = A, seasonality = M) ###
par(mfrow=c(2,2))
  for ( i in 1:4){
    hwin <- ets( train.ts[,i] , model = "MAM")
    hwin.pred <- forecast(hwin, h = 12, level = 0)
    for( k in 1:365) {
      for(j in 1:12){
        if( month(valid_pred$일시)[k] == j ){
          valid_pred[k, i+1] <- hwin.pred$mean[j]
        }
      }
    }
    plot(as.data.frame(train_dt)[,c(1,i+1)] , type='l',col='lightskyblue2' ,ylim=c(0,17))
    lines(valid_pred[,c(1,i+1)],col='red',type='l',lwd=3)
  }
  Cal_MAE_month()
par(mfrow=c(1,1))

### 3.5 판단조정 검증 (6.20-7.10) ###
## 6월20-7월10일 사이 지역별 사용량을 고려해서 비율로 페널티 결정
## mean(기간의 데이터값) * 0.2일때 MAE가 가장 낮음
## ifelse문은 페널티가 더 클때 음수를 방지해줍니다.
ratio <- seq(0,1,0.1)
for(p in 1:length(ratio)){
  valid_pred_67 <- valid_pred
  start_day <- valid_pred_67[valid_pred_67$일시 == ymd('2021-06-20'), ] %>% row.names() %>% as.numeric()
  end_day <- valid_pred_67[valid_pred_67$일시 == ymd('2021-07-10'), ] %>% row.names() %>% as.numeric()
  
  for( i in 1:4){
    penalty <- valid_pred_67[start_day:end_day,i+1] %>% mean*ratio[p]
    valid_pred_67[start_day:end_day,i+1] <- valid_pred_67[start_day:end_day,i+1] - penalty
    valid_pred_67[start_day:end_day,i+1] <- ifelse(valid_pred_67[start_day:end_day,i+1] < 0, 0, valid_pred_67[start_day:end_day,i+1])
  }
  cat(Cal_MAE_month(valid_pred_67),', when penalty ratio is ',ratio[p],'\n')
}



### 3.6 판단조정 검증 (7.20-8.10) ###
## 7월20-8월10일 사이 지역별 사용량을 고려해서 비율로 페널티 결정
## mean(기간의 데이터값) * 0.1일때 MAE가 가장 낮음
## ifelse문은 페널티가 더 클때 음수를 방지해줍니다.
ratio <- seq(0,1,0.1)
for(p in 1:length(ratio)){
  valid_pred_78 <- valid_pred
  start_day <- valid_pred_78[valid_pred_78$일시 == ymd('2021-07-20'), ] %>% row.names() %>% as.numeric()
  end_day <- valid_pred_78[valid_pred_78$일시 == ymd('2021-08-10'), ] %>% row.names() %>% as.numeric()
  
  for( i in 1:4){
    penalty <- valid_pred_78[start_day:end_day,i+1] %>% mean*ratio[p]
    valid_pred_78[start_day:end_day,i+1] <- valid_pred_78[start_day:end_day,i+1] - penalty
    valid_pred_78[start_day:end_day,i+1] <- ifelse(valid_pred_78[start_day:end_day,i+1] < 0, 0, valid_pred_78[start_day:end_day,i+1])
  }
  cat(Cal_MAE_month(valid_pred_78),', when penalty ratio is ',ratio[p],'\n')
}

### 3.7 판단조정 검증 (8.20-9.10) ###
## 8월20-9월10일 사이 지역별 사용량을 고려해서 비율로 페널티 결정
## mean(기간의 데이터값) * 0.1일때 MAE가 가장 낮음
## ifelse문은 페널티가 더 클때 음수를 방지해줍니다.
ratio <- seq(0,1,0.1)
for(p in 1:length(ratio)){
  valid_pred_89 <- valid_pred
  start_day <- valid_pred_89[valid_pred_89$일시 == ymd('2021-08-20'), ] %>% row.names() %>% as.numeric()
  end_day <- valid_pred_89[valid_pred_89$일시 == ymd('2021-09-10'), ] %>% row.names() %>% as.numeric()
  
  for( i in 1:4){
    penalty <- valid_pred_89[start_day:end_day,i+1] %>% mean*ratio[p]
    valid_pred_89[start_day:end_day,i+1] <- valid_pred_89[start_day:end_day,i+1] - penalty
    valid_pred_89[start_day:end_day,i+1] <- ifelse(valid_pred_89[start_day:end_day,i+1] < 0, 0, valid_pred_89[start_day:end_day,i+1])
  }
  cat(Cal_MAE_month(valid_pred_89),', when penalty ratio is ',ratio[p],'\n')
}

# ## 6.20-9.10 전체에 페널티 (MAE가 더 높아 구역별로 빼주는것을 채택) / (실행시 드래그 후 ctrl+shift+c로 일괄주석제거하세요)
# ratio <- seq(0,1,0.1)
# for(p in 1:length(ratio)){
#   valid_pred_69 <- valid_pred
#   start_day <- valid_pred_69[valid_pred_69$일시 == ymd('2021-06-20'), ] %>% row.names() %>% as.numeric()
#   end_day <- valid_pred_69[valid_pred_69$일시 == ymd('2021-09-10'), ] %>% row.names() %>% as.numeric()
# 
#   for( i in 1:4){
#     penalty <- valid_pred_69[start_day:end_day,i+1] %>% mean*ratio[p]
#     valid_pred_69[start_day:end_day,i+1] <- valid_pred_69[start_day:end_day,i+1] - penalty
#     valid_pred_69[start_day:end_day,i+1] <- ifelse(valid_pred_69[start_day:end_day,i+1] < 0, 0, valid_pred_69[start_day:end_day,i+1])
#   }
#   cat(Cal_MAE_month(valid_pred_69),', when penalty ratio is ',ratio[p],'\n')
# }


### 3.8 페널티 고려 후 MAE ###
valid_pred_after <- valid_pred

start_day <- valid_pred_after[valid_pred_after$일시 == ymd('2021-06-20'), ] %>% row.names() %>% as.numeric()
end_day <- valid_pred_after[valid_pred_after$일시 == ymd('2021-07-10'), ] %>% row.names() %>% as.numeric()

for( i in 1:4){
  penalty <- valid_pred_after[start_day:end_day,i+1] %>% mean*0.2
  valid_pred_after[start_day:end_day,i+1] <- valid_pred_after[start_day:end_day,i+1] - penalty
  valid_pred_after[start_day:end_day,i+1] <- ifelse(valid_pred_after[start_day:end_day,i+1] < 0, 0, valid_pred_after[start_day:end_day,i+1])
}

start_day <- valid_pred_after[valid_pred_after$일시 == ymd('2021-07-20'), ] %>% row.names() %>% as.numeric()
end_day <- valid_pred_after[valid_pred_after$일시 == ymd('2021-08-10'), ] %>% row.names() %>% as.numeric()

for( i in 1:4){
  penalty <- valid_pred_after[start_day:end_day,i+1] %>% mean*0.1
  valid_pred_after[start_day:end_day,i+1] <- valid_pred_after[start_day:end_day,i+1] - penalty
  valid_pred_after[start_day:end_day,i+1] <- ifelse(valid_pred_after[start_day:end_day,i+1] < 0, 0, valid_pred_after[start_day:end_day,i+1])
}

start_day <- valid_pred_after[valid_pred_after$일시 == ymd('2021-08-20'), ] %>% row.names() %>% as.numeric()
end_day <- valid_pred_after[valid_pred_after$일시 == ymd('2021-09-10'), ] %>% row.names() %>% as.numeric()

for( i in 1:4){
  penalty <- valid_pred_after[start_day:end_day,i+1] %>% mean*0.1
  valid_pred_after[start_day:end_day,i+1] <- valid_pred_after[start_day:end_day,i+1] - penalty
  valid_pred_after[start_day:end_day,i+1] <- ifelse(valid_pred_after[start_day:end_day,i+1] < 0, 0, valid_pred_after[start_day:end_day,i+1])
}
Cal_MAE_month(valid_pred_after)

## validation 시각화
par(mfrow=c(2,2))
for( i in 1:4){
plot(as.data.frame(train_dt)[,c(1,i+1)] , type='l',col='lightskyblue2' ,ylim=c(0,17))
lines(valid_pred_after[,c(1,i+1)],col='red',type='l',lwd=3)
}
par(mfrow=c(1,1))

#### 4. 모델 생성 , 예측결과 시각화, 예측결과 생성  ####
train_dt <- fread( paste0(dt_address,'/train.csv') , encoding = 'UTF-8')
train_dt$일시 <- ymd(train_dt$일시)

result <- fread(paste0(dt_address,'/sample_submission.csv'), encoding = 'UTF-8')
colnames(result) <- c('일시','광진구','동대문구','성동구','중랑구')
result$일시 <- ymd(result$일시);  result <- as.data.frame(result)

train_month <- train_dt %>%  dplyr::group_by(년 = year(train_dt$일시), 월 = month(train_dt$일시))  %>%   dplyr::summarise(
  광진구 = median(광진구, na.rm = T),
  동대문구 = median(동대문구, na.rm = T),
  성동구= median(성동구, na.rm = T),
  중랑구= median(중랑구, na.rm = T)
  )
ts_dt <- ts(train_month[,c(3:6)], start = c(2018, 1), end = c(2021, 12), freq = 12)

### 4.1 Holt-winters DES ###
## i = 광진구,동대문구,성동구,중랑구 / k = 22.1.1 ~ 22.11.30의 334일 / j = 1월~11월
tic()
  for ( i in 1:4){
    hwin <- ets(ts_dt[,i], model = "MAM") 
    hwin.pred <- forecast(hwin, h = 11, level = 0)
    for( k in 1:334) {
      for(j in 1:11){
        if( month(result$일시)[k] == j ){
          result[k, i+1] <- hwin.pred$mean[j]
        }
      }
    }
  }

### 4.2 최종모형 판단조정 반영 ###
## mean(6월20일~7월10일) * 0.2
  start_day <- result[result$일시 == ymd('2022-06-20'), ] %>% row.names() %>% as.numeric()
  end_day <- result[result$일시 == ymd('2022-07-10'), ] %>% row.names() %>% as.numeric()
  for( i in 1:4){
    penalty <- result[start_day:end_day,i+1] %>% mean*0.2
    result[start_day:end_day,i+1] <- result[start_day:end_day,i+1] - penalty
  }
  
## mean(7월20일~8월10일) * 0.1
  start_day <- result[result$일시 == ymd('2022-07-20'), ] %>% row.names() %>% as.numeric()
  end_day <- result[result$일시 == ymd('2022-08-10'), ] %>% row.names() %>% as.numeric()
  for( i in 1:4){
    penalty <- result[start_day:end_day,i+1] %>% mean*0.1
    result[start_day:end_day,i+1] <- result[start_day:end_day,i+1] - penalty
  }
  
## mean(8월20일~9월10일) * 0.1
  start_day <- result[result$일시 == ymd('2022-08-20'), ] %>% row.names() %>% as.numeric()
  end_day <- result[result$일시 == ymd('2022-09-10'), ] %>% row.names() %>% as.numeric()
  for( i in 1:4){
    penalty <- result[start_day:end_day,i+1] %>% mean*0.1
    result[start_day:end_day,i+1] <- result[start_day:end_day,i+1] - penalty
  }

## 최종모형 시각화
  par(mfrow=c(2,2))
  for( i in 1:4){
    plot(as.data.frame(train_dt)[,c(1,i+1)] , type='l',col='lightskyblue2', ylim=c(0,17), xlim =c(as.numeric(date('2018-01-01')),as.numeric(date('2022-12-31')))  )
    lines(result[,c(1,i+1)],col='red',type='l',lwd=3)
  }
  par(mfrow=c(1,1))
toc()
# tic() 부터 toc()까지 한번에 실행시, 시간 측정


#### 5. CSV저장 ####
result[,1] <- as.character( result[,1] ) %>% stringr::str_remove_all('-') %>% as.integer()
write_csv(result, 'Result_1201.csv')
# write.csv(result, 'Final_result.csv', fileEncoding='utf-8', row.names = F)로도 파일 생성 가능

