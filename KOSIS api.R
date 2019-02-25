library(jsonlite)
library(tidyverse)

### 한국은행 ECOS
api_key_ecos <- "9N885975PQXAH2B7L9FJ"
url <- paste0("http://ecos.bok.or.kr/api/StatisticSearch/",api_key_ecos,"/json/kr/1/1000")
staticCode <- "I01Y001" # 18.1.1 본원통화 & 개발가이드 -> 통계코드검색
period <- "MM"
start <- "20150101"
end <- "20171031"
query <- paste0(paste(url, staticCode, period, start, end, sep="/"),"/")
raw.data <- readLines(query, warn = "F",encoding="UTF-8")
dat<- fromJSON(raw.data)$StatisticSearch$row
dat[c("STAT_NAME","STAT_CODE","TIME","ITEM_NAME1","ITEM_CODE1","DATA_VALUE","UNIT_NAME")]


#######################################################################################################
############################################### KOSIS #################################################
#######################################################################################################

api_key_kosis="YmU1NjBlMDcxZWM1NTNjMmZjZDM2N2U4MjAxOWIzYmY="
start_from=60 #개월수
period="M" #주기=월간

#광공업 생산, 재고, 출하, 계절조정 생산
input_1=list(inst_code=101,data_code="DT_1F01501",item_id=c("T10","T11","T12","T20"),lv1code="00",
           lv2code=paste0(c(0,"B","C","C10","C11","C12","C13","C14","C15","C16","C17","C18",
           "C19","C20","C21","C22","C23","C24","C25","C261","C262","C263","C264",
           "C265","C27","C28","C29","C30","C31","C32","C33","D"),"+"),
           title=NULL,data_set=list(NULL,NULL,NULL,NULL))

#무역협회 수출 및 수입
input_2=list(inst_code=360,data_code="DT_1R11001_FRM101",item_id=paste0(13103103825,c("T1","T2")) ,
             lv1code=paste0("13102103825A.",
                            c("0","1","11","12","2","21","22","23","24","25","26","27","28","29","3","32","33","34",
                              "4","41","42","43","5","51","52","53","54","55","56","57","58","59","6","61",
                              "62","63","64","65","66","67","68","69","7","71","72","73","74","75","76","77","78",
                              "79","8","81","82","83","84","85","87","88","89","9","96","97","99","A","A1","A2","A3",
                              "A4","A5","A6","A7","A8","A9","AA")),lv2code="",title=NULL,data_set=list(NULL,NULL))

#산업통상자원부 ICT수출
input_3=list(inst_code=115,data_code="DT_092_115_2009_S023",item_id="13103870961T1",
             lv1code=paste0("13102870961A.AF1",c("00","10","11","12","13","14","15","16","17","19","20","21","22",
                                                 "30","31","32","40","41","42","49","50","51","52","53","54","55"),"00000"),
             lv2code="",title=NULL,data_set=list(NULL))

#산업통상자원부 ICT수입
input_4=list(inst_code=115,data_code="DT_092_115_2009_S024",item_id="13103870957T1",
             lv1code=paste0("13102870957A.AF1",c("00","10","11","12","13","14","15","16","17","19","20","21","22","30",
                                                 "31","32","40","41","42","49","50","51","52","53","54","55"),"00000"),
             lv2code="",title=NULL,data_set=list(NULL))

#광공업 생산능력/가동률 조사
input_5=list(inst_code=101,data_code="DT_1F30005",item_id=c("T10","T20"),
             lv1code=c("C","C10","C11","C12","C13","C14","C15","C16","C17","C19","C20","C22","C23","C24",
                       "C25","C26","C27","C28","C29","C30","C31","C32","C33"),
             lv2code="",title=NULL,data_set=list(NULL,NULL))


entire_list=list(input_1,input_2,input_3,input_4,input_5)
entire_data=list(input_1$data_set,input_2$data_set,input_3$data_set,input_4$data_set,input_5$data_set)

for(a in 1:length(entire_list)){

  for(i in 1:length(entire_list[[a]]$item_id)){
  for(j in 1:length(entire_list[[a]]$lv1code)){
    for(k in 1:length(entire_list[[a]]$lv2code)){
      url=paste0("http://kosis.kr/openapi/Param/statisticsParameterData.do?method=getList&apiKey=",
                 api_key_kosis,"&itmId=",entire_list[[a]]$item_id[i],"+&objL1=",entire_list[[a]]$lv1code[j],"+&objL2=",
                 entire_list[[a]]$lv2code[k],"&objL3=&objL4=&objL5=&objL6=&objL7=&objL8=&format=json&jsonVD=Y&prdSe=",
                 period,"&newEstPrdCnt=",start_from,"&loadGubun=1&orgId=",entire_list[[a]]$inst_code,"&tblId=",
                 entire_list[[a]]$data_code)
      raw.data=readLines(url, warn = "F",encoding="UTF-8")
      
      if(raw.data[1]=="{err:\"30\",errMsg:\"데이터가 존재하지 않습니다.\"}")
      {dat=data.frame(DT=rep(0,start_from),C1_NM=rep("Missing",start_from),
                      C2_NM=rep("Missing",start_from),PRD_DE=rep(0,start_from))}else{dat=fromJSON(raw.data[1])}
      
      dat_DT=as.numeric(dat$DT)
      entire_data[[a]][[i]]$data_set=cbind(entire_data[[a]][[i]]$data_set,c(rep(0,start_from%%length(dat_DT)),dat_DT))
      entire_list[[a]]$title=c(entire_list[[a]]$title,
                               ifelse(length(entire_list[[a]]$lv1code)>length(entire_list[[a]]$lv2code),
                                      dat$C1_NM[1],dat$C2_NM[1]))
      
    }
  }
  colnames(entire_data[[a]][[i]]$data_set)=entire_list[[a]]$title
  rownames(entire_data[[a]][[i]]$data_set)=as.numeric(dat$PRD_DE)
  entire_list[[a]]$title=NULL
}


}

entire_data


################################## 광공업생산 원지수계산 ##############################
library(fArma)
library(tseries)
library(zoo)
library(vars)
library(foreign)

oilandchem=read.table("C:\\Users\\user\\Desktop\\oilandchem.R",header=T) #remove double
oilandchem=data.frame(oilandchem[(nrow(oilandchem)-start_from+1):nrow(oilandchem),])
colnames(oilandchem)=names(oilandchem)

carprod=read.table("C:\\Users\\user\\Desktop\\Rscript\\carproduction.R",header=T) #remove double
carprod=data.frame(carprod[(nrow(carprod)-start_from+1):nrow(carprod),])
colnames(carprod)=names(carprod)
  
ip_data=data.frame(entire_data[[1]][[1]]$data_set)
colnames(ip_data)=paste(names(ip_data),"생산")

ip_data_stock=data.frame(entire_data[[1]][[2]]$data_set)
colnames(ip_data_stock)=paste(names(ip_data_stock),"재고")

ip_data_output=data.frame(entire_data[[1]][[3]]$data_set)
colnames(ip_data_output)=paste(names(ip_data_output),"출하")

ex_data=data.frame(entire_data[[2]][[1]]$data_set)
colnames(ex_data)=paste(names(ex_data),"수출")

im_data=data.frame(entire_data[[2]][[2]]$data_set)
colnames(im_data)=paste(names(im_data),"수입")

ict_ex_data=data.frame(entire_data[[3]][[1]]$data_set)
colnames(ict_ex_data)=paste(names(ict_ex_data),"수출")

ict_im_data=data.frame(entire_data[[4]][[1]]$data_set)
colnames(ict_ex_data)=paste(names(ict_ex_data),"수입")


ip_data[start_from,]
ex_data[start_from,]
im_data[start_from,]
ict_ex_data[start_from,]
ict_im_data[start_from,]

y.varia=ip_data
x.varia.1=cbind(ip_data_stock,ip_data_output)
x.varia.2=cbind(ex_data,im_data,ict_ex_data,ict_im_data,oilandchem,carprod)
x.variable=cbind(x.varia.1,x.varia.2)
y.diff=y.varia[-1,]
x.diff=cbind(x.varia.1[-1,],x.varia.2[-start_from,])

n.y=names(y.varia)
n.x=names(x.variable)


pred.value=NULL

for(i in 1:length(n.y)){
  
  diff.cor.table=NULL
  
  for(j in 1:length(n.x)){
    
    cor.value=cor(diff(log(y.diff[,i],exp(1))),diff(log(x.diff[,j],exp(1))))
    diff.cor.table[j]=cor.value
        
  }
  high.diff.cor=which.max(diff.cor.table)
  high.diff.cor2=which.max(diff.cor.table[-high.diff.cor])
  var.model=VAR(cbind(y.varia[,i],x.variable[high.diff.cor],x.variable[high.diff.cor2]),p=2,type=c("const"))
  pred=predict(var.model,n.ahead=1)$fcst$y.varia...i.[1]
  pred.value=c(pred.value,pred)
  
}

#leg 1로 한것과 leg 2로 한것, variable 1~3개 넣은것 까지 6개 경우의 수 fit하고 최적의 모델로 저장
# x variable끼리 correlation 높은것 처리
# vAR모델 summary에서 에러 발생하는 경우 있음


pred.value

ip_weight=c(10000, 33.9,9577.7,434.4,82.4,43.2,160.6 ,145.2,42.1 ,31.7,126.8,50.2,471.0,
            847.5,144.1,421.1,271.7,827.6,557.8,484.4,805.2,71.0,354.9,78.8,148.1,479.5,803.6,1076.4,
            506.5,69.5,42.4,388.4)

ip_original_pred=pred.value[c(-1,-3)]%*%ip_weight[c(-1,-3)]/10000
ip_original_pred

cbind(n.y,ip_weight,pred.value)

names(ip_data)
names(ex_data)
names(im_data)
names(ict_ex_data)
names(ict_im_data)

#f=as.formula(paste(n.y[1],"~",paste(n.x[!n.x%in%n.x[1]],collapse="+")))


################################## 계절조정지수계산 ##############################
#◎ 계절조정 
#○ 계절조정 대상계열 및 기간
#- 대상계열 : 설비투자 총지수, 2개 대분류, 6개 중분류, 11개 소분류
#- 계절인자 산출 대상기간 (<= 추정인자법 적용) : 1995.2월 이후 시계열
#○ 산출방법 
#- X-13-ARIMA 를 이용하여 계절요인 등을 산출
#- 계절조정지수=원지수/(계절요인×명절요인×조업일수요인)
#- 대분류 계절조정지수를 가중평균하여 총지수 계절조정지수를 작성(간접법)
#○ 계절조정지수 연간보정
#- 매년 2월에 최근 1년분 시계열을 추가하여 계절인자 등을 재산출하여 과거 계절조정지수를 보정  

# goal 1 : 계절요인, 명절요인, 조업일수요인 제거 (R모델에 반영)
# goal 1-1 : 1.명절/휴일 정리하는법 2.seas에 반영하는 법  
# goal 2 : input 넣고 output 산출 -> OK
# goal 3 : 예측값 산출 -> OK


library(seasonal) ##tseries package와 겹쳐서 쓰지 않도록 주의. (series 명령어 겹침)
library(timeDate)
library(zoo)
library(shiny)
Sys.setenv(X13_PATH = "C:\\Users\\user\\Desktop\\x13ashtmlall\\x13ashtml")
checkX13(fail = FALSE, fullcheck = TRUE, htmlcheck = TRUE)

thanksgiving=read.table("C:\\Users\\user\\Desktop\\thanksgiving.R",header=T)
attach(thanksgiving)

new.y.varia=rbind(y.varia,pred.value)

data(seasonal)
data(holiday)

cnyts=genhol(cny, start = -5, end = 5, center = "calendar")
Thanksgivingts=genhol(as.Date(thanksgiving[,1]),start = -5, end = 5, center = "calendar")

SeasonalForecast=NULL
for(i in 1:32){
  season.adj=ts(c(new.y.varia[,i]),frequency = 12, start = c(2013, 2)) 
  mm=seas(season.adj, xreg = cbind(cnyts,Thanksgivingts))
  FF=c(final(mm))
  SeasonalForecast=cbind(SeasonalForecast,FF[length(final(mm))])
}


ip_seasonal_pred=SeasonalForecast[c(-1,-3)]%*%ip_weight[c(-1,-3)]/10000


ip_original_pred
ip_seasonal_pred

prev.origin=data.frame(entire_data[[1]][[1]]$data_set)[60,1]
prev.season=data.frame(entire_data[[1]][[4]]$data_set)[60,1]

round((ip_original_pred-prev.origin)/prev.origin*100,2)
round((ip_seasonal_pred-prev.season)/prev.season*100,2)
