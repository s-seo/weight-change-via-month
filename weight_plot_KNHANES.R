
#애초에 국건영은 연 단위 데이터인데...계절성 조차도 확인할 수 없다
#분석 목적은, 생활 습관이 체중에 영향을 미치고, 연령대에 따라
#그 영향의 정도가 달라짐을 확인하는 것이다. 이것이 확인된다면 향후 다른 연구를
#뒷받침하는 자료로 활용 가능할 수도.

#국건영 데이터는 매년, 랜덤하게 뽑힌 가구를 대상으로 여러 검사를 한 것.
#시계열 분석을 하기에 매우 부적합한 데이터

#가능한건, 연관성 분석인데 기존의 연구 방향에서 좀 벗어남
#굳이 엮는다면, 여기서 연관성을 확인하고, 검진 데이터에서 그 패턴을 확인한 뒤
#


lapply(c('dplyr',
         'data.table',
         'reshape',
         'haven',
         'rowr',
         'ggplot2',
         'gridExtra'), require, character.only=T)


setwd('C:\\Users\\sseo\\Desktop\\R\\weight_plot\\KNHANES')

lst <- list.files() %>% grep('sas7bdat', ., value=T)
l1 <- lapply(seq_along(lst), function(x){
  if(substr(lst[x], 3, 4) %>% as.numeric() <= 13){
    read_sas(lst[x]) %>%
      as.data.frame() %>%
      select(year, sex, age, BD1:dr_month, BE3_11:pa_walk, HE_ht, HE_wt, HE_BMI)
  } else {
    read_sas(lst[x]) %>%
      as.data.frame() %>%
      select(year, sex, age, BD1:dr_month, BE3_71:pa_aerobic, HE_ht, HE_wt, HE_BMI)
  }
})

res <- c()
for(i in seq_along(lst)){
  cb <- colnames(l1[[i]]) %>% as.data.frame() %>% rename(c(.=substr(lst, 1, 4)[i]))
  res <- cbind.fill(res, cb, fill=NA)
} ; res <- res[,-1]
View(res)

l1[[1]] %>% View()



%>%
  select(year, sex, age, BD1:dr_month, HE_ht, HE_wt, HE_BMI)
dat1 <- read_sas(list.files()[1]) %>% as.data.frame() %>% select()
colnames(dat1)
year
sex
age
BD1:dr_month #음주량
BE3_11:pa_walk #13년까지 운동량
BE3_71:pa_aerobic #14년부터 운동량
HE_ht
HE_wt
HE_BMI
L_BR:LF_CARE #식습관
dat2 <- read_sas(list.files()[2]) %>% as.data.frame()
dat3 <- read_sas(list.files()[3]) %>% as.data.frame()
dat4 <- read_sas(list.files()[4]) %>% as.data.frame()
dat5 <- read_sas(list.files()[5]) %>% as.data.frame()

str(dat)

View(dat)



