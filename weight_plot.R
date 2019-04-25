
#검진 데이터 2003년부터 2013년까지 일별 통계량 구해서 반출 신청.
#신청하기 위한 코드

#x = 키, y = 계절, z = 몸무게, BMI
#x = 월별, y = 몸무게, 음주량
#x = 월별, y = 몸무게, 운동량
#x = 월별, y = BMI, 음주량
#x = 월별, y = BMI, 운동량

#남
#x = 키, y = 계절, z = 몸무게, BMI
#x = 월별, y = 몸무게, 음주량
#x = 월별, y = 몸무게, 운동량
#x = 월별, y = BMI, 음주량
#x = 월별, y = BMI, 운동량

#여
#x = 키, y = 계절, z = 몸무게, BMI
#x = 월별, y = 몸무게, 음주량
#x = 월별, y = 몸무게, 운동량
#x = 월별, y = BMI, 음주량
#x = 월별, y = BMI, 운동량

#성별 : sex
#검진일자 : day
#몸무게 : weight
#키 : height
#BMI : BMI
#음주량 : alcohol
#운동량 : workout


library(haven)
library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)

setwd('C:\\Users\\sseo\\Desktop\\R\\weight_plot')
lf <- grep('dat', list.files(), value=T)
l1 <- lapply(1:4, function(x) read.csv(lf[x]) %>% 
               select(-1) %>%
               mutate(day = as.factor(day)))
names(l1) <- paste0(rep(c('men', 'women'), 2), rep(c('02','09'), each=2))



week(ymd(men09[1000,1]))

men09 <- l1[[3]]


#x = 월별, y = 몸무게, 음주량
d1 <- men09 %>% 
  mutate(month = substr(as.character(day), 1, 6)) %>%
  group_by(month) %>%
  summarise(weight = mean(weight),
            height = mean(height),
            BMI = mean(BMI),
            alcohol = mean(alcohol),
            workout = mean(workout))

mt <- d1 %>% mutate(month = paste0(month, '01')) %>% select(month) %>% unlist()

as.Date()
seq(ymd(mt[1]), ymd(mt[84]), by='1 year')

ggplot(d1 %>%
         mutate(month = paste0(month, '01')), aes(x=as.factor(month), y=weight, group=1, colour='weight')) +
  geom_line(size=1.5) +
  geom_line(aes(y=alcohol+56, colour='alcohol'), size=1.5) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~.-56)) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_vline(xintercept = which(d1$month %in% paste0(rep(2009:2015, 4), c('03', '06', '09', '12'))),
             linetype='dashed', alpha=.2, size=.05) +
  geom_vline(xintercept = which(d1$month %in% paste0(rep(2009:2015), '01')))







#x = 월별, y = 몸무게, 운동량
men09 %>% 
  mutate(month = substr(as.character(day), 1, 6)) %>%
  group_by(month) %>%
  summarise(weight = mean(weight),
            height = mean(height),
            BMI = mean(BMI),
            alcohol = mean(alcohol),
            workout = mean(workout)) %>%
  ggplot(aes(x=as.factor(month), y=weight, group=1, colour='weight')) +
  geom_line(size=1) +
  geom_line(aes(y=workout+70, colour='alcohol'), size=1) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~.-70)) +
  theme(axis.text.x = element_text(angle=60, size=10)) 


#x = 월별, y = BMI, 음주량
men09 %>% 
  mutate(month = substr(as.character(day), 1, 6)) %>%
  group_by(month) %>%
  summarise(weight = mean(weight),
            height = mean(height),
            BMI = mean(BMI),
            alcohol = mean(alcohol),
            workout = mean(workout)) %>%
  ggplot(aes(x=as.factor(month), y=BMI, group=1, colour='BMI')) +
  geom_line(size=1) +
  geom_line(aes(y=alcohol+5, colour='alcohol'), size=1) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~.-5)) +
  theme(axis.text.x = element_text(angle=60, size=10)) 


#x = 월별, y = BMI, 운동량
men09 %>% 
  mutate(month = substr(as.character(day), 1, 6)) %>%
  group_by(month) %>%
  summarise(weight = mean(weight),
            height = mean(height),
            BMI = mean(BMI),
            alcohol = mean(alcohol),
            workout = mean(workout)) %>%
  ggplot(aes(x=as.factor(month), y=weight, group=1, colour='weight')) +
  geom_line(size=1) +
  geom_line(aes(y=workout*7+62, colour='workout'), size=1) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~./7-62)) +
  theme(axis.text.x = element_text(angle=60, size=10)) 






men09 %>% 
  mutate(month = substr(as.character(day), 1, 6)) %>%
  group_by(month) %>%
  summarise(weight = mean(weight),
            height = mean(height),
            BMI = mean(BMI),
            alcohol = mean(alcohol),
            workout = mean(workout)) %>%
  ggplot(aes(x=as.factor(month), y=BMI, group=1, colour='BMI')) +
  geom_line(size=1) +
  geom_line(aes(y=workout*2+22, colour='workout'), size=1) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~./2)) +
  theme(axis.text.x = element_text(angle=60, size=10)) 






men09 %>% 
  mutate(month = substr(as.character(day), 1, 6)) %>%
  group_by(month) %>%
  summarise(weight = mean(weight),
            height = mean(height),
            BMI = mean(BMI),
            alcohol = mean(alcohol),
            workout = mean(workout)) %>%
  View()


ggplot(l1[[1]], aes(x = day, y = height, group=1)) +
  geom_line()













setwd('C:\\Users\\sseo\\Desktop\\SAS\\100_cohort')
lf <- list.files() %>% grep('nhid_gj', ., value=T)

l1 <- lapply(1:7, function(x){
  read_sas(lf[x]) %>%
    select(HCHK_YEAR, #day
           #          SEX,
           HEIGHT,
           WEIGHT,
           #          BMI,
           DRNK_HABIT_RSPS_CD,
           TM1_DRKQTY_RSPS_CD,
           EXERCI_FREQ_RSPS_CD) %>%
    as.data.frame()
}) %>% rbindlist() %>% as.data.frame()
x <- 10
l2 <- lapply(8:12, function(x){
  read_sas(lf[x]) %>%
    select(HCHK_YEAR, #day
           #          SEX,
           HEIGHT,
           WEIGHT,
           #          BMI,
           DRNK_HABIT_RSPS_CD,
           TM1_DRKQTY_RSPS_CD,
           MOV20_WEK_FREQ_ID,
           MOV30_WEK_FREQ_ID,
           WLK30_WEK_FREQ_ID) %>%
    as.data.frame() %>%
    mutate_if(is.character, as.numeric) %>%
    filter(!is.na(HEIGHT) & !is.na(WEIGHT)) %>%
    filter(!is.na(DRNK_HABIT_RSPS_CD), !(DRNK_HABIT_RSPS_CD != 1 & is.na(TM1_DRKQTY_RSPS_CD))) %>%
    mutate(TM1_DRKQTY_RSPS_CD = ifelse(is.na(TM1_DRKQTY_RSPS_CD) & DRNK_HABIT_RSPS_CD == 1, 0, TM1_DRKQTY_RSPS_CD)) %>%
    mutate(alcohol = (DRNK_HABIT_RSPS_CD * TM1_DRKQTY_RSPS_CD * 50 * 20 * 0.785) / 7,
           workout = round((3*MOV20_WEK_FREQ_ID + 2*MOV30_WEK_FREQ_ID + WLK30_WEK_FREQ_ID) / 6, 2)) %>%
    select(-ends_with('CD'), -ends_with('ID')) -> r1
}) %>% rbindlist() %>% as.data.frame()

rowSums(t(is.na(r1)))


par(mfrow=c(3,1))
l1 %>% mutate_if(is.character, as.factor) %>% select(1) %>% table() %>% barplot()
l1 %>% mutate_if(is.character, as.factor) %>% select(2) %>% table() %>% barplot()
l1 %>% mutate_if(is.character, as.factor) %>% select(3) %>% table() %>% barplot()
l1 %>% mutate_if(is.character, as.factor) %>% select(1:2) %>% table() %>% View()

str(l2)

par(mfrow=c(5,1), mar = c(0,0,0,0))
l2 %>% mutate_if(is.character, as.factor) %>% select(1) %>% table() %>% barplot()
l2 %>% mutate_if(is.character, as.factor) %>% select(2) %>% table() %>% barplot()
l2 %>% mutate_if(is.character, as.factor) %>% select(3) %>% table() %>% barplot()
l2 %>% mutate_if(is.character, as.factor) %>% select(4) %>% table() %>% barplot()
l2 %>% mutate_if(is.character, as.factor) %>% select(5) %>% table() %>% barplot()
l2 %>%  select(1:2) %>% table() %>% View()




par(mfrow=c(4,1))
l2$MOV20_WEK_FREQ_ID %>% table() %>% barplot()
l2$MOV30_WEK_FREQ_ID %>% table() %>% barplot()
l2$WLK30_WEK_FREQ_ID %>% table() %>% barplot()
l1$EXERCI_FREQ_RSPS_CD %>% table() %>% barplot()


l1[[1]]
par(mfrow=c(3,1))
l1[[12]]$MOV20_WEK_FREQ_ID %>% table() %>% barplot()
l1[[12]]$MOV30_WEK_FREQ_ID %>% table() %>% barplot()
l1[[12]]$WLK30_WEK_FREQ_ID %>% table() %>% barplot()
apply(l1[[12]][,3:5] %>% mutate_if(is.character, as.numeric), 1, mean) %>% 
  table() %>% barplot()
l1[[1]]$EXERCI_FREQ_RSPS_CD %>% table() %>% barplot()
str(l1[[12]])

library(data.table)
lapply(1:8, function(x){
  l1[[x]]
}) %>%
  rbindlist() %>%
  
  l1[[1]] %>%
  group_by(DRNK_HABIT_RSPS_CD, TM1_DRKQTY_RSPS_CD) %>%
  summarise(n = n()) %>%
  View()





l1[[12]] %>%
  select(1:2) %>%
  group_by(DRNK_HABIT_RSPS_CD, TM1_DRKQTY_RSPS_CD) %>%
  summarise(n = n()) %>%
  View()












