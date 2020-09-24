#### 1단계 패키지 불러오기 #### 
library(tidyverse)
library(gtsummary)
library(psych)
library(dplyr)
library(likert)
library(gridExtra)

#### 2단계 Data Import ####
data <- read_csv('source/data/thesis_mater.csv') %>%
  distinct() %>% # 중복데이터 제거
  rename(Position = founder_employee, # 출력을 위한 변수명 정리
         Age = age_of_respondent,
         Education = Education_Level) %>%
  slice(-c(1:10))


#### 3단계 데이터 확인 #### 
glimpse(data)

#### 4단계 통계 지표 산출 #### 
data2 <- data %>% select(Firm_Age:Business_Area)
tbl_summary(data2)

table(data2$WE2)
table(data2$WE3)

#### (1) Description Statistics ####
# 출처: https://rfriend.tistory.com/124

data3 <- data %>% select(EI_1:NF3)
describe(data3)

#### (2) Likert Visualisation with Entrepreneurial Orientation #### 
data2 <- data.frame(lapply(data[, 1:24], function(x) {
  factor(x, levels = c(1:5), labels = c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree"))
}) %>% as.data.frame(), data[, 25:34])
data2$Firm_Age <- factor(data2$Firm_Age, levels = c("Less than 2 years", "3-4 years", "5 years above"))
data2$Firm_Size <- factor(data2$Firm_Size, levels = c("Less than 5 members", 
                                                      "5-9 members", 
                                                      "10-14 members", 
                                                      "Above 15 members"))
data2$WE2 <- as.factor(data2$WE2)
data2$WE3 <- as.factor(data2$WE3)

#### (~가) EO ####
EO_likert_firmAge <- likert(items = data2[,1:9], grouping=data2[,25])
plot(EO_likert_firmAge, ordered = TRUE)

EO_likert_firmSize <- likert(items = data2[,1:9], grouping=data2[,26])
plot(EO_likert_firmSize, ordered = TRUE)

EO_likert_WE2 <- likert(items = data2[,1:9], grouping=data2[,28])
plot(EO_likert_WE2, ordered = TRUE)

EO_likert_WE3 <- likert(items = data2[,1:9], grouping=data2[,28])
plot(EO_likert_WE3, ordered = TRUE)


#### (~나) SC ####
SC_likert_firmAge <- likert(items = data2[,10:18], grouping=data2[,25])
plot(SC_likert_firmAge, ordered = TRUE)

SC_likert_firmSize <- likert(items = data2[,10:18], grouping=data2[,26])
plot(SC_likert_firmSize, ordered = TRUE)

SC_likert_WE2 <- likert(items = data2[,10:18], grouping=data2[,28])
plot(SC_likert_WE2, ordered = TRUE)

SC_likert_WE3 <- likert(items = data2[,10:18], grouping=data2[,28])
plot(SC_likert_WE3, ordered = TRUE)

#### (~다) Startup Performance ####
SP_likert_firmAge <- likert(items = data2[,19:24], grouping=data2[,25])
plot(SP_likert_firmAge, ordered = TRUE)

SP_likert_firmSize <- likert(items = data2[,19:24], grouping=data2[,26])
plot(SP_likert_firmSize, ordered = TRUE)

SP_likert_WE2 <- likert(items = data2[,19:24], grouping=data2[,28])
plot(SP_likert_WE2, ordered = TRUE)

SP_likert_WE3 <- likert(items = data2[,19:24], grouping=data2[,28])
plot(SP_likert_WE3, ordered = TRUE)

data4 <- data3 %>% 
  mutate(IN = (EI_1 + EI_2 + EI_3)/3, 
         PR = (EP_1 + EP_2 + EP_3)/3, 
         RT = (EP_1 + EP_2 + EP_3)/3, 
         SD = (SS_1 + SS_2 + SS_3)/3, 
         SC = (SC_1 + SC_2 + SC_3)/3, 
         SR = (SR_1 + SR_2 + SR_3)/3, 
         FP = (F1 + F2 + F3)/3, 
         NFP = (NF1 + NF2 + NF3)/3) %>% 
  dplyr::select(IN:NFP) %>% 
  describe()
data4

#### 5단계 Visualization ####
data %>% 
  mutate(IN = (EI_1 + EI_2 + EI_3)/3, 
         PR = (EP_1 + EP_2 + EP_3)/3, 
         RT = (EP_1 + EP_2 + EP_3)/3) %>% 
  dplyr::select(Firm_Age:RT) %>% 
  group_by(Firm_Size) %>% 
  summarise(IN = mean(IN), 
            PR = mean(PR), 
            RT = mean(RT)) %>% 
  mutate(Firm_Size = as.factor(Firm_Size)) %>% 
  gather(key = "Factors", value = "Score", -Firm_Size) %>% 
  mutate(factors = factor(Factors, levels = c("IN", "PR", "RT"))) %>% 
  ggplot(aes(x = Firm_Size, y = Score, fill = factors, label = round(Score, 2))) + 
    geom_col(position = position_dodge()) + 
    geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 7) + 
  theme_bw() + 
  theme(title = element_text(size = 24), 
        axis.text = element_text(size = 20), legend.text = element_text(size = 20))

