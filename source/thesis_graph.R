## Thesis Graph
## 패키지 불러오기
library(tidyverse)
library(gtsummary)
library(dplyr)

## Data Import
data <- read_csv('source/data/thesis_mater.csv') %>%
  distinct() %>% # 중복데이터 제거
  rename(Position = founder_employee, # 출력을 위한 변수명 정리
         Age = age_of_respondent,
         Education = Education_Level) %>%
  slice(-c(1:10))


## 1. Data Check
glimpse(data)

## 2. Demographic of Respondents
data2 <- data %>% select(Firm_Age:Business_Area)
tbl_summary(data2)

table(data2$WE2)
table(data2$WE3)

## 3. Description Statistics
# 출처: https://rfriend.tistory.com/124
library(psych)

data3 <- data %>% select(EI_1:NF3)
describe(data3)

# 요인별 describe 추출
data4 <- data3 %>% 
  mutate(EO  = (EI_1 + EI_2 + EI_3 + EP_1 + EP_2 + EP_3 + ER_1 + ER_2 + ER_3)/9, 
         SD = (SS_1 + SS_2 + SS_3)/3, 
         SC = (SC_1 + SC_2 + SC_3)/3, 
         SR = (SR_1 + SR_2 + SR_3)/3, 
         FP = (F1 + F2 + F3)/3, 
         NFP = (NF1 + NF2 + NF3)/3) %>% 
  dplyr::select(EO:NFP) %>% 
  describe()
data4





