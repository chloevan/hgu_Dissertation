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

## Likert Visualisation
data2 <- data.frame(sapply(data[, 1:24], function(x) {
  factor(x, levels = c(1:5), labels = c("Strong Agree", "Agree", "Neutral", "Disagree", "Strongly Agree"))
}) %>% as.data.frame(), data[, 25:34]) %>% 
  mutate_if(is.character, as.factor)

glimpse(data2)

firm_age <- likert(items = data2[,1:9], grouping=data2[,25])
plot(firm_age)




##  
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

## 4. Visualization
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
  ggplot(aes(x = Firm_Size, y = Score, fill = factors)) + 
    geom_col(position = position_dodge()) + 
  theme_bw()
