#### 1단계 패키지 불러오기 #### 
library(tidyverse)
library(gtsummary)
library(psych)
library(dplyr)
library(likert)
library(gridExtra)
library(rlang)
library(forcats)
library(readxl)

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
data2 <- data %>% 
  select(Firm_Age:Business_Area) %>% 
  mutate_if(is.character, as.factor)

17 + 5+ 9

table(data$WE1)
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
levels(data2$WE2) <- c("No, Experience", "Yes, Experience")
levels(data2$WE3) <- c("No, Experience", "Yes, Experience")

tbl_summary(data2)

#### Independent Variables ####
plot(likert(items = data2[, 1:18]), ordered = FALSE) + 
  theme(axis.text = element_text(size = 16)) + 
  ggtitle("Independent Variables (Strongly Agree ~ Strongly DisAgree)")

#### Dependent Variables ####
plot(likert(items = data2[, 19:24]), ordered = FALSE) + 
  theme(axis.text = element_text(size = 16)) + 
  ggtitle("Dependent Variables (Strongly Agree ~ Strongly DisAgree)")

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


#### Descriptive Analysis ####
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

#### 5단계 Visualization - EO ####
data5 <- data %>% 
  mutate(IN = (EI_1 + EI_2 + EI_3)/3, 
         PR = (EP_1 + EP_2 + EP_3)/3, 
         RT = (EP_1 + EP_2 + EP_3)/3, 
         SD = (SS_1 + SS_2 + SS_3)/3, 
         SC = (SC_1 + SC_2 + SC_3)/3, 
         SR = (SR_1 + SR_2 + SR_3)/3, 
         FP = (F1 + F2 + F3)/3, 
         NFP = (NF1 + NF2 + NF3)/3) %>% 
  dplyr::select(-c(1:24), -Business_Area) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(WE1 = fct_collapse(WE1, 
                            "Yes"=c("As founder or employee,  I have startup experience, one time", 
                                    "As founder or employee, I have startup experiences more than 3 times", 
                                    "As founder or employee, I have startup experiences, two times"),
                            "No"="No, I don't have experience"))
  
  

myTheme <- theme(title = element_text(size = 16), 
                 axis.title.x = element_blank(), 
                 axis.text = element_text(size = 14), 
                 legend.text = element_text(size = 14),
                 legend.key = element_rect(size = 6, fill = "white", colour = "white"), 
                 legend.key.size = unit(1, "cm"),
                 legend.title = element_blank(), 
                 legend.spacing = unit(1.0, 'cm'), 
                 legend.position = "top")


bar_plot_fn <- function(.data, group_col, title) {

  .data %>% 
    dplyr::group_by({{ group_col }}) %>% 
    dplyr::summarise(Innovativeness = mean(IN), 
                     Proactiveness = mean(PR), 
                     Risk_Taking = mean(RT)) %>% 
    tidyr::gather(key = "Factors", value = "Score", -{{ group_col }}) %>% 
    mutate(Factors = factor(Factors, levels = c("Innovativeness", "Proactiveness", "Risk_Taking"))) %>% 
    ggplot(aes(x = {{ group_col }}, y = Score, fill = Factors, label = round(Score, 2))) + 
    geom_col(position = position_dodge2(padding = 0.1)) + 
    geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 6) + 
    ylim(0, 5) + 
    theme_classic() + 
    ggtitle(paste0("Entrepreneurial Orientation by ", title)) + 
    theme(title = element_text(size = 16), 
          axis.title.x = element_blank(), 
          axis.text = element_text(size = 12), 
          legend.text = element_text(size = 14),
          legend.key = element_rect(size = 6, fill = "white", colour = "white"), 
          legend.key.size = unit(1, "cm"),
          legend.title = element_blank(), 
          legend.spacing = unit(1.0, 'cm'), 
          legend.position = "top")
}

#### EO by Demographic ####
bar_plot_fn(data5, Position, "Position") -> p1
bar_plot_fn(data5, gender, "gender") -> p2
bar_plot_fn(data5, Age, "Age") -> p3
bar_plot_fn(data5, Education, "Education") -> p4

grid.arrange(p1, p2, p3, p4)

#### EO by control variables ####
bar_plot_fn(data5, Firm_Age, "Firm Age") -> p5
bar_plot_fn(data5, Firm_Size, "Firm Size") -> p6
bar_plot_fn(data5, WE1, "Startup Exprience") -> p7
bar_plot_fn(data5, WE2, "Venture Capital Exprience") -> p8
bar_plot_fn(data5, WE3, "Industry Exprience") -> p9

grid.arrange(p5, p6, p7, p8, p9)

#### Social Capital ####
bar_plot_sc <- function(.data, group_col, title) {
  
  .data %>% 
    dplyr::group_by({{ group_col }}) %>% 
    dplyr::summarise(Structural = mean(SD), 
                     Cognitive = mean(SC), 
                     Relational = mean(SR)) %>% 
    tidyr::gather(key = "Factors", value = "Score", -{{ group_col }}) %>% 
    mutate(Factors = factor(Factors, levels = c("Structural", "Cognitive", "Relational"))) %>% 
    ggplot(aes(x = {{ group_col }}, y = Score, fill = Factors, label = round(Score, 2))) + 
    geom_col(position = position_dodge2(padding = 0.1)) + 
    geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 6) + 
    ylim(0, 5) + 
    theme_classic() + 
    ggtitle(paste0("Social Capital by ", title)) + 
    theme(title = element_text(size = 16), 
          axis.title.x = element_blank(), 
          axis.text = element_text(size = 12), 
          legend.text = element_text(size = 14),
          legend.key = element_rect(size = 6, fill = "white", colour = "white"), 
          legend.key.size = unit(1, "cm"),
          legend.title = element_blank(), 
          legend.spacing = unit(1.0, 'cm'), 
          legend.position = "top")
}

#### SC by Demographic ####
bar_plot_sc(data5, Position, "Position") -> p1
bar_plot_sc(data5, gender, "gender") -> p2
bar_plot_sc(data5, Age, "Age") -> p3
bar_plot_sc(data5, Education, "Education") -> p4

grid.arrange(p1, p2, p3, p4)

#### SC by control variables ####
bar_plot_sc(data5, Firm_Age, "Firm Age") -> p5
bar_plot_sc(data5, Firm_Size, "Firm Size") -> p6
bar_plot_sc(data5, WE1, "Startup Exprience") -> p7
bar_plot_sc(data5, WE2, "Venture Capital Exprience") -> p8
bar_plot_sc(data5, WE3, "Industry Exprience") -> p9

grid.arrange(p5, p6, p7, p8, p9)


#### Startup Performance ####
bar_plot_sp <- function(.data, group_col, title) {
  
  .data %>% 
    dplyr::group_by({{ group_col }}) %>% 
    dplyr::summarise(Financial = mean(FP), 
                     Non_Financial = mean(NFP)) %>% 
    tidyr::gather(key = "Factors", value = "Score", -{{ group_col }}) %>% 
    mutate(Factors = factor(Factors, levels = c("Financial", "Non_Financial"))) %>% 
    ggplot(aes(x = {{ group_col }}, y = Score, fill = Factors, label = round(Score, 2))) + 
    geom_col(position = position_dodge2(padding = 0.1)) + 
    geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 6) + 
    ylim(0, 5) + 
    theme_classic() + 
    ggtitle(paste0("Startup Performance by ", title)) + 
    theme(title = element_text(size = 16), 
          axis.title.x = element_blank(), 
          axis.text = element_text(size = 12), 
          legend.text = element_text(size = 14),
          legend.key = element_rect(size = 6, fill = "white", colour = "white"), 
          legend.key.size = unit(1, "cm"),
          legend.title = element_blank(), 
          legend.spacing = unit(1.0, 'cm'), 
          legend.position = "top")
}

#### SC by Demographic ####
bar_plot_sp(data5, Position, "Position") -> p1
bar_plot_sp(data5, gender, "gender") -> p2
bar_plot_sp(data5, Age, "Age") -> p3
bar_plot_sp(data5, Education, "Education") -> p4

grid.arrange(p1, p2, p3, p4)

#### SC by control variables ####
bar_plot_sp(data5, Firm_Age, "Firm Age") -> p5
bar_plot_sp(data5, Firm_Size, "Firm Size") -> p6
bar_plot_sp(data5, WE1, "Startup Exprience") -> p7
bar_plot_sp(data5, WE2, "Venture Capital Exprience") -> p8
bar_plot_sp(data5, WE3, "Industry Exprience") -> p9

grid.arrange(p5, p6, p7, p8, p9)

#### Reliability ####
reliability <- read_csv('source/data/reliability.csv')
reliability$Factors <- as.factor(reliability$Factors)
fct_levls <- c("EntrepreneurialOrientation", 
               "Structural Dimension", 
               "Cognitive Dimension", 
               "Relational Dimension", 
               "Financial Performance", 
               "Non-Financial Performance")

new_fct_levels <- c("Entrepreneurial Orientation", 
                    "Structural Dimension", 
                    "Cognitive Dimension", 
                    "Relational Dimension", 
                    "Financial Performance", 
                    "Non-Financial Performance")

reliability$Factors <- factor(reliability$Factors, levels = fct_levls, 
                              labels = new_fct_levels)

glimpse(reliability)

ggplot(reliability, aes(x = Factors, y = Cronbach_Alpha)) + 
  geom_col(fill = "limegreen") + 
  geom_text(mapping = aes(label = Cronbach_Alpha),
            vjust = -1, 
            size = 8) + 
  geom_hline(yintercept = 0.7, color = "red") + 
  theme_classic() + 
  scale_x_discrete(breaks = new_fct_levels,
                   labels = c("Entrepreneurial\nOrientation", 
                              "Structural\nDimension",
                              "Cognitive\nDimension", 
                              "Relational\nDimension", 
                              "Financial\nPerformance", 
                              "Non-Financial\nPerformance")) + 
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.1), 
                     labels = seq(0, 1, by = 0.1)) + 
  labs(y = expression(paste("Cronbach's ", alpha))) + 
  theme(title = element_text(size = 24), 
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 24))

