library(tidyverse)
library(car)
library(broom)
library(DescTools)
library(ROCR)
library(lmtest)
library(dplyr)
library(naniar)
library(MASS)
library(e1071)
library(readxl)
library(olsrr)
library(survival)

library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(ggsurvfit)
library(survminer)

time = c(0:16)
survival = c(1, .9, .9, .9, .8, .7, .7,
             .58, .58, .58, .58,.58, .58,
             .19,.19, .095, 0.095)

plot(time,survival,'s')



survival_data = matrix(c(
  1, 681, 0,
  1, 602, 0,
  1, 996, 0,
  1, 1162, 0,
  1, 833, 0,
  1, 477, 0,
  1, 630, 0,
  1, 596, 0,
  1, 226, 0,
  1, 699, 0,
  1, 811, 0,
  1, 530, 0,
  1, 482, 0,
  1, 367, 0,
  1, 118, 1,
  1, 83, 1,
  1, 76, 1,
  1, 104, 1,
  1, 109, 1,
  1, 72, 1,
  1, 87, 1,
  1, 162, 1,
  1, 94, 1,
  1, 30, 1,
  1, 26, 1,
  1, 22, 1,
  1, 49, 1,
  1, 74, 1,
  1, 122, 1,
  1, 86, 1,
  1, 66, 1,
  1, 92, 1,
  1, 109, 1,
  1, 255, 1,
  1, 1, 1,
  1, 107, 1,
  1, 110, 1,
  1, 232, 1,
  2, 2569, 0,
  2, 2506, 0,
  2, 2409, 0,
  2, 2218, 0,
  2, 1857, 0,
  2, 1829, 0,
  2, 1562, 0,
  2, 1470, 0,
  2, 1363, 0,
  2, 1030, 0,
  2, 1860, 0,
  2, 1258, 0,
  2, 2246, 0,
  2, 1870, 0,
  2, 1799, 0,
  2, 1709, 0,
  2, 1674, 0,
  2, 1568, 0,
  2, 1527, 0,
  2, 1324, 0,
  2, 1957, 0,
  2, 1932, 0,
  2, 1847, 0,
  2, 1848, 0,
  2, 1850, 0,
  2, 1843, 0,
  2, 1535, 0,
  2, 1447, 0,
  2, 1384, 0,
  2, 914, 1,
  2, 2204, 1,
  2, 1063, 1,
  2, 481, 1,
  2, 605, 1,
  2, 641, 1,
  2, 390, 1,
  2, 288, 1,
  2, 421, 1,
  2, 1379, 1,
  2, 1748, 1,
  2, 486, 1,
  2, 448, 1,
  2, 272, 1,
  2, 1074, 1,
  2, 1381, 1,
  2, 1410, 1,
  2, 1353, 1,
  2, 1480, 1,
  2, 435, 1,
  2, 248, 1,
  2, 1704, 1,
  2, 1411, 1,
  2, 219, 1,
  2, 606, 1,
  3, 2640, 0,
  3, 2430, 0,
  3, 2252, 0,
  3, 2140, 0,
  3, 2133, 0,
  3, 1738, 0,
  3, 2631, 0,
  3, 2524, 0,
  3, 1845, 0,
  3, 1936, 0,
  3, 1845, 0,
  3, 422, 1,
  3, 162, 1,
  3, 84, 1,
  3, 100, 1,
  3, 212, 1,
  3, 47, 1,
  3, 242, 1,
  3, 456, 1,
  3, 268, 1,
  3, 318, 1,
  3, 732, 1,
  3, 467, 1,
  3, 947, 1,
  3, 390, 1,
  3, 183, 1,
  3, 105, 1,
  3, 115, 1,
  3, 164, 1,
  3, 693, 1,
  3, 120, 1,
  3, 80, 1,
  3, 677, 1,
  3, 64, 1,
  3, 168, 1,
  3, 874, 1,
  3, 616, 1,
  3, 157, 1,
  3, 625, 1,
  3, 48, 1,
  3, 273, 1,
  3, 163, 1,
  3, 376, 1,
  3, 113, 1,
  3, 363, 1), ncol = 3, byrow = T)

view(survival_data)

colnames(survival_data) <- c('drug_group', 'time', 'event')

view(survival_data)

survival_df = data.frame(survival_data)

survival_fit <- survfit(Surv(time, event) ~ drug_group, data = survival_df)
autoplot(survival_fit)

summary(survival_fit)


ggsurvfit::survfit2(Surv(time, event) ~ 1, data = survival_df) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Survival probability"
  ) + 
  add_confidence_interval() +
  add_risktable()


survdiff(Surv(time, event) ~ drug_group, data = survival_df)

cox = coxph(Surv(time, event) ~ drug_group, data = survival_df)
summary(cox)


pw_surv = survminer::pairwise_survdiff(Surv(time, event) ~ drug_group, data = survival_df)
pw_surv



