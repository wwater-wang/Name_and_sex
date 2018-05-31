library(tidyverse)
library(stringr)
library(ROSE)
library(sigmoid)

abt <- read_csv("name_abt.csv")

#clean_data-------------
abt <- abt %>%
  mutate(name = str_replace_all(final_name, "[A-Za-z0-9?？﹌ㄨㄤㄧ　<@;/ -,.-]{1,}", ""),
         len = nchar(name)) %>%
  filter(len == 3) %>%
  select(name, sex)

abt <- abt %>%
  mutate(name = str_sub(name,2,3),
         sex = ifelse(sex==2, 0, 1),
         name_1st = str_sub(name,1,1),
         name_2nd = str_sub(name,2,2))

#test group
set.seed(203)
test.index <- sample(1:length(abt$name), length(abt$name) * 0.3)
abt.train <- abt[-test.index,]
abt.test <- abt[test.index,]

#Use entirely 2 words---------------------
pred <- abt.train %>%
  group_by(name) %>%
  summarise(pred = mean(sex)) %>%
  ungroup()

#start_predict
tmp <- left_join(abt.test, pred) %>%
  #filter(!is.na(pred)) %>%
  mutate(pred = ifelse(is.na(pred), runif(1), pred))

roc.curve(tmp$sex, tmp$pred, plotit = T)
pred.dt2 = ifelse(tmp$pred>=0.5,1,0)
table(pred.dt2, tmp$sex) %>% prop.table() ##confusion matrix
sum(pred.dt2==tmp$sex)/nrow(tmp)## ACC
sum(pred.dt2==1 & tmp$sex==1)/(sum(pred.dt2==1)) ##precision male
sum(pred.dt2==0 & tmp$sex==0)/(sum(pred.dt2==0)) ##precision female
sum(pred.dt2==1 & tmp$sex==1)/(sum(pred.dt2==1 & tmp$sex==1) + sum(pred.dt2=="0" & tmp$sex=="1")) ##Recall

#Try to use each words + whole words---------------------
a <- abt.train %>%
  group_by(name) %>%
  summarise(pred_tot = mean(sex),
            cnt_tot = n()) %>%
  ungroup()

b <- abt.train %>%
  group_by(name_1st) %>%
  summarise(pred_1st = mean(sex),
            cnt_1st = n()) %>%
  ungroup()

c <- abt.train %>%
  group_by(name_2nd) %>%
  summarise(pred_2nd = mean(sex),
            cnt_2nd = n()) %>%
  ungroup()

#start_predict
tmp <- left_join(abt.test, a) %>%
  left_join(b) %>%
  left_join(c)

tmp[,c(7,9)] <- tmp[,c(7,9)] %>%
  sapply(function(x) ifelse(is.na(x), 0.5, x))

tmp[,c(6,8,10)] <- tmp[,c(6,8,10)] %>%
  sapply(function(x) ifelse(is.na(x), 0, x))

tmp <- tmp %>%
  mutate(pred_tot = ifelse(abs(pred_tot - 0.5) < 1.96*sqrt((pred_tot*(1-pred_tot))/cnt_tot),
                           NA, pred_tot),
         pred = pred_tot,
         pred = ifelse(is.na(pred), sigmoid((pred_1st + pred_2nd - 1)*4), pred),
         pred = ifelse(is.na(pred), runif(1), pred))

#performance
roc.curve(tmp$sex, tmp$pred, plotit = T)
pred.dt2 = ifelse(tmp$pred>=0.5,1,0)
table(pred.dt2, tmp$sex) %>% prop.table() ##confusion matrix
sum(pred.dt2==tmp$sex)/nrow(tmp)## ACC
sum(pred.dt2==1 & tmp$sex==1)/(sum(pred.dt2==1)) ##precision male
sum(pred.dt2==0 & tmp$sex==0)/(sum(pred.dt2==0)) ##precision female
sum(pred.dt2==1 & tmp$sex==1)/(sum(pred.dt2==1 & tmp$sex==1) + sum(pred.dt2=="0" & tmp$sex=="1")) ##Recall

tmp %>%
  mutate(pred = ifelse(pred >= 0.5, 1, 0)) %>%
  filter(sex != pred)