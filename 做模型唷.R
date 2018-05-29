library(tidyverse)
library(stringr)
library(ROSE)
library(sigmoid)

abt <- read_csv("name_abt.csv")

#clean_data-------------
abt <- abt %>%
  #filter(str_detect(final_name, "[A-Za-z0-9?<@;/ -,.-]")) %>%
  mutate(name = str_replace_all(final_name, "[A-Za-z0-9?？﹌ㄨㄤㄧ　<@;/ -,.-]{1,}", ""),
         len = nchar(name)) %>%
  filter(len == 3) %>%
  select(name, sex)

abt <- abt %>%
  mutate(name = str_sub(name,2,3),
         sex = ifelse(sex==2, 0, 1),
         name_1st = str_sub(name,1,1),
         name_2nd = str_sub(name,2,2))

#測試組
set.seed(203)
test.index <- sample(1:length(abt$name), length(abt$name) * 0.3)
abt.train <- abt[-test.index,]
abt.test <- abt[test.index,]

#Ann的規則---------------------
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

#拆字新規則---------------------
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
  mutate(pred_tot = ifelse(cnt_tot < 10, NA, pred_tot),
         pred = pred_tot,
         pred = ifelse(is.na(pred), sigmoid(pred_1st + pred_2nd - 1), pred),
         #pred = ifelse(is.na(pred), (pred_1st*cnt_1st + pred_2nd*cnt_2nd)/(cnt_1st+cnt_2nd), pred),
         #pred = ifelse(pred < (sigmoid(pred_1st + pred_2nd - 1)), (sigmoid(pred_1st + pred_2nd - 1)), pred),
         #pred = ifelse(pred < (pred_1st*cnt_1st + pred_2nd*cnt_2nd)/(cnt_1st+cnt_2nd), (pred_1st*cnt_1st + pred_2nd*cnt_2nd)/(cnt_1st+cnt_2nd), pred),
         pred = ifelse(is.na(pred), runif(1), pred))


tmp
#mutate(pred = ifelse(is.na(pred), runif(1), pred))
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

#拆字新規則，貝氏定理---------------------
a <- abt.train %>%
  count(name, sex) %>%
  group_by(sex) %>%
  mutate(cnt = sum(n)) %>%
  ungroup() %>%
  mutate(note = n / cnt) %>%
  select(name, sex, note) %>%
  spread(sex, note)
a[,c(2,3)] <- a[,c(2,3)] %>% sapply(function(x) ifelse(is.na(x), 0, x))
names(a)[2:3] <- c('F_tot', 'M_tot')


b <- abt.train %>%
  count(name_1st, sex) %>%
  group_by(sex) %>%
  mutate(cnt = sum(n)) %>%
  ungroup() %>%
  mutate(note = n / cnt) %>%
  select(name_1st, sex, note) %>%
  spread(sex, note)
b[,c(2,3)] <- b[,c(2,3)] %>% sapply(function(x) ifelse(is.na(x), 0, x))
names(b)[2:3] <- c('F_1st', 'M_1st')

c <- abt.train %>%
  count(name_2nd, sex) %>%
  group_by(sex) %>%
  mutate(cnt = sum(n)) %>%
  ungroup() %>%
  mutate(note = n / cnt) %>%
  select(name_2nd, sex, note) %>%
  spread(sex, note)
c[,c(2,3)] <- c[,c(2,3)] %>% sapply(function(x) ifelse(is.na(x), 0, x))
names(c)[2:3] <- c('F_2nd', 'M_2nd')


#start_predict
tmp <- left_join(abt.test, a) %>%
  left_join(b) %>%
  left_join(c)

smv <- 1e-20
sex_cnt <- abt.train %>% count(sex) %>% mutate(n_inv = 1/n)
female_rate <- sex_cnt$n_inv[1]
male_rate <- sex_cnt$n_inv[2]

tmp[tmp$sex==0,c(5:10)] <- tmp[tmp$sex==0,c(5:10)] %>% sapply(function(x) ifelse(is.na(x), smv, x))
tmp[tmp$sex==1,c(5:10)] <- tmp[tmp$sex==1,c(5:10)] %>% sapply(function(x) ifelse(is.na(x), smv, x))


tmp <- tmp %>%
  mutate(pred_F = F_tot*F_1st*F_2nd*female_rate,
         pred_M = M_tot*M_1st*M_2nd*male_rate) %>%
  mutate(pred = ifelse(pred_F >= pred_M, 0, 1))

tmp
#mutate(pred = ifelse(is.na(pred), runif(1), pred))
roc.curve(tmp$sex, tmp$pred, plotit = T)

pred.dt2 = tmp$pred
table(pred.dt2, tmp$sex) %>% prop.table() ##confusion matrix
sum(pred.dt2==tmp$sex)/nrow(tmp)## ACC
sum(pred.dt2==1 & tmp$sex==1)/(sum(pred.dt2==1)) ##precision male
sum(pred.dt2==0 & tmp$sex==0)/(sum(pred.dt2==0)) ##precision female
sum(pred.dt2==1 & tmp$sex==1)/(sum(pred.dt2==1 & tmp$sex==1) + sum(pred.dt2=="0" & tmp$sex=="1")) ##Recall

tmp %>%
  mutate(pred = ifelse(pred >= 0.5, 1, 0)) %>%
  filter(sex != pred)
