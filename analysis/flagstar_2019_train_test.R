rm(list = ls())

#set directory
#insert path to analysis directory
setwd("")

#restrict to only flagship
flag <- read.csv("data/flag.csv")

#ignore gender and age and consider minority based only on race.
flag <- flag %>%
  select(-matches("gender*"),-matches("senior*"))
#consider only black borrowers as minority
flag <- flag %>% 
  mutate(minority = black.TRUE,
         minority = ifelse(minority >= 1,1,0))
flag <- flag %>%
  select(-matches("*TRUE"),-matches("*FALSE"))

#drop units149
flag <- flag %>% 
  select(-units..149)
#drop balloon1 and baloon2
flag <- flag %>% 
  select(-c(balloon.1,balloon.2))

#look at 2019 only
flag <- flag %>% 
  filter(year == 2019) %>% 
  select(-year)

#standardize
flag <- flag %>% 
  mutate(across(-c(ID,denied,minority),~ (.x - mean(.x)) / sd(.x)))

#train - test split
mean(ifelse(flag$denied == -1,0,1))
mean(flag$minority)

#80-20
set.seed(1595)
smp_size <- floor(0.80 * nrow(flag))
train_ind <- sample(seq_len(nrow(flag)), size = smp_size)
train <- flag[train_ind, ]
test <- flag[-train_ind, ]

#checking percentages
mean(ifelse(train$denied == -1,0,1))
mean(train$minority)
mean(ifelse(test$denied == -1,0,1))
mean(test$minority)
#they are fine

#balancing the dataset.
#keep all denials and get same number of approvals
num_to_keep_den <- sum(train$denied == 1)
num_to_keep_app <- sum(train$denied == 1)
train %>% write.csv("all_train.csv")
train <- train %>% 
  filter(denied == 1) %>% 
  sample_n(size = num_to_keep_den) %>% 
  rbind(train %>% 
          filter(denied == -1) %>% 
          sample_n(size = num_to_keep_app)
  )

mean(ifelse(train$denied == -1,0,1))
mean(train$minority)

train %>% write.csv("train.csv",row.names = FALSE)
test %>% write.csv("test.csv",row.names = FALSE)