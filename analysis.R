# https://dplyr.tidyverse.org/reference/join.html
# Right join as zip
library(tidyverse)
## library(class)
library(C50)
## library(neuralnet)


dt_en <- read_csv("english.csv", col_names = c("socket_number", "packet_size"), col_types = cols(socket_number = col_integer(), packet_size = col_integer()), comment = "#") %>% filter(socket_number == 77)

dt_cn <- read_csv("chinese.csv", col_names = c("socket_number", "packet_size"), col_types = cols(socket_number = col_integer(), packet_size = col_integer()), comment = "#") %>% filter(socket_number == 76)

dt_cn_contrib <- read_csv("cn_contrib.csv", col_names = c("socket_number", "packet_size"), col_types = cols(socket_number = col_integer(), packet_size = col_integer()), comment = "#") %>% filter(socket_number == 65)


dt_jp <- read_csv("japanese.csv", col_names = c("socket_number", "packet_size"), col_types = cols(socket_number = col_integer(), packet_size = col_integer()), comment = "#") %>% filter(socket_number==79)

dt_gsl <- read_csv("gsl_eng.csv", col_names = c("socket_number", "packet_size"), col_types = cols(socket_number = col_integer(), packet_size = col_integer()), comment = "#") %>% filter(socket_number==78)

## build_3_gram(dt_eng) %>% mutate(language='english')

languages <- c("Chinese", "Japanese", "English")

build_3_gram <- function(dt) {
    ## v_sock <- dt %>% group_by(socket_number) %>% summarise(packet_size=sum(packet_size)) %>% arrange(desc(packet_size)) %>% top_n(1) %>% pull(socket_number)

    dt0 <- dt %>%  select(-socket_number)
    dt1 <- dt0 %>% slice(-1) %>% rename(next_packet_size=packet_size)
    dt2 <- dt1 %>% slice(-1) %>% rename(next_next_packet_size=next_packet_size)
    dt0 %>% mutate(id = row_number()) %>% right_join(dt1 %>% mutate(id = row_number())) %>% right_join(dt2 %>% mutate(id = row_number())) %>% select(-id)
}

label_and_split <- function(dt, label) {
    dt <- dt %>% mutate(language = factor(label, levels=languages))
    second_half <- dt %>% slice((n() %/% 2 + 1) : n())
    first_half <- dt
    ## list(first_half %>% pull(language), second_half %>% select(-language)))
    list("first" = list("data" = first_half %>% select(-language), "class" = first_half %>% pull(language)), "second" = list("data" = second_half %>% select(-language), "class" = second_half %>% pull(language)))
}

# https://hollyemblem.medium.com/training-and-test-dataset-creation-with-dplyr-41d9aa7eab31

en_train_and_test <- dt_en %>% build_3_gram() %>% label_and_split('English')
cn_train_and_test <- dt_cn %>% build_3_gram() %>% label_and_split('Chinese')
jp_train_and_test <- dt_jp %>% build_3_gram() %>% label_and_split('Japanese')

en_train <- en_train_and_test$first
en_test <- en_train_and_test$second
cn_train <- cn_train_and_test$first
cn_test <- cn_train_and_test$second
jp_train <- jp_train_and_test$first
jp_test <- jp_train_and_test$second

## knn(bind_rows(gsl_train$data,cn_train$data,jp_train$data), bind_rows(gsl_test$data,cn_test$data,jp_test$data), c(gsl_train$class,cn_train$class,jp_train$class), k = 50)


gsl_test <- dt_gsl %>% build_3_gram() %>% mutate(language= factor('English', levels=languages))

m <- C5.0(bind_rows(en_train$data,cn_train$data,jp_train$data, gsl_test %>% select(-language)), fct_c(en_train$class,cn_train$class,jp_train$class, gsl_test %>% pull(language)))

## cn_p <- predict(m, cn_test$data, type="class")
## print(sum(cn_p == "Chinese")/length(cn_p)) 

## en_p <- predict(m, en_test$data, type="class")
## print(sum(en_p == "English")/length(en_p)) 

## jp_p <- predict(m, jp_test$data, type="class")
## print(sum(jp_p == "Japanese")/length(jp_p)) 

cn_contrib_p <- predict(m, dt_cn_contrib %>% build_3_gram() %>% mutate(language = factor('Chinese', levels=languages)) , type="class")
print(sum(cn_contrib_p == "Chinese")/length(cn_contrib_p)) 


gsl_p <- predict(m, gsl_test %>% select(-language))
sum(gsl_p == "Chinese")/length(gsl_p)
