# https://dplyr.tidyverse.org/reference/join.html
# Right join as zip
library(tidyverse)
## library(class)
library(C50)


dt_eng <- read_csv("english.csv", col_names = c("socket_number", "packet_size"), col_types = cols(socket_number = col_integer(), packet_size = col_integer()), comment = "#") %>% filter(socket_number == 77)

dt_cn <- read_csv("chinese.csv", col_names = c("socket_number", "packet_size"), col_types = cols(socket_number = col_integer(), packet_size = col_integer()), comment = "#") %>% filter(socket_number == 76)

dt_jp <- read_csv("japanese.csv", col_names = c("socket_number", "packet_size"), col_types = cols(socket_number = col_integer(), packet_size = col_integer()), comment = "#") %>% filter(socket_number==79)

build_3_gram(dt_eng) %>% mutate(language='english')

languages <- c("English", "Chinese", "Japanese")

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
    first_half <- dt %>% slice(1 : (n() %/% 2))
    ## list(first_half %>% pull(language), second_half %>% select(-language)))
    list("first" = list("data" = first_half %>% select(-language), "class" = first_half %>% pull(language)), "second" = list("data" = second_half %>% select(-language), "class" = second_half %>% pull(language)))
}

# https://hollyemblem.medium.com/training-and-test-dataset-creation-with-dplyr-41d9aa7eab31

eng_train_and_test <- dt_eng %>% build_3_gram() %>% label_and_split('English')
cn_train_and_test <- dt_cn %>% build_3_gram() %>% label_and_split('Chinese')
jp_train_and_test <- dt_jp %>% build_3_gram() %>% label_and_split('Japanese')

eng_train <- eng_train_and_test$first
eng_test <- eng_train_and_test$second
cn_train <- cn_train_and_test$first
cn_test <- cn_train_and_test$second
jp_train <- jp_train_and_test$first
jp_test <- jp_train_and_test$second

## knn(bind_rows(eng_train$data,cn_train$data,jp_train$data), bind_rows(eng_test$data,cn_test$data,jp_test$data), c(eng_train$class,cn_train$class,jp_train$class), k = 50)
m <- C5.0(bind_rows(eng_train$data,cn_train$data,jp_train$data), fct_c(eng_train$class,cn_train$class,jp_train$class))

cn_p <- predict(m, cn_test$data, type="class")
print(sum(cn_p == "Chinese")/length(cn_p)) 

eng_p <- predict(m, eng_test$data, type="class")
print(sum(eng_p == "English")/length(eng_p)) 

jp_p <- predict(m, jp_test$data, type="class")
print(sum(jp_p == "Japanese")/length(jp_p)) 
