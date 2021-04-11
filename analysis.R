# https://juliasilge.com/blog/tidy-text-classification/
# https://dplyr.tidyverse.org/reference/join.html
# Right join as zip
library(tidyverse)
library(class)
library(tidytext)

dt_cn <- read_csv("zhxs_1_hour_558_m.csv", col_names = c("packet_size"), col_types = cols(packet_size = col_integer()), comment = "#")

dt_jp <- read_csv("ma_1_hour_m.csv", col_names = c("packet_size"), col_types = cols(packet_size = col_integer()), comment = "#")



languages <- c("Japanese", "Chinese")

build_3_gram <- function(dt0) {
  dt1 <- dt0 %>%
    slice(-1) %>%
    rename(next_packet_size = packet_size)
  dt2 <- dt1 %>%
    slice(-1) %>%
    rename(next_next_packet_size = next_packet_size)
  dt3 <- dt0 %>%
    mutate(id = row_number()) %>%
    right_join(dt1 %>% mutate(id = row_number())) %>%
    right_join(dt2 %>% mutate(id = row_number())) %>%
    select(-id) %>%
    unite(sizes, c("packet_size", "next_packet_size", "next_next_packet_size"))
}

split_packets <- function(dt, lang) {
  dt0 <- dt %>%
    build_3_gram() %>%
    mutate(group = (row_number() - 1) %/% (n() / 60)) %>%
    group_by(group, sizes) %>%
    count()
  dt1 <- left_join(dt0, dt0 %>% ungroup(sizes) %>% summarize(n_total = sum(n))) %>%
    mutate(freq = n / n_total) %>%
    select(-n_total, -n) %>%
    mutate(language = factor(lang, levels = languages))
  dt1 %>% ungroup()
}

dt_cn.g <- dt_cn %>% split_packets("Chinese")
dt_jp.g <- dt_jp %>% split_packets("Japanese")

dt_all <- bind_rows(dt_cn.g, dt_jp.g) %>% pivot_wider(names_from = sizes, values_from = freq, values_fill = 0)

count_total <- function(dt) {
  dt %>%
    group_by(group) %>%
    summarize() %>%
    ungroup() %>%
    count() %>%
    pull(n)
}

tag_new_id <- function(dt, x) {
  dt %>%
    mutate(tmp = x) %>%
    unite(new_id, c(group, tmp), remove = TRUE)
}

dt_train <- dt_all %>%
  filter(group < 30) %>%
  select(-group, -language)
dt_test_jp <- dt_all %>%
  filter(group >= 30, language == "Japanese") %>%
  select(-group, -language)
dt_test_cn <- dt_all %>%
  filter(group >= 30, language == "Chinese") %>%
  select(-group, -language)

japanese <- factor("Japanese", levels = languages)
chinese <- factor("Chinese", levels = languages)
knn(dt_train, dt_test_jp, fct_c(rep(chinese, 30), rep(japanese, 30)))
knn(dt_train, dt_test_cn, fct_c(rep(chinese, 30), rep(japanese, 30)))
