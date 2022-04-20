####################################### PRZETWARZANIE DANYCH W R ###############################################
############################### Adam Kacprzycki DS - Politechnika Warszawska ###################################

library(dplyr)
library(tidyverse)
setwd("~/Labolatoria_R")

################################################################################################################
# --------------------------------- ZADANIE NR 1:--------------------------------------------------------------#

crypto <- readRDS('./crypto.rds')


str(crypto)

crypto %>% 
  filter(Currency == "bitcoin") %>%
  select(Date, Close) %>% 
  mutate(Date = as.Date(Date, , format = '%b %d, %Y')) %>% 
  mutate(Rate = (Close - lag(Close)) / lag(Close)) %>% 
  arrange(desc(Rate)) -> new_crypto_data ; new_crypto_data


################################################################################################################
# --------------------------------- ZADANIE NR 2:--------------------------------------------------------------#

album = read.csv("albums.csv")

# Struktura obiektu:
str(album)

# Podstawowe statystyki poszczególnych zmiennych:
summary(album)

## Rozkłąd albumów w poszczególnych latach:
rozk_album <- album %>% 
  count(year_of_pub)
rozk_album

### W którym roku nagrano najwięcej i najmniej albumów:
rozk_album %>% 
  slice_max(n, n = 1)

rozk_album %>% 
  slice_min(n, n = 1)

### Top 5 najlepiej sprzedanych albumów:

album %>% 
  slice_max(num_of_sales, n = 5)

### Top 5 najgorzej sprzedanych albumów:
album %>% 
  slice_min(num_of_sales, n = 5)

### Najpopularniejsze (top5) gatunki muzyczne według ilości sprzedanych albumów:
top_genre <- aggregate(num_of_sales ~ genre, data = album, FUN = sum)

top_genre %>%
  select(genre, num_of_sales) %>% 
  arrange(desc(num_of_sales)) %>% 
  slice_max(num_of_sales, n = 5)

top_genre %>% 
  select(genre, num_of_sales) %>% 
  arrange(desc(num_of_sales)) %>% 
  slice_min(num_of_sales, n = 5)


### Najpopularniejszy gatunek muzyczny w danym roku:
top_genre_year <- aggregate(num_of_sales ~ genre + year_of_pub, data = album, FUN = sum)
order_gen_num_year <- order(top_genre_year$year_of_pub,top_genre_year$num_of_sales, decreasing = TRUE)
top_genre_year_1 = top_genre_year[order_gen_num_year,]

top_genre_year_1 %>% 
  group_by(year_of_pub) %>% 
  filter(num_of_sales == max(num_of_sales))

### Dno dna, czyli najgorszy album, pod kątem ocen i wielkości sprzedaży:

dno <- album %>% 
  select(album_title, genre, num_of_sales, rolling_stone_critic, mtv_critic, music_maniac_critic) %>%
  filter(rolling_stone_critic == 0.5, mtv_critic == 0.5, music_maniac_critic == 0.5) %>% 
  arrange(num_of_sales)

head(dno, n = 1)

### Top of the top, czyli najlepszy album, pod kątem ocen i wielkości sprzedaży:

top_of_the_top <- album %>% 
  select(album_title, genre, num_of_sales, rolling_stone_critic, mtv_critic, music_maniac_critic) %>%
  filter(rolling_stone_critic == 5, mtv_critic == 5, music_maniac_critic == 5) %>% 
  arrange(desc(num_of_sales))

head(top_of_the_top, n = 1)

################################################################################################################
# --------------------------------- ZADANIE NR 3:--------------------------------------------------------------#

suicides <- readRDS('./suicides.rds')

str(suicides)

## Polecenie 1

suicides %>% 
  group_by(country, year) %>% 
  filter(1985 <= year, year <= 2016) %>% 
  summarise(population_100k = sum(suicides_no)/ (sum(population)/100000),
  .groups = 'drop') -> pop_100

#top 3 najwyższe wskaźniki:
pop_100 %>% 
  slice_max(population_100k,n = 3)

#kraje o najniższym wskaźniku:
pop_100 %>% 
  slice_min(population_100k,n = 3)

## Polecenie 2

suicides %>% 
  group_by(year) %>% 
  summarise(population_100k = sum(suicides_no)/ (sum(population)/100000),
  .groups = 'drop')

## Polecenie 3

suicides %>%
  group_by(sex, age) %>%
  summarise(population_100k = sum(suicides_no)/ (sum(population)/100000)) %>%
  ungroup()

## Polecenie 4

suicides %>%
  group_by(year, country) %>%
  summarise(suicides_no = sum(suicides_no)) %>%
  arrange(year, desc(suicides_no)) %>% 
  slice_max(suicides_no, n = 3)
  
    
suicides %>%
  group_by(year, country) %>%
  summarise(suicides_no = sum(suicides_no)) %>%
  slice_max(suicides_no, n = 3) %>% 
  arrange(year, desc(suicides_no)) %>%
  summarise(sucicides_no = paste0(suicides_no, collapse ='->'),countries = paste0(country, collapse = '->')) %>% 
  as.data.frame()


## Polecenie 5

suicides %>%
  group_by(country, year) %>%
  summarise(con_pop_100 = sum(suicides_no)/ (sum(population)/100000),.groups = 'drop_last') %>%
  summarise(min_suicides = min(con_pop_100), max_suicides = max(con_pop_100),.groups = 'drop_last') %>%
  mutate(diff_worst_best_suc_day = max_suicides - min_suicides) %>% 
  arrange(desc(diff_worst_best_suc_day))-> new_suicides_set

new_suicides_set %>% 
  slice_max(diff_worst_best_suc_day, n = 1)

new_suicides_set %>% 
  slice_min(diff_worst_best_suc_day, n = 1) # jest kilka krajów o takim samym współczynniku


################################################################################################################
# --------------------------------- ZADANIE NR 4:--------------------------------------------------------------#

paid_apps <- readRDS('./paid_apps.rds')
norat_apps <- readRDS('./norat_apps.rds')
free_apps <- readRDS('./free_apps.rds')

new_union_data = bind_rows(free_apps, norat_apps, paid_apps)
new_union_data

write.csv(new_union_data, file = './Google_App_Data.csv')

google_app_csv = read_csv("Google_App_Data.csv")
google_app_csv


################################################################################################################
# --------------------------------- ZADANIE NR 5:--------------------------------------------------------------#

movies <- readRDS('./movies.rds') 
ratings <- readRDS('./ratings.rds')
tags <- readRDS('./tags.rds')


# Polecenie 1

ratings %>% 
  group_by(movieId) %>%
  summarise(num_of_votes = n(), avg_rat = mean(rating)) -> rating1

movies %>% left_join(rating1, by = "movieId") -> movies

# Polecenie 2 

tags %>%
  group_by(movieId) %>%
  summarise(last_timestamp = max(timestamp)) -> tags1

movies %>% left_join(tags1, by = "movieId") -> movies

# Polecenie 3

tags %>%
  group_by(movieId) %>%
  summarise(all_tags = paste0(tag, collapse = ' ; ')) -> all_tags

movies %>% left_join(all_tags, by = "movieId") -> movies

