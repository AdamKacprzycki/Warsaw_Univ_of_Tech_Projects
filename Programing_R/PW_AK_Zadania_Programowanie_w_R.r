####################################### PROGRAMOWANIE W R ######################################################
############################### Adam Kacprzycki DS - Politechnika Warszawska ###################################

################################################################################################################
# --------------------------------- ZADANIE NR 1:--------------------------------------------------------------#

# Przykładowa kwota kredytu:
K = 800000

# Roczna stopa oprocentowania:
r = 0.032

# Okres trwania kredytu w miesiącach:
n = 360

# Obliczenia:
q = 1 + r / 12
R = K * q**n * ((q-1)/(q**n-1))
F = R * n

cat("Wysokość raty to" , format(R, digits = 2, nsmall = 2))

################################################################################################################
# --------------------------------- ZADANIE NR 2:--------------------------------------------------------------#

# Przykładowa kwota kredytu:
K = 800000

# Roczna stopa oprocentowania:
r = 0.032

# Okres trwania kredytu w miesiącach:
n = 360

# Wysokość części kapitałowej raty raty:
R0 = K/n
R0

# Wysokość częśc odsetkowej i-tej raty R1:
R1 = c()

for (i in 1:n)
  R1[i] = ((K - (i - 1)*R0)*r)/12

R1

# Wysokość raty i-tej R:
R = c()

for (i in 1:n)
  R[i] = R0 + R1[i]

R

#Całkowita kwota spłaty:
F = sum(R)
F

# Harmonogram rat:
cat(paste0('Rata nr ', 1:n, ': ', format(R, digits = 2, nsmall = 2), '\n'),sep = '')

#Średnia, najniższ i najwyższa wysokość raty:
mean_rata <- mean(R)
min_rata <- min(R)
max_rata <- max(R)

cat("Średnia wartość spłaty to:",format(mean_rata, digits = 2, nsmall = 2))
cat("Najniższa wartość spłąty to:",format(min_rata, digits = 2, nsmall = 2))
cat("Najwyższa wartość spłaty to:",format(max_rata, digits = 2, nsmall = 2))

################################################################################################################
# --------------------------------- ZADANIE NR 3:--------------------------------------------------------------#


setwd('C:\Users\Adam\Documents\Data_Science_Politechnika_Warszawska\4. Przedmioty\Labolatoria_R\Dane')
changes_WIG <- readRDS('./wig_changes.rds')

changes_matrix_WIG <- matrix(0,2,2)

namest <- c("+ / t","- / t")
namest_1 <- c("+ / t-1","- / t-1")
colnames(changes_matrix_WIG) <- namest
rownames(changes_matrix_WIG) <- namest_1

n <- length(changes_WIG)

for (i in 2:n){
  if (changes_WIG[i-1] == "+" & changes_WIG[i] == "+") {
    changes_matrix_WIG[1,1] = changes_matrix_WIG[1,1] + 1
  }
  else if (changes_WIG[i-1] == "+" & changes_WIG[i] == "-") {
    changes_matrix_WIG[1,2] = changes_matrix_WIG[1,2] + 1
  }
  else if (changes_WIG[i-1] == "-" & changes_WIG[i] == "+") {
    changes_matrix_WIG[2,1] = changes_matrix_WIG[2,1] + 1
  }
  else if (changes_WIG[i-1] == "-" & changes_WIG[i] == "-") {
    changes_matrix_WIG[2,2] = changes_matrix_WIG[2,2] + 1
  }
}

# Bazując na tym wektorze wyznacz  macierz:

changes_matrix_WIG_prob = changes_matrix_WIG / n
changes_matrix_WIG_prob

# Podnieś utworzoną macierz do potęgi ^3:

changes_matrix_WIG_pow3 = changes_matrix_WIG_prob ** 3
changes_matrix_WIG_pow3


################################################################################################################
# --------------------------------- ZADANIE NR 4A:-------------------------------------------------------------#

insurance <- function(K = 100, N = 500, F = 2500000, T = 12){
  
  S <- rep(0,T)
  
  for (t in 1:T){
    if(t == 1){
      S[t] = K * N
    } else {
      S[t] = S[t - 1] + K * N
    }
    
    c <- rt(N,2)
    a <- length(c[c >= qt(0.9999,2)])
    
    S[t] = S[t] - a * F
    
    if(S[t] < 0) {
      S[t] = 0
      break
    }
    
    N = N + sample(0:100, 1)
    N = N - sample(0:90, 1) - a
  }
  
  return(S)
}

insurance()

################################################################################################################
# --------------------------------- ZADANIE NR 4B:--------------------------------------------------------------#

M = 500
T = 12

sim_matrix = matrix(NA, M, T)

for (i in 1:M) {
  sim_matrix[i,] <- insurance(T=T)
}

sim_matrix

################################################################################################################
# --------------------------------- ZADANIE NR 5:--------------------------------------------------------------#


setwd('C:\Users\Adam\Documents\Data_Science_Politechnika_Warszawska\4. Przedmioty\Labolatoria_R\Dane')
age <- readRDS('./age.rds')

# Najmłodszy i najstarszy klient:

the_youngest <- min(age, na.rm = T)
print(paste("Najmłodszy kliet miał:", format(the_youngest, digits = 2, nsmall = 2)))

the_oldes <- max(age, na.rm = T)
print(paste("Najstarszy kliet miał:", format(the_oldes, digits = 2, nsmall = 2)))

# Przeciętny wiek kliena w Banku:

avg_client_age = mean(age, na.rm = T)
print(paste("Przeciętny wiek klienta to:", format(the_oldes, digits = 2, nsmall = 2)))

# Jak bardzo zróżnicowani są klienci pod względem wieku:

v = var(age, na.rm = T)
s = sd(age, na.rm = T)

table(age)

hist(age, breaks = 50, prob = F,
     xlab = 'Wiek',
     ylab = 'Częstość występowania',
     main = paste('Histogram wieku') )
grid()

# Ile klientów banku jest niepełnoletnich. Jaki to procent całości?

under_age = sum(age < 18, na.rm = T)
under_age_per = 100 * under_age / (length(age) - sum(is.na(age)))

# Ile klientów banku jest w wieku 30-50 lat? Jaki to procent całości?
 
client_30_50 = sum((age >= 30 & age <= 50), na.rm = T)
client_30_50_per = 100 * client_30_50 / (length(age) - sum(is.na(age)))

# Ilu klientów nie podało swojego wieku. Jaki to procent całości?

na_client = sum(is.na(age))
na_client_per = 100 * na_client / length(age)

# Ile klientów bank posiada w segmentach wiekowych [16,17], [18,24],
# [25,34], [35,44], [45,64], [65,Inf]? Jaki to procent?


age_df = data.frame(age)

age_df$seg = 0

for (i in 1:nrow(age_df)){
  if (is.na(age[i])){
      age_df[i,'seg'] = "NA"
    }else{
      if (age[i] >= 16 && age[i] <= 17) {
        age_df[i,'seg'] = "[16,17]"
      }else if (age[i]>= 18 && age[i] <= 24) {
        age_df[i,'seg'] = "[18,24]"
      }else if (age[i] >= 25 && age[i] <= 34) {
        age_df[i,'seg'] = "[25,34]"
      }else if (age[i] >= 35 && age[i] <= 44) {
        age_df[i,'seg'] = "[35,44]"
      }else if (age[i] >= 45 && age[i] <= 64) {
        age_df[i,'seg'] = "[45,64]"
      }else if (age[i] >= 65){
        age_df[i,'seg'] = "[65,Inf]"
      }}
}

agg_quant_cliect_age = table(age_df$seg)
agg_quant_cliect_age

agg_proc_table_age = 100 * prop.table(table(age_df$seg))
agg_proc_table_age


################################################################################################################
# --------------------------------- ZADANIE NR 6:--------------------------------------------------------------#

c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)-> ctl
c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)-> trt
gl(2, 10, 20, labels = c("Ctl", "Trt "))-> group
c(ctl,trt)-> weight
summary(lm(weight ~ group)) -> model

# przyjrzyj się strukturze obiektu modelu,

str(model)

# znajdz i wyświetl współczynniki modelu (coefficients).

model$coefficients[,'Estimate']

# najdz i wyświetl wartości resztowe modelu (residuals).

model$residuals

# najdz i wyświetl wartości dopasowanego R2 adj.r.squared

model$adj.r.squared

################################################################################################################
# --------------------------------- ZADANIE NR 7:--------------------------------------------------------------#

ugly_diamonds = read.csv("ugly_diamonds.csv", header = TRUE, sep = '%', dec = ',', na.strings = "Brak Danych",
stringsAsFactors = FALSE, skip = 4)


str(ugly_diamonds)


################################################################################################################
# --------------------------------- ZADANIE NR 8:--------------------------------------------------------------#

setwd('C:\Users\Ela\Documents\Data_Science_Politechnika_Warszawska\4. Przedmioty\Labolatoria_R\Dane')
bank_data = readRDS("bank_register.rds")

as.numeric(sapply(strsplit(as.character(bank_data$id),'_'), "[", 1)) -> bank_data$client_id
as.numeric(sapply(strsplit(as.character(bank_data$id),'_'), "[", 2)) -> bank_data$agreement_id
NULL -> bank_data$id

sapply(strsplit(as.character(bank_data$demographic),','), "[", 1) -> bank_data$sex
as.numeric(sapply(strsplit(as.character(bank_data$demographic),','), "[", 2)) -> bank_data$age
as.numeric(sapply(strsplit(as.character(bank_data$demographic),','), "[", 3)) -> bank_data$child
NULL -> bank_data$demographic

str_detect(bank_data$products, 'DEP') -> bank_data$dep
str_detect(bank_data$products, 'CRE') -> bank_data$cre
str_detect(bank_data$products, 'MOR') -> bank_data$mor
NULL = bank_data$products

substr(bank_data$income,1,nchar(bank_data$income)-1) -> bank_data$income
sub("\\.", "", bank_data$income) -> bank_data$income
gsub(",", ".", bank_data$income) -> bank_data$income
as.numeric(bank_data$income) -> bank_data$income

bank_data$date <- as.Date(strptime(as.character(bank_data$date), "%b %d,%Y"))

bank_data <- bank_data[c("client_id", "agreement_id", "date", "income", "sex", "age", "child", "dep", "cre", "mor")]

str(bank_data)

head(bank_data)


################################################################################################################
# --------------------------------- ZADANIE NR 8:--------------------------------------------------------------#

#Ze wzgędu na brak ograniczeń co do zastosowania pakietu, do analizy wykorzystałem dpylr (którego będę wykorzystywać w kolejnych zadaniach domowych).

setwd('C:\Users\Ela\Documents\Data_Science_Politechnika_Warszawska\4. Przedmioty\Labolatoria_R\Dane')
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

         
         
         