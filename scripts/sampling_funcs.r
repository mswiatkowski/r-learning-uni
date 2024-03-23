# ZADANIE 3
# Kuratorium Oświaty w Warszawie postanowiło zweryfikować wagę plecaków uczniów 
# szkół podstawowych. Z podobnych kontroli w poprzednich latach wiadomo, że 
# waga plecaków ma rozkład normalny ze średnią 2kg i odchyleniem standardowym 
# 0,2kg. Ile plecaków należy zważyć, aby na poziomie ufności 99% 
# i maksymalnym błedem 0,01kg oszacować średnią wagę plecaków?

library(stats)

# dane
mean <- 2
std <- 0.2
alfa_01 <- 0.01
alfa_05 <- 0.05
alfa_1 <- 0.1
d <- 0.01
u_alfa_01 <- qnorm(1 - alfa_01/2)
u_alfa_05 <- qnorm(1 - alfa_05/2)
u_alfa_1 <- qnorm(1 - alfa_1/2)

# kwadraty
mean_2 <- mean^2
std_2 <- std^2
alfa_2 <- alfa^2
d_2 <- d^2
u_alfa_2_01 <- u_alfa_01^2
u_alfa_2_05 <- u_alfa_05^2
u_alfa_2_1 <- u_alfa_1^2

# funkcja do obliczania n
count_n <- function(u_alfa_squared, std_squared, d_squared) {
  n <- (u_alfa_squared * std_squared)/d_squared
  return(n)
}

n_01 <- count_n(u_alfa_2_01, std_2, d_2)
n_05 <- count_n(u_alfa_2_05, std_2, d_2)
n_1 <- count_n(u_alfa_2_1, std_2, d_2)
n_01
n_05
n_1



# ZADANIE 5
# Ile dzieciaków trzeba wylosować przyjmując alfa=0,05 z pytaniem o to czy 
# są za tym, aby ministerstwo wprowadziło zakaz zadawania prac domowych. 
# Z błędem równym 3%.

# dane
alfa <- 0.05
d <- 0.03
u_alfa <- qnorm(1 - alfa/2)
p <- 0.8
q <- 1-p
N <- 10000
h_1 <- 1
std_1 <- 200
h_2 <- 2
std_2 <- 300


# kwadraty
alfa_2 <- alfa^2
d_2 <- d^2
u_alfa_2 <- u_alfa^2

