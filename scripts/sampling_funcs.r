# ZADANIE 1
# W celu zbadania średnich zarobków kobiet w różnych branżach w pewnym 
# województwie, przeprowadzono badanie na 360 tys. firm. Populację podzielono 
# na trzy grupy: firmy usługowe, firmy produkcyjno-handlowe i firmy sektora 
# publicznego. Zastosowano dobór nieproporcjonalny. Przyjęto 5% poziom 
# istotnosci oraz założono ze maksymalny bład oceny przeciętnego wynagrodzenia 
# wynosi 15zł. Jaka powinna być minimalna liczebność próby?
  

# Na początek przygotowujemy sobie dane:
d <- 15                       # błąd maksymalny
d_squared <- d^2              # jego kwadrat (do wzoru)
alfa <- 0.05                  # współczynnik alfa
u_alfa <- qnorm(1 - alfa/2)   # u alfa
u_alfa_squared <- u_alfa^2    # kwadrat u_alfa (do wzoru)
N <- 340000                   # liczebność całkowita (da się ją wyliczyć, ale lepiej tu już wpisać, bo to stała)


#################################################
# Teraz wyliczymy dane dla poszczególnych warstw:

# firmy usługowe:
N_h1 <- 100000                # liczebność warstwy h1
S_h1 <- 750                   # odchylenie standardowe warstwy h1
S_h1_squared <- S_h1^2        # wariancja warstwy h1
W_h1 <- N_h1 / N         # waga warstwy h2 bez uwzględnienia odchylenia standardowego
weight_of_S_h1 <- S_h1 * W_h1     # waga razy odchylenie standardowe
                                       # potrzebne do obliczenia wag firm
                                       # po uwzględnieniu odchylenia std


# firmy produkcyjno-handlowe:
N_h2 <- 150000                # liczebność warstwy h2
S_h2 <- 485                   # odchylenie standardowe warstwy h2
S_h2_squared <- S_h2^2        # wariancja warstwy h2
W_h2 <- N_h2 / N         # waga warstwy h2 bez uwzględnienia odchylenia standardowego
weight_of_S_h2 <- S_h2 * W_h2     # waga razy odchylenie standardowe
                                       # potrzebne do obliczenia wag firm
                                       # po uwzględnieniu odchylenia std

# firmy sektora publicznego:
N_h3 <- 90000                 # liczebność warstwy h3
S_h3 <- 394                   # odchylenie standardowe warstwy h3
S_h3_squared <- S_h3^2        # wariancja warstwy h3
W_h3 <- N_h3 / N         # waga warstwy h3 bez uwzględnienia odchylenia standardowego
weight_of_S_h3 <- S_h3 * W_h3     # waga razy odchylenie standardowe
                                       # potrzebne do obliczenia wag firm
                                       # po uwzględnieniu odchylenia std

# jeśli mamy już wszystkie weight_of_S_h#, to możemy policzyć sumę tych wartości, 
# a następnie wagi uwzględniające odchylenie standardowe

sum_weights_of_S <- weight_of_S_h1 + weight_of_S_h2 + weight_of_S_h3

# wagi firm po uwzgl. std:
W_h1_std <- weight_of_S_h1 / sum_weights_of_S
W_h2_std <- weight_of_S_h2 / sum_weights_of_S
W_h3_std <- weight_of_S_h3 / sum_weights_of_S

# wagi firm (jako wektory- przyda się do wzorów):
W <- c(W_h1, W_h2, W_h3)    # Wagi bez uwzględnienia odchylenia standardowego
sum_W <- sum(W)             # suma wag bez std - jest równa 1, tak jak powinna (obliczamy tylko do sprawdzenia)
W_std <- c(W_h1_std, W_h1_std, W_h1_std)   # Wagi z uwzględnieniem odchylenia standardowego
# tworzymy też wektor odchyleń standardowych i wariancji:
S <- c(S_h1, S_h2, S_h3)   # wektor odchyleń standardowych
S_squared <- S^2           # wektor wariancji


####################################
# Losowanie warstwowe proporcjonalne


# Poniższa przyjmuje pięć parametrów:
# W <-- to jest wektor wag, mogą być z uwzględnieniem odchylenia standardowego lub bez (ale czy powinny, to trzeba dopytać na zajęciach)
# S_squared <-- to jest wektor wariancji
# N <-- liczebność populacji
# alfa <-- wartość alfa
# d <-- maksymalny błąd
# 
# Jak można zauważyć nazwy parametrów pokrywają się z nazwami zmiennych, które sobie 
# potworzyliśmy wyżej. To jest tylko dla naszej wygody, tak być nie musi. Tak więc 
# jeśli do parametru W będziecie chcieli wstawić np. jakiś wektor 'wagi', to też będzie dobrze.
# Używamy wzoru ze slajdu nr 25

count_n_prop <- function(W, S_squared, N, alfa, d) {
  licznik <- sum(W * S_squared)
  d_squared <- d^2
  u_alfa_squared <- (qnorm(1 - alfa/2))^2
  mianownik_part1 <- d_squared / u_alfa_squared
  mianownik_part2 <- (1 / N) * sum(W * S_squared)
  n <- licznik / (mianownik_part1 + mianownik_part2)
  return(n)
}

# Obliczamy więc nasze n używając stworzonej funkcji
n_prop <- count_n_prop(
  W=W_std,
  S_squared=S_squared,
  N=N,
  alfa=alfa,
  d=d
)


# Teraz napiszemy funkcję obliczającą udział frakcji. Parametry to:
# W_h <-- waga warstwy h (uwaga: to nie jest wektor, to tylko jedna waga)
# n <-- wartość wyliczona przez nas wyżej
# 
# Jest też inny wariant tej funkcji, czyli (N_h / N) * n.
# W tym wypadku zamiast W_h mamy policzoną wagę już na miejscu. 
# Z tym że jest to waga bez uwzględnienia odchylenia standardowego.
# Uznałem więc, że lepiej liczenie wagi wyrzucić poza funkcję, a do środka 
# wkładać już policzoną.
# Używamy wzoru ze slajdu nr 26

count_n_h_prop <- function(W_h, n) {
  n_h <- W_h * n
  return(n_h)
}

# Obliczamy więc minimalną liczebność próby dla poszczególnych warstw:
n_h1_prop <- count_n_h_prop(
  W=W_h1_std,
  n=n_prop
)
n_h2_prop <- count_n_h_prop(
  W=W_h2_std,
  n=n_prop
)
n_h3_prop <- count_n_h_prop(
  W=W_h3_std,
  n=n_prop
)

print('Odpowiedź:')
sprintf("Min. wielkość próby dla firm usługowych to: % s", n_h1_prop)
sprintf("Min. wielkość próby dla firm produkcyjno-handlowych to: % s", n_h2_prop)
sprintf("Min. wielkość próby dla firm z sektora publicznego to: % s", n_h3_prop)


#######################################
# Losowanie warstwowe nieproporcjonalne


# Piszemy funkcję analogicznie do te wyżej. Parametry to:
# W <-- wektor wag wszystkich warstw
# S <-- wektor odchyleń standardowych (nie wariancji!!)
# N <-- liczebność populacji
# alfa <-- wartość alfa
# d <-- maksymalny błąd
# 
# Ten wzór jest niemal taki sam jak poprzedni, różni się tylko tym, że sumę, 
# którą obliczamy w liczniku podnosimy do kwadratu.
# Używamy wzoru ze slajdu nr 27

count_n_nprop <- function(W, S, N, alfa, d) {
  licznik <- (sum(W * S))^2
  d_squared <- d^2
  u_alfa_squared <- (qnorm(1 - alfa/2))^2
  mianownik_part1 <- d_squared / u_alfa_squared
  mianownik_part2 <- (1 / N) * sum(W * (S^2))   # Tu ważne: nie podnosimy do kwadratu sumy, a tylko wektor odchyleń standardowych (zmieniając go w wektor wariancji)
  n <- licznik / (mianownik_part1 + mianownik_part2)
  return(n)
}

# Wyliczamy sobie n używając powyższej funkcji:
n_nprop <- count_n_nprop(
  W=W_std,
  S=S,
  N=N,
  alfa=alfa,
  d=d
)

# ...A następnie piszemy funkcję, która obliczy nam udział frakcji. Parametry:
# W <-- wektor wag wszystkich warstw
# S <-- wektor odchyleń standardowych wszystkich warstw
# W_h <-- waga wybranej warstwy (tylko jednej!)
# S_h <-- odchylenie standardowe wybranej warstwy (też tylko jednej!)
# n <-- n, które obliczyliśmy sobie wyżej
# Używamy wzoru ze slajdu nr 28

count_n_h_nprop <- function(W, S, W_h, S_h, n) {
  licznik <- W_h * S_h
  mianownik <- sum(W * S)
  n <- (licznik / mianownik) * n
  return(n)
}

# Liczymy zatem minimalną liczebność próby dla każdej z warstw:
n_h1_nprop <- count_n_h_nprop(
  W=W_std,
  S=S,
  W_h=W_h1_std,
  S_h=S_h1,
  n=n_nprop
)
n_h2_nprop <- count_n_h_nprop(
  W=W_std,
  S=S,
  W_h=W_h2_std,
  S_h=S_h2,
  n=n_nprop
)
n_h3_nprop <- count_n_h_nprop(
  W=W_std,
  S=S,
  W_h=W_h3_std,
  S_h=S_h3,
  n=n_nprop
)


print('Odpowiedź:')
sprintf("Min. wielkość próby dla firm usługowych to: % s", n_h1_nprop)
sprintf("Min. wielkość próby dla firm produkcyjno-handlowych to: % s", n_h2_nprop)
sprintf("Min. wielkość próby dla firm z sektora publicznego to: % s", n_h3_nprop)









# ZADANIE 2
# W celu zbadania przeciętnych wydatków na szkolenia pracowników przeprowadzono 
# badanie na populacji liczącej 106 tys. firm. W badaniu wyróżniono 3 warstwy. 
# Liczbę jednotek do warstw próby wyznaczono na podstawie udziału warstw 
# w całej populacji. Przeciętnie założono, że maksymalny błąd szacunku 
# średniego wydatku na szkolenia ogółem wyniesie 8,80zł. Jaka powinna być 
# min liczebność próby, przy założeniu, że poziom ufności wynosi 0,90?
# Jakie losowanie zastosowano?
  




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






# ZADANIE 4
# Europejski Sondaż Społeczny co dwa lata przeprowadza badanie wśród państw 
# europejskich. Wzorem lat ubiegłych, oczekiwana liczebność próby (próba netto) 
# dla poszczególnych państw została określona na poziomie 1500. Mimo, 
# podejmowania licznych prób naprawczych, wskaźnik odpowiedzi od dłuższego 
# czasu plasuje się na poziomie +/- 65%. Wiadomo również, że 9% jednostek 
# nie spełnia kryteriów uczestnictwa w badaniu.
# Oblicz, ile należy wylosować jednostek, aby udało się dotrzeć 
# do 1500 respondentów?









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
Wh <- c(200, 300) # wagi warstw





h_1 <- 1
std_1 <- 200
h_2 <- 2
std_2 <- 300


# kwadraty
alfa_2 <- alfa^2
d_2 <- d^2
u_alfa_2 <- u_alfa^2

