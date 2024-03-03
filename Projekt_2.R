library(readxl)
library(MASS)
library(psych)
library(scatterplot3d)
library(corrr)
library(ggcorrplot)
library(ggplot2)
library(stats)

#_____________________________________SKALOWANIE WIELOWYMIAROWE_________________________________

# wczytanie i obrobka danych
df <- read_excel("Projekt_dane.xlsx")
df <- data.frame(df)
names <- df$country
data<- subset(df, select = -country)
rownames(data) <- names

# standaryzacja danych
data_normalized <- scale(data)

# macierz odleglosci
distance_before <- dist(data_normalized)
print(distance_matrix)

#-----------------------KLASYCZNE SKALOWANIE WIELOWYMIAROWE (KRUSKALA)---------------------------------

# jeden wymiar (ssw1)
sww1 <- cmdscale(distance_before, 1)
distance_after_ssw1 <- dist(sww1)

# funkcja STRESS dla ssw1
stress_sww1 <- sqrt(sum((distance_before - distance_after_ssw1)^2) / sum(distance_before^2))
stress_sww1

# dwa wymiary (ssw2)
sww2 <- cmdscale(distance_before,2)
distance_after_sww2 <- dist(sww2)

# funkcja STRESS dla sww2 
stress_sww2 <- sqrt(sum((distance_before - distance_after_sww2)^2) / sum(distance_before^2))
stress_sww2

# trzy wymiary 
sww3 <- cmdscale(distance_before,3)
distance_after_sww3 <- dist(sww3)

# cztery wymiary 
sww4 <- cmdscale(distance_before, 4)
distance_after_sww4 <- dist(sww4)

# funkcja STRESS dla sww4
stress_sww4 <- sqrt(sum((distance_before - distance_after_sww4)^2) / sum(distance_before^2))
stress_sww4

# pięć wymiarów
sww5 <- cmdscale(distance_before, 5)
distance_after_sww5 <- dist(sww5)

# funkcja STRESS dla sww4
stress_sww5 <- sqrt(sum((distance_before - distance_after_sww5)^2) / sum(distance_before^2))
stress_sww5


# funkcja STRESS dla sww3
stress_sww3 <- sqrt(sum((distance_before - distance_after_sww3)^2) / sum(distance_before^2))
print(paste("Współczynnik STRESS dla 3 wymiarowego skalowania Kruskala :", stress_sww3))

#------------------------------------METODA SKALOWANIA SAMMONA----------------------------------------- 

# jeden wymiar ss1
ss1 <- sammon(distance_before, k=1)
distance_after_ss1 <- dist(ss1)
# funkcja STRESS dla ss1
ss1$stress

# dwa wymiary ss2
ss2 <- sammon(distance_before, k=2)

# funkcja STRESS dla ss2
ss2$stress

# trzy wymiary ss3
ss3 <- sammon(distance_before, k=3)


# funkcja STRESS dla ss3 
print(paste("Współczynnik STRESS dla 3 wymiarowego skalowania Sammona :", ss3$stress))

