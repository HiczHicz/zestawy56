
dane=c(6.47,6.47,6.60,6.60,6.62,6.62,6.63,6.63,6.65,6.66,
       6.68,6.69,6.70,6.70,6.71,6.71,6.71,6.73,6.73,6.74,
       6.74, 6.75, 6.75, 6.75, 6.77, 6.78, 6.79, 6.79, 6.80, 6.81,
       6.81, 6.81, 6.82, 6.82, 6.84, 6.85, 6.86, 6.86, 6.88, 6.91,
       6.91, 6.91, 6.91, 6.91, 6.92, 6.94, 6.94, 6.94, 6.94, 6.94,
       6.94, 6.95, 6.95, 6.95, 6.96, 6.96, 6.97, 6.98, 7.00, 7.00,
       7.01, 7.01, 7.02, 7.03, 7.04, 7.04, 7.05, 7.06, 7.09, 7.09,
       7.09, 7.10, 7.10, 7.12, 7.12, 7.14, 7.16, 7.17, 7.18, 7.18,
       7.18, 7.24, 7.26, 7.27, 7.29, 7.29, 7.29, 7.29, 7.32, 7.33,
       7.34, 7.35, 7.35, 7.35, 7.36, 7.39, 7.39, 7.40, 7.40, 7.41,
       7.41, 7.44, 7.46, 7.48, 7.53)

#ZADANIE 1 - interpretacja z poprzedniego
#a
quantile(dane)
#  0%  25%  50%  75% 100% 
# 6.47 6.79 6.95 7.18 7.53 

#b
quantile(dane, probs=c(.20, .50, .80))
#  20%   50%   80% 
#6.748 6.950 7.274 

#c
quantile(dane, probs=c(.13, .87))
#   13%    87% 
#  6.7052 7.3448 

#d
quantile(dane, probs=c(2/3, 0.072, 0.974))
#  20%   50%   80% 
#6.748 6.950 7.274 

#ZADANIE 2
#DUŻE!!!
n=length(dane)
round(sqrt(n)) 
k=10
a=0.01 #to alfa
mi=min(dane)
ma=max(dane)
R=ma-mi
#dlugość przedziału
b=ceiling((R+a/2)/(k*a))*a
b
mi-a/2+k*b #oh, nie wiem co to
#wektor krańca przedziałów
kr=mi-a/2+b*c(0:k)
kr

#HISTOGRAM
h=hist(dane, breaks=kr, plot=F)
#SZEREG ROZDZIELCZY
przedział=1:k
lewy_kr=kr[1:k]
prawy_kr=kr[2:(k+1)]
lewy_kr
prawy_kr

#liczebność
liczność_klasy=h$counts

skumulowana=cumsum(h$counts)/n

wspolrzedna_x <- (head(kr, -1) + tail(kr, -1)) / 2
cumulative_freq_table <- data.frame(wspolrzedna_x, skumulowana)
cumulative_freq_table

wielobok=plot(cumulative_freq_table$wspolrzedna_x, cumulative_freq_table$skumulowana, 
     type = "l", col = "red4", xlab = "Wynagrodzenie (tys. zł)", ylab = "Częstość skumulowana",
     main = "Wielobok częstości skumulowanych")
abline(h = c(0.25, 0.5, 0.75), col = c("orange", "coral2", "coral"), lty = 4)  # Linie dla Q1, Q2, Q3
czestoscwartosci=round(liczność_klasy/n,3)

Szereg_rozdzielczy=as.data.frame(cbind(przedział,lewy_kr,prawy_kr,liczebność, czestoscwartosci))
Szereg_rozdzielczy




write.csv(Szereg_rozdzielczy_duze, "szereg_rozdzielczy.csv", row.names = FALSE, fileEncoding = "UTF-8")




