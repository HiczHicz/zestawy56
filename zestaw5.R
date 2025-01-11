
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
     type = "l", col = "navy", xlab = "Klasy", ylab = "Wartości",
     main = "Wielobok częstości skumulowanych")
abline(h = c(0.25, 0.5, 0.75), col = c("cyan", "cyan3", "darkcyan"), lty = 4)  # Linie dla Q1, Q2, Q3
czestoscwartosci=round(liczność_klasy/n,3)

Szereg_rozdzielczy=as.data.frame(cbind(przedział,lewy_kr,prawy_kr,liczebność, czestoscwartosci))
Szereg_rozdzielczy




write.csv(Szereg_rozdzielczy_duze, "szereg_rozdzielczy.csv", row.names = FALSE, fileEncoding = "UTF-8")


#PRÓBA 2 - z tego co robili na zajęciach u strobina

#Przywoałjmy konsytrukcję szeregu dla wynagrodzenia

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

n=length(dane)
alfa=0.01
#rozstęp
ma=max(dane)
mi=min(dane)
R=ma-mi
#liczba klas
n=length(dane)
#k=round(sqrt(n))
k=10
#długość klasy
(R+alfa/2)/k
#po zaokrągleniu
#b=0.1065
#bardziej automatycznie
(R+alfa/2)/k*100
ceiling((R+alfa/2)/k*100)
ceiling((R+alfa/2)/k*100)/100 #100=1/alfa u nas
b=ceiling((R+alfa/2)/k/alfa)*alfa
b
#sprawdźmy czy taka długość jest wystarczająca - czy
#prawy kraniec ostatniej klasy jest większy nią wartość maksymalna
mi-alfa/2+k*b>ma

#definujemy krańce klas
krańce_klas=seq(mi-alfa/2,mi-alfa/2+k*b,by=b)
krańce_klas

hist=hist(dane,breaks=krańce_klas)
liczność=hist$counts

#pojawia się komunikat, że histogram dla liczebności klas
#może zniekształcać informację

długości_klas=prawy_kr-lewy_kr
długości_klas

#wyznaczamy natężenia liczebności oraz częstości - przyjmiemy 
#h=1 (wówczas natężenia częstości to gęstości)

nat_licz=liczebność/długości_klas
nat_licz
gęst=nat_licz/n
gęst

#Konstruujemy szereg dla natężeń i gęstości
nowy_szereg=data.frame(
  lewy_kr,prawy_kr,liczebność,
  natężenie_licz=nat_licz,gęstość=gęst
)
#podajmy z wartościami nat. liecznoścoi zaokrąglonymi
#do jednego, oraz gęstością do trzech miejsc po przecinku
nowy_szereg_z=data.frame(
  lewy_kr,prawy_kr,liczebność,
  natężenie_licz=round(nat_licz,1),gęstość=round(gęst,3)
)
nowy_szereg_z

#Generowanie odpowiedniego histogramu

#w poleceniu hist, parametr freq=F wygeneruje gęstości (podobnie density=T)

hist1=hist(Wynagrodzenie,breaks=nowe_k,freq=F,labels=T)
hist1$counts
hist1$density

#dla innych wartości, np. dla matężeń, postąpimy inaczej - 
#zdefiniujemy odpowiednie wartości w uprzednio zdefiniowanym
#histogramie

h_n1=hist(Wynagrodzenie,breaks=nowe_k)
class(h_n1)
h_n1$counts=nat_licz #przypisujemy wartości natężeń
plot(h_n1,freq = T,labels = T)


#podobnie możemy stworzyć histogram procentowy
h_n2=hist(Wynagrodzenie,breaks=nowe_k)
h_n2$counts=nowe_licz/n*100
plot(h_n2,freq = T,labels = T)
plot(h_n2,freq = T)

#Wieloboki
k_n=length(kr)

nowe_środki=(lewy_kr+prawy_kr)/2
nowe_środki
ppx=kr[1]-długości_klas[1]/2  #wsp. x pierwszego punktu
opx=kr[n+1]+długości_klas[k_n]/2 #wsp. x ostatniego punktu

wsp_x=c(ppx,nowe_środki,opx)
wsp_x

#wielobok natężeń liczebności
licz_y=c(0,liczebność,0)
plot(wsp_x,licz_y, type="l",lwd=2) 
#w funkcji plot - typ "l" aby była linia
#lwd - grubość linii

#wielobok gęstości
gęst_y=c(0,gęst,0)
plot(wsp_x,gęst_y, type="l",lwd=2) 

#naniesiemy wielobok na histogram
#polecenie points() dodaje punkty do już
#utworzonego wykresu

hist(Wynagrodzenie,breaks=nowe_k,freq=F)
points(wsp_x,gęst_y,type="l",lwd=2,col="green")

#rozszerzmy zakres x tak by cały wielobok się mieścił
hist(Wynagrodzenie,breaks=nowe_k,freq=F,xlim=c(ppx-0.1,opx+0.1))
points(wsp_x,gęst_y,type="l",lwd=2,col="green")


#Wieloboki wartości skumulowanych

#wielobok liczebności skumulowanych
licz_sk=cumsum(liczebność)
licz_sk
licz_sk_y=c(0,licz_sk)
plot(kr,licz_sk_y,type="l",col="red",lwd=2)

#wielobok częstości skumulowanych
częst_sk=cumsum(liczebność)/n
częst_sk
częst_sk_y=c(0,częst_sk)
plot(kr,częst_sk_y,type="l",col="blue",lwd=2)

#porównajmy z faktyczną dystrybuantą empiryczną
#paramtr cex  -wielokość kropek/linii
plot(ecdf(Wynagrodzenie),cex=0.1)
points(nowe_k,częst_sk_y,type="l",col="red",lwd=1)

#wrysowanie prostych do wykresu - funkcja abline
#parametr v - linia pozioma, h- pionowa
plot(kr,częst_sk_y,type="l",col="navy",lwd=2, main="Wielobok częstości skumulowanych", xlab="Klasy", ylab="Wartości")
abline(h = c(0.25, 0.5, 0.75), col = c("cyan", "cyan3", "darkcyan"), lty = 4)  # Linie dla Q1, Q2, Q3


#z wykresów odczytujemy orientacyjne wartości mediany i
#pierwszego kwartyla Q1
#pierwszy kwartyl ok. 1.8
#mediana ok. 1.9
#prawdziwe wartości

median(Wynagrodzenie)
quantile(Wynagrodzenie,0.25)



