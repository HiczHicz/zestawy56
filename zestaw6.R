# #FED789FF, #023743FF, #72874EFF, #476F84FF, #A4BED5FF, #453947FF

#ZADANIE
osoba=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
wzrost=c(200,154,153,175,153,198,182,163,200,168,169,167,155,179,167)
waga=c(94,60,51,72,56,96,81,62,99,71,66,70,56,77,66)

dane=as.data.frame(cbind(osoba,wzrost,waga))

#a WZROST
mean(dane$wzrost)
#172.2

#ODCHYLENIE STANDARDOWE POLICZYC RECZNIE BO R COS INACZEJ
odchyl_wzrost=sqrt(1/n*sum((wzrost-śr_wzrost)^2))
#16.1088

#a WAGA
mean(dane$waga)
#71.8
odchyl_waga=sqrt(1/n*sum((waga-śr_waga)^2))
#14.53364


#b KOWARIANCJA 
n=length(waga)
śr_waga=mean(dane$waga)
odchyl_waga=sqrt(1/n*sum((dane$waga-śr_waga)^2))
śr_wzrost=mean(dane$wzrost)
odchyl_wzrost=sqrt(1/n*sum((dane$wzrost-śr_wzrost)^2))

#kowariancja
kow=1/n*sum((wzrost-śr_wzrost)*(waga-śr_waga))
kow
#230.9733
#ewentualnie
cov(Wzrost,Waga) #wyznaczana z mianownikiem n-1
1/(n-1)*sum((Wzrost-śr_wzrost)*(Waga-śr_waga))


#wsp korelacji
kor=kow/(odchyl_wzrost*odchyl_waga)
kor
#ewentualnie
cor(wzrost,waga)
#0.986562

#ZADANIE C

#wyznaczymy prostą regresji

#wykres korelacyjny wagi od wzrostu (raczej waga od wzrostu zależy)

plot(wzrost,waga,lwd=3)

#wyznaczmy współczynniki w regresji linowej
A=kor*odchyl_waga/odchyl_wzrost
B=śr_waga-A*śr_wzrost
A
B



#nanieśmy linię regresji na wykres korelacyjny
#wykorzytamy funkcję points() - połączymy odpowiednie punkty
plot(wzrost,waga,lwd=3)
points(c(min(wzrost),max(wzrost)),
       c(A*min(wzrost)+B,A*max(wzrost)+B),type="l",col="blue")
