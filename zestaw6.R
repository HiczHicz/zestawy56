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
sd(dane$wzrost)
#16.674

#a WAGA
mean(dane$waga)
#71.8
sd(dane$waga)
#15.043