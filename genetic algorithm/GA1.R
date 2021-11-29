#ALGORYTM GENETYCZNY
#----------------------------------------------------
#CZYSZCZENIE
rm(list=ls())

#WCZYTANIE DANYCH
data=read.table("dane1.csv",sep=";",header = TRUE)
#DANE BEZ NR ZADAN
dane<-as.data.frame(data[,2:length(data[1,])],row.names = data[,1])

dlugosc_w<-length(dane[1,])
dlugosc_k<-length(dane[,1])

#----------------------------------------------------
#POPULACJA POCZATKOWA

obiekty<-as.data.frame(0)
#ILOSC OBIEKTOW TO PARAMETR NP 10 12 14 ITD MUSZA BYC PARZYSTE!!!!!!!!!!!!!!!!!!!!!
ilosc_ob<-14
for(i in 1:ilosc_ob){
    obiekty[1:dlugosc_k,i]<-sample(1:dlugosc_k,dlugosc_k)
}

#----------------------------------------------------
#LICZENIE KOSZTU

koszt<-function(obiekt){
  tabela<-as.data.frame(0)
  kolejnosc<-obiekt
  for(i in 1:length(kolejnosc)){
    tabela[i,1:dlugosc_w]<-dane[kolejnosc[i],]
  } #sumy 1 wiersza
  
  for(i in 2:length(dane[1,])){
    
    tabela[1,i]<-as.data.frame(tabela[1,i-1]+tabela[1,i])
  }
  #sumy 1 kolumny
  
  for(i in 2:length(dane[,1])){
    
    tabela[i,1]<-as.data.frame(tabela[i-1,1]+tabela[i,1])
  }
  #pozostale sumy, wybor wiekszej z i,j-1 a i-1,j i dodanie do biezacej
  
  for(j in 2:length(dane[1,])){
    for(i in 2:length(dane[,1])){
      if(tabela[i,j-1]<tabela[i-1,j]){
        tabela[i,j]<-tabela[i-1,j]+tabela[i,j]
      }
      else{
        tabela[i,j]<-tabela[i,j-1]+tabela[i,j]
      }
    }
  }
  koszt_calkowity<-tabela[dlugosc_k,dlugosc_w]
  return(koszt_calkowity)
}
#----------------------------------------------------

#WYBOR RODZICOW
rodzice<-function(obiekty){
  ranking <- as.data.frame(0)
  tab_10<-as.data.frame(0)
  tab_10[1:ilosc_ob,1]<-c(1:ilosc_ob)
  
  x<-length(tab_10[,1])
  
  k<-sample(1:x,1)
  z<-sample(1:x,1)
  while(k==z){
    k<-sample(1:x,1)
    z<-sample(1:x,1)
  }
  
  rodzic1<-tab_10[k,1]
  rodzic2<-tab_10[z,1]
  
  koszt1<-koszt(obiekty[,rodzic1])
  koszt2<-koszt(obiekty[,rodzic2])
  
  if(koszt1>koszt2){
    ranking[1,]<-rodzic2
    tab_10<-tab_10[-z,1]
    row.names(tab_10)<-NULL
    
  }else{
    ranking[1,]<-rodzic1
    tab_10<-tab_10[-k,1]
    row.names(tab_10)<-NULL
    
  }
  
  for(i in 2:(ilosc_ob-1)){
    x<-length(tab_10)
    
    k<-sample(1:x,1)
    z<-sample(1:x,1)
    while(k==z){
      k<-sample(1:x,1)
      z<-sample(1:x,1)
    }
    
    rodzic1<-tab_10[k]
    rodzic2<-tab_10[z]
    
    koszt1<-koszt(obiekty[,rodzic1])
    koszt2<-koszt(obiekty[,rodzic2])
    
    if(koszt1>koszt2){
      ranking[i,]<-rodzic2
      tab_10<-tab_10[-z]
      row.names(tab_10)<-NULL
      
    }else{
      
      ranking[i,]<-rodzic1
      tab_10<-tab_10[-k]
      row.names(tab_10)<-NULL
      
    }
  }
  
  ranking[ilosc_ob,1]<-tab_10[1]
  return(ranking)
}
  
ranking<-rodzice(obiekty)

#----------------------------------------------------

#----------------------------------------------------
#KRZYZOWANIE
#METODA OX

potomkowie<-function(para1){
  #ciecia 15 35 
  potomek1<-as.data.frame(0)
  przeciecie1<-dlugosc_k%/%3
  przeciecie2<-2*dlugosc_k%/%3
  
  potomek1[przeciecie1:przeciecie2,1]<-para1[przeciecie1:przeciecie2,1]
  #wklejanie od 36 indeksu
  l=z=przeciecie2+1
  z1=dlugosc_k+przeciecie2
  p1<-dlugosc_k+1
  x=0
  for(i in l:z1){
    if(i==p1){
      m=1
    }else if(i<p1){
      m=i
    }
    
    if(z==p1){
      z=1
    }
    
    
    x<-0
    for(j in przeciecie1:przeciecie2){
      
      if(para1[m,2]==potomek1[j,1]){
        x=x+1
      }
      
    }
    if(x==0){
      potomek1[z,1]<-para1[m,2]
      z=z+1
    }
    m=m+1
  }
  
  potomek2<-as.data.frame(0)
  przeciecie1<-dlugosc_k%/%3
  przeciecie2<-2*dlugosc_k%/%3
  
  potomek2[przeciecie1:przeciecie2,1]<-para1[przeciecie1:przeciecie2,2]
  #wklejanie od 36 indeksu
  l=z=przeciecie2+1
  z1=dlugosc_k+przeciecie2
  p1<-dlugosc_k+1
  x=0
  for(i in l:z1){
    if(i==p1){
      m=1
    }else if(i<p1){
      m=i
    }
    
    if(z==p1){
      z=1
    }
    
    
    x<-0
    for(j in przeciecie1:przeciecie2){
      
      if(para1[m,1]==potomek2[j,1]){
        x=x+1
      }
      
    }
    if(x==0){
      potomek2[z,1]<-para1[m,1]
      z=z+1
    }
    m=m+1
  }
  
  poto_para<-as.data.frame(0)
  poto_para[1:dlugosc_k,1]<-potomek1[1:dlugosc_k,1]
  poto_para[1:dlugosc_k,2]<-potomek2[1:dlugosc_k,1]
  return(poto_para)
}
#----------------------------------------------------
#NOWA POPULACJA 

dzieci<-as.data.frame(0)
j=1
j1=2
for(i in 1:(ilosc_ob/2)){
  tab<-as.data.frame(obiekty[ranking[j,1]])
  tab[,2]<-as.data.frame(obiekty[ranking[j1,1]])
  
    dzieci[1:dlugosc_k,j:j1]<-potomkowie(tab)
    j=j+2
    j1=j1+2
}
row.names(dzieci)<-NULL
#----------------------------------------------------

#----------------------------------------------------
#MUTACJA

mutacja<-function(populacja){
  dziecko<-sample(1:ilosc_ob,1)
  zadanie1<-sample(1:dlugosc_k,1)
  zadanie2<-sample(1:dlugosc_k,1)
  
  z1<-populacja[zadanie1,dziecko]
  z2<-populacja[zadanie2,dziecko]
  populacja[zadanie1,dziecko]<-z2
  populacja[zadanie2,dziecko]<-z1
  return(populacja)
}
#----------------------------------------------------
#PRAWDOPODOBIENSTWO MUTACJI 
#----------------------------------------------------
pi<-runif(1)
populacja<-dzieci
#BETA TO PARAMETR PRAWDOPODBOBIENSTWA MUTACJI 
beta=0.5
if(pi<beta){
  populacja<-mutacja(dzieci)
}

#----------------------------------------------------

#----------------------------------------------------
#FUNKCJA
koszty<-as.data.frame(0)

for(i in 1:ilosc_ob){
  koszty[1,i]<-koszt(populacja[,i])
  
}
min<-min(koszty[1,])

for(i in 1:ilosc_ob){
  if(min==koszty[1,i]){
    naj<-as.data.frame(populacja[,i])
  }
}

#----------------------------------------------------
#GLOWNY WARUNEK
#----------------------------------------------------
#PARAMETR GLOWNY ZMIENIA SIE W ZALEZNOSCI OD DANYCH I POWINIEN BYC STOPNIOWO ZMNIEJSZANY!!!!!!!!!!!!!!!!!!
while(min>=7000){
  ranking<-rodzice(populacja)
  #----------------------------------------------------
  #NOWA POPULACJA 
  dzieci<-as.data.frame(0)
  j=1
  j1=2
  for(i in 1:(ilosc_ob/2)){
    tab<-as.data.frame(obiekty[ranking[j,1]])
    tab[,2]<-as.data.frame(obiekty[ranking[j1,1]])
    
    dzieci[1:dlugosc_k,j:j1]<-potomkowie(tab)
    j=j+2
    j1=j1+2
  }
  row.names(dzieci)<-NULL
  #----------------------------------------------------
  
  pi<-runif(1)
  #BETA TO PARAMETR PRAWDOPODBOBIENSTWA MUTACJI 
  
  beta=0.5
  if(pi<beta){
    populacja<-mutacja(dzieci)
  }
  
  
  koszty<-as.data.frame(0)
  
  for(i in 1:ilosc_ob){
    koszty[1,i]<-koszt(populacja[,i])
    
  }
  min<-min(koszty[1,])
  
  for(i in 1:ilosc_ob){
    if(min==koszty[1,i]){
      naj<-as.data.frame(populacja[,i])
    }
  }
  
}




































