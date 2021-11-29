#-------------------------------------------------------------------------------
#                               ALGORYTM GENETYCZNY
#-------------------------------------------------------------------------------
#CZYSZCZENIE
rm(list=ls())

#WCZYTANIE DANYCH
data=read.table("dane1.csv",sep=";",header = TRUE)
#DANE BEZ NR ZADAN
dane<-as.data.frame(data[,2:length(data[1,])],row.names = data[,1])

dlugosc_k<-length(dane[,1])
dlugosc<-length(dane[,1])+2
dlugosc2<-length(dane[,1])+1

ilosc_miast<-length(dane[,1])
ilosc_miast1<-length(dane[,1])+1
#-------------------------------------------------------------------------------
#                               POPULACJA POCZATKOWA
#-------------------------------------------------------------------------------

obiekty<-as.data.frame(0)
######################################ILOSC OBIEKTOW TO PARAMETR #########################################################
ilosc_ob<-40
for(i in 1:ilosc_ob){
  obiekty[1:ilosc_miast,i]<-sample(1:dlugosc_k,dlugosc_k)
  obiekty[ilosc_miast1,i]<-obiekty[1,i]
}

#-------------------------------------------------------------------------------
#                    FUNKCJA LICZACA CALKOWITA DROGE
#-------------------------------------------------------------------------------

droga<-function(kol){
  kolejnosc<-as.data.frame(0)
  kolejnosc[1:ilosc_miast1,1]<-kol[1:ilosc_miast1]
  kolejnosc[1,2]<-0
  
  for(i in 1:ilosc_miast){
    
    wiersz<-kolejnosc[i,1]
    kolumna<-kolejnosc[i+1,1]
    kolejnosc[i+1,2]<-dane[wiersz,kolumna]
    
  }
  
  droga_calkowita<-sum(kolejnosc[,2])
  
  return(droga_calkowita)
}
#-------------------------------------------------------------------------------
#                         WYBOR RODZICOW RANKING
#-------------------------------------------------------------------------------

rodzice2<-function(obiekty){
  ranking <- as.data.frame(0)
  droga1 <- as.data.frame(0)
  
  #wylicza calkowita droge dla kazdego obiektu
  for(i in 1:ilosc_ob){
    droga1[1,i]<-droga(obiekty[1:dlugosc2,i])
  }
  droga_pom<-droga2<-droga1
  #wyszukujemy najkrotsze drogi i przypisujemy im rangi
  for(i in 1:ilosc_ob){
    if(i<40){
      
      min1<-min(droga1[1,])
      y=1
      while(min1!=droga1[1,y]){
        y=y+1
      }
      x=1
      while(min1!=droga2[1,x]){
        x=x+1
      }
      droga_pom[1,i]<-min1
      droga_pom[2,i]<-i
      droga_pom[3,i]<-x
      
      droga1<-droga1[1,-y]
    }else{
      min1<-min(droga1)
      droga_pom[1,ilosc_ob]<-min1
      droga_pom[2,ilosc_ob]<-i
      droga_pom[3,ilosc_ob]<-x
    }
   
  }
  
  #wybieramy lepsza polowe i nich losujemy pary rodzicow
  pol<-ilosc_ob/2
  
  rodz<-as.data.frame(0)
  kolejnosc<-as.data.frame(0)
  rodz<-droga_pom[,1:pol]
  
  #par tyle co obserwacji czyli dwa razy wiecej losowan
  los<-ilosc_ob*2
  for(i in 1:los){
    kolejnosc[i,1]<-sample(1:pol,1)
    #para nie moze byc stworzona z 2 tych samych rodzicow
    if(i>3){
      while(kolejnosc[i-1,1]==kolejnosc[i,1] || kolejnosc[i-1,1]==kolejnosc[i,1]){
        kolejnosc[i,1]<-sample(1:pol,1)
      }
    }
    
    #przypisujemy liczba rankingowym ich odpowiedniki w indeksach
    
    for(j in 1:pol){
      if(kolejnosc[i,1]==rodz[2,j]){
        kolejnosc[i,2]<-rodz[3,j]
      }
    }
    
    
  }
  
  wynik<-as.data.frame(0)
  wynik[1:los,1]<-kolejnosc[1:los,2]
  return(wynik)
}


#-------------------------------------------------------------------------------
#                         WYBOR RODZICOW TURNIEJ
#-------------------------------------------------------------------------------

rodzice<-function(obiekty){
  ranking <- as.data.frame(0)
  tab_10<-as.data.frame(0)
  tab_10[1:ilosc_ob,1]<-c(1:ilosc_ob)
  j<-ilosc_ob*2
  for(i in 1:j){
    x<-length(tab_10[,1])
    
    k<-sample(1:x,1)
    z<-sample(1:x,1)
    while(k==z){
      k<-sample(1:x,1)
      z<-sample(1:x,1)
    }
    if(i>1){
      while(ranking[i-1,1]==k || ranking[i-1,1]==z){
        k<-sample(1:x,1)
        z<-sample(1:x,1)
      }
    }
    
    
    rodzic1<-tab_10[k,1]
    rodzic2<-tab_10[z,1]
    
    droga1<-droga(obiekty[,rodzic1])
    droga2<-droga(obiekty[,rodzic2])
    
    if(droga1>droga2){
      ranking[i,]<-rodzic2
      
    }else{
      
      ranking[i,]<-rodzic1
    }
  }
  
  return(ranking)
}

ranking<-rodzice2(obiekty)



#-------------------------------------------------------------------------------
#                                  KRZYZOWANIE
#                                   METODA OX
#-------------------------------------------------------------------------------

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
#-------------------------------------------------------------------------------
#                                  KRZYZOWANIE
#                                   METODA PMX
#-------------------------------------------------------------------------------


potomkowie2<-function(para1){

  potomek1<-as.data.frame(0)
  przeciecie1<-dlugosc_k%/%3
  przeciecie2<-2*dlugosc_k%/%3
  p1=przeciecie1-1
  p2=przeciecie2+1
  
  
  #maska
  maska<-as.data.frame(0)
  dl<-length(para1[przeciecie1:przeciecie2,1])
  dlc<-length(para1[,1])


  maska[1:dl,1]<-para1[przeciecie1:przeciecie2,1]
  maska[1:dl,2]<-para1[przeciecie1:przeciecie2,2]
  
  pow<-0
  for(i in 1:dl){
    for(j in 1:dl){
      if(maska[i,1]==maska[j,2]){
        pow=pow+1
      }
    }
  }
pow=pow-1
  
  potomek1[przeciecie1:przeciecie2,1]<-para1[przeciecie1:przeciecie2,1]

  #od 1 do pierwszego przeciecia
  
    for(i in 1:p1){
      x=0
      
      for(j in 1:dl){
        
        if(para1[i,2]==maska[j,1]){
          potomek1[i,1]<-maska[j,2]
          x=x+1
        }
        
        
      }
      
      if(x==0){
        potomek1[i,1]<-para1[i,2]
      }
      
    }

for(z in 1:pow){
  for(i in 1:p1){
    x=0
    
    for(j in 1:dl){
      
      if(potomek1[i,1]==maska[j,1]){
        potomek1[i,1]<-maska[j,2]
        x=x+1
      }
      
      
    }
    
    
    
  }
  z=z+1
}
  
  
  #od drugiego przeciecia do konca
  for(i in p2:dlc){
    x=0
    
    for(j in 1:dl){
      
      if(para1[i,2]==maska[j,1]){
        potomek1[i,1]<-maska[j,2]
        x=x+1
      }
      
      
    }
    
    if(x==0){
      potomek1[i,1]<-para1[i,2]
    }
    
  }
  
  for(z in 1:pow){
    for(i in p2:dlc){
      x=0
      
      for(j in 1:dl){
        
        if(potomek1[i,1]==maska[j,1]){
          potomek1[i,1]<-maska[j,2]
          x=x+1
        }
        
        
      }
      
      
      
    }
    z=z+1
    
  }
 
  potomek2<-as.data.frame(0)
  
  potomek2[przeciecie1:przeciecie2,1]<-para1[przeciecie1:przeciecie2,2]

  
  #od 1 do pierwszego przeciecia
  for(i in 1:p1){
    x=0
    
    for(j in 1:dl){
      
      if(para1[i,1]==maska[j,2]){
        potomek2[i,1]<-maska[j,1]
        x=x+1
      }
      
      
    }
    
    if(x==0){
      potomek2[i,1]<-para1[i,1]
    }
    
  }
  if(pow>=1){
    for(z in 1:pow){
      for(i in 1:p1){
        x=0
        
        for(j in 1:dl){
          
          if(potomek2[i,1]==maska[j,2]){
            potomek2[i,1]<-maska[j,1]
            x=x+1
          }
          
          
        }
        
        
        
      }
      z=z+1
    }
  }
  
  
  
  #od drugiego przeciecia do konca
  for(i in p2:dlc){
    x=0
    
    for(j in 1:dl){
      
      if(para1[i,2]==maska[j,2]){
        potomek2[i,1]<-maska[j,1]
        x=x+1
      }
      
      
    }
    
    if(x==0){
      potomek2[i,1]<-para1[i,1]
    }
    
  }
  if(pow>=1){
    for(z in 1:pow){
      for(i in p2:dlc){
        x=0
        
        for(j in 1:dl){
          
          if(potomek2[i,1]==maska[j,2]){
            potomek2[i,1]<-maska[j,1]
            x=x+1
          }
          
          
        }
        
        
        
      }
      z=z+1
      
    }
  }
  
  

  poto_para<-as.data.frame(0)
  poto_para[1:dlugosc_k,1]<-potomek1[1:dlugosc_k,1]
  poto_para[1:dlugosc_k,2]<-potomek2[1:dlugosc_k,1]
  return(poto_para)
}




#-------------------------------------------------------------------------------
#                                  MUTACJA 
#-------------------------------------------------------------------------------

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
#-------------------------------------------------------------------------------
#                                  NOWA POPULACJA -KRZYZOWANIE
#-------------------------------------------------------------------------------

  dzieci<-as.data.frame(0)
  j=1
  j1=2
  it<-(ilosc_ob)
  for(i in 1:it){
    tab<-as.data.frame(obiekty[ranking[j,1]])
    tab[,2]<-as.data.frame(obiekty[ranking[j1,1]])
    
    dzieci[1:dlugosc_k,j:j1]<-potomkowie(tab)
    j=j+2
    j1=j1+2
  }
  row.names(dzieci)<-NULL

 



#-------------------------------------------------------------------------------
#                          PRAWDOPODOBIENSTWO MUTACJI 
#-------------------------------------------------------------------------------

pi<-runif(1)
populacja<-dzieci
#BETA TO PARAMETR PRAWDOPODBOBIENSTWA MUTACJI 
beta=0.5
if(pi<beta){
  populacja<-mutacja(dzieci)
}

#-------------------------------------------------------------------------------
#                              FUNKCJA
#-------------------------------------------------------------------------------


koszty<-as.data.frame(0)
j<-ilosc_ob*2
for(i in 1:j){
  populacja[ilosc_miast1,i]<-populacja[1,i]
  
}

for(i in 1:j){
  koszty[1,i]<-droga(populacja[,i])
  
}
pocz<-length(populacja[1,])+1
kon<-length(populacja[1,])+length(obiekty[1,])
ob<-length(obiekty[1,])
populacja[dlugosc,]<-koszty[1,]
populacja[1:dlugosc2,pocz:kon]<-obiekty

koszty2<-as.data.frame(0)
for(i in 1:ob){
  koszty2[1,i]<-droga(obiekty[,i])
  
}
populacja[dlugosc,pocz:kon]<-koszty2[1,]

#-------------------------------------------------------------------------------
#                       WYBOR NAJLEPSZYCH POTOMKOW
#                  ROZMIAR POPULACJI MUSI BYC STALY
#-------------------------------------------------------------------------------

#WYBOR DETERMINISTYCZNY
#j=ilosc_ob
#droga_cal<-as.data.frame(0)
#for(i in 1:j){
  
 # min<-min(populacja[dlugosc,])
  #y=1
  
  #while(populacja[dlugosc,y]!=min){
   # y=y+1
  #}
  
  #droga_cal[1:dlugosc2,i]<-populacja[1:dlugosc2,y]
  #populacja<-populacja[,-y]
  
  #min1=min(populacja[dlugosc,])
  
  #while(min1==min){
   # y=1
    #while(populacja[dlugosc,y]!=min1){
     # y=y+1
    #}
    #populacja<-populacja[,-y]
    #min1=min(populacja[dlugosc,])
    
  #}
#}
#-------------------------------------------------------------------------------
# WYBOR LOSOWY
#-------------------------------------------------------------------------------
j=ilosc_ob
droga_cal<-as.data.frame(0)
l<-length(populacja[1,])
x=l-j
for(i in 1:x){
  y<-sample(1:l,1)
  populacja<-populacja[1:dlugosc,-y]
  i=i+1
  l=l-1
}
droga_cal<-populacja[1:dlugosc2,]
naj<-min(populacja[dlugosc,])

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#                                                                      GLOWNY WARUNEK
#----------------------------------------------------------------------------------------------------------------------------------------------------------
w=0
##########################################parametr w do zmiany #######################################################
#w to ilosc iteracji

while(w<4){
  
  #losujemy rodzicóW
  ranking<-rodzice2(droga_cal)
  
  
  #prawdopodobienstwo krzyzowania 
  ##########################################parametr alpha do zmiany #######################################################
  #alpha odpowiada za prawdopodobieńsrwo krzyzowania
  
  pi<-runif(1)
  alpha=0.5
  if(pi<alpha){
    #krzyzowanie
    dzieci<-as.data.frame(0)
    j=1
    j1=2
    it<-(ilosc_ob)
    for(i in 1:it){
      tab<-as.data.frame(droga_cal[ranking[j,1]])
      tab[,2]<-as.data.frame(droga_cal[ranking[j1,1]])
      ####################################################2 METODY DO SPRAWDZENIA###########################################
      #potomkowie to OX,  a potomkowie2 to PMX
      #FUNKCJE POTOMKOWIE ALBO POTOMKOWIE2
      dzieci[1:dlugosc_k,j:j1]<-potomkowie2(tab)
      j=j+2
      j1=j1+2
    }
    row.names(dzieci)<-NULL
  }
  
  
  
  #mutacja
  pi<-runif(1)
  ##########################################parametr beta do zmiany #######################################################
  #beta odpowiada za prawdopodobienstwo mutacji
  
  beta=0.5
  populacja<-dzieci
  if(pi<beta){
    populacja<-mutacja(dzieci)
  }
  
  #laczenie
  koszty<-as.data.frame(0)
  j<-ilosc_ob*2
  for(i in 1:j){
    populacja[ilosc_miast1,i]<-populacja[1,i]
    
  }
  
  for(i in 1:j){
    koszty[1,i]<-droga(populacja[,i])
    
  }
  
  populacja[dlugosc,]<-koszty[1,]
  populacja[1:dlugosc2,pocz:kon]<-droga_cal
  
  koszty2<-as.data.frame(0)
  for(i in 1:ob){
    koszty2[1,i]<-droga(droga_cal[,i])
    
  }
  populacja[dlugosc,pocz:kon]<-koszty2[1,]
  
  #wybor najlepszego
  #-------------------------------------------------------------------------------
  # WYBOR LOSOWY
  #-------------------------------------------------------------------------------
  j=ilosc_ob
  droga_cal<-as.data.frame(0)
  l<-length(populacja[1,])
  x=l-j
  for(i in 1:x){
    y<-sample(1:l,1)
    populacja<-populacja[1:dlugosc,-y]
    i=i+1
    l=l-1
  }
  droga_cal<-populacja[1:dlugosc2,]
  naj<-min(populacja[dlugosc,])
  
  w=w+1
}

wynik<-as.data.frame(0)
for(i in 1:length(populacja[1,])){
  if(populacja[dlugosc,i]==naj){
    wynik[1:dlugosc,1]<-populacja[,i]
  }
}












