c caeronet.f
c Programme qui ecrit des fichier xy(xyt, sans le nombre de fichier...)
c a partir des donnees d'aeronet
c IMPORTANT: Ce programme demande que toutes les donnees soient dans la
c meme annee.
      
c Jean-Denis Giguere 2003-07-14

      real debut,fin,heure,delta,hfich,hfichp,hfichm
      real ldatas(39), aEcrire(17)
      real jDay,jDay2, annee
      real aot1020,aot870,aot670,aot500,aot440,aot380,aot340,aot1640,
     +water,ang440_870,ang380_500,ang440_675,ang340_440,angp440_675
      real ecart,last
c debut:premier jour de la transposition (en jour de l'annee)
c fin:dernier jour de la transposition (en jour de l'annee)
c heure:heure de la mesure quotidienne
c delta:ecart a l'heure tolore pour la mesure
c hfich: heure du prochain fichier
c hfichp:heure du prochain plus delta
c hfichm:heure du prochain moins delta
c ldatas:donnee pour une heure donnee du photometre.
c jDay:jour de l'annee qui ou une mesure a ete prise
c annee:annee a laqualle a ete prise une mesure
c aot:donnee d'epaisseur optique pour une longueur d'onde
c water:epaisseur d'eau contenu dans le ciel (cm)
c ecart:difference entre l'heure de l'occurence et l'heure choisit
c last:marqueur de la derniere occurence ecrite
      integer nbdefichier,it,anD,anF,unite,itt,iter,jYear
c nbdefichier:nombre de fichier aeronet a transposer
c anD:annee de depart
c anF:annee de fin
c it:iterateur
c itt:iterateur
c iter:iterateur
c jYear:annee de la lecture
      character*60 fichiers(250)
      character*15 location(8)
      character*15 localite,long,lat,alt
c fichiers:nom des fichiers a transposer
c location:information geographique sur le photometre
c ldatas:toutes les donnees contenues dans une ligne du fichier corrige
c localite:nom de l'endroit ou se trouve le photometre
c long:longitude d'un photometre
c lat: lattitude d'un photometre
c alt: altitude d'un photometre

      open(unit=5,file='caeronet.par',status='old',err=15)

      read(5,*) debut
      read(5,*) anD
      read(5,*) fin
      read(5,*) anF
      read(5,*) heure
      read(5,*) delta
      read(5,*) nbdefichier
      read(5,*) (fichiers(it),it=1,nbdefichier)

      close(unit=5)

c Conversion du delta en minute en delta en jour
      delta=(delta/60.)/24.

c Correction de 2doy2
      heure=heure-1.

c Boucle sur tous les fichiers
      do it=1,nbdefichier
      
      print*, 'Lecture du fichier ', fichiers(it)
      
      unite=it+50
      open(unit=unite,file=fichiers(it),status='old',err=16)
      hfich=debut+heure
      hfichm=hfich-delta
      hfichp=hfich+delta

c Lecture de l'en-tete
      read(unite,*)
      read(unite,*) (location(ii),ii=1,8)
      localite=location(2)
      long=location(4)
      lat=location(6)
      alt=location(8)
      
      read(unite,*)
      read(unite,*)
c Debut des donnees on initialise le premier et le dernier element de la matrice
c d'ecriture. Le premier element est 0 car il n'y a pas encore de date
c Le dernier correspond a une valeur un peu plus grande que le plus
c grand ecart accepte
      
      aEcrire(1)=0.
      aEcrire(17)=delta+.01
 77   continue 
      read(unite,*,END=88)(ldatas(ii),ii=1,39)
      
c Specificaton de chaque des valeurs
      jYear=int(ldatas(3))
      jDay=ldatas(7)
      
      if(jDay .gt. hfichm .and. jYear .eq. anD) then
              if  (jDay .lt. hfichp) then
c Ici, l'occurrence respecte les conditions pour etre utilisee

                      ecart=abs(jDay-hfich)
c Si on trouve une occurence meilleure pour une meme date,
c on prend celle-ci
                      if (ecart .lt. aEcrire(17) ) then
                      aot1020=ldatas(8)
                      aot870=ldatas(9)
                      aot670=ldatas(10)
                      aot500=ldatas(11)
                      aot440=ldatas(12)
                      aot380=ldatas(13)
                      aot340=ldatas(14)
                      aot1640=ldatas(17)
                      water=ldatas(18)
                      ang440_870=ldatas(30)
                      ang380_500=ldatas(31)
                      ang440_675=ldatas(32)
                      ang500_870=ldatas(33)
                      ang340_440=ldatas(34)
                      angp440_675=ldatas(35)

                      aEcrire(1)=hfich
                      aEcrire(2)=aot1020
                      aEcrire(3)=aot870
                      aEcrire(4)=aot670
                      aEcrire(5)=aot500
                      aEcrire(6)=aot440
                      aEcrire(7)=aot380
                      aEcrire(8)=aot340
                      aEcrire(9)=aot1640
                      aEcrire(10)=water
                      aEcrire(11)=ang440_870
                      aEcrire(12)=ang380_500
                      aEcrire(13)=ang440_675
                      aEcrire(14)=ang500_870
                      aEcrire(15)=ang340_440
                      aEcrire(16)=angp440_675
                      aEcrire(17)=ecart
                      last=hfich

                      goto 77

                      endif
         

              endif

c On a maintenant depasser le moment de la recherche et nous sommes
c certains qu'il n'y aura pas de meilleure occurence, on change de date
              
              hfich=hfich+1.
              hfichm=hfichm+1.
              hfichp=hfichp+1.
              aEcrire(17)=delta+0.1
              
c On ecrit les 15 fichiers
c La premiere condition verifie que nous avons vraiment trouve un point
c La seconde s'assure qu'on note une seule fois chaque point.
              
      if (aEcrire(1) .gt. 0. .and. (hfich-1) .eq. last) then
      call makefich(aEcrire,localite,long,lat,alt,jYear)
      endif

       endif

c Cette condition est fausse lorsque nous avons couverts
c toute la periode. Il est temps de changer de fichier
       if (hfichp .lt. fin+1. .and. jYear .le. anF) then
       goto 77
       endif

 88   continue
      close(unit=unite)
      unite=unite+1
      enddo

      print*, 'Fin normal de caeronet'


      stop
      
 15   print*,'Impossible de charger caeronet.par'
      stop
 16   print*, 'Impossible de charger ', fichiers(it)
      stop

      end
c
c ---------------------------------------------------------
c
c   julian day to time-date subroutine
c   Martin Aube
c
       subroutine time2date(hre,min,sec,jou,moi,ann,jday)
       real*8 hre,min,sec,jou,moi,ann,jday,nsec,nyear,dyear,mois(12)
       real*8 reste,annee
       integer i,cemoi,cejour
       data mois/31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./
       reste=jday
       annee=1980.
       dowhile (reste.gt.0.)
          if (int(annee/4.)*4.eq.int(annee)) then
             dyear=366.
             mois(2)=29.
             if (int(annee/100.)*100.eq.int(annee)) then
                if (int(annee/400.)*400.ne.int(annee)) then
                   dyear=365.
                   mois(2)=28.
                endif
             endif
          else
             dyear=365.
             mois(2)=28.
          endif
          reste=reste-dyear
          annee=annee+1.
       enddo
       reste=reste+dyear
       ann=annee-1.
       if (ann.lt.1980.) then
          print*,'Invalid date (before 1980-1-1)'
          stop
       endif
c
c      mois
c
       cemoi=1
       dowhile (reste.gt.0.)
          reste=reste-mois(cemoi)
          cemoi=cemoi+1
       enddo
       if (cemoi.gt.1) cemoi=cemoi-1
       reste=reste+mois(cemoi)
       moi=cemoi
c
c      jour
c
       cejour=1
       dowhile (reste.gt.0.)
          reste=reste-1.
          cejour=cejour+1
       enddo
       reste=reste+1.
       jou=cejour-1
c
c      heure
c
       hre=dble(int(reste*24.))
       reste=reste-hre/24.
       min=dble(int(reste*60.*24.))
       reste=reste-min/60./24.
       sec=reste*60.*60.*24.
        if (nint(sec).eq.60) then
          min=min+1.
          sec=0.
       endif
       if (nint(min).eq.60) then
          hre=hre+1.
          min=0.
       endif
       if (nint(hre).eq.24) then
          hre=0.
          jou=jou+1.
       endif
       if (nint(jou).gt.nint(mois(cemoi))) then
          cemoi=cemoi+1
          moi=moi+1.
          jou=1.
       endif
       if (nint(moi).gt.12) then
          moi=1.
          ann=ann+1.
       endif
       return
       end
      

***********************************************************************
      subroutine makefich(aEcrire,localite,long,lat,alt,year)

      integer ii,nbann,ajout,year,nonajout
      integer iheur,iminu,ijou,imois,ianne
      integer llong,llat,lalt,llocalite
c ii:iterateur
c ajout:nombre de jours a jouter pour pouvoir utiliser time2date
c year:annee du test
c iheur,iminu,ijou,imois,ianne:valeur entiere de la date de lecture
c llong,llat,lalt,llocalite:nb de caracter des donnees geographiques
c nbann:nombre d'annee s'eparant l'an de l'occurence et 1980
      character*15 nomfichier
      character*30 nomdufichier
      character*15 localite,long,lat,alt
      character*8 cheur,cminu,cjou,cmois,canne
c nomfichier:partie numerique du nom du fichier de sortie
c nomdufichier:nom du fichier de sortie
      real aEcrire(17)
      real*8 heur,minu,seco,jou,mois,anne,jours
c heur,minu,seco,jou,mois,anne: date de la lecture
c jours: hfich convertit pour fonctionner avec time2date
c aEcrire:donnee qui devront etre ecrite dans des fichiers de sortie

c Correction de hfich pour povoir utiliser la routine time2date
c
c ############## attention ce calcul ne tient pas compte de la correction
c en debut de siecle pour les annees bisextile pour 2000 ca marche car elle
c est bisextile
c
      nbann=year-1980
      ajout=nbann*365+nbann/4
      jours=dble(aEcrire(1))+dble(ajout)
      call time2date(heur,minu,seco,jou,mois,anne,jours)
      
c Construction du nom des fichiers de sorties
      
      iheur=int(heur)
      
      if (seco .gt. 30.) then
              iminu=int(minu)+1
      else
              iminu=int(minu)
      endif

      ijou=int(jou)
      imois=int(mois)
      ianne=int(anne)

      open(unit=2,status='scratch',err=2)
      write(2,22) ianne
      write(2,12) imois
      write(2,12) ijou
      write(2,12) iheur
      write(2,12) iminu

      rewind 2

      read(2,*) canne
      read(2,*) cmois
      read(2,*) cjou
      read(2,*) cheur
      read(2,*) cminu

      close(unit=2)

      nomfichier=canne(1:4)//cmois(1:2)//cjou(1:2)//cheur(1:2)//
     +cminu(1:2)

      
c Ecriture des 15 fichiers de donnees

      llong=index(long,' ')-1
      llat=index(lat,' ')-1
      lalt=index(alt,' ')-1
      llocalite=index(localite,' ')-1
                        
      nomdufichier=nomfichier(1:12)//'aot1020.xy'
      open(unit=11,file=nomdufichier,status='unknown',err=3)

      write(11,*) lat(1:llat),' ', long(1:llong),' ', aEcrire(2),' ',
     +  '  Lieu : ',localite(1:llocalite),' altitude :',' ',alt(1:lalt)

      close(unit=11)


      nomdufichier=nomfichier(1:12)//'aot870.xy'
      open(unit=12,file=nomdufichier,status='unknown',err=3)

      write(12,*) lat(1:llat),' ', long(1:llong),' ', aEcrire(3),' ',
     +  '  Lieu : ',localite(1:llocalite),' altitude :',' ',alt(1:lalt)

      close(unit=12)

      
      nomdufichier=nomfichier(1:12)//'aot670.xy'
      open(unit=13,file=nomdufichier,status='unknown',err=3)

      write(13,*) lat(1:llat),' ', long(1:llong),' ', aEcrire(4),' ',
     +  '  Lieu : ',localite(1:llocalite),' altitude :',' ',alt(1:lalt)

      close(unit=13)
              

      nomdufichier=nomfichier(1:12)//'aot500.xy'
      open(unit=14,file=nomdufichier,status='unknown',err=3)

      write(14,*) lat(1:llat),' ', long(1:llong),' ', aEcrire(5),' ',
     +  '  Lieu : ',localite(1:llocalite),' altitude :',' ',alt(1:lalt)

      close(unit=14)


      nomdufichier=nomfichier(1:12)//'aot440.xy'
      open(unit=15,file=nomdufichier,status='unknown',err=3)

      write(15,*) lat(1:llat),' ', long(1:llong),' ', aEcrire(6),' ',
     +  '  Lieu : ',localite(1:llocalite),' altitude :',' ',alt(1:lalt)

      close(unit=15)


      nomdufichier=nomfichier(1:12)//'aot380.xy'
      open(unit=16,file=nomdufichier,status='unknown',err=3)

      write(16,*) lat(1:llat),' ', long(1:llong),' ', aEcrire(7),' ',
     +  '  Lieu : ',localite(1:llocalite),' altitude :',' ',alt(1:lalt)

      close(unit=16)


      nomdufichier=nomfichier(1:12)//'aot340.xy'
      open(unit=17,file=nomdufichier,status='unknown',err=3)

      write(17,*) lat(1:llat),' ', long(1:llong),' ', aEcrire(8),' ',
     +  '  Lieu : ',localite(1:llocalite),' altitude :',' ',alt(1:lalt)

      close(unit=17)


      nomdufichier=nomfichier(1:12)//'aot1640.xy'
      open(unit=18,file=nomdufichier,status='unknown',err=3)

      write(18,*) lat(1:llat),' ', long(1:llong),' ', aEcrire(9),' ',
     +  '  Lieu : ',localite(1:llocalite),' altitude :',' ',alt(1:lalt)

      close(unit=18)


      nomdufichier=nomfichier(1:12)//'water.xy'
      open(unit=19,file=nomdufichier,status='unknown',err=3)

      write(19,*) lat(1:llat),' ', long(1:llong),' ', aEcrire(10),' ',
     +  '  Lieu : ',localite(1:llocalite),' altitude :',' ',alt(1:lalt)

      close(unit=19)


      nomdufichier=nomfichier(1:12)//'ang440_870.xy'
      open(unit=20,file=nomdufichier,status='unknown',err=3)

      write(20,*) lat(1:llat),' ', long(1:llong),' ', aEcrire(11),' ',
     +  '  Lieu : ',localite(1:llocalite),' altitude :',' ',alt(1:lalt)

      close(unit=20)
     

      nomdufichier=nomfichier(1:12)//'ang380_500.xy'
      open(unit=21,file=nomdufichier,status='unknown',err=3)

      write(21,*) lat(1:llat),' ', long(1:llong),' ', aEcrire(12),' ',
     +  '  Lieu : ',localite(1:llocalite),' altitude :',' ',alt(1:lalt)

      close(unit=21)


      nomdufichier=nomfichier(1:12)//'ang440_675.xy'
      open(unit=22,file=nomdufichier,status='unknown',err=3)

      write(22,*) lat(1:llat),' ', long(1:llong),' ', aEcrire(13),' ',
     +  '  Lieu : ',localite(1:llocalite),' altitude :',' ',alt(1:lalt)

      close(unit=22)


      nomdufichier=nomfichier(1:12)//'ang500_870.xy'
      open(unit=23,file=nomdufichier,status='unknown',err=3)

      write(23,*) lat(1:llat),' ', long(1:llong),' ', aEcrire(14),' ',
     +  '  Lieu : ',localite(1:llocalite),' altitude :',' ',alt(1:lalt)

      close(unit=23)


      nomdufichier=nomfichier(1:12)//'ang340_440.xy'
      open(unit=24,file=nomdufichier,status='unknown',err=3)

      write(24,*) lat(1:llat),' ', long(1:llong),' ', aEcrire(15),' ',
     +  '  Lieu : ',localite(1:llocalite),' altitude :',' ',alt(1:lalt)

      close(unit=24)

      nomdufichier=nomfichier(1:12)//'angp440_675.xy'
      open(unit=25,file=nomdufichier,status='unknown',err=3)

      write(25,*) lat(1:llat),' ', long(1:llong),' ', aEcrire(16),' ',
     +  '  Lieu : ',localite(1:llocalite),' altitude :',' ',alt(1:lalt)

      close(unit=25)


      
      return

 2    print*, 'Impossible de creer un nouvea fichier pourfairelenom'
      stop

 3    print*, 'Impossible d''ouvrir ', nomdufichier
      stop

 12   format(I2.2)
 22   format(I4.4)

      end

