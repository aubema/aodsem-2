c     ceci est un programme servant à traiter le donnée de feux de forêt répertoriées par le Service Canadien des Forêts
c     Copyright Yassine Chihab et Jonathan Poisson-Rioux


c
c     Déclaration des variables
c
c      j= jour de la ligne lue de la base LFD
c      m= mois de la ligne lue de la base LFD
c      a= année de la ligne lue de la base LFD
c      lat= latitude de la ligne lue de la base LFD
c      lon= longitude de la ligne lue de la base LFD
c      size= surface brûlée de la ligne lue de la base LFD
c      nbjb= nb de jours dans l'année de la ligne lue de la base LFD
c      bidon= donnée inintéressante
c      matrice= matrice de la surface brûlée aux degrés pour les mois analysée (basée sur LFD)
c      aa= année spécifiée par la personne
c      mm= mois spécifié par la personne
c      jj= jour de départ de la période (le premier du mois par défaut)
c      jjf= jour de fin de la période (le dernier jour du mois par défaut (lié directement à nbjm)
c      julien= jour julien de la ligne lue de la base LFD
c      julf= jour julien de la dernière journée de la période spécifiée
c      juld= jour julien de la première journée de la période spécifiée
c      nbjm= nb de jours dans le mois analysé
c      nbjp= nb de jours dans l'année du mois analysé
c      matmoy= matrice de surface moyenne par jour de LFD dans le mois calculé (mois spécifié et son précédent pour le lissage)
c      hautmoy= vecteur d'échelle de hauteur des émissions de la base de donnée de Lavoué
c      hauteur= hauteur des émissions de black carbone de la base de donnée de Lavoué
c      hauteur2= hauteur des émissions d'organic carbone de la base de donnée de Lavoué
c      min= Sert seulement à placer les hauteurs d'émissions dans l'échelle (10 cases) de la matrice
c      pmin= Sert seulement à placer les hauteurs d'émissions dans l'échelle (10 cases) de la matrice
c      lalat= latitude de la ligne lue de la base de données de Lavoué
c      lalon= longitude de la ligne lue de la base de données de Lavoué
c      matlavoueb= matrice de la qté de black carbone émise aux degrés pour les mois analysée (basée sur Lavoué)
c      matlavoueo= matrice de la qté d'organic carbone émise aux degrés pour les mois analysée (basée sur Lavoué)
c      emissionbc= matrice finale de la qté d'aérosol émise (black carbone)
c      emissionoc= matrice finale de la qté d'aérosol émise (organic carbone)
c      nblignes= nb de lignes contenues dans LFD
c      lalignes= nb de lignes contenues dans la base de données de Lavoué
c      lajouri= jour initial de la ligne lue de la base de données de Lavoué
c      lajourf= jour finale de la ligne lue de la base de données de Lavoué (normalement= à lajouri)
c      lamoisi= mois initial de la ligne lue de la base de données de Lavoué
c      lamoisf= mois final de la ligne lue de la base de données de Lavoué (normalement= à lamoisi)
c      laani= annee initiale de Lavoué
c      laanf= annee finale de Lavoué (normalement= à laani)
c      blackc= qté d'émission de black carbon(suie) répertoriée dans la base de données à Lavoué
c      orgc= qté d'émission organic carbon (organiques carbonés) répertoriée dans la base de données à Lavoué
c      p= latitude finale
c      q= longitude 
c      qq= longitude finale
c      casefeu= niveau de hauteur des émission (1 à 10) de black carbon
c      casefeu2= niveau de hauteur des émission (1 à 10) d'organiques carbonés


      


      character bidon
      character*1 convert(10)
      character*60 nom

      real lat,lon,size,matrice(181,360),julien,julf,juld,
     +matmoy(181,360),hauteur,hautmoy(10),min,pmin,blackc,orgc,
     +lalat,lalon,matlavoueb(181,360,10),emissionbc(181,360,10),pmin2,
     +min2,matlavoueo(181,360,10),emissionoc(181,360,10),hauteur2
      
      integer j,a,m,aa,mm,jj,jjf,nbjp,nbjb,nblignes,z,
     +lajouri,lamoisi,laani,lajourf,lamoisf,laanf,lalignes,p,q,qq,r,
     +casefeu,casefeu2,o,eanm,eanc,eand,eanu,emod,emou,liss,nbjm,toto
      data convert /'0','1','2','3','4','5','6','7','8','9'/

c     Initialisation des variables
c 
      jjf=0
      jj=1
      mm=0
      aa=0
      lat=0.
      lon=0.
      size=0.
      nblignes=11120
      lalignes=354733
      a=0
      m=0
      j=0
      nbjp=0
      nbjb=0
      lajouri=0
      lamoisi=0
      laani=0
      lajourf=0
      lamoisf=0
      laanf=0
      blackc=0.
      orgc=0.
      min=0.
      data hautmoy /82.5,255.,443.5,651.,883.,
     +1146.,1448.5,1805.5,7000.,21000./
      qq=0





c     Programme principal



      open(unit=10,file='LFD23ds.par',status='old')
      read(10,*) mm,aa
      read(10,*) liss
      call datef(aa,mm,jj,juld,nbjp,nbjm)
      jjf=nbjm
      julf=juld+nbjm-1
      close(unit=10)
 

c     création du nom du fichier (ex: pour juillet 1998 --> LFD-199807.3ds)

      eanm=aa/1000
      eanc=(aa-eanm*1000)/100
      eand=(aa-eanm*1000-eanc*100)/10
      eanu=aa-eanm*1000-eanc*100-eand*10
      emod=mm/10
      emou=(mm-emod*10)
      nom='LFD-'//convert(eanm+1)//convert(eanc+1)//convert(ea
     +nd+1)//convert(eanu+1)//convert(emod+1)//convert(emou+1)//'.3ds'
      open(unit=16,file=nom,status='unknown')


ccccccccccc    Traitement du mois précédent celui demandé (pour le lissage)   cccccccccccc


c     Base de donnée à Lavoué    c


      do p=1,181
      do q=1,360
      do r=1,10 
        matlavoueb(p,q,r)=0.
        matlavoueo(p,q,r)=0.
        emissionbc(p,q,r)=0.
        emissionoc(p,q,r)=0.
      enddo
      enddo
      enddo

      open(unit=14,file='Lavoue_et_al_bb+ff_bc.3ds',status='old')
      open(unit=15,file='Lavoue_et_al_bb+ff_oc.3ds',status='old')



      read (14,*) bidon
      read (15,*) bidon

      do i=1,lalignes


      read (14,*,end=345) lajouri,lamoisi,laani,lajourf,lamoisf,
     +laanf,lalat,lalon,hauteur,bidon,blackc,bidon,bidon,bidon
      
      read (15,*,end=345) lajouri,lamoisi,laani,lajourf,lamoisf,
     +laanf,lalat,lalon,hauteur2,bidon,bidon,bidon,bidon,orgc


      lalat=lalat+91.
      if (lalon.lt.0.) then
      lalon=lalon+360.
      endif
       

      pmin=100000.
      pmin2=100000.
      do z=1,10
      min=abs(hauteur-hautmoy(z))
      min2=abs(hauteur2-hautmoy(z))
      if(min.lt.pmin) then
      pmin=min
      casefeu=z
      endif
      if(min2.lt.pmin2) then
      pmin2=min2
      casefeu2=z
      endif
      enddo


      if (mm-1.eq.0) then
        mm=13
      endif
      if ((mm-1.eq.lamoisi).and.(hauteur.gt.200.)) then     
      matlavoueb(nint(lalat),nint(lalon),casefeu)= matlavoueb(nint
     +(lalat),nint(lalon),casefeu)+blackc
      endif
      if ((mm-1.eq.lamoisi).and.(hauteur2.gt.200.)) then  
      matlavoueo(nint(lalat),nint(lalon),casefeu2)= matlavoueo(nint
     +(lalat),nint(lalon),casefeu)+orgc
      endif
      if (mm.eq.13) then
        mm=1
      endif

      enddo

  345  print*,'loading...'



      do o=nint(juld)-(liss-1),nint(juld)-1

      print*,o-juld+liss,"/",liss+julf-juld


      do p=1,181
      do q=1,360
        matrice(p,q)=0.
        matmoy(p,q)=0.
      enddo
      enddo

      do p=1,181
      do q=1,360
      do r=1,10 
        emissionbc(p,q,r)=0.
        emissionoc(p,q,r)=0.
      enddo
      enddo
      enddo




      open(unit=11,file='LFD_A02_5999_j.txt',status='old')
      read (11,*) bidon

      do i=1,nblignes
      read (11,*,end=123) a,m,j,lat,lon,size
      call datef(a,m,j,julien,nbjb,toto)      

       if (lon.le.0.) then
        lon=lon+360.
       endif
     


c   Matrice de surface totale brûlée pour periode specifiee (mois précédent) par jour

      if ((nint(julien).eq.o).and.(a.eq.aa)) then
      matrice(nint(lat)+91,nint(lon))=matrice(nint(lat)+91,
     +nint(lon))+size
      endif



c   Matrice de surface moyenne par mois jour (du mois précédent) de la base de donnees au complet



      if ((a.ge.1980).and.(a.lt.1990)) then
      if (mm-1.eq.0) then
        mm=13
      endif
      if (mm-1.eq.m) then
c
c  41 est le nombre d'annees de donnees au jour de l'ecriture de ce code
c

      matmoy(nint(lat)+91,nint(lon))=matmoy(nint(lat)+91,
     +nint(lon))+(size/(41.*real(nbjm)))
      endif
      if (mm.eq.13) then
        mm=1
      endif
      endif

      enddo
  123  close(unit=11)


      

      do p=1,181
      do q=1,360
      do r=1,10 
      if (matmoy(p,q).gt.0.) then
      emissionbc(p,q,r)=((matrice(p,q)*matlavoueb(p,q,r))/matmoy(p,q))
      emissionoc(p,q,r)=((matrice(p,q)*matlavoueo(p,q,r))/matmoy(p,q)) 
      else
      emissionbc(p,q,r)=0.
      emissionoc(p,q,r)=0.
      endif     
      enddo
      enddo
      enddo



      do r=1,10 
      do p=1,181
      do q=1,360

      if ((matrice(p,q).ne.0.).and.(matmoy(p,q).ne.0.).and.(matlavoueo
     +(p,q,r).ne.0.)) then
      print*, jj,mm,p,q,matrice(p,q),matmoy(p,q),matlavoueo(p,q,r)
      endif
      if (q.gt.180) then
      qq=q-360
      else
      qq=q
      endif

      if ((emissionbc(p,q,r).ne.0.).or.(emissionoc(p,q,r).ne.0.)) then

      do i=nint(juld)-(liss-1),nint(juld)-1
      if (i+(liss-1).ge.nint(juld)) then
      write (16,*) real(i+liss)-juld,mm,aa,real(i+liss)-juld,mm,
     +aa,p-91,qq,hautmoy(r),' 0. ',emissionbc(p,q,r)/real(liss),' 0. '
     +,' 0. ',emissionoc(p,q,r)/real(liss)
      endif
      enddo

      endif

      enddo
      enddo
      enddo



c    fin do o
      enddo
c    fin do o

      close(unit=14)
      close(unit=15)


cccccccccccccccccc         Traitement du mois demandé          ccccccccccccccccccccc


c     Partie de traitement des données à Lavoué  2    c


      do p=1,181
      do q=1,360
      do r=1,10 
        matlavoueb(p,q,r)=0.
        matlavoueo(p,q,r)=0.
        emissionbc(p,q,r)=0.
        emissionoc(p,q,r)=0.
      enddo
      enddo
      enddo


      open(unit=14,file='Lavoue_et_al_bb+ff_bc.3ds',status='old')
      open(unit=15,file='Lavoue_et_al_bb+ff_oc.3ds',status='old')



      read (14,*) bidon
      read (15,*) bidon

      do i=1,lalignes


      read (14,*,end=346) lajouri,lamoisi,laani,lajourf,lamoisf,
     +laanf,lalat,lalon,hauteur,bidon,blackc,bidon,bidon,bidon
      
      read (15,*,end=346) lajouri,lamoisi,laani,lajourf,lamoisf,
     +laanf,lalat,lalon,hauteur2,bidon,bidon,bidon,bidon,orgc


      lalat=lalat+91.
      if (lalon.lt.0.) then
      lalon=lalon+360.
      endif
       

      pmin=100000.
      pmin2=100000.
      do z=1,10
      min=abs(hauteur-hautmoy(z))
      min2=abs(hauteur2-hautmoy(z))
      if(min.lt.pmin) then
      pmin=min
      casefeu=z
      endif
      if(min2.lt.pmin2) then
      pmin2=min2
      casefeu2=z
      endif
      enddo



      if ((mm.eq.lamoisi).and.(hauteur.gt.200.)) then     
      matlavoueb(nint(lalat),nint(lalon),casefeu)= matlavoueb(nint
     +(lalat),nint(lalon),casefeu)+blackc
      endif
      if ((mm.eq.lamoisi).and.(hauteur2.gt.200.)) then  
      matlavoueo(nint(lalat),nint(lalon),casefeu2)= matlavoueo(nint
     +(lalat),nint(lalon),casefeu)+orgc
      endif
     


      enddo

  346  print*,'loading...'






      do o=nint(juld),nint(julf)

      print*,o-juld+liss,"/",liss+julf-juld

      do p=1,181
      do q=1,360
        matrice(p,q)=0.
        matmoy(p,q)=0.
      enddo
      enddo

      do p=1,181
      do q=1,360
      do r=1,10 
        emissionbc(p,q,r)=0.
        emissionoc(p,q,r)=0.
      enddo
      enddo
      enddo

      open(unit=11,file='LFD_A02_5999_j.txt',status='old')
      read (11,*) bidon
        


      do i=1,nblignes
      read (11,*,end=124) a,m,j,lat,lon,size
      call datef(a,m,j,julien,nbjb,toto)    


       if (lon.le.0.) then
        lon=lon+360.
       endif
     


c     Matrice de surface totale brûlée pour periode specifiee par jour


      if ((nint(julien).eq.o).and.(a.eq.aa)) then
      matrice(nint(lat)+91,nint(lon))=matrice(nint(lat)+91,
     +nint(lon))+size
      endif



c     Matrice de surface moyenne par mois jour de la base de donnees au complet

      if ((a.ge.1980).and.(a.lt.1990)) then
      if (mm.eq.m) then
      matmoy(nint(lat)+91,nint(lon))=matmoy(nint(lat)+91,
     +nint(lon))+(size/(41.*real(nbjm)))
      endif
      endif


      enddo
  124  close(unit=11)

      

     

   


      do p=1,181
      do q=1,360
      do r=1,10 
      if (matmoy(p,q).gt.0.) then
      emissionbc(p,q,r)=((matrice(p,q)*matlavoueb(p,q,r))/matmoy(p,q))
      emissionoc(p,q,r)=((matrice(p,q)*matlavoueo(p,q,r))/matmoy(p,q)) 
      else
      emissionbc(p,q,r)=0.
      emissionoc(p,q,r)=0.
      endif     
      enddo
      enddo
      enddo
  


      do r=1,10 
      do p=1,181
      do q=1,360

      if ((matrice(p,q).ne.0.).and.(matmoy(p,q).ne.0.).and.(matlavoueo
     +(p,q,r).ne.0.)) then
      print*, jj,mm,p,q,matrice(p,q),matmoy(p,q),matlavoueo(p,q,r)
      endif
      if (q.gt.180) then
      qq=q-360
      else
      qq=q
      endif

      if ((emissionbc(p,q,r).ne.0.).or.(emissionoc(p,q,r).ne.0.)) then

      do i=o,o+liss-1
      if (i.le.nint(julf)) then
      write (16,*) real(i)-juld+1.,mm,aa,real(i)-juld+1.,mm,aa,
     +p-91,qq,hautmoy(r),' 0. ',emissionbc(p,q,r)/real(liss),' 0. ',
     +' 0. ',emissionoc(p,q,r)/real(liss)
      endif
      enddo

      endif

      enddo
      enddo
      enddo



c    fin do o
      enddo
c    fin do o

      close(unit=14)
      close(unit=15)
      close(unit=16)
      close(unit=17)
      close(unit=18)
      close(unit=19)


      end

      
c================================================================================================================




c================================================================================================================
c       Ceci est un programme pour determiner quel jour julien de l'année on est !
c       Yassine Chihab et Jonathan Poisson-Rioux Copyright 2003
c
       subroutine datef(a,m,j,julien,nbj,nbjm)
c
c       Declaration des variable
c

       
      integer j,m,h,min,s,mois365 (12),mois366 (12),a, nbj,nbjm
      real julien

c
c      Initialisation des variables
c
      data mois365 /31,28,31,30,31,30,31,31,30,31,30,31/
      data mois366 /31,29,31,30,31,30,31,31,30,31,30,31/
      julien=0.

      nbj=365
      h=0
      min=0
      s=0


c
c       Programme principal
c


c       print *, ' Entrer la date jj mm aaaa et hh min sec'
     
      
c      open (unit=30, file= 'date.in', status = 'unknown')
c      read(30,*)
      
c      print *, 'jour='
c      read*,j 
      
c      print *, 'mois='
c      read*, m        
      if ((m.gt.12).or.(m .lt. 1)) then 
      print*, 'Impossible ! Le mois entré est éronné ! '
      stop
      endif
   
      if ((j.gt.mois366(m)).or.(j.lt.1)) then   
      print *,'Impossible ! Le jour entré est éronné !'
      stop
      endif

c      print *, 'année='
c      read*, a
      
c      print *, 'heure='
c      read*, h
c      if ((h .gt. 24).or.(h .lt. 0)) then 
c      print*, 'Impossible ! L heure entrée est éronnée ! '
c      stop
c      endif

c      print*, 'minute'
c      read*, min
c      if ((min .gt. 60).or.(min .lt. 0)) then 
c      print*, 'Impossible ! La minute entrée est éronnée! '
c      stop
c      endif

c      print *, 'seconde='
c      read*,s
c      if ((s .gt. 60).or.(s .lt. 0)) then 
c      print*, 'Impossible ! La secconde entrée est éronnée ! '
c      stop
c      endif


      do i=1,m-1
      julien=julien+real (mois365(i))
      enddo

      nbjm=mois365(m)

      if (real(a/4).eq.real (a)/4.) then
      julien=0.
      nbj=366
      nbjm=mois366(m)
      do i=1,m-1
      julien=julien+real(mois366(i))
      enddo
      endif

      if (real(a/100).eq.real (a)/100.) then
      julien=0.
      nbj=365
      nbjm=mois365(m)
      do i=1,m-1
      julien=julien + real(mois365(i))
      enddo
      endif   

      if (real(a/400).eq.real (a)/400.) then
      julien=0.
      nbj=366
      nbjm=mois366(m)
      do i=1,m-1
      julien=julien + real(mois366(i))
      enddo
      endif

      if (real(a/4000).eq.real (a)/4000.) then
      julien=0.
      nbj=365
      nbjm=mois365(m)
      do i=1,m-1
      julien=julien + real(mois365(i))
      enddo
      endif 


      julien=julien+real(j)+real((h*3600)+(min*60)+(s))/86400.

c      print*, 'Le jour julien est le numéro', julien

c      open (unit=31, file= 'date.out', status = 'unknown')
c      write (31,*),'Le jour julien de la date entrée est '
c      write (31,*),julien
c      close(unit=31)

      close(unit=30)

      return
      end

c======================================================================== 


      subroutine writemoypgm(matmoy,sizemoy)


c
c ------------------
c
c   fabrication des images pgm (valeur = )
c
c     Déclaration des variables
c

      real sizemoy,matmoy(181,360)

c
c      Initialisation des variables
c


            
      print*,'Making optical depth pgm image...'

     
  



c        Carte representative moyenne       
         print*,'Making optical depth pgm image...'
         open(unit=40,file= 'carte-moy.pgm',status='unknown')
         write(40,171) 'P2'
         write(40,174)
         write(40,1) 
         write(40,2)
         write(40,3) 
            write(40,172) 
            write(40,*) nint(sizemoy)
            do i=181,1,-1
                  write(40,*) (nint(matmoy(i,j)),j=1,360)
            enddo
         close(unit=40)
         
c        Carte moyenne representative standard
         open(unit=41,file= 'carte-moyrep.pgm',status='unknown')
         write(41,171) 'P2'
         write(41,1) 
         write(41,2)
         write(41,3) 
            write(41,172) 
            write(41,*) '255'
            do i=181,1,-1
                  write(41,*) (nint(255.*matmoy(i,j)/sizemoy),j=1,360)
            enddo
         close(unit=41)



 171  format(a)
 172  format('360 181')
 174  format('# surface brulee par deg carre')
 1    format('# pixsiz 1.000')
 2    format('# lat0 -90.00')
 3    format('# lon0 0.')
      return
      end







c========================================================================

      subroutine writepgm(matrice,maxsize)


c
c ------------------
c
c   fabrication des images pgm (valeur = )
c
c     Déclaration des variables
c

      real maxsize,matrice(181,360)
c
c      Initialisation des variables
c


            
      print*,'Making optical depth pgm image...'
         
c        Carte representative         
         open(unit=50,file= 'carte-rep.pgm',status='unknown')
         write(50,171) 'P2'
         write(50,174)
         write(50,1) 
         write(50,2)
         write(50,3) 
            write(50,172) 
            write(50,*) nint(maxsize)
            do i=181,1,-1
                  write(50,*) (nint(matrice(i,j)),j=1,360)
            enddo
         close(unit=50)
         
c        Carte standard          
         open(unit=51,file= 'carte.pgm',status='unknown')
         write(51,171) 'P2'
         write(51,1) 
         write(51,2)
         write(51,3) 
            write(51,172) 
            write(51,*) '255'
            do i=181,1,-1
                  write(51,*) (nint(255.*matrice(i,j)/maxsize),j=1,360)
            enddo
         close(unit=51)
 


 171  format(a)
 172  format('360 181')
 174  format('# surface brulee par deg carre')
 1    format('# pixsiz 1.000')
 2    format('# lat0 -90.00')
 3    format('# lon0 0.')
      return
      end
















