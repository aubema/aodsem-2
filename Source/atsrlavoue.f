C    Ce programme permet d'obtenir les données d'une base de données en fonction du jour
C    Copyright Stéphanie Dumaine Allard 2003
c
c    modifie par Martin Aube 2004
c
C    Identification des variables
C     d=date
C     a=année
C     m=mois
C     j=jour
C     lat=latitude
C     lon=longétitude
C     h=heure
C     dae=année écrite par l'utilisateur
C     dme=mois écrit pas l'utilisateur
C     dje=jour écrit par l'utilisateur
c
C     Déclaration des variables
c
      integer a,m,j,d,nlign,scrap,idje,ijef,ii,jo,
     +nd,nnd,nombref,doy,na,nat,z,i,k,ndat,mef,aef,jef,
     +nbboucle,nb,me,je,ae,annees(50),casefeu2,n
      integer laji,lami,laai,lajf,lamf,laaf,case,casefeu
      real lat,lon,h,nbdat,Nbmoy(181,360),anmois(12),
     +Nbemisbc(181,360,10),Nbemisoc(181,360,10),
     +Nblavbc(181,360,10),Nbfeu(31,181,360),Nblavoc(181,360,10),
     +min,pmin,haumoy,bidon,oc,bc,lalat,lalon,lahau,
     +laji2, lami2, laai2, lajf2, lamf2, laaf2
     + ,lalat2, lalon2, lahau2,pmin2,min2
       real test1,test2,hauteur(10),njmois(12),cloudf
c      
c      
      character*20 nom(10000)
      character*60 nomout
c     
C     Initialisation des variables
c      
      data hauteur /82.5,255.,443.5,651.,883.,1146.,1448.5,1805.5,
     +7000.,21000./
      

      data njmois /31.,28.25,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./
 
      ndat=0    
      a=0
      m=1
      d=1      
      lat=0.0
      lon=0.0
      h=0.0
      ae=0
      me=0
      je=0
      nd=0
      nnd=0
      nombref=0
      jday=0.
      toto=1.
      nbdat=0.
      nat=1
      laji=0
      lami=0
      laai=0
      lajf=0
      lamf=0
      laaf=0
      do na=1,12
        anmois(na)=0.
      enddo
      do i=1,360
        do j=1,181
          Nbmoy(j,i)=0.
          do ii=1,31
	    Nbfeu(ii,j,i)=0.
          enddo
        enddo
      enddo  
      do i=1,360
        do j=1,181
          do k=1,10
            Nblavbc(j,i,k)=0.
            Nblavoc(j,i,k)=0.
            Nbemisbc(j,i,k)=0.
            NBemisoc(j,i,k)=0.
          enddo
        enddo
      enddo

C     ************************************************************
C                       Programme principal
C     ************************************************************
       
      open(unit=9,file='atsrlavoue.par',status='OLD')
        print*,'Reading parameter file...'
        read(9,*) nomout
        read(9,*) ae, me, je
        read(9,*) aef, mef, jef
c mean monthly cloud fraction
        read(9,*) cloudf
      close(unit=9)
c
C       *************************** Lavoue et al ***************
c
      print*,'Chargement des fichiers de Lavoue et al.'
      open(unit=7,file='Lavoue_et_al_bb+ff_bc.3ds',status='old')
         open(unit=8,file='Lavoue_et_al_bb+ff_oc.3ds',status
     +   ='old')  
             read(7,*) nlign
             read(8,*) scrap
             print*,'Nombre de lignes de donnees:',nlign
             do i=1,nlign
                read(7,*) laji,lami,laai,lajf,lamf,laaf
     +          ,lalat,lalon,lahau,bidon,bc,bidon,bidon,bidon   
                if (lalat.gt.90.) print*,lalat,'lalat'
                if (lalat.lt.-90.) print*,lalat,'lalat'
                if (lalat.gt.360.) print*,lalon,'lalon'
                read(8,*) laji2,lami2,laai2,lajf2,lamf2,laaf2
     +          ,lalat2,lalon2,lahau2,bidon,bidon,bidon,bidon,oc
                if (lalon.le.0.) then
                   lalon=lalon+360.
                endif
                pmin=1000000.
                pmin2=1000000.
                do z=1,10
c                  print*,'hauteur'
                   min=abs(lahau-hauteur(z))
                   min2=abs(lahau2-hauteur(z))
                   if (min.lt.pmin) then
                      pmin=min
                      casefeu=z
                   endif
                   if (min2.lt.pmin2) then
                      pmin2=min2
                      casefeu2=z
                   endif
                enddo 
                if ((me.eq.lami).and.(lahau.gt.200.)) then
                    Nblavbc(nint(lalat)+91,nint(lalon),casefeu)=
     +              Nblavbc(nint(lalat)+91,nint(lalon),casefeu)+bc
                endif
                if ((me.eq.lami).and.(lahau2.gt.200.)) then
                    Nblavoc(nint(lalat)+91,nint(lalon),casefeu2)=
     +              Nblavoc(nint(lalat)+91,nint(lalon),casefeu2)+oc
                endif
             enddo
         close(unit=8)
      close(unit=7)
      open(unit=1,file='output.3ds',status='unknown') 
c
c   calcul de la moyenne mensuelle du nombre de feux journalier
c
          print*,'Calcul de la moyenne mensuelle des emissions...'
          open(unit=2,file='atsrlist',status='old')
            read(2,*) nombref
            do i=1,nombref
              read(2,*) nom(i)
              if (nom(i)(3:4).eq.'01') anmois(1)=anmois(1)+1.
              if (nom(i)(3:4).eq.'02') anmois(2)=anmois(2)+1.
              if (nom(i)(3:4).eq.'03') anmois(3)=anmois(3)+1.
              if (nom(i)(3:4).eq.'04') anmois(4)=anmois(4)+1.
              if (nom(i)(3:4).eq.'05') anmois(5)=anmois(5)+1.
              if (nom(i)(3:4).eq.'06') anmois(6)=anmois(6)+1.
              if (nom(i)(3:4).eq.'07') anmois(7)=anmois(7)+1.
              if (nom(i)(3:4).eq.'08') anmois(8)=anmois(8)+1.
              if (nom(i)(3:4).eq.'09') anmois(9)=anmois(9)+1.
              if (nom(i)(3:4).eq.'10') anmois(10)=anmois(10)+1.
              if (nom(i)(3:4).eq.'11') anmois(11)=anmois(11)+1.
              if (nom(i)(3:4).eq.'12') anmois(12)=anmois(12)+1.
            enddo
          close(unit=2)     
          do i=1,nombref
            open(unit=21,file=nom(i),status='old')
c              print*,'Lecture du fichier:',nom(i)
              do n=1,10000000
                read(21,*,end=100) d, h, lat, lon
                if (nint(lon).le.0) lon=lon+360.
                a=d/10000
                if (a.lt.90) then
                  a=a+2000
                else
                  a=a+1900
                endif
                nd=d-d/10000*10000
                m=nd/100
                nnd=nd-nd/100*100
                jo=nnd
                if ((ae.eq.aef).and.(me.eq.mef)) then
                  if (m.eq.me) then
                    Nbmoy(nint(lat)+91,nint(lon))=Nbmoy(nint(lat)+
     +              91,nint(lon))+1.
 
                  endif          
                endif
              enddo
            close(unit=21)
  100     enddo
          if (me.gt.12) then
            print*,'Le mois est superieur a 12!'
	    stop
          endif
          do i=1,360
            do j=1,181
              Nbmoy(j,i)=Nbmoy(j,i)/anmois(me)/njmois(me)
            enddo          
	  enddo 
c
c===================================
c
c  Calcul du nombre de feux par jour pour chaque jour du mois
c
c   boucle sur les jours
c
        do ii=je,jef
          open(unit=2,file='atsrlist',status='old')
            read(2,*) nombref
            do i=1,nombref
              read(2,*) nom(i)
            enddo
          close(unit=2)     
          do i=1,nombref
            open(unit=21,file=nom(i),status='old')
c              print*,'Lecture du fichier:',nom(i)
              do n=1,10000000
                read(21,*,end=110) d, h, lat, lon
                if (nint(lon).le.0) lon=lon+360.
                a=d/10000
                if (a.lt.90) then
                  a=a+2000
                else
                  a=a+1900
                endif
                nd=d-d/10000*10000
                m=nd/100
                nnd=nd-nd/100*100
                jo=nnd
                if ((a.eq.ae).and.(m.eq.me).and.(jo.eq.ii)) then
                  Nbfeu(ii,nint(lat)+91,nint(lon))=Nbfeu(ii,nint(lat)+
     +            91,nint(lon))+1.
                endif
              enddo
            close(unit=21)
  110     enddo
          print*,'Calcul des emissions du',ii,me,ae
          do i=1,360
            do j=1,181
              do k=1,10  
                if ((Nbfeu(ii,j,i).gt.0.).and.(Nblavbc(j,i,k).gt.0.)
     +          .and.(Nbmoy(j,i).gt.0.)) then
c
c nous augmentons la valeur du nombre moyen de feux par le facteur 1/(1-cloudf)
c
                  Nbemisbc(j,i,k)=Nbfeu(ii,j,i)*Nblavbc(j,i,k)/
     +            (Nbmoy(j,i)/(1.-cloudf))    
                else 
                  Nbemisbc(j,i,k)=0.
                endif     
                if ((Nbfeu(ii,j,i).gt.0.).and.(Nblavoc(j,i,k).gt.0.)
     +          .and.(Nbmoy(j,i).gt.0.)) then
                  Nbemisoc(j,i,k)=Nbfeu(ii,j,i)*Nblavoc(j,i,k)/
     +            (Nbmoy(j,i)/(1.-cloudf))   
                else 
                  Nbemisoc(j,i,k)=0.   
                endif
              enddo
            enddo
          enddo
c
c  ecriture du 3ds
c 
          do k=1,10
            do j=181,1,-1   
              do i=1,360  
                if ((Nbemisoc(j,i,k).ne.0.).or.(Nbemisbc(j,i,k).ne.0.)) 
     +          then
                  ndat=ndat+1
                  write(1,*) ii,me,ae,ii,me,ae,real(j-91)+.5,real(i)+
     +            0.5,hauteur(k),' 0. ',Nbemisbc(j,i,k),' 0. 0. ',
     +            Nbemisoc(j,i,k)
                endif
              enddo
            enddo
          enddo
c                   
c  fin du do sur chaque jour
        enddo
      close(unit=1)
      open(unit=1,file='output.3ds',status='unknown')           
        open(unit=2,file=nomout,status='unknown')
          print*,'Ecriture du fichier de sortie: atsr.3ds ...'
          write(2,*) ndat, ' daily'
          do i=1,ndat

            read(1,*) laji , lami, laai, lajf, lamf, laaf
     +      ,lalat, lalon , lahau, bidon, bc, bidon, bidon, oc  
            write(2,*) laji , lami, laai, lajf, lamf, laaf
     +      ,lalat, lalon , lahau, bidon, bc, bidon, bidon, oc
          enddo

          print*,'Fermeture du fichier: atsr.3ds.'      
        close(unit=2)
      close(unit=1)
c
c      ************************ Écriture des cartes ****************
c         
c      open(unit=1,file='Nombrefeux.pgm',status='unknown')
c	print*,'Ecriture fichier: Nombrefeux.pgm'
c        print*,Nbfeu
c        write(1,1001)
c        write(1,1000)
c	write(1,1004)
c	write(1,1005)
c	write(1,1006)
c        write(1,1002)
c        write(1,1003)
c        do j=181,1,-1
c          write(1,*) (10.*Nbfeu(j,i),i=1,360)
c        enddo
c      close(unit=1)
c       print*,'Termine'
      if (me.eq.mef) then
        open(unit=11,file='Moyennefeux.pgm',status='unknown') 
	print*,'Ecriture fichier: Moyennefeux.pgm'
          write(11,1001)
          write(11,1000)
	  write(11,1004)
	  write(11,1005)
	  write(11,1006)
          write(11,1002)
          write(11,1003)
          do j=181,1,-1
            write(11,*)  (int(10.*Nbmoy(j,i)),i=1,360)  
          enddo
        close(unit=11)
      else     
        print*,'Le mois ou lannée de départ nest pas égale au mois ou'   
        print*,'a lannée de fin La moyenne ne pourra pas être calculée' 
      endif
      print*, 'Fin normale du programme'
 1000 format('# image cree par Feux.f S. Dumaine 2003')
 1001 format('P2')
 1002 format('360 181')
 1003 format('255')
 1004 format('# pixsiz 1.000')
 1005 format('# lat0   -90.000')
 1006 format('# lon0   0.000001')
      stop
      end





























