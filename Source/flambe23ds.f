c   programme de conversion des fichiers de flux de FLAMBE du NRL
c   obtenu de Jeff Reid
c
c copyright Martin Aubé 2002
c
c
c
      character*60 fichier,nomf,nomlis
      character*11 bidon
      integer i,j,an,mois,jour,year(1000000),month(1000000),
     +day(1000000),day1(1000000),day2(1000000),k,heure,hour1(1000000),
     +hour2(1000000)
      real lat(1000000),lon(1000000),bidonr,surf(1000000),
     +ocsurbc,emisbc(1000000),fluxmax,hmax,height,dangx,dangy,
     +ccontent,nflux(1000000),surface,dx,dy,lcellx,lcelly
      real duration(1000000)
       real*8 hre,min,sec,jou,moi,ann,jday
c
c     valeurs initiales
c
      k=0
      ndata=0
      fluxmax=0.
      dangx=1.
      dangy=dangx
c
c     rapport oc/bc
c
      ocsurbc=19.5
c
c  total carbon content = 60% as stated by Jeff Reid 2002 (personal comm)
c  51% Sussot et al., 1998 (a confirmer)
      ccontent=0.6
c
c  hauteur maximale
c
      hmax=7300.
       open(unit=3,file='flambe23ds.par',status='old')
         read(3,*) nomlis
         read(3,*) nomf
       close(unit=3)
c
c     ouverture du fichier liste des fichiers
c
      open(unit=1,file=nomlis,status='old')
        do i=1,1000000
          read(1,*,end=100) fichier
          backspace 1





c          read(1,10,end=200) bidon,an,mois,jour
c          print*,'Processing ',bidon,an,mois,jour
          read(1,10,end=200) bidon,an,mois,jour,heure
          print*,'Processing ',bidon,an,mois,jour,heure
          call  julian(dble(heure),dble(0.),dble(0.),dble(jour),
     +        dble(mois),dble(an),jday)
cccc to insure a best model - aeronet fit it seem that the injections
cccc have to be done 6h later          
cccc      e.g. jday=jday+0.25
          jday=jday+0.25
          call timedate(hre,min,sec,jou,moi,ann,jday)
          an=nint(ann)
          mois=nint(moi)
          jour=nint(jou)
          heure=nint(hre)



        open(unit=2,file=fichier,status='old')
          do j=1,1000000
            k=k+1
            read(2,*,end=200) lat(k),lon(k),lat(k),lon(k),bidonr,
     +      surf(k),nflux(k),bidon,bidon,duration(k)
c conversion des unites kg en tonnes/an en tenant compte que le feux 
c c'est ainsi que le 3ds est traite (le flux est mis a jour a tous les 6h pour
c les emissions classees hourly
c un feux de moins de 6h est considere avoir brule pendant 6h et il faut donc
c normalise le emission rate pour le ramener a une frequence de 1/6h (d'ou 
c le facteur 4
c            emisbc(k)=4.*surf(k)*nflux(k)/1000.*365.25/(ocsurbc+1.)
c     +      *24.*3600./duration(k) 
c
c  important note, the factor 7 below have been set to insure a better fit between
c  model forecast and aeronet data. This value is consistent with O'Neill et al 2006
c Atmospheric Environment 40 3737-3749
c
           emisbc(k)=4.*surf(k)*nflux(k)/1000.*365.25/(ocsurbc+1.)
     +      *7. 

            emisbc(k)=emisbc(k)*ccontent
            if (emisbc(k).eq.0.) then
                    k=k-1
            endif        
            if (nflux(k).gt.fluxmax) then
              fluxmax=nflux(k)
            endif
            year(k)=an
            month(k)=mois
c            day(k)=jour



            day1(k)=jour
            day2(k)=jour



            hour1(k)=heure
            if (hour1(k)+6.eq.24) then
               hour2(k)=0
               day2(k)=day1(k)+1
            else 
               hour2(k)=hour1(k)+6
            endif





          enddo
 200    close(unit=2)
        enddo
 100  close(unit=1)
 

 
        lcellx=1.
	lcelly=1. 
c
c   ecriture du 3ds
c
        open(unit=1,file=nomf,status='unknown')
          write(1,*) k, ' hourly'
            do j=1,k

            height=hmax*nflux(j)/fluxmax
	    
	    
c  convertir en tonne/an/deg^2 (i.e. repartir l'emission en tonne provenant de 
c  la surface brulee sur une surface de 1 deg x 1 deg
c	    call echelle(lat(j),lcellx,lcelly,dx,dy)             
c            surface=dx*dy
c	    emisbc(j)=emisbc(j)*surf(j)/surface





c            write(1,20) day(j),month(j),year(j),day(j),month(j),
c     +      year(j),lat(j),lon(j),height,emisbc(j),
c     +      emisbc(j)*ocsurbc
            write(1,20) hour1(j),day1(j),month(j),year(j),
     +      hour2(j),day2(j),month(j),
     +      year(j),lat(j),lon(j),height,emisbc(j),
     +      emisbc(j)*ocsurbc











          enddo
        close(unit=1)

 10    format(A11,I4,I2,I2,I2)
 20    format(I2,1x,I2,1x,I2,1x,I4,1x,I2,1x,I2,1x,I2,1x,I4,1x,F7.2,1x,
     + F7.2,1x,f7.1,1x,' 0. ',f12.3,1x,' 0. 0. ',f12.3)
         stop
         end
c
c ---------------------------------------------------------
c
c   routine permettant le calcul de l echelle d une cellule centree
c   a latt et avec une largeur angulaire dang pour une terre 
c   ellipsoide.
c
c   epsilon = difference d angle entre l horizon sur une sphere et
c             l horizon sur un ellipsoide
c   dangx =  taille de la cellule en degres
c   dangy =  taille de la cellule en degres
c   dx = taille de la cellule dans la direction s-n [m]
c   dy = taille de la cellule dans la direction w-e [m]
c   r = distance du point geographique par rapport au centre de la terre
c   a = demi grand axe (centre a l equateur)
c   b = demi petit axe (centre au pole)
c   he = angle d horizon de l ellipsoide
c   hs = angle d horizon de la sphere
c   x = coord de l ellipsoide
c   y=  coord de l ellipsoide
c   
c
      subroutine echelle(latt,dangx,dangy,dx,dy)
      real latt, dangx,dangy,dx, dy, r, a, b, epsilon,pi,x,y,he,hs
      pi=3.141592654
      b=6357000.
      a=6378000.
      latt=abs(latt)
      if (latt.lt.0.01) latt=0.01
      x=((b**2.)/((tan(pi*latt/180.))**2.+(b/a)**2.))**0.5
      y=(b**2.-b**2.*x**2./a**2.)**0.5
      r=(x**2.+y**2.)**.5
      hs=90.-latt
      he=180.*atan(((b**2.)/(a**2.))*x/y)/pi
      epsilon=hs-he
      dx=r*(tan(dangx*pi/180.))/cos(pi*epsilon/180.)
      dy=x*(tan(dangy*pi/180.))
      return
      end

