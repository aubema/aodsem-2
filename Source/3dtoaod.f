c    programme de simulation de l epaisseur optique des aerosols
c    mesuree par un capteur pointant vers le haut
c    dans une direction quelconque.  l epaisseur optique est 
c    calculee en integrant l effet de chaque cellule de la matrice 3-d
c    le long de la ligne de vise.  
c    pour chaque couche du modele, nous retiendrons les proprietes de la 
c    cellule correspondant a la face inferieure traversee par la ligne
c    de vise.  la ligne de vise est definie par la position du 
c    photometre (xpho,ypho,zpho) et l'angle d'altitude
c    de vise (elepho) ainsi que par l'angle
c    azimutal de vise (phypho).
c 
c    elepho est l angle d elevation par rapport a l'horizontale
c    phipho est l angle du nord vers l est sur le plan horizontal
c
c    Le programme permet aussi l'integration de l epaisseur optique 
c    selon l un des trois axes et entre des bornes entieres correspon-
c    dant aux elements de la matrice 3-d.
c  
c    Altitude cible-capteur variable
c
c    Un photometre pointant vers le sol peut etre simule de la facon 
c    suivante.  Determiner le point observe au sol et le poser egal a 
c    la position d un photometre fictif.  Determiner les angles de 
c    d elevation et d azimut de la droite joignant ce point et le photo
c    metre en altitude.  Calculer l'epaisseur optique du photometre
c    fictif et soustraire l epaisseur optique du photometre en altitude
c    en considerant qu il pointe dans la meme direction que le photome-
c    tre fictif.
c    L'angle de diffusion peut 
c    etre defini pour chaque point (x,y,z) par rapport aux angles
c    d'altitude et d'azimut du soleil.
c    
c    les axes sont definis comme suit
c    x = direction nord = degres
c    y = direction est = degres
c    z = direction haut = metres
c
c    le programme calcule les epaisseur optiques de toutes les cellules
c    du modele.  Peut faire l integrale de l epaisseur optique 
c    sur un axe entre deux bornes definies dans le fichier
c    .vis  afin de produire une carte d'epaisseur  optique 
c
c    copyright martin aube 02/11/1999
c
c -----------------
c   identification des variables 
c
c   visfile = fichier contenant l orientation et position du photometre
c             ainsi que la direction du soleil
c   elepho = angle d elevation du photometre en degres
c   phipho = angle d azimut du photometre en degres
c   xpho = position x du photometre (degres)
c   ypho = position y du photometre (degres)
c   zpho = position z du photometre (metres)
c   elesol = angle d elevation du soleil en radians
c   phisol = angle d azimut du soleil en radians
c   datfile = fichier contenant l information sur la distribution 
c             modale et spatiale des aerosols
c   ncellx = nombre de cellules en x (ouest-est) 
c   ncelly = nombre de cellules en y (sud-nord)
c   ncellz = nombre de cellules en z (altitude) 
c   lcellx = largeur de la cellule selon x en degres
c   lcelly = largeur de la cellule selon y en degres
c   lcellz() = largeur de la cellule selon z en metres
c   xcell0 = position x de la cellule du coin sud-ouest (degres)
c   ycell0 = position y de la cellule du coin sud-ouest (degres)
c   zcell0 = position z de la cellule du coin sud-ouest (metres)
c   nbns = nombre de bins de la distribution de taille
c   nref = nombre de type d aerosols (suie, sulfate, sea salt, etc)
c   wavlen = longueur d onde en microns
c   nr = indice de composante
c   nb = indice de bin
c   pi = 3.1415...
c   pi2 = pi * pi
c   numvol() = nombre volumique de chaque mode pour chaque cellule /m^3
c   densite() = densite de chaque type d aerosol kg/m^3
c   volume() = volume d'une particule du debut de chaque bin m^3
c   dx = largeur n-s d une cellule au photometre solaire en metres
c   dy = largeur e-w d une cellule au photometre solaire en metres 
c
c --------------------
c
c   programme principal
c
c
c ----------
c
c   declaration des variables
c
c   nr=10 nrh=13 nb=12 nw=14 i=125 j=275 k=20
c
      real elepho(100),phipho(100),xpho(100),ypho(100),zpho(100)
      real elesol,phisol,lcellx,lcelly
      real lcellz(20),pi2,pi,xcell0,ycell0,zcell0
      real numvol(5,12,125,275,20),zgrid
      real volume(12),densite(10),thetad
      real wavelen(14),rayon1(12)
      real rayon2(12)
      real ke,kecell
      real*8 heure,minute,seconde,jour,mois,annee
      real sigmae(10,13,14,12),taumax
      real taucell(14,125,275,20),taumap(14,125,275),tauvis(12)
      real dx,dy,hz,top,rien
      real cabase(10,13,14,12),csbase(10,13,14,12),cebase(10,13,14,12)
      real mibase(10,13,14),mrbase(10,13,14),wlenbase(14), masksiz
      real pmaskx, pmasky,rhgro(10,13),xrh0,yrh0,zrh0,zrh,zrh_1
      real lcelxrh,lcelyrh,lcelzrh(30),z(21),airmass    
      character*60 datfile, nomtype(10),bidon,nom,paramf
      character*60 visfile,phofile
      character*60 htmfile,aotfile,crofile,rhufile
      character*60 nomtbase(10)
      character*10 binflag(12),waveflag(14)
      character*2 numero
      integer ncellx,ncelly,ncellz,i,j,k,nr,nb,imwidth
      integer rsize,cellvx(30),cellvy(30)
      integer lentype(10),lennom,ia,ib,ic
      integer nwav,nref,nbns,ntop
      integer lenwflag(14),atanu,lencro
      integer lenbflag(12),nw,r,g,b,itaumax,n,nn,kk,ii,jj
      integer rhbase(13),nrbase,nrhbase,nwbase,nbbase,nrb,nwb,nbb
      integer rcc(401),gcc(401),bcc(401),ngris,nrh
      integer pmaskno(15),maskval(15),npm,ncelxrh,ncelyrh,ncelzrh
      integer rh(13),nxrh,nyrh,nzrh,t_drh(181,360,30),nobs,no,nslice,ns
      integer axis(100),liminf(100),limsup(100),ncxi,ncxf,ncyi,ncyf
      integer nczf,kmin,nczi,rh3d(181,360,20)
      real*8 rhjj(1000),hre,min,sec,jou,moi,ann,jday,dtmin
      character*60 nomrh(1000)
      integer nrhfi,nrhf,nws



      character*60 rhufile2
      real*8 rhtim,rhtim2,ordon,pente,dtminm
      integer t_drh2(181,360,30)


c   
c ----------
c
c   initialisation des variables
c
       pi=3.141592654
       pi2=pi*pi
       nwav=14
       data WAVEFLAG/'340','380','440','470','500','550'
     + ,'670','860','870','940','1020','1240','1640','2130'/
       data lenwflag/3,3,3,3,3,3,3,3,3,3,4,4,4,4/
       data WAVELEN/0.34,0.38,0.44,0.47,0.5,0.55,0.67,0.86,0.87
     + ,0.94,1.02,1.24,1.64,2.13/      
c
c -----------
c
c   formats
c
 2200  format(i3,1x,' ** morphologie de l aerosol **')
 2210  format(f10.5,1x,e10.5,' ** indice de refraction (a-ib) **')
 2220  format(f6.2,1x,f6.2,1x,f6.2,' ** angle i, angle f, delta **')
 2230  format(i2,1x,' ** etat de polarisation (1=//,2=_|_,3=random) **')
c
c     lecture du fichier de parametres
c
      open(unit=25,file='3dtoaod.par',status='old',err=8)
       read(25,*) nom
       read(25,*) crofile
      close(unit=25)
c
c    lecture du fichier index d humidite relative
c
      open(unit=7,file="relhum.index",status='old',err=17)
      print*,'Reading rel. hum. file index...'
      read(7,*) nrhfi
      do nrhf=1,nrhfi
       read(7,*) hre,min,sec,jou,moi,ann,nomrh(nrhf)
       call julian(hre,min,sec,jou,moi,ann,jday)
       rhjj(nrhf)=jday
      enddo
      close(unit=7)
c
c -----------
c
c   choix du nom de la racine de fichiers
c
C 3    print*,'Root name of the files (ext .dat and .vis will be add) ?'  
C      read*, nom
c
c   calcul de la longueur du nom
c
      lennom=index(nom,' ')-1
c
c -----------
c
c   noms des fichiers d entree
c
      datfile=nom(1:lennom)//'.dat'
      visfile=nom(1:lennom)//'.vis'
      phofile=nom(1:lennom)//'.pho'
       open(unit=20,file=datfile,status='old',err=3)
       open(unit=7,file=visfile,status='old',err=3)
       open(unit=15,file=phofile,status='unknown')
c
c -----------
c
c   utiliser un fichier de sections efficaces precalculees
c
c   the programm will assume the bmies (.mie.res) data file format for 
c   the cross section database.  make shure to get the rigth format.
c   the aerosol types names have to fit exactly with those of the input
c   file .asn.ref
c
c   example of cross section file
c
c     2  13  7  12                * #types, #r.h., #wavelength, #bins *
c    sulfate_opac        00       * aerosol type *
c   0                             * Relative humitity (0-99)*
c  .440  .143e+01  .100e-07       * wavelength (um),  mr,  mi          *
c     1  .611e-08  .611e-08  .426e-12 * bin number , c, cs, ca (um^2)  *
c     2  .391e-06  .391e-06  .347e-11    
c     3  .249e-04  .249e-04  .299e-10    
c     4  .144e-02  .144e-02  .291e-09    
c     5  .473e-01  .473e-01  .332e-08    
c     6  .623e+00  .623e+00  .316e-07    
c     7  .192e+01  .192e+01  .272e-06    
c     8  .668e+01  .668e+01  .219e-05    
c     9  .264e+02  .264e+02  .155e-04    
c    10  .101e+03  .101e+03  .115e-03    
c    11  .380e+03  .380e+03  .830e-03    
c    12  .152e+04  .152e+04  .651e-02    
c  .500  .143e+01  .100e-07       * wavelength (um),  mr,  mi          *
c     1  .362e-08  .362e-08  .375e-12 * bin number , c, cs, ca (um^2)  *
c     2  .232e-06  .232e-06  .304e-11    
c     3  .148e-04  .148e-04  .258e-10    
c     .
c     .
c     .
c
c   10                            * Relative humitity (0-99)*
c  .440  .143e+01  .100e-07       * wavelength (um),  mr,  mi          *
c     1  .611e-08  .611e-08  .426e-12 * bin number , c, cs, ca (um^2)  *
c     2  .391e-06  .391e-06  .347e-11
c     .
c     .
c     .    
c    
c    
C 6       print*,'Root name of the cross section data base file (extensi 
C     +on .res.bmi will be add)?'
C         read*,crofile
         lencro=index(crofile,' ')-1
         crofile=crofile(1:lencro)//'.res.bmi'
          open(unit=37,file=crofile,status='old',err=6)
c
c ------------------------------------------
c
c   lecture des sorties desirees et conditions geometriques d observ.
c
          print*,'Reading observing geometry file: ',visfile(1:18)
          read(7,*)
          read(7,*) nobs
          do 108 no=1,nobs
             read(7,*) elepho(no)
             read(7,*) phipho(no)
             read(7,*) xpho(no)
             read(7,*) ypho(no)
             read(7,*) zpho(no)
 108      continue
          read(7,*) nslice
          do 110 ns=1,nslice
             read(7,*) axis(ns)
             read(7,*) liminf(ns)
             read(7,*) limsup(ns)
 110      continue
          read(7,*) elesol
          read(7,*) phisol
       close(unit=7)
c
c ------------------------------------------
c
c   lecture des donnees de distribution des aerosols 
c   
c   les donnees sont en unites de 1.e+4 part/m^3
c
          print*,'Reading data file: ',datfile(1:18)
          read(20,*) bidon
          read(20,*) bidon
          read(20,*) heure,minute,seconde,jour,mois,annee
          read(20,*) nbns



c
c -----------
c
c   lecture des bins secs
c
         do 313 nb=1,nbns
            read(20,*) binflag(nb),rayon1(nb),rayon2(nb)
            lenbflag(nb)=index(binflag(nb),' ')-1
 313     continue
c
c ----------
c
c   lecture des donnees concernant la geometrie de la grille du modele
c
          read(20,*) ncellx
          read(20,*) lcellx
          read(20,*) ncelly
          read(20,*) lcelly
          read(20,*) ncellz
          do 100 k=1,ncellz
             read(20,*) lcellz(k)
 100      continue
          read(20,*) xcell0, ycell0, zcell0
c
c -----------
c
c   lecture des types d aerosols presents 
c
          read(20,*) nref
          do 105 nr=1,nref
             read(20,*) nomtype(nr)
             lentype(nr)=index(nomtype(nr),' ')-1
 105      continue
          read(20,*) bidon
          do 3111 nr=1,nref
             do 3112 nb=1,nbns
                do 3113 k=1,ncellz
                   read(20,*) bidon
	           read(20,*) ((numvol(nr,nb,i,j,k)
     +             ,j=1,ncelly),i=ncellx,1,-1)
 3113           continue
 3112        continue
 3111     continue

       close(unit=20)
c
c  conversion en unite de part/m^3
c
          do 121 nr=1,nref
          do 122 nb=1,nbns
          do 123 i=1,ncellx
          do 124 j=1,ncelly
          do 125 k=1,ncellz
             numvol(nr,nb,i,j,k)=numvol(nr,nb,i,j,k)*1.e+4
 125      continue
 124      continue
 123      continue
 122      continue
 121      continue

c
c       Determine the right relative humidity file nearest to the 
c       current time 
c
        call julian(heure,minute,seconde,jour,mois,annee,jday)




        dtmin=10000000.
        do 610 nws=1,nrhfi
         if (abs(jday-rhjj(nws)).lt.dtmin) then
          dtmin=abs(jday-rhjj(nws))
          rhtim=rhjj(nws)
          rhufile=nomrh(nws)
         endif
 610    continue
        dtminm=dtmin
        dtmin=10000000.
        do nws=1,nrhfi
         if ((abs(jday-rhjj(nws)).lt.dtmin).and.(rhjj(nws).ne.
     +   rhtim)) then
          dtmin=abs(jday-rhjj(nws))
          rhtim2=rhjj(nws)
          rhufile2=nomrh(nws)
         endif
        enddo



c ----------------
c
c   lecture des fichiers 3-d de la repartition de l humidite relative
c   et projection sur la grille des aerosols
c
 21    format(i2)
c
       open(unit=11,file=rhufile,status='old',err=7)    
          print*,'Reading relative humidity file: ',rhufile(1:18)
          read(11,*)
          read(11,*) 
          read(11,*) ncelxrh
          read(11,*) lcelxrh
          read(11,*) ncelyrh
          read(11,*) lcelyrh
          read(11,*) ncelzrh
          do k=1,ncelzrh
             read(11,*) lcelzrh(k)
          enddo
          read(11,*) xrh0, yrh0, zrh0
         if (yrh0.lt.0.) yrh0=yrh0+360.           
          read(11,*)
          do kk=1,ncelzrh
            read(11,*) 
            read(11,*) ((rh3d(ii,jj,kk),jj=1,ncelyrh),ii=ncelxrh,1,-1) 
          enddo  
          do i=1,ncellx
            do j=1,ncelly
                do k=1,ncellz
                   zgrid=lcellz(k)/2.
                   do nn=1,k-1
                      zgrid=zgrid+lcellz(nn)
                   enddo
                   ii=nint(1.+(xcell0-xrh0+lcellx*int(i-1))/lcelxrh)
                   jj=nint(1.+(ycell0-yrh0+lcelly*int(j-1))/lcelyrh)        
                   zrh=zrh0
                   do nn=1,ncelzrh
                      zrh_1=zrh
                      zrh=zrh+lcelzrh(nn)
                      if ((zrh.gt.zgrid).and.(zrh_1.le.zgrid)) then  
                        kk=nn
                      endif
                   enddo
                   t_drh(i,j,k)=rh3d(ii,jj,kk)
                enddo
             enddo
          enddo
       close(unit=11)
       if (nrhfi.gt.1) then
       open(unit=11,file=rhufile2,status='old',err=7)    
        print*,'Reading relative humidity file: ',rhufile2(1:18)
          read(11,*)
          read(11,*) 
          read(11,*) ncelxrh
          read(11,*) lcelxrh
          read(11,*) ncelyrh
          read(11,*) lcelyrh
          read(11,*) ncelzrh
          do k=1,ncelzrh
             read(11,*) lcelzrh(k)
          enddo
          read(11,*) xrh0, yrh0, zrh0
         if (yrh0.lt.0.) yrh0=yrh0+360.           
          read(11,*)
          do kk=1,ncelzrh
            read(11,*) 
            read(11,*) ((rh3d(ii,jj,kk),jj=1,ncelyrh),ii=ncelxrh,1,-1) 
          enddo  
          do i=1,ncellx
            do j=1,ncelly
                do k=1,ncellz
                   zgrid=lcellz(k)/2.
                   do nn=1,k-1
                      zgrid=zgrid+lcellz(nn)
                   enddo
                   ii=nint(1.+(xcell0-xrh0+lcellx*int(i-1))/lcelxrh)
                   jj=nint(1.+(ycell0-yrh0+lcelly*int(j-1))/lcelyrh)        
                   zrh=zrh0
                   do nn=1,ncelzrh
                      zrh_1=zrh
                      zrh=zrh+lcelzrh(nn)
                      if ((zrh.gt.zgrid).and.(zrh_1.le.zgrid)) then  
                        kk=nn
                      endif
                   enddo
                   t_drh2(i,j,k)=rh3d(ii,jj,kk)
                enddo
             enddo
          enddo
       close(unit=11)

c
c   interpoler la valeur de RH
c
         do ii=1,ncellx
            do jj=1,ncelly
                 do kk=1,ncellz
                       pente=(dble(t_drh2(ii,jj,kk)-
     +                t_drh(ii,jj,kk)))/(rhtim2-rhtim)
                       ordon=dble(t_drh2(ii,jj,kk))-pente
     +                *rhtim2
                       t_drh(ii,jj,kk)=idnint(pente*jday+ordon)

c             if (ii+jj+kk.eq.3) then
c              print*,'=============R.H=============='
c              print*,t_drh(1,1,1),t_drh2(1,1,1),t_drh(1,1,1)
c        print*,pente*time+ordon,pente,time,ordon
c              print*,'============ timeRH==========='
c              print*,rhtim,rhtim2,time
c             endif

                 enddo
             enddo
          enddo
        endif





c
c -----------
c
c   calcul du volume moyen sec d une particule du bin nb en m^3 
c   (1e-18 est 
c   le facteur de conversion de micron^3 a m^3
c
       print*,'Computing dry mean volume of particles for each bin...'
       do 699 nr=1,nref
       do 700 nb=1,nbns
          volume(nb)=4.*pi*1.e-18*((rayon1(nb)+rayon2(nb))/2.)**3./3.
 700   continue
 699   continue
c
c ------------------------------------------------------
c
c   sections efficaces
c
c ------
c
c   lecture du fichier de sections efficaces pre calculees
c   par bmies.f
c
             print*,'Reading extinction cross section database...'
             read(37,*) nrbase,nrhbase,nwbase,nbbase
             do 223 nr=1,nrbase
                read(37,*) nomtbase(nr)
                do 221 nrh=1,nrhbase
                   read(37,*) rhbase(nrh)
                   do 224 nw=1,nwbase
                      read(37,*) wlenbase(nw), mrbase(nr,nrh,nw),
     +                mibase(nr,nrh,nw) 
                      do 225 nb=1,nbbase
                         read(37,*) bidon,cebase(nr,nrh,nw,nb),
     +                   csbase(nr,nrh,nw,nb),cabase(nr,nrh,nw,nb)
 225                  continue
 224               continue
 221            continue
 223         continue 
          close(unit=37)
c
c   recherche des valeurs de sigmae dans la base de donnee
c
          do 226 nr=1,nref
             do 231 nrb=1,nrbase
             if (nomtype(nr)(1:5).eq.nomtbase(nrb)(1:5)) then
                do 232 nrh=1,13
                   do 233 nw=1,nwav
                      do 234 nb=1,nbns
                        sigmae(nr,nrh,nw,nb)=cebase(nrb,nrh,nw,nb)
 234                  continue
 233               continue
 232            continue
             endif
 231         continue
 226      continue
c
c ---------------
c
c   calcul de l epaisseur optique de chaque cellule 
c
c   les coefficients d extinctions sont calcules en unites de m^-1
c   le facteur 1.e-12 permet de convertir la section efficace en unites
c   de m^2.
c
c 
      print*,'Computing aerosol optical depth for each cell...'     
      do 1211 i=1,ncellx
         do 1311 j=1,ncelly
            do 1411 k=1,ncellz
c
c   Determiner la valeur de nrh en fonction de i,j,k
c
             if ((t_drh(i,j,k).ge.0).and.(t_drh(i,j,k).lt.10)) then
                nrh=1
             elseif ((t_drh(i,j,k).ge.10).and.(t_drh(i,j,k).lt.20)) then
                nrh=2
             elseif ((t_drh(i,j,k).ge.20).and.(t_drh(i,j,k).lt.30)) then
                nrh=3
             elseif ((t_drh(i,j,k).ge.30).and.(t_drh(i,j,k).lt.40)) then
                nrh=4
             elseif ((t_drh(i,j,k).ge.40).and.(t_drh(i,j,k).lt.50)) then
                nrh=5
             elseif ((t_drh(i,j,k).ge.50).and.(t_drh(i,j,k).lt.60)) then
                nrh=6
             elseif ((t_drh(i,j,k).ge.60).and.(t_drh(i,j,k).lt.70)) then
                nrh=7
             elseif ((t_drh(i,j,k).ge.70).and.(t_drh(i,j,k).lt.80)) then
                nrh=8
             elseif ((t_drh(i,j,k).ge.80).and.(t_drh(i,j,k).lt.90)) then
                nrh=9
             elseif ((t_drh(i,j,k).ge.90).and.(t_drh(i,j,k).lt.95)) then
                nrh=10
             elseif ((t_drh(i,j,k).ge.95).and.(t_drh(i,j,k).lt.98)) then
                nrh=11
             elseif ((t_drh(i,j,k).ge.98).and.(t_drh(i,j,k).lt.99)) then
                nrh=12
             elseif (t_drh(i,j,k).ge.99) then
                nrh=13
             endif
               do 1511 nw=1,nwav
                  kecell=0.
                  do 1611 nr=1,nref
                     do 1711 nb=1,nbns
                   ke=sigmae(nr,nrh,nw,nb)*numvol(nr,nb,i,j,k)*1.e-12
                        kecell=kecell+ke
 1711                continue
 1611             continue
                  taucell(nw,i,j,k)=kecell*lcellz(k)
 1511          continue
 1411       continue
 1311    continue
 1211 continue
c
c
c  ------------------------
c
c   Integrale des epaisseurs optiques
c
c
      do 501 ns=1,nslice 
      do 400 ia=1,14
      do 401 ib=1,100
      do 402 ic=1,100
         taumap(ia,ib,ic)=0.
 402  continue
 401  continue
 400  continue        
      if (ns.eq.1) numero='01'
      if (ns.eq.2) numero='02'
      if (ns.eq.3) numero='03'
      if (ns.eq.4) numero='04'
      if (ns.eq.5) numero='05'
      if (ns.eq.6) numero='06'
      if (ns.eq.7) numero='07'
      if (ns.eq.8) numero='08'
      if (ns.eq.9) numero='09'
      if (ns.eq.10) numero='10'
      if (ns.eq.11) numero='11'
      if (ns.eq.12) numero='12'
      if (ns.eq.13) numero='13'
      if (ns.eq.14) numero='14'
      if (ns.eq.15) numero='15'
      if (ns.eq.16) numero='16'
      if (ns.eq.17) numero='17'
      if (ns.eq.18) numero='18'
      if (ns.eq.19) numero='19'
      if (ns.eq.20) numero='20'
      if (ns.eq.21) numero='21'
      if (ns.eq.22) numero='22'
      if (ns.eq.23) numero='23'
      if (ns.eq.24) numero='24'
      if (ns.eq.25) numero='25'
      if (ns.eq.26) numero='26'
      if (ns.eq.27) numero='27'
      if (ns.eq.28) numero='28'
      if (ns.eq.29) numero='29'
      if (axis(ns).eq.3) then
         ncxi=1
         ncxf=ncellx
         ncyi=1
         ncyf=ncelly
         nczi=liminf(ns)
         nczf=limsup(ns)
      elseif (axis(ns).eq.2) then
         ncxi=1
         ncxf=ncellx
         ncyi=liminf(ns)
         ncyf=limsup(ns)
         nczi=1
         nczf=ncellz
      elseif (axis(ns).eq.1) then
         ncxi=liminf(ns)
         ncxf=limsup(ns)
         ncyi=1
         ncyf=ncelly
         nczi=1
         nczf=ncellz
      endif
c
c ------------------------
c
c   integrale de l epaisseur optique  (carte d aerosols)
c
         print*,'Computation of the aerosol optical depth map...'
         taumax=0.
         do 2111 nw=1,nwav
            do 1811 i=ncxi,ncxf
               do 1911 j=ncyi,ncyf
                  do 2011 k=nczi,nczf
                     if (axis(ns).eq.3) then
                        taumap(nw,i,j)=taumap(nw,i,j)+taucell(nw,i,j,k)
                        if (taumap(nw,i,j).gt.taumax) then
                           taumax=taumap(nw,i,j)
                           itaumax=int(100000.*taumax)
                        endif
                     elseif (axis(ns).eq.2) then 
                        taumap(nw,i,k)=taumap(nw,i,k)+taucell(nw,i,j,k)
                        if (taumap(nw,i,k).gt.taumax) then
                           taumax=taumap(nw,i,k)
                           itaumax=int(100000.*taumax)
                        endif
                     elseif (axis(ns).eq.1) then 
                        taumap(nw,j,k)=taumap(nw,j,k)+taucell(nw,i,j,k)
                        if (taumap(nw,j,k).gt.taumax) then
                           taumax=taumap(nw,j,k)
                           itaumax=int(100000.*taumax)
                        endif
                     endif
                     if (taumap(nw,i,j).gt.2.52) taumap(nw,i,j)=2.54
 2011             continue
 1911          continue
 1811       continue
 2111    continue
c
c ------------------
c
c   fabrication des images pgm (valeur = epaisseur optique * 100)
c

      print*,'Making optical depth pgm image...'
      do 3211 nw=1,nwav
         aotfile=nom(1:lennom)//waveflag(nw)
     +   (1:lenwflag(nw))//'_'//numero//'.pgm'
         open(unit=27,file=aotfile,status='unknown')
         write(27,171) 'P2'
         write(27,174) 
         write(27,173) axis(ns),liminf(ns),limsup(ns)
         write(27,180) '# date ',nint(heure),nint(minute),
     +   nint(seconde),nint(jour),nint(mois),nint(annee)
         if (axis(ns).eq.3) then
            write(27,179) '# pixsiz ',lcellx
            write(27,179) '# lat0   ',xcell0
            write(27,179) '# lon0   ',ycell0        
            write(27,172) ncelly, ncellx
            write(27,*) '255'
            do 3411 i=ncellx,1,-1
c                  if (taumap(nw,i,j).gt.2.55) taumap(nw,i,j)=2.55
                  write(27,*) (nint(taumap(nw,i,j)*100.),j=1,ncelly)
 3411       continue
         elseif (axis(ns).eq.2) then 
            write(27,172) ncellx, ncellz
            write(27,*) '255'
            do 3412 k=ncellz,1,-1
               write(27,*) (nint(taumap(nw,i,k)*100.),i=1,ncellx)           
 3412       continue
         elseif (axis(ns).eq.1) then 
            write(27,172) ncelly, ncellz
            write(27,*) '255'
            do 3413 k=ncellz,1,-1
               write(27,*) (nint(taumap(nw,j,k)*100.),j=1,ncelly)           
 3413       continue
         endif
 177     format(i3)
         close(unit=27)
 3211 continue
 171  format(a)
 172  format(i6,1x,i6)
 174  format('# AOD map from AODSEM (100= aod 1)')
 173  format('# integration axis=',I1,' from:',I2,' to:',I2)
 179  format(A,F8.3)
 180  format(A,I2,1x,I2,1x,I2,1x,I2,1x,I2,1x,I4)
 501  continue
      do 109 no=1,nobs
c
c ------------
c
c   determination de l angle de diffusion le long de la ligne de vise
c
      print*,'Computing the diffusion angle of the line of sight...'
       thetad=acos(cos(elepho(no))*cos(elesol)*cos(phisol-phipho(no))+
     + sin(elesol)*sin(elepho(no)))
c
c ---------------
c
c   echelle locale en metre pres de la position du photometre
c
      print*,'Computing local scales near photometer...'
      call echelle(xpho(no),lcellx,lcelly,dx,dy)
c
c ---------------
c
c   cellules traversees par la ligne de vise
c
      print*,'Cells in the line of sight...'
      hz=0.
      do 3001 k=1,ncellz
         vnz=hz/sin(elepho(no)*pi/180.)
         xnz=vnz*cos(elepho(no)*pi/180.)*cos(phipho(no)*pi/180.)
         ynz=vnz*cos(elepho(no)*pi/180.)*sin(phipho(no)*pi/180.)
         cellvx(k)=int((xnz+(xpho(no)-xcell0)*dx)/(lcellx*dx))+1
         cellvy(k)=int((ynz+(ypho(no)-ycell0)*dy)/(lcelly*dy))+1
         if (cellvx(k).gt.ncellx) then
            cellvx(k)=ncellx
         endif
         if (cellvx(k).lt.1) then
            cellvx(k)=1
         endif
         if (cellvy(k).gt.ncelly) then
            cellvy(k)=ncelly
         endif
         if (cellvy(k).lt.1) then
            cellvy(k)=1
         endif
         hz=hz+lcellz(k)
 3001 continue  
c
c ---------------
c
c   calcul de l'epaisseur optique de chaque couche traversee par la 
c   ligne de vise et ecriture des resultats dans le fichier .pho
c
      print*,'Determining the optical depth along the line of sight...'
      write(15,*) xpho(no),ypho(no),'  photometer position (lat,long)'
      write(15,*) zpho(no),'  photometer height (meter)'
      write(15,*) elepho(no),phipho(no),'  elevation, Azimut sight'
      z(1)=0.
      do 404 ia=1,12
        tauvis(ia)=0.
 404  continue     
      kmin=ncellz+1
      do 2004, k=2,ncellz+1
         z(k)=z(k-1)+lcellz(k-1)
         if ((zpho(no).ge.z(k-1)).and.(zpho(no).lt.z(k))) kmin=k
 2004 continue
      do 3003 nw=1,nwav
         tauvis(nw)=tauvis(nw)+taucell(nw,cellvx(kmin-1),cellvy(kmin-1)
     +   ,kmin-1)*(z(kmin)-zpho(no))/lcellz(kmin-1)
         
         do 3002 k=kmin,ncellz
            tauvis(nw)=tauvis(nw)+taucell(nw,cellvx(k),cellvy(k),k)
 3002    continue
c
c   correction pour la masse d'air (Kasten and Young 1989)
c
         airmass=1./(cos((90.-elepho(no))*pi/180.)+0.50572*(96.07995-
     +   (90.-elepho(no)))-1.6364)
         tauvis(nw)=tauvis(nw)*airmass
         write(15,*) 'tau(',waveflag(nw),'nm):',tauvis(nw)
 3003 continue
c ------------
c
c   calcul de l'intensite de diffusion de chaque couche traversee par la 
c   ligne de vise
c
 109  continue
       close(unit=15)
       stop
 7     print*,'Bad relative humidity file name: ',rhufile
       stop
 3     print*,'Bad root file name: ',nom
       stop
 6     print*,'Bad cross section data base file: ',crofile//'.res.bmi'
       stop
 8     print*,'Parameter file: 3dtoaod.par dont exist.'
       stop
 17    print*,'Can t find file relhum.index!'

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
c   dx = taille de la cellule dans la direction s-n
c   dy = taille de la cellule dans la direction w-e
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
