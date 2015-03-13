c    Programme permettant de produire une matrice 3-d de densite 
c    numerique des aerosols a partir d une carte d epaisseur optique
c    des aerosols a une longueur d onde determinee
c
c    Pour passer de l'epaisseur optique a la densite numerique, un 
c    un profil de McClatchey 1971 ou un profil exponentiel avec une 
c    echelle de hauteur de 2 km ou enfin preferablement un profil
c    inspire du Standard RAdiative Atmosphere (SRA) 
c    sera employe ainsi que les modeles
c    d aerosols standard de Shettle ant Fenn 1979.
c
c    les axes sont definis comme suit
c    x = direction nord = degres
c    y = direction est = degres
c    z = direction haut = metres
c
c    Martin Aube 2000
c
c ------
c
c   Description des variables
c
c     sigtot=valeur de sigma_e ponderee pour la psd et pour le melange
c            d aerosols.
c
c
c --------------
c
c    Declaration des variables
c
      integer nbins,nzlev,nxwid,nywid,ntyp 
      parameter (nbins=12,nzlev=10,nxwid=181,nywid=360,ntyp=5)
c  ntyp=nombre de type d aerosols
c  nbins=nombre d intervalles de taille
c  nzlev=nombre de niveaux verticaux
c  nxwid=nombre de cellules N-S
c  nywid=nombre de cellules E-O   
      real fraction(ntyp),fractro(ntyp),tau(nxwid,nywid),wavelen(14)
      real rayon1(nbins),rayon2(nbins),ttot
      real hcellz(nzlev),lcellz(nzlev),lcellx,lcelly,lcelxrh,lcelyrh
      real zgrid,psdtro(ntyp,nbins)
     + ,psdpbl(ntyp,nbins)
      real psdttot(ntyp),psdptot(ntyp),value
      real densite(ntyp,nbins,nxwid,nywid,nzlev),ns,L1,L2,H2,ehau,den
      real lcelzrh(nzlev),xrh0,yrh0,zrh0,wlenbase(14),mrbase(10,13,14)
      real mibase(10,13,14),sigmae(ntyp,13,14,nbins)
      real cebase(10,13,14,nbins)
      real n1(ntyp),n2(ntyp),tautro,taupbl,nstro,hpbl,htro,zrh,zrh_1
      real r1(ntyp),r2(ntyp),sg1(ntyp),sg2(ntyp)
      real r1rural(13),H6S,z1,z2
      real r2rural(13),r1urban(13),r2urban(13),r1marit(13),rhbase(13)
      real densi0(nzlev),latitu,longit,H1(1000),x,deltaz,z
      real*8 heure,minute,seconde,jour,mois,annee
      character*60 datfile,rhufile,obsfile
      character*60 crofile,nom,aotfile,tag
      character*15 nomtype(ntyp),waveflag(14),bidon
      character*45 nomtbase(ntyp)
      integer humrel,ntype,ncellx,ncelly,i,j,nbns,ncellz,ncelxrh
      integer ncelyrh
      integer ncelzrh,ii,jj,kk,nrbase,nrhbase,nwbase,nbbase,nr,nrh,nt
      integer nb,nwave,n_rh(nxwid,nywid,nzlev),n,urbmask,
     + seamask,nz,maxi,hcnt
      integer lontemp,vertpro,ia,rh3d(nxwid,nywid,nzlev)
      real*8 rhjj(1000),hre,min,sec,jou,moi,ann,jday,dtmin
      character*60 nomrh(1000)
      integer nrhfi,nrhf,nws
      character*60 rhufile2
      real*8 rhtim,rhtim2,ordon,pente,dtminm
      integer rh3d2(nxwid,nywid,nzlev)
c
c ---------
c
c   Initialisation des variables
c
       pi=3.141592654
       pi2=pi*pi

c   Number of wavelength
       nwav=14
c   Wavelength flags
       data waveflag/'340','380','440','470','500','550'
     +,'670','860','870','940','1020','1240','1640','2130'/
c   Wavelength values
       data wavelen/.34,.38,.44,.47,.5,.55,.67,.86,.87,.94,1.02,
     +1.24,1.64,2.13/
c   number of size bins
       nbns=12
c   limites des bins de taille
       data rayon1/0.005, 0.01,0.02,0.04,0.08,0.16,0.32,0.64,1.28,2.56,
     + 5.12,10.24/
       data rayon2/0.01,0.02,0.04,0.08,0.16,0.32,0.64,1.28,2.56,5.12,
     + 10.24,20.48/  
c   number of cells along z
       ncellz=nzlev
c 
c   geometrie verticale
c
c   cas 1: 10 niveaux
c       
c   altitude centrale de chaque couche verticale
       data hcellz/82.5,255.,443.5,651.,883.,
     + 1146.,1448.5,1805.5,7000.,21000./
c   largeur de chaque couche verticale
       data lcellz/165.,180.,197.,218.,246.,
     + 280.,325.,389.,10000.,18000./
c   nombre d aerosols
       ntype=5
c   types d aerosols
       data nomtype/'SULFATE','BLACKCARBON','SOILDUST','SEASALT'
     +,'OMCARBON'/
c   psd pour chaque modele selon Shettle & Fenn p 14 (table 2)
       data r1rural/0.027,0.027,0.027,0.027,0.027,0.02748,0.02748,
     + 0.02846,0.03274,0.03884,0.04238,0.04751,0.05215/
       data r2rural/0.43,0.43,0.43,0.43,0.43,0.4377,0.4377,
     + 0.4571,0.5477,0.6462,0.7078,0.9728,1.1755/
       data r1marit/0.16,0.16,0.16,0.16,0.16,0.1711,0.1711,
     + 0.2041,0.3180,0.3803,0.4606,0.6024,0.7505/
       data r1urban/0.025,0.025,0.025,0.025,0.025,0.02563,0.02563,
     + 0.02911,0.03514,0.04187,0.04904,0.05996,0.06847/
       data r2urban/0.4,0.4,0.4,0.4,0.4,0.4113,0.4113,
     + 0.4777,0.5805,0.7061,0.8634,1.1691,1.4858/
c   largeur des psd lognormales pour chaque type d aerosol
       data sg1/0.35,0.35,0.35,0.4,0.35/
       data sg2/0.4,0.4,0.4,1.,0.4/
c   poids de la dist log-norm
       data n1/0.999875,0.999875,0.999875,1.,1./
       data n2/0.000125,0.000125,0.000125,0.,0./
c   profil vertical McClatchey 1971
c   ns=55 cm-3 = 55x10^6 part/m^3 la valeur de 1. est d abord adoptee
c   elle sera ensuite calculee a partir de la valeur de sigma_e
c
               L1=5500.
               L2=18000.
               H2=3770.
c   echelle de hauteur du profil exponentiel tel que defini
c   par Vermote (6-S)
c
               H6S=2000.
c   aerosol type fraction for the tropospheric model (z>2km)  
c   the tropospheric correspond to the rural model except for
c   the size distribution (psdtro)
c
               fractro(1)=.7
               fractro(2)=0.
               fractro(3)=.3
               fractro(4)=0.
               fractro(5)=0.
c   constant tau tropospheric (550 nm) from wcp-55 (2-12 km) 
c   ttwcp=0.025
c
               ttwcp=0.025
c   planetary bondary layer height
c
               hpbl=2000.
c   tropospheric layer height
c
               htro=12000.

c
c      lecture du fichier de parametres
c
       open(unit=25,file='aodto3d.par',status='old',err=19)
        read(25,*) nom
        read(25,*) nwave
        read(25,*) heure,minute,seconde,jour,mois,annee
        read(25,*) vertpro
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
c   Donnees de base
c 
      lennom=index(nom,' ')-1
      aotfile=nom(1:lennom)//'.pgm'
         open(unit=27,file=aotfile,status='old',err=7)
      if ((nwave.lt.1).or.(nwave.gt.14)) goto 8
c
c ---------
c
c   choix de la racine du nom du fichier .res.bmi pour les valeurs 
c   de section efficaces d extinction
c
      lencro=index(crofile,' ')-1
      crofile=crofile(1:lencro)//'.res.bmi'
          open(unit=37,file=crofile,status='old',err=9)
c
c
c -----------
c
c   noms des fichiers d entree-sortie
c
      datfile=nom(1:lennom)//'.dat'
      obsfile=nom(1:lennom)//'.vis'
c
c   convertir ttwcp pour la longueur d onde d observation en supposant
c   alpha=1.3
c
         ttwcp=ttwcp*(0.55/wavelen(nwave))**1.3
         print*,'Value of the tropospheric (z>2km) WCP-55 AOD value: ',
     +ttwcp
c
c ----------
c
c   lecture de l image d epaisseur optique
c
      print*,'Loading optical depth pgm image...'
c
c   recherche de la position des headers
c
         bidon='#'
         hcnt=0
         read(27,*)
         do 54 i=1,10
            read(27,*,end=56,err=57) bidon,tag,value
 57         if (bidon.eq.'#') then
               hcnt=hcnt+1
               if (tag(1:6).eq.'pixsiz') lcellx=value
               if (tag(1:4).eq.'lat0') xcell0=value
               if (tag(1:4).eq.'lon0') ycell0=value 
               lcelly=lcellx
            endif
 54      continue            
 56      rewind 27
         read(27,*)
         do 55 i=1,hcnt
            read(27,*)
 55      continue
         if (xcell0*ycell0*lcellx.eq.0.) then
            print*,'Horizontal pixel size in deg. ?'
            read*, lcellx
            lcelly=lcellx
 5         print*,'Center latitude of the south-west pixel in degrees?'
               read*,xcell0
               if (xcell0.lt.-90.) then
                  print*,'Should be greater than -90.'
                  goto 5
               endif
           print*,'Center longitude of the south-west pixel in degrees?'
               read*,ycell0
         endif
         read(27,*) ncelly, ncellx,maxi
         read(27,*) ((tau(i,j),j=1,ncelly),i=ncellx,1,-1)
         do 3411 i=ncellx,1,-1
            do 3311 j=1,ncelly
               tau(i,j)=tau(i,j)/100.
               if (tau(i,j).ge.2.53) tau(i,j)=0.
 3311       continue
 3411    continue
         close(unit=27)
         if (ycell0.lt.0.) ycell0=ycell0+360.


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
c
c ----------
c
c   lecture des fichiers d humidite relative
c
      open(unit=11,file=rhufile,status='old',err=10)
          print*,'Reading relative humidity file: '
     +,rhufile(1:45)
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
       close(unit=11)
       if (nrhfi.gt.1) then
       open(unit=11,file=rhufile2,status='old',err=11)
         print*,'reading relative humidity file: ',rhufile2(1:45)
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
            read(11,*) ((rh3d2(ii,jj,kk),jj=1,ncelyrh),ii=ncelxrh,1,-1)
          enddo 
       
       close(unit=11)
c
c   interpoler la valeur de RH
c
         do ii=1,ncelxrh
            do jj=1,ncelyrh
                 do kk=1,ncelzrh
                       pente=(dble(rh3d2(ii,jj,kk)-
     +                rh3d(ii,jj,kk)))/(rhtim2-rhtim)
                       ordon=dble(rh3d2(ii,jj,kk))-pente
     +                *rhtim2
                       rh3d(ii,jj,kk)=idnint(pente*jday+ordon)
                 enddo
             enddo
          enddo
       endif    
c
c   Projection sur la grille aodsem
c   et determination des classes de RH
c  
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
c
c   Determiner la valeur de nrh en fonction de i,j,k
c
        if ((rh3d(ii,jj,kk).ge.0).and.(rh3d(ii,jj,kk).lt.10)) then
                n_rh(i,j,k)=1
        elseif ((rh3d(ii,jj,kk).ge.10).and.(rh3d(ii,jj,kk).lt.20)) then
                n_rh(i,j,k)=2
        elseif ((rh3d(ii,jj,kk).ge.20).and.(rh3d(ii,jj,kk).lt.30)) then
                n_rh(i,j,k)=3
        elseif ((rh3d(ii,jj,kk).ge.30).and.(rh3d(ii,jj,kk).lt.40)) then
                n_rh(i,j,k)=4
        elseif ((rh3d(ii,jj,kk).ge.40).and.(rh3d(ii,jj,kk).lt.50)) then
                n_rh(i,j,k)=5
        elseif ((rh3d(ii,jj,kk).ge.50).and.(rh3d(ii,jj,kk).lt.60)) then
                n_rh(i,j,k)=6
        elseif ((rh3d(ii,jj,kk).ge.60).and.(rh3d(ii,jj,kk).lt.70)) then
                n_rh(i,j,k)=7
        elseif ((rh3d(ii,jj,kk).ge.70).and.(rh3d(ii,jj,kk).lt.80)) then
                n_rh(i,j,k)=8
        elseif ((rh3d(ii,jj,kk).ge.80).and.(rh3d(ii,jj,kk).lt.90)) then
                n_rh(i,j,k)=9
        elseif ((rh3d(ii,jj,kk).ge.90).and.(rh3d(ii,jj,kk).lt.95)) then
                n_rh(i,j,k)=10
        elseif ((rh3d(ii,jj,kk).ge.95).and.(rh3d(ii,jj,kk).lt.98)) then
                n_rh(i,j,k)=11
        elseif ((rh3d(ii,jj,kk).ge.98).and.(rh3d(ii,jj,kk).lt.99)) then
                n_rh(i,j,k)=12
        elseif (rh3d(ii,jj,kk).ge.99) then
                n_rh(i,j,k)=13
        endif
                enddo
             enddo
          enddo
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
                         read(37,*) bidon,cebase(nr,nrh,nw,nb)
 225                  continue
 224               continue
 221            continue
 223         continue 
          close(unit=37)
c
c   recherche des valeurs de sigmae dans la base de donnee
c
          do 226 nr=1,ntype
             do 231 nrb=1,nrbase
                if (nomtype(nr)(1:7).eq.nomtbase(nrb)(1:7)) then
                  do 232 nrh=1,13
                    do 233 nw=1,nwav
                      do 234 nb=1,nbns
                        sigmae(nr,nrh,nw,nb)=cebase(nrb,nrh,nw,nb)
 234                  continue
 233                continue
 232              continue
                endif
 231         continue
 226      continue
          if (vertpro.eq.3) then
c
c ----------
c
c   Lecture du fichier permettant de passer de tau infini a H1 pour
c   la determination du profil vertical.  Ce tableau a ete cree en 
c   utilisant les fit des donnees de McClatchey et les deux variables
c   sont parametrees en fonction de la visibilite.
c
          open(unit=14,file='tau-H1.txt',status='old')
             read(14,*) 
             do 78 n=1,1000
                read(14,*) bidon,H1(n)
 78          continue
          close(unit=14)
          endif
c
c ----------
c
c   creation de la matrice 3-d des dentites numeriques en part./m^3
c   pour chaque type de particules (SULFATE,SOILDUST,BLACKCARBON,
c   SEASALT) a l aide des modeles d aerosols de Shettle & Fenn 1979
c   et du profil en altitude de McClatchey 1971
c
c
      print*,'Computing the aerosol 3-d distribution...'
      open(unit=28,file='urbanmask.txt',status='old',
     +access='direct',recl=2,form='formatted')
      open(unit=29,file='land-sea.txt',status='old',
     +access='direct',recl=2,form='formatted')
       do 306 nx=1,ncellx
          do 308 ny=1,ncelly
             sigtot=0.
             latitu=(real(nx)-1.)*lcellx+xcell0
             if (latitu.gt.90.) then
                print*,'Latitude is to high!'
                stop
             endif
             longit=(real(ny)-1.)*lcelly+ycell0
             if (longit.lt.0.) longit=longit+360.
             if (longit.gt.360.) longit=longit-360.
c
c   determiner le type de zone (oceanique, urbain, rural)
c   a partir des fichiers urbanmask.txt (3=ville,2=33%ville 66%rural,
c   1=11%ville 89%rural) et land-sea.txt
c   (0=mer,1=33% terre, 2=66% terre, 3=terre) qui possedent
c   une resolution de 1x1 degres
c
            lontemp=int(longit+180.)
            if (lontemp.ge.360) then
               lontemp=lontemp-360
            endif
            nbdat=int(-latitu+90.)*360+lontemp+1
            read(28,2415,rec=nbdat) urbmask          
            read(29,2415,rec=nbdat) seamask
               if (urbmask.eq.0) then
c
c   rural case from Shettle and Fenn 1979
c   fraction=Ni/N
c
                  fraction(1)=.7
                  fraction(2)=0.
                  fraction(3)=.3
                  fraction(4)=0.
                  fraction(5)=0.
                  r1(1)=r1rural(1)
                  r1(2)=r1(1)
                  r1(3)=r1(1)
                  r1(4)=r1(1)
                  r1(5)=r1(1)
                  r2(1)=r2rural(1)
                  r2(2)=r2(1)
                  r2(3)=r2(1)
                  r2(4)=r2(1)
                  r2(5)=r2(1)
                  do 3001 ia=1,ntyp
                    psdttot(ia)=0.
                    psdptot(ia)=0.
 3001             continue  
               elseif (urbmask.eq.3) then
c
c   urban case from Shettle and Fenn 1979
c
                  fraction(1)=.56
                  fraction(2)=0.2
                  fraction(3)=.24
                  fraction(4)=0.
                  fraction(5)=0.
                  r1(1)=r1urban(1)
                  r1(2)=r1(1)
                  r1(3)=r1(1)
                  r1(4)=r1(1)
                  r1(5)=r1(1)
                  r2(1)=r2urban(1)
                  r2(2)=r2(1)
                  r2(3)=r2(1)
                  r2(4)=r2(1)
                  r2(5)=r2(1)
                  do 3002 ia=1,ntyp
                    psdttot(ia)=0.
                    psdptot(ia)=0.
 3002             continue                   
               elseif (urbmask.eq.2) then
c
c   mixed urban-rural case 66% urban 34% rural
c
                  fraction(1)=.60
                  fraction(2)=.12
                  fraction(3)=.28
                  fraction(4)=0.
                  fraction(5)=0.
                  r1(1)=0.66*r1urban(1)+0.34*r1rural(1)
                  r1(2)=r1(1)
                  r1(3)=r1(1)
                  r1(4)=r1(1)
                  r1(5)=r1(1)
                  r2(1)=0.66*r2urban(1)+0.34*r2rural(1)
                  r2(2)=r2(1)
                  r2(3)=r2(1)
                  r2(4)=r2(1)
                  r2(5)=r2(1)
                  do 3003 ia=1,ntyp
                    psdttot(ia)=0.
                    psdptot(ia)=0.
 3003             continue  
c                  taupbl=0.2
c                  tautro=0.025

               elseif (urbmask.eq.1) then
c
c   mixed urban-rural case 22% urban 78% rural
c
                  fraction(1)=.67
                  fraction(2)=0.04
                  fraction(3)=.29
                  fraction(4)=0.
                  fraction(5)=0.
                  r1(1)=0.22*r1urban(1)+0.78*r1rural(1)
                  r1(2)=r1(1)
                  r1(3)=r1(1)
                  r1(4)=r1(1)
                  r1(5)=r1(1)
                  r2(1)=0.22*r2urban(1)+0.78*r2rural(1)
                  r2(2)=r2(1)
                  r2(3)=r2(1)
                  r2(4)=r2(1)
                  r2(5)=r2(1)
                  do 3004 ia=1,ntyp
                    psdttot(ia)=0.
                    psdptot(ia)=0.
 3004             continue 
               endif
            if (seamask.eq.0) then
c
c  mer seulement
c
               fraction(1)=0.
               fraction(2)=0.
               fraction(3)=0.
               fraction(4)=1.
               fraction(5)=0.
               r1(4)=r1marit(1)
                  do 3005 ia=1,ntyp
                    psdttot(ia)=0.
                    psdptot(ia)=0.
 3005             continue
c
c   mixed land-sea
c
            elseif (seamask.eq.1) then
               fraction(1)=fraction(1)*0.33
               fraction(2)=fraction(2)*0.33
               fraction(3)=fraction(3)*0.33
               fraction(4)=1.-fraction(1)-fraction(2)-fraction(3)
               fraction(5)=fraction(5)
               r1(4)=r1marit(1)
               do 3006 ia=1,ntyp
                 psdttot(ia)=0.
                 psdptot(ia)=0.
 3006          continue
            elseif (seamask.eq.2) then
               fraction(1)=fraction(1)*0.67
               fraction(2)=fraction(2)*0.67
               fraction(3)=fraction(3)*0.67
               fraction(4)=1.-fraction(1)-fraction(2)-fraction(3)
               fraction(5)=fraction(5)
               r1(4)=r1marit(1)
               do 3007 ia=1,ntyp
                 psdttot(ia)=0.
                 psdptot(ia)=0.
 3007          continue
            endif
c
c --------
c 
c   ponderer pour la distribution de taille lognormale bi-modale
c   Shettle et Fenn 1979 equation (1) p.13
c
c   psdpbl=psd in the bondary layer (z<2km)
c   psdtro=psd in the tropospheric model (z>2km) rural model
c          only one fine mode
c
            do 1977 nt=1,ntype
            do 1978 n=1,nbns
               ri=(rayon1(n)+rayon2(n))/2.
               psdpbl(nt,n)=n1(nt)*(1./(ri*sg1(nt)))*exp((-(log10(ri)-
     +         log10(r1(nt)))**2.)/(2.*sg1(nt)**2.))+n2(nt)*(1./
     +         (ri*sg2(nt)))*exp((-(log10(ri)-log10(r2(nt)))
     +         **2.)/(2.*sg2(nt)**2.))
               psdtro(nt,n)=(1./(ri*sg1(nt)))*exp((-(log10(ri)-
     +         log10(r1rural(nt)))**2.)/(2.*sg1(nt)**2.))
c
c   multiplier par la largeur du bin
c  
               psdpbl(nt,n)=psdpbl(nt,n)*(rayon2(n)-rayon1(n))
               psdtro(nt,n)=psdtro(nt,n)*(rayon2(n)-rayon1(n))        
               psdptot(nt)=psdpbl(nt,n)+psdptot(nt) 
               psdttot(nt)=psdtro(nt,n)+psdttot(nt)
 1978       continue
c
c  normaliser la psd
c
            do 1979 n=1,nbns
               psdpbl(nt,n)=psdpbl(nt,n)/psdptot(nt)
               psdtro(nt,n)=psdtro(nt,n)/psdttot(nt)
 1979       continue
 1977       continue
c
c -----
c  
c   profil vertical 
c
            do 317 nb=1,nbns
            do 327 nt=1,ntype
            if (vertpro.eq.3) then
c
c -----
c  
c   profil vertical selon McClatchey 
c
c   valeur de l echelle de hauteur ehau=H1
c
               if ((1000.*tau(nx,ny)*(0.55/wavelen(nwave))**1.3)
     +         .gt.1019.) then
                  if ((tau(nx,ny)*(0.55/wavelen(nwave))**1.3).lt.1.19)
     + then
                     ehau=0.94
                  else 
                     if ((tau(nx,ny)*(0.55/wavelen(nwave))**1.3).lt.
     + 1.45) then
                        ehau=0.93
                     else 
                        if ((tau(nx,ny)*(0.55/wavelen(nwave))**1.3).lt.
     + 1.86) then
                           ehau=0.92
                        else
                           if ((tau(nx,ny)*(0.55/wavelen(nwave))**1.3)
     + .lt.2.6) then
                              ehau=0.91
                           else
                              ehau=0.9
                           endif
                        endif
                     endif
                  endif
               elseif ((tau(nx,ny)*(0.55/wavelen(nwave))**1.3)
     +         .le.0.196) then
                  ehau=H1(196)
               else
                  ehau=H1(int(1000.*tau(nx,ny)*(0.55/wavelen(nwave)
     +            )**1.3))
               endif 
            endif     
                  do 311 nz=1,ncellz
            if (vertpro.eq.3) then
c
c   subdivision de la couche en 100 sous couches afin de calculer la
c   valeur moyenne de la densite sur la couche
c
                     z=hcellz(nz)-lcellz(nz)/2.
                     deltaz=lcellz(nz)/100.
                     do 333 n=1,10
                        z=z+deltaz
c    profil de McClatchey
                        if (z.lt.L1) then
                           densite(nt,nb,nx,ny,nz)=exp((L1-z)/
     +                     (ehau*1000.))+densite(nt,nb,nx,ny,nz)
                        else
                           if (z.lt.L2) then
                              densite(nt,nb,nx,ny,nz)=1.+
     +                        densite(nt,nb,nx,ny,nz)
                           else
                              densite(nt,nb,nx,ny,nz)=exp((L2-z)/H2)+
     +                        densite(nt,nb,nx,ny,nz)
                           endif
                        endif
 333                 continue
c
c   moyenne de densite sur la couche
c
                     densite(nt,nb,nx,ny,nz)=densite(nt,nb,nx,ny,nz)/
     +               100.
            elseif (vertpro.eq.1) then
c
c    profil exponentiel + tropospherique SRA
c
                     z1=hcellz(nz)-lcellz(nz)/2.
                     z2=hcellz(nz)+lcellz(nz)/2.
                     if (z2.le.hpbl) then
                        densite(nt,nb,nx,ny,nz)=H6S*(exp(-z1/H6S)-
     +                  exp(-z2/H6S))/(z2-z1)
                     elseif ((z2.gt.hpbl).and.(z2.le.htro)) then
c   densite constante sur les niveaux tropospheriques
                        densite(nt,nb,nx,ny,nz)=1.
                     else
                        densite(nt,nb,nx,ny,nz)=0.
                     endif
            elseif (vertpro.eq.2) then
c
c   Profil exponentiel pur
c
                     z1=hcellz(nz)-lcellz(nz)/2.
                     z2=hcellz(nz)+lcellz(nz)/2.
                     densite(nt,nb,nx,ny,nz)=H6S*(exp(-z1/H6S)-
     +               exp(-z2/H6S))/(z2-z1)
            endif
 311              continue
 327        continue
 317        continue
            if (vertpro.eq.1) then 
              do nb=1,nbns
               do nt=1,ntype
                do nz=1,ncellz
                   z2=hcellz(nz)+lcellz(nz)/2.
c
c   ponderer la densite pour le modele d aerosol (composition + psd)
c
                     den=densite(nt,nb,nx,ny,nz)
                     if (z2.le.hpbl) then
c   planetary bondary layer
                        densite(nt,nb,nx,ny,nz)=psdpbl(nt,nb)*
     +                  fraction(nt)*den
                     else
c   tropospheric model
                        densite(nt,nb,nx,ny,nz)=psdtro(nt,nb)*
     +                  fractro(nt)*den
                     endif 
                enddo
               enddo
              enddo    
c
c   normalisation du profil tropospherique z>2km selon la valeur 
c   du wcp-55 tau(550nm)=0.025 corrige pour la longueur d onde 
c   d observation
c
          tautro=0.
          do 1312 nt=1,ntype
             do 1314 nb=1,nbns
                do 1315 nz=1,ncellz
                   z2=hcellz(nz)+lcellz(nz)/2.
                   if (z2.gt.hpbl) then
c   tropospheric
                      tautro=densite(nt,nb,nx,ny,nz)*lcellz(nz)*1.E-12*
     +                sigmae(nt,n_rh(nx,ny,nz),nwave,nb)+tautro
                   endif
 1315            continue
 1314         continue
 1312      continue
           if (tau(nx,ny).ge.ttwcp) then
              nstro=ttwcp/tautro
           else
c    mettre tous les aerosols dans la troposphere           
              nstro=tau(nx,ny)/tautro      
           endif
c
c   calcul de ns pour la pbl
c
c     integrale de l epaisseur optique sur nb,nt,nz 
c     de la fonction de densite non normalise
c
          taupbl=0.
          do 312 nt=1,ntype
             do 314 nb=1,nbns
                do 315 nz=1,ncellz
                   z2=hcellz(nz)+lcellz(nz)/2.
                   if (z2.le.hpbl) then
c   planetary bondary layer
                      taupbl=densite(nt,nb,nx,ny,nz)*lcellz(nz)*1.E-12*
     +                sigmae(nt,n_rh(nx,ny,nz),nwave,nb)+taupbl
                   endif
 315            continue
 314         continue
 312      continue
          ns=(tau(nx,ny)-tautro*nstro)/taupbl
          if (ns.lt.0.) ns=0.
c
c   ponderer le profil pour les valeurs de ns et de nstro
c
          do 322 nt=1,ntype
             do 324 nb=1,nbns
                do 325 nz=1,ncellz
                   z2=hcellz(nz)+lcellz(nz)/2.
                   den=densite(nt,nb,nx,ny,nz)
                   if (z2.le.hpbl) then
                      densite(nt,nb,nx,ny,nz)=ns*den
                   else 
                      densite(nt,nb,nx,ny,nz)=nstro*den
                   endif
 325            continue
 324         continue
 322      continue
            else
             do nb=1,nbns
               do nt=1,ntype
                do nz=1,ncellz
c
c   ponderer la densite pour le modele d aerosol (composition + psd)
c
                     den=densite(nt,nb,nx,ny,nz)
                        densite(nt,nb,nx,ny,nz)=psdpbl(nt,nb)*
     +                  fraction(nt)*den
                enddo
               enddo
              enddo   
c     integrale de l epaisseur optique sur nb,nt,nz 
c     de la fonction de densite non normalise
c
          taupbl=0.
          do nt=1,ntype
             do nb=1,nbns
                do nz=1,ncellz
                      taupbl=densite(nt,nb,nx,ny,nz)*lcellz(nz)*1.E-12*
     +                sigmae(nt,n_rh(nx,ny,nz),nwave,nb)+taupbl
                enddo
             enddo
          enddo
          ns=(tau(nx,ny))/taupbl
          if (ns.lt.0.) ns=0.
c
c   ponderer le profil pour les valeurs de ns
c
          do nt=1,ntype
             do nb=1,nbns
                do nz=1,ncellz
                   den=densite(nt,nb,nx,ny,nz)
                   densite(nt,nb,nx,ny,nz)=ns*den
                enddo
             enddo
          enddo      
            endif
 
 
 
 
 308      continue
 306   continue
       close(unit=28)
       close(unit=29)
 2415    format(i1)
c
c ----------------------
c
c   sorties
c
c -----------
c
c   Ecriture du fichier d observation 
c
       open(unit=7,file=obsfile,status='unknown')
          print*,'Writing observation file: ',obsfile(1:18)
          write(7,*) 'Observation definition file for AODSEM'
          write(7,*) '1    Number of sights'
             write(7,*) '90.    ANGLE ELEVATION PHOTOMETRE'
             write(7,*) '0.    ANGLE AZIMUT PHOTOMETRE'
             write(7,*) xcell0,'  X PHOTOMETRE degres'
             write(7,*) ycell0,'  Y PHOTOMETRE degres'
             write(7,*) zcell0,'  Z PHOTOMETRE'
          write(7,*) '1    Number of slices'
             write(7,*) '3    Integration Axis(1=lat,2=lon,3=height)'
             write(7,*) '1    from'
             write(7,*) '10    to'
          write(7,*) '45.    ANGLE ELEVATION SOLEIL'
          write(7,*) '0.    ANGLE AZIMUT SOLEIL'
       close(unit=7)
c
c ----------
c
c   Ecriture des donnees de distribution des aerosols 
c   
c   les donnees sont en unites de 1.e+4 part/m^3
c
      open(unit=1,file=datfile,status='unknown')
          print*,'Writing .dat file...'
          write(1,*) '3-d aerosol distribution file for AODSEM from aod
     +to3d.f'
          write(1,*) 'Units of 1.e+4 part/m^3'
          write(1,*) int(heure),int(minute),int(seconde),int(jour),
     +    int(mois),int(annee),' TU (hh mm ss), Date (jj mm yyyy)'
          write(1,*) nbns,'  Number of size bins'
          do 313 nb=1,nbns
             write(1,*) nb,rayon1(nb),rayon2(nb)
 313      continue
          write(1,*) ncellx,'  Number of X cells'
          write(1,*) lcellx,'  Size of X cell'
          write(1,*) ncelly,'  Number of Y cells'
          write(1,*) lcelly,'  Size of Y cell'
          write(1,*) ncellz,'  Number of Z cells'
          do 100 k=1,ncellz
             write(1,*) lcellz(k)
 100      continue
          write(1,*) xcell0, ycell0, zcell0,'  X0, Y0, Z0'
          write(1,*) ntype,'  Number of aerosol types'
          do 105 nr=1,ntype
             write(1,*) nomtype(nr)
 105      continue
          write(1,*) 'DATA'
          do 2111 nt=1,ntype
             do 2112 nb=1,nbns
                do 2113 k=1,ncellz
                   write(1,2325) nomtype(nt),nb,k
	           write(1,*) ((nint(densite(nt,nb,i,j,k)/1.e+4)
     +             ,j=1,ncelly),i=ncellx,1,-1)
 2113           continue
 2112        continue
 2111     continue
       close(unit=1)
 2323  format(E8.3)
 2324  format(A2)
 2325  format('# AEROSOL TYPE: ',A9,' SIZE BIN: ',I2,
     +' VERTICAL LEVEL: ',I2)
           stop
 10        print*,'Can t find first',rhufile(1:lennom+12),' relative 
     +humidity 3-d distribution.'  
           stop
 11        print*,'Can t find second',rhufile2(1:lennom+12),' relative 
     +humidity 3-d distribution.'  
           stop
 19        print*,'Parameter file: aodto3d.par don t exist.'
           stop
 7         print*,'Bad root name!'
           stop
 8         print*,'Bad wavelength.'
           stop
 9         print*,'Bad cross section file.'
           stop
 17        print*,'Can t find file relhum.index!'
           end
