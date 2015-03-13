c  Module de transport des aerosol AODSEM
c   
c  Les processus suivants sont pris en compte:
c
c      - force gravitationnelle
c      - force d entrainement par un champ de vents 
c           - horizontaux   
c           - verticaux
c      - force de poussee d Archimede
c
c  options:
c 
c  commentaires:  les valeurs de 80,160 pour la taille horizontale
c  des matrices entrainent l usage d environ 214 Mb de RAM!
c
c  copyright Martin Aube 2003
c
c ---------------------------------------------------------------------
c
      program dynamic

c
c     Declaration des variables
c
      integer nbins,nzlev,nxwid,nywid,ntyp,tampon,boxw 
      real gain,accept,scavratio
cc
cc
      parameter (nbins=12,nzlev=10,nxwid=80,nywid=160,ntyp=5)
cc
cc
      parameter (accept=0.002,scavratio=1.e2)
c  scarat=mean incloud scavenging ratio from Walton et al. 1998    
c  ntyp=nombre de type d aerosols
c  nbins=nombre d intervalles de taille
c  nzlev=nombre de niveaux verticaux
c  nxwid=nombre de cellules N-S
c  nywid=nombre de cellules E-O 
c  gain=unites de densite numeriques
c  accept=fraction du rafinement de la phase d assimilation sur la
c         densite num lorsque le programme
c         atteind cette valeur, l assimilation est stoppee
c  tampon=largeur de la zone tampon en pixels
c  boxw=taille de la matrice de lissage des sources
c  tt_3d est la matrice de temperature sur la grille 3d du modele
 
      real volume(nbins),rayon1(nbins),loncel,latcel,diar,diarmem,
     +     rayon2(nbins),masse,b,hcellz(nzlev),zgrid,dx,dy,
     +     lcellz(nzlev),scarat,numvolp(ntyp,nbins,nxwid,nywid,nzlev),
     +     longit,latitu,pi,precip(181,360),value,corre,
     +     numvol(ntyp,nbins,nxwid,nywid,nzlev),lcellx,lcelly,
     +     vxp,vyp,vzp,depot(ntyp,nbins,nxwid,nywid),
     +     vlz,vlimz,ratx(3),raty(3),ratz(3),wetrem(ntyp,
     +     nbins,nxwid,nywid),
     +     ncelxrh,lcelxrh,ncelyrh,lcelyrh,ncelzrh,xrh0,yrh0,zrh0,
     +     vxwind(181,360,nzlev),vywind(181,360,nzlev),
     +     vzwind(181,360,nzlev),
     +     vxwindp(181,360,nzlev),vywindp(181,360,nzlev),
     +     vzwindp(181,360,nzlev),xp,yp,zp
     +     vx0,vy0,vz0,x,y,z,x0,y0,z0,g,diamet(nbins),vlx,vly,
     +     radrat(6,13,nbins),finden(6,13,nbins),num2tot,
     +     rho0,hgaz,cc,rhoair,L,Lstp,viscos,source(ntyp,nbins,
     +     nxwid,nywid,nzlev),uncloud,
     +     ucloudh(12),ucloudd(12),rval,
     +     numtemp(ntyp,nbins,nxwid,nywid,nzlev),
     +     vxwind2(181,360,nzlev),vywind2(181,360,nzlev),
     +     vzwind2(181,360,nzlev),
     +     vxwind3(181,360,nzlev),vywind3(181,360,nzlev),
     +     vzwind3(181,360,nzlev),
     +     precip2(181,360),source2(nxwid,nywid),
     +     timecor,pcor,cloudfract(nxwid,nywid,nzlev)

c
      real*8 jday,rhjj(1000),wsjj(1000),jday1,jday2,time,deltat,
     +       wejj(1000),
     +       hre,min,sec,jou,moi,ann,dtmin,srcjj(1000),timestep,
     +       heure,minute,seconde,jour,mois,annee,vref,corremem,
     +       hret,mint,sect,jout,moit,annt,
     +       rhtim,rhtim2,ordon,pente,dtminm,wstim,wstim2,pentep,
     +       ordonp
     +       wetim,wetim2,srctim,srctim2,wstim3
c
      integer lenom1,nwspfi,nrhfi,nrhf,ncellx,ncelly,ncellz,
     +        ntype,nbns,nx,ny,nz,nt,nb,nh,nhum,nbnhum,lenbflag(nbins),
     +        i,j,k,ii,jj,kk,tint,t,nrh,newnx,newny,newnz,nn,nloop,nl,
     +        t_drh(181,360,nzlev),ncroi,nc,ncr(6),
     +        relhum,nwetfi,nwe,fixsour,pluie,mask2(nxwid,nywid),
     +        lentype(6),ni,nj,nk,botc,topc,ia,ib,ic,id,ie,skip,
     +        nsr,nsource,nsrcz,ival,RH0,coswi,
     +        t_drh2(181,360,nzlev),seamask,
     +        cloudmsk(nxwid,nywid),lontemp,nbdat,ntt
c
      character*60 nom1,nomrh(1000),nomws(1000),rhfile,wsfile,
     +             nomwet(1000),wetfile,srcfile,
     +             bidon,nomsrc(1000),
     +             rhfile2,wsfile2,wetfile2,srcfile2,wsfile3
c
      character*10 nomtype(6),croiname(8)
c
      character*20 humname(6),binflag(nbins)
      integer nttfi,nb1,nb2,bido1,ntmp,nbuffer,nbu
      real tt_3d(181,360,nzlev),tt2_3d(181,360,nzlev)
      real masse1,masse2,tempera,diam1,diam2,k12,dn_dt(12),numv(12)
      real kcoag(nzlev,29,13,ntyp,nbins,nbins)
      character*60 nomtt(1000),ttfile,ttfile2,nombuf(1000),buffile
      real*8 ttjj(1000),tttim,tttim2,bufjj(1000),buftime

         real Tlu,metgrid,Khor,sigmau,dmean
         integer difsiz,nx1,ny1,pufsiz
c
c  <<<<<<<<<<<
c
c  Initialisation des variables
c  units of num vol for archiving
c
      gain=1.e+4
c
c  tint=number of time intervals between two distributions
c
      tint=1
c
c  dynamical speed correction factor (for small timesteps,
c  the model seem to evolve slower than reality probably due
c  to the spatial discretisation.  This parameter will be used to
c  correct all speed used in the model
c
      timecor=1.0  
c
c  gravitationnal acceleration
c
      g=9.81
c
c  pi
c
      pi=3.1415926
c
c
c   activer la coagulation => coswi =1
c   desactiver coswi=0
c
      coswi=1
      rhfile='bidon'
      rhtim=0.
      fixsour=0
      pluie=1
      diarmem=200.  
      corremem=0.
c
c  wet under cloud scavenging coeff. for heavy rain and drizzle  
c  (from Garcia Nieto et al.1994)  
c  h for heavy rain 
c     d for drizzle 
c   
      data ucloudh/3.e-5,1.7e-5,9.e-6,4.e-6,2.e-6,1.e-6,8.e-7,1.e-6,
     +3.e-6,1.e-3,5.e-3,6.e-3/  
      data ucloudd/3.e-6,1.7e-6,9.e-7,4.e-7,2.e-7,1.e-7,8.e-8,1.e-7,
     +3.e-7,9.e-5,2.e-4,3.e-4/   
      scarat=scavratio
c
c  >>>>>>>>>>>>
c
c  <<<<<<<<<<<<
c
c  reading parameter file
c
      open(unit=25,file='dynamic.par',status='old',err=21)
        read(25,*) nom1
        read(25,*) heure,minute,seconde,jour,mois,annee
c  time step in minute
        read(25,*) timestep
        read(25,*) fixsour
        read(25,*) pluie
        read(25,*) tampon
      close(unit=25)
c  convert timestep in hour
      timestep=timestep/60.
c
      call julian(heure,minute,seconde,jour,mois,annee,jday) 
      jday2=jday    
c
c     autre initialisations
c
      do 40 ia=1,ntyp
        do 41 ib=1,nbins
          do 42 ic=1,nxwid 
            do 43 id=1,nywid
              depot(ia,ib,ic,id)=0.
              wetrem(ia,ib,ic,id)=0.
              do 44 ie=1,nzlev
                numtemp(ia,ib,ic,id,ie)=0.
                numvol(ia,ib,ic,id,ie)=0.
                numvolp(ia,ib,ic,id,ie)=0.
                source(ia,ib,ic,id,ie)=0.
 44           continue
 43         continue 
 42       continue
 41     continue
 40   continue  
      do ic=1,nxwid
        do id=1,nywid
           source2(ic,id)=0.  
        enddo
      enddo 
c
c  Input
c
      lenom1=index(nom1,' ')-1 
      nom1=nom1(1:lenom1)//'.dat'
c 
c <<<<<<<<<<
c
c  loading data file
c  
c  Lecture des donnees de distribution des aerosols 
c  les donnees sont en unites de (gain) part/m^3
c     
      open(unit=20,file=nom1,status='old',err=4)
        print*,'Reading data file: ',nom1(1:lenom1+4)
        read(20,*) bidon
        read(20,*) bidon
        read(20,*) hre,min,sec,jou,moi,ann
        call julian(hre,min,sec,jou,moi,ann,jday1)
        read(20,*) nbns
c
c  lecture des bins secs
c
        do 410 nb=1,nbns
          read(20,*) binflag(nb),rayon1(nb),rayon2(nb)
          lenbflag(nb)=index(binflag(nb),' ')-1
 410    continue
c
c  lecture des donnees concernant la geometrie de la grille du 
c  modele
c
        read(20,*) ncellx
        read(20,*) lcellx
        read(20,*) ncelly
        read(20,*) lcelly
        read(20,*) ncellz
        do 420 k=1,ncellz
          read(20,*) lcellz(k)
 420    continue
        read(20,*) xcell0, ycell0, zcell0
        if (ycell0.lt.0.) ycell0=360.+ycell0
c
c  lecture des types d aerosols presents 
c
        read(20,*) ntype
        do 430 nr=1,ntype
          read(20,*) nomtype(nr)
         lentype(nr)=index(nomtype(nr),' ')-1
 430    continue
        read(20,*) bidon
        do 440 nr=1,ntype
          do 450 nb=1,nbns
            do 460 k=1,ncellz
              read(20,*) bidon
              read(20,*) ((numvol(nr,nb,i,j,k),j=1,ncelly),
     +        i=ncellx,1,-1)
 460        continue
 450      continue
 440    continue
      close(unit=20)
c
c  conversion en unite de part/m^3
c
c  converting number density into total number of particles by
c  cell 
c
      do 520 i=1,ncellx
        latitu=xcell0+lcellx*real(i-1)
        call echelle(latitu,lcellx,lcelly,dx,dy)
        do 530 j=1,ncelly
          do 540 k=1,ncellz
            do 500 nt=1,ntype
              do 510 nb=1,nbns
                numvol(nt,nb,i,j,k)=numvol(nt,nb,i,j,k)*gain
                numvol(nt,nb,i,j,k)=numvol(nt,nb,i,j,k)*dx*dy*
     +          lcellz(k)
 510          continue
 500        continue
 540      continue
 530    continue       
 520  continue                         
c
c
c  determine the number of time step
c
      timeint=jday2-jday1
      tint=nint(timeint/(timestep/24.))
      deltat=timeint/dble(tint)
      print*,'Time step (days): ',deltat
      print*,'Number of time steps: ',tint
      time=jday1
c
c  taille de la boite de lissage cette taille est dependante
c  du pas de calcul.  Pour une source ponctuelle trop localisee
c  l effet de l advection avec un pas de calcul trop grand
c  sera de produire un point a chaque pas.  Donc au lieu
c  d un panache, nous aurons un pointille.  
c
      boxw=(int(timestep/2./lcellx))/2*2+1  
c
c  Lecture des fichiers d index 
c
c  format: 
c
c  nombre_de_fichiers
c  hre1 min1 sec1 jour1 mois1 annee1  nom1
c  hre2 min2 sec2 jour2 mois2 annee2  nom2
c  .
c  .
c  .
c
      open(unit=5,file="wspeed.index",status='old',err=6)
        open(unit=7,file="relhum.index",status='old',err=7)
          print*,'Reading meteo file index...' 
          read(5,*) nwspfi
          do 100 nws=1,nwspfi
            read(5,*) hre,min,sec,jou,moi,ann,nomws(nws)
            call julian(hre,min,sec,jou,moi,ann,jday)
            wsjj(nws)=jday
 100      continue
          read(7,*) nrhfi
          do 120 nrhf=1,nrhfi
            read(7,*) hre,min,sec,jou,moi,ann,nomrh(nrhf)
            call julian(hre,min,sec,jou,moi,ann,jday)
            rhjj(nrhf)=jday
 120      continue
        close(unit=5)
      close(unit=7)
      if (pluie.ne.0) then
        open(unit=8,file="precip.index",status='old',err=8)
          read(8,*) nwetfi
          do 140 nwe=1,nwetfi
            read(8,*) hre,min,sec,jou,moi,ann,nomwet(nwe)
            call julian(hre,min,sec,jou,moi,ann,jday)
            wejj(nwe)=jday
 140      continue
        close(unit=8)
      endif

        open(unit=9,file="airtmp.index",status='old',err=18)
          read(9,*) nttfi
          do ntt=1,nttfi
            read(9,*) hre,min,sec,jou,moi,ann,nomtt(ntt)
            call julian(hre,min,sec,jou,moi,ann,jday)
            ttjj(ntt)=jday
          enddo
        close(unit=9)
      if (fixsour.eq.1) then
c
c  lecture de l index des emissions
c
        open(unit=4,file='source.index',status='old',err=555)
          print*,'Reading emission file index...' 
          read(4,*) nsource
          do nsr=1,nsource
            read(4,*) hre,min,sec,jou,moi,ann,nomsrc(nsr)
            call julian(hre,min,sec,jou,moi,ann,jday)
            srcjj(nsr)=jday
          enddo
        close(unit=4)
      endif
c
c  lecture de l index des eponges (buffer)
c
      open(unit=4,file='buffer.index',status='old',err=555)
        print*,'Reading buffer file index...' 
        read(4,*) nbuffer
        do nbu=1,nbuffer
          read(4,*) hre,min,sec,jou,moi,ann,nombuf(nbu)
          call julian(hre,min,sec,jou,moi,ann,jday)
          bufjj(nbu)=jday
        enddo
      close(unit=4)
c
c  lecture du fichier de croissance hygrometrique (humidite)
c  and associate values to each particle type
c
      OPEN(UNIT=18,FILE='humout',STATUS='OLD')
        print*,'Reading hygrometric growing factor file: humout'
        READ(18,*) ncroi,nhum,nbnhum
        do 260 nc=1,ncroi
          read(18,*) croiname(nc)
          do 270 nh=1,nhum
            read(18,*) bidon
            read(18,*) bidon
            do 280 nb=1,nbnhum
              read(18,*) bido1,radrat(nc,nh,nb),finden(nc,nh,nb)
 280        continue
 270      continue
 260    continue
        do 290 nt=1,ntype
          do 300 nc=1,ncroi
            if (nomtype(nt)(1:5).eq.croiname(nc)(1:5)) then
              ncr(nt)=nc
            endif  
 300      continue
 290    continue
      CLOSE(UNIT=18)
c
c  computing center height of each vertical slice
c
      do 310 k=1,ncellz
        zgrid=lcellz(k)/2.
        do 320 nn=1,k-1
          zgrid=zgrid+lcellz(nn)
 320    continue
        hcellz(k)=zgrid
 310  continue
c
c  calcul du volume moyen sec d une particule du bin nb en m^3
c  (1e-18 est le facteur de conversion de micron^3 a m^3
c
      print*,'Computing dry mean volume and diameter...'
      do 330 nt=1,ntype
        do 340 nb=1,nbns
          volume(nb)=4.*pi*1.e-18*((rayon1(nb)+rayon2(nb))/2.)**3./3.
          diamet(nb)=rayon1(nb)+rayon2(nb)
 340    continue
 330  continue
c
c =============================================
c
c   Calcul des coefficients de coagulation (Brownian coagulation)
c   according to Seinfeld and Pandis 1998 
c   
c   coagulation coefficients Ki,j are computed from Fuchs 1964
c
c  L=mean free path (L=6.53E-8 for stp=Lstp)
c  hgaz=8000. m (as in 6-S)
c  rho0=1.29 kg/m^3 from Giancoli 1993
c
       if (coswi.eq.1) then
         print*,'Computing coagulation coefficient database...'
         rho0=1.29
         hgaz=8000.
         Lstp=6.53E-8
         do nt=1,ntype
          do nz=1,nzlev
             rhoair=rho0*exp(-hcellz(nz)/hgaz)
             L=Lstp*(rho0/rhoair)
          do ntmp=1,29
           tempera=real(ntmp-15)*5.+273.15      
           do nh=1,nhum
             do nb1=1,nbns                      
               do nb2=1,nbns    
                 masse1=finden(ncr(nt),nh,nb1)*volume(nb1)*
     +           radrat(ncr(nt),nh,nb1)**3.
                 diam1=diamet(nb1)*radrat(ncr(nt),nh,nb1)*1.e-6
                 masse2=finden(ncr(nt),nh,nb2)*volume(nb2)*
     +           radrat(ncr(nt),nh,nb2)**3.
                 diam2=diamet(nb2)*radrat(ncr(nt),nh,nb2)*1.e-6
                 call kcoagul(L,masse1,masse2,diam1,diam2,tempera,
     +           k12)
                 kcoag(nz,ntmp,nh,nt,nb1,nb2)=k12   
               enddo
             enddo
           enddo
          enddo
          enddo
         enddo
       endif
c
c ---------------------------------------------------------------------
c
c  beginning dynamic loop
c
      do 600 t=1,tint
        call timedate(hret,mint,sect,jout,moit,annt,time)
        print*,'Computing dynamics for time: '
     +  ,nint(hret),'H',nint(mint),'M',nint(sect),'S (',
     +  nint(jout),'/',nint(moit),'/',nint(annt),')'
        if (fixsour.eq.1) then
c
c  initializing source matrix
c
          do i=1,ncellx
            do j=1,ncelly
                    source2(i,j)=0.
              do k=1,ncellz
                do nr=1,ntype
                  do nb=1,nbns
                    source(nr,nb,i,j,k)=0.
                  enddo
                enddo
              enddo
            enddo   
          enddo   
c
c  loading source data file
c  les donnees sont en unites de (gain) part/m^3/day
c
c  Determine the right file
c
          dtmin=10000000.
          do nsr=1,nsource
            if (abs(time-srcjj(nsr)).lt.dtmin) then
              dtmin=abs(time-srcjj(nsr))
              srctim=srcjj(nsr)
              srcfile=nomsrc(nsr)
            endif
          enddo
          dtminm=dtmin
          dtmin=10000000.
          do nsr=1,nsource
            if ((abs(time-srcjj(nsr)).lt.dtmin).and.(srcjj(nsr)
     +      .ne.srctim)) then
              dtmin=abs(time-srcjj(nsr))
              srctim2=srcjj(nsr)
              srcfile2=nomsrc(nsr)
            endif
          enddo
          open(unit=20,file=srcfile,status='old',err=22)
            print*,'Reading emission file: ',srcfile(1:28)
            read(20,*) bidon
            read(20,*) bidon
            read(20,*) hre,min,sec,jou,moi,ann
            read(20,*) ival
            if (ival.ne.nbns) goto 33
c
c  lecture des bins secs
c
            do nb=1,nbns
              read(20,*) 
            enddo
c    
c  lecture des donnees concernant la geometrie de la grille
c  du modele
c
            read(20,*) ival
            if (ival.ne.ncellx) goto 34
            read(20,*) rval
            if (rval.ne.lcellx) goto 35
            read(20,*) ival
            if (ival.ne.ncelly) goto 34
            read(20,*) rval
            if (rval.ne.lcelly) goto 35
            read(20,*) nsrcz
            if (nsrcz.ge.1) then
              do k=1,nsrcz
                read(20,*) 
              enddo      
              read(20,*) 
c
c  lecture des types d aerosols presents
c
              read(20,*) ival
              if (ival.ne.ntype) goto 37
              do nr=1,ntype
                read(20,*) 
              enddo
              read(20,*) bidon
              do nr=1,ntype
                do nb=1,nbns
                  do k=1,nsrcz
                    read(20,*) bidon
	            read(20,*) ((source(nr,nb,i,j,k),j=1,ncelly),
     +              i=ncellx,1,-1)
                  enddo
                enddo
              enddo
            endif
          close(unit=20)
          open(unit=20,file=srcfile2,status='old',err=22)
            print*,'Reading emission file: ',srcfile2(1:28)
            read(20,*) bidon
            read(20,*) bidon
            read(20,*) hre,min,sec,jou,moi,ann
            read(20,*) ival
            if (ival.ne.nbns) goto 33
c
c  lecture des bins secs
c
            do nb=1,nbns
              read(20,*) 
            enddo
c
c  lecture des donnees concernant la geometrie de la grille
c  du modele
c
            read(20,*) ival
            if (ival.ne.ncellx) goto 34
            read(20,*) rval
            if (rval.ne.lcellx) goto 35
            read(20,*) ival
            if (ival.ne.ncelly) goto 34
            read(20,*) rval
            if (rval.ne.lcelly) goto 35
            if (nsrcz.ge.1) then
              read(20,*) nsrcz
              do k=1,nsrcz
                read(20,*) 
              enddo
              read(20,*) 
c
c  lecture des types d aerosols presents
c
              read(20,*) ival
              if (ival.ne.ntype) goto 37
              do nr=1,ntype
                read(20,*) 
              enddo
              read(20,*) bidon
              do nr=1,ntype
                do nb=1,nbns
                  do k=1,nsrcz
                    read(20,*) bidon
                    read(20,*) ((source2(i,j),j=1,ncelly),
     +              i=ncellx,1,-1)
c
c   interpoler la valeur de l inventaire de source
c
              do i=1,ncellx
                do j=1,ncelly
                    pente=(dble(source2(i,j)-
     +              source(nr,nb,i,j,k)))/(srctim2-srctim)
                    ordon=dble(source2(i,j))-pente
     +              *srctim2
                    source(nr,nb,i,j,k)=real(pente*time+ordon)
                enddo
              enddo
                  enddo
                enddo
              enddo
            endif
          close(unit=20)
c
c  conversion en unite de part/m^3/timeint
c
c  converting number density into total number of particles by
c  cell
c
          do i=1,ncellx
            latitu=xcell0+lcellx*real(i-1)
            call echelle(latitu,lcellx,lcelly,dx,dy)
            do j=1,ncelly
              do k=1,ncellz
                do nt=1,ntype
                  do nb=1,nbns
                    source(nt,nb,i,j,k)=source(nt,nb,i,j,k)*gain*
     +              deltat
                    source(nt,nb,i,j,k)=source(nt,nb,i,j,k)*dx*dy*
     +              lcellz(k)
                  enddo
                enddo
              enddo
            enddo
          enddo
c
c  lissage de l inventaire d emission
c    
          if (boxw.gt.1) then  
            print*,'Resampling emission inventory (1 ->',boxw,
     +      'pixels)...'
            call lissage(boxw,source,ncellx,ncelly,ncellz,ntype,
     +      nbns)
          endif
        endif 
c
c
c
c  Reading the buffer files nearest to the current time
c  
c
c  Determine the nearest file to the current time
c
        dtmin=10000000.
        do nbu=1,nbuffer
          if (abs(time-bufjj(nbu)).lt.dtmin) then
            dtmin=abs(time-bufjj(nbu))
            buftim=bufjj(nbu)
            buffile=nombuf(nbu)
          endif
        enddo
        call readbuf(buffile,nbns,ntype,ncellx,ncelly,ncellz,lcellx,
     +  lcelly,lcellz,xcell0,ycell0,numtemp)
c
c  Initialiser les frontieres avec la valeur de la climatologie
c  du GADS (P.Koepke, M.Hess, I.Schult,and E.P.Shettle (1997))
c
        do i=1,ncellx
          do j=1,ncelly              
            if (((i.le.tampon).or.(i.ge.ncellx-tampon+1)).or.
     +      ((j.le.tampon).or.(j.ge.ncelly-tampon+1))) then 
              do k=1,ncellz
                do nt=1,ntype
                  do nb=1,nbns
                    numvolp(nt,nb,i,j,k)=numtemp(nt,nb,i,j,k)
                  enddo
                enddo  
              enddo 
            endif              
          enddo
        enddo  
        if (fixsour.eq.1) then
c           
c  Adding primary emission 
c 
          do 960 nt=1,ntype
            do 970 nb=1,nbns
              do 980 i=1,ncellx
                do 990 j=1,ncelly
                  do 1000 k=1,ncellz
                    numvolp(nt,nb,i,j,k)=numvolp(nt,nb,i,j,k)+
     +              source(nt,nb,i,j,k)
                    if (numvolp(nt,nb,i,j,k).lt.0.) then
                      numvolp(nt,nb,i,j,k)=0.
                    endif
 1000             continue
 990            continue       
 980          continue                         
 970        continue
 960      continue
        endif
c
c  Reading the relative humidity files nearest to the current time
c  and converting onto the aerosol dist grid
c
c  Determine the right preceeding and succeeding files
c
        dtmin=10000000.
        do 610 nws=1,nrhfi
          if (abs(time-rhjj(nws)).lt.dtmin) then
            dtmin=abs(time-rhjj(nws))
            rhtim=rhjj(nws)
            rhfile=nomrh(nws)
          endif
 610    continue
        dtminm=dtmin
        dtmin=10000000.
        do nws=1,nrhfi
          if ((abs(time-rhjj(nws)).lt.dtmin).and.(rhjj(nws)
     +    .ne.rhtim)) then
            dtmin=abs(time-rhjj(nws))
            rhtim2=rhjj(nws)
            rhfile2=nomrh(nws)
          endif
        enddo
        call readrh(rhfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     +  lcellz,xcell0,ycell0,t_drh)
        if (nrhfi.gt.1) then
          call readrh(rhfile2,ncellx,ncelly,ncellz,lcellx,lcelly,
     +    lcellz,xcell0,ycell0,t_drh2)
c
c  interpoler la valeur de RH
c
          do ii=1,ncellx
            do jj=1,ncelly
              do kk=1,ncellz
                pente=(dble(t_drh2(ii,jj,kk)-
     +          t_drh(ii,jj,kk)))/(rhtim2-rhtim)
                ordon=dble(t_drh2(ii,jj,kk))-pente
     +          *rhtim2
                t_drh(ii,jj,kk)=idnint(pente*time+ordon)
              enddo
            enddo
          enddo
        endif
c
c  Reading the wind velocity file nearest to the time
c  and converting onto the aerosol dist grid
c
c  Determine the right file
c
        dtmin=10000000.
        do nws=1,nwspfi
          if (abs(time-wsjj(nws)).lt.dtmin) then
            if (time.ge.wsjj(nws)) then
              dtmin=abs(time-wsjj(nws))
              wstim=wsjj(nws)
              wsfile=nomws(nws)
            endif
          endif
        enddo
        dtminm=dtmin
        dtmin=10000000.
        do nws=1,nwspfi
          if ((abs(time-wsjj(nws)).lt.dtmin).and.(wsjj(nws)
     +    .ne.wstim)) then
            if (time.lt.wsjj(nws)) then
              dtmin=abs(time-wsjj(nws))
              wstim2=wsjj(nws)
              wsfile2=nomws(nws)
            endif
          endif
        enddo
        dtminm=dtmin
        dtmin=10000000.
        do nws=1,nwspfi
          if ((abs(time+deltat-wsjj(nws)).lt.dtmin).and.(wsjj(nws)
     +    .ne.wstim).and.(wsjj(nws).ne.wstim2)) then
            if (time+deltat.lt.wsjj(nws)) then
              dtmin=abs(time+deltat-wsjj(nws))
              wstim3=wsjj(nws)
              wsfile3=nomws(nws)
            endif
          endif
        enddo
        call readws(wsfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     +  lcellz,xcell0,ycell0,vxwind,vywind,vzwind)
        if (nwspfi.gt.2) then
          call readws(wsfile2,ncellx,ncelly,ncellz,lcellx,lcelly,
     +    lcellz,xcell0,ycell0,vxwind2,vywind2,vzwind2)
          call readws(wsfile3,ncellx,ncelly,ncellz,lcellx,lcelly,
     +    lcellz,xcell0,ycell0,vxwind3,vywind3,vzwind3)
c
c  interpoler la valeur de wspeed pour le temps et le pas de calcul suivant
c
          do ii=1,ncellx
            do jj=1,ncelly
              do kk=1,ncellz
                pente=(dble(vxwind2(ii,jj,kk)-
     +          vxwind(ii,jj,kk)))/(wstim2-wstim)
                ordon=dble(vxwind2(ii,jj,kk))-pente
     +          *wstim2
                pentep=(dble(vxwind3(ii,jj,kk)-
     +          vxwind2(ii,jj,kk)))/(wstim3-wstim2)
                ordonp=dble(vxwind3(ii,jj,kk))-pentep
     +          *wstim3
                vxwind(ii,jj,kk)=real(pente*time+ordon)
                if (time+deltat.gt.wstim2) then
                  vxwindp(ii,jj,kk)=real(pentep*(time+deltat)+ordonp)
                else
                  vxwindp(ii,jj,kk)=real(pente*(time+deltat)+ordon)
                endif
c
                pente=(dble(vywind2(ii,jj,kk)-
     +          vywind(ii,jj,kk)))/(wstim2-wstim)
                ordon=dble(vywind2(ii,jj,kk))-pente
     +          *wstim2
                pentep=(dble(vywind3(ii,jj,kk)-
     +          vywind2(ii,jj,kk)))/(wstim3-wstim2)
                ordonp=dble(vywind3(ii,jj,kk))-pentep
     +          *wstim3
                vywind(ii,jj,kk)=real(pente*time+ordon)
                if (time+deltat.gt.wstim2) then
                  vywindp(ii,jj,kk)=real(pentep*(time+deltat)+ordonp)
                else
                  vywindp(ii,jj,kk)=real(pente*(time+deltat)+ordon)
                endif
c
                pente=(dble(vzwind2(ii,jj,kk)-
     +          vzwind(ii,jj,kk)))/(wstim2-wstim)
                ordon=dble(vzwind2(ii,jj,kk))-pente
     +          *wstim2
                pentep=(dble(vxwind3(ii,jj,kk)-
     +          vzwind2(ii,jj,kk)))/(wstim3-wstim2)
                ordonp=dble(vzwind3(ii,jj,kk))-pentep
     +          *wstim3
                vzwind(ii,jj,kk)=real(pente*time+ordon) 
                if (time+deltat.gt.wstim2) then
                  vzwindp(ii,jj,kk)=real(pentep*(time+deltat)+ordonp)
                else
                  vzwindp(ii,jj,kk)=real(pente*(time+deltat)+ordon)
                endif
c
c  pour corriger le probleme occasionnel de tres grands vents verticaux
c  avec gem
c
                if (abs(vzwind(ii,jj,kk)).gt.0.05) vzwind(ii,jj,kk)=0.
              enddo
            enddo
          enddo
        endif 
        if (timecor.ne.1.0) then
          do ii=1,ncellx
            do jj=1,ncelly
              do kk=1,ncellz
c
c  corriger la vitesse avec timecor
c
                vxwind(ii,jj,kk)=vxwind(ii,jj,kk)*timecor  
                vywind(ii,jj,kk)=vywind(ii,jj,kk)*timecor
                vzwind(ii,jj,kk)=vzwind(ii,jj,kk)*timecor
              enddo
            enddo
          enddo
        endif
c
c  Reading the precipitation file nearest to the time
c  and converting onto the aerosol dist grid 
c  
c  initial units from GEM file is accumulated in meter for 
c  6 hour time step
c  precip= m /s  nombre de metres d eau tombes en 1 seconde =
c  tau de precipitation
c
c  Determine the right file
c
        if (pluie.ne.0) then
          dtmin=10000000.
          do nwe=1,nwetfi
            if (abs(time-wejj(nwe)).lt.dtmin) then
              dtmin=abs(time-wejj(nwe))
              wetim=wejj(nwe)
              wetfile=nomwet(nwe)
            endif
          enddo
          dtminm=dtmin
          dtmin=10000000.
          do nwe=1,nwetfi
            if ((abs(time-wejj(nwe)).lt.dtmin).and.(wejj(nwe)
     +      .ne.wetim)) then
              dtmin=abs(time-wejj(nwe))
              wetim2=wejj(nwe)
              wetfile2=nomwet(nwe)
            endif
          enddo
          call readwet(wetfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     +    lcellz,xcell0,ycell0,precip)
          if (nwetfi.gt.1) then
            call readwet(wetfile2,ncellx,ncelly,ncellz,lcellx,lcelly,
     +      lcellz,xcell0,ycell0,precip2)
c
c  interpoler la valeur du taux de precipitation
c
            do ii=1,ncellx
              do jj=1,ncelly
                pente=(dble(precip2(ii,jj)-
     +          precip(ii,jj)))/(wetim2-wetim)
                ordon=dble(precip2(ii,jj))-pente
     +          *wetim2
                 precip(ii,jj)=real(pente*time+ordon)
              enddo
            enddo
          endif
c
c  converting meter/6 hour to m/s
c 
          do ia=1,ncellx
            do ib=1,ncelly
              precip(ia,ib)=precip(ia,ib)/(6.*3600.)
            enddo
          enddo    
        endif 
c
c  Reading the air temperature files nearest to the current time
c  and converting onto the aerosol dist grid
c
c  Determine the right preceeding and succeeding files
c

        dtmin=10000000.
        do ntt=1,nttfi
          if (abs(time-ttjj(ntt)).lt.dtmin) then
            dtmin=abs(time-ttjj(ntt))
            tttim=ttjj(ntt)
            ttfile=nomtt(ntt)
          endif
        enddo
        dtminm=dtmin
        dtmin=10000000.
        do ntt=1,nttfi
          if ((abs(time-ttjj(ntt)).lt.dtmin).and.(ttjj(ntt)
     +    .ne.tttim)) then
            dtmin=abs(time-ttjj(ntt))
            tttim2=ttjj(ntt)
            ttfile2=nomtt(ntt)
          endif
        enddo
        call readtt(ttfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     +  lcellz,xcell0,ycell0,tt_3d)
        if (nttfi.gt.1) then
          call readtt(ttfile2,ncellx,ncelly,ncellz,lcellx,lcelly,
     +    lcellz,xcell0,ycell0,tt2_3d)
c
c  interpoler la valeur de tt
c
          do ii=1,ncellx
            do jj=1,ncelly
              do kk=1,ncellz
                pente=(dble(tt2_3d(ii,jj,kk)-
     +          tt_3d(ii,jj,kk)))/(tttim2-tttim)
                ordon=dble(tt2_3d(ii,jj,kk))-pente
     +          *tttim2
                tt_3d(ii,jj,kk)=idnint(pente*time+ordon)
              enddo
            enddo
          enddo
        endif
c
c ===========================================================
c
c  Dynamical computations
c
        print*,'Computing trajectories...'
        if (t.gt.1) then
          do 700 nt=1,ntype
            do 710 nb=1,nbns
              do 720 nx=1,ncellx
                do 730 ny=1,ncelly
                  do 740 nz=1,ncellz
c
c     calcul de la diffusion (puff horizontal dispersion)
c
c     Khor=subgris-scale horizontal mixing coefficient
c        c.f. Smagorinsku, 1963 and Deardorff, 1973 (m2/s)
c     gradvx=partial diffential of vxwind as a function of x
c     gradvy=partial diffential of vywind as a function of x
c     u=zonal=est-wes=> =vywind(in aodsem) 
c     v=medional wind = north-south direction => =-vxwind (in aodsem)
c    




           numvol(nt,nb,nx,ny,nz)=0.
           metgrid=1.
c     selon Draxler et al.
           Tlu=10800.
           latitu=xcell0+lcellx*real(nx-1)
           call echelle(latitu,lcellx,lcelly,dx,dy) 
           difsiz=int(metgrid/lcellx)+1
                    Khor=2.**(-0.5)*dx*dy*(0.14*real(difsiz))**2.*
     + abs((vxwind(nx,ny,nz)-vxwind(nx+difsiz,ny,nz))/
     + (dy*real(difsiz))+(vywind(nx,ny,nz)-vywind(nx,ny+difsiz,nz))
     + /(dx*real(difsiz)))
           sigmau=(Khor/Tlu)**0.5
           dmean=(dx+dy)/2.
           pufsiz=int(1.54*(dmean/2.+1.4142*sigmau*(deltat*24.*3600.))
     + /dmean)
c
c  pour d�sactiver rapidement ce processus de diffusion
c  il s'agit de decommenter la ligne suivante
c           pufsiz=1000000


           if ((nx.gt.pufsiz).and.(nx.le.ncellx-pufsiz).and.(ny.gt.
     +     pufsiz).and.(ny.le.ncelly-pufsiz)) then
             do nx1=nx-pufsiz,nx+pufsiz
               do ny1=ny-pufsiz,ny+pufsiz
                 numvol(nt,nb,nx,ny,nz)=numvol(nt,nb,nx,ny,nz)+
     + numvolp(nt,nb,nx1,ny1,nz)/((2.*real(pufsiz)+1)**2.)  
               enddo
             enddo            
           else
             numvol(nt,nb,nx,ny,nz)=numvolp(nt,nb,nx,ny,nz)
           endif
 














 740              continue
 730            continue
 720          continue
 710        continue
 700      continue
          do 770 ia=1,ntyp
            do 771 ib=1,nbins
              do 772 ic=1,nxwid
                do 773 id=1,nywid
                  do 774 ie=1,nzlev
                    numvolp(ia,ib,ic,id,ie)=0.
 774              continue
 773            continue
 772          continue  
 771        continue
 770      continue          
        endif
c
c  production de la matrice de fraction de nuages
c  NCEP notes  Zhao et al, 1997 
c  production du masque de nuages
c
        open(unit=29,file='land-sea.txt',status='old',
     +  access='direct',recl=2,form='formatted')
          do nx=1,ncellx
            do ny=1,ncelly
              latitu=(real(nx)-1.)*lcellx+xcell0
              if (latitu.gt.90.) then
                print*,'Latitude is to high!'
                stop
              endif
              longit=(real(ny)-1.)*lcelly+ycell0
              if (longit.lt.0.) longit=longit+360.
              if (longit.gt.360.) longit=longit-360.
              lontemp=int(longit+180.)
              if (lontemp.ge.360) then
                lontemp=lontemp-360
              endif
              nbdat=int(-latitu+90.)*360+lontemp+1       
              read(29,2415,rec=nbdat) seamask
              if (seamask.eq.0) then
c  water surface case
                RH0=85
              else
c  land surface case
                RH0=75
              endif
              cloudmsk(nx,ny)=0
              cloudfract(nx,ny,nz)=0.
              do nz=1,ncellz
                if (t_drh(nx,ny,nz).lt.RH0) then
                  cloudfract(nx,ny,nz)=0.
                else
                cloudfract(nx,ny,nz)=(1.-((1.-real(t_drh(nx,ny,nz))/
     +          100.)/(1.-real(RH0)/100.))**0.5)
                endif
                if (cloudmsk(nx,ny).eq.0) then              
                  if (cloudfract(nx,ny,nz).gt.0.001) then
                    cloudmsk(nx,ny)=1
                  endif
                endif
              enddo
            enddo
          enddo
        close(unit=29)
 2415   format(i1)
c
c   Calcul de la coagulation (Brownian coagulation)
c   according to Seinfeld and Pandis 1998 adapted and simplified 
c   to the context of 12 size bins on a log scale (bin i+1 = 2 * bin i)
c   it could be shown in that case that coagulation is approximately
c   obtained by considering that the biggest particle incorporate the smallest
c   as a result the small particle disapear and the concentration and size of the
c   big one innt midified so much.  This is no thrue for 2 particles of same size 
c   to correct for that we suppose that when 2 n part. of size ri collides, the 
c   result will be that the concentration 2 rhoi of part. on size ri is the
c   becomes rhoi of size ri and rhoi of size 2 ri. 
c   
c   coagulation coefficients Ki,j are computed from Fuchs 1964
c
       if (coswi.eq.1) then
         print*,'Computing brownian coagulation process...'
       
         do nx=1,ncellx
           latitu=xcell0+lcellx*real(nx-1)
           call echelle(latitu,lcellx,lcelly,dx,dy)
           do ny=1,ncelly
             do nz=1,ncellz
               ntmp=nint(tt_3d(nx,ny,nz)/5.)+15
               if (ntmp.lt.1) ntmp=1
               if (ntmp.gt.29) ntmp=29
               relhum=t_drh(nx,ny,nz)
               call detnrh(relhum,nrh)      
               do nt=1,ntype              
                 do nb=1,nbns
                    numv(nb)=numvol(nt,nb,nx,ny,nz)/dx/dy/lcellz(nz)
                    dn_dt(nb)=0.
                 enddo
                 do nb1=1,nbns
                  if (numv(nb1).ne.0.) then



c                   dn_dt(nb1)=dn_dt(nb1)-0.41*numv(nb1)**2.*kcoag(nz,
c     +             ntmp,nrh,nt,nb1,nb1)
c                   if (nb1.ne.1) then
c                     dn_dt(nb1)=dn_dt(nb1)+0.41*numv(nb1-1)**2.*
c     +               kcoag(nz,ntmp,nrh,nt,nb1-1,nb1-1)  
c                   endif
c                   if (nb1.ne.nbns)  then  
                     
                     do nb2=nb1,nbns  

            if (nb2.gt.nb1) then
              dn_dt(nb1)=dn_dt(nb1)-numv(nb1)*numv(nb2)*
     +        kcoag(nz,ntmp,nrh,nt,nb1,nb2) 
              dn_dt(nb2)=dn_dt(nb2)-numv(nb1)*numv(nb2)*
     +        kcoag(nz,ntmp,nrh,nt,nb1,nb2)/(7.*2.**(3*(nb2-nb1)))
              if (nb2.lt.nbns) then
                dn_dt(nb2+1)=dn_dt(nb2+1)+numv(nb1)*numv(nb2)*
     +          kcoag(nz,ntmp,nrh,nt,nb1,nb2)/(7.*2.**(3*(nb2-nb1)))
              endif
            else
c
c  cas de 2 particules de meme taille
c

                dn_dt(nb1)=dn_dt(nb1)-numv(nb1)*numv(nb2)*
     +          kcoag(nz,ntmp,nrh,nt,nb1,nb2)/7./2.
                if (nb1.lt.nbns) then
                  dn_dt(nb1+1)=dn_dt(nb1+1)+numv(nb1)*numv(nb2)*
     +            kcoag(nz,ntmp,nrh,nt,nb1,nb2)/7./2.
                endif
             endif

c                       dn_dt(nb1)=dn_dt(nb1)-numv(nb1)*numv(nb2)*
c     +                 kcoag(nz,ntmp,nrh,nt,nb1,nb2)  
                  
                     enddo
     

c                    endif


c fin du if numvol ne 0
                  endif
                 enddo
c
c    calcul de la variation du nombre de particules due a la 
c    coagulation
c
                 do nb=1,nbns
                   numvol(nt,nb,nx,ny,nz)=numvol(nt,nb,nx,ny,nz)+
     +             dn_dt(nb)*(deltat*24.*3600.)*dx*dy*lcellz(nz)
                   if (numvol(nt,nb,nx,ny,nz).lt.0.) 
     +             numvol(nt,nb,nx,ny,nz)=0.
                 enddo
               enddo
             enddo
           enddo
         enddo
       endif
cccccccccc
cccccccccc
        do 820 nx=1,ncellx
          do 830 ny=1,ncelly
c
c  <<<<<<<<<
c
c  searching cloud bottom and top
c  bottom=first reach of cloudfract>0
c  top=next reach of cloudfract=0
c  ce calcul ne doit etre fait qu'une fois pour un meme
c  point de grille horizontal
c 
                if ((nt.eq.1).and.(nb.eq.1)) then
                  botc=0
                  topc=10
                  do 841 k=1,ncellz
                    if (botc.eq.0) then
                      if (cloudfract(nx,ny,k).ge.0.) then
                        botc=k
                      endif  
                    endif
                    if (botc.ne.0) then
                      if (cloudfract(nx,ny,k).eq.0.) then
                        topc=k-1
                      endif  
                    endif
 841              continue
                  if (topc.gt.9) topc=9
                endif
c
c  fin de la recherche de limites du nuage
c
c  >>>>>>>>>>
c
            do 840 nz=1,ncellz
c
c  determine the local relative humidity class (nrh)
c
              relhum=t_drh(nx,ny,nz)
              call detnrh(relhum,nrh)
              do 800 nt=1,ntype
                do 810 nb=1,nbns
                  if (numvol(nt,nb,nx,ny,nz).gt.0.) then
                    x=0.
                    y=0.
                    z=hcellz(nz)
                    if (pluie.ne.0) then
                      if (precip(nx,ny).ne.0.) then
c
c  Computing under-cloud wet removal
c  with a time constant from Garcia Nieto et al 1994
c  depend de la taille et du degre
c  de precipitation (heavy rain, drizzle)
c
                        if (nz.lt.botc) then
                          if ((precip(nx,ny).le.6.9e-6).and.
     +                    (precip(nx,ny).ge.1.4e-7)) then
                            if (6.9e-6/precip(nx,ny).lt.precip(nx,ny)/
     +                      1.4e-7) then
                              uncloud=ucloudh(nb)
                            else
                              uncloud=ucloudd(nb)
                            endif 
                          elseif (precip(nx,ny).lt.1.4e-7) then
                            uncloud=ucloudd(nb) 
                          else
                            uncloud=ucloudh(nb)     
                          endif
                          wetrem(nt,nb,nx,ny)=wetrem(nt,nb,nx,ny)+
     +                    numvol(nt,nb,nx,ny,nz)*(1.-exp(-deltat*24.
     +                    *3600.*uncloud))
                          numvol(nt,nb,nx,ny,nz)=numvol(nt,nb,nx,ny,
     +                    nz)*exp(-deltat*24.*3600.*uncloud)
                        endif
c
c  incloud scavenging
c
c  Due to poor vertical resolution at
c  typical cloud height we limit in-cloud scavenging to max
c  level 9 (max z=12km)
c
                        if ((nz.ge.botc).and.(nz.lt.topc)) then
c
c  from Walton et al. 1988
c  ponderer pour le taux de precipitation dans le niveau
c  par rapport au nuage complet
c  nz  pcor=lcellz(nz)/((hcellz(topc)-lcellz(topc)/2)-
c  (hcellz(botc)-lcellz(botc)/2))
c
                          pcor=lcellz(nz)/((hcellz(topc)+lcellz(topc)
     +                    /2.)-(hcellz(botc)-lcellz(botc)/2.))
                          wetrem(nt,nb,nx,ny)=wetrem(nt,nb,nx,ny)+
     +                    numvol(nt,nb,nx,ny,nz)*(1.-exp(-deltat*
     +                    scarat*24.*3600.*precip(nx,ny)*pcor))
                          numvol(nt,nb,nx,ny,nz)=numvol(nt,nb,nx,ny,
     +                    nz)*exp(-deltat*24.*3600.*scarat*precip(nx,
     +                    ny)*pcor)
                        endif
c
c  fin du calcul wet removal
c
                      endif
                    endif
c
c  determine the particle mass
c
                    masse=finden(ncr(nt),nrh,nb)*volume(nb)*
     +              radrat(ncr(nt),nrh,nb)**3.
c
c  determine fiction coeff. of the particle
c  from Van der Hoven 1968
c
c  cc=correction coeff for small particles
c  L=mean free path (L=6.53E-8 for stp=Lstp)
c  air viscosity coeff.=0.018E-3 Pa.s
c  hgaz=8000. m (as in 6-S)
c  rho0=1.29 kg/m^3 from Giancoli 1993
c
                    viscos=0.01789E-3
                    rho0=1.29
                    hgaz=8000.
                    Lstp=6.53E-8
                    rhoair=rho0*exp(-z/hgaz)
                    L=Lstp*(rho0/rhoair)
                    cc=1.+2.*(L/diamet(nb)/radrat(ncr(nt),nrh,nb))*
     +              (1.26+0.4*exp(-1.1*diamet(nb)*radrat(ncr(nt),nrh,
     +              nb)/2./L))
                    b=3.*pi*1.E-6*diamet(nb)*radrat(ncr(nt),nrh,nb)*
     +              viscos/cc
c
c  calcul de la vitesse aerodynamique limite en z
c
                    vlimz=(rhoair-finden(ncr(nt),nrh,nb))*volume(nb)*
     +              radrat(ncr(nt),nrh,nb)**3.*g/b+vzwind(nx,ny,nz)
c
c  Vitesse limite verticale = limite aerodynamique
c
                      vz0=vlimz
c
c  Vitesse limite horizontale = vitesse du vent
c
                      vx0=vxwind(nx,ny,nz)
                      vy0=vywind(nx,ny,nz)
c
c  determining next particle position (first guess)
c
                    xp=vx0*deltat*24.*3600.
                    yp=vy0*deltat*24.*3600.
                    zp=vz0*deltat*24.*3600.+z
c
c  new position in latitude longitude (0.1-->degres)
c
                    latitu=xcell0+lcellx*real(nx-1)
                    longit=ycell0+lcelly*real(ny-1)
                    call echelle(latitu,0.1,0.1,dx,dy)
                    latitu=latitu+xp*0.1/dx
                    longit=longit+yp*0.1/dy
c
c  Computing the actual cell position of the tracer
c
                    newnx=nint((latitu-xcell0)/lcellx)+1
                    newny=nint((longit-ycell0)/lcelly)+1
                    latcel=xcell0+lcellx*real(newnx-1) 
                    loncel=ycell0+lcelly*real(newny-1)
                    do nn=1,ncellz
                      if (z.gt.hcellz(nn)-lcellz(nn)/2.) then
                        if (z.le.hcellz(nn)+lcellz(nn)/2.) then 
                          newnz=nn
                        endif
                      endif
                    enddo 
                    if (z.le.0.) newnz=0



c
c  ajouter ici le calcul de la vitesse prime a la position estimee
c
                    vxp=vxwindp(newnx,newny,newnz)
                    vyp=vywindp(newnx,newny,newnz)
c
c  calcul de la vitesse aerodynamique limite en z
c
                    vlimz=(rhoair-finden(ncr(nt),nrh,nb))*volume(nb)*
     +              radrat(ncr(nt),nrh,nb)**3.*g/b+vzwindp(newnx,newny
     +              ,newnz)
                    vzp=vlimz


c
c  calcul de la position finale
c
                    x=(vx0+vxp)/2.*deltat*24.*3600.
                    y=(vy0+vyp)/2.*deltat*24.*3600.
                    z=(vz0+vzp)/2.*deltat*24.*3600.+z
c
c  final position in latitude longitude (0.1-->degres)
c
                    latitu=xcell0+lcellx*real(nx-1)
                    longit=ycell0+lcelly*real(ny-1)
                    call echelle(latitu,0.1,0.1,dx,dy)
                    latitu=latitu+x*0.1/dx
                    longit=longit+y*0.1/dy
c
c  Computing the final cell position of the tracer
c
                    newnx=nint((latitu-xcell0)/lcellx)+1
                    newny=nint((longit-ycell0)/lcelly)+1
                    latcel=xcell0+lcellx*real(newnx-1) 
                    loncel=ycell0+lcelly*real(newny-1)
                    do nn=1,ncellz
                      if (z.gt.hcellz(nn)-lcellz(nn)/2.) then
                        if (z.le.hcellz(nn)+lcellz(nn)/2.) then 
                          newnz=nn
                        endif
                      endif
                    enddo 
                    if (z.le.0.) newnz=0                  






c
c  Computing new aerosol distribution
c
                    if (latitu.ge.latcel) then
                      ratx(3)=(latitu-latcel)/lcellx
                      ratx(1)=0.
                      ratx(2)=1.-ratx(3)
                    else
                      ratx(3)=0.
                      ratx(1)=(latcel-latitu)/lcellx
                      ratx(2)=1.-ratx(1)
                    endif
                    if (longit.ge.loncel) then
                      raty(3)=(longit-loncel)/lcelly
                      raty(1)=0.
                      raty(2)=1.-raty(3)
                    else
                      raty(3)=0.
                      raty(1)=(loncel-longit)/lcelly
                      raty(2)=1.-raty(1)
                    endif
                    if (newnz.ge.1) then
                      if (z.ge.hcellz(newnz)) then
                        ratz(3)=(z-hcellz(newnz))/lcellz(newnz)
                        ratz(1)=0.
                        ratz(2)=1.-ratz(3)
                      else
                        ratz(3)=0.
                        ratz(1)=(hcellz(newnz)-z)/lcellz(newnz)
                        ratz(2)=1.-ratz(1)
                      endif
                    else
c
c  dry removal case
c
                      if (z.gt.-hcellz(1)) then
                        ratz(3)=(hcellz(1)-z)/lcellz(1)
                        ratz(1)=0.
                        ratz(2)=1.-ratz(3)
                      else
                        ratz(3)=0.
                        ratz(1)=0.
                        ratz(2)=1.
                      endif
                    endif
                    do 900 ni=-1,1
                      do 910 nj=-1,1
                        do 920 nk=-1,1
                          if (((newnx+ni).ge.1).and.((newnx+ni).le.
     +                    ncellx)) then
                            if (((newny+nj).ge.1).and.((newny+nj).
     +                      le.ncelly)) then
c
c  ni+2 car le vecteur ratx() commence a l  
c  indice 1(ni+2 E 1-3)
c
                              if (((newnz+nk).ge.1).and.((newnz+nk).
     +                        le. ncellz)) then
                                numvolp(nt,nb,newnx+ni,newny+nj,
     +                          newnz+nk)= ratx(ni+2)*raty(nj+2)*
     +                          ratz(nk+2)*numvol(nt,nb,nx,ny,nz)+
     +                          numvolp(nt,nb,newnx+ni,newny+nj,newnz+
     +                          nk)
                              else
                                depot(nt,nb,newnx+ni,newny+nj)=ratx(ni+
     +                          2)*raty(nj+2)*ratz(nk+2)*numvol(nt,nb,
     +                          nx,ny,nz)+depot(nt,nb,newnx+ni,newny+
     +                          nj)
                              endif
                            endif
                          endif
 920                    continue
 910                  continue
 900                continue  
                  endif
 810            continue
 800          continue
 840        continue
 830      continue
          if (int(real(nx*100)/real(ncellx)).eq.int(real(nx*5)/ 
     +    real(ncellx*20))) print*,nx*100/ncellx,'%'
 820    continue

c
c  Ecrire le fichier masque de nuage
c
        call timedate(hre,min,sec,jou,moi,ann,time+deltat)
        print*,'Writing cloud file for time: ' 
     +  ,nint(hre),'H',nint(min),'M',nint(sec),'S (',nint(jou),
     +  '/',nint(moi),'/',nint(ann),')'
        call wrcloud(cloudmsk,ncellx,ncelly,xcell0,ycell0,
     +  lcellx,t,hre,min,sec,jou,moi,ann)
c
c  Ecrire du fichier de distribution background
c
        print*,'Writing output for time: ' 
     +  ,nint(hre),'H',nint(min),'M',nint(sec),'S (',nint(jou),
     +  '/',nint(moi),'/',nint(ann),')'
        call wrnumvol(numvolp,ncellx,lcellx,ncelly,lcelly,ncellz,
     +  lcellz,xcell0,ycell0,zcell0,nbns,rayon1,rayon2,ntype,
     +  nomtype,gain,t,t_drh,hre,min,
     +  sec,jou,moi,ann,tampon) 
c
c  incremente le temps
c
        time=jday1+dble(t)*deltat                 
 600  continue 
c
c  converting total wet and dry removal in unit of part/m^2
c
      do 1120 i=1,ncellx
        latitu=xcell0+lcellx*real(i-1)
        call echelle(latitu,lcellx,lcelly,dx,dy)
        do 1100 nt=1,ntype
          do 1110 nb=1,nbns
            do 1130 j=1,ncelly
              depot(nt,nb,i,j)=depot(nt,nb,i,j)/dx/dy
              wetrem(nt,nb,i,j)=wetrem(nt,nb,i,j)/dx/dy
 1130       continue       
 1110     continue                         
 1100   continue
 1120 continue
c
c  Ecriture du fichier de sedimentation seche depot(nt,nb,nx,ny)
c  j -> longitude i->latitude
c  first line of data correspond to the upper line of the image
c  first data of the first line correspond to the north-east 
c  corner. units of part./m^2
c
      open(unit=1,file='drydepf.dat',status='unknown')
        print*,'Writing final dry deposition file: drydepf.dat'
        write(1,*) '2-d aerosol dry deposition file'
        write(1,*) 'Units of ',gain,' part/m^2 for ',timeint,' days.'
        write(1,*) nint(heure),nint(minute),nint(seconde),nint(jour),
     +  nint(mois),nint(annee),' TU (hh mm ss), Date (jj mm yyyy)'
        write(1,*) nbns,'  Number of size bins'
        do 1300 nb=1,nbns
          write(1,*) nb,rayon1(nb),rayon2(nb)
 1300   continue
        write(1,*) ncellx,'  Number of X cells'
        write(1,*) lcellx,'  Size of X cell'
        write(1,*) ncelly,'  Number of Y cells'
        write(1,*) lcelly,'  Size of Y cell'
        write(1,*) '1  Number of Z cells'
        write(1,*) '0.'         
        write(1,*) xcell0, ycell0,zcell0,'  X0, Y0, Z0'
        write(1,*) ntype,'  Number of aerosol types'
        do 1310 nr=1,ntype
          write(1,*) nomtype(nr)
 1310   continue
        write(1,*) 'DATA'
        do 1320 nt=1,ntype
          do 1330 nb=1,nbns
            write(1,*) '# AEROSOL TYPE= ',nt,' SIZE BIN= ',nb,
     +      'VERTICAL LEVEL= SURFACE' 
            do 1340 i=ncellx,1,-1
              write(1,*) (nint(depot(nt,nb,i,j)/gain),j=1,ncelly)
 1340       continue
 1330     continue
 1320   continue
      close(unit=1)
c
c  Ecriture du fichier de sedimentation humide  
c  wetrem(nt,nb,nx,ny) j -> longitude i->latitude
c  first line of data correspond to the upper line of the image
c  first data of the first line correspond to the north-east 
c  corner. units of part./m^2
c
      open(unit=1,file='wetdepf.dat',status='unknown')
        print*,'Writing final wet deposition file: wetdepf.dat'
        write(1,*) '2-d aerosol wet deposition file'
        write(1,*) 'Units of ',gain,' part/m^2 for ',timeint,' days.'
        write(1,*) nint(heure),nint(minute),nint(seconde),nint(jour),
     +  nint(mois),nint(annee),' TU (hh mm ss), Date (jj mm yyyy)'
        write(1,*) nbns,'  Number of size bins'
        do 1400 nb=1,nbns
          write(1,*) nb,rayon1(nb),rayon2(nb)
 1400   continue
        write(1,*) ncellx,'  Number of X cells'
        write(1,*) lcellx,'  Size of X cell'
        write(1,*) ncelly,'  Number of Y cells'
        write(1,*) lcelly,'  Size of Y cell'
        write(1,*) '1  Number of Z cells'
        write(1,*) '0.'         
        write(1,*) xcell0, ycell0,zcell0,'  X0, Y0, Z0'
        write(1,*) ntype,'  Number of aerosol types'
        do 1410 nr=1,ntype
          write(1,*) nomtype(nr)
 1410   continue
        Write(1,*) 'DATA'
        do 1420 nt=1,ntype
          do 1430 nb=1,nbns
            write(1,*) '# AEROSOL TYPE= ',nt,' SIZE BIN= ',nb,
     +      'VERTICAL LEVEL= SURFACE'
            do 1440 i=ncellx,1,-1
	      write(1,*) (nint(wetrem(nt,nb,i,j)/gain),j=1,ncelly)
 1440       continue
 1430     continue
 1420   continue
      close(unit=1)
      print*,'Normal end of dynamic.'
      stop
 4    print*,'File ',nom1(1:lenom1+8),' not found!' 
      stop
 6    print*,'File wspeed.index not found!'
      stop
 7    print*,'File relhum.index not found!'
      stop
 8    print*,'File precip.index not found!'
      stop
 18   print*,'File airtmp.index not found!'
      stop
 9    print*,'Aerosol dist. file too big (max ',nxwid,'x',
     +nywid,'x',nzlev,')!'
      stop
 19   print*,'Bad number of size bins (should be 12)!'
      stop
 20   print*,'Bad number of aerosol types (should be 5)!'
      stop
 21   print*,'Parameter file dynamic.par not found!'
      stop
 22   print*,'Source file not found! Please run mksrc.'
      stop
 23   print*,'Nb of size bins in ',buffile,' don t match run prop.'
      stop
 24   print*,'Nb of cells in ',buffile,' don t match run properties'
      stop
 25   print*,'Cell size in ',buffile,' don t match run properties'
      stop
 26   print*,'Nb of vert. cells in ',buffile,' don t match run prop.'
      stop
 27   print*,'Nb of aerosol types in ',buffile,' don t match run prop.'
      stop
 33   print*,'Nb of size bins in scr file don t match run prop.'
      stop
 34   print*,'Nb of cells in src file don t match run properties'
      stop
 35   print*,'Cell size in src file don t match run properties'
      stop
 37   print*,'Nb of aerosol types in src file don t match run prop.'
      stop
 555  print*,'Source index source.index not found!'
      end
c
c =============================================================
c ===============  FIN DU PROGRAMME PRINCIPAL  ================
c =============================================================
c
c
c ----------------------------------------------------------
c
c  Routine d ecriture des distributions
c
c
c
c  Ecriture du fichier de distribution des aerosols pour ftime
c
c  Ecriture du fichier d observation
c
      subroutine wrnumvol(numvolp,ncellx,lcellx,ncelly,lcelly,
     +ncellz,lcellz,xcell0,ycell0,zcell0,nbns,rayon1,rayon2,
     +ntype,nomtype,gain,t,t_drh,hre,min,
     +sec,jou,moi,ann,tamp)
c
c  declarations des variables
c
c
      integer nbins,nzlev,nxwid,nywid,ntyp
      parameter (nbins=12,nzlev=10,nxwid=80,nywid=160,ntyp=5)
c
c
c
        integer ncellz,ncellx,ncelly,nbns,ntype,t,nt,nb,i,j,k
        integer t_drh(nxwid,nywid,nzlev),tamp
        real gain,numvo(5,12,nxwid,nywid,nzlev)
        real numvolp(5,12,nxwid,nywid,nzlev)
        real lcellx,lcelly,lcellz(nzlev)
        real xcell0,ycell0,zcell0,rayon1(12),rayon2(12),latitu
        real dx,dy
        real*8 hre,min,sec,jou,moi,ann
        character*10 nomtype(6)
        character*20 novis,nodat,norhu
        integer nm,nc,nd,nu
        character*1 table(10)
        character*4 tag
        data table /'0','1','2','3','4','5','6','7','8','9'/
        nm=t/1000
        nc=(t-nm*1000)/100
        nd=(t-nm*1000-nc*100)/10
        nu=t-nm*1000-nc*100-nd*10
        nm=nm+1
        nc=nc+1
        nd=nd+1
        nu=nu+1
        tag=table(nm)//table(nc)//table(nd)//table(nu)
        novis='numvol_'//tag//'.vis'
        nodat='numvol_'//tag//'.dat'
        norhu='numvol_'//tag//'.rhu'
c
c   Convertir le nombre de particules en nombre volumique
c   and setting a zero value in the buffer zone
c
        do i=1,ncellx
          latitu=xcell0+lcellx*real(i-1)
          call echelle(latitu,lcellx,lcelly,dx,dy)
          do nt=1,ntype
            do nb=1,nbns            
              do j=1,ncelly
                do k=1,ncellz
                  if (((i.le.tamp).or.(i.ge.ncellx-tamp+1)).or.
     +            ((j.le.tamp).or.(j.ge.ncelly-tamp+1))) then 
                    numvo(nt,nb,i,j,k)=0.
                  else   
                    numvo(nt,nb,i,j,k)=numvolp(nt,nb,i,j,k)/dx/dy/
     +              lcellz(k)
                  endif
                enddo
              enddo        
            enddo                         
          enddo
        enddo 
c
c  Fichiers .vis
c                                       
        open(unit=7,file=novis,status='unknown')
          print*,'Writing aerosol distribution file: ',novis
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
c  Ecriture des donnees de distribution des aerosols .dat
c
        open(unit=1,file=nodat,status='unknown')
          print*,'Writing aerosol distribution file: ',nodat
          write(1,*) '3-d aerosol distribution file for AODSEM from atm
     +o'
          write(1,*) 'Units of ',gain,' part/m^3'
          write(1,*) int(hre),int(min),int(sec),int(jou),int(moi),
     +    int(ann),' Starting Date (hh mm ss jj mm yyyy)'
          write(1,*) nbns,'  Number of size bins'
          do 1500 nb=1,nbns
            write(1,*) nb,rayon1(nb),rayon2(nb)
 1500     continue
          write(1,*) ncellx,'  Number of X cells'
          write(1,*) lcellx,'  Size of X cell'
          write(1,*) ncelly,'  Number of Y cells'
          write(1,*) lcelly,'  Size of Y cell'
          write(1,*) ncellz,'  Number of Z cells'
          do 1510 k=1,ncellz
            write(1,*) lcellz(k)
 1510     continue
          write(1,*) xcell0, ycell0, zcell0,'  X0, Y0, Z0'
          write(1,*) ntype,'  Number of aerosol types'
          do 1520 nr=1,ntype
            write(1,*) nomtype(nr)
 1520     continue
          write(1,*) 'DATA'
          do 1530 nt=1,ntype
            do 1540 nb=1,nbns
              do 1550 k=1,ncellz
                write(1,2625) nomtype(nt),nb,k
	        write(1,*) ((nint(numvo(nt,nb,i,j,k)/gain)
     +          ,j=1,ncelly),i=ncellx,1,-1)
 1550         continue
 1540       continue
 1530     continue
 2625     format('# AEROSOL TYPE: ',A9,' SIZE BIN: ',I2,
     +    ' VERTICAL LEVEL: ',I2)
        close(unit=1)
      return
      stop
      end

c
c ---------------------------------------------------------
c
c  routine permettant le calcul de l echelle d une cellule centree
c  a latt et avec une largeur angulaire dang pour une terre 
c  ellipsoide.
c
c  epsilon = difference d angle entre l horizon sur une sphere et
c            l horizon sur un ellipsoide
c  dangx =  taille de la cellule en degres
c  dangy =  taille de la cellule en degres
c  dx = taille de la cellule dans la direction s-n [m]
c  dy = taille de la cellule dans la direction w-e [m]
c  r = distance du point geographique par rapport au centre de la terre
c  a = demi grand axe (centre a l equateur)
c  b = demi petit axe (centre au pole)
c  he = angle d horizon de l ellipsoide
c  hs = angle d horizon de la sphere
c  x = coord de l ellipsoide
c  y=  coord de l ellipsoide
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
c
c -------------------------------------------------------
c
c  Routine de lecture du fichier d humidite relative
c
      subroutine readrh(rhfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     +lcellz,xcell0,ycell0,t_drh)
c
      integer nbins,nzlev,nxwid,nywid,ntyp
      parameter (nbins=12,nzlev=10,nxwid=181,nywid=360,ntyp=5)
c


        integer ncelxrh,ncelyrh,ncelzrh,ii,jj,kk,i,j,k,ncellx
        integer ncelly,ncellz,t_drh(nxwid,nywid,nzlev)
        integer rh3d(nxwid,nywid,nzlev)
        real lcelxrh,lcelyrh,lcelzrh(nzlev),xrh0,yrh0,zrh0,zrh,zrh_1
        real lcellz(nzlev),lcellx,lcelly,xcell0,ycell0,zgrid   
        character*60 rhfile  
        print*,'Reading humidity file: ',rhfile(1:28)
        open(unit=3,file=rhfile,status='old',err=24) 
          read(3,*)
          read(3,*) 
          read(3,*) ncelxrh
          read(3,*) lcelxrh
          read(3,*) ncelyrh
          read(3,*) lcelyrh
          read(3,*) ncelzrh
          do 61 k=1,ncelzrh
            read(3,*) lcelzrh(k)
 61       continue
          read(3,*) xrh0, yrh0, zrh0
          if (yrh0.lt.0.) yrh0=yrh0+360.           
          read(3,*)
          do kk=1,ncelzrh
            read(3,*)
            read(3,*) ((rh3d(ii,jj,kk),jj=1,ncelyrh),ii=ncelxrh,1,-1)
          enddo   
          do 62 i=1,ncellx
            do 63 j=1,ncelly
              do 64 k=1,ncellz
                zgrid=lcellz(k)/2.
                do 66 nn=1,k-1
                  zgrid=zgrid+lcellz(nn)
 66             continue
                ii=nint(1.+(xcell0-xrh0+lcellx*int(i-1))/lcelxrh)
                jj=nint(1.+(ycell0-yrh0+lcelly*int(j-1))/lcelyrh)
                zrh=zrh0
                do 67 nn=1,ncelzrh
                  zrh_1=zrh
                  zrh=zrh+lcelzrh(nn)
                  if ((zrh.gt.zgrid).and.(zrh_1.le.zgrid)) then
                    kk=nn
                  endif
 67             continue
                t_drh(i,j,k)=rh3d(ii,jj,kk)
 64           continue
 63         continue
 62       continue
        close(unit=3)
      return
 24   print*,'File ',rhfile,' inexistant!'
      stop
      end
c
c -------------------------------------------------------
c
c  Routine de lecture du fichier de vents
c
      subroutine readws(wsfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     +lcellz,xcell0,ycell0,vxwind,vywind,vzwind)
c
      integer nbins,nzlev,nxwid,nywid,ntyp
      parameter (nbins=12,nzlev=10,nxwid=181,nywid=360,ntyp=5)
c

        integer ncelxrh,ncelyrh,ncelzrh,ii,jj,kk,i,j,k,ncellx
        integer ncelly,ncellz,up
        real lcelxrh,lcelyrh,lcelzrh(30),xrh0,yrh0,zrh0,zrh
        real lcellz(nzlev),lcellx,lcelly,xcell0,ycell0
        real vxwind(nxwid,nywid,nzlev),vywind(nxwid,nywid,nzlev)
        real vzwind(nxwid,nywid,nzlev)
        real ws(3,nxwid,nywid,nzlev),zgrid 
        character*60 wsfile
        print*,'Reading wind speed file: ',wsfile(1:28)
        open(unit=3,file=wsfile,status='old',err=14) 
          read(3,*)
          read(3,*) 
          read(3,*) ncelxrh
          read(3,*) lcelxrh
          read(3,*) ncelyrh
          read(3,*) lcelyrh
          read(3,*) ncelzrh
          do 61 k=1,ncelzrh
            read(3,*) lcelzrh(k)
 61       continue
          read(3,*) xrh0, yrh0, zrh0
          if (yrh0.lt.0.) yrh0=yrh0+360.           
          read(3,*)
          do kk=1,ncelzrh
            read(3,*)
	    read(3,*) ((ws(1,ii,jj,kk),jj=1,ncelyrh),ii=ncelxrh,1,-1)      
          enddo
          do kk=1,ncelzrh
            read(3,*)
	    read(3,*) ((ws(2,ii,jj,kk),jj=1,ncelyrh),ii=ncelxrh,1,-1)     
          enddo
          do kk=1,ncelzrh
            read(3,*)
	    read(3,*) ((ws(3,ii,jj,kk),jj=1,ncelyrh),ii=ncelxrh,1,-1)     
          enddo          
          do 62 i=1,ncellx
            do 63 j=1,ncelly
              do 64 k=1,ncellz
                zgrid=lcellz(k)/2.
                do 66 nn=1,k-1
                  zgrid=zgrid+lcellz(nn)
 66             continue
                ii=nint(1.+(xcell0-xrh0+lcellx*int(i-1))/lcelxrh)
                jj=nint(1.+(ycell0-yrh0+lcelly*int(j-1))/lcelyrh)
                zrh=zrh0
                up=0
                do 67 nn=1,ncelzrh
                  zrh=zrh+lcelzrh(nn)
                  if ((zrh.gt.zgrid).and.(up.eq.0)) then
                    up=up+1
                    kk=nn
                  endif
 67             continue
                vxwind(i,j,k)=ws(1,ii,jj,kk)
                vywind(i,j,k)=ws(2,ii,jj,kk)
                vzwind(i,j,k)=ws(3,ii,jj,kk)
  64          continue
 63         continue
 62       continue
        close(unit=3)
      return
 14   print*,'File ',wsfile,' inexistant!'
      stop
      end
c
c -------------------------------------------------------
c
c  Routine de lecture du fichier de precipitation
c
      subroutine readwet(wetfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     +lcellz,xcell0,ycell0,precip)
c
      integer nbins,nzlev,nxwid,nywid,ntyp
      parameter (nbins=12,nzlev=10,nxwid=181,nywid=360,ntyp=5)
c


        integer ncelxrh,ncelyrh,ncelzrh,ii,jj,kk,i,j,k,ncellx
        integer ncelly,ncellz,up
        real lcelxrh,lcelyrh,lcelzrh(nzlev),xrh0,yrh0,zrh0,zrh
        real lcellz(nzlev),lcellx,lcelly,xcell0,ycell0
        real precip(nxwid,nywid)
        real zgrid,wmask(nxwid,nywid)
        character*60 wetfile
        print*,'Reading precipitation file: ',wetfile(1:28)
        open(unit=3,file=wetfile,status='old',err=15) 
          read(3,*)
          read(3,*) 
          read(3,*) ncelxrh
          read(3,*) lcelxrh
          read(3,*) ncelyrh
          read(3,*) lcelyrh
          read(3,*) xrh0, yrh0
          if (yrh0.lt.0.) yrh0=yrh0+360.           
            read(3,*)
            read(3,*) ((wmask(ii,jj),jj=1,ncelyrh),ii=1,ncelxrh) 
            do 72 i=1,ncellx
              do 73 j=1,ncelly
                ii=nint(1.+(xcell0-xrh0+lcellx*int(i-1))/lcelxrh)
                jj=nint(1.+(ycell0-yrh0+lcelly*int(j-1))/lcelyrh)
                precip(i,j)=wmask(ii,jj)
 74           continue
 73         continue
 72       continue
        close(unit=3)
      return
 15   print*,'File ',wetfile,' inexistant!'
      stop
      end
c
c -------------------------------------------------------
c
c  Routine de lecture du fichier de temperature
c
      subroutine readtt(ttfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     +lcellz,xcell0,ycell0,temper)

c
      integer nbins,nzlev,nxwid,nywid,ntyp
      parameter (nbins=12,nzlev=10,nxwid=181,nywid=360,ntyp=5)
c


        integer ncelxtt,ncelytt,ncelztt,ii,jj,kk,i,j,k,ncellx
        integer ncelly,ncellz,up
        real lcelxtt,lcelytt,lcelztt(30),xtt0,ytt0,ztt0,ztt
        real lcellz(nzlev),lcellx,lcelly,xcell0,ycell0
        real temper(nxwid,nywid,nzlev)
        real tt(nxwid,nywid,nzlev),zgrid 
        character*60 ttfile
        print*,'Reading air temperature file: ',ttfile(1:28)
        open(unit=3,file=ttfile,status='old',err=414) 
          read(3,*)
          read(3,*) 
          read(3,*) ncelxtt
          read(3,*) lcelxtt
          read(3,*) ncelytt
          read(3,*) lcelytt
          read(3,*) ncelztt
          do 461 k=1,ncelztt
            read(3,*) lcelztt(k)
 461      continue
          read(3,*) xtt0, ytt0, ztt0
          if (ytt0.lt.0.) ytt0=ytt0+360.           
          read(3,*)
          do kk=1,ncelztt
            read(3,*)
	    read(3,*) ((tt(ii,jj,kk),jj=1,ncelytt),ii=ncelxtt,1,-1)      
          enddo
          do 462 i=1,ncellx
            do 463 j=1,ncelly
              do 464 k=1,ncellz
                zgrid=lcellz(k)/2.
                do 466 nn=1,k-1
                  zgrid=zgrid+lcellz(nn)
 466            continue
                ii=nint(1.+(xcell0-xtt0+lcellx*int(i-1))/lcelxtt)
                jj=nint(1.+(ycell0-ytt0+lcelly*int(j-1))/lcelytt)
                ztt=ztt0
                up=0
                do 467 nn=1,ncelztt
                  ztt=ztt+lcelztt(nn)
                  if ((ztt.gt.zgrid).and.(up.eq.0)) then
                    up=up+1
                    kk=nn
                  endif
 467             continue
                temper(i,j,k)=tt(ii,jj,kk)
 464            continue
 463         continue
 462       continue
        close(unit=3)
      return
 414   print*,'File ',ttfile,' inexistant!'
      stop
      end
c
c ---------------------------------------------------------------------
c
c  Routine de lissage de la matrice de source 
c
      subroutine lissage(boxw,source,ncellx,ncelly,nsrcz,ntype,
     +nbns)  
c
      integer nbins,nzlev,nxwid,nywid,ntyp
      parameter (nbins=12,nzlev=10,nxwid=80,nywid=160,ntyp=5)
c
        real source(5,12,nxwid,nywid,nzlev),nval
        real sourcep(5,12,nxwid,nywid,nzlev)
        integer ncellx,ncelly,nsrcz,ntype,nbns,nx,ny,nz,nt,nb,i,j,boxw
        do nt=1,ntype
          do nb=1,nbns
            do nz=1,nsrcz
              do nx=1,ncellx
                do ny=1,ncelly
                  sourcep(nt,nb,nx,ny,nz)=0.
                  nval=0.
                  do i=nx-boxw/2,nx+boxw/2
                    do j=ny-boxw/2,ny+boxw/2
                      if ((i.ge.0).and.(i.le.ncellx).and.(j.ge.0).and.
     +                (j.le.ncelly)) then
                        sourcep(nt,nb,nx,ny,nz)=sourcep(nt,nb,nx,ny,
     +                  nz)+source(nt,nb,i,j,nz)
                        nval=nval+1.
                      endif  
                    enddo
                  enddo 
                  sourcep(nt,nb,nx,ny,nz)=sourcep(nt,nb,nx,ny,nz)/nval
                enddo
              enddo
            enddo
          enddo
        enddo 
        do nt=1,ntype
          do nb=1,nbns
            do nz=1,nsrcz
              do nx=1,ncellx
                do ny=1,ncelly
                  source(nt,nb,nx,ny,nz)=sourcep(nt,nb,nx,ny,nz)
                enddo
              enddo
            enddo
          enddo
        enddo               
      return
      stop
      end           
c
c ---------------------------------------------------------------------
c
c  Routine d ecriture du masque de nuage 
c
      subroutine wrcloud(cloudmsk,ncellx,ncelly,xcell0,ycell0,
     +lcellx,t,hre,min,sec,jou,moi,ann)
        real lcellx,xcell0,ycell0
c
      integer nbins,nzlev,nxwid,nywid,ntyp
      parameter (nbins=12,nzlev=10,nxwid=80,nywid=160,ntyp=5)
c
        integer ncellx,ncelly,nx,ny,cloudmsk(nxwid,nywid),nm,nc,nd
        integer nu,t
        character*1 table(nzlev)
        character*4 tag
        character*60 name
        real*8 hre,min,sec,jou,moi,ann
        data table /'0','1','2','3','4','5','6','7','8','9'/
        nm=t/1000
        nc=(t-nm*1000)/100
        nd=(t-nm*1000-nc*100)/10
        nu=t-nm*1000-nc*100-nd*10
        nm=nm+1
        nc=nc+1
        nd=nd+1
        nu=nu+1
        tag=table(nm)//table(nc)//table(nd)//table(nu)
        name='cloud_'//tag//'.pgm'
        open(unit=27,file=name,status='unknown')
          write(27,171) 'P2'
          write(27,174) 
          write(27,180) '# date ',int(hre),int(min),int(sec),int(jou),
     +    int(moi),int(ann)
          write(27,179) '# pixsiz ',lcellx
          write(27,179) '# lat0   ',xcell0
          write(27,179) '# lon0   ',ycell0        
          write(27,172) ncelly, ncellx
          write(27,*) '2'
          do nx=ncellx,1,-1
            write(27,*) (cloudmsk(nx,ny),ny=1,ncelly)
          enddo
        close(unit=27)
 171    format(a)
 172    format(i6,1x,i6)
 174    format('# cloud mask from AODSEM (1=cloud)')
 179    format(A,F8.3)  
 180    format(A,I2,1x,i2,1x,i2,1x,i2,1x,i2,1x,i4)      
      return
      stop
      end 
c
c ===================================================
c
c    routine de calcul du coefficient de coagulation
c
      subroutine kcoagul(L,m1,m2,dp1,dp2,tmp,k12)
c    L=mean free path
c    alpha=efficacite de collision
c    viscos=viscosite de l'air
        real cm1,cm2,pi,kbolt,m1,m2,kn1,kn2,d1,d2,L,l1,l2
        real g1,g2,dp1,dp2,k12,tmp,alpha,viscos
        pi=3.1415926
        alpha=1.
        kbolt=1.381e-23
        viscos=0.01789E-3
        cm1=(8.*kbolt*tmp/pi/m1)**0.5
        cm2=(8.*kbolt*tmp/pi/m2)**0.5
        kn1=2.*L/dp1
        kn2=2.*L/dp2
        D1=kbolt*tmp*(5.+4.*kn1+6.*kn1**2.+18.*kn1**3.)/(3.*pi*viscos*
     +  dp1*(5.-kn1+(8.+pi)*kn1**2.))
        D2=kbolt*tmp*(5.+4.*kn2+6.*kn2**2.+18.*kn2**3.)/(3.*pi*viscos*
     +  dp2*(5.-kn2+(8.+pi)*kn2**2.))
        l1=8.*D1/pi/cm1
        l2=8.*D2/pi/cm2
        g1=((dp1+l1)**3.-(dp1**2.+l1**2.)**1.5)/3./dp1/l1-dp1
        g2=((dp2+l2)**3.-(dp2**2.+l2**2.)**1.5)/3./dp2/l2-dp2
        k12=2.*pi*(D1+D2)*(dp1+dp2)/((dp1+dp2)/(dp1+dp2+2.*(g1**2.+
     +  g2**2.)**0.5)+8.*alpha*(D1+D2)/((dp1+dp2)*(cm1**2.+
     +  cm2**2.)**0.5))
        return
        stop
        end
c
c ====================================================
c
c   routine pour trouver le numero de RH
c
        subroutine detnrh(relhum,nrh)
        integer relhum,nrh
                    if ((relhum.ge.0).and.(relhum.lt.10)) 
     +              then
                      nrh=1
                    elseif ((relhum.ge.10).and.(relhum.lt.
     +              20)) then
                      nrh=2
                    elseif ((relhum.ge.20).and.(relhum.lt.
     +              30)) then    
                      nrh=3
                    elseif ((relhum.ge.30).and.(relhum.lt.
     +              40)) then
                      nrh=4
                    elseif ((relhum.ge.40).and.(relhum.lt.
     +              50)) then
                      nrh=5
                    elseif ((relhum.ge.50).and.(relhum.lt.
     +              60)) then
                      nrh=6
                    elseif ((relhum.ge.60).and.(relhum.lt.
     +              70)) then
                      nrh=7
                    elseif ((relhum.ge.70).and.(relhum.lt.
     +              80)) then
                      nrh=8
                    elseif ((relhum.ge.80).and.(relhum.lt.
     +              90)) then
                      nrh=9
                    elseif ((relhum.ge.90).and.(relhum.lt.
     +              95)) then
                      nrh=10
                    elseif ((relhum.ge.95).and.(relhum.lt.
     +              98)) then
                      nrh=11
                    elseif ((relhum.ge.98).and.(relhum.lt.
     +              99)) then
                      nrh=12
                    elseif (relhum.ge.99) then
                      nrh=13
                    endif
         return
         stop
         end
c
c==========================
c
c     routine de lecture du buffer
c 
c >>>>>>>>>>>
c
c  loading data to fill the buffer zone
      subroutine readbuf(buffile,nbns,ntype,ncellx,ncelly,ncellz,
     +  lcellx,lcelly,lcellz,xcell0,ycell0,numtemp)
c
      integer nbins,nzlev,nxwid,nywid,ntyp
      parameter (nbins=12,nzlev=10,nxwid=80,nywid=160,ntyp=5)
c
c
c  Declaration des variables
c
        integer ival,nbns,nb,ncellx,ncelly,ncellz,ntype,nr,i,j,k
        real rval,lcellx,lcelly,numtemp(5,12,nxwid,nywid,nzlev)
        real latitu,dx,dy,gain,lcellz(nzlev)
        character*60 buffile,bidon
c  
c  Lecture des donnees de distribution des aerosols 
c  les donnees sont en unites de (gain) part/m^3
c
      open(unit=20,file=buffile,status='old',err=2314)
        print*,'Reading buffer file: ',buffile
        read(20,*) bidon
        read(20,*) bidon,bidon,gain
        read(20,*) 
        read(20,*) ival
        if (ival.ne.nbns) goto 2323
c
c  lecture des bins secs
c
        do nb=1,nbns
          read(20,*) 
        enddo
c
c  lecture des donnees concernant la geometrie de la grille du 
c  modele
c
        read(20,*) ival
        if (ival.ne.ncellx) goto 2324
        read(20,*) rval
        if (rval.ne.lcellx) goto 2325
        read(20,*) ival
        if (ival.ne.ncelly) goto 2324
        read(20,*) rval
        if (rval.ne.lcelly) goto 2325
        read(20,*) ival
        if (ival.ne.ncellz) goto 2326
        do k=1,ncellz
          read(20,*) 
        enddo
        read(20,*) 
c
c  lecture des types d aerosols presents 
c
        read(20,*) ival
        if (ival.ne.ntype) goto 2327
        do nr=1,ntype
          read(20,*) 
        enddo
        read(20,*) bidon
        do nr=1,ntype
          do nb=1,nbns
            do k=1,ncellz
              read(20,*) bidon
              read(20,*) ((numtemp(nr,nb,i,j,k),j=1,ncelly),
     +        i=ncellx,1,-1)
            enddo
          enddo
        enddo
      close(unit=20)
c
c  conversion en unite de part/m^3
c
c  converting number density into total number of particles by
c  cell et creation de la matrice tampon numtemp qui sera utilisee
c  en input aux frontieres du domaine
c
      do nt=1,ntype
        do nb=1,nbns
          do i=1,ncellx
            latitu=xcell0+lcellx*real(i-1)
            call echelle(latitu,lcellx,lcelly,dx,dy)
            do j=1,ncelly
              do k=1,ncellz
                numtemp(nt,nb,i,j,k)=numtemp(nt,nb,i,j,k)*gain
                numtemp(nt,nb,i,j,k)=numtemp(nt,nb,i,j,k)*dx*dy*
     +          lcellz(k)
              enddo
            enddo
          enddo
        enddo
      enddo
      return
      stop
 2314   print*,'File ',buffile,' not found! '
      stop
 2323 print*,'Nb of size bins in buffer file don t match run prop.'
      stop
 2324 print*,'Nb of cells in buffer file don t match run properties'
      stop
 2325 print*,'Cell size in bufer file don t match run properties'
      stop
 2326 print*,'Nb of vert. cells in buffer file don t match run prop.'
      stop
 2327 print*,'Nb of aerosol types in buffer file don t match run prop.'
      stop
         end
      
