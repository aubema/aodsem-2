c     Programme pour produire les fichiers d emission
c     sur la periode et le domaine de l experience
c     il y a production d un fichier par jour
c
c     les parametres microphysiques utilises dans ce programme
c     proviennent du draft de la documentation du 
c     canadian aerosol model (CAM)
c     Gong, Barrie et al. 
c
c
c   
c ---------------------------------------------------------------------
c
      program makesource
c
c     Declaration des variables
c
      integer nbins,nzlev,nxwid,nywid,ntyp,boxw 
      real gain
      character tflag
      parameter (nbins=12,nzlev=10,nxwid=125,nywid=275,ntyp=5)     
c     ntyp=nombre de type d aerosols
c     nbins=nombre d intervalles de taille
c     nzlev=nombre de niveaux verticaux
c     nxwid=nombre max de cellules N-S
c     nywid=nombre max de cellules E-O 
c     gain=unites de densite numeriques
c     boxw=taille de la matrice de lissage de asimil, boxw doit etre impair
c 
      real psd,psdtot
      real volume(nbins),rayon1(nbins),hb,ha,alt
      real rayon2(nbins),masse(nbins,ntyp),zgrid
      real longit,latitu,pi,dx,dy,lcellz(nzlev)
      real numvol(ntyp,nbins,nxwid,nywid,nzlev),lcellx,lcelly,BB
      real ncelxrh,lcelxrh,ncelyrh,lcelyrh,ncelzrh,xrh0,yrh0,zrh0
      real vxwind(181,360),vywind(181,360)
      real diamet(nbins),dr,rm,rp,surface,surfgeia,lx,ly
      real radrat(8,13,nbins),finden(8,13,nbins)
      real vitesse,ABC,ASU,rpsd,spsd,drp,ASD,ASS,AOC
      real*8 jday,rhjj(1000),wsjj(1000),time,deltat,wejj(1000)
      real*8 hre,min,sec,jou,moi,ann,ftime,dtmin,itime
      real*8 heuref,minutef,secondef,jourf,moisf,anneef
      real*8 heure0,minute0,seconde0,jour0,mois0,annee0  
      real*8 hh1,hh2,dd1,dd2,mm1,mm2,aaaa1,aaaa2,lat,lon   
      integer lenom1,lenom2,nwspfi,nrhfi,nrhf,ncellx,ncelly,ncellz
      integer ntype,nx,ny,nz,nt,nb,nh,nhum,nbnhum,lenbflag(nbins)
      integer i,j,k,ii,jj,kk,tint,t,nrh,nn,nl,nnb,nnt,nfire
      integer ncroi,nc,ncr(8),nzmax,nfile,nzsrc,seasw
      integer ni,nj,nk,ia,ib,ic,id,ie,nf,nda,ndata
      integer mois1(50),mois2(50),seamsk(180,360),seam
      character*60 nomws(1000),wsfile,nomfile(1000)
      character*60 nomfi
      character*60 bidon,nomtype(ntyp),croiname(8)
      character*20 binflag(nbins)
      integer nm,nce,nd,nu
      character*1 table(10)
      character*2 mois,jour,heure,minute
      character*4 annee
      character*60 wsfile2
      real*8 ordon,pente,dtminm,wstim,wstim2,limtim1,limtim2
      real*8 virtjday
      real vxwind1(181,360),vywind1(181,360)
      real vxwind2(181,360),vywind2(181,360),ycell0,xcell0
      real su,bc,sd,ss,om
c
c     Initialisation des variables
c
c     limites des intervalles de taille
c
      data rayon1/0.005, 0.01,0.02,0.04,0.08,0.16,0.32,0.64,1.28,2.56,
     +5.12,10.24/
      data rayon2/0.01,0.02,0.04,0.08,0.16,0.32,0.64,1.28,2.56,5.12,
     +10.24,20.48/ 
      ncellz=10  
c     largeur de chaque couche verticale
      data lcellz/165.,180.,197.,218.,246.,
     +280.,325.,389.,10000.,18000./
c     types d aerosols
      data nomtype/'SULFATE','BLACKCARBON','SOILDUST','SEASALT',
     +'OMCARBON'/ 
      data table /'0','1','2','3','4','5','6','7','8','9'/
c
c     units of num vol for archiving
c
      gain=1.e+4
c
c     pi
c
      pi=3.1415926  
      nzmax=1
      do i=1,nxwid
        do j=1,nywid
          do k=1,10
            do nt=1,ntyp
              do nb=1,nbins
                numvol(nt,nb,i,j,k)=0.
              enddo
            enddo
          enddo
        enddo
      enddo 
c
c     reading parameter file
c
      open(unit=25,file='makesource.par',status='old',err=21)
        read(25,*) heure0,minute0,seconde0,jour0,mois0,annee0
        read(25,*) heuref,minutef,secondef,jourf,moisf,anneef
c   
        read(25,*) ncellx
        read(25,*) ncelly
        read(25,*) lcellx
        lcelly=lcellx
        read(25,*) xcell0
        read(25,*) ycell0
        if (ycell0.lt.0.) ycell0=360.+ycell0
        read(25,*) nfile
        if (nfile.ne.0) then
        do i=1,nfile
          read(25,*) nomfile(i)
        enddo
        endif
        read(25,*) seasw
      close(unit=25)
c
c     calcul des jours juliens du debut et fin du run
c            
      call julian(heure0,minute0,seconde0,jour0,mois0,annee0,jday) 
      itime=jday  
      call julian(heuref,minutef,secondef,jourf,moisf,anneef,jday) 
      ftime=jday                    
      timeint=ftime-itime
c
c     intervalle de temps de 1 jour
c


c      deltat=1.
      deltat=0.25


c
c     determine the number of time step


cc     1 map per day
c     4 maps per day


c
      tint=nint(timeint)
c     si l'intervalle n'est pas d au moins 1 jour
      if (tint.eq.0) goto 321



cc     fixer le temps initial a 12 h TU
c      time=dble(int(itime))+0.5
c     fixer le temps initial a 03 h TU
      time=dble(int(itime))+0.125




c
c     Lecture des fichiers index qui repertorient
c     les fichiers de vents, d humidite relative et
c     des inventaires d emission disponibles.
c
c
      open(unit=5,file="wspeed.index",status='old',err=6)
        print*,'Reading wind speed file index...' 
        read(5,*) nwspfi
        do 100 nws=1,nwspfi
          read(5,*) hre,min,sec,jou,moi,ann,nomws(nws)
          call julian(hre,min,sec,jou,moi,ann,jday)
          wsjj(nws)=jday
 100    continue
      close(unit=5)
c
c     lecture du fichier de croissance hygrometrique (humidite)
c     and associate values to each particle type
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
              read(18,*) bidon,radrat(nc,nh,nb),finden(nc,nh,nb)
 280        continue
 270      continue
 260    continue
        do 290 nt=1,ntyp
          do 300 nc=1,ncroi
            if (nomtype(nt)(1:5).eq.croiname(nc)(1:5)) then
              ncr(nt)=nc
            endif  
 300      continue
 290    continue
      CLOSE(UNIT=18)
c
c     calcul du volume moyen sec d une particule du bin nb en m^3
c     (1e-18 est le facteur de conversion de micron^3 a m^3
c
      print*,'Computing dry mean volume and diameter...'
      do 330 nt=1,ntyp
        do 340 nb=1,nbins
          volume(nb)=4.*pi*1.e-18*((rayon1(nb)+rayon2(nb))/2.)**3./3.
          diamet(nb)=rayon1(nb)+rayon2(nb)
 340    continue
 330  continue   
c
c     determine the particle mass RH=0
c
      do nnb=1,nbins
        do nnt=1,ntyp
          masse(nnb,nnt)=finden(ncr(nnt),1,nnb)*
     +    volume(nnb)*radrat(ncr(nnt),1,nnb)**3.
        enddo
      enddo  
c
c     Lecture du masque oceanique fichier land-sea.pbm
c 
      open(unit=18,file='land-sea.pbm',status='old',err=8) 
        read(18,*)
        read(18,*)
        read(18,*) ((seamsk(i,j),j=1,360),i=180,1,-1)
      close(unit=18)       
c
c     debut de la boucle temporelle
c
c
      open(unit=4,file='source.index',status='unknown')



c      write(4,*) tint+1,' number of files'
      write(4,*) 4* tint+1,' number of files'


      print*,'Computing AODSEM inventories...'
      do 2000 t=1,4*tint+1
c
c
c       determiner le nom du fichier en fonction de la date
c
        call timedate(hre,min,sec,jou,moi,ann,time) 
        nm=int(ann/1000.)
        nce=int((ann-dble(nm)*1000.)/100.)
        nd=int((ann-dble(nm)*1000.-dble(nce)*100.)/10. )
        nu=int(ann-dble(nm)*1000.-dble(nce)*100.-dble(nd)*10.)
        nm=nm+1
        nce=nce+1
        nd=nd+1
        nu=nu+1
        annee=table(nm)//table(nce)//table(nd)//table(nu)
        nd=int(moi/10.)
        nu=int(moi-dble(nd)*10.)
        nd=nd+1
        nu=nu+1
        mois=table(nd)//table(nu)
        nd=int(jou/10.)
        nu=int(jou-dble(nd)*10.)
        nd=nd+1
        nu=nu+1
        jour=table(nd)//table(nu)
        nd=int(hre/10.)
        nu=int(hre-dble(nd)*10.)
        nd=nd+1
        nu=nu+1
        heure=table(nd)//table(nu)
        nd=int(min/10.)
        nu=int(min-dble(nd)*10.)
        nd=nd+1
        nu=nu+1
        minute=table(nd)//table(nu)



c        nomfi=annee//mois//jour//'_src.dat'
        nomfi=annee//mois//jour//heure//'_src.dat' 


c       Reading the near surface wind velocity file nearest to 
c       the time and converting onto the aerosol dist grid
c
c       Determine the right file
c
        if (seasw.eq.1) then
          dtmin=10000000.
          do nws=1,nwspfi
            if (abs(time-wsjj(nws)).lt.dtmin) then
              dtmin=abs(time-wsjj(nws))
              wstim=wsjj(nws)
              wsfile=nomws(nws)
            endif
          enddo
          dtminm=dtmin
          dtmin=10000000.
          do nws=1,nwspfi
            if ((abs(time-wsjj(nws)).lt.dtmin).and.(abs(time-
     +      wsjj(nws)).gt.dtminm)) then
              dtmin=abs(time-wsjj(nws))
              wstim2=wsjj(nws)
              wsfile2=nomws(nws)
            endif
          enddo
          call readws(wsfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     +    lcellz,xcell0,ycell0,vxwind1,vywind1)
          call readws(wsfile2,ncellx,ncelly,ncellz,lcellx,lcelly,
     +    lcellz,xcell0,ycell0,vxwind2,vywind2)
c
c         interpoler la valeur de wspeed
c
          do ii=1,ncellx
            do jj=1,ncelly
              pente=(dble(vxwind2(ii,jj)-
     +        vxwind1(ii,jj)))/(wstim2-wstim)
              ordon=dble(vxwind2(ii,jj))-pente
     +        *wstim2
              vxwind(ii,jj)=real(pente*time+ordon)
              pente=(dble(vywind2(ii,jj)-
     +        vywind1(ii,jj)))/(wstim2-wstim)
              ordon=dble(vywind2(ii,jj))-pente
     +        *wstim2
              vywind(ii,jj)=real(pente*time+ordon)
            enddo
          enddo
        endif


ccccccccccccccccccccccccccccc
c       lecture des donnees sources
c
c       le logiciel supporte uniquement le format .3ds et les unites de
c       flux sont des tonne/an considere emise a la position lat lon nominale
c       donnee sur la ligne du fichier 3ds en bout de ligne ces emissions sont 
c       compilees sur chaque cellule de 1 deg², soit la resolution choisie 
c       pour les emissions du modele.
c       une annee 1980 indique qu'il s'agit d'une climatologie
c

        do nf=1,nfile
          open(unit=1,file=nomfile(nf),status='old')
            print*,'Reading ',nomfile(nf)
            read(1,*) ndata, tflag
            do nda=1,ndata


            if (tflag.eq.'d') then
c daily file
              read(1,*) dd1,mm1,aaaa1,dd2,mm2,aaaa2,lat,lon,alt,su,bc,
     +        sd,ss,om
              hh1=0.
              hh2=23.
            endif
            if (tflag.eq.'h') then
c hourly file
               read(1,*) hh1,dd1,mm1,aaaa1,hh2,dd2,mm2,aaaa2,lat,lon,
     +        alt,su,bc,sd,ss,om
c           if (om.ne.0.) then
c           print*,om,'stop'
c           stop
c           endif
              hh2=hh2-1
            endif


              if (lon.lt.0.) lon=360.+lon
c
c             conversion des unites tonne/deg^2/an vers kg/deg^2/jour
c
              su=su*1000./365.
              bc=bc*1000./365.
              sd=sd*1000./365.
              ss=ss*1000./365.
              om=om*1000./365.
c
c




c              call julian(dble(0.),dble(0.),dble(0.),dd1,mm1,aaaa1,jday)   
c              limtim1=jday  
c              call julian(dble(23.),dble(59.),dble(59.9),dd2,mm2,
c     +        aaaa2,jday)   
c              limtim2=jday
              call julian(hh1,dble(0.),dble(0.),dd1,mm1,aaaa1,jday)   
              limtim1=jday  
              call julian(hh2,dble(59.),dble(59.999999999999),dd2,mm2,
     +        aaaa2,jday)   
              limtim2=jday





              call julian(hre,min,sec,jou,moi,
     +        dble(1980.),jday)
              virtjday=jday   
              if (((time.ge.limtim1).and.(time.le.limtim2).and.
     +        (virtjday.lt.limtim2).and.(aaaa1.gt.1980.001)).or.
     +        ((virtjday.gt.limtim1).and.(aaaa1.eq.1980.))) then 

c
c               determination du niveau vertical nzsrc et recherche du niveau le plus haut
c               nzmax
c
                hb=0.
                ha=0.
                do nz=1,nzlev 
                  ha=hb
                  hb=lcellz(nz)+hb
                  if ((hb.ge.alt).and.(ha.lt.alt)) then
                    nzsrc=nz
                    if (nzsrc.gt.nzmax) nzmax=nzsrc
                  endif
                enddo 
                nz=nzsrc
c
c               Boucle sur la matrice numvol horizontale de sortie
c      
                do nx=1,ncellx

                  latitu=xcell0+lcellx*real(nx-1)
                  if (nint(latitu).eq.nint(lat)) then
c
c                 surface de la cellule AODSEM (m^2)
c
                  call echelle(latitu,lcellx,lcelly,dx,dy)             
                  surface=dx*dy
c
c                 surface de la cellule un degre (m^2)
c 
                  lx=1.
                  ly=1.
                  call echelle(latitu,lx,ly,dx,dy)             
                  surfgeia=dx*dy
                  do ny=1,ncelly
                    longit=ycell0+lcelly*real(ny-1)

                    if (nint(longit).eq.nint(lon)) then
c
c #################
c                     Sulfate case
c #################
c
                    if (su.gt.0.000001) then
                      ASU=su*surface/surfgeia
                      rpsd=0.1
                      spsd=1.5
                      psdtot=0.
                      nt=1
                      do ii=1,nbins
                        drp=rayon2(ii)-rayon1(ii)
                        rp=(rayon2(ii)+rayon1(ii))/2.
                        psdtot=psdtot+masse(ii,nt)*(exp(-1.*(log(rp)-
     +                  log(rpsd))**2./(2.*(log(spsd))**2.)))/((2.
     +                  *pi)**0.5*log(spsd))*drp 
                      enddo       
                    do nb=1,nbins
                      dr=rayon2(nb)-rayon1(nb)
                      rm=(rayon2(nb)+rayon1(nb))/2.
                      psd=masse(nb,nt)*exp(-1.*(log(rm)-log(rpsd))**
     +                2./(2.*(log(spsd))**2.))/((2.*pi)**0.5*   
     +                log(spsd))*dr/psdtot
                      numvol(nt,nb,nx,ny,nz)=ASU*psd/masse(nb,nt)+
     +                numvol(nt,nb,nx,ny,nz)
c
c                     les nouvelles unites deviennent part./cellule/jour/bin
c                     dans la routine d'impression ces unites seront converties
c                     en part./cellule/bin/m^3/jour
c
                    enddo
                    endif
c
c ################
c                     Black carbon case
c#################
c
                    if (bc.gt.0.000001) then
                      ABC=bc*surface/surfgeia
                      rpsd=0.05
                      spsd=1.7
                      psdtot=0.
                      nt=2
                      do ii=1,nbins
                        drp=rayon2(ii)-rayon1(ii)
                        rp=(rayon2(ii)+rayon1(ii))/2.
                        psdtot=psdtot+masse(ii,nt)*(exp(-1.*(log(rp)
     +                  -log(rpsd))**2./(2.*(log(spsd))**2.)))/((2.* 
     +                  pi)**0.5*log(spsd))*drp
                      enddo 
                    do nb=1,nbins

                      dr=rayon2(nb)-rayon1(nb)     
                      rm=(rayon2(nb)+rayon1(nb))/2.
          
                      psd=masse(nb,nt)*exp(-1.*(log(rm)-log(rpsd))
     +                **2./(2.*(log(spsd))**2.))/((2.*pi)**0.5*
     +                log(spsd))*dr/psdtot         
                      numvol(nt,nb,nx,ny,nz)=ABC*psd/masse(nb,nt)+
     +                numvol(nt,nb,nx,ny,nz)
                    enddo 
                    endif
c
c ################
c                     Soil dust
c#################
c                     il faudrait faire une recherche pour determiner rpsd et spsd
c
                    if (sd.gt.0.000001) then
                      ASD=sd*surface/surfgeia
                      rpsd=0.05
                      spsd=1.7
                      psdtot=0.
                      nt=3
                      do ii=1,nbins
                        drp=rayon2(ii)-rayon1(ii)
                        rp=(rayon2(ii)+rayon1(ii))/2.
                        psdtot=psdtot+masse(ii,nt)*(exp(-1.*(log(rp)
     +                  -log(rpsd))**2./(2.*(log(spsd))**2.)))/((2.* 
     +                  pi)**0.5*log(spsd))*drp
                      enddo
                    do nb=1,nbins

                      dr=rayon2(nb)-rayon1(nb)     
                      rm=(rayon2(nb)+rayon1(nb))/2.
           
                      psd=masse(nb,nt)*exp(-1.*(log(rm)-log(rpsd))
     +                **2./(2.*(log(spsd))**2.))/((2.*pi)**0.5*
     +                log(spsd))*dr/psdtot         
                      numvol(nt,nb,nx,ny,nz)=ASD*psd/masse(nb,nt)+
     +                numvol(nt,nb,nx,ny,nz) 
                    enddo 
                    endif
c
c ################
c                     Sea salt
c#################
c                     il faudrait faire une recherche pour determiner rpsd et spsd
c
                    if (ss.gt.0.000001) then
                      ASS=ss*surface/surfgeia
                      rpsd=0.05
                      spsd=1.7
                      psdtot=0.
                      nt=4
                      do ii=1,nbins
                        drp=rayon2(ii)-rayon1(ii)
                        rp=(rayon2(ii)+rayon1(ii))/2.
                        psdtot=psdtot+masse(ii,nt)*(exp(-1.*(log(rp)
     +                  -log(rpsd))**2./(2.*(log(spsd))**2.)))/((2.* 
     +                  pi)**0.5*log(spsd))*drp
                      enddo  
                    do nb=1,nbins

                      dr=rayon2(nb)-rayon1(nb)     
                      rm=(rayon2(nb)+rayon1(nb))/2.
         
                      psd=masse(nb,nt)*exp(-1.*(log(rm)-log(rpsd))
     +                **2./(2.*(log(spsd))**2.))/((2.*pi)**0.5*
     +                log(spsd))*dr/psdtot         
                      numvol(nt,nb,nx,ny,nz)=ASS*psd/masse(nb,nt)+
     +                numvol(nt,nb,nx,ny,nz) 
                    enddo
                    endif
c
c ####################
c                     Organic carbon
c ####################
c                     pour l'instant la distribution de taille est prise egale
c                     a celle des sulfates.  Il faudrait trouver une reference
c                     plus fiable a cet effet
c
                    if (om.gt.0.000001) then

                      AOC=om*surface/surfgeia
                      rpsd=0.1
                      spsd=1.5
                      psdtot=0.
                      nt=5
                      do ii=1,nbins
                        drp=rayon2(ii)-rayon1(ii)
                        rp=(rayon2(ii)+rayon1(ii))/2.
                        psdtot=psdtot+masse(ii,nt)*(exp(-1.*(log(rp)
     +                  -log(rpsd))**2./(2.*(log(spsd))**2.)))/((2.* 
     +                  pi)**0.5*log(spsd))*drp
                      enddo       
                    do nb=1,nbins
                      dr=rayon2(nb)-rayon1(nb)     
                      rm=(rayon2(nb)+rayon1(nb))/2.
    
                      psd=masse(nb,nt)*exp(-1.*(log(rm)-log(rpsd))
     +                **2./(2.*(log(spsd))**2.))/((2.*pi)**0.5*
     +                log(spsd))*dr/psdtot         
                      numvol(nt,nb,nx,ny,nz)=AOC*psd/masse(nb,nt)+
     +                numvol(nt,nb,nx,ny,nz) 
                    enddo 
                    endif
                    endif 
                  enddo
                  endif
                enddo
              endif
            enddo
          close(unit=1)
        enddo 

c ####################################################
c
c       section des sources dynamiques
c
c       Sea salt case dynamic pour le premier niveau vertical
c ####################
c
c       position dans le 
c       masque maritime ayant une resolution de 1x1 deg
c 
        if (seasw.eq.1) then
          nz=1
          nt=4
          do nx=1,ncellx
            latitu=xcell0+lcellx*real(nx-1)
c
c           surface de la cellule AODSEM (m^2)
c
            call echelle(latitu,lcellx,lcelly,dx,dy)             
            surface=dx*dy
c
c           surface de la cellule un degre (m^2)
c 
            lx=1.
            ly=1.
            call echelle(latitu,lx,ly,dx,dy)             
            surfgeia=dx*dy
            do ny=1,ncelly
              longit=ycell0+lcelly*real(ny-1)
              do nb=1,nbins       

                if (nz.eq.1) then    
                  i=int(xcell0+real(nx-1)*lcellx)
                  i=91+i
                  j=int(ycell0+real(ny-1)*lcellx)
                  if (j.gt.360) j=j-360
                  if (j.lt.0.) j=j+360
                  j=j+181
                  if (j.gt.360) j=j-360
c
c                 Selon Monahan et al 1986
c      determine du vent au sol et de la taille humide de la particule (RH=80%)
c  
                  seam=seamsk(i,j)
                  if (seam.eq.1) then         
                    vitesse=(vxwind(nx,ny)**2.+vywind(nx,ny)**2.)**.5
                    dr=(rayon2(nb)-rayon1(nb))*radrat(ncr(4),9,nb)
                    rm=(rayon2(nb)+rayon1(nb))*radrat(ncr(4),9,nb)/2.
                    BB=(0.380-log10(rm))/0.650 
                    numvol(nt,nb,nx,ny,nz)=1.373*vitesse**3.41/
     +              rm**3.*(1+0.057*rm**1.05)*10**(1.19*exp(-BB**
     +              2.))*dr*deltat*24.*3600.*surface+
     +              numvol(nt,nb,nx,ny,nz)
                  endif
                endif
              enddo
            enddo
          enddo
        endif                      
c
c       Ecrire les fichier d emission
c
        print*,'Writing output for julian day:',time
        call wrnumvol(numvol,ncellx,lcellx,ncelly,lcelly,nzmax,
     +  lcellz,xcell0,ycell0,zcell0,nbins,rayon1,rayon2,ntyp,
     +  nomtype,gain,t,hre,min,
     +  sec,jou,moi,ann,nomfi) 
c
c       ecriture du fichier source.index
c                  
        write(4,*) hre,min,sec,jou,moi,ann,nomfi
c
c       incremente le temps
c
        time=time+deltat             
 2000 continue 
      close(unit=4)
      print*,'Normal end of makesource.'
      stop
 6    print*,'File wspeed.index inexistant!'
      stop
 7    print*,'File relhum.index inexistant!'
      stop
 8    print*,'Can t find file land-sea.pbm!'
      stop
 9    print*,'Can t find file lowsu##.pgm!'
      stop
 10   print*,'Can t find file higsu##.pgm!'
      stop
 11   print*,'Can t find file lowbc##.pgm!'
      stop
 21   print*,'Can t find file makesource.par please run epar_mksrc!'
      stop
 321  print*,'Can t handle time intervals lower than 12 hours!'
      stop
      end
c =============================================================
c
c   Routine d ecriture des distributions
c
c
c
c        Ecriture du fichier d emissions 
c
c
         subroutine wrnumvol(numvo,ncellx,lcellx,ncelly,lcelly,
     +   ncellz,lcellz,xcell0,ycell0,zcell0,nbns,rayon1,rayon2,
     +   ntype,nomtype,gain,t,hre,min,
     +   sec,jou,moi,ann,filename)
c
c   declarations des variables
c
         integer ncellz,ncellx,ncelly,nbns,ntype,t,nt,nb,i,j,k
         real gain,numvo(5,12,125,275,10)
         real lcellx,lcelly,lcellz(10)
         real xcell0,ycell0,zcell0,rayon1(12),rayon2(12),latitu
         real dx,dy
         real*8 hre,min,sec,jou,moi,ann
         character*60 nomtype(5),filename
c
c   Convertir le nombre de particules en nombre volumique
c   i.e. les unites de numvo = part/m^3/jour/cellule
c
         do i=1,ncellx
            latitu=xcell0+lcellx*real(i-1)
            call echelle(latitu,lcellx,lcelly,dx,dy)

          do nt=1,ntype
           do nb=1,nbns            
            do j=1,ncelly
             do k=1,ncellz
                numvo(nt,nb,i,j,k)=numvo(nt,nb,i,j,k)/dx/dy/
     +          lcellz(k)
             enddo
            enddo        
           enddo                         
          enddo
         enddo 

c
c   Ecriture des donnees de distribution des aerosols .dat
c
         open(unit=1,file=filename,status='unknown')
         print*,'Writing aerosol emission file: ',filename
         write(1,*) '3-d aerosol emission file for AODSEM'
         write(1,*) 'Units of ',gain,' part/m^3/day'
         write(1,*) int(hre),int(min),int(sec),int(jou),int(moi),
     +   int(ann),' Starting Date (hh mm ss jj mm yyyy)'
         write(1,*) nbns,'  Number of size bins'
         do 1500 nb=1,nbns
          write(1,*) nb,rayon1(nb),rayon2(nb)
 1500    continue
         write(1,*) ncellx,'  Number of X cells'
         write(1,*) lcellx,'  Size of X cell'
         write(1,*) ncelly,'  Number of Y cells'
         write(1,*) lcelly,'  Size of Y cell'
         write(1,*) ncellz,'  Number of Z cells'
         do 1510 k=1,ncellz
          write(1,*) lcellz(k)
 1510    continue
         write(1,*) xcell0, ycell0, zcell0,'  X0, Y0, Z0'
         write(1,*) ntype,'  Number of aerosol types'
         do 1520 nr=1,ntype
          write(1,*) nomtype(nr)
 1520    continue
         write(1,*) 'DATA'
         do 1530 nt=1,ntype
          do 1540 nb=1,nbns
           do 1550 k=1,ncellz
            write(1,2625) nomtype(nt),nb,k
	    write(1,*) ((nint(numvo(nt,nb,i,j,k)/gain)
     +      ,j=1,ncelly),i=ncellx,1,-1)
 1550      continue
 1540     continue
 1530    continue
 2625    format('# AEROSOL TYPE: ',A11,' SIZE BIN: ',I2,
     +   ' VERTICAL LEVEL: ',I2)
         close(unit=1)
         return
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
c
c -------------------------------------------------------
c
c   Routine de lecture du fichier de vents
c
       subroutine readws(wsfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     + lcellz,xcell0,ycell0,vxwind,vywind)
       integer ncelxrh,ncelyrh,ncelzrh,ii,jj,kk,i,j,k,ncellx
       integer ncelly,ncellz,ax,up
       real lcelxrh,lcelyrh,lcelzrh(30),xrh0,yrh0,zrh0,zrh
       real lcellz(10),lcellx,lcelly,xcell0,ycell0
       real vxwind(181,360),vywind(181,360)
       real ws(3,181,360,30),zgrid 
       character*60 wsfile
       print*,'Reading wind speed file: ',wsfile(1:30)
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
 66                continue
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
 67                continue
                   vxwind(i,j)=ws(1,ii,jj,1)
                   vywind(i,j)=ws(2,ii,jj,1)
  64             continue
 63          continue
 62       continue
       close(unit=3)
       return
 14    print*,'File ',wsfile,' inexistant!'
       stop
       end

