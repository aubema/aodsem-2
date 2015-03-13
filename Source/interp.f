c    programme pour interpoler une carte d epaisseur optique
c    sous echantillonnee Õ partir de quelques mesures ponctuelles 
c    effectuees par des photometres solaires ou par inversion d images 
c    de teledetection.
c
c    L entree du programme est une carte pgm d epaisseur optique
c    qui peut etre incomplete.
c      
c    L interpolation est effectuee Õ l aide d une fonction poids 
c    ou statistiques soit:
c
c    nearest neighbour ... 0
c    linear .............. 1
c    2nd order ........... 2
c    mean ................ 3
c    minimum ............. 4
c    maximum ............. 5
c    
c    les axes sont definis comme suit
c    x = direction nord = degres
c    y = direction est = degres
c
c
c    copyright Martin Aube 29/10/1999
c
c -----------------
c   identification des variables 
c
c
c --------------------
c
c   programme principal
c
      program interp
c
c ----------
c
c   declaration des variables
c
      real pi2,pi,xcell0,ycell0,lcellx,lcelly,dmin,maxim,minim,npix
      real pixsiz,lattau0,lontau0,tau(2000,2000),tauout(2000,2000)
      real latitu,longit,distan,dtotal,factor,latcen,loncen,ndat
      real Vx,Vy,tau1,tau2,tau3,tau4,distan1,distan2,distan3,distan4
      real defval
      integer maxside,he,mi,se,jo,mo,an
      character*60 bidon,nom,aotfile,taufile,header,tag
      integer ncellx,ncelly,i,j,intype,autom,n1,n2,n3,n4
      integer percent,pcentold,nxcen,nycen,lennom,dens(10,2000,2000)
      integer nx,ny,nxtcel,nytcel,side,maxi,ngris,hcnt,theta
      integer xdata(4000000),ydata(4000000),nnd,nd
              

c   
c ----------
c
c   initialisation des variables
c
       pi=3.141592654
       pi2=pi*pi
       he=0
       mi=0
       se=0
       jo=1
       mo=1
       an=1980

c
c -----------
c
c   fichier de parametres
c
      open(unit=19,file="interp.par",status="old",err=12)
c
c   choix du nom de la racine de fichiers
c
c      print*,'root name of the files (.pgm will be add) ?'  
c      read*,nom
       read(19,*) nom
       read(19,*) pixsiz   
       read(19,*) lattau0      
       read(19,*) lontau0       
       read(19,*) factor       
       read(19,*) intype       
       read(19,*) autom      
       read(19,*) lcellx
       read(19,*) ncellx             
       read(19,*) ncelly             
       read(19,*) xcell0             
       read(19,*) ycell0  
       read(19,*) defval   
      close(unit=19)
        
      defval=defval/100.   
      if ((pixsiz.gt.lcellx).and.((intype.eq.1).or.
     + (intype.eq.2))) then    
         print*,'***Invalid resampling function.'
         print*,'   Output mesh is smaller than input mesh.'
         print*,'   Falling down to nearest neighbour.'
         intype=0
      endif       

c
c   calcul de la longueur du nom
c
      lennom=index(nom,' ')-1
c
c
c -----------
c
c   Lecture du fichier d'epaisseur optique
c
c   fichier pgm pour les images polder 254 et 255=aucun signification
c   253=lignes cotieres, autre=aod*100
c
         taufile=nom(1:lennom)//'.pgm'
         open(unit=2,file=taufile,status='old')
c
c   recherche de la position des headers
c
         bidon='#'
         hcnt=0
         read(2,*)
         do 54 i=1,50
            read(2,*,end=56) bidon,tag    
            if (bidon.eq.'#') then
              hcnt=hcnt+1
              if (tag(1:4).eq.'date') then
                 backspace 2
                 read(2,*) bidon,tag,he,mi,se,jo,mo,an
              endif
            endif
 54      continue            
 56      rewind 2
         read(2,*)
         do 55 i=1,hcnt
            read(2,*)
 55      continue
         read(2,*) nytcel, nxtcel, maxi 
         print*,'Reading AOD data...'
         read(2,*) ((tau(nx,ny),ny=1,nytcel),nx=nxtcel,1,-1)
         close(unit=2)
c
c   normaliser tau
c
         nnd=0
         do 555 nx=1,nxtcel
         do 556 ny=1,nytcel
            tau(nx,ny)=tau(nx,ny)/100.
            if (tau(nx,ny).lt.2.53) then
              nnd=nnd+1
              xdata(nnd)=nx
              ydata(nnd)=ny
            endif
 556     continue
 555     continue
c
c   calcul de la taille maximale d interpolation
c    
      if (nxtcel.gt.nytcel) then
         maxside=2*nxtcel+1
      else
         maxside=2*nytcel+1
      endif   
c         print*,'Pixel size in degrees?'
c         read*,pixsiz
c         print*,'Center latitude of the south-west pixel in degrees?'
c         read*,lattau0
c         print*,'Center longitude of the south-west pixel in degrees?'
c         read*,lontau0
         if (lontau0.lt.0.) lontau0=lontau0+360.
c         print*,'Interpolating window size in degrees?'
c         read*,factor
c 2121    print*,'Choose an interpolating function:'
c         print*,'    nearest neighbour ... 0'
c         print*,'    linear .............. 1'
c         print*,'    cubic ............... 2'
c         print*,'    mean ................ 3'
c         print*,'    minimum ............. 4'
c         print*,'    maximum ............. 5'
c         read*,intype
c         if ((intype.lt.0).or.(intype.gt.5)) goto 2121
c         print*,'Activate interpolating box size optimization?'
c         print*,'(no=0, yes=1)'
c         read*,autom
c        print*,' '
c        print*,' '
c        print*,'---------------------------------'
c        print*,'     Output file properties.'
c        print*,' '
c        print*,'Size of each cell in degrees?'
c        read*,lcellx
        lcelly=lcellx
c        print*,'Number of cells along N-S axis (sugg.',
c     +  int(nxtcel*pixsiz/lcellx),') ?'
c        read*,ncellx
c        print*,'Number of cells along E-W axis (sugg.',
c     +  int(nytcel*pixsiz/lcellx),') ?'
c        read*,ncelly
c        write(*,666) 'Center latitude of the south-west cell (sugg.',
c     +  (lattau0+pixsiz*(lcellx/pixsiz-1.)/2.),') ?'
c        read*,xcell0
c        write(*,666) 'Center longitude of the south-west cell (sugg.',
c     +  (lontau0+pixsiz*(lcelly/pixsiz-1.)/2.),') ?'
c        read*,ycell0
        if (ycell0.lt.0.) ycell0=ycell0+360.
 666    format(A,1x,F6.2,1x,A)
c
c    Calcul de la matrice de voisinage 
c
c    definition des orientations de theta
c               
c    4 3 2   
c    5 X 1
c    6 7 8
c 
c    le dixieme element est le nombre de voisin pour l instant, seul
c    cette valeur est utilisee
c             
              do i=1,nxtcel
              do j=1,nytcel
                do n=1,10
                   dens(n,i,j)=0
                enddo 
               if (tau(i,j).lt.2.53) dens(9,i,j)=1
            if (j.ne.ncelly) then 
               if (tau(i,j+1).lt.2.53) dens(1,i,j)=1
            elseif ((i.ne.ncellx).and.(j.ne.ncelly)) then
               if (tau(i+1,j+1).lt.2.53) dens(2,i,j)=1
            elseif (i.ne.ncelly) then
               if (tau(i+1,j).lt.2.53) dens(3,i,j)=1 
            elseif ((i.ne.ncellx).and.(j.ne.1)) then               
               if (tau(i+1,j-1).lt.2.53) dens(4,i,j)=1
            elseif (j.ne.1) then            
               if (tau(i,j-1).lt.2.53) dens(5,i,j)=1 
            elseif ((i.ne.1).and.(j.ne.1)) then 
               if (tau(i-1,j-1).lt.2.53) dens(6,i,j)=1 
            elseif (i.ne.1) then               
               if (tau(i-1,j).lt.2.53) dens(7,i,j)=1
            elseif ((i.ne.1).and.(j.ne.ncelly)) then                
                if (tau(i-1,j+1).lt.2.53) dens(8,i,j)=1  
            endif
                dens(10,i,j)=dens(1,i,j)+ dens(2,i,j)+dens(3,i,j)+ 
     +          dens(4,i,j)+dens(5,i,j)+dens(6,i,j)+dens(7,i,j)+
     +          dens(8,i,j)+dens(9,i,j)                                        
              enddo            
              enddo   
c
c    Taille de la fenetre d interpolation
c                         
 225    side=nint(factor/pixsiz/2.0001) 
       if (nnd.le.(2*side+1)**2) then
          print*,'Using all available date to perform interpolation'
       else 
          print*,'Using sliding box interpolation'
       endif
        if (side.gt.maxside) then
           print*,'   ERROR: Maximum interpolating width overpass'
           print*,'   ERROR: while resampling file: ',
     +     taufile(1:lennom+5)
           print*,'   ERROR: Must have 3 or more data!'
           stop
        endif
        print*,'Interpolation box size (in original pixel):',2*side+1      
c
c ---------
c
c   Interpoler la carte d epaisseur optique
c
        print*,'Interpolating aerosol map...'
        do 113 nx=1,ncellx
           do 114 ny=1,ncelly
              latitu=(real(nx)-1.)*lcellx+xcell0
              longit=(real(ny)-1.)*lcelly+ycell0
              dtotal=0.
              tauout(nx,ny)=0.
            nxcen=nint(((real(nx)-1.)*lcellx+xcell0-lattau0)/pixsiz+1.)
            nycen=nint(((real(ny)-1.)*lcelly+ycell0-lontau0)/pixsiz+1.)
              dmin=2000000.
              npix=0.
              minim=2000000.
              maxim=-2000000.
              ndat=0.
c
c    corriger pour la repartition inegale des donnees
c    
       if (nnd.le.(2*side+1)**2) then
c
c ce cas d'interpolation lorsque le nombre de donnees est inférieur 
c au nombre de pixel de la fenetre d'interpolation
c
             do 1161 nd=1,nnd
                  i=xdata(nd)
                  j=ydata(nd)

                 latcen=(real(i)-1.)*pixsiz+lattau0
                 loncen=(real(j)-1.)*pixsiz+lontau0
                 distan=sqrt((latitu-latcen)**2.+(longit-loncen)**2.)
                  
              if (distan.le.real(side)*pixsiz) then
                 if (distan.lt.0.01*pixsiz) distan=0.01*pixsiz
                 if ((i.gt.0).and.(i.le.nxtcel)) then
                    if ((j.gt.0).and.(j.le.nytcel)) then
                       if (tau(i,j).le.2.52) then
                          ndat=ndat+1.
                          if (intype.eq.0) then
                             if (distan.lt.dmin) then
                                dmin=distan
                                tauout(nx,ny)=tau(i,j)
                             endif                                        
                          elseif (intype.eq.1) then
                             tauout(nx,ny)=tau(i,j)/(distan*
     + (real(dens(10,i,j))))+tauout(nx,ny) 
                  dtotal=1./(distan*(real(dens(10,i,j))))
     + +dtotal  
                          elseif (intype.eq.2) then
                             tauout(nx,ny)=tau(i,j)/(distan**2.*
     + (real(dens(10,i,j))))+tauout(nx,ny)
                             dtotal=1./(distan**2.*(real(dens(10,i,j))
     + ))+dtotal
                          elseif (intype.eq.3) then
                             npix=npix+1.
                             tauout(nx,ny)=tau(i,j)+tauout(nx,ny)
                          elseif (intype.eq.4) then
                             if (tau(i,j).lt.minim) then
                                minim=tau(i,j)
                             endif
                          elseif (intype.eq.5) then
                             if (tau(i,j).gt.maxim) then
                                maxim=tau(i,j)
                             endif
                          endif
                       endif
                    endif
                 endif
              endif
 1161       continue
       else
c
c ce cas d'interpolation lorsque le nombre de donnees est supérieur 
c au nombre de pixel de la fenetre d'interpolation
c
              do 115 i=nxcen-side,nxcen+side
              do 116 j=nycen-side,nycen+side
                 latcen=(real(i)-1.)*pixsiz+lattau0
                 loncen=(real(j)-1.)*pixsiz+lontau0
                 distan=sqrt((latitu-latcen)**2.+(longit-loncen)**2.)
                  
              if (distan.le.real(side)*pixsiz) then
                 if (distan.lt.0.01*pixsiz) distan=0.01*pixsiz
                 if ((i.gt.0).and.(i.le.nxtcel)) then
                    if ((j.gt.0).and.(j.le.nytcel)) then
                       if (tau(i,j).le.2.52) then
                          ndat=ndat+1.
                          if (intype.eq.0) then
                             if (distan.lt.dmin) then
                                dmin=distan
                                tauout(nx,ny)=tau(i,j)
                             endif                                        
                          elseif (intype.eq.1) then
                             tauout(nx,ny)=tau(i,j)/(distan*
     + (real(dens(10,i,j))))+tauout(nx,ny) 
                  dtotal=1./(distan*(real(dens(10,i,j))))
     + +dtotal  
                          elseif (intype.eq.2) then
                             tauout(nx,ny)=tau(i,j)/(distan**2.*
     + (real(dens(10,i,j))))+tauout(nx,ny)
                             dtotal=1./(distan**2.*(real(dens(10,i,j))
     + ))+dtotal
                          elseif (intype.eq.3) then
                             npix=npix+1.
                             tauout(nx,ny)=tau(i,j)+tauout(nx,ny)
                          elseif (intype.eq.4) then
                             if (tau(i,j).lt.minim) then
                                minim=tau(i,j)
                             endif
                          elseif (intype.eq.5) then
                             if (tau(i,j).gt.maxim) then
                                maxim=tau(i,j)
                             endif
                          endif
                       endif
                    endif
                 endif
              endif
 116          continue
 115          continue
       endif



              if (dtotal.eq.0.) dtotal=0.000001   
              if ((intype.eq.1).or.(intype.eq.2)) then
                 tauout(nx,ny)=tauout(nx,ny)/dtotal   
                 if (ndat.lt.3.) then
                    tauout(nx,ny)=defval
                    if (autom.eq.1) then
c                      factor=factor+8.*pixsiz
                       factor=factor*2.
                       goto 225
                    endif
                 endif 
              elseif (intype.eq.3) then
                 if (npix.eq.0.) then
                    tauout(nx,ny)=defval
                    if (autom.eq.1) then
                       factor=factor+8.*pixsiz
                       goto 225
                    endif
                 else
                    tauout(nx,ny)=tauout(nx,ny)/npix
                 endif
              elseif (intype.eq.4) then
                 tauout(nx,ny)=minim
              elseif (intype.eq.5) then
                 tauout(nx,ny)=maxim                 
              elseif (intype.eq.0) then
                 if (ndat.lt.1.) then
                    tauout(nx,ny)=defval
                 endif                 
              endif
              if (tauout(nx,ny).ge.2.52) then
                 tauout(nx,ny)=2.54
                 if (autom.eq.1) then
c                   factor=factor+8.*pixsiz
                    factor=factor*2.
                    goto 225
                 endif
              endif
 114       continue
           percent=int((real(nx)/real(ncellx))*100./25.)*25
           if (percent.ne.pcentold) then
              print*,percent,'%'
           endif
           pcentold=percent
 113    continue
c
c ----------
c
c   fabrication d'un nouveau fichier pgm
c
      print*,'making optical depth pgm image...'
         aotfile=nom(1:lennom)//'_i.pgm'
         open(unit=27,file=aotfile,status="unknown")
         write(27,178) 'P2'
         write(27,178) '# image d epaisseurs optiques interpolee par int
     +erp.f, (100=aod 1)'
         write(27,180) '# date ',he,mi,se,jo,mo,an
         write(27,179) '# pixsiz ',lcellx
         write(27,179) '# lat0   ',xcell0
         write(27,179) '# lon0   ',ycell0
         write(27,*) ncelly, ncellx
         write(27,*) '255'
         do 3411 i=ncellx,1,-1
               write(27,*) (nint(tauout(i,j)*100.),j=1,ncelly)
 3411    continue
 177     format(i3)
 178     format(A)
 179     format(A,F12.6)
 180  format(A,I2,1x,I2,1x,I2,1x,I2,1x,I2,1x,I4)
         close(unit=27)
       stop
 12    print*,'Edit parameter file : interp.par by typing epar_inter!'
       stop    
       end
