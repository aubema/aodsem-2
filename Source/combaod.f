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
c    copyright Martin Aube 10/2003
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
      real pixsiz,lattau0,lontau0,tau(4,1000,2000),tauout(1000,2000)
      real latitu,longit,distan,dtotal,factor,latcen,loncen,ndat
      real Vx,Vy,tau1,tau2,tau3,tau4,distan1,distan2,distan3,distan4
      real defval,offset(4),gain(4),value,nbmin
      integer maxside,he,mi,se,jo,mo,an,nbdat
      character*60 bidon,aotfile,taufile,header,tag,dfile(4),nom
      integer i,j,intype,autom,n1,n2,n3,n4
      integer percent,pcentold,nxcen,nycen,lennom(4)
      integer dens(4,10,1000,2000),lenout
      integer nx,ny,ncellx,ncelly,side,maxi,ngris,hcnt,theta
c   
c ----------
c
c   initialisation des variables
c
c======
c nbmin=nombre de donnees minimales pour proceder a une 
c interpolation lineaire ou quadratique
       nbmin=1.
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
      open(unit=19,file="combaod.par",status="old",err=12)
c
c   parametres
c
c      factor=rayon d'interpolation initial
       read(19,*) nom
       read(19,*) factor
       read(19,*) intype       
       read(19,*) autom      
       read(19,*) defval  
       read(19,*) nbdat
c
c      errors sous la forme gain*aod+offset
c
       do n=1,nbdat
          read(19,*) dfile(n)
          read(19,*) gain(n), offset(n) 
c
c         calcul de la longueur du nom
c
          lenout=index(nom,' ')-1
          lennom(n)=index(dfile(n),' ')-1
       enddo 
      close(unit=19)
        
      defval=defval/100.
    


c
c
c -----------
c
c   Lecture des fichiers d'epaisseur optique
c
c   fichier pgm pour les images polder 254 et 255=aucun signification
c   253=lignes cotieres, autre=aod*100
c
       do n=1,nbdat

         dfile(n)=dfile(n)(1:lennom(n))//'.pgm'
         open(unit=2,file=dfile(n),status='old')
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
              if (tag(1:6).eq.'pixsiz') then
                 backspace 2
                 read(2,*) bidon,tag,value
                 lcellx=value
              elseif (tag(1:4).eq.'lat0') then
                 backspace 2
                 read(2,*) bidon,tag,value 
                 xcell0=value
              elseif (tag(1:4).eq.'lon0')  then
                 backspace 2
                 read(2,*) bidon,tag,value 
                 ycell0=value 
              endif
              lcelly=lcellx
            endif
 54      continue            
 56      rewind 2
         read(2,*)
         do 55 i=1,hcnt
            read(2,*)
 55      continue      
         read(2,*) ncelly, ncellx, maxi 
         print*,'Reading AOD data...'
         read(2,*) ((tau(n,nx,ny),ny=1,ncelly),nx=ncellx,1,-1)
         close(unit=2)
c
c   normaliser tau
c
         do 555 nx=1,ncellx
         do 556 ny=1,ncelly
            tau(n,nx,ny)=tau(n,nx,ny)/100.
 556     continue
 555     continue
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
        
              do i=1,ncellx
              do j=1,ncelly
                do nn=1,10
                   dens(n,nn,i,j)=0
                enddo 
               if (tau(n,i,j).lt.2.53) dens(n,9,i,j)=1
            if (j.ne.ncelly) then 
               if (tau(n,i,j+1).lt.2.53) dens(n,1,i,j)=1
            elseif ((i.ne.ncellx).and.(j.ne.ncelly)) then
               if (tau(n,i+1,j+1).lt.2.53) dens(n,2,i,j)=1
            elseif (i.ne.ncelly) then
               if (tau(n,i+1,j).lt.2.53) dens(n,3,i,j)=1 
            elseif ((i.ne.ncellx).and.(j.ne.1)) then               
               if (tau(n,i+1,j-1).lt.2.53) dens(n,4,i,j)=1
            elseif (j.ne.1) then            
               if (tau(n,i,j-1).lt.2.53) dens(n,5,i,j)=1 
            elseif ((i.ne.1).and.(j.ne.1)) then 
               if (tau(n,i-1,j-1).lt.2.53) dens(n,6,i,j)=1 
            elseif (i.ne.1) then               
               if (tau(n,i-1,j).lt.2.53) dens(n,7,i,j)=1
            elseif ((i.ne.1).and.(j.ne.ncelly)) then                
                if (tau(n,i-1,j+1).lt.2.53) dens(n,8,i,j)=1  
            endif
              dens(n,10,i,j)=dens(n,1,i,j)+dens(n,2,i,j)+ 
     +        dens(n,3,i,j)+dens(n,4,i,j)+dens(n,5,i,j)+
     +        dens(n,6,i,j)+dens(n,7,i,j)+dens(n,8,i,j)+dens(n,9,i,j)                                      
              enddo            
              enddo 
c  fin de chargement des donnees
       enddo
c
c   calcul de la taille maximale d interpolation
c    
       if (ncellx.gt.ncelly) then
         maxside=2*ncellx+1
       else
         maxside=2*ncelly+1
       endif   
        if (ycell0.lt.0.) ycell0=ycell0+360.
 666    format(A,1x,F6.2,1x,A)
c
c    Taille de la fenetre d interpolation
c                         
 225    side=nint(factor/2.0001) 
        if (side.gt.maxside) then
           print*,'   ERROR: Maximum interpolating width overpass'
           print*,'   ERROR: while resampling file: ',
     +     taufile(1:lennom(n)+5)
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

              dtotal=0.
              tauout(nx,ny)=0.
              dmin=2000000.
              npix=0.
              minim=2000000.
              maxim=-2000000.
              ndat=0.
              do n=1,nbdat

c
c    corriger pour la repartition inegale des donnees
c    
              do 115 i=nx-side,nx+side
              do 116 j=ny-side,ny+side

                 distan=sqrt((real(ny)-real(j))**2.+(real(nx)-
     +           real(i))**2.)

                if (distan.le.real(side)) then
                 if (distan.lt.0.01) distan=0.01
                 if ((i.gt.0).and.(i.le.ncellx)) then
                    if ((j.gt.0).and.(j.le.ncelly)) then
                       if (tau(n,i,j).le.2.52) then

                          ndat=ndat+1.
                          if (intype.eq.0) then
                             if (distan.lt.dmin) then
                                dmin=distan
                                tauout(nx,ny)=tau(n,i,j)
                             endif                                        
                          elseif (intype.eq.1) then
                             tauout(nx,ny)=tau(n,i,j)/(distan*
     +                       (real(dens(n,10,i,j))))/(gain(n)*
     +                       tau(n,i,j)+offset(n))+tauout(nx,ny) 
                             dtotal=1./(distan*(real(dens(n,10,i,j))))/
     +                       (gain(n)*tau(n,i,j)+offset(n))+dtotal  
                          elseif (intype.eq.2) then
                             tauout(nx,ny)=tau(n,i,j)/(distan**2.*
     +                       (real(dens(n,10,i,j))))/(gain(n)*
     +                       tau(n,i,j)+offset(n))+tauout(nx,ny)
                             dtotal=1./(distan**2.*(real(dens(n,10,i,j))
     +                       ))/(gain(n)*tau(n,i,j)+offset(n))+dtotal
                          elseif (intype.eq.3) then
                             npix=npix+1.
                             tauout(nx,ny)=tau(n,i,j)+tauout(nx,ny)
                          elseif (intype.eq.4) then
                             if (tau(n,i,j).lt.minim) then
                                minim=tau(n,i,j)
                             endif
                          elseif (intype.eq.5) then
                             if (tau(n,i,j).gt.maxim) then
                                maxim=tau(n,i,j)
                             endif
                          endif
                     endif
                    endif
                 endif
                endif
 116          continue
 115          continue
c fin do n
              enddo
              if (dtotal.eq.0.) dtotal=0.000001   
              if ((intype.eq.1).or.(intype.eq.2)) then
                 tauout(nx,ny)=tauout(nx,ny)/dtotal   
                 if (ndat.lt.nbmin) then
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
           percent=int((real(nx)/real(ncellx))*100./
     +     20.)*20
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
         aotfile=nom(1:lenout)//'_i.pgm'
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
 12    print*,'Edit parameter file : combaod.par by typing epar_inter!'
       stop    
       end
