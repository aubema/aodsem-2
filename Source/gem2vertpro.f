c
c   Programme permettant de generer un profil vertical de
c   variables meteo
c   a partir des fichiers ASCII provenant du modele GEM du
c   Centre Meteorologique Canadien (CMC).
c
c
       program gem2vertprof

c
c   Declaration des variables
c
       integer n,nn,nlign,nfield,ncellx,ncelly,begind(200),endda,nldat
       integer nf,varp(200),plevel(16),nl,k,i,j,RH(125,275,16),ncellz
       integer lenom,hre,min,sec,jou,moi,ann,prswitch,ptime
       integer x,y
       real donnee(125,275,16),lcellx,lcelly,xcell0,ycell0,hcellz(16)
       real P(125,275,16),h0,P0,dP_dz(125,275,16),dP_dx(125,275,16),m,b
       real dP_dy(125,275,16),dx,dy,latt,VV(125,275,16),UU(125,275,16)
       real vz(125,275,16),dP_dt(125,275,16),dt,P2(125,275,16)
       real TT(125,275,16),ea,eta,ES(125,275,16),pi,PR06(125,275)
       real PR12(125,275),lcellz(16),latitu,longit 
       character bidon1,bidon2
       character*3 bidon3 
       character*2 varn(200)
       character*60  gemfi1,gemfi2,bidon,wetfile,rhfile,wsfile
c
c   Initialisation des variables
c
c   niveaux de pression du GEM
       data plevel /1000,925,850,700,500,400,300,250,200,150,100,70
     + ,50,30,20,10/
c   nombre de niveaux verticaux dans AODSEM
       ncellz=16 
c   altitude centrale de chaque couche verticale (en metres)
       data hcellz/80.,264.,503.2,814.16,1218.41,
     + 1743.93,2427.1,3315.2,4469.8,5970.8,7922.,10458.6,13756.2,18043.,
     + 23615.9,30868.7/
c   largeur de chaque couche verticale
       data lcellz/160.,208.,270.4,351.5,457.,594.1,772.3,1004.,
     $ 1305.2,1696.8,2205.8,2867.5,3727.7,4846.,6299.8,8189.8/
       pi=3.14159 
       nlign=0
c
c   Donnees d entree
c       
c       print*,'First GEM file name?'
       open(unit=1,file='gem2vertpro.par',status='old',err=111)
         read(1,*) gemfi1
         lenom=index(gemfi1,' ')-1
c       print*,'Date (HH MM SS JJ MM YYYY)?'
         read(1,*) hre,min,sec,jou,moi,ann
c       print*,'Second GEM file name?'
         read(1,*) gemfi2
c       print*,'Time interval between the two GEM files (hour) ?'
         read(1,*) dt
         dt=dt*3600.
c       print*,'Site latitude, longitude?'
         read(1,*) latitu,longit
         if (longit.gt.360.) longit=longit-360.
         if (longit.lt.0.) longit=longit+360.
       close(unit=1)
c       
c   Ouverture des fichiers GEM
c
       open(unit=1,file=gemfi1,status='old')
       open(unit=2,file=gemfi2,status='old')
c
c   Inspecteur de fichier GEM  (second fichier) makesource
c
c   Calcul du nombre de lignes du fichier GEM, du nombre de variables
c   meteo et de la position du debut de chaque variable meteo.
c   Recherche des titre de variables et des niveaux de pression ou 
c   de l heure pour la variable de precipitation PR   
c
       nfield=0
       nlign=0
       print*,'Inspection of second file '
       do n=1,1000000
         read(2,*,end=110) bidon
         nlign=nlign+1
         if (bidon(1:10).eq.'START_DATA') then
           nfield=nfield+1 
           begind(nfield)=nlign+1    
         endif  
         if ((bidon(1:8).eq.'END_DATA').and.(nfield.eq.1)) then
            endda=nlign
         endif  
         nldat=endda-begind(1)
 110   enddo  
       rewind 2
       nf=0
       do n=1,nlign
         read(2,*,end=120) bidon
         if (bidon(1:8).eq.'variable') then
           backspace 2
           read(2,*) bidon1,bidon2,bidon3
           nf=nf+1
           varn(nf)=bidon3(1:2)
           if (varn(nf).ne.'PR') then
             do nn=1,8
               read(2,*)
             enddo
             read(2,*) bidon1,bidon2,varp(nf)  
           else
             do nn=1,5
               read(2,*)
             enddo 
             read(2,*) bidon1,bidon2,varp(nf)
           endif  
         endif  
       enddo 
 120   rewind 2
c
c   Lecture de la taille de la matrice de donnees
c
       do n=1,14
         read(2,*)
       enddo
       read(2,*) bidon1,bidon2,ncelly
       read(2,*) bidon1,bidon2,ncellx  
       do n=1,6
         read(2,*)
       enddo
       read(2,*) bidon1,bidon2,lcellx
       read(2,*) bidon1,bidon2,lcelly
       read(2,*) bidon1,bidon2,xcell0
       read(2,*) bidon1,bidon2,ycell0   
       rewind 2
c
c    determiner la position du site dans ces matrices
c
       x=nint((latitu-xcell0)/lcellx)+1
       y=nint((longit-ycell0)/lcelly)+1
c
c   Lecture des niveaux GZ et chargement dans la matrice donnee
c
       print*,'Loading GZ field'
       do nf=1,nfield
         do n=1,42
           read(2,*)
         enddo  
         if (varn(nf).eq.'GZ') then
           do nl=1,16
             if (varp(nf).eq.plevel(nl)) then
               read(2,*) ((donnee(i,j,nl),j=1,ncelly),i=1,ncellx)
             endif
           enddo
         else
           do n=1,nldat
             read(2,*)
           enddo     
         endif 
         read(2,*)
       enddo
       rewind 2
c
c   conversion des altitudes GZ en de dam a metre
c
       do nl=1,16
       do i=1,ncellx
       do j=1,ncelly
         donnee(i,j,nl)=donnee(i,j,nl)*10.
       enddo
       enddo
       enddo
c  
c   Transposition de GZ en distribution de pression 3D sur la grille
c   AODSEM interpolation avec une fonction exp(-z/h0) 
c 
       print*,'Computing Pressure field' 
       do i=1,ncellx
       do j=1,ncelly
       do k=1,ncellz
         do nl=1,15
           if (donnee(i,j,nl).lt.hcellz(k)) then
             if (donnee(i,j,nl+1).ge.hcellz(k)) then
               h0=(donnee(i,j,nl+1)-donnee(i,j,nl))/log(real(plevel(nl))
     +         /real(plevel(nl+1)))      
               P0=real(plevel(nl))/(exp(-donnee(i,j,nl)/h0)) 
               P2(i,j,k)=P0*exp(-hcellz(k)/h0) 
             endif
           endif
           if (hcellz(k).lt.donnee(i,j,1)) then
             h0=(donnee(i,j,2)-donnee(i,j,1))/log(real(plevel(1))
     +       /real(plevel(2)))
             P0=real(plevel(1))/(exp(-donnee(i,j,1)/h0)) 
             P2(i,j,k)=P0*exp(-hcellz(k)/h0)
           endif 
        enddo   
       enddo
       enddo
       enddo
       close(unit=2)
c
c   Inspecteur de fichier GEM  (premier fichier=fichier principal) 
c
c   Calcul du nombre de lignes du fichier GEM, du nombre de variables
c   meteo et de la position du debut de chaque variable meteo.
c   Recherche des titre de variables et des niveaux de pression ou 
c   de l heure pour la variable de precipitation PR   
c
       print*,'Inspection of first file'
       nfield=0
       nlign=0
       do n=1,1000000
         read(1,*,end=10) bidon
         nlign=nlign+1
         if (bidon(1:10).eq.'START_DATA') then
           nfield=nfield+1 
           begind(nfield)=nlign+1
         endif  
         if ((bidon(1:8).eq.'END_DATA').and.(nfield.eq.1)) then
            endda=nlign
         endif  
         nldat=endda-begind(1)
 10    enddo  
       rewind 1
       nf=0
       do n=1,nlign
         read(1,*,end=20) bidon
         if (bidon(1:8).eq.'variable') then
           backspace 1
           read(1,*) bidon1,bidon2,bidon3
           nf=nf+1
           varn(nf)=bidon3(1:2)
           if (varn(nf).ne.'PR') then
             do nn=1,8
               read(1,*)
             enddo
             read(1,*) bidon1,bidon2,varp(nf) 
           else
             do nn=1,5
               read(1,*)
             enddo 
             read(1,*) bidon1,bidon2,varp(nf)
           endif  
         endif  
       enddo 
 20    rewind 1 
       print*,'   Total number of lines          : ',nlign
       print*,'   Number of meteorological fields: ',nfield
       print*,'   Number of data lines           : ',nldat
c
c   Lecture de la taille de la matrice de donnees
c
       do n=1,14
         read(1,*)
       enddo
       read(1,*) bidon1,bidon2,ncelly
       read(1,*) bidon1,bidon2,ncellx  
       do n=1,6
         read(1,*)
       enddo
       read(1,*) bidon1,bidon2,lcellx
       read(1,*) bidon1,bidon2,lcelly
       read(1,*) bidon1,bidon2,xcell0
       read(1,*) bidon1,bidon2,ycell0   
       print*,'   Number of N-S cells            : ',ncellx
       print*,'   Number of E-W cells            : ',ncelly   
       print*,'   Width of N-S cell              : ',lcellx
       print*,'   Width of E-W cell              : ',lcelly
       print*,'   S-W cell latitude              : ',xcell0
       print*,'   S-W cell longitude             : ',ycell0         
       rewind 1
c
c   Lecture des niveaux GZ et chargement dans la matrice donnee
c
       print*,'Loading GZ field '
       do nf=1,nfield
         do n=1,42
           read(1,*)
         enddo  
         if (varn(nf).eq.'GZ') then
           do nl=1,16
             if (varp(nf).eq.plevel(nl)) then
               read(1,*) ((donnee(i,j,nl),j=1,ncelly),i=1,ncellx)
             endif
           enddo
         else
           do n=1,nldat
             read(1,*)
           enddo     
         endif 
         read(1,*)
       enddo
       rewind 1
c
c   conversion des altitudes GZ en de dam a metre
c
       do nl=1,16
       do i=1,ncellx
       do j=1,ncelly
         donnee(i,j,nl)=donnee(i,j,nl)*10.
       enddo
       enddo
       enddo
c  
c   Transposition de GZ en distribution de pression 3D sur la grille
c   AODSEM interpolation avec une fonction exp(-z/h0) et calcul de la 
c   matrice de gradient vertical de P (dP_dz)
c 
       print*,'Computing Pressure field '
       print*,'Computing vertical Pressure gradient.'
       do i=1,ncellx
       do j=1,ncelly
       do k=1,ncellz
         do nl=1,15
           if (donnee(i,j,nl).lt.hcellz(k)) then
             if (donnee(i,j,nl+1).ge.hcellz(k)) then
               h0=(donnee(i,j,nl+1)-donnee(i,j,nl))/log(real(plevel(nl))
     +         /real(plevel(nl+1)))      
               P0=real(plevel(nl))/(exp(-donnee(i,j,nl)/h0)) 
               P(i,j,k)=P0*exp(-hcellz(k)/h0) 
               dP_dz(i,j,k)=-P(i,j,k)/h0              
             endif
           endif
           if (hcellz(k).lt.donnee(i,j,1)) then
             h0=(donnee(i,j,2)-donnee(i,j,1))/log(real(plevel(1))
     +       /real(plevel(2)))
             P0=real(plevel(1))/(exp(-donnee(i,j,1)/h0)) 
             P(i,j,k)=P0*exp(-hcellz(k)/h0)
             dP_dz(i,j,k)=-P(i,j,k)/h0             
           endif 
        enddo   
       enddo
       enddo
       enddo
c
c   Calcul des matrices de gradient horizontal de la Pression 
c   dP_dx et dP_dy
c  
       print*,'Computing horizontal Pressure gradients.' 
       do i=1,ncellx
       do j=1,ncelly
       do k=1,ncellz
         latt=xcell0+real(i-1)*lcelly
         call echelle(latt,lcellx,lcelly,dx,dy)
         if ((i.gt.1).and.(i.lt.ncellx)) then
           dP_dx(i,j,k)=(P(i+1,j,k)-P(i-1,j,k))/(2.*dx)
         elseif (i.eq.1) then
           dP_dx(i,j,k)=(P(i+1,j,k)-P(i,j,k))/dx  
         else
           dP_dx(i,j,k)=(P(i,j,k)-P(i-1,j,k))/dx 
         endif         
         if ((j.gt.1).and.(j.lt.ncelly)) then
           dP_dy(i,j,k)=(P(i,j+1,k)-P(i,j-1,k))/(2.*dy)
         elseif (j.eq.1) then
           dP_dy(i,j,k)=(P(i,j+1,k)-P(i,j,k))/dy  
         else
           dP_dy(i,j,k)=(P(i,j,k)-P(i,j-1,k))/dy 
         endif                        
       enddo
       enddo
       enddo       
c
c   Calcul de la variation temporelle de la Pression dP_dt
c 
       print*,'Computing Pressure temporal derivative dP/dt.'
       do i=1,ncellx
       do j=1,ncelly
       do k=1,ncellz
         dP_dt(i,j,k)=(P2(i,j,k)-P(i,j,k))/dt 
       enddo
       enddo
       enddo                   
c
c   Lecture de la variable de vitesse N-S (VV)
c  
       print*,'Loading VV field '
       do nf=1,nfield
         do n=1,42
           read(1,*)
         enddo  
         if (varn(nf).eq.'VV') then
           do nl=1,16
             if (varp(nf).eq.plevel(nl)) then
               read(1,*) ((donnee(i,j,nl),j=1,ncelly),i=1,ncellx)
             endif
           enddo
         else
           do n=1,nldat
             read(1,*)
           enddo     
         endif 
         read(1,*)
       enddo
       rewind 1
c
c   conversion des vitesses VV de knot a metre/s
c
       do nl=1,16
       do i=1,ncellx
       do j=1,ncelly
         donnee(i,j,nl)=donnee(i,j,nl)/1.94384
       enddo
       enddo
       enddo  
c
c   Projection sur la grille AODSEM
c   
       print*,'Projecttion de VV sur la grille'     
       do i=1,ncellx
       do j=1,ncelly
       do k=1,ncellz
         do nl=1,15
           if ((real(plevel(nl)).gt.P(i,j,k)).and.(real(plevel(nl+1))
     +     .le.P(i,j,k))) then
             m=(donnee(i,j,nl+1)-donnee(i,j,nl))/(real(plevel(nl+1)
     +       -plevel(nl)))
             b=donnee(i,j,nl)-m*real(plevel(nl))
             VV(i,j,k)=m*P(i,j,k)+b
           elseif (P(i,j,k).ge.real(plevel(1))) then
             VV(i,j,k)=donnee(i,j,1)
           elseif (P(i,j,k).le.real(plevel(16))) then   
            VV(i,j,k)=donnee(i,j,16) 
           endif
         enddo
       enddo
       enddo
       enddo           
c
c   Lecture de la variable de vitesse E-W (UU)
c  
       print*,'Loading UU field '
       do nf=1,nfield
         do n=1,42
           read(1,*)
         enddo  
         if (varn(nf).eq.'UU') then
           do nl=1,16
             if (varp(nf).eq.plevel(nl)) then
               read(1,*) ((donnee(i,j,nl),j=1,ncelly),i=1,ncellx)
             endif
           enddo
         else
           do n=1,nldat
             read(1,*)
           enddo     
         endif 
         read(1,*)
       enddo
       rewind 1
c
c   conversion des vitesses UU de knot a metre/s
c
       do nl=1,16
       do i=1,ncellx
       do j=1,ncelly
         donnee(i,j,nl)=donnee(i,j,nl)/1.94384
       enddo
       enddo
       enddo  
c
c   Projection sur la grille AODSEM
c       
       do i=1,ncellx
       do j=1,ncelly
       do k=1,ncellz
         do nl=1,15
           if ((real(plevel(nl)).gt.P(i,j,k)).and.(real(plevel(nl+1))
     +      .le.P(i,j,k))) then
             m=(donnee(i,j,nl+1)-donnee(i,j,nl))/(real(plevel(nl+1)
     +        -plevel(nl)))
             b=donnee(i,j,nl)-m*real(plevel(nl))
             UU(i,j,k)=m*P(i,j,k)+b
           elseif (P(i,j,k).ge.real(plevel(1))) then
             UU(i,j,k)=donnee(i,j,1)
           elseif (P(i,j,k).le.real(plevel(16))) then   
            UU(i,j,k)=donnee(i,j,16) 
           endif
          enddo
       enddo
       enddo
       enddo  
c
c   Calcul de la vitesse verticale vz en m/s a l aide de la differentielle
c   totale dP/dr
c         
       print*,'Computing vertical winds.'
       do i=1,ncellx
       do j=1,ncelly
       do k=1,ncellz
         vz(i,j,k)=(-dP_dt(i,j,k)-VV(i,j,k)*dP_dx(i,j,k)-UU(i,j,k)*
     +   dP_dy(i,j,k))/dP_dz(i,j,k)
       enddo
       enddo
       enddo 
c
c   Lecture de la temperature de l ecart du point de rosee (ES)
c  
       print*,'Loading ES field '
       do nf=1,nfield
         do n=1,42
           read(1,*)
         enddo  
         if (varn(nf).eq.'ES') then
           do nl=1,16
             if (varp(nf).eq.plevel(nl)) then
               read(1,*) ((donnee(i,j,nl),j=1,ncelly),i=1,ncellx)
             endif
           enddo
           if (varp(nf).eq.0) then
             do n=1,nldat
               read(1,*)
             enddo   
           endif          
         else
           do n=1,nldat
             read(1,*)
           enddo     
         endif 
         read(1,*) bidon
       enddo
       rewind 1
c
c   Projection sur la grille AODSEM
c       
       do i=1,ncellx
       do j=1,ncelly
       do k=1,ncellz
         do nl=1,15
           if ((real(plevel(nl)).gt.P(i,j,k)).and.(real(plevel(nl+1))
     +      .le.P(i,j,k))) then
             m=(donnee(i,j,nl+1)-donnee(i,j,nl))/(real(plevel(nl+1)
     +        -plevel(nl)))
             b=donnee(i,j,nl)-m*real(plevel(nl))
             ES(i,j,k)=m*P(i,j,k)+b
           elseif (P(i,j,k).ge.real(plevel(1))) then
             ES(i,j,k)=donnee(i,j,1)
           elseif (P(i,j,k).le.real(plevel(16))) then   
            ES(i,j,k)=donnee(i,j,16) 
           endif
          enddo
       enddo
       enddo
       enddo         
c
c   Lecture de la temperature (TT)
c  
       print*,'Loading TT field '
       do nf=1,nfield
         do n=1,42
           read(1,*)
         enddo  
         if (varn(nf).eq.'TT') then
           do nl=1,16
             if (varp(nf).eq.plevel(nl)) then
               read(1,*) ((donnee(i,j,nl),j=1,ncelly),i=1,ncellx)
             endif  
           enddo
         else
           do n=1,nldat
             read(1,*)
           enddo     
         endif 
         read(1,*)
       enddo
       rewind 1
c
c   Projection sur la grille AODSEM
c       
       do i=1,ncellx
       do j=1,ncelly
       do k=1,ncellz
         do nl=1,15
           if ((real(plevel(nl)).gt.P(i,j,k)).and.(real(plevel(nl+1))
     +      .le.P(i,j,k))) then
             m=(donnee(i,j,nl+1)-donnee(i,j,nl))/(real(plevel(nl+1)
     +        -plevel(nl)))
             b=donnee(i,j,nl)-m*real(plevel(nl))
             TT(i,j,k)=m*P(i,j,k)+b
           elseif (P(i,j,k).ge.real(plevel(1))) then
             TT(i,j,k)=donnee(i,j,1)
           elseif (P(i,j,k).le.real(plevel(16))) then   
            TT(i,j,k)=donnee(i,j,16) 
           endif
          enddo
       enddo
       enddo
       enddo          
c
c   Calcul de l'humidite relative (RH)
c 
       print*,'Computing relative humidity.'      
       do i=1,ncellx
       do j=1,ncelly
       do k=1,ncellz
         ea=6.1070*(1.+sqrt(2.)*sin(pi*(TT(i,j,k)-ES(i,j,k))/
     +   (3.*180.)))**8.827
         eta=6.1070*(1.+sqrt(2.)*sin(pi*TT(i,j,k)/(3.*180.)))**8.827
         RH(i,j,k)=int(100.*ea/eta)  
       enddo
       enddo
       enddo       
c
c   Lecture de la precipitation (unite=m/6 hre) (PR)
c  
       prswitch=0
       do nf=1,nfield
         do n=1,42
           read(1,*)
         enddo  
         if (varn(nf).eq.'PR') then
           print*,'Loading PR field '
           prswitch=1
           read(1,*) ((donnee(i,j,1),j=1,ncelly),i=1,ncellx)
           if (varp(nf).eq.6) then
             do i=1,ncellx
               do j=1,ncelly
                 PR06(i,j)=donnee(i,j,1)
               enddo
             enddo
           elseif (varp(nf).eq.12) then       
             do i=1,ncellx
               do j=1,ncelly
                 PR12(i,j)=donnee(i,j,1)
               enddo
             enddo  
           endif             
         else
           do n=1,nldat
             read(1,*)
           enddo     
         endif 
         read(1,*) bidon
       enddo
       rewind 1               
       close(unit=1)
c
c   Ecriture du fichier de sortie
c 
c
       open(unit =1,file=gemfi1(1:lenom)//'.pro',status='unknown')
        write(1,*) '#  Fichier de profil vertical meteo re-echanti-'
        write(1,*) '#  llonne a partir des analyses globales du GEM'
        write(1,*) 'latitude= ',latitu,'  longitude=',longit
        write(1,*) 'H (m)     RH (%)    UU (m/s)  VV (m/s)  VZ (m/s)
     +P (mbar)  TT (oC)   ES (oC)'
           do nz=1,ncellz
              write(1,1212) hcellz(nz),RH(x,y,nz),UU(x,y,nz),VV(x,y,nz)
     +  ,vz(x,y,nz),P(x,y,nz),TT(x,y,nz),ES(x,y,nz)
           enddo
        close(unit=1)
 1212   format(f9.1,1x,I9,1x,f9.3,1x,f9.3,1x,f9.5,1x,f9.1,1x,
     +f9.1,1x,f9.1)       
       stop
 111   print*,'No parameter file: gem2vertpro.par found!'
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
      
