c
c   Programme permettant de generer les champs meteo AODSEM
c   a partir des fichiers ASCII provenant du modele NARCM.
c
c
       program narcm2aodsem
c
c   Declaration des variables
c
       integer nn,nlign,nfield,ncellx,ncelly,begind(200),endda,nldat
       integer nf,varp(200),plevel(14),nl,k,i,j,RH(75,150,10),ncellz
       integer lenom,prswitch,ptime,nexp,heure,jour,mois,annee
       real*8 hre,min,sec,jou,moi,ann,href,minf,secf,jouf,moif,annf
       integer nfile,ncely,ncelx,reclen,recnum
       integer level,n,o,p
       real donnee(75,150,14),lcellx,lcelly,xcell0,ycell0,hcellz(10)
       real P1(75,150,10),h0,P0,dP_dz(75,150,10),dP_dx(75,150,10),m,b
       real dP_dy(75,150,10),dx,dy,latt,VV(75,150,10),UU(75,150,10)
       real vz(75,150,10),dP_dt(75,150,10),dt,P2(75,150,10)
       real TT(75,150,10),ea,eta,pi,PR(75,150)
       real lcellz(10),mesh
       real*8 dh,jday,jdayf
       character bidon2,l1,l2,l3,l4,l7,l8,l9,l10
       character*3 bidon1,bidon3 
       character*2 varn(200),l56
       character*4 bidon
       character*60  gemfi1,wetfile,rhfile,wsfile,nom
       character*132 ligne
       character*70 header
c
c   Initialisation des variables
c
c   niveaux de pression du NARCM
       data plevel /1000,950,900,850,700,600,500,400,250,100,70
     + ,50,30,10/
c   nombre de niveaux verticaux dans AODSEM
       ncellz=10     
c   altitude centrale de chaque couche verticale (en metres)
       data hcellz/82.5,255.,443.5,651.,883.,
     + 1146.,1448.5,1805.5,7000.,21000./
c   largeur de chaque couche verticale
       data lcellz/165.,180.,197.,218.,246.,
     $ 280.,325.,389.,10000.,18000./
       pi=3.14159 
       nlign=0
       reclen=11
  11  format(A132)
  12  format(A70)   
  13  format(A10,1X,A10,1X,A10,1X,A10,1X,A10,1X,A10)  
  14  format(I3,'% ')
c
c   Donnees d entree
c 

      
       print*,'NARCM meteorological variable root file name?'
       print*,'(.npr, .ngz, .nrh, .nuu and .nvv will be add)'
       read*,nom
       lenom=index(nom,' ')-1
       print*,'Date of the first dataset (HH MM SS JJ MM YYYY)?'
       read*,hre,min,sec,jou,moi,ann
       call julian(hre,min,sec,jou,moi,ann,jday)
       print*,'Date of the last dataset (HH MM SS JJ MM YYYY)?'
       read*,href,minf,secf,jouf,moif,annf 
       call julian(href,minf,secf,jouf,moif,annf,jdayf)    
       print*,'Time interval between each NARCM files (hour) ?'
       read*,dh
        dt=dh*3600.
        nexp=nint((jdayf-jday)/(dh/24.))+1       
       print*,'Latitude of the S-W cell?'
       read*,xcell0
       print*,'Longitude of the S-W cell?'  
       read*,ycell0
       if (ycell0.lt.0.) ycell0=360.+ycell0    
c       
c   Ouverture des fichiers NARCM
c
       open(unit=16,file=nom(1:lenom)//'.npr',status='old')
       open(unit=17,file=nom(1:lenom)//'.ngz',status='old')
       open(unit=18,file=nom(1:lenom)//'.nrh',status='old')
       open(unit=19,file=nom(1:lenom)//'.nuu',status='old')
       open(unit=20,file=nom(1:lenom)//'.nvv',status='old')
       open(unit=1,file=nom(1:lenom)//'R.npr',status='unknown')
       open(unit=2,file=nom(1:lenom)//'R.ngz',status='unknown',
     +   access='direct')
       open(unit=33,file=nom(1:lenom)//'R.nrh',status='unknown')
       open(unit=4,file=nom(1:lenom)//'R.nuu',status='unknown')
       open(unit=5,file=nom(1:lenom)//'R.nvv',status='unknown')
c
c   Inpecteur d un  fichier NARCM 
c
c   Nombre de lignes et colonnes du fichier NARCM, 
c   meteo et de la position du debut de chaque variable meteo.
c   Recherche des titre de variables et des niveaux de pression ou 
c   de l heure pour la variable de precipitation PR   
c
      print*,'Inspecting file format' 
      read(17,*) bidon
      read(17,*) bidon
      read(17,*) bidon
      read(17,*) bidon
      read(17,*) bidon, bidon,bidon, level, ncelly, ncellx, lcellx
      lcelly=lcellx
      nldat=0
      bidon='bla '
      dowhile (bidon.ne.'SUBA')
         read(17,*) bidon
         nldat=nldat+1
      enddo 
         nldat=nldat-1
      rewind 17
c
c   reformattage des fichiers
c
       print*,'Reformatting NARCM files'
       do n=1,4
          read(17,*) bidon1
          read(18,*) bidon1
          read(19,*) bidon1
          read(20,*) bidon1
       enddo  
       do nl=1,14
          do o=1,nexp
             read(17,12) header
             read(18,12) header
             read(19,12) header
             read(20,12) header 
             do p=1,nldat
                read(17,11) ligne
                write(2,13) ligne(1:6)//ligne(19:22)
     +                   ,ligne(23:28)//ligne(41:44)
     +                   ,ligne(45:50)//ligne(63:66)
     +                   ,ligne(67:72)//ligne(85:88)
     +                   ,ligne(89:94)//ligne(107:110)
     +                   ,ligne(111:116)//ligne(129:132)
                read(18,11) ligne
                write(33,13) ligne(1:6)//ligne(19:22)
     +                   ,ligne(23:28)//ligne(41:44)
     +                   ,ligne(45:50)//ligne(63:66)
     +                   ,ligne(67:72)//ligne(85:88)
     +                   ,ligne(89:94)//ligne(107:110)
     +                   ,ligne(111:116)//ligne(129:132) 
                read(19,11) ligne
                write(4,13) ligne(1:6)//ligne(19:22)
     +                   ,ligne(23:28)//ligne(41:44)
     +                   ,ligne(45:50)//ligne(63:66)
     +                   ,ligne(67:72)//ligne(85:88)
     +                   ,ligne(89:94)//ligne(107:110)
     +                   ,ligne(111:116)//ligne(129:132)      
                read(20,11) ligne
                write(5,13) ligne(1:6)//ligne(19:22)
     +                   ,ligne(23:28)//ligne(41:44)
     +                   ,ligne(45:50)//ligne(63:66)
     +                   ,ligne(67:72)//ligne(85:88)
     +                   ,ligne(89:94)//ligne(107:110)
     +                   ,ligne(111:116)//ligne(129:132)      
             enddo
          enddo
          write(*,14) 100*nl/14
       enddo    
          do n=1,4
             read(16,*) bidon1
          enddo  
c
          do o=1,nexp
             read(16,12) header
             do p=1,nldat
                read(16,11) ligne
                write(1,13) ligne(1:6)//ligne(19:22)
     +                   ,ligne(23:28)//ligne(41:44)
     +                   ,ligne(45:50)//ligne(63:66)
     +                   ,ligne(67:72)//ligne(85:88)
     +                   ,ligne(89:94)//ligne(107:110)
     +                   ,ligne(111:116)//ligne(129:132)
             enddo
          enddo
          close(unit=16)
          close(unit=17)
          close(unit=18)
          close(unit=19)
          close(unit=20)
          close(unit=1)
          close(unit=2)
          close(unit=33)
          close(unit=4)
          close(unit=5)
      do 5000 nex=1,nexp-1 
c
c     determiner le nom du fichier en fonction de la date
c
      call timedate(hre,min,sec,jou,moi,ann,jday) 
      annee=nint(ann)
      mois=nint(moi)
      jour=nint(jou)
      heure=nint(hre) 
         if (annee/1000.eq.1) then
            l1='1'
            annee=annee-1000
         elseif (annee/1000.eq.2) then
            l1='2'
            annee=annee-2000  
         endif 
         if (annee/100.eq.0) then
            l2='0'       
         elseif (annee/100.eq.1) then
            l2='1'
            annee=annee-100  
         elseif (annee/100.eq.2) then
            l2='2'
            annee=annee-200  
         elseif (annee/100.eq.3) then
            l2='3'
            annee=annee-300  
         elseif (annee/100.eq.4) then
            l2='4'
            annee=annee-400  
         elseif (annee/100.eq.5) then
            l2='5'
            annee=annee-500  
         elseif (annee/100.eq.6) then
            l2='6'
            annee=annee-600  
         elseif (annee/100.eq.7) then
            l2='7'
            annee=annee-700  
         elseif (annee/100.eq.8) then
            l2='8'
            annee=annee-800  
         elseif (annee/100.eq.9) then
            l2='9'
            annee=annee-900
         endif 
         if (annee/10.eq.0) then
            l3='0'        
         elseif (annee/10.eq.1) then
            l3='1'
            annee=annee-10  
         elseif (annee/10.eq.2) then
            l3='2'
            annee=annee-20  
         elseif (annee/10.eq.3) then
            l3='3'
            annee=annee-30  
         elseif (annee/10.eq.4) then
            l3='4'
            annee=annee-40  
         elseif (annee/10.eq.5) then
            l3='5'
            annee=annee-50  
         elseif (annee/10.eq.6) then
            l3='6'
            annee=annee-60  
         elseif (annee/10.eq.7) then
            l3='7'
            annee=annee-70  
         elseif (annee/10.eq.8) then
            l3='8'
            annee=annee-80  
         elseif (annee/10.eq.9) then
            l3='9'
            annee=annee-90
         endif  
         if (annee.eq.0) then
            l4='0'                         
         elseif (annee.eq.1) then
            l4='1'
         elseif (annee.eq.2) then
            l4='2'
         elseif (annee.eq.3) then
            l4='3'
         elseif (annee.eq.4) then
            l4='4'
         elseif (annee.eq.5) then
            l4='5'
         elseif (annee.eq.6) then
            l4='6'
         elseif (annee.eq.7) then
            l4='7'
         elseif (annee.eq.8) then
            l4='8'
         elseif (annee.eq.9) then
            l4='9'
         endif                   
         if (mois.eq.1) then
            l56='01'
         elseif (mois.eq.2) then
            l56='02'
         elseif (mois.eq.3) then
            l56='03'
         elseif (mois.eq.4) then
            l56='04'
         elseif (mois.eq.5) then
            l56='05'
         elseif (mois.eq.6) then
            l56='06'
         elseif (mois.eq.7) then
            l56='07'
         elseif (mois.eq.8) then
            l56='08'
         elseif (mois.eq.9) then
            l56='09'
         elseif (mois.eq.10) then
            l56='10'
         elseif (mois.eq.11) then
            l56='11'
         elseif (mois.eq.12) then
            l56='12'            
         endif                
         if (jour/10.eq.0) then
            l7='0'  
         elseif (jour/10.eq.1) then
            l7='1'
            jour=jour-10             
         elseif (jour/10.eq.2) then
            l7='2'
            jour=jour-20  
         elseif (jour/10.eq.3) then
            l7='3'
            jour=jour-30  
         endif         
         if (jour.eq.0) then
            l8='0'
         elseif (jour.eq.1) then
            l8='1'
         elseif (jour.eq.2) then
            l8='2'
         elseif (jour.eq.3) then
            l8='3'
         elseif (jour.eq.4) then
            l8='4'
         elseif (jour.eq.5) then
            l8='5'
         elseif (jour.eq.6) then
            l8='6'
         elseif (jour.eq.7) then
            l8='7'
         elseif (jour.eq.8) then
            l8='8'
         elseif (jour.eq.9) then
            l8='9'           
         endif                   
         if (heure/10.eq.0) then
            l9='0'
         elseif (heure/10.eq.1) then
            l9='1'
            heure=heure-10   
         elseif (heure/10.eq.2) then
            l9='2'
            heure=heure-20   
         endif 
         if (heure.eq.0) then
            l10='0'                          
         elseif (heure.eq.1) then
            l10='1'
         elseif (heure.eq.2) then
            l10='2'
         elseif (heure.eq.3) then
            l10='3'
         elseif (heure.eq.4) then
            l10='4'
         elseif (heure.eq.5) then
            l10='5'
         elseif (heure.eq.6) then
            l10='6'
         elseif (heure.eq.7) then
            l10='7'
         elseif (heure.eq.8) then
            l10='8'
         elseif (heure.eq.9) then
            l10='9'
         endif                      
         gemfi1=l1//l2//l3//l4//l56//l7//l8//l9//l10  
         print*,'Creating file ',gemfi1(1:10),' (',nex,'/',nexp-1,')'   
       open(unit=1,file=nom(1:lenom)//'R.npr',status='OLD',
     + access='direct',form='formatted',recl=reclen)
       open(unit=2,file=nom(1:lenom)//'R.ngz',status='old',
     + access='direct',form='formatted',recl=reclen)
       open(unit=33,file=nom(1:lenom)//'R.nrh',status='old',
     + access='direct',form='formatted',recl=reclen)
       open(unit=4,file=nom(1:lenom)//'R.nuu',status='old',
     + access='direct',form='formatted',recl=reclen)
       open(unit=5,file=nom(1:lenom)//'R.nvv',status='old',
     + access='direct',form='formatted',recl=reclen)
c
c   Lecture des niveaux GZ et chargement dans la matrice donnee
c
       print*,'Loading first GZ field'
       do nl=1,14
         do i=1,ncellx
           do j=1,ncelly
             recnum=(i-1)*ncelly+j+(nex-1)*ncellx*ncelly+(14-nl)*
     +       ncellx*ncelly*nexp 
             read(2,'(E11.3E2)',rec=recnum) donnee(i,j,nl)       
           enddo
         enddo                       
       enddo    
c
c   conversion des altitudes GZ en m2/s2 a metre
c
       do nl=1,14
       do i=1,ncellx
       do j=1,ncelly
         donnee(i,j,nl)=donnee(i,j,nl)/9.8
         if (donnee(i,j,nl).lt.0.) donnee(i,j,nl)=10.
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
       do k=1,10
         do nl=1,13
           if (donnee(i,j,nl).lt.hcellz(k)) then
             if (donnee(i,j,nl+1).ge.hcellz(k)) then
               h0=(donnee(i,j,nl+1)-donnee(i,j,nl))/log(real(plevel(nl))
     +         /real(plevel(nl+1)))   

               if (h0.le.0.) h0=8000.
   
               P0=real(plevel(nl))/(exp(-donnee(i,j,nl)/h0)) 
               P1(i,j,k)=P0*exp(-hcellz(k)/h0) 

             endif
           endif
           if (hcellz(k).lt.donnee(i,j,1)) then
             h0=(donnee(i,j,2)-donnee(i,j,1))/log(real(plevel(1))
     +       /real(plevel(2)))
             if (h0.le.0.) h0=8000.
             P0=real(plevel(1))/(exp(-donnee(i,j,1)/h0)) 
             P1(i,j,k)=P0*exp(-hcellz(k)/h0)
           endif 
        enddo    
c              if ((j.eq.1).and.(i.eq.1)) print*,P1(i,j,k)
        
       enddo
       enddo
       enddo
c
c   Lecture des niveaux GZ et chargement dans la matrice donnee
c   fichier suivant
c
       print*,'Loading second GZ field'
       do nl=1,14
         do i=1,ncellx
           do j=1,ncelly
             recnum=(i-1)*ncelly+j+(nex-1+1)*ncellx*ncelly+(14-nl)
     +       *ncellx*ncelly*nexp 
             read(2,'(E11.3E2)',rec=recnum) donnee(i,j,nl)
             
c             if ((j.eq.1).and.(i.eq.1)) print*,donnee(i,j,nl)
             
             
           enddo
         enddo                
       enddo    
c
c   conversion des altitudes GZ en m2/s2 a metre
c
       do nl=1,14
       do i=1,ncellx
       do j=1,ncelly
         donnee(i,j,nl)=donnee(i,j,nl)/9.8
         if (donnee(i,j,nl).lt.0.) donnee(i,j,nl)=10.
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
       do k=1,10
         do nl=1,13
           if (donnee(i,j,nl).lt.hcellz(k)) then
             if (donnee(i,j,nl+1).ge.hcellz(k)) then
               h0=(donnee(i,j,nl+1)-donnee(i,j,nl))/log(real(plevel(nl))
     +         /real(plevel(nl+1)))  

               if (h0.le.0.) h0=8000.

    
               P0=real(plevel(nl))/(exp(-donnee(i,j,nl)/h0)) 
               P2(i,j,k)=P0*exp(-hcellz(k)/h0) 
               dP_dz(i,j,k)=-P2(i,j,k)/h0    

             endif
           endif
           if (hcellz(k).lt.donnee(i,j,1)) then
             h0=(donnee(i,j,2)-donnee(i,j,1))/log(real(plevel(1))
     +       /real(plevel(2)))

             if (h0.le.0.) h0=8000.
             P0=real(plevel(1))/(exp(-donnee(i,j,1)/h0)) 
             P2(i,j,k)=P0*exp(-hcellz(k)/h0)
             dP_dz(i,j,k)=-P2(i,j,k)/h0   

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
       do k=1,10
         latt=xcell0+real(i-1)*lcelly
         call echelle(latt,lcellx,lcelly,dx,dy)
         if ((i.gt.1).and.(i.lt.ncellx)) then
           dP_dx(i,j,k)=(P1(i+1,j,k)-P1(i-1,j,k))/(2.*dx)
         elseif (i.eq.1) then
           dP_dx(i,j,k)=(P1(i+1,j,k)-P1(i,j,k))/dx  
         else
           dP_dx(i,j,k)=(P1(i,j,k)-P1(i-1,j,k))/dx 
         endif         
         if ((j.gt.1).and.(j.lt.ncelly)) then
           dP_dy(i,j,k)=(P1(i,j+1,k)-P1(i,j-1,k))/(2.*dy)
         elseif (j.eq.1) then
           dP_dy(i,j,k)=(P1(i,j+1,k)-P1(i,j,k))/dy  
         else
           dP_dy(i,j,k)=(P1(i,j,k)-P1(i,j-1,k))/dy 
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
       do k=1,10
         dP_dt(i,j,k)=(P2(i,j,k)-P1(i,j,k))/dt 
       enddo
       enddo
       enddo            
c
c   Lecture de la variable de vitesse N-S (VV)
c  
       print*,'Loading VV field '
       do nl=1,14
         do i=1,ncellx
           do j=1,ncelly
             recnum= (i-1)*ncelly+j+(nex-1)*ncellx*ncelly+(14-nl)*
     +       ncellx*ncelly*nexp 
             read(5,'(E11.3E2)',rec=recnum) donnee(i,j,nl)
           enddo
         enddo                 
       enddo    
c
c   Projection sur la grille AODSEM
c   
       print*,'Projecttion de VV sur la grille'     
       do i=1,ncellx
       do j=1,ncelly
       do k=1,10
         do nl=1,13
           if ((real(plevel(nl)).gt.P1(i,j,k)).and.(real(plevel(nl+1))
     +     .le.P1(i,j,k))) then
             m=(donnee(i,j,nl+1)-donnee(i,j,nl))/(real(plevel(nl+1)
     +       -plevel(nl)))
             b=donnee(i,j,nl)-m*real(plevel(nl))
             VV(i,j,k)=m*P1(i,j,k)+b
           elseif (P1(i,j,k).ge.real(plevel(1))) then
             VV(i,j,k)=donnee(i,j,1)
           elseif (P1(i,j,k).le.real(plevel(14))) then   
            VV(i,j,k)=donnee(i,j,14) 
           endif
         enddo
       enddo
       enddo
       enddo  
c
c   Lecture de la variable de vitesse E-W (UU)
c  
       print*,'Loading UU field '
       do nl=1,14
         do i=1,ncellx
           do j=1,ncelly
             recnum=(i-1)*ncelly+j+(nex-1)*ncellx*ncelly+(14-nl)*
     +       ncellx*ncelly*nexp 
             read(4,'(E11.3E2)',rec=recnum) donnee(i,j,nl)
           enddo
         enddo                    
       enddo    
c
c   Projection sur la grille AODSEM
c   
       print*,'Projecttion de UU sur la grille'     
       do i=1,ncellx
       do j=1,ncelly
       do k=1,10
         do nl=1,13
           if ((real(plevel(nl)).gt.P1(i,j,k)).and.(real(plevel(nl+1))
     +     .le.P1(i,j,k))) then
             m=(donnee(i,j,nl+1)-donnee(i,j,nl))/(real(plevel(nl+1)
     +       -plevel(nl)))
             b=donnee(i,j,nl)-m*real(plevel(nl))
             UU(i,j,k)=m*P1(i,j,k)+b
           elseif (P1(i,j,k).ge.real(plevel(1))) then
             UU(i,j,k)=donnee(i,j,1)
           elseif (P1(i,j,k).le.real(plevel(14))) then   
            UU(i,j,k)=donnee(i,j,14) 
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
       do k=1,10
         vz(i,j,k)=(-dP_dt(i,j,k)-VV(i,j,k)*dP_dx(i,j,k)-UU(i,j,k)*
     +   dP_dy(i,j,k))/dP_dz(i,j,k)
         if (dP_dz(i,j,k).eq.0.) vz(i,j,k)=0.
c         if (dP_dt(i,j,k).gt.1000000.) vz(i,j,k)=0.
c         if (dP_dx(i,j,k).gt.1000000.) vz(i,j,k)=0.
c         if (dP_dy(i,j,k).gt.1000000.) vz(i,j,k)=0.
       enddo
       enddo
       enddo 
c
c   Lecture de l HUMIDITE RELATIVE (RH)
c  
       print*,'Loading RH field '
       do nl=1,14
         do i=1,ncellx
           do j=1,ncelly
             recnum=(i-1)*ncelly+j+(nex-1)*ncellx*ncelly+(14-nl)*
     +       ncellx*ncelly*nexp 
             read(33,'(E11.3E2)',rec=recnum) donnee(i,j,nl)
             if (donnee(i,j,nl).lt.0.) donnee(i,j,nl)=0.
           enddo
         enddo                          
       enddo        
C
C   CONVERTIR DE FRACTION A POURCENTAGE
C  
       do nl=1,14
       do i=1,ncellx
       do j=1,ncelly
         if (donnee(i,j,nl).lt.0.) donnee(i,j,nl)=0.
         if (donnee(i,j,nl).gt.1.) donnee(i,j,nl)=1.
         donnee(i,j,nl)=donnee(i,j,nl)*100.
       enddo
       enddo
       enddo    
c
c   Projection sur la grille AODSEM
c       
       do i=1,ncellx
       do j=1,ncelly
       do k=1,10
         do nl=1,13
           if ((real(plevel(nl)).gt.P1(i,j,k)).and.(real(plevel(nl+1))
     +      .le.P1(i,j,k))) then
             m=(donnee(i,j,nl+1)-donnee(i,j,nl))/(real(plevel(nl+1)
     +        -plevel(nl)))
             b=donnee(i,j,nl)-m*real(plevel(nl))
             RH(i,j,k)=int(m*P1(i,j,k)+b)
           elseif (P1(i,j,k).ge.real(plevel(1))) then
             RH(i,j,k)=int(donnee(i,j,1))
           elseif (P1(i,j,k).le.real(plevel(14))) then   
            RH(i,j,k)=int(donnee(i,j,14)) 
           endif
          enddo
       enddo
       enddo
       enddo         
c
c   Lecture de la precipitation (unite=mm/sec) (PR)
c  
       print*,'Loading PR field '
       nl=1
         do i=1,ncellx
           do j=1,ncelly
             recnum= (i-1)*ncelly+j+(nex-1)*ncellx*ncelly+(nl-1)*
     +       ncellx*ncelly*nexp 
             read(1,'(E11.3E2)',rec=recnum) donnee(i,j,nl)
             if (donnee(i,j,nl).lt.0.) donnee(i,j,nl)=0.
           enddo
         enddo                    
C   
C   CONVERTIR EN M/6 HRE)
C    
       do i=1,ncellx
       do j=1,ncelly
         PR(i,j)=donnee(i,j,1)/0.046

c       if (PR(i,j).gt.10.) then
c         print*,PR(i,j),i,j,donnee(i,j,1),ncellx,ncelly,nex
c         stop
c      endif


       enddo
       enddo   
c
c   Ecriture des fichiers de sortie
c 
c   fichier de vents
c 
       wsfile=gemfi1(1:10)//'.wsp' 
       call writews(wsfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     & lcellz,xcell0,ycell0,VV,UU,vz,idnint(hre),idnint(min),
     + idnint(sec),idnint(jou),idnint(moi),idnint(ann))
c
c   Fichier d humidite relative
c 
       rhfile=gemfi1(1:10)//'.rhu'     
       call writerh(rhfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     & lcellz,xcell0,ycell0,RH,idnint(hre),idnint(min),
     + idnint(sec),idnint(jou),idnint(moi),idnint(ann))
c
c   Fichier de precipitation
c      
         wetfile=gemfi1(1:10)//'.PR'
         ptime=nint(hre+dh/2.) 
         call writewet(wetfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     +   lcellz,xcell0,ycell0,PR,ptime,idnint(jou),idnint(moi),
     +   idnint(ann))   
       jday=jday+dh/24. 
 5000  continue  
       close(unit=1)
       close(unit=2)
       close(unit=33)
       close(unit=4)
       close(unit=5) 
       stop
       end   
c
c ---------------------------------------------------------

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
c   Routine d ecriture du fichier de vents
c
       subroutine writews(wsfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     & lcellz,xcell0,ycell0,vxwind,vywind,vzwind,hre,min,
     + sec,jou,moi,ann)
       integer ncellx,ncellz,i,j,k
       integer ncelly,ax,hre,min,
     + sec,jou,moi,ann
       real lcellz(10),lcellx,lcelly,xcell0,ycell0,zcell0
       real vxwind(75,150,10),vywind(75,150,10),vzwind(75,150,10)
       character*60 wsfile
       zcell0=0.
       print*,'Writing wind speed file: ',wsfile
       open(unit=3,file=wsfile,status='unknown',err=14) 
          write(3,1024) 
          write(3,*) hre,min,sec,jou,moi,ann,'Date 
     +(HH MM SS DD MM YYYY)'
          write(3,*) ncellx,' Nb cells along N-S'
          write(3,*) lcellx,' S-N cell width'
          write(3,*) ncelly,' Nb cells along E-W'
          write(3,*) lcelly,' W-E cell width'
          write(3,*) ncellz,' Nb cells along Z'
          do 61 k=1,ncellz
             write(3,*) lcellz(k)
 61       continue
          write(3,*) xcell0, ycell0, zcell0,' S-W lat lon Z'
          write(3,*) 'DATA'
          do k=1,ncellz
             write(3,1028) k
	     write(3,*) ((vxwind(i,j,k),j=1,ncelly),i=ncellx,1,-1)          
          enddo
          do k=1,ncellz
             write(3,1029) k
	     write(3,*) ((vywind(i,j,k),j=1,ncelly),i=ncellx,1,-1)          
          enddo
          do k=1,ncellz
             write(3,1031) k
	     write(3,*) ((vzwind(i,j,k),j=1,ncelly),i=ncellx,1,-1)          
          enddo                     
       close(unit=3)
 1024  format('# Wind speed 3-d in m/s for AODSEM')  
 1028  format('# Axis: S-N  Units: m/s Level: ',I2)  
 1029  format('# Axis: W-E  Units: m/s Level: ',I2)
 1031  format('# Axis: Z    Units: m/s Level: ',I2)
       return
 14    print*,'File ',wsfile,' inexistant!'
       stop
       end
c
c -------------------------------------------------------
c
c   Routine d ecriture du fichier de precipitation
c
       subroutine writewet(wetfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     + lcellz,xcell0,ycell0,precip,ptime,jou,moi,ann)
       integer ii,jj,kk,i,j,k,ncellx,ptime,jou,moi,ann
       integer ncelly,ncellz
       real lcellz(10),lcellx,lcelly,xcell0,ycell0,precip(75,150)
       character*60 wetfile
       print*,'Writing precipitation file: ',wetfile
       open(unit=3,file=wetfile,status='unknown',err=15) 
          write(3,1025)
         write(3,*) ptime,' 00 00 ',jou,moi,ann,' (HH MM SS DD MM YYYY)'
          write(3,*) ncellx,' Nb S-N cells'
          write(3,*) lcellx,' S-N cell width'
          write(3,*) ncelly,' Nb W-E cells'
          write(3,*) lcelly,' W-E cell width' 
          write(3,*) xcell0, ycell0,' S-W corner LAT and LON'
          write(3,*) 'DATA'
          write(3,*) ((precip(i,j),j=1,ncelly),i=ncellx,1,-1) 
       close(unit=3)
 1025  format('# Precipitation file for AODSEM (units of meter/6
     + hours)')     
       return  
 15    print*,'File ',wetfile,' inexistant!'
        stop
       end
c
c -------------------------------------------------------
c
c   Routine d ecriture du fichier d humidite relative
c
       subroutine writerh(rhfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     & lcellz,xcell0,ycell0,t_drh,hre1,min1,
     + sec1,jou1,moi1,ann1)
       integer i,j,k,ncellx,hre1,min1,sec1,jou1,moi1,ann1
       integer ncelly,ncellz,t_drh(75,150,10)
       real lcellz(10),lcellx,lcelly,xcell0,ycell0,zcell0   
       character*60 rhfile  
       zcell0=0.
       print*,'Writing humidity file: ',rhfile
       open(unit=3,file=rhfile,status='unknown',err=24) 
          write(3,1026) 
          write(3,*) hre1,min1,sec1,jou1,moi1,ann1,' Date (HH MM SS DD MM
     + YYYY)'
          write(3,*) ncellx,' Nb S-N cells'
          write(3,*) lcellx,' S-N cell width'
          write(3,*) ncelly,' Nb W-E cells'
          write(3,*) lcelly,' W-E cell width'
          write(3,*) ncellz,' Nb cells along Z'
          do 61 k=1,ncellz
             write(3,*) lcellz(k)
 61       continue
          write(3,*) xcell0, ycell0, zcell0,' S-W lat lon Z'
          write(3,*) 'DATA'
          do k=1,ncellz
             write(3,1032) k
	     write(3,*) ((t_drh(i,j,k),j=1,ncelly),i=ncellx,1,-1)         
          enddo 
       close(unit=3)
 1026  format('# Relative humidity file for AODSEM')
 1032  format('# Level: ',I2)
       return
 24    print*,'File ',rhfile,' inexistant!'
       stop
       end        

