c
c   Programme permettant de generer les champs meteo AODSEM
c   a partir des fichiers ASCII provenant du modele GEM du
c   Centre Meteorologique Canadien (CMC).
c
c
       program gem2aodsem
c
c   Declaration des variables
c
       integer n,nn,nlign,nfield,ncellx,ncelly,begind(200),endda,nldat
       integer nf,plevel(16),nl,k,i,j,RH(125,275,10),ncellz
       integer lenom,hre,min,sec,jou,moi,ann,prswitch,ptime,nfil,nfi
       integer bid,hre2,nhre(200),pr00s,pr06s,pr12s,joutmp
       real varp(200)
       real donnee(125,275,16),lcellx,lcelly,xcell0,ycell0,hcellz(10)
       real P(125,275,10),h0,P0,dP_dz(125,275,10),dP_dx(125,275,10),m,b
       real dP_dy(125,275,10),dx,dy,latt,VV(125,275,10),UU(125,275,10)
       real vz(125,275,10),dP_dt(125,275,10),dt,P2(125,275,10)
       real TT(125,275,10),ea,eta,ES(125,275,10),pi,PR06(125,275)
       real PR00(125,275),PR12(125,275),lcellz(10)
       real*8 time(200),jday,an,mo,jo,hr,mi,se
       character bidon1,bidon2
       character*9 bido
       character*10 caractere
       character*3 bidon3 
       character*2 varn(200)
       character*60  gemfi1,gemfi2,bidon,wetfile,rhfile,wsfile,ttfile
c
c   Initialisation des variables
c
c   niveaux de pression du GEM
       data plevel /1000,925,850,700,500,400,300,250,200,150,100,70
     + ,50,30,20,10/
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
c
c   Donnees d entree
c    
       print*,'Time step between two GEM files (hour)?'
       read*,dt   
       dt=dt*3600.	   
       open(unit=7,file='gem.index',status='old')
          read(7,*) nfil
          do nfi=1,nfil-1
             read(7,*) hre,min,sec,jou,moi,ann,gemfi1
             read(7,*) hre2,bid,bid,bid,bid,bid,gemfi2             
             backspace 7
       lenom=index(gemfi1,' ')-1
       call julian(dble(hre),dble(min),dble(sec),dble(jou),
     + dble(moi),dble(ann),jday) 
        
c       
c   Ouverture des fichiers GEM
c
       open(unit=1,file=gemfi1,status='old')
       open(unit=2,file=gemfi2,status='old')
c
c   Inspecteur de fichier GEM  (second fichier) 
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
c   Lecture des niveaux GZ et chargement dans la matrice donnee
c
       print*,'Loading GZ field'
       do nf=1,nfield
         do n=1,42
           read(2,*)
         enddo  
         if (varn(nf).eq.'GZ') then
           do nl=1,16
             if (nint(varp(nf)).eq.plevel(nl)) then
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
       do k=1,10
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
             do nn=1,3
               read(1,*)
             enddo
             read(1,1111) bido,time(nf)
             nhre(nf)=nint(100.*(time(nf)-real(int(time(nf)))))
 1111        format(1x,A9,D14.3)
             do nn=1,4
               read(1,*)
             enddo          
             read(1,*) bidon1,bidon2,varp(nf) 
           else
             do nn=1,3
               read(1,*)
             enddo
             read(1,1111) bido,time(nf)
             nhre(nf)=nint(100.*(time(nf)-real(int(time(nf)))))
             read(1,*) 
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
             if (nint(varp(nf)).eq.plevel(nl)) then
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
       do k=1,10
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
       do k=1,10
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
       do k=1,10
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
             if (nint(varp(nf)).eq.plevel(nl)) then
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
       do k=1,10
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
             if (nint(varp(nf)).eq.plevel(nl)) then
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
       do k=1,10
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
c   Calcul de la vitesse verticale vz en m/s a l aide de la 
c   differentielle
c   totale dP/dr
c         
       print*,'Computing vertical winds.'
       do i=1,ncellx
       do j=1,ncelly
       do k=1,10
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
             if (nint(varp(nf)).eq.plevel(nl)) then
               read(1,*) ((donnee(i,j,nl),j=1,ncelly),i=1,ncellx)
             endif
           enddo
           if (nint(varp(nf)).eq.0) then
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
       do k=1,10
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
             if (nint(varp(nf)).eq.plevel(nl)) then
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
       do k=1,10
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
       do k=1,10
         ea=6.1070*(1.+sqrt(2.)*sin(pi*(TT(i,j,k)-ES(i,j,k))/
     +   (3.*180.)))**8.827
         eta=6.1070*(1.+sqrt(2.)*sin(pi*TT(i,j,k)/(3.*180.)))**8.827
         RH(i,j,k)=nint(100.*ea/eta)
         if (RH(i,j,k).gt.100) RH(i,j,k)=100
       enddo
       enddo
       enddo       
c
c   Lecture de la precipitation (unite=m/6 hre) (PR)
c
       pr00s=0
       pr06s=0
       pr12s=0
       prswitch=0
       do nf=1,nfield
         do n=1,42
           read(1,*)
         enddo  
         if (varn(nf).eq.'PR') then

         



           print*,'Loading PR field '
           prswitch=1
           read(1,*) ((donnee(i,j,1),j=1,ncelly),i=1,ncellx)
           if (nint(varp(nf)).eq.6) then
             pr06s=1
             do i=1,ncellx
               do j=1,ncelly
                 PR06(i,j)=donnee(i,j,1)
               enddo
             enddo
           elseif (nint(varp(nf)).eq.12) then    
             pr12s=1   
             do i=1,ncellx
               do j=1,ncelly
                 PR12(i,j)=donnee(i,j,1)
               enddo
             enddo  
           elseif ((nint(varp(nf)).eq.0).and.(hre.eq.nhre(nf))) then
             pr00s=1
             do i=1,ncellx
               do j=1,ncelly
                 PR00(i,j)=donnee(i,j,1)
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
c   Ecriture des fichiers de sortie
c 
c   fichier de vents
c 
       wsfile=gemfi1(1:lenom)//'.wsp' 
       call writews(wsfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     & lcellz,xcell0,ycell0,VV,UU,vz,hre,min,
     + sec,jou,moi,ann)
c
c   Fichier d humidite relative
c 
       rhfile=gemfi1(1:lenom)//'.rhu'     
       call writerh(rhfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     & lcellz,xcell0,ycell0,RH,hre,min,
     + sec,jou,moi,ann)
c
c   Fichier de temperature
c
       ttfile=gemfi1(1:lenom)//'.tt'
       call writett(ttfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     & lcellz,xcell0,ycell0,TT,hre,min,
     + sec,jou,moi,ann)
c
c   Fichier de precipitation
c   

  
       if (prswitch.eq.1) then 
         if (pr00s.eq.1) then
             call jday2char(jday-3./24.,caractere)
             wetfile=caractere//'.PR'
             call timedate(hr,mi,se,jo,mo,an,jday-3./24.)

           call writewet(wetfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     +     lcellz,xcell0,ycell0,PR00,nint(hr),nint(jo),nint(mo),
     +     nint(an))
         elseif (pr06s.eq.1) then
           if (gemfi1(9:10).eq.'00') then
             wetfile=gemfi1(1:8)//'03.PR'
             ptime=3
             joutmp=jou 
           elseif (gemfi1(9:10).eq.'06') then
             wetfile=gemfi1(1:8)//'09.PR'
             ptime=9
             joutmp=jou
           elseif (gemfi1(9:10).eq.'12') then
             wetfile=gemfi1(1:8)//'15.PR'
             ptime=15
             joutmp=jou
           elseif (gemfi1(9:10).eq.'18') then
             wetfile=gemfi1(1:8)//'21.PR'
             ptime=21
             joutmp=jou
           endif
           call writewet(wetfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     +     lcellz,xcell0,ycell0,PR06,ptime,joutmp,moi,ann)
           if ((pr12s.eq.1).and.(pr06s.eq.1)) then
c             do ii=1,ncellx
c               do jj=1,ncelly
c                 PR12(ii,jj)=PR12(ii,jj)-PR06(ii,jj)
c               enddo
c             enddo
             if (gemfi1(9:10).eq.'00') then
               wetfile=gemfi1(1:8)//'09.PR'
               ptime=9
               joutmp=jou 
             elseif (gemfi1(9:10).eq.'06') then
               wetfile=gemfi1(1:8)//'15.PR'
               ptime=15
               joutmp=jou
             elseif (gemfi1(9:10).eq.'12') then
               wetfile=gemfi1(1:8)//'21.PR'
               ptime=21
               joutmp=jou
             elseif (gemfi1(9:10).eq.'18') then
               wetfile=gemfi1(1:8)//'03.PR'
               ptime=3
               joutmp=jou+1
             endif
             call writewet(wetfile,ncellx,ncelly,ncellz,lcellx,
     +       lcelly,lcellz,xcell0,ycell0,PR12,ptime,joutmp,moi,ann)
           endif
         endif
       endif 
c   fin de la boucle principale (sur la liste de fichiers)
       enddo
       close(unit=7)  
       stop
       end  
c
c -----------------------------------------------------
c
c  Routine pour la convertion d une date vers caractere
c
c             
         subroutine jday2char(time,caract)
         character*10 caract
         character*4 annee
         character*2 mois,jour,heure,minute
         character*1 table(10)
         real*8 hre,min,sec,jou,moi,ann,time
         integer nm,nce,nd,nu


         data table /'0','1','2','3','4','5','6','7','8','9'/

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
         caract=annee//mois//jour//heure 
         return
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
c   Routine d ecriture du fichier de vents
c
       subroutine writews(wsfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     & lcellz,xcell0,ycell0,vxwind,vywind,vzwind,hre,min,
     + sec,jou,moi,ann)
       integer ncellx,ncellz,i,j,k
       integer ncelly,ax,hre,min,
     + sec,jou,moi,ann
       real lcellz(10),lcellx,lcelly,xcell0,ycell0,zcell0
       real vxwind(125,275,10),vywind(125,275,10),vzwind(125,275,10)
       character*60 wsfile
       zcell0=0.
       print*,'Writing wind speed file: ',wsfile
       open(unit=3,file=wsfile,status='unknown',err=14) 
          write(3,1024) 
          write(3,*) hre,min,sec,jou,moi,ann,' Date 
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
       integer i,j,k,ncellx,ptime,jou,moi,ann
       integer ncelly,ncellz
       real lcellz(10),lcellx,lcelly,xcell0,ycell0,precip(125,275)
       character*60 wetfile
       print*,'Writing precipitation file: ',wetfile
       open(unit=3,file=wetfile,status='unknown',err=15) 
          write(3,1025)
          write(3,*) ptime,' 00 00 ',jou,moi,ann,'(HH MM SS DD MM YYYY)'
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
     & lcellz,xcell0,ycell0,t_drh,hre,min,
     + sec,jou,moi,ann)
       integer i,j,k,ncellx,hre,min,
     + sec,jou,moi,ann
       integer ncelly,ncellz,t_drh(125,275,10)
       real lcellz(10),lcellx,lcelly,xcell0,ycell0,zcell0   
       character*60 rhfile  
       zcell0=0.
       print*,'Writing humidity file: ',rhfile
       open(unit=3,file=rhfile,status='unknown',err=24) 
          write(3,1026) 
          write(3,*) hre,min,sec,jou,moi,ann,' Date (HH MM SS DD MM
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

c
c -------------------------------------------------------
c
c   Routine d ecriture du fichier de temperature de l'air
c
       subroutine writett(ttfile,ncellx,ncelly,ncellz,lcellx,lcelly,
     & lcellz,xcell0,ycell0,tt,hre,min,
     + sec,jou,moi,ann)
       integer i,j,k,ncellx,hre,min,
     + sec,jou,moi,ann
       integer ncelly,ncellz
       real lcellz(10),lcellx,lcelly,xcell0,ycell0,zcell0,tt(125,275,10)   
       character*60 ttfile  
       zcell0=0.
       print*,'Writing temperature file: ',ttfile
       open(unit=3,file=ttfile,status='unknown',err=224) 
          write(3,1226) 
          write(3,*) hre,min,sec,jou,moi,ann,' Date (HH MM SS DD MM
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
             write(3,1232) k
	     write(3,*) ((tt(i,j,k),j=1,ncelly),i=ncellx,1,-1)         
          enddo 
       close(unit=3)
 1226  format('# Temperature file for AODSEM')
 1232  format('# Level: ',I2)
       return
 224    print*,'File ',ttfile,' inexistant!'
       stop
       end             