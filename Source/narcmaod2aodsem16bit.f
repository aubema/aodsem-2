c
c   Programme permettant de generer les champs meteo AODSEM
c   a partir des fichiers ASCII provenant du modele NARCM.
c
c
       program narcmaod2aodsem
c
c   Declaration des variables
c
       integer nn,nlign,nfield,ncellx,ncelly,begind(200),endda,nldat
       integer i,j,lenom,nexp,heure,jour,mois,annee
       integer nfile,ncely,ncelx,reclen,recnum,level,n,o,p
       real donnee(50,100,1),lcellx,lcelly,xcell0,ycell0
       real m,b,dt,pi
       real*8 hre,min,sec,jou,moi,ann,href,minf,secf,jouf,moif,annf
       real*8 dh,jday,jdayf
       character l1,l2,l3,l4,l7,l8,l9,l10
       character*3 bidon1 
       character*2 l56
       character*4 bidon
       character*60 gemfi1,nom
       character*132 ligne
       character*70 header
c
c   Initialisation des variables
c
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

      open(unit=15,file='narcmaod2aodsem.par',err=121)     
c       print*,'NARCM ASCII aod file name?'

       read(15,*) nom
       lenom=index(nom,' ')-1
c       print*,'Date of the first dataset (HH MM SS JJ MM YYYY)?'

       read(15,*) hre,min,sec,jou,moi,ann
       call julian(hre,min,sec,jou,moi,ann,jday)
c       print*,'Date of the last dataset (HH MM SS JJ MM YYYY)?'

       read(15,*) href,minf,secf,jouf,moif,annf 
       call julian(href,minf,secf,jouf,moif,annf,jdayf)    
c       print*,'Time interval between each NARCM files (hour) ?'

       read(15,*) dh
        dt=dh*3600.
        nexp=nint((jdayf-jday)/(dh/24.))+1       
c       print*,'Latitude of the S-W cell?'


       read(15,*) xcell0
       print*,'Longitude of the S-W cell?'  
       read(15,*) ycell0    
       close(unit=15)
c       
c   Ouverture des fichiers NARCM
c
       open(unit=16,file=nom(1:lenom),status='old')
       open(unit=1,file=nom(1:lenom)//'R.aod',status='unknown')
c
c   Inpecteur d un  fichier NARCM 
c
c   Nombre de lignes et colonnes du fichier NARCM, 
c   meteo et de la position du debut de chaque variable meteo.
c   Recherche des titre de variables et des niveaux de pression ou 
c   de l heure pour la variable de precipitation PR   
c
      print*,'Inspecting file format' 
      read(16,*) bidon
      read(16,*) bidon
      read(16,*) bidon
      read(16,*) bidon
      read(16,*) bidon, bidon,bidon, level, ncelly, ncellx,lcellx
      lcelly=lcellx
      nldat=0
      bidon='bla '
      dowhile (bidon.ne.'SUBA')
         read(16,*) bidon
         nldat=nldat+1
      enddo 
         nldat=nldat-1
      rewind 16
c
c   reformattage des fichiers
c
       print*,'Reformatting NARCM files'
       do n=1,4
          read(16,*) bidon1
       enddo  
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
          close(unit=1)
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
         gemfi1=l1//l2//l3//l4//l56//l7//l8//l9//l10//'.pgm'  
         print*,'Creating file ',gemfi1(1:10)//'.pgm',' (',
     +nex,'/',nexp-1,')'   
       open(unit=1,file=nom(1:lenom)//'R.aod',status='OLD',
     + access='direct',form='formatted',recl=reclen)


c
c   Lecture de l AOD
c  
       print*,'Loading AOD field '

         do i=1,ncellx
           do j=1,ncelly
             recnum=(i-1)*ncelly+j+(nex-1)*ncellx*ncelly
             read(1,'(E11.3E2)',rec=recnum) donnee(i,j,1)
           enddo
         enddo                          
      
C
C   CONVERTIR DE reel a un codage aod*1000 et elimination
c   des valeurs negatives
C  
       do i=1,ncellx
       do j=1,ncelly
         if (donnee(i,j,1).lt.0.) donnee(i,j,1)=0.
         donnee(i,j,1)=donnee(i,j,1)*1000.
       enddo
       enddo           
c
c   Ecriture des fichiers de sortie
c 
       open(unit=27,file=gemfi1,status='unknown')
         write(27,171) 'P2'
         write(27,174) 
            write(27,179) '# pixsiz ',lcellx
            write(27,179) '# lat0   ',xcell0
            write(27,179) '# lon0   ',ycell0        
            write(27,172) ncelly, ncellx
            write(27,*) '65535'
            do 3411 i=ncellx,1,-1
                  write(27,*) (nint(donnee(i,j,1)),j=1,ncelly)
 3411       continue
 177  format(i3)
 171  format(a)
 172  format(i6,1x,i6)
 174  format('# AOD map from NARCM (1000= aod 1)')
 179  format(A,F8.3)
       close(unit=27)


  
       jday=jday+dh/24. 
 5000  continue  
       close(unit=1)
       stop
 121   print*,'No narcmaod2aodsem.par file found!'
       stop
       end   

