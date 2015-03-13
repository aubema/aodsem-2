c    programme pour interpoler une carte d epaisseur optique
c    vers un autre longueur d'onde en supposant soit un coefficient 
c    d Angstrom de 1.3 ou en consultant une image de coefficient d 
c    Angstrom *
c      
c    L interpolation est effectuee a l aide d une fonction 
c    tau1=tau0*(wave0/wave1)**alpha
c    
c    les axes sont definis comme suit
c    x = direction nord = degres
c    y = direction est = degres
c
c
c  * pas encore implemente
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
      program angstrom
c
c ----------
c
c   declaration des variables
c
      real lcellx,lcelly,alpdef,xcell0,ycell0
      real tau(1000,1000),val
      character*60 bidon,nom,aotfile,taufile,header,tag
      integer ncellx,ncelly,i,j
      integer percent,pcentold,lennom
      integer nx,ny,maxi,ngris,hcnt,he,mi,se,jo,mo,an
c   
c ----------
c
c   initialisation des variables
c
       alpdef=1.3
       he=0
       mi=0
       se=0
       jo=1
       mo=1
       an=1980
c
c      lecture du fichier de parametres
c
       open(unit=25,file='angstrom.par',status='old',err=8)
        read(25,*) nom
        read(25,*) wave0
        read(25,*) wave1
       close(unit=25)
c
c   choix du nom de la racine de fichiers
c
C      print*,'root name of the files (.pgm will be add) ?'  
C      read*, nom
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
c   fichier pgm pour les images polder 65534 et 65535=aucun signification
c   65533=cotes, autre=aod*1000
c
         taufile=nom(1:lennom)//'.pgm'
C         print*,'Source wavelength in nm?'
C         read*,wave0
C         print*,'Target wavelength in nm?'
C         read*,wave1
         open(unit=2,file=taufile,status='old')
c
c   recherche de la position des headers
c
         bidon='#'
         hcnt=0
         read(2,*)
         do 54 i=1,50
            read(2,*,end=56) bidon,tag,val
            if (bidon.eq.'#') then 
                 hcnt=hcnt+1
              if (tag(1:6).eq.'pixsiz') lcellx=val
              if (tag(1:4).eq.'lat0') xcell0=val
              if (tag(1:4).eq.'lon0') ycell0=val 
               if (tag(1:4).eq.'date') then
                 backspace 2
                 read(2,*) bidon,tag,he,mi,se,jo,mo,an
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
         read(2,*) ((tau(nx,ny),ny=1,ncelly),nx=ncellx,1,-1)
         close(unit=2)
c
c   normaliser tau
c
         do 555 nx=1,ncellx
         do 556 ny=1,ncelly
            tau(nx,ny)=tau(nx,ny)/1000.
 556     continue
 555     continue
c
c ---------
c
c   Interpoler la carte d epaisseur optique pour la nouvelle longueur
c   d onde
c
        print*,'Interpolating AOD map from',wave0,'nm to',wave1,'nm ...'
        do 113 nx=1,ncellx
           do 114 ny=1,ncelly
              if (tau(nx,ny).lt.65.533) then
                tau(nx,ny)=tau(nx,ny)*(wave0/wave1)**alpdef
              endif  
 114       continue
           percent=int((real(nx)/real(ncellx))*100./5.)*5
           if (percent.ne.pcentold) then
              print*,percent,'%'
           endif
           pcentold=percent
 113    continue
c
c   eliminer les saturations
c
           do i=1,ncellx
            do j=1,ncelly
               if (tau(i,j).gt.65.535) tau(i,j)=65.535
            enddo
	   enddo
c
c ----------
c
c   fabrication d'un nouveau fichier pgm
c
      print*,'making optical depth pgm image...'
         aotfile=nom(1:lennom)//'_w.pgm'
         open(unit=27,file=aotfile,status='unknown')
         write(27,178) 'P2'
         write(27,178) '# image d epaisseurs optiques interpolee par int
     $erp.f, (1000=aod 1)'
         write(27,180) '# date ',he,mi,se,jo,mo,an
         write(27,179) '# wavelength= ',wave1
         write(27,179) '# pixsiz ',lcellx
         write(27,179) '#   lat0 ',xcell0 
         write(27,179) '#   lon0 ',ycell0  
        write(27,*) ncelly, ncellx
         write(27,*) '65535'
         do 3411 i=ncellx,1,-1
              write(27,*) (nint(tau(i,j)*1000.),j=1,ncelly)
 3411    continue
 177     format(i3)
 178     format(A)
 179     format(A,F8.3)
 180  format(A,I2,1x,I2,1x,I2,1x,I2,1x,I2,1x,I4)
         close(unit=27)
       stop
 8     print*,'Parameter file: angstrom.par don t exist.'
       stop
       end
