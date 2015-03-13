c    programme generer une carte d epaisseur optique
c    pour une longueur d'onde quelconque a partir de deux cartes
c    d epaisseur optiques a differentes longueurs d ondes
c      
c    L interpolation est effectuee a l aide d une fonction 
c    tau1=tau0*(wave0/wave1)**alpha
c    
c    les axes sont definis comme suit
c    x = direction nord = degres
c    y = direction est = degres
c
c
c
c    copyright Martin Aube 2000
c
c -----------------
c   identification des variables 
c
c
c --------------------
c
c   programme principal
c
      program specint
c
c ----------
c
c   declaration des variables
c
      real lcellx,lcelly,wave2,wave1,wavef,lattau0,lontau0
      real tau1(1000,1000),tau2(1000,1000),tauf(1000,1000)
     + ,value
      character*60 bidon,nom1,nom2,nomf,file1,file2,filef,header
     + ,tag
      integer ncellx,ncelly,i,j,he,mi,se,jo,mo,an
      integer percent,pcentold,lenno1,lenno2,lennof
      integer nx,ny,maxi,ngris,hcnt
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
c -----------
c
c   choix du nom de la racine de fichiers
c
      open(unit=13,file='specint.par',status='old')
      read(13,*) nom1
c      print*,'root name of the 1st file (.pgm will be add) ?'  
c      read*, nom1
      read(13,*) wave1
c         print*,'Wavelength in nm?'
c         read*,wave1
      read(13,*) nom2
c      print*,'root name of the 2nd file (.pgm will be add) ?'  
c      read*, nom2
      read(13,*) wave2
c         print*,'Wavelength in nm?'
c         read*,wave2
      read(13,*) nomf
c      print*,'root name of the target file (.pgm will be add) ?'  
c      read*, nomf
      read(13,*) wavef
      close(unit=13)
c         print*,'Target wavelength in nm?'
c         read*,wavef
c
c   calcul de la longueur du nom
c
      lenno1=index(nom1,' ')-1
      lenno2=index(nom2,' ')-1
      lennof=index(nomf,' ')-1
c
c
c -----------
c
c   Lecture du 1er fichier d'epaisseur optique
c
c   fichier pgm pour les images polder 65534 et 65535=aucun signification
c   65533=cotes, autre=aod*1000
c
         file1=nom1(1:lenno1)//'.pgm'
         open(unit=2,file=file1,status='old')
c
c   recherche de la position des headers
c
         bidon='#'
         hcnt=0
         read(2,*)
         do 54 i=1,50
            read(2,*,end=56,err=57) bidon,tag
 57         if (bidon(1:1).eq.'#') then
               hcnt=hcnt+1
               if (tag(1:6).eq.'pixsiz') then
                 backspace 2
                 read(2,*,end=56,err=57) bidon,tag,value
                 lcellx=value
               endif
               if (tag(1:4).eq.'lat0') then
                 backspace 2
                 read(2,*,end=56,err=57) bidon,tag,value
                 lattau0=value
               endif
               if (tag(1:4).eq.'lon0') then
                 backspace 2
                 read(2,*,end=56,err=57) bidon,tag,value
                 lontau0=value
               endif 
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
         read(2,*) ncelly, ncellx, maxi 
         print*,'Reading AOD data...'
         read(2,*) ((tau1(nx,ny),ny=1,ncelly),nx=ncellx,1,-1)
         close(unit=2)
c
c   normaliser tau
c
         do 555 nx=1,ncellx
         do 556 ny=1,ncelly
            tau1(nx,ny)=tau1(nx,ny)/1000.
 556     continue
 555     continue
c
c
c -----------
c
c   Lecture du 2e fichier d'epaisseur optique
c
c   fichier pgm pour les images polder 65534 et 65535=aucun signification
c   65533=cotes, autre=aod*1000
c
         file2=nom2(1:lenno2)//'.pgm'
         open(unit=3,file=file2,status='old')
c
c   recherche de la position des headers
c
         bidon='#'
         hcnt=0
         read(3,*)
         do 154 i=1,50
            read(3,*,end=156) bidon
            if (bidon.eq.'#') hcnt=hcnt+1
 154     continue            
 156     rewind 3
         read(3,*)
         do 155 i=1,hcnt
            read(3,*)
 155     continue
         read(3,*) ncelly, ncellx, maxi 
         print*,'Reading AOD data...'
         read(3,*) ((tau2(nx,ny),ny=1,ncelly),nx=ncellx,1,-1)
         close(unit=3)
c
c   normaliser tau
c
         do 1555 nx=1,ncellx
         do 1556 ny=1,ncelly
            tau2(nx,ny)=tau2(nx,ny)/1000.
 1556    continue
 1555    continue
c
c
c ---------
c
c   Interpoler la carte d epaisseur optique pour la nouvelle longueur
c   d onde
c
        print*,'Interpolating AOD map to',wavef,'nm ...'
        do 113 nx=1,ncellx
           do 114 ny=1,ncelly
              alpha=log(tau1(nx,ny)/tau2(nx,ny))/log(wave2/wave1)
              tauf(nx,ny)=tau1(nx,ny)*(wave1/wavef)**alpha
 114       continue
           percent=int((real(nx)/real(ncellx))*100./5.)*5
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
         filef=nomf(1:lennof)//'.pgm'
      print*,'making optical depth pgm image...'
         open(unit=27,file=filef,status='unknown')
         write(27,178) 'P2'
         write(27,178) '# image d epaisseurs optiques interpolee par int
     $erp.f, (1000=aod 1)'
         write(27,179) '# wavelength=',wave1
         write(27,180) '# date ',he,mi,se,jo,mo,an
         write(27,179) '# pixsiz ',lcellx
         write(27,179) '# lat0 ',lattau0
         write(27,179) '# lon0 ',lontau0

        write(27,*) ncelly, ncellx
         write(27,*) '65535'
         do 3411 i=ncellx,1,-1
            do 3311 j=1,ncelly
               ngris=nint(tauf(i,j)*1000.)
               if (ngris.gt.65535) ngris=65535
               write(27,177) ngris
 3311       continue
 3411    continue
 177     format(i3)
 178     format(A)
 179     format(A,F8.3)
 180  format(A,I2,1x,I2,1x,I2,1x,I2,1x,I2,1x,I4)
         close(unit=27)
       stop
       end
