c    programme pour ecrire une valeur sur une carte d epaisseur optique
c    Õ partir d'un fichier texte de longitude tatitude
c    
c    les axes sont definis comme suit
c    x = direction nord = degres
c    y = direction est = degres
c
c
c    copyright martin aube 10/05/1999
c
c -----------------
c   identification des variables 
c
c
c --------------------
c
c   programme principal
c
      program writeaod
c
c ----------
c
c   declaration des variables
c
      real pixsiz,lattau0,lontau0,tau(1000,1000)
      real lat(64800),lon(64000),value,tauxy(64800)
      character*60 bidon,nom,taufile,header,nomxy,tag
      integer nxcen,nycen,lennom,i,lenxy,he,mi,se,jo,mo,an
      integer nx,ny,nxtcel,nytcel,maxi,hcnt
c
c initialisation variables
c
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

      open(unit=11,file='writeaod.par',status='old')
        read(11,*) nom

c      print*,'root name of the image file (.pgm will be add) ?'  
c      read*, nom
c
c   calcul de la longueur du nom
c
      lennom=index(nom,' ')-1
      
        read(11,*) nomxy
       close(unit=11) 
c      print*,'Name of the x-y file ?'  
c      read*,nomxy
c
c   calcul de la longueur du nom
c
      lenxy=index(nomxy,' ')-1
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
         open(unit=2,file=taufile,status='old')
c
c   recherche de la position des headers
c
         bidon='#'
         hcnt=0
         read(2,*)
         do 54 i=1,50
            read(2,*,end=56,err=57) bidon,tag,value
 57         if (bidon(1:1).eq.'#') then
               hcnt=hcnt+1
               if (tag(1:6).eq.'pixsiz') pixsiz=value
               if (tag(1:4).eq.'lat0') lattau0=value
               if (tag(1:4).eq.'lon0') lontau0=value 
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
         if (lattau0*pixsiz*lontau0.eq.0.) then
            print*,'Horizontal pixel size in deg. ?'
            read*, pixsiz
 5         print*,'Center latitude of the south-west pixel in degrees?'
               read*,lattau0
               if (lattau0.lt.-90.) then
                  print*,'Should be greater than -90.'
                  goto 5
               endif
           print*,'Center longitude of the south-west pixel in degrees?'
               read*,lontau0
         endif
               if (lontau0.lt.0.) lontau0=lontau0+360.
               if ((real(nytcel)*pixsiz+lontau0).gt.360.) then
                  lontau0=0. 
                  print*,'Left longitude has been shifted to 0.'
               endif
               if (lontau0.gt.360.) lontau0=lontau0-360.
c
c   normaliser tau
c
         do 555 nx=1,nxtcel
         do 556 ny=1,nytcel
            tau(nx,ny)=tau(nx,ny)/1000.
 556     continue
 555     continue
c
c -----------------
c
c   Lecture des donnees
c
         nomxy=nomxy(1:lenxy)//'.xyt'
         open(unit=1,file=nomxy,status='old')
         read(1,*) nsite
         do 777 i=1,nsite
            read(1,*) lat(i),lon(i),tauxy(i)
            if (lon(i).lt.0.) lon(i)=lon(i)+360.
 777     continue
         do 778 i=1,nsite
            nxcen=nint((lat(i)-lattau0)/pixsiz+1.)
            nycen=nint((lon(i)-lontau0)/pixsiz+1.)
            if ((nxcen.le.nxtcel).and.(nxcen.gt.0)) then
            if ((nycen.le.nytcel).and.(nycen.gt.0)) then
               tau(nxcen,nycen)=tauxy(i)
            endif
            endif
 778     continue
         close(unit=1)
c
c ----------------
c
c   Ecriture de l image de sortie
c
         taufile=nom(1:lennom)//'_ed.pgm' 
         open(unit=3,file=taufile,status='unknown')
         write(3,1000) 'P2' 
         write(3,1000) '# Aod map edited with writeaod'
         write(3,180) '# date ',he,mi,se,jo,mo,an
         write(3,1010) pixsiz
         write(3,1011) lattau0
         write(3,1012) lontau0
 1010    format('# pixsiz ',F12.6)
 1011    format('# lat0 ',F7.3)
 1012    format('# lon0 ',F7.3)
 180  format(A,I2,1x,I2,1x,I2,1x,I2,1x,I2,1x,I4)
         write(3,*) nytcel, nxtcel
         write(3,*) maxi 
         print*,'Writing AOD data...'
         do 1014 nx=nxtcel,1,-1
            write(3,*) (nint(tau(nx,ny)*1000.),ny=1,nytcel)
 1014    continue           
         close(unit=3)
 1000    format(A)       
       end
