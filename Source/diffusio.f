c    Programme permettant de creer un diffusiogramme spatial
c    a partir de deux cartes d epaisseur optique
c    des aerosols a une longueur d onde determinee
c    Cela permet de verifier s il y a correlation entre deux images
c    par exemple entre une observation et une modelisation
c    les valeurs superieures ou egales a 253 (tau>=2.53) ne sont pas
c    considerees. 
c    les axes sont definis comme suit
c    x = direction nord = degres
c    y = direction est = degres
c    z = direction haut = metres
c
c    Martin Aube 03-2000
c
c ------
c
c   Description des variables
c
c
      program composite
c
c --------------
c
c    Declaration des variables
c
      character*60 nom,aotfi
      character*15 bidon
      character*22 date
      character*2 deltat
      integer nxtcel,nytcel,n,lennom,hcnt,i,nx,ny,datelen,nt,maxi
      integer ia,ndata,buffer
      real tau(3,125,275)
c
c ---------
c
c   Initialisation des variables
c
       pi=3.141592654
       pi2=pi*pi
c
c -----------
c
c   Donnees de base
c 
      open(unit=13,file='diffusio.par',status='old')
c      print*,'Output root file name (.cxy will be add)'
c      read*,date
      read(13,*) date
      datelen=index(date,' ')-1
      do 700 n=1,2
      if (n.eq.1) then  
c         print*,'Root name of observation file (.pgm will be add)?'
c         read*,nom
         read(13,*) nom
      else
c         print*,'Root name of model results file (.pgm will be add)?'
c         read*,nom
         read(13,*) nom
      endif  
         lennom=index(nom,' ')-1
         aotfi=nom(1:lennom)//'.pgm'
         open(unit=27,file=aotfi,status='old')
c
c --------------
c
c   recherche de la position des headers et lecture de la premiere 
c   image
c
         bidon='#'
         hcnt=0
         read(27,*)
         do 54 i=1,50
            read(27,*,end=56) bidon
            if (bidon.eq.'#') hcnt=hcnt+1
 54      continue            
 56      rewind 27
         read(27,*)
         do 55 i=1,hcnt
            read(27,*)
 55      continue
         read(27,*) nytcel, nxtcel, maxi 
         print*,'Reading AOD data...'
         read(27,*) ((tau(n,nx,ny),ny=1,nytcel),nx=nxtcel,1,-1)
         close(unit=27)
 700  continue
c       print*,'Width of the buffer zone (in pixel)?'
c       read*,buffer 
        read(13,*) buffer
       close(unit=13)
c
c  Open output file
c 
         date=date(1:datelen)//'.cxy'
         open(unit=1,file=date,status='unknown')
        ndata=0
        do nx=1+buffer,nxtcel-buffer
         do ny=1+buffer,nytcel-buffer
            if ((tau(1,nx,ny).gt.0.).and.(tau(1,nx,ny).lt.253.)) then
              if ((tau(2,nx,ny).gt.0.).and.(tau(2,nx,ny).lt.253.)) then
                ndata=ndata+1
              endif
            endif    
         enddo
        enddo
         write(1,*), ndata,' Observ. 	Model'
c -----------
c
c   Scanning all data
c
      do 301 nx=1+buffer,nxtcel-buffer
         do 302 ny=1+buffer,nytcel-buffer
            if ((tau(1,nx,ny).gt.0.).and.(tau(1,nx,ny).lt.253.)) then
              if ((tau(2,nx,ny).gt.0.).and.(tau(2,nx,ny).lt.253.)) then
                write(1,*) tau(1,nx,ny)/100.,tau(2,nx,ny)/100.
              endif
            endif    
 302     continue
 301  continue
         close(unit=1)
      stop
      end
