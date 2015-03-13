c    Programme permettant proceder a des operations arithmetiques de 
c    base sur une carte d epaisseur optique
c    des aerosols a une longueur d onde determinee
c
c    les axes sont definis comme suit
c    x = direction nord = degres
c    y = direction est = degres
c    z = direction haut = metres
c
c    Martin Aube 05-2000
c
c ------
c
c   Description des variables
c
c
      program aodarit
c
c --------------
c
c    Declaration des variables
c
      character*60 aotfi,nom(1),tag
      character*15 bidon
      character*22 date
      character*2 deltat
      integer nxtcel,nytcel,nfile,n,lennom(1),hcnt,i,nx,ny,datelen,maxi
      integer oper,ia,neg,he,mi,se,jo,mo,an
      real tau(1,2200,2200),tauout(2200,2200),value,val
      real lcellx,lcelly,xcell0,ycell0,limit
c ---------
c
c   Initialisation des variables
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
c   Donnees de base
c 
      nfile=1
      open(unit=1,file='simplearit.par',status='old')
         read(1,*) oper
         read(1,*) date
         read(1,*) nom(1)
         read(1,*) value
         read(1,*) neg
         read(1,*) limit
      close(unit=1)   
c      print*,'Choose an operation:'
c      print*,' '
c      print*,'   1 ..... +'
c      print*,'   2 ..... -'
c      print*,'   3 ..... x'
c      print*,'   4 ..... /'
c      read*,oper
c      print*,'Output root file name (.pgm will be add)'
c      read*,date
      datelen=index(date,' ')-1
      do 700 n=1,nfile
c 7       print*,'Root name of file #',n,' (.pgm will be add)?'
c         read*,nom
         lennom(n)=index(nom(n),' ')-1
         aotfi=nom(n)(1:lennom(n))//'.pgm'
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
            read(27,*,end=56,err=57) bidon,tag,val
 57         if (bidon.eq.'#') then
              hcnt=hcnt+1
              if (tag(1:6).eq.'pixsiz') lcellx=val
              if (tag(1:4).eq.'lat0') xcell0=val
              if (tag(1:4).eq.'lon0') ycell0=val 
               if (tag(1:4).eq.'date') then
                 backspace 27
                 read(27,*) bidon,tag,he,mi,se,jo,mo,an
               endif
              lcelly=lcellx
            endif
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
c
c -----------
c
c   Proceed to arithmetics
c
      do 301 nx=1,nxtcel
         do 302 ny=1,nytcel
            tauout(nx,ny)=254.
            if (tau(1,nx,ny).lt.limit) then
               if (oper.eq.1) then
                  tauout(nx,ny)=tau(1,nx,ny)+value
               elseif (oper.eq.2) then
                  tauout(nx,ny)=tau(1,nx,ny)-value  
               elseif (oper.eq.3) then
                  tauout(nx,ny)=tau(1,nx,ny)*value 
               elseif (oper.eq.4) then
                  tauout(nx,ny)=tau(1,nx,ny)/value 
               endif   
            endif  
            if (neg.eq.1) then
               if (tauout(nx,ny).lt.0.) then 
                  tauout(nx,ny)=0.   
               endif 
            endif 
 300        continue
 302     continue
 301  continue
c
c -----------
c
c   Writing output image
c
      date=date(1:datelen)//'.pgm'
      open(unit=27,file=date,status='unknown')
      print*,'Writing output image ',date
      write(27,2000) 'P2'
      write(27,2001) '#   Image from aodarit.f'
      write(27,180) '# date ',he,mi,se,jo,mo,an
      write(27,2002) '#   pixsiz ',lcellx 
      write(27,2002) '#   lat0 ',xcell0 
      write(27,2002) '#   lon0 ',ycell0  
      write(27,*) nytcel, nxtcel
      write(27,*) maxi 
      do 2004 nx=nxtcel,1,-1
         write(27,*) (nint(tauout(nx,ny)),ny=1,nytcel)
 2004 continue 
      close(unit=27)
 2000 format(A)
 2001 format(A,A)
 2002 format(A,F10.3)
 180  format(A,I2,1x,I2,1x,I2,1x,I2,1x,I2,1x,I4)
      stop
      end

