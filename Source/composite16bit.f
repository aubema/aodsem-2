c    Programme permettant de combiner deux cartes d epaisseur optique
c    des aerosols a une longueur d onde determinee
c
c    les axes sont definis comme suit
c    x = direction nord = degres
c    y = direction est = degres
c    z = direction haut = metres
c
c    Martin Aube 01-2000
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
      character*60 nom,aotfi,tag
      character*15 bidon
      character*22 date
      character*2 deltat
      integer nxtcel,nytcel,nfile,n,lennom,hcnt,i,nx,ny,datelen,nt,maxi
      integer oper,ia
      real tau(31,2086,1600),ndata,tauout(2086,1600),errtot,error(31)
      real lcellx,xcell0,ycell0,val
c
c ---------
c
c   Initialisation des variables
c
       pi=3.141592654
       pi2=pi*pi
       do 40 ia=1,10
         error(ia)=1.
 40    continue 
       lcellx=0.       
c
c -----------
c
c   Donnees de base
c 
     
      Open(unit=13,file='composite.par',status='old')
         read(13,*) nfile
         read(13,*) oper
         read(13,*) date
         read(13,*) nt
         
c     print*,'Number of pgm images files to combine?'
c      read*,nfile       
c      print*,'Choose an operation:'
c      print*,' '
c      print*,'   1 ..... Composite from same instrumental source'
c      print*,'   2 ..... Composite from different instrumental sources'
c      print*,'   3 ..... Add'
c      read*,oper
c      print*,'Output root file name (.pgm will be add)'
c      read*,date
      datelen=index(date,' ')-1
c 9    print*,'Compositing period ?'
c      print*,' '
c      print*,'   1 ....... hour'
c      print*,'   2 ....... day'
c      print*,'   3 ....... week'
c      print*,'   4 ....... month'
c      print*,'   5 ....... year'
c      print*,'   6 ....... other'
c      read*,nt
      if (nt.eq.1) deltat='hr'
      if (nt.eq.2) deltat='dy'
      if (nt.eq.3) deltat='wk'
      if (nt.eq.4) deltat='mo'
      if (nt.eq.5) deltat='yr'
      if (nt.eq.6) deltat='ot'
c      if ((nt.gt.6).or.(nt.lt.1)) goto 9

      do 700 n=1,nfile
          read(13,*) nom,error(n)
c 7       print*,'Root name of file #',n,' (.pgm will be add)?'
c         read*,nom
         if (oper.eq.2) then
            print*,'Error associated with dataset #',n,'=',error(n)
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
            read(27,*,end=56,err=57) bidon, tag,val
            goto 58
 57         backspace 27
            read(27,*,end=56) bidon
 58         if (bidon(1:1).eq.'#') then
               hcnt=hcnt+1
               if (tag(1:6).eq.'pixsiz') lcellx=val
               if (tag(1:4).eq.'lat0') xcell0=val
               if (tag(1:4).eq.'lon0') ycell0=val 
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
         do nx=1,nxtcel
           do ny=1,nytcel
              if (tau(n,nx,ny).lt.0.) then
                 tau(n,nx,ny)=0.
              endif
           enddo
         enddo                 
 700  continue
      close(unit=13)
c
c -----------
c
c   Combining all data
c
      do 301 nx=1,nxtcel
         do 302 ny=1,nytcel
            ndata=0.
            errtot=0.
            tauout(nx,ny)=0.
            do 300 n=1,nfile
               if ((tau(n,nx,ny).ne.0.).and.(tau(n,nx,ny).lt.65533.)) 
     +         then
                  ndata=ndata+1.
                  tauout(nx,ny)=tauout(nx,ny)+tau(n,nx,ny)/error(n)
                  errtot=errtot+1./error(n)
               endif
 300        continue
            if (oper.eq.3) errtot=1.
            if (ndata.gt.0.) then
                  tauout(nx,ny)=tauout(nx,ny)/errtot
            else
               tauout(nx,ny)=65534.
            endif
 302     continue
 301  continue
c
c -----------
c
c   Writing composite image
c
      date=date(1:datelen)//'_'//deltat(1:2)//'.pgm'
      open(unit=27,file=date,status='unknown')
      print*,'Writing output image ',date
      write(27,2000) 'P2'
      write(27,2001) '# Composite image: ',date(1:datelen+3)
      if (lcellx.ne.0.) then
         write(27,2002) '# pixsiz= ',lcellx
         write(27,2002) '# lat0= ',xcell0
         write(27,2002) '# lon0= ',ycell0
      endif
      write(27,*) nytcel, nxtcel
      write(27,*) maxi 
      do 2004 nx=nxtcel,1,-1
         write(27,*) (nint(tauout(nx,ny)),ny=1,nytcel)
 2004 continue 
      close(unit=27)
 2000 format(A)
 2001 format(A,A)
 2002 format(A,F8.3)
      stop
      end



