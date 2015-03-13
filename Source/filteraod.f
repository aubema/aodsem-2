c    Programme permettant filtrer avec un seuil 
c    une carte d epaisseur optique des aerosols
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
      program filteraod
c
c --------------
c
c    Declaration des variables
c
      character*60 aotfi,nom,date,tag
      character*15 bidon
      character*2 deltat
      integer nxtcel,nytcel,nfile,n,lennom,hcnt,i,nx,ny,datelen,maxi
      integer oper,ia
      real tau(500,1000),tauout(500,1000),value,lattau0,lontau0
     + ,lcellx,maximum
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
      nfile=1
      open(unit=1,file='filteraod.par',status='old')
         read(1,*) nom
         read(1,*) maximum
      close(unit=1)   
         value=value*100.
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
            read(27,*,end=56,err=57) bidon,tag,value
 57         if (bidon(1:1).eq.'#') then
               hcnt=hcnt+1
               if (tag(1:6).eq.'pixsiz') lcellx=value
               if (tag(1:4).eq.'lat0') lattau0=value
               if (tag(1:4).eq.'lon0') lontau0=value 
            endif
 54      continue            
 56      rewind 27

         read(27,*)
         do 55 i=1,hcnt
            read(27,*)
 55      continue
         read(27,*) nytcel, nxtcel, maxi 
         print*,'Reading AOD data...'
         read(27,*) ((tau(nx,ny),ny=1,nytcel),nx=nxtcel,1,-1)
         close(unit=27)
c
c -----------
c
c   Proceed to filtering
c
      do 301 nx=1,nxtcel
         do 302 ny=1,nytcel
            tauout(nx,ny)=254.
            if (tau(nx,ny).le.maximum) then
                tauout(nx,ny)=tau(nx,ny)
            endif          
 300        continue
 302     continue
 301  continue
c
c -----------
c
c   Writing output image
c
      date=nom(1:lennom)//'_f.pgm'
      open(unit=27,file=date,status='unknown')
      print*,'Writing output image ',date
      write(27,2000) 'P2'
      write(27,2001) '# Image from filteraod.f'
         write(27,179) '# pixsiz ',lcellx
         write(27,179) '# lat0 ',lattau0
         write(27,179) '# lon0 ',lontau0
      write(27,*) nytcel, nxtcel
      write(27,*) maxi 
      do 2004 nx=nxtcel,1,-1
         write(27,*) (nint(tauout(nx,ny)),ny=1,nytcel)
 2004 continue 
      close(unit=27)
 2000 format(A)
 179     format(A,F8.3)
 2001 format(A,A)
      stop
      end
