c    Programme permettant proceder a une extrapolation spectrale
c    base sur une carte d epaisseur optique et une carte de 
c    coefficient d angstrom a une longueur d onde determinee. 
c
c    les axes sont definis comme suit
c    x = direction nord = degres
c    y = direction est = degres
c
c    Martin Aube 2000
c
c ------
c
c   Description des variables
c
c
      program aodalpha2aod
c
c --------------
c
c    Declaration des variables
c
      character*60 aotfi,nom(2),nomout,tag
      character*15 bidon
      character*2 deltat
      integer nxtcel,nytcel,nfile,n,lennom(2),hcnt,i,nx,ny
      integer oper,ia,neg,maxi,outlen,he,mi,se,jo,mo,an
      real val(2,500,1000),tauout(500,1000),ndat,wave1,wave2
      real gain,offset,value,lcellx,xcell0,ycell0
c ---------
c
c   Initialisation des variables
c
       pi=3.141592654
       pi2=pi*pi
       ndat=-1.
c
c -----------
c
c   Donnees de base
c 
      nfile=2
      open(unit=1,file='aodalpha2aod.par',status='old')
c       print*,'AOD file root name?'
         read(1,*) nom(1)
c       print*,'ALPHA file root name?'
         read(1,*) nom(2)
c       print*,'Gain and Offset (Phys.Val.=Gain*Num.Val.-Offset)?'
       read(1,*) gain,offset
c       print*,'First wavelenght (nm)?'
         read(1,*) wave1
c       print*,'Target wavelenght (nm)?'
         read(1,*) wave2
c       print*,'AOD output root name?'
         read(1,*) nomout
       
      close(unit=1)   


      outlen=index(nomout,' ')-1
      do 700 n=1,nfile
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
            read(27,*,end=56,err=57) bidon,tag,value
 57         if (bidon.eq.'#') then
              hcnt=hcnt+1
              if (tag(1:6).eq.'pixsiz') lcellx=value
              if (tag(1:4).eq.'lat0') xcell0=value
              if (tag(1:4).eq.'lon0') ycell0=value
              if (tag(1:4).eq.'date') then
                 backspace 27
                 read(27,*) bidon,tag,he,mi,se,jo,mo,an
              endif 
            endif 
 54      continue            
 56      rewind 27
         read(27,*)
         do 55 i=1,hcnt
            read(27,*)
 55      continue
         read(27,*) nytcel, nxtcel, maxi 
         print*,'Reading AOD data...'
         read(27,*) ((val(n,nx,ny),ny=1,nytcel),nx=nxtcel,1,-1)
         close(unit=27)  
 700  continue
c
c   convertir la valeur numerique en epaisseur optique
c   et en coefficient d angstrom
c
         do nx=1,nxtcel
           do ny=1,nytcel
               if (val(1,nx,ny).lt.65533.) then
                  val(1,nx,ny)=val(1,nx,ny)/1000.
               else
                  val(1,nx,ny)=65.534
               endif
               if (val(2,nx,ny).lt.65533.) then
                  val(2,nx,ny)=val(2,nx,ny)*gain-offset
               else
                  val(2,nx,ny)=65.534
               endif
           enddo
         enddo
c
c -----------
c
c   Proceed to arithmetics
c
      do 301 nx=1,nxtcel
         do 302 ny=1,nytcel
            tauout(nx,ny)=65.534
            if (val(1,nx,ny).le.65.533) then
            if (val(2,nx,ny).le.65.533) then
               tauout(nx,ny)=val(1,nx,ny)*(wave1/wave2)**val(2,nx,ny)
               ndat=ndat+1.
            endif
            endif  
c   coder sur 16 bit
            tauout(nx,ny)=tauout(nx,ny)*1000.        
 302     continue
 301  continue
c
c   assurer que l image de sortie est positive 
c
      if (neg.eq.1) then
        do nx=1,nxtcel
          do ny=1,nytcel
            if (tauout(nx,ny).lt.0.) tauout(nx,ny)=0.
          enddo
        enddo     
      endif     
c
c -----------
c
c   Writing output image
c
      nomout=nomout(1:outlen)//'.pgm'
      open(unit=27,file=nomout,status='unknown')
      print*,'Writing output image ',nomout
      write(27,2000) 'P2'
      write(27,2000) '#   Image from aodalpha2aod.f '
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
 2002 format(A,F10.3)
 180  format(A,I2,1x,I2,1x,I2,1x,I2,1x,I2,1x,I4)
      stop
      end
