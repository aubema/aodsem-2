c    programme pour creer une image pgm buffer masque 0=buffer
c    1=zone d etude
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
      program mkbuffer
c
c ----------
c
c   declaration des variables
c
      real xcell0,ycell0,lcellx,lcelly,val
      real pixsiz,tauout(1000,1000),tau(1000,1000)     
      character*60 nom,aotfile,bidon,tag
      integer ncellx,ncelly,i,j,intype,autom,n1,n2,n3,n4,tampon
      integer nx,ny,ngris,lennom,maxi,hcnt,he,mi,se,jo,mo,an
c
c -----------
c
c   fichier de parametres
c
      open(unit=19,file="mkbuffer.par",status="old",err=12)
       read(19,*) nom    
       read(19,*) tampon   
      close(unit=19)   
c
c   calcul de la longueur du nom
c

      lennom=index(nom,' ')-1  
      aotfile=nom(1:lennom)//'.pgm'           
         open(unit=27,file=aotfile,status='old')
c
c --------------
c
c   recherche de la position des headers et lecture de  
c   l image
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
         read(27,*) ncelly, ncellx, maxi 
         print*,'Reading AOD data...'
         read(27,*) ((tau(nx,ny),ny=1,ncelly),nx=ncellx,1,-1)
         close(unit=27)           

c
c   appliquer le masque
c
      do i=1,ncellx
        do j=1,ncelly
             if (((i.le.tampon).or.(i.ge.ncellx-tampon+1)).or.
     +       ((j.le.tampon).or.(j.ge.ncelly-tampon+1))) then 
                tauout(i,j)=65534.
             else   
                tauout(i,j)=tau(i,j)
             endif
         enddo
       enddo
c
c ----------
c
c   fabrication d'un nouveau fichier pgm
c
      print*,'Applying buffer mask...'

         open(unit=27,file=aotfile,status="unknown")
         write(27,178) 'P2'
         write(27,178) '# + buffer mask'
         write(27,180) '# date ',he,mi,se,jo,mo,an
         write(27,2002) '#   pixsiz ',lcellx 
         write(27,2002) '#   lat0 ',xcell0 
         write(27,2002) '#   lon0 ',ycell0  
         write(27,*) ncelly, ncellx
         write(27,*) '65535'
         do 3411 i=ncellx,1,-1
               write(27,*) (nint(tauout(i,j)),j=1,ncelly)
 3411    continue
 178     format(A)
 180  format(A,I2,1x,I2,1x,I2,1x,I2,1x,I2,1x,I4)
 2002 format(A,F10.3)
         close(unit=27)
       stop
 12    print*,'Edit parameter file : mkbuffer.par by typing epar_buffe!'
       stop    
       end
