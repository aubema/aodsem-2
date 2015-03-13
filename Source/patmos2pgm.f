       Program patmos2pgm
c      convert a patmos .out file into a pgm image
c
c      Declaration des variables
c
       character*80 header,infile
       integer ncellx,ncelly,i,j,maxval,tau(1000,1000),lennom
c
c      Initialisation
c
       ncellx=180
       ncelly=360
       maxval=255
c
c      Valeurs initiales
c
       Print*,'  PATMOS-2 .out file to pgm conversion program'
       print*,'  Martin Aube 1999'
       print*,' '
       print*,'Input patmos-2 file name?'
       read*,infile
c
c   calcul de la longueur du nom
c
      lennom=index(infile,' ')-1     
c
c      Lecture du fichier patmos
c
       Open(unit=3,file=infile,status='old',err=300)
          read(3,100) header
          read(3,*) ((tau(i,j),j=1,ncelly),i=1,ncellx)
       close(unit=3)
c
c      eliminer les valeurs negatives
c
       do 200 i=1,ncellx
          do 220 j=1,ncelly
             if (tau(i,j).le.0) tau(i,j)=254
 220      continue
 200   continue
c
c      Ecriture du fichier pgm
c
       open(unit=4,file=infile(1:lennom)//'.pgm',status='unknown')
          write(4,110)
          write(4,120) header
          write(4,130)
          write(4,131)
          write(4,132)
          write(4,133)
          write(4,*) ncelly,ncellx,maxval
          write(4,*) ((tau(i,j),j=1,ncelly),i=ncellx,1,-1)
       close(unit=4)
 100   format(A80)
 110   format('P2')
 120   format('#',A80)
 130   format('# lower-left pixel coords. lat=-90, lon=0')
 131   format('# pixsiz 1')
 132   format('# lat0 -90')
 133   format('# lon0 0')
       print*,'Fin normale du programme.'
 300   stop
       end
