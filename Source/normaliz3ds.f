c programme pour normaliser un fichier 3ds
c copyright martin aube 2004
c
       real val,x(17)
       integer i,m
       character tflag
       character*60 nomi,nomf
       open(unit=3,file='normaliz3ds.par',status='old')
         read(3,*) nomi
         read(3,*) nomf
         read(3,*) val
       close(unit=3)
c       print*,'Normalizing gain value (output val.=original val x gain)=?'
c       read*,val
       open(unit=1,file=nomi,status='old')
       open(unit=2,file=nomf,status='unknown')
         read(1,*) m, tflag
         write(2,*) m, tflag
         do i=1,m
         
        if (tflag.eq.'daily') then
         read(1,*) x(1),x(2),x(3),x(4),x(5),x(6),x(7),x(8),x(9),
     +   x(10),x(11),x(12),x(13),x(14)
         write(2,*) x(1),x(2),x(3),x(4),x(5),x(6),x(7),x(8),x(9),
     +   val*x(10),val*x(11),val*x(12),val*x(13),val*x(14)
        endif
        if (tflag.eq.'hourly') then
         read(1,*) x(15),x(1),x(2),x(3),x(16),x(4),x(5),x(6),x(7),
     +   x(8),x(9),x(10),x(11),x(12),x(13),x(14)
         write(2,*) x(15),x(1),x(2),x(3),x(16),x(4),x(5),x(6),x(7),
     +   x(8),x(9),val*x(10),val*x(11),val*x(12),val*x(13),val*x(14)
        endif
         enddo

       close(unit=2)
       close(unit=1)
       stop
       end