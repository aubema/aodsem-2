c   Programme aerosol model zones
c
c    copyright martin aube 09/04/1999

      program coloraod
c
c ----------
c
c   declaration des variables
c
      real landsea(180,360),urban(180,360)
      integer i,j,r(180,360),g(180,360),b(180,360)
c
c ----------
c
c   donnees initiales

c
c ----------
c
c   lecture de l image d epaisseur optique interpolee avec interp.f
c
         open(unit=27,file='land-sea.pgm',status='old')
      print*,'Loading land-sea pgm image...'
         do 55 i=1,3
            read(27,*)
 55      continue
         read(27,*) ((landsea(i,j),j=1,360),i=180,1,-1)
         close(unit=27)
         open(unit=28,file='urbanmask.pgm',status='old')
      print*,'Loading urban mask pgm image...'
         do 56 i=1,3
            read(28,*)
 56      continue
         read(28,*) ((urban(i,j),j=1,360),i=180,1,-1)
         close(unit=28)
c
c   code couleur
c
         do 300 i=1,180
         do 400 j=1,360
                   if (urban(i,j).eq.0.) then
c      100% rural
                      r(i,j)=0
                      g(i,j)=150
                      b(i,j)=0
                   elseif (urban(i,j).eq.1.) then
c      78% rural
                      r(i,j)=100
                      g(i,j)=100
                      b(i,j)=0
                   elseif (urban(i,j).eq.2.) then
c      34% rural
                      r(i,j)=210
                      g(i,j)=50
                      b(i,j)=0
                   elseif (urban(i,j).eq.3.) then
c x     100% urban case
                      r(i,j)=255
                      g(i,j)=0
                      b(i,j)=0
                   endif
		if (landsea(i,j).eq.0.) then
c
c   100% mer
c
                   b(i,j)=50
                   r(i,j)=0
                   g(i,j)=0
                elseif (landsea(i,j).eq.1.) then
c
c   33% terre
c
                   b(i,j)=190

                   g(i,j)=60
                elseif (landsea(i,j).eq.2.) then
c
c   66% terre
c
                   b(i,j)=250

c                   g(i,j)=100
                elseif (landsea(i,j).eq.3.) then
c
c   100% terre
c
                   b(i,j)=0
c                   r(i,j)=r(i,j)
c                   g(i,j)=g(i,j)
                endif
 400     continue
 300     continue
      print*,'Creating model zone ppm image...'
         open(unit=26,file='modelzone.ppm',status='new')
             write(26,171) 'P3'
             write(26,171) '360 180'
             write(26,171) '255'
             write(26,*) ((r(i,j),g(i,j),b(i,j),j=1,360),i=180,1,-1)       
         close(unit=26)
 171  format(a)
         stop
         end


