c
c   compute de correlation coefficient f between two dataset
c   stored in a xy file begining by labels on the first line
c
c   label1   label2
c   1.1      1.11
c   2        1.8
c   .
c   .
c   .	
c
      program corcoef
c
c   declarations
c
      real x(34375),y(34375),r,n,sumxy,sumx,sumy,sumx2,sumy2
      integer i,filenlen
      character*60 filen,filout
c
c   initial values
c      
      n=0.
      r=0.
      sumxy=0.
      sumx=0.
      sumy=0.
      sumx2=0.
      sumy2=0.
c
c   inputs
c 
      open(unit=1,file='corcoef.par',status='old')
c       print*,'Input file name ?'
c      read*,filen
         read(1,*) filen
         read(1,*) filout
         read(1,*) xsca
         read(1,*) ysca
      close(unit=1)
      filenlen=index(filen,' ')-1  
c
c
      open(unit=1,file=filen,status='old')
      read(1,*)
      do i=1,5000       
        read(1,*,end=100) x(i),y(i)
        if (x(i).ne.0.) then
        if (y(i).ne.0.) then
         if (xsca.eq.1) x(i)=log10(x(i))
         if (ysca.eq.1) y(i)=log10(y(i))
         n=n+1.
         sumxy=sumxy+x(i)*y(i)
         sumx=sumx+x(i)
         sumy=sumy+y(i)
         sumx2=sumx2+x(i)**2.
         sumy2=sumy2+y(i)**2.
        endif
        endif
      enddo
 100  close(unit=1)
      r=(n*sumxy-sumx*sumy)/(sqrt(n*sumx2-sumx**2.)*sqrt(n*sumy2
     + -sumy**2.))   
      print*,'r = ',r 
      open(unit=1,file=filout,status='unknown')
         write(1,*) 'Correlation coefficient for file ',
     + filen(1:filenlen),' =',r
      close(unit=1)    
      stop
      end         
           
