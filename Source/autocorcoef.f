c
c   compute de correlation coefficient f between two temporal
c   series stored in cxy file fromat.  The first axis of the cxy 
c   file have to be day of year (date) and the second is the 
c   variable to correlate.  To perform the correlation, the programm
c   will resample the second file to the date grid of the first file.
c   So ideally the second file should be the model result.  The user 
c   choose between two executing mode 1-compute correlation with a 
c   constant temporal offset between the files or 2-autocorrelation
c   algorithm were the offset is choosen to maximise the correlation
c   coefficient
c   

c
c   label1   label2
c   1.1      1.11
c   2        1.8
c   .
c   .
c   .	
c
      program autocorcoef
c
c   declarations
c
      real x1(34375),y1(34375),r,n,sumxy,sumx,sumy,sumx2,sumy2
      real x2(34375),y2(34375),offset,x2p(34375),y2p(34375)
      real y2out(34375),x2out(34375),chi,chi2
      real m,b,off,rmax,offmax,tmin,tmax
      integer i,ii,iii,filenlen1,filenlen2,ndat1,ndat2,debut,auto
      integer filoutlen,flagout(34375)
      character*60 filen1,filen2,filout,name
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
      rmax=0.
      nout=0.
      chimin=1000000.
c
c   inputs
c 
      open(unit=1,file='autocorcoef.par',status='old')
         read(1,*) filen1
         read(1,*) filen2
         read(1,*) auto
         read(1,*) offset
         read(1,*) filout
      close(unit=1)
      filenlen1=index(filen1,' ')-1  
      filenlen2=index(filen2,' ')-1
      filoutlen=index(filout,' ')-1
c
c
      name=filen1
      open(unit=1,file=filen1,status='old')
      read(1,*) ndat1
      do i=1,ndat1       
        read(1,*,end=100) x1(i),y1(i)
      enddo
      close(unit=1)
      name=filen2
      open(unit=2,file=filen2,status='old')
      read(2,*) ndat2

      do i=1,ndat2       
        read(2,*,end=100) x2(i),y2(i)
      enddo
      close(unit=2)


c
c    boucle d automatisation calcul a toutes les 15 min
c
 200  if (auto.eq.1) then 
         debut=1
         off=0.
      else
         debut=(abs(int(offset*96.))+1)
         off=offset
      endif
      do iii=debut,(abs(int(offset*96.))+1)
         if (offset.ne.0.) then
           off=off+(1./96.)*offset/abs(offset)
         endif
c
c     appliquer l offset
c
      do i=1,ndat2
         x2p(i)=x2(i)+off
      enddo
c
c    recherche du domaine des valeurs communes 
c
        if (x1(1).le.x2p(1)) then  
           tmin=x2p(1)
        else 
           tmin=x1(1)
        endif
        if (x1(ndat1).le.x2p(ndat2)) then  
           tmax=x1(ndat1)
        else 
           tmax=x2p(ndat2)
        endif 
c
c     interpoler y2 pour les valeurs de x1
c
      nout=0
      do i=1,ndat1
         flagout(i)=0
         do ii=1,ndat2-1
            if ((x1(i).ge.tmin).and.(x1(i).le.tmax)) then
            if ((x2p(ii).lt.x1(i)).and.(x2p(ii+1).ge.x1(i))) then
               x2out(i)=x1(i)
               m=(y2(ii+1)-y2(ii))/(x2p(ii+1)-x2p(ii))
               b=y2(ii)-m*x2p(ii)
               y2out(i)=m*x2out(i)+b
               flagout(i)=1
               nout=nout+1
            endif
            endif
         enddo
      enddo
c
c   compute correlation
c
      sumxy=0.
      sumx=0.
      sumy=0.
      sumx2=0.
      sumy2=0.
      do i=1,ndat1
       if (flagout(i).eq.1) then
        n=n+1.
        sumxy=sumxy+y1(i)*y2out(i)
        sumx=sumx+y1(i)
        sumy=sumy+y2out(i)
        sumx2=sumx2+y1(i)**2.
        sumy2=sumy2+y2out(i)**2.
       endif
      enddo
      r=(n*sumxy-sumx*sumy)/(sqrt(n*sumx2-sumx**2.)*sqrt(n*sumy2
     + -sumy**2.))   
c
c     calcul de l ecart quadratique moyen
c
      chi2=0.
      chi=0.
      do i=1,ndat1
       if (flagout(i).eq.1) then
         chi2=chi2+(y1(i)-y2out(i))**2.
       endif
      enddo
      chi=sqrt(chi2/nout)
      if (chi.lt.chimin) then
         chimin=chi
         offmax=off
      endif
      enddo
c
c     calcul final
c
      if (auto.eq.1) then
         auto=0
         offset=offmax
         goto 200
      endif
c
c    sorties
c  
c    fichier .cxy
c
      open(unit=6,file=filout(1:filoutlen)//'.cxy',status='unknown')
          write(6,*) nout,' data1 data2 doy offset=',offset
          do i=1,ndat1
           if (flagout(i).eq.1) then
             write(6,*) y1(i),y2out(i),x1(i)
           endif
          enddo
      close(unit=6)
      open(unit=1,file=filout,status='unknown')
         write(1,*) 'Correlation coefficient for file ',
     + filout(1:filoutlen),' =',r
         write(1,*) 'RMS difference= ',chimin
      close(unit=1)    
      stop
 100  print*,'*** Bad number of data in cxy file!', name
      stop
      end         
           
