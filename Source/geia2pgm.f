c  programme pour convertir les fichiers GEIA
c  en fichier pgm
c
c
c
       programm geia2pgm
       integer i,li,lj,matrice(360,180),lat,lon,ftype,n,ii,jj
       integer latlon,lennom
       real value,frac(12)
       character*2 tt(12)
       character*60 geiaf,outf   
c 
c initialisation de variables
c
        data tt/'01','02','03','04','05','06','07','08','09',
     +   '10','11','12'/

        open(unit=8,file='geia2pgm.par',err=121)
c        print*,'GEIA file?'

        read(8,*) geiaf
c        print*,'File type? (0=annual,1=seasonal,2=monthly)'

        read(8,*) ftype 
        if (ftype.eq.0) then
              ncols=1
        elseif (ftype.eq.1) then
           ncols=4
        elseif (ftype.eq.2) then
           ncols=12
        else
           print*,'Bad file type!'
           stop
        endif
c        print*,'Output root file name?'

        read(8,*) outf
        close(unit=8)
        lennom=index(outf,' ')-1
       open(unit=1,file=geiaf,status='old')
        do n=1,ncols
          outf=outf(1:lennom)//tt(n)//'.pgm'
c  passer le header
          do i=1,10
             read(1,*)
          enddo  
c  lecture des donnees                  
          do i=1,64800 
             if (ncols.eq.1) then
                read(1,1000,end=10)lat,lon,value
             elseif (ncols.eq.4) then 
                read(1,*,end=10)latlon,value,frac(1),frac(2),
     +          frac(3),frac(4)
             elseif (ncols.eq.12) then   
                read(1,1030,end=10)lat,lon,frac(1),frac(2),
     +          frac(3),frac(4),frac(5),frac(6),frac(7),frac(8),
     +          frac(9),frac(10),frac(11),frac(12)
                value=1.
             endif
             if (ncols.eq.1) then   
                lj=-lat+181             
                li=lon 
                if (value.lt.0.1) then
                   matrice(li,lj)=0
                else           
                   matrice(li,lj)=nint(log10(10.*value)*40.)
                endif   
             elseif (ncols.eq.4) then
                lj=-latlon/1000+181
                li=latlon-latlon/1000*1000 
                if (value*frac(n).lt.0.1) then
                   matrice(li,lj)=0
                else                  
                   matrice(li,lj)=nint(log10(10.*4.*value*frac(n))*40.)
                endif
                
                
c                print*,matrice(li,lj),latlon,li,lj,value,frac(n)
                
                
             else
                lj=-lat+181             
                li=lon 
                if (frac(n).lt.0.1) then
                   matrice(li,lj)=0
                else                                      
                   matrice(li,lj)=nint(log10(10.*12.*value*frac(n))*40.)
                endif      
             endif
          enddo 
 10          rewind 1 
       open(unit=2,file=outf,status='unknown')
       print*,'Writing file: ',outf
         write(2,1010) 'P2'
         write(2,1020) '# numerical value=40*log10(10*Mtons/Yr)'
         write(2,*) '360 180'
         write(2,*) '255'
         do 3411 jj=1,180
            write(2,*) (matrice(ii,jj),ii=1,360)
 3411    continue
       close(unit=2)
       enddo           
       close(unit=1)  
 1000  format(I3,I3,1x,E10.4) 
 1030  format(I3,I3,1x,E10.4,1x,E10.4,1x,E10.4,1x,E10.4,
     + 1x,E10.4,1x,E10.4,1x,E10.4,1x,E10.4,1x,E10.4,1x,
     + E10.4,1x,E10.4,1x,E10.4)
 1010  format(A2)  
 1020  format(A)
       stop
 121   print*,'No geia2pgm.par file found!'
       stop
       end
         
