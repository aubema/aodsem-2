c  calculer l humidite relative moyenne sur les niveaux aodsem
        programm rhmoyen
        integer valin(1000,1000),i,j,lennom,nlon,nlat,ii,n
        integer valou(1000,1000),l0,lf
        real pixsiz,lat0,lon0
        character*2 flaglf,flagl0
        character*60 nomfil,nomout
        open(unit=3,file='rhmoyen.par',status='old')
c        print*,'Input .rhu file name?'
        read(3,*) nomfil
        lennom=index(nomfil,' ')-1
        if (nomfil(lennom-3:lennom).ne.'.rhu') then
           print*,'This is not a .rhu file!'
           stop
        endif 
c        print*,'Starting level (1 to 10)?'
        read(3,*) l0
c        print*,'Last level (1 to 10)?'
        read(3,*) lf
        if ((lf.lt.l0).or.(lf.gt.10).or.(l0.lt.1)) then
           print*,'Bad level choice!'
           stop
        endif
        close(unit=3)
        
          
        if (l0.eq.1) flagl0='01'
        if (l0.eq.2) flagl0='02'
        if (l0.eq.3) flagl0='03'
        if (l0.eq.4) flagl0='04'        
        if (l0.eq.5) flagl0='05'
        if (l0.eq.6) flagl0='06'        
        if (l0.eq.7) flagl0='07'
        if (l0.eq.8) flagl0='08'
        if (l0.eq.9) flagl0='09'        
        if (l0.eq.10) flagl0='10'        
        if (lf.eq.1) flaglf='01'
        if (lf.eq.2) flaglf='02'        
        if (lf.eq.3) flaglf='03'
        if (lf.eq.4) flaglf='04'        
        if (lf.eq.5) flaglf='05'
        if (lf.eq.6) flaglf='06'  
        if (lf.eq.7) flaglf='07'
        if (lf.eq.8) flaglf='08'        
        if (lf.eq.9) flaglf='09'
        if (lf.eq.10) flaglf='10'        
              
        nomout=nomfil(1:lennom-4)//'_rhm'//flagl0//'-'//
     +  flaglf//'.pgm'
        do i=1,1000  
        do j=1,1000
          valou(i,j)=0
        enddo
        enddo  
        open(unit=1,file=nomfil,status='old',err=100)
          read(1,*)
          read(1,*)
          read(1,*) nlat
          read(1,*) pixsiz
          read(1,*) nlon
          read(1,*)
          read(1,*) ii
          do n=1,ii
             read(1,*)
          enddo
          read(1,*) lat0,lon0
          read(1,*)
          do n=1,ii
            read(1,*)
               read(1,*) ((valin(i,j),i=1,nlon),j=1,nlat)
           if ((n.ge.l0).and.(n.le.lf)) then
            do j=1,nlat
              do i=1,nlon
                 valou(i,j)=valou(i,j)+valin(i,j)
              enddo
            enddo
           endif  
          enddo
        close(unit=1)
        do j=1,nlat
           do i=1,nlon
             valou(i,j)=valou(i,j)/(lf-l0+1)
           enddo
        enddo 
       open(unit=2,file=nomout,status='unknown')
         print*,'Writing file: ',nomout
         write(2,1010) 'P2'
         write(2,1020) '# Mean vertical relative humidity level:'
     +   ,l0,'-',lf
         write(2,179) '# pixsiz ',pixsiz
         write(2,179) '# lat0   ',lat0
         write(2,179) '# lon0   ',lon0
         write(2,*) nlon, nlat
         write(2,*) '255'
         do 3411 j=1,nlat
            write(2,*) (valou(i,j),i=1,nlon)
 3411    continue
       close(unit=2)
 179   format(A,F8.3)
 1010  format(A2)  
 1020  format(A,1x,I2,A,I2)
        stop
 100    print*,'Can t find this file!'
        stop
        end
