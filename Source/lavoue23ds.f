c  programme pour convertir les fichiers GEIA
c  en fichier 3ds de AODSEM
c
c
       programm lavoue23ds
       integer i,li,lj,matrice(360,180),lat,lon,ftype,n,ii,jj
       integer latlon,lennom,aertype,nbdata,nbdat
       real value,frac(12),height,latitu,longit
       character*60 geiaf,outf,bidon   
c 
c initialisation de variables
c
      open(unit=11,file='lavoue23ds.par',status='old')
c        print*,'Lavoue file?'
        read(11,*) geiaf
        ncols=12
c  aertype = type d'aerosol 1=su,2=bc,3=sd,4=ss,5=oc
c        print*,'Type d''aerosol (1=su,2=bc,3=sd,4=ss,5=oc)?'
        read(11,*) aertype
c        print*,'Output root file name?'
        read(11,*) outf
        lennom=index(outf,' ')-1
        outf=outf(1:lennom)//'.3ds'
       close(unit=11)
       open(unit=1,file=geiaf,status='old')
c
c  determiner le nombre de lignes de donnees
c
       do i=1,64810

        read(1,*,end=10) bidon
        nbdata=i-1
       enddo
 10       rewind 1
       print*,'nbdata=',nbdata
c
c  passer le header
c
          do i=1,1
             read(1,*)
          enddo  
c  lecture et ecriture des donnees  
       open(unit=2,file=outf,status='unknown')
       print*,'Writing file: ',outf
         nbdat=nbdata*ncols
         write(2,*) nbdat,' daily  number of data line units=tons/year/
     +deg^2'
          do i=1,nbdata    
                read(1,1030)lat,lon,frac(1),frac(2),
     +          frac(3),frac(4),frac(5),frac(6),frac(7),frac(8),
     +          frac(9),frac(10),frac(11),frac(12),height
   
c
c  passer des unites de grammes par mois par deg^2 de Lavoue a l'equivalent
c  en tonnes/an/deg^2
c
             value=12.
             do n=1,12
               frac(n)=frac(n)/1000000.
             enddo



                latitu=-90.501+real(lat)         
                longit=-180.5+real(lon)
                if (aertype.eq.1) then                  
                  write(2,*) '01 01 1980 31 01 1980 ',latitu,
     +            longit,height,value*frac(1),' 0. 0. 0. 0.'
                  write(2,*) '01 02 1980 28 02 1980 ',latitu,
     +            longit,height,value*frac(2),' 0. 0. 0. 0.'
                  write(2,*) '01 03 1980 31 03 1980 ',latitu,
     +            longit,height,value*frac(3),' 0. 0. 0. 0.'
                  write(2,*) '01 04 1980 30 04 1980 ',latitu,
     +            longit,height,value*frac(4),' 0. 0. 0. 0.'
                  write(2,*) '01 05 1980 31 05 1980 ',latitu,
     +            longit,height,value*frac(5),' 0. 0. 0. 0.'
                  write(2,*) '01 06 1980 30 06 1980 ',latitu,
     +            longit,height,value*frac(6),' 0. 0. 0. 0.'
                  write(2,*) '01 07 1980 31 07 1980 ',latitu,
     +            longit,height,value*frac(7),' 0. 0. 0. 0.'
                  write(2,*) '01 08 1980 31 08 1980 ',latitu,
     +            longit,height,value*frac(8),' 0. 0. 0. 0.'
                  write(2,*) '01 09 1980 30 09 1980 ',latitu,
     +            longit,height,value*frac(9),' 0. 0. 0. 0.'
                  write(2,*) '01 10 1980 31 10 1980 ',latitu,
     +            longit,height,value*frac(10),' 0. 0. 0. 0.'
                  write(2,*) '01 11 1980 30 11 1980 ',latitu,
     +            longit,height,value*frac(11),' 0. 0. 0. 0.'
                  write(2,*) '01 12 1980 31 12 1980 ',latitu,
     +            longit,height,value*frac(12),' 0. 0. 0. 0.'
                elseif (aertype.eq.2) then
                  write(2,*) '01 01 1980 31 01 1980 ',latitu,
     +            longit,height,' 0. ',value*frac(1),' 0. 0. 0.'
                  write(2,*) '01 02 1980 28 02 1980 ',latitu,
     +            longit,height,' 0. ',value*frac(2),' 0. 0. 0.'
                  write(2,*) '01 03 1980 31 03 1980 ',latitu,
     +            longit,height,' 0. ',value*frac(3),' 0. 0. 0.'
                  write(2,*) '01 04 1980 30 04 1980 ',latitu,
     +            longit,height,' 0. ',value*frac(4),' 0. 0. 0.'
                  write(2,*) '01 05 1980 31 05 1980 ',latitu,
     +            longit,height,' 0. ',value*frac(5),' 0. 0. 0.'
                  write(2,*) '01 06 1980 30 06 1980 ',latitu,
     +            longit,height,' 0. ',value*frac(6),' 0. 0. 0.'
                  write(2,*) '01 07 1980 31 07 1980 ',latitu,
     +            longit,height,' 0. ',value*frac(7),' 0. 0. 0.'
                  write(2,*) '01 08 1980 31 08 1980 ',latitu,
     +            longit,height,' 0. ',value*frac(8),' 0. 0. 0.'
                  write(2,*) '01 09 1980 30 09 1980 ',latitu,
     +            longit,height,' 0. ',value*frac(9),' 0. 0. 0.'
                  write(2,*) '01 10 1980 31 10 1980 ',latitu,
     +            longit,height,' 0. ',value*frac(10),' 0. 0. 0.'
                  write(2,*) '01 11 1980 30 11 1980 ',latitu,
     +            longit,height,' 0. ',value*frac(11),' 0. 0. 0.'
                  write(2,*) '01 12 1980 31 12 1980 ',latitu,
     +            longit,height,' 0. ',value*frac(12),' 0. 0. 0.'  
                elseif (aertype.eq.3) then
                  write(2,*) '01 01 1980 31 01 1980 ',latitu,
     +            longit,height,' 0. 0. ',value*frac(1),' 0. 0. '
                  write(2,*) '01 02 1980 28 02 1980 ',latitu,
     +            longit,height,' 0. 0. ',value*frac(2),' 0. 0.'
                  write(2,*) '01 03 1980 31 03 1980 ',latitu,
     +            longit,height,' 0. 0. ',value*frac(3),' 0. 0.'
                  write(2,*) '01 04 1980 30 04 1980 ',latitu,
     +            longit,height,' 0. 0. ',value*frac(4),' 0. 0.'
                  write(2,*) '01 05 1980 31 05 1980 ',latitu,
     +            longit,height,' 0. 0. ',value*frac(5),' 0. 0. '
                  write(2,*) '01 06 1980 30 06 1980 ',latitu,
     +            longit,height,' 0. 0. ',value*frac(6),' 0. 0.'
                  write(2,*) '01 07 1980 31 07 1980 ',latitu,
     +            longit,height,' 0. 0. ',value*frac(7),' 0. 0.'
                  write(2,*) '01 08 1980 31 08 1980 ',latitu,
     +            longit,height,' 0. 0. ',value*frac(8),' 0. 0.'
                  write(2,*) '01 09 1980 30 09 1980 ',latitu,
     +            longit,height,' 0. 0. ',value*frac(9),' 0. 0.'
                  write(2,*) '01 10 1980 31 10 1980 ',latitu,
     +            longit,height,' 0. 0. ',value*frac(10),' 0. 0.'
                  write(2,*) '01 11 1980 30 11 1980 ',latitu,
     +            longit,height,' 0. 0. ',value*frac(11),' 0. 0.'
                  write(2,*) '01 12 1980 31 12 1980 ',latitu,
     +            longit,height,' 0. 0. ',value*frac(12),' 0. 0. 0.'
                elseif (aertype.eq.4) then
                  write(2,*) '01 01 1980 31 01 1980 ',latitu,
     +            longit,height,' 0. 0. 0. ',value*frac(1),' 0.'
                  write(2,*) '01 02 1980 28 02 1980 ',latitu,
     +            longit,height,' 0. 0. 0. ',value*frac(2),' 0.'
                  write(2,*) '01 03 1980 31 03 1980 ',latitu,
     +            longit,height,' 0. 0. 0. ',value*frac(3),' 0.'
                  write(2,*) '01 04 1980 30 04 1980 ',latitu,
     +            longit,height,' 0. 0. 0. ',value*frac(4),' 0.'
                  write(2,*) '01 05 1980 31 05 1980 ',latitu,
     +            longit,height,' 0. 0. 0. ',value*frac(5),' 0.'
                  write(2,*) '01 06 1980 30 06 1980 ',latitu,
     +            longit,height,' 0. 0. 0. ',value*frac(6),' 0.'
                  write(2,*) '01 07 1980 31 07 1980 ',latitu,
     +            longit,height,' 0. 0. 0. ',value*frac(7),' 0.'
                  write(2,*) '01 08 1980 31 08 1980 ',latitu,
     +            longit,height,' 0. 0. 0. ',value*frac(8),' 0.'
                  write(2,*) '01 09 1980 30 09 1980 ',latitu,
     +            longit,height,' 0. 0. 0. ',value*frac(9),' 0.'
                  write(2,*) '01 10 1980 31 10 1980 ',latitu,
     +            longit,height,' 0. 0. 0. ',value*frac(10),' 0.'
                  write(2,*) '01 11 1980 30 11 1980 ',latitu,
     +            longit,height,' 0. 0. 0. ',value*frac(11),' 0.'
                  write(2,*) '01 12 1980 31 12 1980 ',latitu,
     +            longit,height,' 0. 0. 0. ',value*frac(12),' 0.'
                else
                  write(2,*) '01 01 1980 31 01 1980 ',latitu,
     +            longit,height,' 0. 0. 0. 0. ',value*frac(1)
                  write(2,*) '01 02 1980 28 02 1980 ',latitu,
     +            longit,height,' 0. 0. 0. 0. ',value*frac(2)
                  write(2,*) '01 03 1980 31 03 1980 ',latitu,
     +            longit,height,' 0. 0. 0. 0. ',value*frac(3)
                  write(2,*) '01 04 1980 30 04 1980 ',latitu,
     +            longit,height,' 0. 0. 0. 0. ',value*frac(4)
                  write(2,*) '01 05 1980 31 05 1980 ',latitu,
     +            longit,height,' 0. 0. 0. 0. ',value*frac(5)
                  write(2,*) '01 06 1980 30 06 1980 ',latitu,
     +            longit,height,' 0. 0. 0. 0. ',value*frac(6)
                  write(2,*) '01 07 1980 31 07 1980 ',latitu,
     +            longit,height,' 0. 0. 0. 0. ',value*frac(7)
                  write(2,*) '01 08 1980 31 08 1980 ',latitu,
     +            longit,height,' 0. 0. 0. 0. ',value*frac(8)
                  write(2,*) '01 09 1980 30 09 1980 ',latitu,
     +            longit,height,' 0. 0. 0. 0. ',value*frac(9)
                  write(2,*) '01 10 1980 31 10 1980 ',latitu,
     +            longit,height,' 0. 0. 0. 0. ',value*frac(10)
                  write(2,*) '01 11 1980 30 11 1980 ',latitu,
     +            longit,height,' 0. 0. 0. 0. ',value*frac(11)
                  write(2,*) '01 12 1980 31 12 1980 ',latitu,
     +            longit,height,' 0. 0. 0. 0. ',value*frac(12)
                endif
             
          enddo 
       close(unit=2)        
       close(unit=1)  
 1030  format(I3,I3,1x,E9.3,1x,E9.3,1x,E9.3,1x,E9.3,
     + 1x,E9.3,1x,E9.3,1x,E9.3,1x,E9.3,1x,E9.3,1x,
     + E9.3,1x,E9.3,1x,E9.3,1x,f5.0)
 1010  format(A2)  
 1020  format(A)
       stop
       end
         
