c  programme pour convertir les fichiers GEIA
c  en fichier 3ds de AODSEM
c
c
       programm geia23ds
       integer i,li,lj,matrice(360,180),lat,lon,ftype,n,ii,jj
       integer latlon,lennom,aertype,nbdata,nbdat
       real value,frac(12),height,latitu,longit
       character*60 geiaf,outf,bidon   
c 
c initialisation de variables
c
        open(unit=8,file='geia23ds.par',status='old',err=121)
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
c  aertype = type d'aerosol 1=su,2=bc,3=sd,4=ss,5=oc
        read(8,*) aertype
c  injection height (m)
        read(8,*) height
c        print*,'Output root file name?'

        read(8,*) outf
        close(unit=8)
        lennom=index(outf,' ')-1
        outf=outf(1:lennom)//'.3ds'
       open(unit=1,file=geiaf,status='old')
c
c  determiner le nombre de lignes de donnees
c
       do i=1,64810

        read(1,*,end=10) bidon
        nbdata=i-10
       enddo
 10       rewind 1
       print*,'nbdata=',nbdata
c
c  passer le header
c
          do i=1,10
             read(1,*)
          enddo  
c  lecture et ecriture des donnees  
       open(unit=2,file=outf,status='unknown')
       print*,'Writing file: ',outf
         nbdat=nbdata*ncols
         write(2,*) nbdat,'  number of data line units=tons/year/deg^2'                
          do i=1,nbdata 
             if (ncols.eq.1) then
                read(1,1000)lat,lon,value
             elseif (ncols.eq.4) then 
                read(1,*)latlon,value,frac(1),frac(2),
     +          frac(3),frac(4)
c  passer de taux par saison a taux par an
                value=value*4.
             elseif (ncols.eq.12) then   
                read(1,1030)lat,lon,frac(1),frac(2),
     +          frac(3),frac(4),frac(5),frac(6),frac(7),frac(8),
     +          frac(9),frac(10),frac(11),frac(12)
c  passer de taux par mois a taux par an
                value=12.
             endif
             if (ncols.eq.1) then       
                latitu=-90.501+real(lat)         
                longit=-180.5+real(lon)           
                if (aertype.eq.1) then                  
                  write(2,*) '01 01 1980 31 12 1980 ',latitu,
     +            longit,height,value,' 0. 0. 0. 0.'
                elseif (aertype.eq.2) then
                  write(2,*) '01 01 1980 31 12 1980 ',latitu,
     +            longit,height,' 0. ',value,' 0. 0. 0. '
                elseif (aertype.eq.3) then
                  write(2,*) '01 01 1980 31 12 1980 ',latitu,
     +            longit,height,' 0. 0. ',value,' 0. 0. '
                elseif (aertype.eq.4) then
                  write(2,*) '01 01 1980 31 12 1980 ',latitu,
     +            longit,height,' 0. 0. 0. ',value,' 0. '
                else
                  write(2,*) '01 01 1980 31 12 1980 ',latitu,
     +            longit,height,' 0. 0. 0. 0. ',value
                endif
             elseif (ncols.eq.4) then
                latitu=-90.5001+real(latlon/1000)
                longit=-180.5+real(latlon-latlon/1000*1000)
                if (aertype.eq.1) then                  
                  write(2,*) '01 01 1980 31 3 1980 ',latitu,
     +            longit,height,value*frac(1),' 0. 0. 0. 0.'
                  write(2,*) '01 04 1980 30 6 1980 ',latitu,
     +            longit,height,value*frac(2),' 0. 0. 0. 0.'
                  write(2,*) '01 07 1980 30 9 1980 ',latitu,
     +            longit,height,value*frac(3),' 0. 0. 0. 0.'
                  write(2,*) '01 10 1980 31 12 1980 ',latitu,
     +            longit,height,value*frac(4),' 0. 0. 0. 0.'
                elseif (aertype.eq.2) then
                  write(2,*) '01 01 1980 31 3 1980',latitu,
     +            longit,height,' 0. ',value*frac(1),' 0. 0. 0. '
                  write(2,*) '01 04 1980 30 6 1980 ',latitu,
     +            longit,height,' 0. ',value*frac(2),' 0. 0. 0.'
                  write(2,*) '01 07 1980 30 9 1980 ',latitu,
     +            longit,height,' 0. ',value*frac(3),' 0. 0. 0.'
                  write(2,*) '01 10 1980 31 12 1980 ',latitu,
     +            longit,height,' 0. ',value*frac(4),' 0. 0. 0.'
                elseif (aertype.eq.3) then
                  write(2,*) '01 01 1980 31 3 1980 ',latitu,
     +            longit,height,' 0. 0. ',value*frac(1),' 0. 0. '
                  write(2,*) '01 04 1980 30 6 1980 ',latitu,
     +            longit,height,' 0. 0. ',value*frac(2),' 0. 0.'
                  write(2,*) '01 07 1980 30 9 1980 ',latitu,
     +            longit,height,' 0. 0. ',value*frac(3),' 0. 0.'
                  write(2,*) '01 10 1980 31 12 1980 ',latitu,
     +            longit,height,' 0. 0. ',value*frac(4),' 0. 0.'
                elseif (aertype.eq.4) then
                  write(2,*) '01 01 1980 31 3 1980 ',latitu,
     +            longit,height,' 0. 0. 0. ',value*frac(1),' 0.'
                  write(2,*) '01 04 1980 30 6 1980 ',latitu,
     +            longit,height,' 0. 0. 0. ',value*frac(2),' 0.'
                  write(2,*) '01 07 1980 30 9 1980 ',latitu,
     +            longit,height,' 0. 0. 0. ',value*frac(3),' 0.'
                  write(2,*) '01 10 1980 31 12 1980 ',latitu,
     +            longit,height,' 0. 0. 0. ',value*frac(4),' 0.'
                else
                  write(2,*) '01 01 1980 31 3 1980 ',latitu,
     +            longit,height,' 0. 0. 0. 0. ',value*frac(1)
                  write(2,*) '01 04 1980 30 6 1980 ',latitu,
     +            longit,height,' 0. 0. 0. 0. ',value*frac(2)
                  write(2,*) '01 07 1980 30 9 1980 ',latitu,
     +            longit,height,' 0. 0. 0. 0. ',value*frac(3)
                  write(2,*) '01 10 1980 31 12 1980 ',latitu,
     +            longit,height,' 0. 0. 0. 0. ',value*frac(4)
                endif
             else
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
             endif
          enddo 
       close(unit=2)        
       close(unit=1)  
 1000  format(I3,I3,1x,E10.4) 
 1030  format(I3,I3,1x,E10.4,1x,E10.4,1x,E10.4,1x,E10.4,
     + 1x,E10.4,1x,E10.4,1x,E10.4,1x,E10.4,1x,E10.4,1x,
     + E10.4,1x,E10.4,1x,E10.4)
 1010  format(A2)  
 1020  format(A)
       stop
 121   print*,'No geia23ds.par file found!'
       stop
       end
         
