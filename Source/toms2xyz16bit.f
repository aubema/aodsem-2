c   Programme permettant de transformer un fichier toms
c   aerosol index ascii en format xyz de aodsem
c   le fichier xyz peut etre porte en format image
c   aodsem (.pgm) grace a la fonction xytto

        integer data,reclen,n,nn,i,ii,nnmax
        integer d(51840)
        real lat0,dlat,lon0,dlon,la(51840),lo(51840)
        character*60 nomin,nomout
        nomin='toms_ai_tmp'
        nomout='toms_ai_tmp.xyz'
        nnmax=180
        open(unit=1,file=nomin,status='old')
          i=0
          lon0=-179.375
          lat0=-89.5
          dlon=1.25
          dlat=1.
          read(1,*)
          read(1,*)
          read(1,*)
          do nn=1,nnmax
           do n=1,11 
            read(1,1000) d(i+1),d(i+2),d(i+3),d(i+4),d(i+5)
     +      ,d(i+6),d(i+7),d(i+8),d(i+9),d(i+10),d(i+11),d(i+12),d(i+13)
     +      ,d(i+14),d(i+15),d(i+16),d(i+17),d(i+18),d(i+19),d(i+20)
     +      ,d(i+21),d(i+22),d(i+23),d(i+24),d(i+25)
             i=i+25
           enddo
           read(1,1005) d(i+1),d(i+2),d(i+3),d(i+4),d(i+5),d(i+6)
     +     ,d(i+7),d(i+8),d(i+9),d(i+10),d(i+11),d(i+12),d(i+13)
           i=i+13
           do ii=1,288
               lo(ii+(nn-1)*288)=lon0+real(ii-1)*dlon
               la(ii+(nn-1)*288)=lat0+real(nn-1)*dlat
           enddo
          enddo
         close(unit=1)
 1000   format(1x,25I3.2)
 1005   format(1x,13I3)
        open(unit=1,file=nomout,status='unknown') 
          write(1,*) ' 51840'       
          do i=1,nnmax*288
            if (d(i).eq.999) d(i)=65534
            write(1,*)la(i),lo(i),d(i)/100.
          enddo
        close(unit=1)
        stop
        end
