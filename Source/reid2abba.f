c programme pour reformatter les fichiers de J. Reid en format abba tel que reconnu par le programme
c de Bruno Courtemanche
c
       integer i,j,jday,annee,h,m,vali(2)
       character*40 nom,filein,filout
       character*1 bidon
       real vale(6)
       real*8 hre,min,sec,jou,moi,ann,jday0
       open(unit=1,file='reid2abba.par',status='old')
         read(1,*) filein
         read(1,*) filout
       close(unit=1)  
   
      
       open(unit=3,file=filout,status='unknown')
           write(3,*) 'Date     Time Longitude Latitude    T4(K)   
     +T11(K) Size(km2)   Temp(K)  Ecosystem  Fire Flag'
        open(unit=1,file=filein,status='old')
          do i=1,10000000
         
            read(1,10,end=200) bidon,annee,jday,h,m
            print*,bidon,annee,jday,h,m
            backspace 1
            read(1,*) nom
            hre=0.
            min=0.
            sec=0.
            jou=0.
            moi=0.
            call julian(hre,min,sec,jou,moi,dble(annee),jday0)
            jday0=jday0+dble(jday)
            call timedate(hre,min,sec,jou,moi,ann,jday0)
            open (unit=2,file=nom,status='old')


               read(2,*) 
               read(2,*) 
               read(2,*) 
               read(2,*) 
               read(2,*) 
               do j=1,10000000
                 read(2,*,end=100) vale(1),vale(2),vale(3),vale(4),
     +           vale(5),vale(6),vali(1),vali(2)
                 write(3,20) idint(ann),idint(moi),idint(jou),
     +           h,m,vale(1),vale(2),vale(3),vale(4)
     +           ,vale(5),vale(6),vali(1),vali(2)
               enddo

 100       close(unit=2)
           enddo
 200      print*,'fin de list'
           close(unit=1)
           close(unit=3)
 10    format(A1,I4,I3,I2,I2)
 20    format(I4,1x,I2,1x,I2,1x,I2.2,I2.2,1x,F7.2,1x,F6.2,1x,f7.1,1x,
     + f7.1,1x,f7.4,1x,f7.1,1x,I2,1x,I2,1x,I2)

          stop
          end

            

