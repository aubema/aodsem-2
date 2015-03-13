c  programme combiner deux fichier fichiers GEIA
c  en fichier pgm code sur une echelle speciale
c
c
       programm cmbgeia
       integer i,li,lj,lat,lon,ftype,n,ii,jj
       integer latlon,lennom
       integer freq1,freq2
       real value,frac(12)
       real matrice1(360,180),matrice(360,180),matrice2(360,180)
       character*2 tt(12)
       character*60 geiaf1,geiaf2,outf   
c 
c initialisation de variables
c
        do ii=1,360
        do jj=1,180
          matrice1(ii,jj)=0
          matrice2(ii,jj)=0
          matrice(ii,jj)=0
        enddo
        enddo  
c        print*,'Add two GEIA-pgm files'
c        print*,' '
c        print*,'First GEIA-pgm file (.pgm will be add)?'
        open(unit=8,file='cmbgeiapgm.par',err=121)
        read(8,*) geiaf1
c        print*,'Second GEIA-pgm file (.pgm will be add)?'

        read(8,*) geiaf2               
c        print*,'Output root file name (.pgm will be add)?'

        read(8,*) outf 
        close(unit=8)
        lennom1=index(geiaf1,' ')-1
        lennom2=index(geiaf2,' ')-1
        lennomo=index(outf,' ')-1
        geiaf1=geiaf1(1:lennom1)//'.pgm'
        geiaf2=geiaf2(1:lennom2)//'.pgm' 
        outf=outf(1:lennomo)//'.pgm'               
       open(unit=1,file=geiaf1,status='old')
         read(1,*) 
         read(1,*) 
         read(1,*) 
         read(1,*)
         do jj=1,180
            read(1,*) (matrice1(ii,jj),ii=1,360)
         enddo
       close(unit=1)
       do jj=1,180
         do ii=1,360
           matrice1(ii,jj)=10.**(matrice1(ii,jj)/40.-1.)
         enddo
       enddo
       open(unit=2,file=geiaf2,status='old')
         read(2,*) 
         read(2,*) 
         read(2,*) 
         read(2,*)
         do jj=1,180
            read(2,*) (matrice2(ii,jj),ii=1,360)
         enddo
       close(unit=2) 
       do jj=1,180
         do ii=1,360
           matrice2(ii,jj)=10.**(matrice2(ii,jj)/40.-1.)
         enddo 
       enddo

       do jj=1,180
         do ii=1,360
           matrice(ii,jj)=matrice1(ii,jj)+matrice2(ii,jj)
         enddo 
       enddo 
       do jj=1,180
         do ii=1,360
           matrice(ii,jj)=40.*log10(10.*matrice(ii,jj))
         enddo 
       enddo          
c          
c     output tonnes/year                              
c
       open(unit=3,file=outf,status='unknown')
       print*,'Writing file: ',outf
         write(3,1010) 'P2'
         write(3,1020) '# numerical value=40*log10(10*Mtons/Yr)'
         write(3,*) '360 180'
         write(3,*) '255'
         do 3411 jj=1,180
            write(3,*) (int(matrice(ii,jj)),ii=1,360)
 3411    continue
       close(unit=3)                

 1000  format(I3,I3,1x,E10.4) 
 1030  format(I3,I3,1x,E10.4,1x,E10.4,1x,E10.4,1x,E10.4,
     + 1x,E10.4,1x,E10.4,1x,E10.4,1x,E10.4,1x,E10.4,1x,
     + E10.4,1x,E10.4,1x,E10.4)
 1010  format(A2)  
 1020  format(A)
       stop
 121   print*,'No cmbgeiapgm.par file found!'
       stop
       end
         
