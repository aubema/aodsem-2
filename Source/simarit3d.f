c    Programme permettant proceder a des operations arithmetiques de 
c    base sur une distribution 3D des aerosol fichier .dat
c    du progiciel AODSEM
c
c    copyright martin aube 2001
c
c -----------------
c   identification des variables 
c
c       neg= 1-> replace negative values by 0.
c
c --------------------
c
c   programme principal
c
c       PROGRAM simarit3d
c
c ----------
c
c   declaration des variables
c
c   nr=10 nrh=13 nb=12 nw=7 i=125 j=275 k=20
c
       character*60 nom1,nom3,datfil1,datfil3,
     + obsfile,nomtype(4),binflag(12)
       real numvol1(4,12,125,275,10),
     + numvol3(4,12,125,275,10),rayon1(12),rayon2(12),lcellx
     + ,lcelly,lcellz(10),xcell0,ycell0,zcell0
       real heure,minute,seconde,jour,mois,annee,value
       integer lennom1,lennom3,nr,nb,i,j,k,oper,neg
     + ,nbns,lenbflag(12)
     + ,ncellx,ncelly,ncellz,lentype(4),nref

c   
c ----------
c
c   initialisation des variables
c
       pi=3.141592654
       pi2=pi*pi
       do nr=1,4
       do nb=1,12
       do i=1,125
       do j=1,275
       do k=1,10
          numvol3(nr,nb,i,j,k)=0.
       enddo
       enddo
       enddo
       enddo
       enddo
c
c -----------
c
c   formats
c
 2325  format('# AEROSOL TYPE: ',A9,' SIZE BIN: ',I2,
     +' VERTICAL LEVEL: ',I2)
c
c     lecture du fichier de parametres
c
      open(unit=25,file='simarit3d.par',status='old',err=8)
       read(25,*) nom1
c      print*,'Choose an operation:'
c      print*,' '
c      print*,'   1 ..... +'
c      print*,'   2 ..... -'
c      print*,'   3 ..... x'
c      print*,'   4 ..... /'
       read(25,*) oper
       read(25,*) value
       read(25,*) nom3
       read(25,*) neg
      close(unit=25)
c
c -----------
c
c   calcul de la longueur des noms
c
      lennom1=index(nom1,' ')-1
      lennom3=index(nom3,' ')-1
c
c -----------
c
c   noms des fichiers d entree
c
      obsfile=nom3(1:lennom3)//'.vis'
      datfil1=nom1(1:lennom1)//'.dat'
      datfil3=nom3(1:lennom3)//'.dat'
      open(unit=20,file=datfil1,status='old',err=3)
c
c ------------------------------------------
c
c   lecture des donnees de distribution des aerosols 
c   fichier1
c   les donnees sont en unites de 1.e+4 part/m^3
c
          print*,'Reading data file: ',datfil1
          read(20,*) 
          read(20,*) 
          read(20,*) heure,minute,seconde,jour,mois,annee
          read(20,*) nbns
c
c -----------
c
c   lecture des bins secs
c
         do nb=1,nbns
            read(20,*) binflag(nb),rayon1(nb),rayon2(nb)
            lenbflag(nb)=index(binflag(nb),' ')-1
         enddo
c
c ----------
c
c   lecture des donnees concernant la geometrie de la grille du modele
c
          read(20,*) ncellx
          read(20,*) lcellx
          read(20,*) ncelly
          read(20,*) lcelly
          read(20,*) ncellz
          do k=1,ncellz
             read(20,*) lcellz(k)
          enddo
          read(20,*) xcell0, ycell0, zcell0
c
c -----------
c
c   lecture des types d aerosols presents 
c
          read(20,*) nref
          do nr=1,nref
             read(20,*) nomtype(nr)
             lentype(nr)=index(nomtype(nr),' ')-1
          enddo
          read(20,*)
          do nr=1,nref
             print*,(nr-1)*100/nref,'%'
             do nb=1,nbns
                do k=1,ncellz
                   read(20,*) 
	           read(20,*) ((numvol1(nr,nb,i,j,k)
     +             ,j=1,ncelly),i=ncellx,1,-1)
                enddo
             enddo
          enddo
       print*,'100%'
       close(unit=20)

c
c  ------------------------
c
c      Operation aritmetique
c
       print*,'Computing arithmetics...'
       do nr=1,nref
         print*,(nr-1)*100/nref,'%'
         do nb=1,nbns
           do i=1,ncellx
             do j=1,ncelly
               do k=1,ncellz
c         somme
                 if (oper.eq.1) then
                   numvol3(nr,nb,i,j,k)=numvol1(nr,nb,i,j,k)+
     +             value
c         soustraction
                 elseif (oper.eq.2) then
                   numvol3(nr,nb,i,j,k)=numvol1(nr,nb,i,j,k)-
     +             value
c         multiplication
                 elseif (oper.eq.3) then
                   numvol3(nr,nb,i,j,k)=numvol1(nr,nb,i,j,k)*
     +             value
c         division
                 elseif (oper.eq.4) then   
                   numvol3(nr,nb,i,j,k)=numvol1(nr,nb,i,j,k)/
     +             value
                 endif   
                 if (numvol3(nr,nb,i,j,k).lt.0.) then
                    if (neg.eq.1) then
                       numvol3(nr,nb,i,j,k)=0.
                    endif
                 endif
               enddo
             enddo
           enddo
         enddo
       enddo   
       print*,'100%'  
c
c ---------------------
c
c       ecriture du fichier de sortie
c
c -----------
c
c   Ecriture du fichier d observation 
c
       open(unit=7,file=obsfile,status='unknown')
          print*,'Writing observation file: ',obsfile
          write(7,*) 'Observation definition file for AODSEM'
          write(7,*) '1    Number of sights'
             write(7,*) '90.    ANGLE ELEVATION PHOTOMETRE'
             write(7,*) '0.    ANGLE AZIMUT PHOTOMETRE'
             write(7,*) xcell0,'  X PHOTOMETRE degres'
             write(7,*) ycell0,'  Y PHOTOMETRE degres'
             write(7,*) zcell0,'  Z PHOTOMETRE'
          write(7,*) '1    Number of slices'
             write(7,*) '3    Integration Axis(1=lat,2=lon,3=height)'
             write(7,*) '1    from'
             write(7,*) '10    to'
          write(7,*) '45.    ANGLE ELEVATION SOLEIL'
          write(7,*) '0.    ANGLE AZIMUT SOLEIL'
       close(unit=7)
c
c ----------
c
c   Ecriture des donnees de distribution des aerosols 
c   
c   les donnees sont en unites de 1.e+4 part/m^3
c
      open(unit=1,file=datfil3,status='unknown')
          print*,'Writing .dat file:',datfil3
          write(1,*) '3-d aerosol distribution file for AODSEM from aod
     +to3d.f'
          write(1,*) 'Units of 1.e+4 part/m^3'
          write(1,*) int(heure),int(minute),int(seconde),int(jour),
     +    int(mois),int(annee),' TU (hh mm ss), Date (jj mm yyyy)'
          write(1,*) nbns,'  Number of size bins'
          do nb=1,nbns
             write(1,*) nb,rayon1(nb),rayon2(nb)
          enddo
          write(1,*) ncellx,'  Number of X cells'
          write(1,*) lcellx,'  Size of X cell'
          write(1,*) ncelly,'  Number of Y cells'
          write(1,*) lcelly,'  Size of Y cell'
          write(1,*) ncellz,'  Number of Z cells'
          do k=1,ncellz
             write(1,*) lcellz(k)
          enddo
          write(1,*) xcell0, ycell0, zcell0,'  X0, Y0, Z0'
          write(1,*) nref,'  Number of aerosol types'
          do nr=1,nref
             write(1,*) nomtype(nr)
          enddo
          write(1,*) 'DATA'
          do nr=1,nref
             print*,(nr-1)*100/nref,'%'
             do nb=1,nbns
                do k=1,ncellz
                   write(1,2325) nomtype(nr),nb,k
	           write(1,*) ((nint(numvol3(nr,nb,i,j,k))
     +             ,j=1,ncelly),i=ncellx,1,-1)
                enddo
             enddo
          enddo
       print*,'100%'
       close(unit=1)
       stop
 3     print*,'Bad root file name: ',nom1
       stop
 8     print*,'Parameter file: simarit3d.par dont exist.'
       stop
       end      
