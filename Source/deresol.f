c   Programme permettant de reduire la resolution d une distribution
c   3-D des aerosols (fichier *.dat).  Le programme fera la moyenne 
c   des valeurs (de number density) sur des fenetres horizontales
c   carres
c   dont la taille est un multiple entier du pixel.  Les fichiers 
c   qui accompagnent le fichier .dat sont aussi crees.
c 
c
c
c    copyright martin aube 18/03/1999
c
      program deresol
c  
c   declaration des variables
c
       real*8 ir,jr,kr,densite(5,12,125,275,10)
       real*8 lcellx
       real*8 lcelly,lcellz(30),rh(13),rhgro(2,13),wavelen(20)
       real*8 rayon1(15),rayon2(15),numvol(5,12,125,275,10)
       real*8 heure,minute,seconde,jour,mois,annee
       integer i,j,k,nb,nx,ny,nz,nt
       integer boxsize,lennom,ncellx,ncelly,ncellz,ntype,nwav
       integer lentype(20),nrh,nr,nw,lenflag(20),nbns,lenbflag(20)
       integer datanu,lenwflag(20),id,jd
       character*60 nom,nomtype(20),bidon
       character*60 waveflag(20),binflag(20),datfile,derfile,obsfile
       character*2 bxflag
c
c -----------
c
c   choix du nom de la racine de fichiers
c
      print*,'Root name of the file (extension .dat will be add)?'  
      read*, nom
c
c   calcul de la longueur du nom
c
      lennom=index(nom,' ')-1
      nwav=7
c 
c --------------
c
c   nombre de pixel de la boite pour la reduction de la resolution
c
      print*,'Pixel size of the deres box (2-10 integer only):'
      read*,boxsize
c
c
c -----------
c
c   noms des fichiers d entree
c
      datfile=nom(1:lennom)//'.dat'
c
c ------------------------------------------
c
c   lecture des donnees de distribution des aerosols 
c
c   format adopte:         , chaque donnee est separee par un espace
c   ou une tabulation la premiere donnee de chaque ligne commence des le 
c   premier caractere.
c   
c   les donnees sont en unites de part/m^3
c
       open(unit=20,file=datfile,status='old')
          print*,'Reading data file: ',datfile
          read(20,*) bidon
          read(20,*) bidon
          read(20,*) heure,minute,seconde,jour,mois,annee
          read(20,*) nbns
c
c -----------
c
c   lecture des bins secs
c
         do 313 nb=1,nbns
            read(20,*) binflag(nb),rayon1(nb),rayon2(nb)
            lenbflag(nb)=index(binflag(nb),' ')-1
 313     continue
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
          do 100 k=1,ncellz
             read(20,*) lcellz(k)
 100      continue
          read(20,*) xcell0, ycell0, zcell0
c
c -----------
c
c   lecture des types d aerosols presents 
c
          read(20,*) ntype
          do 105 nr=1,ntype
             read(20,*) nomtype(nr)
             lentype(nr)=index(nomtype(nr),' ')-1
 105      continue
          read(20,*) bidon
          do 3111 nr=1,ntype
             do 3112 nb=1,nbns
                do 3113 k=1,ncellz
                   read(20,*) bidon
	           read(20,*) ((numvol(nr,nb,i,j,k)
     +             ,j=1,ncelly),i=ncellx,1,-1)
 3113           continue
 3112        continue
 3111     continue
       close(unit=20)
c
c ---------------
c
c   combinaison des pixels
c
       print*,'Reducing resolution...'
       print*,'ncellx=',ncellx
       print*,'ncelly=',ncelly
       print*,'ncellz=',ncellz
       print*,'nbns=',nbns
       print*,'ntype=',ntype

          do 202 i=1,ncellx
            do 302 j=1,ncelly
                do 402 k=1,ncellz
                   do 502 nb=1,nbns
                      do 602 nr=1,ntype
                         id=(i+boxsize-1)/boxsize
                         jd=(j+boxsize-1)/boxsize
      densite(nr,nb,id,jd,k)=densite(nr,nb,id,jd,k)+numvol(nr,nb,i,j,k)
 602                  continue
 502               continue
 402            continue
 302         continue
         print*,i*100/ncellx,'%'
 202      continue      
c
c   nom du fichier de sortie deres
c
      if (boxsize.eq.2) then
         derfile=nom(1:lennom)//'_02'//'.dat'
         obsfile=nom(1:lennom)//'_02'//'.vis'
         bxflag='02'
      elseif (boxsize.eq.3) then
         derfile=nom(1:lennom)//'_03'//'.dat'
         obsfile=nom(1:lennom)//'_03'//'.vis'
         bxflag='03'
      elseif (boxsize.eq.4) then
         derfile=nom(1:lennom)//'_04'//'.dat'
         obsfile=nom(1:lennom)//'_04'//'.vis'
         bxflag='04'
      elseif (boxsize.eq.5) then
         derfile=nom(1:lennom)//'_05'//'.dat'
         obsfile=nom(1:lennom)//'_05'//'.vis'
         bxflag='05'
      elseif (boxsize.eq.6) then
         derfile=nom(1:lennom)//'_06'//'.dat'
         obsfile=nom(1:lennom)//'_06'//'.vis'
         bxflag='06'
      elseif (boxsize.eq.7) then
         derfile=nom(1:lennom)//'_07'//'.dat'
         obsfile=nom(1:lennom)//'_07'//'.vis'
         bxflag='07'
      elseif (boxsize.eq.8) then
         derfile=nom(1:lennom)//'_08'//'.dat'
         obsfile=nom(1:lennom)//'_08'//'.vis'
         bxflag='08'
      elseif (boxsize.eq.9) then
         derfile=nom(1:lennom)//'_09'//'.dat'
         obsfile=nom(1:lennom)//'_09'//'.vis'
         bxflag='09'
      elseif (boxsize.eq.10) then
         derfile=nom(1:lennom)//'_10'//'.dat'
         obsfile=nom(1:lennom)//'_10'//'.vis'
         bxflag='10'
      endif
c
c   ecriture dans le fichier de sortie deres
c
      nx=ncellx/boxsize
      ny=ncelly/boxsize
      nz=ncellz
      open(unit=1,file=derfile,status='unknown')
      print*,'Writing .dat deres file...'
          write(1,*) 'From deresol.f'
          write(1,*) 'Units of 1E+4 part/m^3'
          write(1,*) int(heure),int(minute),int(seconde),int(jour),
     +int(mois),int(annee),'TU (hh mm ss), Date (jj mm yyyy)'
          write(1,*) nbns,'  Number of size bins'
          do 413 nb=1,nbns
             write(1,*) nb,rayon1(nb),rayon2(nb)
 413      continue
          write(1,*) nx,'  Number of X cells'
          write(1,*) lcellx*dble(boxsize),'  Size of X cell'
          write(1,*) ny,'  Number of Y cells'
          write(1,*) lcelly*dble(boxsize),'  Size of Y cell'
          write(1,*) nz,'  Number of Z cells'
          do 700 k=1,ncellz
             write(1,*) lcellz(k)
 700      continue
          write(1,*) xcell0-lcellx+dble(boxsize)*lcellx/2., 
     +    ycell0-lcelly+dble(boxsize)*lcelly/2., zcell0,'  X0, Y0, Z0' 
          write(1,*) ntype,'  Number of aerosol types'
          do 405 nr=1,ntype
             write(1,*) nomtype(nr)
 405      continue
          write(1,*) 'DATA'
          do 2111 nt=1,ntype
             do 2112 nb=1,nbns
                do 2113 k=1,nz
                   write(1,2325) nomtype(nt),nb,k
	           write(1,*) ((nint(densite(nt,nb,i,j,k)/
     +             dble(boxsize**2))
     +             ,j=1,ny),i=nx,1,-1)
 2113           continue
 2112        continue
 2111     continue
 2324  format(A2)
 2325  format('# AEROSOL TYPE: ',A9,' SIZE BIN: ',I2,
     +' VERTICAL LEVEL: ',I2)
      close(unit=1)
c -----------
c
c   Ecriture du fichier d observation 
c
       open(unit=7,file=obsfile,status='unknown')
          print*,'Writing observation file: ',obsfile
          write(7,*) 'Observation definition file for aeros.f'
          write(7,*) '1    Number of sights'
          write(7,*) '90.    ANGLE ELEVATION PHOTOMETRE'
          write(7,*) '0.    ANGLE AZIMUT PHOTOMETRE'
          write(7,*) xcell0-lcellx+dble(boxsize)*lcellx/2.,
     +    '  X PHOTOMETRE degres'
          write(7,*) ycell0-lcelly+dble(boxsize)*lcelly/2.,
     +    '  Y PHOTOMETRE degres'
          write(7,*) zcell0,'  Z PHOTOMETRE'
          write(7,*) '1    Number of slices'
          write(7,*) '3    Integration Axis(1=lat,2=lon,3=height)'
          write(7,*) '1    from'
          write(7,*) '20    to'
 110      continue
          write(7,*) '45.    ANGLE ELEVATION SOLEIL'
          write(7,*) '0.    ANGLE AZIMUT SOLEIL'
       close(unit=7)
      stop
      end

