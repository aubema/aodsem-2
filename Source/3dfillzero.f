c   Programme permettant de remplacer les colonnes vides des fichiers .dat 
c   par le contenu de la colonne non vide la plus proche 
c 
c
c
c    copyright Martin Aubé 03/2004
c    routine pvoisnn realisee par Jean-Denis Giguère 03/2004
c
c     program 3dfillzero
c  
c   declaration des variables
c
       real ir,jr,kr,densite(5,12,181,360,10)
       real lcellx,lon0f,lat0f,pixsf,latitu,longit
       real lcelly,lcellz(30),rh(13),rhgro(2,13),wavelen(20)
       real rayon1(15),rayon2(15),numvol(5,12,181,360,10)
       real*8 heure,minute,seconde,jour,mois,annee
       integer i,j,k,nb,nx,ny,nz,nt,nlatf,nlonf
       integer proche(181,360,2)
       integer boxsize,lennom,ncellx,ncelly,ncellz,ntype
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
      open(unit=25,file='3dfillzero.par',status='old')
c      print*,'Root name of the file (extension .dat will be add)?'  
      read(25,*) nom
c
c   calcul de la longueur du nom
c
      lennom=index(nom,' ')-1
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
          if (ycell0.le.0.) ycell0=ycell0+360.
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
c  production d'un masque fichier de position du plus proche voisin
c  non nul
c
       call pvoisnn(numvol,ntype,nbns,ncellx,ncelly,ncellz,proche)
c
c ---------------
c
c   interpolation plus proche voisin
c
       print*,'Changing resolution...'

          do 202 i=1,ncellx
            do 302 j=1,ncelly
                do 402 k=1,ncellz
                   do 502 nb=1,nbns
                      do 602 nr=1,ntype
                         densite(nr,nb,i,j,k)=numvol(nr,nb,i+
     +proche(i,j,1),j+proche(i,j,2),k)
 602                  continue
 502               continue
 402            continue
 302         continue
 202      continue      
c
c   nom du fichier de sortie deres
c
         derfile=nom(1:lennom)//'_f0'//'.dat'
         obsfile=nom(1:lennom)//'_f0'//'.vis'
c
c   ecriture dans le fichier de sortie deres
c
      nz=ncellz
      pixsf=lcellx
      nlonf=ncelly
      nlatf=ncellx
      open(unit=1,file=derfile,status='unknown')
      print*,'Writing .dat zero filled file...'
          write(1,*) 'From 3Dfillzero.f'
          write(1,*) 'Units of 1E+4 part/m^3'
          write(1,*) int(heure),int(minute),int(seconde),int(jour),
     +int(mois),int(annee),' TU (hh mm ss), Date (jj mm yyyy)'
          write(1,*) nbns,'  Number of size bins'
          do 413 nb=1,nbns
             write(1,*) nb,rayon1(nb),rayon2(nb)
 413      continue
          write(1,*) nlatf,'  Number of X cells'
          write(1,*) pixsf,'  Size of X cell'
          write(1,*) nlonf,'  Number of Y cells'
          write(1,*) pixsf,'  Size of Y cell'
          write(1,*) nz,'  Number of Z cells'
          do 700 k=1,ncellz
             write(1,*) lcellz(k)
 700      continue
          write(1,*) xcell0,ycell0, zcell0,'  X0, Y0, Z0' 
          write(1,*) ntype,'  Number of aerosol types'
          do 405 nr=1,ntype
             write(1,*) nomtype(nr)
 405      continue
          write(1,*) 'DATA'
          do 2111 nt=1,ntype
             do 2112 nb=1,nbns
                do 2113 k=1,nz
                   write(1,2325) nomtype(nt),nb,k
	           write(1,*) ((nint(densite(nt,nb,i,j,k))
     +             ,j=1,ncelly),i=ncellx,1,-1)
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
          write(7,*) xcell0,
     +    '  X PHOTOMETRE degres'
          write(7,*) ycell0,
     +    '  Y PHOTOMETRE degres'
          write(7,*) zcell0,'  Z PHOTOMETRE'
          write(7,*) '1    Number of slices'
          write(7,*) '3    Integration Axis(1=lat,2=lon,3=height)'
          write(7,*) '1    from'
          write(7,*) '10    to'
 110      continue
          write(7,*) '45.    ANGLE ELEVATION SOLEIL'
          write(7,*) '0.    ANGLE AZIMUT SOLEIL'
       close(unit=7)
      stop
      end

