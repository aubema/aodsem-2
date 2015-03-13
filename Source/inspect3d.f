c   Programme permettant de naviguer a l interieur d une distribution
c   3-D des aerosols (fichier *.dat). 
c 
c
c
c    copyright martin aube 27/01/2000
c
      program inspect3d
c  
c   declaration des variables
c
       real*8 lcellx,maxi
       real*8 lcelly,lcellz(30),scale
       real*8 rayon1(15),rayon2(15),numvol(5,12,181,360,10)
       real*8 heure,minute,seconde,jour,mois,annee
       integer i,j,k,nb,nx,ny,nz,nt
       integer leveln,lennom,ncellx,ncelly,ncellz,ntype
       integer lentype(20),nrh,nr,lenflag(20),nbns,lenbflag(20)
       integer typen,binn
       character*60 nom,nomtype(20),bidon
       character*60 binflag(20),datfile
c
c -----------
c
c   lecture des parametres
c
      open(unit=1,file='inspect3d.par',status='old')  
        read(1,*) nom
        read(1,*) leveln
        read(1,*) typen      
        read(1,*) binn    
        read(1,*) scale  
      close(unit=1)
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
c   recherche de la valeur de densite numerique maximale
c
c 
       maxi=0.
        do nt=1,ntype
         do nb=1,nbns
          do 330 i=1,ncellx
           do 340 j=1,ncelly
            do k=1,ncellz
              if (numvol(nt,nb,i,j,k).gt.maxi)
     +         maxi=numvol(nt,nb,i,j,k) 
             enddo
 340       continue
 330      continue
         enddo
        enddo
 
         print*,'Maximum value found=',maxi           

c
c   
c      
c ---------------
c
c   creation de l image pgm de densite numerique du niveau
c
       print*,'Creating output image...'
      open(unit=1,file='inspect3d.pgm',status='unknown')
          write(1,2323) 'P2'
          write(1,2323) '# From inspect3d.f'
          write(1,2323) '# Units of 1E+4 part/m^3 x ',maxi/255.
          write(1,2323) '# pixsiz ',lcellx
          write(1,2323) '# lat0   ',xcell0
          write(1,2323) '# lon0   ',ycell0        
          write(1,*) ncelly,ncellx
          write(1,*) ' 255'  
	  write(1,*) ((nint(numvol(typen,binn,i,j,leveln)*255./maxi
     +        *scale),j=1,ncelly),i=ncellx,1,-1)
 2113           continue
 2112        continue
 2111     continue
 2323  format(A,F10.3)
      close(unit=1)
      stop
      end

