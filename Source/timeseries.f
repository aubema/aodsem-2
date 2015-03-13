c    programme pour lire une valeur sur des cartes d epaisseur optique
c    a partir de la latitude et la longitude du lieu 
c    
c    les axes sont definis comme suit
c    x = direction nord = degres
c    y = direction est = degres
c
c
c    copyright martin aube 2001
c
c -----------------
c   identification des variables 
c
c
c --------------------
c
c   programme principal
c
      program timeseries
c
c ----------
c
c   declaration des variables
c
      real pixsiz,lattau0,lontau0,tau(1000,1000)
      real lat,lon,value,la0,lo0,pxs
      real dtau
      real*8 zero,un
      real*8 dt,hre0,min0,sec0,jou0,moi0,ann0,jday,jjan0
      character*60 bidon,nom(5000),taufile,header,tag,barre
      integer nxcen,nycen,lennom(5000),i,ii,iii,his(10)
      integer nx,ny,nxtcel,nytcel,tmax,hcnt,nfiles,com
c
c -----------
c
c   choix du nom de la racine de fichiers
c
      com=0
      zero=0.
      un=1.     
      Open(unit=13,file='timeseries.par',status='old')
       read(13,*) nfiles
       do i=1,nfiles
         read(13,*) nom(i)
c
c   calcul de la longueur du nom
c
         lennom(i)=index(nom(i),' ')-1
       enddo
       read(13,*) lat,lon
       if (lon.lt.0.) lon=lon+360.
       read(13,*) dt
       read(13,*) hre0,min0,sec0,jou0,moi0,ann0
       call julian(hre0,min0,sec0,jou0,moi0,ann0,jday) 
       call julian(zero,zero,zero,un,un,ann0,jjan0)
       jday=jday-jjan0+1.
c
c   ouvrir le fichier de sortie
c
       open(unit=11,file='timeseries.cxy',status='unknown')
        write(11,*) nfiles,' jday(since1980) AOD'
c
c
c -----------
c
c   Lecture du fichier d'epaisseur optique
c
c   fichier pgm pour les images polder 254 et 255=aucun signification
c   253=cotes, autre=aod*100
c
        do ii=1,nfiles
         taufile=nom(ii)(1:lennom(ii))//'.pgm'
         open(unit=2,file=taufile,status='old')
c
c   recherche de la position des headers
c
         bidon='#'
         hcnt=0
         read(2,*)
         do 54 i=1,50
            read(2,*,end=56,err=57) bidon,tag,value
 57         if (bidon.eq.'#') then
               hcnt=hcnt+1
               if (tag(1:6).eq.'pixsiz') pixsiz=value
               if (tag(1:4).eq.'lat0') lattau0=value
               if (tag(1:4).eq.'lon0') lontau0=value 
            endif
 54      continue            
 56      rewind 2
         read(2,*)
         do 55 i=1,hcnt
            read(2,*)
 55      continue
         read(2,*) nytcel, nxtcel,tmax 
         read(2,*) ((tau(nx,ny),ny=1,nytcel),nx=nxtcel,1,-1)
         close(unit=2)
         if (com.ne.0) then
            lattau0=la0
            lontau0=lo0
            pixsiz=pxs
         else
          if (lattau0*pixsiz*lontau0.eq.0.) then
            com=1
            print*,'Horizontal pixel size in deg. ?'
            read*, pixsiz
 5        print*,'Center latitude of the south-west pixel in degrees?'
               read*,lattau0
               if (lattau0.lt.-90.) then
                  print*,'Should be greater than -90.'
                  goto 5
               endif
          print*,'Center longitude of the south-west pixel in degrees?'
               read*,lontau0
              la0=lattau0
              lo0=lontau0
              pxs=pixsiz
           endif
          endif
               if (lontau0.lt.0.) lontau0=lontau0+360.
               if (lontau0.gt.360.) lontau0=lontau0-360.
c
c -----------------
c
c   ecriture des donnees
c
            nxcen=nint((lat-lattau0)/pixsiz+1.)
            nycen=nint((lon-lontau0)/pixsiz+1.)
          write(11,*) jday,tau(nxcen,nycen)/100.
          jday=jday+dt/60./24.
         enddo
         close(unit=11)   
       end
