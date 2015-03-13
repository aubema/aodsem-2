c    programme pour ecrire un fichier texte de longitude tatitude 
c    afin de dessiner des formes et trajectoires sur une image
c    a l aide de writeaod.f
c
c    copyright martin aube 10/05/1999
c
c -----------------
c   identification des variables 
c
c
c --------------------
c
c   programme principal
c
      program drawaod
c
c ----------
c
c   declaration des variables
c
      real lat(34375),lon(34375),pixsiz,lat1,lat2,lon1,lon2,ordor,pente
      real deltax,deltay,latit,longi,rayon,deltat,pi,theta,largeur
      real longi1(6),latit1(6),longi2(6),latit2(6),hauteur
      character*60 nomxy
      integer nxcen,nycen,i,lenxy
      integer nx,ny,ans,n,npts
c
c -----------
c
      print*,'Name of the latitude-longitude output file ?'  
      read*,nomxy
c
c   calcul de la longueur du nom
c
      lenxy=index(nomxy,' ')-1
c
c   initialisation
c
      pi=3.14159265
c
c ---------------
c
c   Drawing loop
c
      n=0
      print*,'Pixel size of the image ?'
      read*,pixsiz
      do 500 i=1,1000
 3       print*,'Wich type of shape?'
         print*,'  point ........ 1'
         print*,'  line  ........ 2'
         print*,'  square ....... 3'
         print*,'  rectangle .... 4'
         print*,'  circle ....... 5'
         print*,'  target ....... 6'
         print*,' '
         print*,'  EXIT ......... 9'
         read*,ans
         if (ans.eq.1) then
            n=n+1
            print*,'latitude, longitude of the point ?'
            read*,lat(n),lon(n)
         elseif (ans.eq.2) then
c
c   line 
c
            print*,'latitude, longitude of the first point ?'
            read*,lat1,lon1
            print*,'latitude, longitude of the second point ?'
            read*,lat2,lon2
            pente=(lon1-lon2)/(lat1-lat2)
            ordor=lon1-pente*lat1
            if (lat2.eq.lat1) then
               npts=nint(abs(2.*(lon2-lon1)/pixsiz))
               deltay=(lon2-lon1)/real(npts)
               longi=lon1
               do 111 np=1,npts
                 longi=longi+deltay
                 n=n+1
                 lon(n)=longi
                 lat(n)=lat1
 111           continue
            elseif (lon2.eq.lon1) then
               npts=nint(abs((2.*(lat2-lat1)/pixsiz)))
               deltax=(lat2-lat1)/real(npts)
               latit=lat1
               do 112 np=1,npts
                 latit=latit+deltax
                 n=n+1
                 lon(n)=lon1
                 lat(n)=latit
 112           continue
            else
               npts=2.*(((lat1-lat2)**2.+(lon1-lon2)**2.)**0.5)/pixsiz
               deltax=(lat2-lat1)/real(npts)
               deltay=(lon2-lon1)/real(npts)
               latit=lat1
               longi=lon1
               do 113 np=1,npts
                 latit=latit+deltax
                 longi=longi+deltay
                 n=n+1
                 lon(n)=longi
                 lat(n)=latit
 113           continue
            endif
         elseif (ans.eq.3) then
c
c   square
c
            print*,'latitude, longitude of the center point ?'
            read*,lat1,lon1
            print*,'Square size (degrees)?'
            read*,largeur
            latit1(1)=lat1-largeur/2.
            latit2(1)=lat1-largeur/2.
            longi1(1)=lon1-largeur/2.
            longi2(1)=lon1+largeur/2.
            latit1(2)=lat1+largeur/2.
            latit2(2)=lat1+largeur/2.
            longi1(2)=lon1-largeur/2.
            longi2(2)=lon1+largeur/2.
            latit1(3)=lat1-largeur/2.
            latit2(3)=lat1+largeur/2.
            longi1(3)=lon1+largeur/2.
            longi2(3)=lon1+largeur/2.
            latit1(4)=lat1+largeur/2.
            latit2(4)=lat1-largeur/2.
            longi1(4)=lon1-largeur/2.
            longi2(4)=lon1-largeur/2.
           do 544 ii=1,4
            if (latit2(ii).eq.latit1(ii)) then
               npts=nint(abs(2.*(longi2(ii)-longi1(ii))/pixsiz))
               deltay=(longi2(ii)-longi1(ii))/real(npts)
               longi=longi1(ii)
               do 511 np=1,npts
                 longi=longi+deltay
                 n=n+1
                 lon(n)=longi
                 lat(n)=latit1(ii)
 511           continue
            elseif (longi2(ii).eq.longi1(ii)) then
               npts=nint(abs((2.*(latit2(ii)-latit1(ii))/pixsiz)))
               deltax=(latit2(ii)-latit1(ii))/real(npts)
               latit=latit1(ii)
               do 512 np=1,npts
                 latit=latit+deltax
                 n=n+1
                 lon(n)=longi1(ii)
                 lat(n)=latit
 512           continue
            endif
 544      continue

         elseif (ans.eq.4) then
            print*,'latitude, longitude of the center point ?'
            read*,lat1,lon1
            print*,'width (degrees)?'
            read*,largeur 
            print*,'height (degrees ?'
            read*,hauteur
c     rectangle
            latit1(1)=lat1-hauteur/2.
            latit2(1)=lat1-hauteur/2.
            longi1(1)=lon1-largeur/2.
            longi2(1)=lon1+largeur/2.
            latit1(2)=lat1+hauteur/2.
            latit2(2)=lat1+hauteur/2.
            longi1(2)=lon1-largeur/2.
            longi2(2)=lon1+largeur/2.
            latit1(3)=lat1-hauteur/2.
            latit2(3)=lat1+hauteur/2.
            longi1(3)=lon1+largeur/2.
            longi2(3)=lon1+largeur/2.
            latit1(4)=lat1+hauteur/2.
            latit2(4)=lat1-hauteur/2.
            longi1(4)=lon1-largeur/2.
            longi2(4)=lon1-largeur/2.
           do 744 ii=1,4
            if (latit2(ii).eq.latit1(ii)) then
               npts=nint(abs(2.*(longi2(ii)-longi1(ii))/pixsiz))
               deltay=(longi2(ii)-longi1(ii))/real(npts)
               longi=longi1(ii)
               do 711 np=1,npts
                 longi=longi+deltay
                 n=n+1
                 lon(n)=longi
                 lat(n)=latit1(ii)
 711           continue
            elseif (longi2(ii).eq.longi1(ii)) then
               npts=nint(abs((2.*(latit2(ii)-latit1(ii))/pixsiz)))
               deltax=(latit2(ii)-latit1(ii))/real(npts)
               latit=latit1(ii)
               do 712 np=1,npts
                 latit=latit+deltax
                 n=n+1
                 lon(n)=longi1(ii)
                 lat(n)=latit
 712           continue
            endif
 744      continue
         elseif (ans.eq.5) then
            print*,'latitude, longitude of the center point ?'
            read*,lat1,lon1
            print*,'Circle radius (in degrees)?'
            read*,rayon
               theta=0.
               deltat=pixsiz/(2.*rayon)
               npts=2*pi/deltat
               latit=lat1
               longi=lon1-rayon
               do 114 np=1,npts-1
                 theta=theta+deltat
                 n=n+1
                 lon(n)=lon1+rayon*cos(theta)
                 lat(n)=lat1+rayon*sin(theta)
 114           continue
         elseif (ans.eq.6) then
c
c   target: a square with a cross
c 
            print*,'latitude, longitude of the center point ?'
            read*,lat1,lon1
            print*,'Target size (in degrees)?'
            read*,largeur
c     carre
            latit1(1)=lat1-largeur/2.
            latit2(1)=lat1-largeur/2.
            longi1(1)=lon1-largeur/2.
            longi2(1)=lon1+largeur/2.
            latit1(2)=lat1+largeur/2.
            latit2(2)=lat1+largeur/2.
            longi1(2)=lon1-largeur/2.
            longi2(2)=lon1+largeur/2.
            latit1(3)=lat1-largeur/2.
            latit2(3)=lat1+largeur/2.
            longi1(3)=lon1+largeur/2.
            longi2(3)=lon1+largeur/2.
            latit1(4)=lat1+largeur/2.
            latit2(4)=lat1-largeur/2.
            longi1(4)=lon1-largeur/2.
            longi2(4)=lon1-largeur/2.
c    cross
            latit1(5)=lat1
            latit2(5)=lat1
            longi1(5)=lon1-largeur/2.
            longi2(5)=lon1+largeur/2.
            latit1(6)=lat1+largeur/2.
            latit2(6)=lat1-largeur/2.
            longi1(6)=lon1
            longi2(6)=lon1
           do 444 ii=1,6
            if (latit2(ii).eq.latit1(ii)) then
               npts=nint(abs(2.*(longi2(ii)-longi1(ii))/pixsiz))
               deltay=(longi2(ii)-longi1(ii))/real(npts)
               longi=longi1(ii)
               do 411 np=1,npts
                 longi=longi+deltay
                 n=n+1
                 lon(n)=longi
                 lat(n)=latit1(ii)
 411           continue
            elseif (longi2(ii).eq.longi1(ii)) then
               npts=nint(abs((2.*(latit2(ii)-latit1(ii))/pixsiz)))
               deltax=(latit2(ii)-latit1(ii))/real(npts)
               latit=latit1(ii)
               do 412 np=1,npts
                 latit=latit+deltax
                 n=n+1
                 lon(n)=longi1(ii)
                 lat(n)=latit
 412           continue
            endif
 444      continue
         elseif (ans.eq.9) then
            goto 34
         else 
            goto 3
         endif
 500  continue
c
c
c -----------------
c
c   Ecriture des donnees
c
 34      open(unit=1,file=nomxy(1:lenxy),status='unknown')
         write(1,*) n
         do 777 i=1,n
            if (lon(i).lt.0.) lon(i)=lon(i)+360.
            write(1,*) lat(i),lon(i),' 65.535'
 777     continue
         close(unit=1)
 1000    format(A)       
       end
