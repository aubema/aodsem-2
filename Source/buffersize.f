c    Programme permettant proceder au calcul d'une valeur de 
c    largeur de l'eponge 
c
c    copyright martin aube 2003
c
c -----------------
c   identification des variables 
c
c
c --------------------
c
c   programme principal
c
c ----------
c
c   declaration des variables
c
c
       real latcen,pixsiz,dx,dy,vref,timestep
       integer buffer
c   
c ----------
c
c   initialisation des variables
c
c      vref=vitesse du vent typique 
       vref=7.5
c
c   entree de valeurs par l'usager
c
c      pixsiz=largeur du pixel en degres
       print*,'Pixel size in degrees?'
       read*,pixsiz
c      latcen=latitude centrale du domaine
       print*,'Domain center latitude?'
       read*,latcen
       print*,'Time step in minute?'
       read*,timestep
c   convertir timestep en secondes
c
       timestep=timestep*60.
c
c  Calcul du buffer stantard
c
       call echelle(latcen,pixsiz,pixsiz,dx,dy)
       buffer=nint(vref*timestep/dy)+1
       print*,'Minimum suggested buffer size=',buffer
       stop
       end 
c
c ---------------------------------------------------------
c
c   routine permettant le calcul de l echelle d une cellule centree
c   a latt et avec une largeur angulaire dang pour une terre 
c   ellipsoide.
c
c   epsilon = difference d angle entre l horizon sur une sphere et
c             l horizon sur un ellipsoide
c   dangx =  taille de la cellule en degres
c   dangy =  taille de la cellule en degres
c   dx = taille de la cellule dans la direction s-n [m]
c   dy = taille de la cellule dans la direction w-e [m]
c   r = distance du point geographique par rapport au centre de la terre
c   a = demi grand axe (centre a l equateur)
c   b = demi petit axe (centre au pole)
c   he = angle d horizon de l ellipsoide
c   hs = angle d horizon de la sphere
c   x = coord de l ellipsoide
c   y=  coord de l ellipsoide
c   
c
      subroutine echelle(latt,dangx,dangy,dx,dy)
      real latt, dangx,dangy,dx, dy, r, a, b, epsilon,pi,x,y,he,hs
      pi=3.141592654
      b=6357000.
      a=6378000.
      latt=abs(latt)
      if (latt.lt.0.01) latt=0.01
      x=((b**2.)/((tan(pi*latt/180.))**2.+(b/a)**2.))**0.5
      y=(b**2.-b**2.*x**2./a**2.)**0.5
      r=(x**2.+y**2.)**.5
c      print*,r
      hs=90.-latt
      he=180.*atan(((b**2.)/(a**2.))*x/y)/pi
c      print*,he
      epsilon=hs-he
      dx=r*(tan(dangx*pi/180.))/cos(pi*epsilon/180.)
      dy=x*(tan(dangy*pi/180.))
      return
      end


       
     


