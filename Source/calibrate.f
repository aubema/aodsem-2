c    programme permettant de normaliser une carte d epaisseur optique
c    des aerosols a une longueur d onde determinee Õ partir d un 
c    fichier lat lon tau de reference.  le programme utilisera ce 
c    fichier afin de determiner le coefficient de normalisation
c    a appliquer Õ la carte de facon a obtenir un accord maximal entre
c    la carte et les donnees du fichier lat lon tau.
c
c    les axes sont definis comme suit
c    x = direction nord = degres
c    y = direction est = degres
c    z = direction haut = metres
c
c    martin aube 19-11-1999
c
c ------
c
c   description des variables
c
c
      program normalize
c
c --------------
c
c    declaration des variables
c
      character*60 nom,aotfi,nomxyt,outfile,tag,fitfile
      character*15 bidon
      integer nxtcel,nytcel,n,lennom,hcnt,i,nx,ny,nt,maxi

      integer nsite,nxcen,nycen,futype,ngaus,nordp,mode,nterms
      integer dflaga(20),k,ii,npts

      real lat(100),lon(100),tauxy(100),x(2048),y(2048),lontau0,lattau0
      real sigmay(2048),deltaa(60),chisqr,memchi,dy,a(60),pixsiz
      real yfit(2048),tau(1000,1000),ndata,sigmaa(60)
c
c ---------
c
c   initialisation des variables
c
       pi=3.141592654
       pi2=pi*pi
       do 40 ii=1,2048
              sigmay(ii)=0.
 40    continue
       do 41 ii=1,60          
           sigmaa(ii)=0.
 41    continue        
c   erreur acceptable sur y
       dy=1.e-20
c
c -----------
c
c   donnees de base
c 
         open(unit=13,file='calibrate.par',status='old')
c 7       print*,'root name of file to normalize (.pgm will be add)?'
c         read*,nom
         read(13,*) nom 
         lennom=index(nom,' ')-1
         aotfi=nom(1:lennom)//'.pgm'  

c 8       print*,'name of the reference data file?'
c         read*,nomxyt
         read(13,*) nomxyt
c 9       print*,'normalization function:'
c         print*,' '
c         print*,'   0 ..... y = m * x'
c         print*,'   1 ..... y = m * x + b'
c         print*,'   2 ..... y = a * x^2 + b * x + c'
c         read*,futype
         read(13,*) futype
         close(unit=13)
 111     if ((futype.gt.2).or.(futype.lt.0)) goto 9
c
c --------------
c
c   lecture de la carte d epaisseur optique
c
         open(unit=2,file=aotfi,status='old')
         open(unit=1,file=nomxyt,status='old')
c
c   recherche de la position des headers
c
         bidon='#'
         hcnt=0
         read(2,*)
         do 54 i=1,50
            read(2,*,end=56,err=57) bidon,tag,value
 57         if (bidon(1:1).eq.'#') then
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
         read(2,*) nytcel, nxtcel, maxi 
         print*,'reading aod data...'
         read(2,*) ((tau(nx,ny),ny=1,nytcel),nx=nxtcel,1,-1)
         close(unit=2)
         if (lattau0*pixsiz*lontau0.eq.0.) then
            print*,'horizontal pixel size in deg. ?'
            read*, pixsiz
 5         print*,'center latitude of the south-west pixel in degrees?'
               read*,lattau0
               if (lattau0.lt.-90.) then
                  print*,'should be greater than -90.'
                  goto 5
               endif
           print*,'center longitude of the south-west pixel in degrees?'
               read*,lontau0
         endif
               if (lontau0.lt.0.) lontau0=lontau0+360.
               if ((real(nytcel)*pixsiz+lontau0).gt.360.) then
                  lontau0=0. 
                  print*,'left longitude has been shifted to 0.'
               endif
               if (lontau0.gt.360.) lontau0=lontau0-360.
c
c   normaliser tau
c
         do 555 nx=1,nxtcel
         do 556 ny=1,nytcel
            tau(nx,ny)=tau(nx,ny)/100.
 556     continue
 555     continue
c
c -----------
c
c   lecture des valeurs ponctuelles pour la normalisation
c
         print*,'reading reference aod data...'
         read(1,*) nsite
         do 777 i=1,nsite
            read(1,*) lat(i),lon(i),tauxy(i)
            if (lon(i).lt.0.) lon(i)=lon(i)+360.
 777     continue
         close(unit=1)
c
c -----------
c
c   comparaison des mesures ponctuelle avec les valeurs correspondantes
c   sur la carte.
c
         ii=0
         do 778 i=1,nsite
            nxcen=nint((lat(i)-lattau0)/pixsiz+1.)
            nycen=nint((lon(i)-lontau0)/pixsiz+1.)
            if ((nxcen.le.nxtcel).and.(nxcen.gt.0)) then
            if ((nycen.le.nytcel).and.(nycen.gt.0)) then
             if (tau(nxcen,nycen).le.2.52) then
               ii=ii+1
               x(ii)=tau(nxcen,nycen)
               y(ii)=tauxy(i)
             endif  
            endif
            endif
 778     continue
         npts=ii
c
c ------------
c
c   calcul de la fonction de normalisation
c
         if (npts.eq.1) then
            if (futype.eq.1) then
               print*,'can t fit a linear function over 1 data.'
               print*,'switching to proportionality function!'
               futype=0
            endif
         elseif (npts.lt.1) then
            print*,'not enough data!!!'
            stop
         elseif (npts.eq.2) then
            if (futype.eq.2) then
               print*,'can t fit a cubic function over 2 data.'
               print*,'switching to linear function!'
               futype=1
            endif
         endif
         if (futype.eq.0) then
            ngaus=0
            nordp=2
            a(1)=0.
            a(2)=y(1)/x(1)
            a(3)=0.
            dflaga(1)=0
            dflaga(2)=1
            dflaga(3)=0
            mode=-1
            nterms=1
            deltaa(1)=a(1)/5.
            deltaa(2)=a(2)/5.
            deltaa(3)=a(3)/5.
         elseif (futype.eq.1) then
            ngaus=0
            nordp=2
            a(2)=(y(1)-y(2))/(x(1)-x(2))
            a(1)=y(1)-a(2)*x(1)
            a(3)=0.
            dflaga(1)=1
            dflaga(2)=1
            dflaga(3)=0
            mode=-1
            nterms=2
            deltaa(1)=a(1)/5.
            deltaa(2)=a(2)/5.
            deltaa(3)=a(3)/5.
         elseif (futype.eq.2) then
            ngaus=0
            nordp=2
            a(2)=(y(1)-y(2))/(x(1)-x(2))
            a(1)=y(1)-a(2)*x(1)
            xinterv=(x(3)-x(1))/2. 
            if (xinterv.eq.0.) then
               a(3)=0.
            else 
               a(3)=((y(2)-y(3))/(x(2)-x(3))-a(2))/xinterv
            endif   
            dflaga(1)=1
            dflaga(2)=1
            dflaga(3)=1
            mode=-1
            nterms=3
            deltaa(1)=a(1)/5.
            deltaa(2)=a(2)/5.
            deltaa(3)=a(3)/5.
         endif
         do 70 k=1,100000
  60        call gridls (x, y, sigmay, npts, nterms, mode, a,
     +      deltaa, sigmaa, yfit, chisqr, ngaus, nordp, dflaga)
  70     continue
  72      print*,'ajustement termine: ',k,' iterations'
          if (a(2).lt.0.) then
             print*,'Bad fit results, switching to y=m*x function!' 
            futype=0
            goto 111
          endif   
c
c ------
c
c   correction de la carte d epaisseur optique
c
         do 656 nx=1,nxtcel
            do 657 ny=1,nytcel
               if (futype.eq.0) then
                  tau(nx,ny)=a(2)*tau(nx,ny)
               elseif (futype.eq.1) then
                  tau(nx,ny)=a(1)+a(2)*tau(nx,ny)
               elseif (futype.eq.2) then
                  tau(nx,ny)=a(1)+a(2)*tau(nx,ny)+a(3)*tau(nx,ny)**2.  
               endif
               if (tau(nx,ny).lt.0.) then
                print*,'Negative corrected value',
     +                  ' switching to y=m*x function!'
                futype=0
                goto 111
               endif                   
 657        continue
 656     continue 
c
c -----------
c
c   writing output aod image
c 
         outfile=nom(1:lennom)//'_nor.pgm'
         open(unit=14,file=outfile,status='unknown')
         print*,'Writing output corrected AOD map: ',outfile
         write(14,1000) 
         write(14,1013)
         write(14,1010) pixsiz
         write(14,1011) lattau0
         write(14,1012) lontau0
 1010    format('# pixsiz ',F7.3)
 1011    format('# lat0 ',F7.3)
 1012    format('# lon0 ',F7.3)
         write(14,*) nytcel, nxtcel 
         write(14,*) maxi
         print*,'writing aod data...'
       write(14,*) ((nint(tau(nx,ny)*100.),ny=1,nytcel),nx=nxtcel,1,-1)
         close(unit=14)
 1000    FORMAT('P2') 
 1013    format('# aod map normalized with normalize.f') 
c
c -----------
c
c   Writing fit results
c  
         fitfile=nom(1:lennom)//'_nor.fit'
         open(unit=4,file=fitfile,status='unknown')
            print*,'Writing fit results in: ',fitfile
            write(4,*), npts, ' ODimage	ODref	ODfit'
            do 506 ii=1,npts
               write(4,*) x(ii),y(ii),yfit(ii)
  506       continue    
            write(4,*) ' '
            write(4,*) 'AOD corrected=',a(1),'+',a(2),'* AOD','+'
     +,a(3),'* AOD^2'
            write(4,*) 'ChiSquare=',chisqr  
         close(unit=4)  
      print*,'End of normalize.'
      stop
  9   print*,'bad function!'
      stop
      end

c
c
c =============================================================
c
c      routine de fit
c
c      but
c       faire un fit par les moindres carres avec une fonction
c       specifiee dans la sous-routine functn
c
c      utilisation
c       call gridls (x, y, sigmay, npts, nterms, mode, a, deltaa,
c          sigmaa, yfit, chisqr, ngaus, nordp, dflaga)
c
c      parametres
c       x  -vecteur de coordonnee 
c       y  -vecteur de la fonction 
c       sigmay  -vecteur de deviation standard pour les y
c       npts  -nombre de paires de points de data
c       nterms  -nombre de parametres de la fonction
c       mode  -poids pour chaque points utile dans la resolution
c              par les moindres carres
c                +1 (instrumental)  poids(i)=1./sigmay(i)**2
c                 0 (aucun poids specifiques)
c                -1 (statistique)   poids(i)=1/y(i)
c       a  - matrice de parametres
c       deltaa  - vecteur pour l'increment des parametres
c       sigmaa  - vecteur de deviation standard
c       yfit  -vecteur des valeurs de y calculees
c       chisqr  -chi carre pour le fit
c       dflaga  - vecteur permettant d'imposer des contraintes
c                 sur les parametres0=valeur fixe 1=libre 2=valeur
c                 precedente
c
c      sous-routines requises
c       functn (x, i, a, ngausdb, ngaus, nordp)
c           evalue la valeur de la fonction pour x
c       fchisq (y, sigmay, npts, nfree, mode, yfit)
c           evalue chi carre
c      commentaires
c        deltaa est editee automatiquement au cours du programme
c
      subroutine gridls (x, y, sigmay, npts, nterms, mode, a, deltaa,
     + sigmaa, yfit, chisqr, ngaus, nordp, dflaga)
      real x(2048), y(2048), sigmay(2048), a(60), deltaa(60),
     + sigmaa(60), yfit(2048)
      real delta, chisqr, free, chisq1, chisq2, chisq3, fn,
     + save
      integer memchi12,dflaga(60)
   11 nfree= npts-nterms
      free=nfree
      chisqr=0.
      if (nfree) 100, 91, 20
   20 do 90 j=1, nterms
c
c     evaluation de chi carre aux deux premiers pts
c
      if (dflaga(j).eq.2.) then
         a(j)=a(j-3)
      else  
        if (deltaa(j).eq.0.) then
          goto 90
        endif
      endif
   21 do 22 i=1, npts
   22 yfit(i)=functn (x, i, a, ngaus, nordp)
   23 chisq1= fchisq (y, sigmay, npts, nfree, mode, yfit)
      fn=0.
      delta=deltaa(j)
      memchi12=0
   41 a(j)=a(j)+delta
      memchi12=memchi12+1
      if (memchi12.eq.3)  then
         a(j)=a(j)-delta
         go to 90
      endif
        do 43 i=1, npts
   43 yfit(i) =functn (x, i, a, ngaus, nordp)
   44 chisq2= fchisq (y, sigmay, npts, nfree, mode, yfit)
   45 if (chisq1-chisq2) 51, 41, 61
c
c     renverse la direction de la recherche (si chi carre augmente)
c
   51 delta =-delta
      a(j) = a(j) +delta
      do 54 i=1, npts
   54 yfit(i)=functn (x, i, a, ngaus, nordp)
      save =chisq1
      chisq1=chisq2
   57 chisq2=save
c
c     incremente a(j) jusqu'a ce que chi carre augmente
c
   61 fn=fn+1.
      a(j)=a(j)+delta
      do 64 i=1, npts
   64 yfit(i)=functn (x, i, a, ngaus, nordp)
      chisq3= fchisq (y, sigmay, npts, nfree, mode, yfit)
   66 if (chisq3-chisq2) 71, 81, 81
   71 chisq1= chisq2
      chisq2=chisq3
      go to 61
c
c     recherche du minimum de la parabole definie par les trois
c     points precedents
c
   81 if ((chisq3-chisq2).eq.0.) then
         go to 82
      endif
      if ((chisq2-chisq3).eq.(chisq1-chisq2)) then
          go to 82
      endif
      delta=delta*(1./(1.+(chisq1-chisq2)/(chisq3-chisq2))+0.5)
   82 a(j)=a(j)-delta
   83 sigmaa(j)=deltaa(j)*sqrt(2./(free*(chisq3-2.*chisq2+chisq1)))
   84 deltaa(j)=deltaa(j)*fn/3.
   90 continue
c
c     evaluation du fit et chi carre pour les parametres finaux
c
   91 do 92 i=1, npts
        yfit(i)=functn(x, i, a, ngaus, nordp)
   92 continue
   93 chisqr=fchisq(y, sigmay, npts, nfree, mode, yfit)
  100 return
      end
c
c
c =============================================================
c
c     fonction functn
c
c    
c     fonction de n gaussiennes + polynome d ordre inferieur ou egal 
c     a 2
c
      function functn (x, i, a, ngaus, nordp)
      dimension x(2048), a(60), z(60), z2(60)
      real a, functn
      ll=1
      functn=0.
      xi=x(i)
      do 200 l=1,ngaus
       z(l)=(xi-a(ll+1))/a(ll+2)
       z2(l)=z(l)**2
       if (z2(l)-50.) 116, 199, 199
  116  functn=functn+a(ll)*exp(-z2(l)/2.)
  199  ll=ll+3
  200 continue
      if (nordp.eq.0) then
        functn=functn+a(ll)
      elseif (nordp.eq.1) then
        functn=functn+a(ll)+a(ll+1)*xi
      elseif (nordp.eq.2) then
        functn=functn+a(ll)+a(ll+1)*xi+a(ll+2)*xi*xi
      endif
      return
      end
c
c
c =============================================================
c
c     fonction  fchisq
c
c
c     evaluation de chi carre pour le fit des donnees
c     fchisq=somme((y-yfit)**2/sigma**2)/nfree
c
c
      function fchisq (y, sigmay, npts, nfree, mode, yfit)
      dimension y(2048), sigmay(2048), yfit(2048)
      real chisq, fchisq, weight, free
   11 chisq=0.
   12 if (nfree) 13, 20, 20
   13 fchisq=0.
      go to 40
c 
c      sommation des chi carres
c
   20 do 30 i=1, npts
   21 if (mode) 22, 27, 29
   22 if (y(i)) 25, 27, 23
   23 weight=1/y(i)
      go to 30
   25 weight=-1/y(i)
      go to 30
   27 weight=1.
      go to 30
   29 weight=1./sigmay(i)**2
   30 chisq=chisq+weight*(y(i)-yfit(i))**2
c
c       division par le nombre de degres de liberte (nfree)
c
   31 free=nfree
      if (free.ne.0) then
        fchisq=chisq/free
      else 
        fchisq=chisq
      endif          
   40 return
      end
