c    Programme permettant de creer un diffusiogramme spatial
c    a partir de deux cartes d epaisseur optique
c    des aerosols a une longueur d onde determinee
c    Cela permet de verifier s il y a correlation entre deux images
c    par exemple entre une observation et une modelisation en faisant 
c    varier un decallage spatial entre les images nous cherchons le 
c    maximum de correlation.
c    les valeurs superieures ou egales a 253 (tau>=2.53) ne sont pas
c    considerees. 
c    les axes sont definis comme suit
c    x = direction nord = degres
c    y = direction est = degres
c    z = direction haut = metres
c
c    Martin Aube 03-2002
c
c ------
c
c   Description des variables
c
c
      program crosscorrelation
c
c --------------
c
c    Declaration des variables
c
      character*60 nom,aotfi,filout,name
      character*15 bidon
      character*22 date
      character*2 deltat
      character*1 fdx,fdy
      integer nxtcel,nytcel,n,lennom,hcnt,i,nx,ny,datelen,nt,maxi
      integer ia,ndata,buffer,wid,dx,dy,sizt,ndat0,iii,jjj,k
      parameter(sizt=125*275)
      real tau(4,125,275),nn,r,sumxy,sumx,sumy,sumx2,sumy2,x(sizt)
      real y(sizt),rms,resid,residu(sizt)
      integer dflaga(20),mode,ngaus,nordp,nterms,jj,ii
      real a(60),deltaa(60)
c
c ---------
c
c   Initialisation des variables
c
       pi=3.141592654
       pi2=pi*pi
       do i=1,sizt
          residu(i)=0.
       enddo
c
c -----------
c
c   Donnees de base
c 
      open(unit=13,file='crosscorrelation.par',status='old')
c      print*,'Output root file name (.cxy will be add)'
c      read*,date
      read(13,*) date
      datelen=index(date,' ')-1
      do 700 n=1,2
      if (n.eq.1) then  
c         print*,'Root name of observation file (.pgm will be added)?'
c         read*,nom
         read(13,*) nom
      else
c         print*,'Root name of model results file (.pgm will be added)?'
c         read*,nom
         read(13,*) nom
      endif  
         lennom=index(nom,' ')-1
         aotfi=nom(1:lennom)//'.pgm'
         open(unit=27,file=aotfi,status='old')
c
c --------------
c
c   recherche de la position des headers et lecture de la premiere 
c   image
c
         bidon='#'
         hcnt=0
         read(27,*)
         do 154 i=1,50
            read(27,*,end=156) bidon
            if (bidon.eq.'#') hcnt=hcnt+1
 154      continue            
 156      rewind 27
         read(27,*)
         do 155 i=1,hcnt
            read(27,*)
 155      continue
         read(27,*) nytcel, nxtcel, maxi 
         print*,'Reading AOD data...'
         read(27,*) ((tau(n,nx,ny),ny=1,nytcel),nx=nxtcel,1,-1)
         close(unit=27)
 700  continue
c       print*,'Width of the buffer zone (in pixel)?'
c       read*,buffer 
        read(13,*) buffer
       close(unit=13)
c
c   remplacer les valeurs a l'interieur du buffer par des 253.
c
        do nx=1,nxtcel
        do ny=1,nytcel
           if (nx.le.buffer) then
             tau(1,nx,ny)=253.
             tau(2,nx,ny)=253.
           endif
           if (nx.gt.nxtcel-buffer) then
             tau(1,nx,ny)=253.
             tau(2,nx,ny)=253.
           endif
           if (ny.le.buffer) then
             tau(1,nx,ny)=253.
             tau(2,nx,ny)=253.
           endif
           if (ny.gt.nytcel-buffer) then
             tau(1,nx,ny)=253.
             tau(2,nx,ny)=253.
           endif



        enddo
        enddo
c
c  decallage de la seconde image par rapport a la premiere
c  wid=demi largeur du decallage max
c
      filout=date(1:datelen)//'.cxy'
      print*,'fichier de sortie:',filout
      open(unit=1,file=filout,status='unknown')
         wid=9
         write(1,*) 4*wid*wid,'   ! d dx dy r rms residu'
         
         do 123 ii=1,2*wid+1

          dx=-wid+ii-1
          do 124 jj=1,2*wid+1
            dy=-wid+jj-1

          ndat0=0
           do nx=1,nxtcel
            do ny=1,nytcel
             if ((nx+dx.le.nxtcel).and.(nx+dx.ge.1)) then
              if ((ny+dy.le.nytcel).and.(ny+dy.ge.1)) then              
                 tau(4,nx,ny)=tau(2,nx+dx,ny+dy)
              else
                 tau(4,nx,ny)=0.
              endif
             else
                 tau(4,nx,ny)=0.
             endif
         if ((dx.eq.0).and.(dy.eq.0)) then
             if ((tau(4,nx,ny).gt.0.).and.(tau(4,nx,ny).lt.253.)) then
             if ((tau(1,nx,ny).gt.0.).and.(tau(1,nx,ny).lt.253.)) then
                 ndat0=ndat0+1
             endif
             endif
         endif   
                    
            enddo
            
           enddo
         fdx='+'
         fdy='+'
         if (dx.lt.0) fdx='-'
         if (dy.lt.0) fdy='-'
         name='dif'//date(1:datelen)//fdx//char(48+abs(dx))//fdy//
     +   char(48+abs(dy))//'.cxy'
 
           open(unit=12,file=name,status='unknown')
              write(12,*) ndat0,' Observ. 	Model'
              do iii=1,nxtcel
              do jjj=1,nytcel
          if ((tau(4,iii,jjj).gt.0.).and.(tau(4,iii,jjj).lt.253.)) then
          if ((tau(1,iii,jjj).gt.0.).and.(tau(1,iii,jjj).lt.253.)) then
                write(12,*) tau(1,iii,jjj),tau(4,iii,jjj)
          endif
          endif
              enddo
              enddo
            close(unit=12)





c
c   Calcul de la correlation
c
c
           i=0
           nn=0.
           r=0.
           sumxy=0.
           sumx=0.
           sumy=0.
           sumx2=0.
           sumy2=0.
           resid=0.
           rms=0.
           do nx=1,nxtcel
            do ny=1,nytcel
              i=i+1
              x(i)=tau(1,nx,ny)
              y(i)=tau(4,nx,ny) 
              if ((x(i).gt.0.).and.(x(i).lt.253.)) then
              if ((y(i).gt.0.).and.(y(i).lt.253.)) then
                nn=nn+1.
                sumxy=sumxy+x(i)*y(i)
                sumx=sumx+x(i)
                sumy=sumy+y(i)
                sumx2=sumx2+x(i)**2.
                sumy2=sumy2+y(i)**2.
c
c   Calcul du rms
c
                rms=rms+(x(i)-y(i))**2.
c
c   Calcul du residu moyen
c
                resid=resid+(x(i)-y(i))
c
c   calcul du residu
c
                residu(int(nn))=x(i)-y(i)
              endif
              endif
            enddo
           enddo
c
c   Fit lineaire sur x et le residu
c
            ngaus=0
            nordp=2
            a(2)=(residu(1)-residu(2))/(x(1)-x(2))
            a(1)=residu(1)-a(2)*x(1)
            a(3)=0.
            dflaga(1)=1
            dflaga(2)=1
            dflaga(3)=0
            mode=-1
            nterms=2
            deltaa(1)=a(1)/5.
            deltaa(2)=a(2)/5.
            deltaa(3)=a(3)/5. 
c         do 170 k=1,1000
c 160         call gridls (x, residu, sigmay, int(nn), nterms, mode, a,
c     +      deltaa, sigmaa, yfit, chisqr, ngaus, nordp, dflaga)
c 170      continue
c 172      print*,'Ajustement lineaire du residu termine: ',k,
c     +' iterations'
c
c  coeff. correlation
c
           r=(nn*sumxy-sumx*sumy)/(sqrt(nn*sumx2-sumx**2.)*
     + sqrt(nn*sumy2-sumy**2.))   
c
c  rms
c
           rms=sqrt(rms)/nn
c
c  residu moyen
c
           resid=resid/nn
c
c  ecriture des resultats
c

           write(1,*) sqrt(dx**2.+dy**2.),dx,dy,r,rms,resid





  124     continue
  123     continue

      close(unit=1) 




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
c                 sur les parametres 0=valeur fixe 1=libre 2=valeur
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
      integer sizt
      parameter(sizt=125*275)
      real x(sizt), y(sizt), sigmay(sizt), a(60), deltaa(60),
     + sigmaa(60), yfit(sizt)
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
      integer sizt
      parameter(sizt=125*275)
      real x(sizt), a(60), z(60), z2(60)

      real functn
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
      integer sizt
      parameter(sizt=125*275)
      real y(sizt), sigmay(sizt), yfit(sizt)
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




