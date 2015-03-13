c    programme pour calculer une statistique simple sur une
c    image
c    
c    les axes sont definis comme suit
c    x = direction nord = degres
c    y = direction est = degres
c
c
c    copyright martin aube 2000
c
c -----------------
c   identification des variables 
c
c
c --------------------
c
c   programme principal
c
      program statistique
c
c ----------
c
c   declaration des variables
c
      real pixsiz,lattau0,lontau0,tau(1000,1000)
      real value,mean9,absmean,sigma,maxi,mini,sigma2,median,mode
      real modeval,statmean
      real dtau,nb9,min,max,limit
      character*70 bidon,nom,taufile,header,tag,barre
      integer nxcen,nycen,lennom,i,ii,iii,his(1512),nbin,n
      integer nx,ny,nxtcel,nytcel,tmax,hcnt,normal
c
c -----------
c
c   choix du nom de la racine de fichiers
c
     
      Open(unit=13,file='statistique.par',status='old')
      read(13,*) nom
      close(unit=13)
c      print*,'root name of the file (.pgm will be add) ?'  
c      read*, nom
c
c   calcul de la longueur du nom
c
      lennom=index(nom,' ')-1
c
c   valeurs initiales
c
      barre='|**********************************************************
     +*******************'
      dtau=1.
c
c
c -----------
c
c   Lecture du fichier d'epaisseur optique
c
c   fichier pgm pour les images polder 65534 et 65535=aucun signification
c   65533=cotes, autre=aod*1000
c
         taufile=nom(1:lennom)//'.pgm'
         open(unit=2,file=taufile,status='old')
         open(unit=11,file='statistique.res',status='unknown')
c
c   recherche de la position des headers
c
         bidon='#'
         hcnt=0
         read(2,*)
         do 54 i=1,50
            read(2,*,end=56,err=57) bidon,tag
 57         if (bidon.eq.'#') then
               hcnt=hcnt+1
               if (tag(1:6).eq.'pixsiz') then
                 backspace 2
                 read(2,*,end=56,err=57) bidon,tag,value
                 pixsiz=value
               endif
               if (tag(1:4).eq.'lat0') then
                 backspace 2
                 read(2,*,end=56,err=57) bidon,tag,value
                 lattau0=value
               endif
               if (tag(1:4).eq.'lon0') then
                 backspace 2
                 read(2,*,end=56,err=57) bidon,tag,value
                 lontau0=value
               endif 
            endif
 54      continue            
 56      rewind 2
         read(2,*)
         do 55 i=1,hcnt
            read(2,*)
 55      continue
         read(2,*) nytcel, nxtcel,tmax 
         print*,'Reading AOD data...'
         read(2,*) ((tau(nx,ny),ny=1,nytcel),nx=nxtcel,1,-1)
         close(unit=2)
         if (lattau0*pixsiz*lontau0.eq.0.) then
            print*,'Horizontal pixel size in deg. ?'
            read*, pixsiz
 5         print*,'Center latitude of the south-west pixel in degrees?'
               read*,lattau0
               if (lattau0.lt.-90.) then
                  print*,'Should be greater than -90.'
                  goto 5
               endif
           print*,'Center longitude of the south-west pixel in degrees?'
               read*,lontau0
         endif
               if (lontau0.lt.0.) lontau0=lontau0+360.
               if (lontau0.gt.360.) lontau0=lontau0-360.
          absmean=0.
          mean9=0.
          sigma=0.
          maxi=-65532.
          mini=65532.
          nb9=0.
          nb7=0.
          nb5=0.
          nb3=0.
          do 996 ii=1,nxtcel
          do 997 iii=1,nytcel
            if (tau(ii,iii).lt.65533.) then
             if (tau(ii,iii).gt.maxi) maxi=tau(ii,iii)
             if (tau(ii,iii).lt.mini) mini=tau(ii,iii)
             mean9=mean9+tau(ii,iii)
             absmean=absmean+abs(tau(ii,iii))
             nb9=nb9+1.
            endif
 997      continue
 996      continue
          nbin=nint(maxi)-nint(mini)+2
          limit=real(int(mini))-1
c
c  simple stat
c
          mean9=mean9/nb9
          absmean=absmean/nb9    
          do 994 ii=1,nxtcel
          do 995 iii=1,nytcel
            if (tau(ii,iii).lt.65533.) then          
             sigma=sigma+(tau(ii,iii)-mean9)**2.
            endif 
 995      continue
 994      continue
          sigma=(sigma/nb9)**0.5 
c
c   computing histogram
c
          do 893 ii=1,nbin   
             his(ii)=0
 893      continue   
          do 894 ii=1,nxtcel
          do 895 iii=1,nytcel
            do n=1,nbin
              if (tau(ii,iii).ge.(limit+real(n-1)*dtau)) then
              if (tau(ii,iii).lt.(limit+real(n)*dtau)) then
                      his(n)=his(n)+1
                 endif
             endif
            enddo


 895      continue
 894      continue 
          modeval=0.
          statmean=0.
          median=0.
          medianval=0.
          sigma2=0.
          do n=1,nbin
             if (his(n).gt.modeval) then
                modeval=his(n)
                mode=(real(n)-1.)*dtau+limit
             endif
             statmean=statmean+((real(n)-1.)*dtau+limit)*his(n)/nb9
             medianval=medianval+his(n)
             if (medianval.lt.nb9/2)  then
                 median=(real(n)-0.5)*dtau+limit
             endif
          enddo
          do n=1,nbin
             sigma2=sigma2+(((real(n)-1.)*dtau+limit)-statmean)**2.*
     +       his(n)/nb9

          enddo
 
          normal=1
          do ii=1,nbin
             if (normal.lt.his(ii)) normal=his(ii)
          enddo
          if (normal.eq.0) normal=1
          if (normal.lt.60) normal=60
          do ii=1,nbin   
             his(ii)=his(ii)*60/normal
          enddo  
          write(11,*) 'Number of points   =',nb9          
          write(11,*) 'Arithmetic mean   =',nint(mean9)
          write(11,*) 'Absolute arithmetic mean   =',nint(absmean)
          write(11,*) 'Mean (from histogram)   =',statmean
          write(11,*) 'Median (from histogram)   =',median
          write(11,*) 'Mode (from histogram)   =',mode          
          write(11,*) 'Min =',nint(mini),' Max =',nint(maxi)
          write(11,*) 'Stdv (from histogram)  =',sqrt(sigma2)
          write(11,*) ' '
          write(11,*) 'Histogram (each * correspond to',real(normal/60),
     +    ' count)'
          write(11,*) ' '
          write(11,783)
          write(11,780) (real(ii-1)*dtau+limit,barre(1:his(ii)+1),
     +    his(ii),ii=1,nbin)
          write(11,781)
          write(11,782)

 780     format(F6.1,2x,A,i4) 
 781     format('        |')
 782     format('        |/')
 783   format('        |-------------------------------------------->')
         close(unit=11)   
       end
