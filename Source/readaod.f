c    programme pour lire une valeur sur une carte d epaisseur optique
c    Õ partir de la latitude et la longitude du lieu 
c    
c    les axes sont definis comme suit
c    x = direction nord = degres
c    y = direction est = degres
c
c
c    copyright martin aube 06/05/1999
c
c -----------------
c   identification des variables 
c
c
c --------------------
c
c   programme principal
c
      program readaod
c
c ----------
c
c   declaration des variables
c
      real pixsiz,lattau0,lontau0,tau(1000,1000)
      real lat(25),lon(25),value,mean9,mean7,mean5,mean3,sigma,maxi,mini
      real dtau,nb9,nb7,nb5,nb3
      character*60 bidon,nom,taufile,header,tag,barre
      integer nxcen,nycen,lennom,i,ii,iii,his(10)
      integer nx,ny,nxtcel,nytcel,tmax,hcnt
c
c -----------
c
c   choix du nom de la racine de fichiers
c
     
      Open(unit=13,file='readaod.par',status='old')
      read(13,*) nom
c      print*,'root name of the file (.pgm will be add) ?'  
c      read*, nom
c
c   calcul de la longueur du nom
c
      lennom=index(nom,' ')-1
c
c   valeurs initiales
c
      barre='***************************************************'
c
c
c -----------
c
c   Lecture du fichier d'epaisseur optique
c
c   fichier pgm pour les images polder 254 et 255=aucun signification
c   253=cotes, autre=aod*100
c
         taufile=nom(1:lennom)//'.pgm'
         open(unit=2,file=taufile,status='old')
         open(unit=11,file='readaod.res',status='unknown')
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
c
c -----------------
c
c   Lecture des donnees
c
c         print*,'Number of sites?'
c         read*,nsite
         read(13,*) nsite
         do 777 i=1,nsite
c            print*,'Latitude, longitude of site #',i,' ?'
c            read*,lat(i),lon(i)
             read(13,*) lat(i),lon(i)
            if (lon(i).lt.0.) lon(i)=lon(i)+360.
 777     continue
         print*,' '
         print*,'------------------------------------------------------'
         print*,'                       RESULTS'
         print*,' '
         write(11,*) ' '
         write(11,*) '----------------------------------------------'
         write(11,*) '                       RESULTS'
         write(11,*) ' '
         do 778 i=1,nsite
            nxcen=nint((lat(i)-lattau0)/pixsiz+1.)
            nycen=nint((lon(i)-lontau0)/pixsiz+1.)
            if (tau(nxcen,nycen).gt.252.) then
               print*,'AOD*100 for site #',i,'= no data'
               write(11,*) 'AOD*100 for site #',i,'= no data'
            else
               Print*,'AOD*100 around site #',i
               print*,' '
               write(11,*) 'AOD*100 around site #',i
               write(11,*) ' '
          do 998 ii=4,-4,-1
          write(*,1111) (nint(tau(nxcen+ii,nycen+iii)),iii=-4,4)
          print*,' '
          write(11,1111) (nint(tau(nxcen+ii,nycen+iii)),iii=-4,4)
          write(11,*) ' '
 1111     format(9I4)
 998      continue
          mean5=0.
          mean7=0.
          mean3=0.
          sigma=0.
          maxi=0.
          mini=252.
          nb9=0.
          nb7=0.
          nb5=0.
          nb3=0.
          do 996 ii=4,-4,-1
          do 997 iii=-4,4
            if (tau(nxcen+ii,nycen+iii).lt.253.) then
             if (tau(nxcen+ii,nycen+iii).gt.maxi) maxi=
     +       tau(nxcen+ii,nycen+iii)
             if (tau(nxcen+ii,nycen+iii).lt.mini) mini=
     +       tau(nxcen+ii,nycen+iii)
             mean9=mean9+tau(nxcen+ii,nycen+iii)
             nb9=nb9+1.
             if ((abs(ii).le.3).and.(abs(iii).le.3)) then
               mean7=mean7+tau(nxcen+ii,nycen+iii)
               nb7=nb7+1.
             endif
             if ((abs(ii).le.2).and.(abs(iii).le.2)) then
               mean5=mean5+tau(nxcen+ii,nycen+iii)
               nb5=nb5+1.
             endif
             if ((abs(ii).le.1).and.(abs(iii).le.1)) then
               mean3=mean3+tau(nxcen+ii,nycen+iii)
               nb3=nb3+1.
             endif
            endif
 997      continue
 996      continue
c
c  simple stat
c
          mean9=mean9/nb9
          mean7=mean7/nb7
          mean5=mean5/nb5
          mean3=mean3/nb3
          dtau=(maxi-mini)/10.
          do 994 ii=1,-1,-1
          do 995 iii=-1,1
            if (tau(nxcen+ii,nycen+iii).lt.253.) then          
             sigma=sigma+(tau(nxcen+ii,nycen+iii)-mean3)**2.
            endif 
 995      continue
 994      continue
          sigma=(sigma/nb3)**0.5
c
c   computing histogram
c
          do 893 ii=1,10   
             his(ii)=0
 893      continue         
          do 894 ii=4,-4,-1
          do 895 iii=-4,4
           if (tau(nxcen+ii,nycen+iii).lt.mini+dtau) then
            his(1)=his(1)+1
           else
            if (tau(nxcen+ii,nycen+iii).lt.mini+2.*dtau) then
             his(2)=his(2)+1
            else
             if (tau(nxcen+ii,nycen+iii).lt.mini+3.*dtau) then
              his(3)=his(3)+1
             else
              if (tau(nxcen+ii,nycen+iii).lt.mini+4.*dtau) then
               his(4)=his(4)+1
              else
               if (tau(nxcen+ii,nycen+iii).lt.mini+5.*dtau) then
                his(5)=his(5)+1
               else
                if (tau(nxcen+ii,nycen+iii).lt.mini+6.*dtau) then
                 his(6)=his(6)+1
                else
                 if (tau(nxcen+ii,nycen+iii).lt.mini+7.*dtau) then
                  his(7)=his(7)+1
                 else
                  if (tau(nxcen+ii,nycen+iii).lt.mini+8.*dtau) then
                   his(8)=his(8)+1
                  else
                   if (tau(nxcen+ii,nycen+iii).lt.mini+9.*dtau) then
                    his(9)=his(9)+1
                   else
                    his(10)=his(10)+1
                   endif
                  endif
                 endif
                endif
               endif
              endif
             endif
            endif
           endif
 895      continue
 894      continue   
          print*,'Central pixel value =',nint(tau(nxcen,nycen))
          write(11,*) 'Site #',i
          write(11,*) ' '
          write(11,*) 'Central pixel value =',nint(tau(nxcen,
     +    nycen))
          print*,'Mean over 3x3 box   =',nint(mean3)
          write(11,*) 'Mean over 3x3 box   =',nint(mean3)
          print*,'Mean over 5x5 box   =',nint(mean5)
          write(11,*) 'Mean over 5x5 box   =',nint(mean5)
          print*,'Mean over 7x7 box   =',nint(mean7)
          write(11,*) 'Mean over 7x7 box   =',nint(mean7)
          print*,'Mean over 9x9 box   =',nint(mean9)
          write(11,*) 'Mean over 9x9 box   =',nint(mean9)
          print*,'Min =',nint(mini),' Max =',nint(maxi)
          write(11,*) 'Min =',nint(mini),' Max =',nint(maxi)
          print*,'Stdv (3x3 box)  =',sigma
          write(11,*) 'Stdv (3x3 box)  =',sigma
          print*,' '
          write(11,*) ' '
          print*,'Histogram for the 9x9 box'
          write(11,*) 'Histogram for the 9x9 box'
          print*,' '
          write(11,*) ' '
          write(*,780) ((real(ii)-0.5)*dtau+mini,barre(1:his(ii)),
     +    ii=1,10)
          write(11,780) ((real(ii)-0.5)*dtau+mini,barre(1:his(ii)),
     +    ii=1,10)
            endif
 778     continue
 780     format(F6.2,2x,A) 
         close(unit=11)   
         close(unit=13)
       end
