c   Programme compcolor.f 
c   ce programme permet de transformer 3 images pgm d epaisseur optique
c   des aerosols en image ppm couleur 3 couches avec un masque 
c   geographique
c
c
c    copyright martin aube 2001

      program compcolor
c
c ----------
c
c   declaration des variables
c
      real tau1(1000,1000),tau2(1000,1000),tau3(1000,1000)
      real lcellx,lcelly,xcell0,ycell0,pmaskx,offset1,offset2
      real pmasky,latit,longi,val,offset3,gain1,gain2,gain3
      integer i,j,ncellx,ncelly,r,g,b
      integer maskval(1000,2000),npm,pmaskno(9),imwidth,geomsk
      integer n,hcnt,maxi
      integer hauteur,maskx,masky,ntot,nc,newnx,newny,boxval,box,nbx
      integer nby,lennom1,lennom2,lennom3
      character*60 mapfile,aotfile,tag
      character*60 rien,nom1,nom2,nom3,aot1,aot2,aot3
      character bidon
c
c ----------
c
c   donnees initiales
c
      lcellx=0.
      xcell0=0.
      ycell0=0.
      geomsk=0
      tag='scrapout'
      val=0
      do i=1,1000
      do j=1,1000
         tau1(i,j)=0.
         tau2(i,j)=0.
         tau3(i,j)=0.
      enddo
      enddo
      
c
c     lecture du fichier de parametres
c
      open(unit=25,file='compcolor.par',status='old',err=19)
       read(25,*) nom1
       read(25,*) offset1, gain1
       read(25,*) nom2
       read(25,*) offset2, gain2
       read(25,*) nom3
       read(25,*) offset3, gain3
       read(25,*) geomsk
      close(unit=25)      
c
c -----------
c
c   choix du nom de la racine de fichiers
c
c   calcul de la longueur du nom
c
      lennom1=index(nom1,' ')-1
      aot1=nom1(1:lennom1)//'.pgm'
      lennom2=index(nom2,' ')-1
      aot2=nom2(1:lennom2)//'.pgm'
      lennom3=index(nom3,' ')-1
      aot3=nom3(1:lennom3)//'.pgm'
c
c ----------
c
c   lecture des images d epaisseur optique
c
      print*,'Loading optical depth pgm images...'
c
c   rouge
c
      if (nom1(1:lennom1).ne.'0') then
         open(unit=1,file=aot1,status='unknown',err=6 )    
c
c   recherche de la position des headers
c
         bidon='#'
         hcnt=0
         read(1,*)
         do i=1,50
            read(1,*,end=56,err=57) bidon,tag,val
            goto 58
 57         backspace 1
            read(1,*,end=56) bidon
 58         if (bidon(1:1).eq.'#') then
               hcnt=hcnt+1
               if (tag(1:6).eq.'pixsiz') lcellx=val
               if (tag(1:4).eq.'lat0') xcell0=val
               if (tag(1:4).eq.'lon0') ycell0=val 
               lcelly=lcellx
            else
               goto 56
            endif
         enddo            
 56      rewind 1
c
c   annuler l option de masque geographique si les proprites
c   de la carte sont inconnues
c       
         if (geomsk.eq.1) then
          if (lcellx*xcell0*ycell0.eq.0.) then
            geomsk=0
            print*,'No geographical information in header 1'
            print*,'Can t draw geographical mask!'
          endif
         endif
         read(1,*)
         do i=1,hcnt
            read(1,*)
         enddo
         read(1,*) ncelly, ncellx,maxi
               read(1,*) ((tau1(i,j),j=1,ncelly),i=ncellx,1,-1)
         close(unit=1)
      endif
c
c    vert
c   
      if (nom2(1:lennom2).ne.'0') then
         open(unit=1,file=aot2,status='unknown',err=6 )    
c
c   recherche de la position des headers
c
         bidon='#'
         hcnt=0
         read(1,*)
         do i=1,50
            read(1,*,end=156,err=157) bidon,tag,val
            goto 158
 157        backspace 1
            read(1,*,end=156) bidon
 158        if (bidon(1:1).eq.'#') then
               hcnt=hcnt+1
               if (tag(1:6).eq.'pixsiz') lcellx=val
               if (tag(1:4).eq.'lat0') xcell0=val
               if (tag(1:4).eq.'lon0') ycell0=val 
               lcelly=lcellx
            else
               goto 156
            endif
         enddo            
 156     rewind 1
c
c   annuler l option de masque geographique si les proprites
c   de la carte sont inconnues
c       
         if (geomsk.eq.1) then
          if (lcellx*xcell0*ycell0.eq.0.) then
            geomsk=0
            print*,'No geographical information in header 2'
            print*,'Can t draw geographical mask!'
          endif
         endif
         read(1,*)
         do i=1,hcnt
            read(1,*)
         enddo
         read(1,*) ncelly, ncellx,maxi
               read(1,*) ((tau2(i,j),j=1,ncelly),i=ncellx,1,-1)
         close(unit=1)
      endif
c
c    bleu
c   
      if (nom3(1:lennom3).ne.'0') then
         open(unit=1,file=aot3,status='unknown',err=6 )    
c
c   recherche de la position des headers
c
         bidon='#'
         hcnt=0
         read(1,*)
         do i=1,50
            read(1,*,end=256,err=257) bidon,tag,val
            goto 258
 257        backspace 1
            read(1,*,end=256) bidon
 258        if (bidon(1:1).eq.'#') then
               hcnt=hcnt+1
               if (tag(1:6).eq.'pixsiz') lcellx=val
               if (tag(1:4).eq.'lat0') xcell0=val
               if (tag(1:4).eq.'lon0') ycell0=val 
               lcelly=lcellx
            else
               goto 256
            endif
         enddo            
 256     rewind 1
c
c   annuler l option de masque geographique si les proprites
c   de la carte sont inconnues
c       
         if (geomsk.eq.1) then
          if (lcellx*xcell0*ycell0.eq.0.) then
            geomsk=0
            print*,'No geographical information in header 3'
            print*,'Can t draw geographical mask!'
          endif
         endif
         read(1,*)
         do i=1,hcnt
            read(1,*)
         enddo
         read(1,*) ncelly, ncellx,maxi
               read(1,*) ((tau3(i,j),j=1,ncelly),i=ncellx,1,-1)
         close(unit=1)
      endif
      do i=1,ncellx
       do j=1,ncelly
          tau1(i,j)=tau1(i,j)*gain1+offset1
          if (tau1(i,j).gt.65535.) then
             tau1(i,j)=65535.
             tau2(i,j)=65535.
             tau3(i,j)=65535.
          else
             tau2(i,j)=tau2(i,j)*gain2+offset2
             if (tau2(i,j).gt.65535.) then
                tau1(i,j)=65535.
                tau2(i,j)=65535.
                tau3(i,j)=65535.
             else
               tau3(i,j)=tau3(i,j)*gain3+offset3
               if (tau3(i,j).gt.65535.) then
                  tau1(i,j)=65535.
                  tau2(i,j)=65535.
                  tau3(i,j)=65535.
               endif
             endif
          endif
       enddo
      enddo
c
c ----------------
c
c   Lecture du masque geographique
c
         if (geomsk.eq.1) then
         open(unit=3,file='worldmask.pbm',status='unknown')
          read(3,*) 
          read(3,*) masky,maskx
          read(3,*) ((maskval(nx,ny),ny=1,masky),nx=maskx,1,-1)
         close(unit=3)    
         endif         
c
c ------------------
c
c   fabrication de l image ppm (carte de l epaisseur optique)
c
      print*,'Making optical depth color map...'
 171  format(a)
 172  format(i6,1x,i6)
 173  format(i3,1x,i3,1x,i3)
         mapfile='compcolor.ppm'
         open(unit=21,file=mapfile,status='unknown')
         write(21,171) 'P3'
         write(21,171) '# AOD ppm image from compcolor'
         write(21,171) '# min. = 0. , max. = 1.'
         write(21,171) '# first data correspond to pixel x=xmax'
         write(21,171) '# and y=1, the second , x=xmax, y=2 and so on.'
         write(21,171) '# x -> nord,  y -> est'
         write(21,172) ncelly, ncellx
         write(21,*) '65535'
         do 2411 i=ncellx,1,-1
           do 2311 j=1,ncelly
              r=tau1(i,j)
              g=tau2(i,j) 
              b=tau3(i,j) 
c
c   ajout du masque geographique
c
c   pour utilisation du masque worldmask
c   et file='worldmask.txt' dans l instruction
c   open(unit=28,...
c
c   worldmaskx2 possede une resolution de 0.197044 degres
c
c
             if (geomsk.eq.1) then
                latit=real(i-1)*lcellx+xcell0
                longi=real(j-1)*lcelly+ycell0
                if (longi.gt.360.) longi=longi-360.
                if (longi.lt.0.) longi=longi+360.
                newnx=nint((latit-(-90.+180./real(maskx)/2.))/
     +          (180./real(maskx)))+1
                newny=nint((longi-360./real(masky)/2.)/
     +          (360./real(masky)))+1
                box=nint(lcellx/(360./real(maskx))/2.)
                boxval=0
                do  1010 nbx=newnx-box,newnx+box+1
                do 1011 nby=newny-box,newny+box+1
                  if ((nbx.ge.1).and.(nbx.le.maskx)) then
                  if ((nby.ge.1).and.(nby.le.masky)) then          
                    boxval=boxval+maskval(nbx,nby)
                  endif
                  endif
 1011           continue
 1010           continue
c
c   196605=valeur de 65535 dans le fichier pgm
              if (r+g+b.ne.196605) then
                if (boxval.gt.0) then
                         r=200
                         g=200
                         b=200
                endif
              endif
            endif
               write(21,173) r,g,b
 2311       continue
 2411    continue
         close(unit=21)
         stop
 6       print*,'Bad root name.'
         stop
 19      print*,'Parameter file: compcolor.par don t exist.'
         stop
         end
