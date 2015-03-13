c   Programme coloraod.f 
c   ce programme permet de transformer une image pgm d epaisseur optique
c   des aerosols en image ppm couleur avec un masque geographique
c
c
c    copyright martin aube 09/04/1999

      program coloraod
c
c ----------
c
c   declaration des variables
c
      real tau(2000,2000),lcellx,lcelly,xcell0,ycell0,pmaskx
      real pmasky,latit,longi,val,scale
      integer lennom,i,j,ncellx,ncelly,rcc(405),gcc(405),bcc(405),r,g,b
      integer maskval(2000,2000),npm,pmaskno(9),imwidth,geomsk
      integer n,hcnt,maxi,maptyp,number(500000),value(500000),multip
      integer hauteur,maskx,masky,ntot,nc,newnx,newny,boxval,box,nbx
      integer nby,bufw,codetype,ncolor
      character*60 nom,mapfile,htmfile,aotfile,tag,codefile
      character*60 rien
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
      scale=1.
c
c     lecture du fichier de parametres
c
      open(unit=25,file='coloraod.par',status='old',err=19)
       read(25,*) nom
       read(25,*) maptyp
       read(25,*) bufw
       read(25,*) codetype
       read(25,*) multip
       read(25,*) scale
       read(25,*) geomsk
      close(unit=25)
c
c    nom du fichier du code couleur
c   
      if (codetype.eq.0) then 
         codefile='color-code.txt'
         ncolor=400
      elseif (codetype.eq.1) then
         codefile='contour10.txt'
         ncolor=10
      else
         codefile='contour20.txt'
         ncolor=20 
      endif           
c
c -----------
c
c   choix du nom de la racine de fichiers
c
c   calcul de la longueur du nom
c
      lennom=index(nom,' ')-1
      aotfile=nom(1:lennom)//'.pgm'
         open(unit=27,file=aotfile,status='old',err=6)
C      Type of aerosol map 
C      1 .......... horizontal map'
C      2 .......... vertical profile'
C      scale the values (0=no, 1=yes)'
c
c ----------
c
c   lecture de l image d epaisseur optique interpolee avec interp.f
c
      print*,'Loading optical depth pgm image:',aotfile(1:lennom+4)
c
c   recherche de la position des headers
c
         bidon='#'
         hcnt=0
         read(27,*)
         do 54 i=1,50
            read(27,*,end=56,err=57) bidon,tag,val
            goto 58
 57         backspace 27
            read(27,*,end=56) bidon
 58         if (bidon(1:1).eq.'#') then
               hcnt=hcnt+1
               if (tag(1:6).eq.'pixsiz') lcellx=val
               if (tag(1:4).eq.'lat0') xcell0=val
               if (tag(1:4).eq.'lon0') ycell0=val 
               lcelly=lcellx
               if (ycell0.eq.0.) ycell0=0.000001
               if (ycell0.lt.0.) ycell0=ycell0+360.
            else
               goto 56
            endif
 54      continue            
 56      rewind 27
c
c   annuler l option de masque geographique si les proprites
c   de la carte sont inconnues
c
         if (lcellx*xcell0*ycell0.eq.0.) geomsk=0
         read(27,*)
         do 55 i=1,hcnt
            read(27,*)
 55      continue
         read(27,*) ncelly, ncellx,maxi
               read(27,*) ((tau(i,j),j=1,ncelly),i=ncellx,1,-1)
         close(unit=27)
         do 5001 i=1,ncellx
            do 5002 j=1,ncelly
               tau(i,j)=scale*tau(i,j)/100.

               if (tau(i,j).gt.2.53) tau(i,j)=2.54

 5002       continue
 5001    continue
         if (xcell0*ycell0*lcellx.eq.0.) then
            if ((maptyp.eq.1).and.(geomsk.eq.1)) then
               print*,'Horizontal pixel size in deg. ?'
               read*, lcellx
               lcelly=lcellx
            endif
            if ((maptyp.eq.1).and.(geomsk.eq.1)) then     
 5         print*,'Center latitude of the south-west pixel in degrees?'
               read*,xcell0
               if (xcell0.lt.-90.) then
                  print*,'Should be greater than -90.'
                  goto 5
               endif
           print*,'Center longitude of the south-west pixel in degrees?'
               read*,ycell0
            endif
         endif      
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
         mapfile='coloraod.ppm'
         open(unit=21,file=mapfile,status='unknown')
         write(21,171) 'P3'
         write(21,171) '# aerosol optical depth ppm image from coloraod'
         write(21,171) '# min. = 0. , max. = 1.'
         write(21,171) '# first data correspond to pixel x=xmax'
         write(21,171) '# and y=1, the second , x=xmax, y=2 and so on.'
         write(21,171) '# x -> nord,  y -> est'
         write(21,171) '# autre details dans le fichier .html'
         write(21,172) ncelly, ncellx
         write(21,*) '255'
         open(unit=25,file=codefile,status='old')
 2415    format(i1)
         do 2416 n=1,ncolor
            read(25,*) rien,rcc(n),gcc(n),bcc(n)
 2416    continue
         close(unit=25)
         do 2411 i=ncellx,1,-1
           do 2311 j=1,ncelly
                   r=254
                   g=254
                   b=254  
              if ((i.gt.bufw).and.(i.le.ncellx-bufw)) then
               if ((j.gt.bufw).and.(j.le.ncelly-bufw)) then                  
                if (nint(tau(i,j)*real(ncolor)).gt.real(ncolor)) then
                   r=254
                   g=254
                   b=254
                   if (nint(tau(i,j)*100.).eq.255) then
                     r=255
                     g=255
                     b=255
                   endif
                elseif (nint(tau(i,j)*real(ncolor)).eq.0) then
                     r=0
                     g=0
                     b=0  
                elseif (nint(tau(i,j)*real(ncolor)).lt.0) then 
                     r=0
                     g=0
                     b=0                          
                else 
                   r=rcc(nint(tau(i,j)*real(ncolor)))
                   g=gcc(nint(tau(i,j)*real(ncolor))) 
                   b=bcc(nint(tau(i,j)*real(ncolor))) 
                endif
               endif
              endif  
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
             if ((maptyp.eq.1).and.(geomsk.eq.1)) then
                latit=real(i-1)*lcellx+xcell0
                longi=real(j-1)*lcelly+ycell0
                newnx=nint((latit-(-90.+180./real(maskx)/2.))/
     +          (180./real(maskx)))+1
                if (newnx.gt.maskx) newnx=newnx-maskx
     
c           print*,'lcelly,ycell0',lcelly,ycell0
c           print*,maskx,masky
c           pause
     
                newny=nint((longi-360./real(masky)/2.)/
     +          (360./real(masky)))+1
                if (newny.gt.masky) newny=newny-masky
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
c   765=valeur de 255 dans le fichier pgm
              if (r+g+b.ne.765) then
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
c
c -------------------------
c
c   fichier html pour presenter les resultats de la carte des
c   epaisseur optique
c
c   imwidth = largeur de l image en pixel a l affichage sur le fichier
c   html
c
c         print*,'Making output html file'
c         imwidth=500
c         hauteur=500*ncellx/ncelly
c 2911    format(a,e8.1e2,a)
c         htmfile='coloraod.html'         
c         open(unit=23,file=htmfile,status='unknown')
c            write(23,*) '<html><head><title>'
c            write(23,*) 'aerosol optical depth map'
c            write(23,*) '</title></head><body bgcolor=white>'
c            write(23,*) '<center><h1>aerosol optical depth map
c     +      </h1></center>'
c            write(23,*) '<center><table border=0 cellpadding=5 
c     +      bgcolor="aaaaaa"><tr><td>'
c            write(23,2814) 'coloraod.gif',hauteur
c 2814       format('<img src="',a,'" height=',I3,' width=500
c     +       border=1></td><td>')
c            write(23,*) '<font size=2><p><b><u>legend:</u></b></p>'
c            write(23,*) '<p>image: coloraod.gif</p>'
c            write(23,*) '<p>image size: ',ncellx,'x',ncelly,'</p>'
c 2912       format(a,f6.3,a)
c            write(23,2912) '<p>pixel size=',lcellx,' deg.</p>'
c            if (maptyp.eq.1) then
c               write(23,2913) '<p>s-w pixel center coords.: '
c     +         ,xcell0,' deg. , ',ycell0,' deg. </p>'
c 2913          format(a,f6.3,a,f7.3,a)
c            endif
c            write(23,*) '<p>black = aod of 0.</p>'
c            write(23,*) '<p>red   = aod of 1.</p>'
c            write(23,*) '<p>white = aod > 1. or no data</p>'
c            write(23,*) '<p><b><u>notes:</u></b></p>'
c            write(23,*) '<p>values of the .pgm file correspond 
c     +to aod * 100</p></td></font></tr>'
c            write(23,*) '<tr><td colspan=2><img src="color-code.gif" 
c     +      width=500 border=1></td></tr>'
c            write(23,*) '</table></body></html>'
c         close(unit=23)              
         stop
 6       print*,'Bad root name.'
         stop
 19      print*,'Parameter file: coloraod.par don t exist.'
         stop
         end
