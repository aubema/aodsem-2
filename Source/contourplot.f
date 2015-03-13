       program contourplot
       integer ndata,i,j,option,lennom,nmark,nm,mode,n
       integer ncellx,ncelly,maxi,nlev,ni,nj,scaling
       real x(100000), y(100000),ordrex,ordrey,scalex,scaley,xmin,ymin
       real ymax,xmax,lcellx,lcelly,xcell0,ycell0,value,fg(275,275)
       real tau(275,275),trans(6),taumax,dlev,level(100),bg(275,275)
       character*15 bidon
       character*60 filename,tag
       character*60 title,xlabel,ylabel
       do i=1,275
       do j=1,275
          tau(i,j)=0.
          bg(i,j)=0.
       enddo
       enddo
       taumax=0.


         print*,'Choose the operating mode:'
         print*,'  0 ........ contour plot on a pgm map'
         print*,'  1 ........ contour plot from cxy file'
         read*,mode
       


          print*,'Root file name (.cxy or .pgm will be added) ?'
          read*,filename
          lennom=index(filename,' ')-1
          if (mode.eq.0) filename=filename(1:lennom)//'.pgm'
          if (mode.eq.1) filename=filename(1:lennom)//'.cxy'
          print*,'Title (in quote)?'
          read*,title
          print*,'X label (in quote)?'
          read*,xlabel
          print*,'Y label (in quote)?'
          read*,ylabel
            print*,'Scaling mode:'
            print*,'  0 .......... normal'
            print*,'  1 .......... log10'
            read*,scaling

       call pgbegin(0,'/PS',1,1)


          xmin=1.e+20
          ymin=1.e+20
          xmax=-1.e+20
          ymax=-1.e+20     
          if (mode.eq.0) then
c
c ----------
c
c   lecture de l image d epaisseur optique
c
      print*,'Loading optical depth pgm image...'
c
c   recherche de la position des headers
c
        open(unit=27,file=filename,status='old',err=7)
         bidon='#'
         hcnt=0
         read(27,*)
         do i=1,10
            read(27,*,end=56,err=57) bidon,tag,value
 57         if (bidon.eq.'#') then
               hcnt=hcnt+1
               if (tag(1:6).eq.'pixsiz') lcellx=value
               if (tag(1:4).eq.'lat0') ycell0=value
               if (tag(1:4).eq.'lon0') xcell0=value 
               lcelly=lcellx
            endif
         enddo            
 56      rewind 27
         read(27,*)
         do i=1,hcnt
            read(27,*)
         enddo
         if (xcell0*ycell0*lcellx.eq.0.) then
            print*,'Horizontal pixel size in deg. ?'
            read*, lcellx
            lcelly=lcellx
 5         print*,'Center latitude of the south-west pixel in degrees?'
               read*,ycell0
               if (ycell0.lt.-90.) then
                  print*,'Should be greater than -90.'
                  goto 5
               endif
           print*,'Center longitude of the south-west pixel in degrees?'bgd-0152 
               read*,xcell0
         endif
         read(27,*) ncellx, ncelly,maxi
         read(27,*) ((tau(i,j),i=1,ncellx),j=ncelly,1,-1)
         do 3411 j=ncelly,1,-1
            do 3311 i=1,ncellx
               tau(i,j)=tau(i,j)/100.
               if (tau(i,j).ge.2.53) tau(i,j)=0.
 3311       continue
 3411    continue
         close(unit=27)
         if (xcell0.lt.0.) xcell0=xcell0+360.
         xmax=xcell0+real(ncellx)*lcellx  
         xmin=xcell0
         ymax=ycell0+real(ncelly)*lcelly 
         ymin=ycell0   
         do i=1,ncellx
          do j=1,ncelly
            if (tau(i,j).gt.taumax) taumax=tau(i,j)
          enddo
         enddo
c
c   echelle
c
         if (scaling.eq.1) then
           do i=1,ncellx
            do j=1,ncelly
              if (tau(i,j).gt.0.) tau(i,j)=log10(tau(i,j))
            enddo
           enddo  
           taumax=log10(taumax)    
          endif  
 
          elseif (mode.eq.1) then
       open(unit=1,file=filename,status='old')
            print*,'Reading and inspecting .cxy file...'
            read(1,*) ndata
            do i=1,ndata
               read(1,*) x(i),y(i)
               if (x(i).gt.xmax) xmax=x(i)
               if (x(i).lt.xmin) xmin=x(i)
               if (y(i).gt.ymax) ymax=y(i)
               if (y(i).lt.ymin) ymin=y(i)
            enddo
c
c           diviser le domaine des valeurs
c
            ncellx=100
            ncelly=100
            xmin=0.
            ymin=0.
            dx=xmax/real(ncellx)
            dy=ymax/real(ncelly)
            do n=1,ndata
              ni=int(x(n)/dx)
              nj=int(y(n)/dy)
              tau(ni,nj)=tau(ni,nj)+1.

            enddo 
            taumax=0.
            do i=1,ncellx
            do j=1,ncelly
             if (tau(i,j).gt.taumax) taumax=tau(i,j)
            enddo
            enddo

c
c   echelle
c
         if (scaling.eq.1) then
           do i=1,ncellx
            do j=1,ncelly
              
              if (tau(i,j).gt.0.) tau(i,j)=log10(tau(i,j))

            enddo
           enddo  
           taumax=log10(taumax)    
          endif 
            xcell0=dx/2.
            ycell0=dy/2.
            lcellx=dx
            lcelly=dy
          close(unit=1)
          endif

       do i=1,275
       do j=1,275
          fg(i,j)=taumax
       enddo
       enddo
c
c      determiner le nombre de niveau et le vecteur de niveaux
c
          print*,'taumax=',taumax
          nlev=10
          dlev=taumax/real(nlev)
          do n=1,nlev
            level(n)=real(n)*dlev
            print*,'level(',n,')=',level(n)
          enddo
c
c     matrice de transformation
c
       trans(1)=xcell0
       trans(2)=lcellx
       trans(3)=0.
       trans(4)=ycell0
       trans(5)=0.
       trans(6)=lcelly
         print*,xcell0,ycell0,lcellx,lcelly

c            do j=ncelly,1,-1         
c         write(*,*) ((tau(i,j)),i=1,ncellx) 
c           enddo  

          print*,'Creating contour plot...'
          call pgenv(xmin,xmax,ymin,ymax,0,1)
          call pglabel(xlabel,ylabel,title)
          call pggray(tau,ncellx,ncelly,1,ncellx,1,ncelly,fg,bg,trans)
          call pgcont(tau,ncellx,ncelly,1,ncellx,1,ncelly,level,nlev,
     +trans)

       
          call pgend
       stop
  7    print*,'Erreur de lecture!'
       stop
       end  



