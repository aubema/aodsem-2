       programm histogram
       integer ndata,i,filty,ncellx,ncelly,maxi,autom,interv
       integer lennom,hcnt
       real x, y(1000000),ordrex,ordrey,scalex,scaley,xmin,ymin
       real ymax,xmax,xmaxlim,ymaxlim,xminlim,yminlim,ticx,ticy
       character*60 filename
       character*60 title,xlabel,ylabel
       character*15 bidon
          print*,'Root file name (extension will be added) ?'
          read*,filename
          lennom=index(filename,' ')-1
          print*,'File type (0=cxy file, 1=pgm file)?'
          read*,filty
          if (filty.eq.0) then
             filename=filename(1:lennom)//'.cxy'
          else 
             filename=filename(1:lennom)//'.pgm'
          endif
          print*,'Title (in quote)?'
          read*,title
          print*,'Variable name (in quote)?'
          read*,xlabel
          print*,'Automatic scale (1=yes,0=no)?'
          read*,autom
       call pgbegin(0,'/PS',1,1)

       open(unit=1,file=filename,status='old')
          if (filty.eq.0) then
             read(1,*) ndata
             do i=1,ndata
                read(1,*) x,y(i)
             enddo
          elseif (filty.eq.1) then
c
c   recherche de la position des headers
c
            bidon='#'
            hcnt=0
            read(1,*)
            do i=1,10
               read(1,*,end=56,err=57) bidon
 57            if (bidon.eq.'#') then
                  hcnt=hcnt+1
               endif
            enddo            
 56         rewind 1
            read(1,*)
            do 55 i=1,hcnt
               read(1,*)
 55         continue
            read(1,*) ncelly, ncellx,maxi
            ndata=ncellx*ncelly
            read(1,*) (y(i),i=1,ndata)
          endif
          ymin=1.e+20
          ymax=-1.e+20
          do i=1,ndata
             if (y(i).gt.ymax) ymax=y(i)
             if (y(i).lt.ymin) ymin=y(i)
          enddo
c
c        determiner l echelle
c      

          print*,'Minimum value=',ymin
          print*,'Maximum value=',ymax
          if (autom.eq.1) then    
             ordrey=ymax-ymin
             scaley=0. 
             if (ordrey.gt.1.) then 
                dowhile (ordrey.gt.1.)
                  scaley=scaley+1. 
                  ordrey=ordrey/10.   
                enddo  
             else
                dowhile (ordrey.lt.1.)
                  scaley=scaley+1. 
                  ordrey=ordrey*10.   
                enddo  
             endif
             ticy=10.**(scaley-3.)
             ymaxlim=(real(int(ymax/ticy)))*ticy
             yminlim=(real(int(ymin/ticy)))*ticy
             interv=int(ymaxlim)-int(yminlim)
          else
            print*,'Histogram Min, Max limits?'
            read*,yminlim,ymaxlim 
            print*,'Number of intervals?'
            read*,interv           
          endif               

       call pghist(ndata,y,yminlim,ymaxlim,interv,0)
       call pglabel(xlabel,'Number of occurence',title)
       call pgend
       end  

