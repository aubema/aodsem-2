       programm plotfile
       integer ndata,i,autom,option,lennom,nmark,nm
       real x(100000), y(100000),ordrex,ordrey,scalex,scaley,xmin,ymin
       real ymax,xmax,xmaxlim,ymaxlim,xminlim,yminlim,ticx,ticy
       character*60 filename
       character*60 title,xlabel,ylabel
          print*,'Root file name (.cxy will be added) ?'
          read*,filename
          lennom=index(filename,' ')-1
          filename=filename(1:lennom)//'.cxy'
          print*,'Title (in quote)?'
          read*,title
          print*,'X label (in quote)?'
          read*,xlabel
          print*,'Y label (in quote)?'
          read*,ylabel
          print*,'Automatic scale (1=yes,0=no)?'
          read*,autom

       call pgbegin(0,'/PS',1,1)

       open(unit=1,file=filename,status='old')
          read(1,*) ndata
          
          xmin=1.e+20
          ymin=1.e+20
          xmax=-1.e+20
          ymax=-1.e+20
          do i=1,ndata
             read(1,*) x(i),y(i)
             if (x(i).gt.xmax) xmax=x(i)
             if (x(i).lt.xmin) xmin=x(i)
             if (y(i).gt.ymax) ymax=y(i)
             if (y(i).lt.ymin) ymin=y(i)
          enddo
c
c        determiner l echelle
c    
          print*,'X minimum value=',xmin
          print*,'X maximum value=',xmax
          print*,'Y minimum value=',ymin
          print*,'Y maximum value=',ymax
          if (autom.eq.1) then   
            ordrex=xmax-xmin
            scalex=0. 
            if (ordrex.gt.1.) then 
               dowhile (ordrex.gt.1.)
                 scalex=scalex+1. 
                 ordrex=ordrex/10.   
               enddo  
            else
               dowhile (ordrex.lt.1.)
                 scalex=scalex+1. 
                 ordrex=ordrex*10.   
               enddo  
            endif             
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
            ticx=10.**(scalex-3.)
            ticy=10.**(scaley-3.)
            xmaxlim=(real(int(xmax/ticx))+1.)*ticx
            ymaxlim=(real(int(ymax/ticy))+1.)*ticy
            xminlim=(real(int(xmin/ticx))-1.)*ticx
            yminlim=(real(int(ymin/ticy))-1.)*ticy
          else
            print*,'X axis Min, Max limits?'
            read*,xminlim,xmaxlim
            print*,'Y axis Min, Max limits?'
            read*,yminlim,ymaxlim            
          endif
          call pgenv(xminlim,xmaxlim,yminlim,ymaxlim,0,1)
          call pglabel(xlabel,ylabel,title)
          print*,'Printing options (0=points, 1=lines, 2=both)'
          read*,option
          if (option.ne.1) then
             print*,'Choose a symbol:'
             print*,'1 ........  square'
             print*,'2 ........  circle'
             print*,'3 ........  triangle'
             print*,'4 ........  x'
             print*,'5 ........  +'
             print*,'6 ........  *'
             print*,'7 ........  .'
             read*,nmark
             if (nmark.eq.1) nm=0
             if (nmark.eq.2) nm=4
             if (nmark.eq.3) nm=7
             if (nmark.eq.4) nm=5 
             if (nmark.eq.5) nm=2
             if (nmark.eq.6) nm=3
             if (nmark.eq.7) nm=1
          endif     
          if (option.eq.0) then
              call pgpoint(ndata,x,y,nm)
          elseif (option.eq.1) then
              call pgline(ndata,x,y)
          else
              call pgpoint(ndata,x,y,nm)
              call pgline(ndata,x,y)
          endif
       
          call pgend
       end  



