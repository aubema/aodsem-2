c
c   Programme pour creer rapidement un fichier index pour les vitesses de vent
c
c
       Program wspindex
c
c   Declarations
c 
       integer nlign,n
       character*45 name(1000)
c
c   Valeurs initiales
c
       nlign=0
c
c   inspection du fichier wsptemp
c
       open(unit=1,file='wsptemp',status='old')
       open(unit=2,file='wspeed.index',status='unknown')
       do n=1,1000
         read(1,*,end=10) name(n)
         nlign=nlign+1
       enddo 
 10    close(unit=1)  
       write(2,*) nlign,' number of files'
       do n=1,nlign
         write(2,*) name(n)(9:10),' 00 00 ',name(n)(7:8),' ',
     +   name(n)(5:6),' ',name(n)(1:4),' ',name(n) 
       enddo  
       close(unit=2) 
       stop
       end
       
