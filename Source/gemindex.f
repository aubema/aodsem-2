c
c   Programme pour creer rapidement un fichier index pour les fichiers GEM
c
c
       Program gemindex
c
c   Declarations
c 
       integer nlign,n
       character*45 name(1000)
       character*2 hre
       character*60 liste1,liste2
c
c   Valeurs initiales
c
       nlign=0
c
c   inspection du fichier gemtemp
c
c       print*,'Name of the chronological list of GEM pcp files?'
c       read*,liste1
c       print*,'Name of the chronological list of GEM ana files?'
c       read*,liste2
       liste1='listpcp'
       liste2='listana'
       open(unit=1,file=liste1,status='old')
       open(unit=2,file='gempcp.index',status='unknown')
       do n=1,1000
         read(1,*,end=10) name(n)
         nlign=nlign+1
       enddo 
 10    close(unit=1)  
       write(2,*) nlign,' number of files'
       do n=1,nlign
         hre=name(n)(9:10)
         write(2,*) hre,' 00 00 ',name(n)(7:8),' ',
     +   name(n)(5:6),' ',name(n)(1:4),' ',name(n) 
       enddo  
       close(unit=2) 

       nlign=0
       open(unit=1,file=liste2,status='old')
       open(unit=2,file='gemana.index',status='unknown')
       do n=1,1000
         read(1,*,end=20) name(n)
         nlign=nlign+1
       enddo 
 20    close(unit=1)  
       write(2,*) nlign,' number of files'
       do n=1,nlign
         hre=name(n)(9:10)
         write(2,*) hre,' 00 00 ',name(n)(7:8),' ',
     +   name(n)(5:6),' ',name(n)(1:4),' ',name(n) 
       enddo  
       close(unit=2)
       stop
       end
       
