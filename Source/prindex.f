c
c   Programme pour creer rapidement un fichier index pour les vitesses de vent
c   comme ce champ est une prevision, il correspond a la precipitation cumulee entre 
c   la date du fichier et les 6h qui suivent
c
       Program prindex
c
c   Declarations
c 
       integer nlign,n
       character*45 name(1000)
       character*2 hre
c
c   Valeurs initiales
c
       nlign=0
c
c   inspection du fichier prtemp
c
       open(unit=1,file='prtemp',status='old')
       open(unit=2,file='precip.index',status='unknown')
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
       stop
       end
       
