c     programme ayant pour fonction de convertir les fichiers
c     .res.bmi en format compatible avec NARCM
c
c     convention de nommage des fichiers de sortie:
c
c     11-2222-3333.dat
c     11) SS=sea salt, SU=sulfate, OC= organic carbon, BC=black carbon
c         SD= soil dust
c     2222) CNST=ponderation sous-bin constante
c           MNT0=moment 0 en log(r)
c           MNT1=moment 1 en log(r)
c     333) longueur d'onde en nanometre
c
      integer nrbase,nrhbase,nwbase,nbbase,nr,nrh,nw,nb,nq
      integer rhbase(13),lennom
      real mrbase(5,13,14),mibase(5,13,14),sigmae(5,13,14,12)
      real wlenbase(14)
      real cabase(5,13,14,12),csbase(5,13,14,12)
      real cebase(5,13,14,12)
      character*2 typ(5),rh(13)
      character*4 quadrat(5),waven(14),wavelen
      character*32 nomtype(5),outname,nombns,nomtbase(5),
     +crofile
      character*60 nom
      data typ/'SU','SD','BC','SS','OC'/
      data nomtype/'SULFATE','SOILDUST','BLACKCARBON','SEASALT',
     +'OMCARBON'/
      data rh/'00','10','20','30','40','50','60','70','80','90','95',
     +'98','99'/
      data quadrat/'CNST','ERRO','LOGN','MNT0','MNT1'/
      data waven/'0340','0380','0440','0470','0500','0550','0670',
     +'0860','0870','0940','1020','1240','1640','2130'/
      nwav=14
      nref=5
      nbns=12
c
c    entree interactive
c
      print*,'Root name of the files (ext .dat and .vis will be added)?'  
      read*, nom
c
c   calcul de la longueur du nom
c
      lennom=index(nom,' ')-1
      crofile=nom(1:lennom)//'.res.bmi'
      nombns=nom(1:lennom)//'.bns.bmi'


c ------
c
c   lecture du fichier de sections efficaces pre calculees
c   par bmies.f
c
          open(unit=37,file=crofile,status='old',err=6)
             print*,'Reading extinction cross section database...'
             read(37,*) nrbase,nrhbase,nwbase,nbbase
             do 223 nr=1,nrbase
                read(37,*) nomtbase(nr)
                do 221 nrh=1,nrhbase
                   read(37,*) rhbase(nrh)
                   do 224 nw=1,nwbase
                      read(37,*) wlenbase(nw), mrbase(nr,nrh,nw),
     +                mibase(nr,nrh,nw) 
                      do 225 nb=1,nbbase
                         read(37,*) bidon,cebase(nr,nrh,nw,nb),
     +                   csbase(nr,nrh,nw,nb),cabase(nr,nrh,nw,nb)
 225                  continue
 224               continue
 221            continue
 223         continue 
          close(unit=37)
c
c   recherche des valeurs de sigmae dans la base de donnee
c
          do 226 nr=1,nref
             do 231 nrb=1,nrbase
             if (nomtype(nr)(1:5).eq.nomtbase(nrb)(1:5)) then
                do 232 nrh=1,13
                   do 233 nw=1,nwav
                      do 234 nb=1,nbns
                        sigmae(nr,nrh,nw,nb)=cebase(nrb,nrh,nw,nb)
 234                  continue
 233               continue
 232            continue
             endif
 231         continue
 226      continue


          print*,sigmae(1,1,1,1),cebase(1,1,1,1)

c
c  --------------
c
c   lecture du type de quadrature
c
          open(unit=13,file=nombns,status='old',err=7)
            read(13,*)
            read(13,*) nq
          close(unit=13)
          if (nq.eq.0) then
             print*,'Quadrature= constant weight'
          elseif (nq.eq.1) then
             print*,'Quadrature=  error'           
          elseif (nq.eq.2) then
             print*,'Quadrature= log normal'
          elseif (nq.eq.3) then
             print*,'Quadrature= log(r) moment 0'
          elseif (nq.eq.4) then
             print*,'Quadrature= log(r) moment 1'
          endif
c
c ------------
c
c   Ecriture des fichiers NARCM
c
         do nw=1,nwav
          do nr=1,nref
           outname=typ(nr)//'-'//quadrat(nq+1)//'-'
     +//waven(nw)//'.dat'
              
           open(unit=5,file=outname,status='unknown')
             do nrh=1,13
               write(5,11) (sigmae(nr,nrh,nw,nb),nb=1,nbbase)
             enddo
           close(unit=5)
          enddo
         enddo
         stop
 11    format(13(E10.4,1x))        
 6     print*,'Bad cross section data base file: '
     +,nom(1:lennom)//'.res.bmi'
       stop
 7     print*,'Bad size bin data base file: '
     +,nom(1:lennom)//'.bns.bmi'
       stop
         end
