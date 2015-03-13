c
c pvoisnn.f
c Une routine pour trouver une colonne d'epaisseur optique non-nulle
c pres d'une colonne d'epaisseur optique nulle
c 
c Jean-Denis Giguere (c) 2004

      subroutine pvoisnn(numvol, nTyp, nBin, nLat, nLon, nZ, voisin)
      integer nTyp, nBin, nLat, nLon, nZ, graph, gval
      real numvol(5,12,181,360,10),totalp
      integer voisin(181,360,2), aodnn(181,360,1),iter,iterh,iterv,cval

c La variable graph vaut 1 si on desire produire des pgm
      graph=1

c Creation de la matrice indiquant si une valeur existe pour
c chaque colonne

      print*, 'Construction de la matrice'

      do 202 i=1,nLat
        do 302 j=1,nLon
          totalp=0
          do 402 k=1,nZ
            do 502 l=1,nBin
              do 602 m=1,nTyp
              totalp=totalp+numvol(m,l,i,j,k)
 602          continue
 502        continue
 402      continue
              if (totalp .gt. 1.) then
                      aodnn(i,j,1)=1
              else
                      aodnn(i,j,1)=0
              endif
 302    continue
 202  continue
              if (graph .eq. 1) then

              print*, 'Ecriture du fichier aodnn.pgm'

              open(unit=12,file='aodnn.pgm',status='unknown')
              write(12,901) 'P2'
              write(12,902) nLon, nLat
              write(12,901) '1'
              do 104 i=nLat,1,-1
                write(12,*) (aodnn(i,j,1),j=1,nLon)
 104          continue
              
              close(unit=12)

              endif

c Remplissage des trous

              do 105 i=1,nLat
                do 204 j=1,nLon

c On verifie si le point possede une valeur
                cval=aodnn(i,j,1)
                
                 if (cval .eq. 0 ) then
                         iter=0
                       
                         dowhile (iter .lt. nLat .AND. iter .lt. nLon 
     +                    .AND. cval .eq. 0)
                            iter=iter+1
                            

c On fait le cote du haut                          
                            do 109 iterh=iter,-iter,-1
                              if (i-iter .ge. 1 .AND. j+iterh .ge. 1
     +                         .AND.  cval .eq. 0 .AND. 
     +                         j+iterh .le. nLon) then
                                if (aodnn(i-iter,j+iterh,1) .eq. 1)
     +                           then
                                        voisin(i,j,1)=iterh
                                        voisin(i,j,2)=-iter
                                        cval=1
                                endif
                              endif
 109  continue
c On fait le cote gauche
                             do 209 iterv=iter,-iter,-1
                              if (i+iterv .ge. 1 .AND. j-iter .ge. 1
     +                         .AND. cval .eq. 0 .AND. i+iterv .le.
     +                         nLat) then
                                if (aodnn(i+iterv,j-iter,1) .eq. 1)
     +                           then
                                        voisin(i,j,1)=-iter
                                        voisin(i,j,2)=iterv
                                        cval=1
                                endif
                              endif
 209  continue
c On fait le cote d'en bas
                            do 309 iterh=iter,-iter,-1
                              if (j+iterh .ge. 1
     +                         .AND. cval .eq. 0 .AND. i+iter .le.
     +                         nLat .AND. j+iterh .le. nLon) then
                                if (aodnn(i+iter,j+iterh,1) .eq. 1) 
     +                           then
                                        voisin(i,j,1)=iterh
                                        voisin(i,j,2)=iter
                                        cval=1
                                 else
                                endif
                              endif
 309  continue

c On fait le cote de droite
                             do 409 iterv=iter,-iter,-1
                              if (i+iterv .ge. 1
     +                         .AND. cval .eq. 0 .AND. i+iterv .le.
     +                         nLat .AND. j+iter .le. nLon) then
                                if (aodnn(i+iterv,j+iter,1) .eq. 1)
     +                           then
                                        voisin(i,j,1)=iter
                                        voisin(i,j,2)=iterv
                                        cval=1
                                endif
                              endif
 409  continue

                         
                         enddo
                 else 
                         voisin(i,j,1)=0
                         voisin(i,j,2)=0

                 endif

 204  continue
 105  continue
                 if (graph .eq. 1) then 
              print*, 'Ecriture du fichier de decalage horizontal'

              open(unit=13,file='horiz.ppm',status='unknown')
              write(13,901) 'P3'
              write(13,902) nLon, nLat
              write(13,901) '500'
              do 114 i=1,nLat
                do 115 j=1,nLon
                gval=voisin(i,j,2)
                if (gval .lt. 0) then
                        write(13,*) -1*gval, ' 0 0   '
                else
                        write(13,*) '0 0 ', gval, '   '
                endif
 115          continue
 114          continue

              print*, 'Ecriture du fichier de decalage horizontal'

              open(unit=14,file='verti.ppm',status='unknown')
              write(14,901) 'P3'
              write(14,902) nLon, nLat
              write(14,901) '500'
              do 124 i=1,nLat
              do 125  j=1,nLon
                gval=voisin(i,j,1)
                if (gval .lt. 0) then
                        write(14,*) -1*gval, ' 0 0   '
                else
                        write(14,*) '0 0 ', gval, '   '
                endif
 125          continue
 124          continue

                close(unit=13)
                close(unit=14)

              endif
              
             
              
              
 901  format(A,F8.3)
 902  format(i6,1x,i6)

      return
      end
