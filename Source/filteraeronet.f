c
c    programme pour filtrer les epaisseurs optiques
c    mesurees au photometres solaire
c    Principe: calculer des moyennes sur une fenetre temporelle
c    glissante de une heure et ecarter les donnees superieures
c    a cette moyenne 
c
       character*60 nomfi
       integer ndat,i,newn,lennom,n,loop,his(5000),ii,limit
       integer nbin,nmin,nmax,nn,numb,nout
       real time(5000),aod(5000),daod(5000),d2aod(5000),sigma 
       real modeval,statmean,median,medianval,mini,maxi,aodoond
       real mean,newaod(5000),newtime(5000),newdaod(5000),newd2aod(5000)
       real aodout(5000),aodmoy(5000),timout(5000)
       print*,'Nom du fichier de donnee (.cxy will be add)?'
       read*, nomfi
       lennom=index(nomfi,' ')-1
       nomfi=nomfi(1:lennom)//'.cxy'
       open(unit=2,file=nomfi,status='old')   
         read(2,*) ndat
         do i=1,ndat
            read(2,*) time(i),aod(i)
         enddo
       close(unit=2)
c
c  garder minimum pour le duplicata de donnee meme date
c  
        
 
c       do loop=1,50
c       i=0
c       ii=0
c       do n=1,ndat-1
c           i=i+1
c           ii=ii+1
c           if (time(i).eq.time(i+1)) then
c              if (aod(i).le.aod(i+1)) then
c                 newaod(ii)=aod(i)
c                 newtime(ii)=time(i)
c              else
c                 newaod(ii)=aod(i+1)
c                 newtime(ii)=time(i+1)
c                 i=i+1
c                 ndat=ndat-1
c              endif
c           else
c              newaod(ii)=aod(i)
c              newtime(ii)=time(i)
c           endif
c       enddo
c         do n=1,ndat
c            aod(n)=newaod(n)
c            time(n)=newtime(n)
c         enddo
c       enddo




         do n=1,ndat
            newaod(n)=aod(n)
            newtime(n)=time(n)
         enddo






c
c      generer une moyenne sur 2 heure
c   
       do n=1,ndat
         time(n)=newtime(n)

         aod(n)=0.
         numb=0
         if (n.lt.20) then
           nmin=0
         else
           nmin=n-20
         endif
         if (n.gt.ndat-20) then
           nmax=ndat
         else
           nmax=n+20
         endif
         
         do nn=nmin,nmax
            if (abs(newtime(nn)-newtime(n)).lt.0.04) then
               numb=numb+1
               aod(n)=newaod(nn)+aod(n)
            endif
         enddo
         aod(n)=aod(n)/real(numb)
         if (numb.lt.4) aod(n)=0.
       enddo
c
c    rejetter les aod superieures a la moyenne
c    et les valeurs nulles
c
       nout=0
       do n=1,ndat
          if (newaod(n).le.aod(n)+0.005) then
             nout=nout+1
             aodout(nout)=newaod(n)
             aodmoy(nout)=aod(n)
             timout(nout)=newtime(n)
          endif
       enddo


         


c
c   ecriture du fichier de sortie
c
       open(unit=3,file=nomfi(1:lennom)//'_f.cxy',status='unknown')
          write(3,*) nout,' doy, inf_moy2h moy2h'
          do i=1,nout
            write(3,*) timout(i),aodout(i), aodmoy(i)
          enddo
       close(unit=3)
       stop
       end







