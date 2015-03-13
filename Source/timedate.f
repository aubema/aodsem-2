c         
c ---------------------------------------------------------
c
c   julian day to time-date subroutine
c
       subroutine timedate(hre,min,sec,jou,moi,ann,jday)
       real*8 hre,min,sec,jou,moi,ann,jday,nsec,nyear,dyear,mois(12)
       real*8 reste,annee
       integer i,cemoi,cejour
       data mois/31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./
c       jday=jday+0.00000001
       reste=jday
       annee=1980.
       dowhile (reste.gt.0.)  
          if (int(annee/4.)*4.eq.int(annee)) then
             dyear=366.
             mois(2)=29.
             if (int(annee/100.)*100.eq.int(annee)) then
                if (int(annee/400.)*400.ne.int(annee)) then
                   dyear=365.
                   mois(2)=28.
                endif
             endif
          else
             dyear=365.
             mois(2)=28.
          endif  
          reste=reste-dyear  
          annee=annee+1.              
       enddo  
       reste=reste+dyear 
       ann=annee-1.
       if (ann.lt.1980.) then
          print*,'Invalid date (before 1980-1-1)'
          stop
       endif
c
c      mois
c   
       cemoi=1
       dowhile (reste.gt.0.)
          reste=reste-mois(cemoi)
          cemoi=cemoi+1
       enddo
       if (cemoi.gt.1) cemoi=cemoi-1
       reste=reste+mois(cemoi)
       moi=cemoi
c
c      jour
c   
       cejour=1
       dowhile (reste.gt.0.) 
          reste=reste-1.
          cejour=cejour+1
       enddo
       reste=reste+1.
       jou=cejour-1
c
c      heure
c
       hre=dble(int(reste*24.)) 
       reste=reste-hre/24. 
       min=dble(int(reste*60.*24.))   
       reste=reste-min/60./24.
       sec=reste*60.*60.*24.
       if (nint(sec).eq.60) then
          min=min+1.
          sec=0.
       endif
       if (nint(min).eq.60) then
          hre=hre+1.
          min=0.
       endif
       if (nint(hre).eq.24) then
          hre=0.
          jou=jou+1.
       endif  
       if (nint(jou).gt.nint(mois(cemoi))) then
          cemoi=cemoi+1
          moi=moi+1.
          jou=1.
       endif
       if (nint(moi).gt.12) then
          moi=1.
          ann=ann+1.
       endif   
       return
       end         