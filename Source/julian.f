c         
c ---------------------------------------------------------
c
c   time-date to julian day subroutine
c
       subroutine julian(hre,min,sec,jou,moi,ann,jday)
       real*8 hre,min,sec,jou,moi,ann,jday,nsec,nyear,dyear,mois(12)
       integer i
       data mois/31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./
          nyear=ann-1980.
          jday=0.
          do 3000 i=int(1980.),int(ann)-1
             if (int(real(i)/4.)*4.eq.i) then
                dyear=366.
                if (int((real(i))/100.)*100.eq.i) then
                   if (int((real(i))/400.)*400.ne.i) then
                      dyear=365.
                   endif
                endif
             else
                dyear=365.
             endif   
             jday=jday+dyear
 3000     continue
             if (int(real(i)/4.)*4.eq.i) then
             mois(2)=29.
                if (int((real(i))/100.)*100.eq.i) then
                   if (int((real(i))/400.)*400.ne.i) then
                   mois(2)=28.
                   endif
                endif
             else
                mois(2)=28.
             endif   
          do 3010 i=1,int(moi)-1
             jday=jday+mois(i)
 3010     continue
          jday=jday+jou-1.
          jday=jday+hre/24.+min/1440.+sec/86400. 
       return
       end