c
c   programme pour convertir les doy en date
c
      integer mode,again
      real*8 heure,minute,seconde,jour,mois,annee,doy

      real*8 jdaynow,jday0,jday,zero,un
      zero=0.
      un=1.
  1   print*,'Select your need:'
      print*,'    0 ............ Day of year to date'
      print*,'    1 ............ Date to day of year'
      read*,mode
      if (mode.eq.1) then
        print*,'Enter time and date (HH MM SS DD MM YYYY):'
        read*, heure,minute,seconde,jour,mois,annee
        call julian(heure,minute,seconde,jour,mois,annee,jday)
        jdaynow=jday
        call julian(zero,zero,zero,un,un,annee,jday)
        jday0=jday
        doy=jdaynow-jday0+1.

        write(*,212) idnint(heure),idnint(minute),idnint(seconde)
     +,idnint(jour),idnint(mois),idnint(annee)
        write(*,213) doy
 212    format('DOY for Date ',I2,'h. ',I2,'m. ',I2,'s. ',I2
     +,'/',I2,'/',I4)
 213    format(F9.5)        
      else
        print*,'Enter day of year and year:'
        read*,doy,annee
        call julian(zero,zero,zero,un,un,annee,jday)
        jdaynow=jday+doy-1.
        call timedate(heure,minute,seconde,jour,mois,annee,jdaynow)
        write(*,112) doy,idnint(annee)
        write(*,113) idnint(heure),idnint(minute),idnint(seconde),
     +idnint(jour),idnint(mois),idnint(annee)
 112    format('Date for DOY ',F9.5,' Year ',I4)
 113    format(I2,'h. ',I2,'m. ',I2,'s. ',I2,'/',I2,'/',I4)
      endif
      print*,'Run again? (0=no, 1=yes)?'
      read*,again
      if (again.eq.1) goto 1
      stop
      end


