c    programme pour un masque aleatoire sur une image
c    de l'epaisseur optique en format .pgm
c
c    L entree du programme est une carte pgm d epaisseur optique
c    qui peut etre incomplete.
c      
c    
c    les axes sont definis comme suit
c    x = direction nord = degres
c    y = direction est = degres
c
c
c    copyright Martin Aube 16/09/2002
c
c -----------------
c   identification des variables 
c
c
c --------------------
c
c   programme principal
c
      program randommask
c
c ----------
c
c   declaration des variables
c
      real pi2,pi,xcell0,ycell0,lcellx,lcelly,dmin,maxim,minim,npix
      real pixsiz,lattau0,lontau0,tau(1000,1000),tauout(1000,1000)
      real latitu,longit,distan,dtotal,factor,latcen,loncen,ndat
      real Vx,Vy,tau1,tau2,tau3,tau4,distan1,distan2,distan3,distan4
      real defval,msk(1000,1000),value,ranval
      integer maxside,step,n,he,mi,se,jo,mo,an
      character*60 bidon,nom,aotfile,taufile,header,tag
      integer ncellx,ncelly,i,j,intype,autom,n1,n2,n3,n4
      integer percent,pcentold,nxcen,nycen,lennom
      integer nx,ny,nxtcel,nytcel,side,maxi,ngris,hcnt,theta

      DIMENSION RVEC1(1000000),RVEC2(10000000)


c   
c ----------
c
c   initialisation des variables
c
       pi=3.141592654
       pi2=pi*pi
       he=0
       mi=0
       se=0
       jo=1
       mo=1
       an=1980 
c
c -----------
c
c   fichier de parametres
c
      open(unit=19,file="randommask.par",status="old",err=12)
c
c   choix du nom de la racine de fichiers
c
c      print*,'root name of the files (.pgm will be add) ?'  
c      read*,nom
       read(19,*) nom
c
c  randrat = pourcentage de l'image a conserver
c
       read(19,*) randrat   
      close(unit=19)  
 
c
c   calcul de la longueur du nom
c
      lennom=index(nom,' ')-1
c
c
c -----------
c
c   Lecture du fichier d'epaisseur optique
c
c   fichier pgm pour les images polder 254 et 255=aucun signification
c   253=lignes cotieres, autre=aod*100
c
         taufile=nom(1:lennom)//'.pgm'
         open(unit=2,file=taufile,status='old')
c
c   recherche de la position des headers
c
         bidon='#'
         hcnt=0
         read(2,*)
         do 54 i=1,50
            read(2,*,end=56,err=57) bidon, tag, value
 57         if (bidon.eq.'#') then
               hcnt=hcnt+1
               if (tag(1:6).eq.'pixsiz') lcellx=value
               if (tag(1:4).eq.'lat0') xcell0=value
               if (tag(1:4).eq.'lon0') ycell0=value 
               if (tag(1:4).eq.'date') then
                 backspace 2
                 read(2,*) bidon,tag,he,mi,se,jo,mo,an
               endif
               lcelly=lcellx
             endif
 54      continue            
 56      rewind 2
         read(2,*)
         do 55 i=1,hcnt
            read(2,*)
 55      continue
         read(2,*) nytcel, nxtcel, maxi 
         print*,'Reading AOD data...'
         read(2,*) ((tau(nx,ny),ny=1,nytcel),nx=nxtcel,1,-1)
         close(unit=2)


c
c ==========
c
c   create random mask
c
c
      do nx=1,nxtcel
      do ny=1,nytcel   
         msk(nx,ny)=0.
      enddo
      enddo 
      nx=1
      CALL RANLUX(RVEC1,nxtcel*nytcel)
      CALL RANLUX(RVEC2,nxtcel*nytcel)
      print*,int(real(nxtcel)*sqrt(real(randrat)/100.)),
     +int(real(nytcel)*sqrt(real(randrat)/100.))
      DO i=1,int(real(nxtcel)*sqrt(real(randrat)/100.))
        DO j=1,int(real(nytcel)*sqrt(real(randrat)/100.))



         if (msk(NINT(RVEC1(nx)*(nxtcel-1))+1,NINT(RVEC2(nx)*(nytcel
     +-1)+1)).eq.1.) then


           nx=nx+1
         endif


         msk(NINT(RVEC1(nx)*(nxtcel-1))+1,NINT(RVEC2(nx)*(nytcel
     +-1))+1)=1.

        nx=nx+1
        ENDDO
      ENDDO

c
c ===============
c
c    multiplier le masque avec AOD
c
        do nx=1,nxtcel
          do ny=1,nytcel
             if (tau(nx,ny).lt.253.) then
                 tauout(nx,ny)=msk(nx,ny)*tau(nx,ny)
                if (tauout(nx,ny).le.0.) then
                  tauout(nx,ny)=253.
                endif
             else
                tauout(nx,ny)=254. 

             endif
          enddo
        enddo        
c
c ----------
c
c   fabrication d'un nouveau fichier pgm
c
      print*,'making optical depth pgm image...'
         aotfile=nom(1:lennom)//'_g.pgm'
         open(unit=27,file=aotfile,status="unknown")
         write(27,178) 'P2'
         write(27,178) '# image d epaisseurs optiques interpolee par int
     +erp.f, (100=aod 1)'
         write(27,180) '# date ',he,mi,se,jo,mo,an
         write(27,179) '# pixsiz ',lcellx
         write(27,179) '# lat0   ',xcell0
         write(27,179) '# lon0   ',ycell0
         write(27,*) nytcel, nxtcel
         write(27,*) '255'
         do 3411 i=nxtcel,1,-1
               write(27,*) (nint(tauout(i,j)),j=1,nytcel)
 3411    continue
 177     format(i3)
 178     format(A)
 179     format(A,F8.3)
 180  format(A,I2,1x,I2,1x,I2,1x,I2,1x,I2,1x,I4)
         close(unit=27)
       stop
 12    print*,'Edit parameter file : randommask.par by typing 
     +epar_randm!'
       stop    
       end


      SUBROUTINE RANLUX(RVEC,LENV)
C         Subtract-and-borrow random number generator proposed by
C         Marsaglia and Zaman, implemented by F. James with the name
C         RCARRY in 1991, and later improved by Martin Luescher
C         in 1993 to produce "Luxury Pseudorandom Numbers".
C     Fortran 77 coded by F. James, 1993
C          
C       references: 
C  M. Luscher, Computer Physics Communications  79 (1994) 100
C  F. James, Computer Physics Communications 79 (1994) 111
C
C   LUXURY LEVELS.
C   ------ ------      The available luxury levels are:
C
C  level 0  (p=24): equivalent to the original RCARRY of Marsaglia
C           and Zaman, very long period, but fails many tests.
C  level 1  (p=48): considerable improvement in quality over level 0,
C           now passes the gap test, but still fails spectral test.
C  level 2  (p=97): passes all known tests, but theoretically still
C           defective.
C  level 3  (p=223): DEFAULT VALUE.  Any theoretically possible
C           correlations have very small chance of being observed.
C  level 4  (p=389): highest possible luxury, all 24 bits chaotic.
C
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C!!!  Calling sequences for RANLUX:                                  ++
C!!!      CALL RANLUX (RVEC, LEN)   returns a vector RVEC of LEN     ++
C!!!                   32-bit random floating point numbers between  ++
C!!!                   zero (not included) and one (also not incl.). ++
C!!!      CALL RLUXGO(LUX,INT,K1,K2) initializes the generator from  ++
C!!!               one 32-bit integer INT and sets Luxury Level LUX  ++
C!!!               which is integer between zero and MAXLEV, or if   ++
C!!!               LUX .GT. 24, it sets p=LUX directly.  K1 and K2   ++
C!!!               should be set to zero unless restarting at a break++ 
C!!!               point given by output of RLUXAT (see RLUXAT).     ++
C!!!      CALL RLUXAT(LUX,INT,K1,K2) gets the values of four integers++
C!!!               which can be used to restart the RANLUX generator ++
C!!!               at the current point by calling RLUXGO.  K1 and K2++
C!!!               specify how many numbers were generated since the ++
C!!!               initialization with LUX and INT.  The restarting  ++
C!!!               skips over  K1+K2*E9   numbers, so it can be long.++
C!!!   A more efficient but less convenient way of restarting is by: ++
C!!!      CALL RLUXIN(ISVEC)    restarts the generator from vector   ++
C!!!                   ISVEC of 25 32-bit integers (see RLUXUT)      ++
C!!!      CALL RLUXUT(ISVEC)    outputs the current values of the 25 ++
C!!!                 32-bit integer seeds, to be used for restarting ++
C!!!      ISVEC must be dimensioned 25 in the calling program        ++
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION RVEC(LENV)
      DIMENSION SEEDS(24), ISEEDS(24), ISDEXT(25)
      PARAMETER (MAXLEV=4, LXDFLT=3)
      DIMENSION NDSKIP(0:MAXLEV)
      DIMENSION NEXT(24)
      PARAMETER (TWOP12=4096., IGIGA=1000000000,JSDFLT=314159265)
      PARAMETER (ITWO24=2**24, ICONS=2147483563)
      SAVE NOTYET, I24, J24, CARRY, SEEDS, TWOM24, TWOM12, LUXLEV
      SAVE NSKIP, NDSKIP, IN24, NEXT, KOUNT, MKOUNT, INSEED
      INTEGER LUXLEV
      LOGICAL NOTYET
      DATA NOTYET, LUXLEV, IN24, KOUNT, MKOUNT /.TRUE., LXDFLT, 0,0,0/
      DATA I24,J24,CARRY/24,10,0./
C                               default
C  Luxury Level   0     1     2   *3*    4
      DATA NDSKIP/0,   24,   73,  199,  365 /
Corresponds to p=24    48    97   223   389
C     time factor 1     2     3     6    10   on slow workstation
C                 1    1.5    2     3     5   on fast mainframe
C
C  NOTYET is .TRUE. if no initialization has been performed yet.
C              Default Initialization by Multiplicative Congruential
      IF (NOTYET) THEN
         NOTYET = .FALSE.
         JSEED = JSDFLT  
         INSEED = JSEED
         WRITE(6,'(A,I12)') ' RANLUX DEFAULT INITIALIZATION: ',JSEED
         LUXLEV = LXDFLT
         NSKIP = NDSKIP(LUXLEV)
         LP = NSKIP + 24
         IN24 = 0
         KOUNT = 0
         MKOUNT = 0
         WRITE(6,'(A,I2,A,I4)')  ' RANLUX DEFAULT LUXURY LEVEL =  ',
     +        LUXLEV,'      p =',LP
            TWOM24 = 1.
         DO 25 I= 1, 24
            TWOM24 = TWOM24 * 0.5
         K = JSEED/53668
         JSEED = 40014*(JSEED-K*53668) -K*12211
         IF (JSEED .LT. 0)  JSEED = JSEED+ICONS
         ISEEDS(I) = MOD(JSEED,ITWO24)
   25    CONTINUE
         TWOM12 = TWOM24 * 4096.
         DO 50 I= 1,24
         SEEDS(I) = REAL(ISEEDS(I))*TWOM24
         NEXT(I) = I-1
   50    CONTINUE
         NEXT(1) = 24
         I24 = 24
         J24 = 10
         CARRY = 0.
         IF (SEEDS(24) .EQ. 0.) CARRY = TWOM24
      ENDIF
C
C          The Generator proper: "Subtract-with-borrow",
C          as proposed by Marsaglia and Zaman,
C          Florida State University, March, 1989
C
      DO 100 IVEC= 1, LENV
      UNI = SEEDS(J24) - SEEDS(I24) - CARRY 
      IF (UNI .LT. 0.)  THEN
         UNI = UNI + 1.0
         CARRY = TWOM24
      ELSE
         CARRY = 0.
      ENDIF
      SEEDS(I24) = UNI
      I24 = NEXT(I24)
      J24 = NEXT(J24)
      RVEC(IVEC) = UNI
C  small numbers (with less than 12 "significant" bits) are "padded".
      IF (UNI .LT. TWOM12)  THEN
         RVEC(IVEC) = RVEC(IVEC) + TWOM24*SEEDS(J24)
C        and zero is forbidden in case someone takes a logarithm
         IF (RVEC(IVEC) .EQ. 0.)  RVEC(IVEC) = TWOM24*TWOM24
      ENDIF
C        Skipping to luxury.  As proposed by Martin Luscher.
      IN24 = IN24 + 1
      IF (IN24 .EQ. 24)  THEN
         IN24 = 0
         KOUNT = KOUNT + NSKIP
         DO 90 ISK= 1, NSKIP
         UNI = SEEDS(J24) - SEEDS(I24) - CARRY
         IF (UNI .LT. 0.)  THEN
            UNI = UNI + 1.0
            CARRY = TWOM24
         ELSE
            CARRY = 0.
         ENDIF
         SEEDS(I24) = UNI
         I24 = NEXT(I24)
         J24 = NEXT(J24)
   90    CONTINUE
      ENDIF
  100 CONTINUE
      KOUNT = KOUNT + LENV
      IF (KOUNT .GE. IGIGA)  THEN
         MKOUNT = MKOUNT + 1
         KOUNT = KOUNT - IGIGA
      ENDIF
      RETURN
C
C           Entry to input and float integer seeds from previous run
      ENTRY RLUXIN(ISDEXT)
*     IF block added by Phillip Helbig after correpondence with James
      IF (NOTYET) THEN
         WRITE(6,'(A)')  ' PROPER RESULTS ONLY WITH INITIALISATION FROM 
     $25 INTEGERS OBTAINED WITH RLUXUT'
         NOTYET = .FALSE.
      ENDIF
         TWOM24 = 1.
         DO 195 I= 1, 24
         NEXT(I) = I-1
  195    TWOM24 = TWOM24 * 0.5
         NEXT(1) = 24
         TWOM12 = TWOM24 * 4096.
      WRITE(6,'(A)') ' FULL INITIALIZATION OF RANLUX WITH 25 INTEGERS:'
      WRITE(6,'(5X,5I12)') ISDEXT
      DO 200 I= 1, 24
      SEEDS(I) = REAL(ISDEXT(I))*TWOM24
  200 CONTINUE
      CARRY = 0.
      IF (ISDEXT(25) .LT. 0)  CARRY = TWOM24
      ISD = IABS(ISDEXT(25))
      I24 = MOD(ISD,100)
      ISD = ISD/100
      J24 = MOD(ISD,100)
      ISD = ISD/100
      IN24 = MOD(ISD,100)
      ISD = ISD/100
      LUXLEV = ISD
        IF (LUXLEV .LE. MAXLEV) THEN
          NSKIP = NDSKIP(LUXLEV)
          WRITE (6,'(A,I2)') ' RANLUX LUXURY LEVEL SET BY RLUXIN TO: ',
     +                         LUXLEV
        ELSE  IF (LUXLEV .GE. 24) THEN
          NSKIP = LUXLEV - 24
          WRITE (6,'(A,I5)') ' RANLUX P-VALUE SET BY RLUXIN TO:',LUXLEV
        ELSE
          NSKIP = NDSKIP(MAXLEV)
          WRITE (6,'(A,I5)') ' RANLUX ILLEGAL LUXURY RLUXIN: ',LUXLEV
          LUXLEV = MAXLEV
        ENDIF
      INSEED = -1
      RETURN
C
C                    Entry to ouput seeds as integers
      ENTRY RLUXUT(ISDEXT)
      DO 300 I= 1, 24
         ISDEXT(I) = INT(SEEDS(I)*TWOP12*TWOP12)
  300 CONTINUE
      ISDEXT(25) = I24 + 100*J24 + 10000*IN24 + 1000000*LUXLEV
      IF (CARRY .GT. 0.)  ISDEXT(25) = -ISDEXT(25)
      RETURN
C
C                    Entry to output the "convenient" restart point
      ENTRY RLUXAT(LOUT,INOUT,K1,K2)
      LOUT = LUXLEV
      INOUT = INSEED
      K1 = KOUNT
      K2 = MKOUNT
      RETURN
C
C                    Entry to initialize from one or three integers
      ENTRY RLUXGO(LUX,INS,K1,K2)
         IF (LUX .LT. 0) THEN
            LUXLEV = LXDFLT
         ELSE IF (LUX .LE. MAXLEV) THEN
            LUXLEV = LUX
         ELSE IF (LUX .LT. 24 .OR. LUX .GT. 2000) THEN
            LUXLEV = MAXLEV
            WRITE (6,'(A,I7)') ' RANLUX ILLEGAL LUXURY RLUXGO: ',LUX
         ELSE
            LUXLEV = LUX
            DO 310 ILX= 0, MAXLEV
              IF (LUX .EQ. NDSKIP(ILX)+24)  LUXLEV = ILX
  310       CONTINUE
         ENDIF
      IF (LUXLEV .LE. MAXLEV)  THEN
         NSKIP = NDSKIP(LUXLEV)
         WRITE(6,'(A,I2,A,I4)') ' RANLUX LUXURY LEVEL SET BY RLUXGO :',
     +        LUXLEV,'     P=', NSKIP+24
      ELSE
          NSKIP = LUXLEV - 24
          WRITE (6,'(A,I5)') ' RANLUX P-VALUE SET BY RLUXGO TO:',LUXLEV
      ENDIF
      IN24 = 0
      IF (INS .LT. 0)  WRITE (6,'(A)')   
     +   ' Illegal initialization by RLUXGO, negative input seed'
      IF (INS .GT. 0)  THEN
        JSEED = INS
        WRITE(6,'(A,3I12)') ' RANLUX INITIALIZED BY RLUXGO FROM SEEDS',
     +      JSEED, K1,K2
      ELSE
        JSEED = JSDFLT
        WRITE(6,'(A)')' RANLUX INITIALIZED BY RLUXGO FROM DEFAULT SEED'
      ENDIF
      INSEED = JSEED
      NOTYET = .FALSE.
      TWOM24 = 1.
         DO 325 I= 1, 24
           TWOM24 = TWOM24 * 0.5
         K = JSEED/53668
         JSEED = 40014*(JSEED-K*53668) -K*12211
         IF (JSEED .LT. 0)  JSEED = JSEED+ICONS
         ISEEDS(I) = MOD(JSEED,ITWO24)
  325    CONTINUE
      TWOM12 = TWOM24 * 4096.
         DO 350 I= 1,24
         SEEDS(I) = REAL(ISEEDS(I))*TWOM24
         NEXT(I) = I-1
  350    CONTINUE
      NEXT(1) = 24
      I24 = 24
      J24 = 10
      CARRY = 0.
      IF (SEEDS(24) .EQ. 0.) CARRY = TWOM24
C        If restarting at a break point, skip K1 + IGIGA*K2
C        Note that this is the number of numbers delivered to
C        the user PLUS the number skipped (if luxury .GT. 0).
      KOUNT = K1
      MKOUNT = K2
      IF (K1+K2 .NE. 0)  THEN
        DO 500 IOUTER= 1, K2+1
          INNER = IGIGA
          IF (IOUTER .EQ. K2+1)  INNER = K1
          DO 450 ISK= 1, INNER
            UNI = SEEDS(J24) - SEEDS(I24) - CARRY 
            IF (UNI .LT. 0.)  THEN
               UNI = UNI + 1.0
               CARRY = TWOM24
            ELSE
               CARRY = 0.
            ENDIF
            SEEDS(I24) = UNI
            I24 = NEXT(I24)
            J24 = NEXT(J24)
  450     CONTINUE
  500   CONTINUE
C         Get the right value of IN24 by direct calculation
        IN24 = MOD(KOUNT, NSKIP+24)
        IF (MKOUNT .GT. 0)  THEN
           IZIP = MOD(IGIGA, NSKIP+24)
           IZIP2 = MKOUNT*IZIP + IN24
           IN24 = MOD(IZIP2, NSKIP+24)
        ENDIF
C       Now IN24 had better be between zero and 23 inclusive
        IF (IN24 .GT. 23) THEN
           WRITE (6,'(A/A,3I11,A,I5)')  
     +    '  Error in RESTARTING with RLUXGO:','  The values', INS,
     +     K1, K2, ' cannot occur at luxury level', LUXLEV
           IN24 = 0
        ENDIF
      ENDIF
      RETURN
      END



