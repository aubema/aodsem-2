C PHASE FUNCTION AND EFFICIENCY FACTORS FOR MOST PARTICLE SHAPES AND
C PARTICLE SIZE DISTRIBUTIONS
C BY BLAIR EVANS/DREV
C JANUARY 1988
C	
C	** mod **
C	Mods (Norm O'Neill, 06/07/93):
C	1. replace RANDOMF & RANDOMC % RANDOMCC BY RANDF & RANDC % RANDCC
C	since this Fortran sees only 6 characters.
C	2. replace subroutine name MIE by MIEQ to avoid duplication with
C	program name
C	3. Replace COATCYL & COATCYLPHASE by COCYL & COCYLPHASE
C	since this Fortran sees only 6 characters (it will see COCYL
C	and COCYLP).
C	4. replace C:\IPHASE\IPH.DAT by IPH.DAT
C	5. replace READ(5 & WRITE(6 by READ(* and WRITE(*
C	6. add initialization of all mainline variables (known that vector
C	PSD was a problem) to avoid intialization problems
C	7. In MIEPHASE replace ETA1(NL2)=DCMPLX(0.D0,0.D0) by
C	ETA1(NL2 + 1)=DCMPLX(0.D0,0.D0) and ETA2(NL3) = 0.
C	BY ETA2(NL3 + 1) = 0. for reasons which are obvious in the DO loops
C	which follow these initializations (this omission before being
C	fixed caused all kinds of weird problems).
C       8. In SPHEREPHASE added clearer WRITE statements
C       9. In SPHEREPHASE add proper optical weighting (QSCA) to PSD integration of
C       phase function. Also added a WRITE to indicate where processing was
C       in terms of size parameter integration (26 April, 1998). A factor of 4*pi
C	had to be added to the phase function expression to yield acceptable
C	validation results (see mie.tst).
C	10. The modified log-normal PSD expression was incorrectly expressed. The
C	x (RR) argument of this expression was replaced by x*lamda/2*pi and the
C	the user is now asked for a value of lamda (June 7, 1998)
C
C	Below are some reasonable musings which seem to jive with the results
C
CLOG
C	For a log-normal PSD a logical definition is (L = lambda, x 
C	= 2*pi*r/L, s = sigma):
C	<Q> = cross section / pi*r2**2 (r2 = rN exp[ln^2 s]; see MEO report)
C	    = int{Q*pi*{L*x/(2*pi)}**2*(1/N)*dN/dx}dx / pi*r2**2
C	    = int{Q*pi*{L*x/(2*pi)}**2*(1/N)*dN/dr*L/(2pi)}dx / pi*r2**2
C	but:
C	dN/dr 	= log(e)/r*dN/dlog(r)
C	    	= log(e)/r/sqrt(2*pi)/log(s)*
C			exp(-(log(r) - log(rN))**2/(2*log(s)**2)
C		= 1/r/sqrt(2*pi)/ln(s)*
C			exp(-(ln(r) - ln(rN))**2/(2*ln(s)**2)
C		= [(2pi)/L]/*1/x/sqrt(2*pi)/ln(s)*
C			exp(-(ln(x) - ln(xN))**2/(2*ln(s)**2) where xN = 2*pi*rN/L
C	Let "PSD" = dN/dr*L/(2pi) = 1/x/sqrt(2*pi)/ln(s)*
C			exp(-(log(x) - ln(xN))**2/(2*ln(s)**2) as per the
C	defining Fortran statement below:
C
C 612 PSD(I)=PSD(I)+(1.D0/SG/XI/R2PI)*EXP(-((LOG(XI)-LOG(XM))**2/
C     $2/SG**2))*RIMP
C	Furthermore "RIMP" is asked for whenever there is a question of
C	multi-modal distributions "RELATIVE IMPORTANCE FACTOR" .... therefore 
C	it must be a number density weight for each mode.
C	
C
C	A little algebra then yields:
C	<Q> = 1/x2**2/sqrt(2*pi)/ln(s)*
C		int{Q*x*exp(-(log(x) - ln(xN))**2/(2*ln(s)**2)}dx
C	a result which is only dependent on size parameter (x2 = 2*pi*r2/L)
C
CLOG
C
C	If XXX = EXT, SCA or ABS then one can infer from the code that:
C	SUMN = int{dN/dx}dx = N (total number of all particles) 
C	SUMNS = int{dN/dx*x^2}dx, 
C	so that, area of all part. = A = int{dN/dr*pi*r**2}dr
C		              = [lambda^2/(4*pi)] * SUMNS
C	QXXX1 = int{Q*x^2*dN/dx}dx / (4*pi*N)
C	QXXX2 = int{Q*x^2*dN/dx}dx / SUMNS = 4*pi*N*QXXX1 / SUMNS
C
C	then the efficiency factor of <Q>XXX must = QXXX2
C	and the cross sect. (XXX) = <Q>XXX*(A/N) (<Q> * avg. area of 1 part.) 
C	= QXXX2*( [lambda^2/(4*pi)] * SUMNS / N )
C	= (4*pi*N*QXXX1 / SUMNS) ( [lambda^2/(4*pi)] * SUMNS / N )
C	= QXXX1*lambda^2 with the units of lambda^2
C
C	Summary:
C	<Q>XXX = QXXX2
C	cross section (XXX) = QXXX1*lambda^2
C	QXXX2 = 4*pi*SUMN*QXXX1 / SUMNS
C
C	Phase function considerations
C
C	Below, in SPHEREPHASE, it is noted that the integrated phase function is: 
C       (4*pi) * int { RI * QSCA * PSD * x**2 } dx / int { QSCA * PSD * x**2 } dx
C	but clearly PSD(i) must = f( [x(i)*L / 2*pi] ) for uniformly incrementing 
C	values of x(i) (i.e. the quadrature expressions over size distribution).
C	This presents no problem for the log-normal case as long as xN is
C	properly defined (as shown above), but is less simple for the modified gamma.
C
CMGD	modified gamma distribution:
C	From CMGD section below (near statement 751): PSD = x**DA*EXP(-DB*x**DG)/RN
C	But the proper expression is PSD = r**DA*EXP(-DB*r**DG)/RN
C	= (x*L/2*pi)**DA*EXP(-DB*(x*L/2*pi)**DG)/RN
C	which clearly cannot be separated out into a multiplicative factor times the
C	f(x) expression of statement 751
C	Therefore the original PSD expression cannot be correct for any lambda
C	and x (RR) must be replaced by x*L/2*pi.
C	
CMGD
C	** mod **
C
C	Tests: see mie.tst in Documentation (sub folder to this folder)
C
C     References:
C
C     1. Deirmendjian, D., (1969), Electromagnetic Scattering on Spherical Poly-
C     Dispersions, American Elsevier, New York.
C
C -----------------
C     Identification des variables (Martin Aube 1998)
C
C        ITYPE = type de particule
C        PSD(*) = distribution de taille pour chaque dX
C        M = indice de refraction complexe
C        REM = partie reelle de l incide de refraction
C        IMM = partie imaginaire de l indice de refraction
C        MABS = Valeur absolue = (RE^2+IM^2)^.5
C        IPSD = type de distribution de taille
C        RINC = increment de la distribution de taille (dX) X=size param
C               N.B. le size param = 2 pi r / lambda
C        ISIZE = nombre d'intervalles dX de la distribution de taille
C                par defaut  5001
C        RLOW = Valeur minimale du size parameter
C        RUP = Valeur maximale du size parameter
C        IUP = entier representant le bin maximal 
C        ILOW = entier representant le bin minimal
C        RINCINV = 1/dX
C        XM = taille moyenne de la distribution log-normale
C        SG = sigma de la distribution log-normale
C        WAVLEN = longueur d onde en  um
C        ANGLEL = angle minimal pour la fonction de phase
C        ANGLEU = angle maximal pour la fonction de phase
C        DELTA = increment de l'angle pour la fonction de phase
C        ANS = reponse a une question
C        NANG = nombre d'angles pour la fonction de phase
C        
C
C        
C
C -----------------
C       
C     Subroutines Called:
C	
C     SPHEREPHASE	PSD quadrature of optical parameters for spheres
C     MIEPHASE		phase function and optical parameters for homogeneous
C			sphere (i.e. the classical Mie case)
C     RANDCC		Phase function for randomn orientation of coated
C     			perfectly reflecting infinitie cylinders
C     COAT		Mie calculations (including phase function) for coated
C			spheres
C     PROGRAM (FROM RUCK 1970 WITH CORRECTIONS)
      REAL*4 RI(0:18000),SUMISS(0:18000),SUMIDL(0:18000)
      REAL*8 QSCA1(5002),QABS(5002),QEXT2,QSCA2,X1,QALPHAD
      REAL*8 PSD(5002),REM,IMM,REM1,IMM1,MABS,RC,RIC,RT,RIT,RN,RIN
      REAL*8 DUMMYR8(10500),DX,DY
      COMPLEX*8 M1,Z
      COMPLEX*8 DUMC8(3003)
      COMPLEX*16 M,CM
      COMPLEX*16 DUMC16(17500)
      COMMON /PHASE/ RI
      COMMON /DATA/ ANGLEL,ANGLEU,DELTA,NANG,ILOW,IUP,RINC,ANGLEU1
      COMMON /INIT/ Z,EP,EDP,PHI,Q
      COMMON /POLAR/ IPARA,IPERP
      COMMON /GLOBALR4/ SUMISS
      COMMON /GLOBALR8/ PSD,QSCA1,QABS,DUMMYR8
      COMMON /GLOBALC8/ DUMC8
      COMMON /GLOBALC16/ DUMC16
      OPEN (UNIT=7,FILE='IPH.DAT',STATUS='UNKNOWN')
      WRITE(*,*) '***************************************************'
      WRITE(*,*) '*  Copyright  Her Majesty the Queen in Right      *'
      WRITE(*,*) '*             of Canada as Represented by the     *'
      WRITE(*,*) '*             Minister of National Defence, 1990. *'
      WRITE(*,*) '***************************************************'
      WRITE(*,*) '    '
C
C     ** mod ** initialization mod. because original code was only tested
C     on compilers with automatic zero initialization (for PSD this is 
C     a real problem because a zero value is assumed in it's definition
C     below.
C
C     ********* initialization **********
      DO I = 1,5002
      		PSD(I) = 0.
		QSCA1(I) = 0.
		QABS(I) = 0.
      END DO
      DO I = 0,18000
      		RI(I) = 0.
		SUMISS(I) = 0.
		SUMIDL(I) = 0.
      END DO
C
	QEXT2 = 0.
	QSCA2 = 0.
	X1 = 0.
	QALPHAD = 0.
	REM = 0.
	IMM = 0.
	REM1 = 0.
	IMM1 = 0.
	MABS = 0.
	RC = 0.
	RIC = 0.
	RT = 0.
	RIT = 0.
	RN = 0.
	RIN = 0.
	DX = 0.
	DY = 0.
        ANGLEL = 0.
	ANGLEU = 0.
	DELTA = 0.
	NANG = 0
	RINC = 0.
	ANGLEU1 = 0.
        M1 = CMPLX(0.,0.)
        Z = CMPLX(0.,0.)
        M = DCMPLX(0.,0.)
        CM = DCMPLX(0.,0.)
        EP = 0.
	EDP = 0.
	PHI = 0.
	Q = 0.
C
	DO I = 1,3003
      		DUMC8(I) = CMPLX(0.,0.)
	END DO
	DO I = 1,3003
      		DUMC8(I) = CMPLX(0.,0.)
	END DO
	DO I = 1,17500
      		DUMC16(I) = DCMPLX(0.,0.)
	END DO
C	********* initialization **********
C
      IPARA=0
      IPERP=0
      IFL=0
      ILOW=5001
      IUP=0
      PI=3.141592653
      R2PI=SQRT(2.*PI)
      ISIZE=5001
      IMODE=0
      IJ=0
      RIMP=1.
      WRITE(*,1002)
 1002 FORMAT(1X,'WHICH PARTICLE TYPE:')
      WRITE(*,3004)
 3004 FORMAT(1X,'1)  SPHERE                    EXACT')
      WRITE(*,3001)
 3001 FORMAT(1X,'2)  COATED SPHERE             EXACT')
      WRITE(*,3006)
 3006 FORMAT(1X,'3)  ANISOTROPIC COATED SPHERE EXACT')
      WRITE(*,3002)
 3002 FORMAT(1X,'4)  INFINITE CYLINDER         EXACT')
      WRITE(*,3003)
 3003 FORMAT(1X,'5)  FINITE CYLINDER           1ST ORDER VARIATIONAL')
      WRITE(*,3005)
 3005 FORMAT(1X,'6)  COATED INFINITE CYLINDER  EXACT')
      WRITE(*,1003)
 1003 FORMAT(1X,'7)  CUBE                      SEMI-EMPIRICAL')
      WRITE(*,1004)
 1004 FORMAT(1X,'8)  OCTAHEDRA                 SEMI-EMPIRICAL')
      WRITE(*,1005)
 1005 FORMAT(1X,'9)  FLAKE                     SEMI-EMPIRICAL')
      WRITE(*,1006)
 1006 FORMAT(1X,'10) CONVEX-CONCAVE            SEMI-EMPIRICAL')
      WRITE(*,1036)
 1036 FORMAT(1X,'11) OTHER IRREGULAR           SEMI-EMPIRICAL',/)
      READ(*,*) ITYPE
      IF(ITYPE.NE.2.AND.ITYPE.NE.6.AND.ITYPE.NE.3) GOTO10
      IF(ITYPE.EQ.3) THEN
      WRITE(*,1064)
 1064 FORMAT(1X,'TANGENT INDEX OF REFRACTION FOR COATING mt & kt ?',/)
      READ(*,*) RT,RIT
      WRITE(*,1067)
 1067 FORMAT(1X,'NORMAL INDEX OF REFRACTION FOR COATING mn & kn ?',/)
      READ(*,*) RN,RIN
      ELSE
      WRITE(*,1062)
 1062 FORMAT(1X,'INDEX OF REFRACTION FOR COATING m1 & k1 ?',/)
      READ(*,*) REM1,IMM1
      M1=CMPLX(REM1,IMM1)
      IF(ITYPE.EQ.6) GOTO11
      ENDIF
      WRITE(*,1061)
 1061 FORMAT(1X,'INDEX OF REFRACTION FOR CORE m2 & k2 ?',/)
      READ(*,*) REM,IMM
      M=DCMPLX(REM,IMM)
   11 WRITE(*,1063)
 1063 FORMAT(1X,'(CORE RADIUS)/(COATING RADIUS)= ? (0=CORE RADIUS IS
     $ CONSTANT)',/)
      READ(*,*) Y
      IF(Y.LT.1.AND.Y.GT.0.) GOTO500
      IF(Y.NE.0) GOTO12
      WRITE(*,1066)
 1066 FORMAT(1X,'INPUT SIZE PARAMETER OF CORE RADIUS',/)
      READ(*,*) X
      IF(X*IMM.GT.30..OR.X*IMM1.GT.30..AND.ITYPE.EQ.2) WRITE(*,1102)
 1102 FORMAT(1X,'*** WARNING: CALCULATION MAY BE INACCURATE DUE TO
     $ BESSEL FCNS ***',/)
      GOTO500
   12 WRITE(*,1065)
 1065 FORMAT(1X,'RATIO MUST BE POSITIVE AND LESS THAN 1',/)
      GOTO11
C
C     Initialize refractive index (homogeneous particles)
   10 WRITE(*,1000)
 1000 FORMAT(1X,'INDEX OF REFRACTION m & k (of m - ik) ->',/)
      READ(*,*) REM,IMM
      M=DCMPLX(REM,-IMM)
      MABS=CDABS(M)
      IF (ITYPE.NE.5) GOTO13
C
      WRITE(*,1072)
 1072 FORMAT(1X,'RADIUS SIZE PARAMETER ?',/)
      READ(*,*) A
   13 IF (ITYPE.LE.6) GOTO500
      IF (ITYPE.NE.7) GOTO100
      VF=6./PI
      R=1.3
      G=1.75
      X0=5.D0-INT(3.*(DSQRT(MABS)-1.))
      GOTO500
  100 IF (ITYPE.NE.8) GOTO200
      VF=2.
      R=1.1
      G=2.0
      X0=9.D0-INT(3.*(DSQRT(MABS)-1.))
      GOTO500
  200 IF (ITYPE.NE.9) GOTO300
      IF (MABS**2-1..GT.10.) WRITE(*,1103)
 1103 FORMAT(1X,'*** WARNING: MAY NOT BE VALID FOR X~<5 ***',/)
      G=5.0
      X0=3.0
      WRITE(*,2055)
 2055 FORMAT(1X,'THICKNESS SIZE PARAMETER ?',/)
      READ(*,*) T
      GOTO500
  300 IF (ITYPE.NE.10) GOTO400
      VF=2.5
      R=1.3
      G=3.5
      X0=11.D0-INT(3.*(SQRT(MABS)-1.))
      GOTO500
  400 WRITE(*,1015)
 1015 FORMAT(1X,'INPUT (SURFACE AREA)/(EQUAL VOLUME SPHERE SURFACE AREA)
     1',/)
      READ (*,*) R
      WRITE(*,1017)
 1017 FORMAT(1X,'ELONGATION FACTOR, 1~SPHERICAL LIKE -- 5~FLAKE LIKE',/)
      READ(*,*) G
      IF(G.LT.4) GOTO901
      IF(MABS**2-1..GT.10.) WRITE(*,1103)
      WRITE(*,2055)
      READ(*,*) T
      ITYPE=9
      GOTO902
  901 VF=G**.5+.6
  902 WRITE(*,1019)
 1019 FORMAT(1X,'ROUGHNESS FACTOR, 1~VERY ROUGH -- 10~VERY SMOOTH',/)
      READ(*,*) X0
Cpsd
C     Define the particle size distribution
  500 IJ=IJ+1
  501 WRITE(*,1008) IJ
 1008 FORMAT(1X,'WHICH PARTICLE SIZE DISTRIBUTION (MODE#=',I2,'):',/)
      WRITE(*,1009)
 1009 FORMAT(1X,'1) MONODISPERSED          X')
  499 WRITE(*,1010)
 1010 FORMAT(1X,'2) GATES-GAUDIN-SCHUMANN  X**(-A), A>1')
      WRITE(*,1011)
 1011 FORMAT(1X,'3) LOG-NORMAL             1/(SG*X*SQRT[2*pi])
     1*EXP(-(LOG(X)-LOG(XM))**2/(2*SG**2))')
      WRITE(*,1012)
 1012 FORMAT(1X,'4) GAMMA                  U**(U+1)/U!*(X**U/S**(
     1U+1))*EXP(-U*X/S)')
      WRITE(*,1023)
 1023 FORMAT(1X,'5) MODIFIED GAMMA         X**A*EXP(-B*X**G)')
      WRITE(*,1013)
 1013 FORMAT(1X,'6) ROSIN-RAMMLER          X**(N-1)*EXP(-B*X**N)')
      IF (IMODE.NE.0) GOTO 598 !initialization loop (does not mean 
C                               that IMODE .EQ. 0 => MULTI-MODAL)
      WRITE(*,1024)
 1024 FORMAT(1X,'7) MULTI-MODAL',/)
Cpsd
  598 READ(*,*) IPSD
      IF (IMODE.EQ.0) GOTO 597
      IF (IPSD.GT.1.AND.IPSD.LT.7) GOTO498
      WRITE(*,1001)
 1001 FORMAT(1X,'NOT A VALID CHOICE !',/)
      WRITE(*,1008) IJ
      GOTO499
  498 WRITE(*,1037)
 1037 FORMAT(1X,'RELATIVE IMPORTANCE FACTOR',/
     $       1X,'(number density weight for this mode)',/)
      READ(*,*) RIMP1
      RIMP=RIMP1
      GOTO599
C
C     continue with uni-modal, non monodispersive inputs
  597 IF (IPSD.EQ.1) GOTO632
      WRITE(*,1027)
 1027 FORMAT(1X,'INPUT INTEGRATION dX [d(SIZE PAR.)] OVER SIZE DIST.'
     $,/)
      READ(*,*) RINC
      IF (IPSD.LT.7) GOTO599!go to uni-modal section
C
      WRITE(*,1025)
 1025 FORMAT(1X,'HOW MANY MODES ?',/)
      READ(*,*) IMODE
      WRITE(*,1008) 1
      GOTO499
C MONODISPERSED PARTICLE SIZE DISTRIBUTION
  632 IF(ITYPE.LE.6) GOTO592
      WRITE(*,1068)
 1068 FORMAT(1X,'CANNOT HAVE MONODISPERSED FOR THIS CASE',/)
      GOTO501
  592 WRITE(*,1034)
      READ(*,*) ANS
C
C     In the next section: if ANS.NE.0 then angle limits are set in reverse!
C     so that below, NANG = - 180 and ANGLEU1 = -180 + 180 = 0
C     Apparently the program then works with the negative value of
C     NANG and can not perform DO loops (in statement 550 
C     for example (phase function output) the WRITE does not attempt 
C     to write anything)
C
      IF(ANS.EQ.0.) GOTO593
      ANGLEL=180.
      ANGLEU=0.
      DELTA=1.
      GOTO551
  593 WRITE(*,1039)
      READ(*,*) ANGLEL,ANGLEU,DELTA
      IF(DELTA.GE.0.01) GOTO553
      WRITE(*,1041)
      GOTO593
  553 IF(ITYPE.GT.3) GOTO551
      WRITE(*,1038)
      WRITE(*,1035)
      WRITE(*,1032)
      WRITE(*,1030)
 1038 FORMAT(1X,'WHICH POLARIZATION STATE ? 1) PARALLEL')
 1035 FORMAT(1X,'                           2) PERPENDICULAR')
 1032 FORMAT(1X,'                        OR 3) RANDOM')
 1030 FORMAT(1X,'WITH RESPECT TO SCATTERING PLANE.',/)
      READ(*,*) ANS
      IF(ANS.NE.2.) IPARA=1
      IF(ANS.NE.1.) IPERP=1
  551 NANG=(ANGLEU-ANGLEL)/DELTA
      ANGLEU1=NANG*DELTA+ANGLEL
      IF(ITYPE.EQ.2.OR.ITYPE.EQ.6.OR.ITYPE.EQ.3) GOTO596
      WRITE(*,1069)
 1069 FORMAT(1X,'ENTER PARTICLE SIZE OR LENGTH PARAMETER',/)
      READ(*,*) X
      GOTO595
  596 WRITE(*,1071)
      IF(Y.NE.0) X=Y
 1071 FORMAT(1X,'ENTER SIZE PARAMETER FOR COATING',/)
      READ(*,*) ZZ
      IF(ZZ*IMM1.GT.30..AND.ITYPE.EQ.2) WRITE(*,1102)
      IF(Y.NE.0) X=X*ZZ
      Y=ZZ
  579 IF(X.LE.Y) GOTO 580
      WRITE(*,2060)
 2060 FORMAT(1X,'CORE SIZE MUST BE LESS THAN OR EQUAL TO COATING SIZE
     $ !',/)
      WRITE(*,1071)
      READ(*,*) Y
      GOTO579
  580 IF(ITYPE.EQ.6) GOTO591
      IF(ITYPE.EQ.3) THEN
      DY=Y
      DX=X
      RC=REM
      RIC=IMM
      CALL ANISO(RC,RIC,RT,RIT,RN,RIN,DX,DY,QEXT2,QSCA2,QALPHAD)
      QALPHA=QALPHAD
      QEXT=QEXT2
      QSCA=QSCA2
      ELSE
      IF(X*IMM.GT.30..OR.X*IMM1.GT.30.) WRITE(*,1102)
      PARM=1000.
      IF(REM.LT.1.) PARM=200.**REM
      IF(REM1.LT.1.) PARM=MIN(PARM,200.**SNGL(REM1))
      IF(Y.GT.(PARM+2.)) WRITE(*,1102)
  521 CALL COAT(X,Y,M,M1,QEXT,QSCA,QALPHA)
      ENDIF
      GOTO550
  595 IF(ITYPE.NE.1) GOTO591
      X1=X
      CALL MIEPHASE(X1,M,QEXT2,QSCA2,QALPHA)
      QEXT=QEXT2
      QSCA=QSCA2
      GOTO550
  591 WRITE(*,1073)
 1073 FORMAT(1X,'ORIENTATION ANGLE (0=RANDOM) ?',/)
      READ(*,*) PHIX
      PHI=PHIX/180.*PI
      IF(ITYPE.EQ.5) GOTO594
      IF(ITYPE.EQ.6) GOTO589
      IF(PHI.EQ.0.) GOTO582
      IF(PHIX.NE.90.) IFL=1
      CALL CYLINDERPHASE(X,M,PHI,QEXT,QSCA,QALPHA)
      WRITE(*,1044) 2.*PHIX
 1044 FORMAT(1X,'PHASE FUNCTION ON CONE OF APEX ANGLE',1X,F6.2,1X,
     $'DEG.')
      GOTO550
  582 WRITE(*,1038)
      WRITE(*,1035)
      WRITE(*,1032)
      WRITE(*,1030)
      READ(*,*) ANS
      IF(ANS.NE.2.) IPARA=1
      IF(ANS.NE.1.) IPERP=1
      CALL RANDC(X,M,QEXT,QSCA,QALPHA)
      GOTO550
  594 CALL SETUP(A,M)
      IF(PHI.EQ.0.) GOTO581
      IFL=2
      CALL FIBER(X,A,M,QEXT,QSCA,QALPHA)
      WRITE(7,1079)
 1079 FORMAT(1X,'PHASE FUNCTION FOR THETA, PHI=0. DEG. FIBER CO-ORDS.')
      GOTO550
  581 CALL RANDF(X,A,QEXT,QSCA,QALPHA)
      GOTO550
  589 M=M1
      IF(PHI.EQ.0.) GOTO587
      CALL COCYL(X,Y,M,PHI,QEXT,QSCA,QALPHA)
      WRITE(*,1044) 2.*PHIX
      GOTO550
  587 WRITE(*,1038)
      WRITE(*,1035)
      WRITE(*,1032)
      WRITE(*,1030)
      READ(*,*) ANS
      IF(ANS.NE.2.) IPARA=1
      IF(ANS.NE.1.) IPERP=1
      CALL RANDCC(X,Y,M,QEXT,QSCA,QALPHA)
  550 WRITE(7,1075) (ANGLEL+I*DELTA,RI(I),I=0,NANG)
 1075 FORMAT(1X,F6.2,1X,G12.5)
      WRITE(7,1076) QEXT,QSCA,QEXT-QSCA
 1076 FORMAT(1X,'QEXT=',G15.8,1X,'QSCA=',G15.8,1X,'QABS=',G15.8)
      WRITE(7,1078) QALPHA
 1078 FORMAT(1X,'MASS EXTINCTION COEF.=',G15.8,'/LAMBDA/DENSITY')
      IF(ANGLEL.GT.ANGLEU) STOP
      IF (QEXT.GT.0.) GOTO574
      WRITE(7,1077) 0.
      STOP
  574 IF(ANGLEU1.EQ.180.AND.IFL.EQ.0) WRITE(7,1077) RI(NANG)*QSCA/QEXT
      IF(IFL.EQ.2.AND.MOD((PHIX-ANGLEL)/DELTA,1.).EQ.0) WRITE(7,1077)
     $ RI(IFIX((PHIX-ANGLEL)/DELTA))*QSCA/QEXT
 1077 FORMAT(1X,'LIDAR RATIO=',G17.8)
      STOP
C
C Polydispersed particle size distributions
  599 WRITE(*,1057) FLOAT(ISIZE-1)*RINC
      WRITE(*,1059)
 1057 FORMAT(1X,'LOWER AND UPPER LIMITS OF PSD (BETWEEN 0 AND ',F9.2,
     $')',/)
 1059 FORMAT(1X,'*** WARNING: DO NOT EXCEED FUNDAMENTAL SHAPE SIZE
     $LIMIT ***')
      READ(*,*) RLOW,RUP
      IF(RLOW.LT.RINC) RLOW=RINC
      RINCINV=1./RINC
      ILOW1=RLOW*RINCINV+0.5
      IUP1=RUP*RINCINV+0.5
      IF(IUP1*RINC.LE.ISIZE) GOTO631
      WRITE(*,1022)
 1022 FORMAT(1X,'TOO MANY SIZES, PLEASE REDUCE LOWER AND/OR UPPER LIMIT
     $S',/)
      GOTO599
  631 IF(ILOW.GT.ILOW1) ILOW=ILOW1
      IF(IUP.LT.IUP1) IUP=IUP1
      IF (IPSD.NE.3) GOTO600
C
CLOG
      WRITE(*,1021)
CMOD1021 FORMAT(1X,'INPUT (2*pi*geo. mean rad. / lambda) and ln (sigma)'
CMOD    $,/)
 1021 FORMAT(1X,'INPUT geo. mean rad. (um) , sigma, lambda(um) )',/)
CMOD      READ(*,*) XM,SG
      READ(*,*) GEORAD,SIGLN,WAVLEN !** mod **
      XM = 2.*PI*GEORAD / WAVLEN !** mod **
      SG = LOG(SIGLN) !** mod **
      DO 612 I=ILOW1,IUP1
      XI=I*RINC
  612 PSD(I)=PSD(I)+(1.D0/SG/XI/R2PI)*EXP(-((LOG(XI)-LOG(XM))**2/
     $2/SG**2))*RIMP
      GOTO800
CLOG
C
  600 IF (IPSD.NE.2) GOTO700
      WRITE(*,1028)
 1028 FORMAT(1X,'VALUE FOR A ?',/)
      READ(*,*) A
      DO 620 I=ILOW1,IUP1
  620 PSD(I)=PSD(I)+(I*RINC)**(-A)*(A-1)*RLOW**(A-1)*RIMP
      GOTO800
  700 IF (IPSD.NE.4) GOTO750
      WRITE(*,1029)
 1029 FORMAT(1X,'INPUT S-MOST PROB. RADIUS & U WHERE 1/(U+1)=
     $HALF WIDTH',/)
      READ(*,*) DS,DU
      IU=INT(DU)
      FU=DU-IU
      GAMMA=1-.5748646*FU+.9512363*FU**2-.6998588*FU**3+.4245549*FU**4
     1-.1010678*FU**5
      IF (IU.EQ.0) GOTO 699
      DO 698 II=1,IU
  698 GAMMA=(FLOAT(II)+FU)*GAMMA
  699 DO 701 I=ILOW1,IUP1
      RR=I*RINC
  701 PSD(I)=PSD(I)+1.D0/GAMMA*DU**(DU+1)*(RR**DU/DS**(DU+1))*
     1EXP(-DU*RR/DS)*RIMP
      GOTO800
CMGD
C     modified gamma distribution 
  750 IF(IPSD.NE.5) GOTO775
      WRITE(*,1031)
 1031 FORMAT(1X,'INPUT A,B,G and lambda (um)',/)
      READ(*,*) DA,DB,DG,WAVLEN
      DU=(DA+1.)/DG
      IU=INT(DU)
      FU=DU-IU
      GAMMA=1-.5748646*FU+.9512363*FU**2-.6998588*FU**3+.4245549*FU**4
     1-.1010678*FU**5
      IF (IU.EQ.0) GOTO 752
      DO 753 II=1,IU
  753 GAMMA=(FLOAT(II)+FU)*GAMMA
C
C     N = a *(1/DG)*DB^[-(DA + 1)/DG] * Gamma [(DA + 1)/DG] (from ref. 1)
C     therefore a = N / (1/DG)*DB^[-(DA + 1)/DG] * Gamma [(DA + 1)/DG] = N / RN
C     If N = 1 then: a(1) = 1 / RN
C     and (for uni-modal: RIMP = 1) PSD = x^DA * EXP(-DB*x**DG) / RN 
C
  752 RN=(1./DG)*DB**(-DU)*GAMMA
C     summation in psd definition is for multi-mode summation 
      DO 751 I=ILOW1,IUP1
      RR=I*RINC
      PSDARG = RR*WAVLEN / (2.*PI)!** mod **
  751 PSD(I)=PSD(I)+PSDARG**DA*EXP(-DB*PSDARG**DG)/RN*RIMP!** mod **
      GOTO800
CMGD
  775 WRITE(*,1033)
 1033 FORMAT(1X,'INPUT B=1/MODE OF DISTRIBUTION & N',/)
      READ (*,*) DB,DN
      DO 776 I=ILOW1,IUP1
      RR=I*RINC
  776 PSD(I)=PSD(I)+RR**DN/RR*EXP(-DB*RR**DN)*DB*DN*RIMP
  800 IMODE=IMODE-1
      IF (IMODE.LE.0) GOTO852
      IJ=IJ+1
      WRITE(*,1008) IJ
      GOTO499
  852 WRITE(*,1034)
 1034 FORMAT(1X,'DO YOU WANT ANY PART OF THE PHASE FUNCTION (YES=0) ?'
     $,/)
      READ(*,*) ANS
      IF(ANS.EQ.0.) GOTO843
      ANGLEL=180.
      ANGLEU=0.
      DELTA=1.
      GOTO851
  843 WRITE(*,1039)
 1039 FORMAT(1X,'ENTER LOWEST,HIGHEST AND INCREMENT OF ANGLES IN PHASE
     1 FUNCTION',/)
      READ(*,*) ANGLEL,ANGLEU,DELTA
      IF (DELTA.GE.0.01) GOTO855
      WRITE(*,1041)
 1041 FORMAT(1X,'INCREMENT MUST BE .01 OR GREATER, PLEASE CHANGE',/)
      GOTO843
  855 IF(ITYPE.GT.3) GOTO851
      WRITE(*,1038)
      WRITE(*,1035)
      WRITE(*,1032)
      WRITE(*,1030)
      READ(*,*) ANS
      IF(ANS.NE.2.) IPARA=1
      IF(ANS.NE.1.) IPERP=1
  851 NANG=(ANGLEU-ANGLEL)/DELTA
      ANGLEU1=NANG*DELTA+ANGLEL
      IF(ITYPE.LE.6) GOTO20
      ITEST=X0*RINCINV+1
      IF(IUP.GT.ITEST) GOTO20
      ITYPE=1
      IPARA=1
      IPERP=1
   20 WRITE(*,7001)
 7001 FORMAT(1X,'DO YOU WANT TO SEE PSD ? (0=YES)',/)
      READ(*,*) ANS
      IF(ANS.EQ.0.) WRITE(*,7000) (I*RINC,PSD(I),I=ILOW,IUP)
 7000 FORMAT(4(1X,G9.3,1X,G9.4))
      WRITE(*,9991)
 9991 FORMAT(1x, /'Press return when ready to proceed'/)
      PAUSE ! ** mod **
C IS PARTICLE SHAPE SPHERICAL?
      IF(ITYPE.EQ.1) CALL SPHEREPHASE(M)
      IF(ITYPE.NE.2.AND.ITYPE.NE.6.AND.ITYPE.NE.3) GOTO850
C OR A COATED SPHERE (ANISOTROPIC OR HOMOGENEOUS)?
      IF(ITYPE.EQ.6) GOTO854
      IF(ITYPE.EQ.2) THEN
      CM=DCMPLX(-1.0001D0,0.0D0)
      IF(IUP*RINC*IMM1.GT.30.) WRITE(*,1101)
 1101 FORMAT(1X,'*** WARNING: CALCULATION FOR LARGER SIZES MAY BE
     $ INACCURATE DUE TO BESSEL FCNS ***')
      ELSE
      CM=DCMPLX(RN,RIN)
      M1=DCMPLX(RT,RIT)
      ENDIF
       print*,'X=',X
       print*,'Y=',Y
       print*,'M=',M
       print*,'M1=',M1
       print*,'CM=',CM
      IF(ITYPE.EQ.2.OR.ITYPE.EQ.3) CALL COATPHASE(X,Y,M,M1,CM)
C IS PARTICLE A COATED INFINITE CYLINDER?
  854 M=M1
      IF(ITYPE.EQ.6) CALL COCYLPHASE(X,Y,M)
C IS PARTICLE AN INFINITE CYLINDER?
  850 IF(ITYPE.EQ.4) CALL CYLPHASE(M)
C IS PARTICLE SHAPE FINITE CYLINDRICAL?
      IF(ITYPE.NE.5) GOTO853
      CALL FIBERPHASE(A,M)
C PARTICLE SHAPE IS IRREGULAR WITH RANDOM ORIENTATION
C (FROM J.E. POLLACK AND J.N. CUZZI)
C A SEMI-EMPIRICAL APPROACH
C CALCULATE SCATTERING AND ABSORPTION EFFICENCIES FOR ALL SIZES
  853 CALL QMIE(M)
      IPARA=1
      IPERP=1
C PERFORM ALL REQUIRED INTEGRATIONS FOR EFFICENCIES AND PARTIAL
C PHASE FUNCTIONS
C LIMITS FROM 0-X0
      RINCI2=RINCINV**2
      LIMIT=X0*RINCINV-1
      IF(ILOW.GT.LIMIT) GOTO2015
      SUMNS=PSD(ILOW)*ILOW**2/RINCI2
      SUM=PSD(ILOW)
      SUMA=SUMNS*ILOW*RINC
      SUMSCAS=QSCA1(ILOW)*SUMNS
      SUMABSS=QABS(ILOW)*SUMNS
      X1=ILOW*RINC
      CALL MIEPHASE(X1,M,QEXT2,QSCA2,QALPHA)
      DO 2001 II=0,NANG
 2001 SUMISS(II)=RI(II)*SUMNS
      DO 2000 I=2,LIMIT,2
      	RR=I*RINC
      	R1=RR+RINC
      	PROD1=PSD(I)*RR**2
      	PROD2=PSD(I+1)*R1**2
      	SUMNS=SUMNS+4.*PROD1+2.*PROD2
      	SUM=SUM+4.*PSD(I)+2.*PSD(I+1)
      	SUMA=SUMA+4.*PROD1*RR+2.*PROD2*R1
      	SUMSCAS=SUMSCAS+4.*QSCA1(I)*PROD1+2.*QSCA1(I+1)*PROD2
      	SUMABSS=SUMABSS+4.*QABS(I)*PROD1+2.*QABS(I+1)*PROD2
      	X1=RR
      	CALL MIEPHASE(X1,M,QEXT2,QSCA2,QALPHA)
      	DO 2002 II=0,NANG
 2002 	SUMISS(II)=SUMISS(II)+4.*RI(II)*PROD1
      	X1=R1
      	CALL MIEPHASE(X1,M,QEXT2,QSCA2,QALPHA)
      	DO 2003 II=0,NANG
 2003 	SUMISS(II)=SUMISS(II)+2.*RI(II)*PROD2
 2000 CONTINUE
      IF ((LIMIT/2)*2.EQ.LIMIT) GOTO2010
      PROD1=1.5*PSD(LIMIT+1)*((LIMIT+1)*RINC)**2
      SUMNS=SUMNS+PROD1
      SUM=SUM+1.5*PSD(LIMIT+1)
      SUMA=SUMA+PROD1*(LIMIT+1)*RINC
      SUMSCAS=SUMSCAS+QSCA1(LIMIT+1)*PROD1
      SUMABSS=SUMABSS+QABS(LIMIT+1)*PROD1
      X1=R1+RINC
      CALL MIEPHASE(X1,M,QEXT2,QSCA2,QALPHA)
      DO 2005 II=0,NANG
 2005 SUMISS(II)=SUMISS(II)+RI(II)*PROD1
      GOTO 2015
 2010 SUMNS=SUMNS-PROD2
      SUM=SUM-PSD(LIMIT+1)
      SUMA=SUMA-PROD2*(LIMIT+1)*RINC
      SUMSCAS=SUMSCAS-QSCA1(LIMIT+1)*PROD2
      SUMABSS=SUMABSS-QABS(LIMIT+1)*PROD2
      DO 2006 II=0,NANG
 2006 SUMISS(II)=SUMISS(II)-RI(II)*PROD2
C LIMITS FROM X0-MAXIMUM SIZE
 2015 LIMIT=LIMIT+1
      SUMNL=PSD(LIMIT)*X0**2
      SUMA=SUMA+SUMNL*X0
      SUMD=PSD(LIMIT)
      SUMSCAL=QSCA1(LIMIT)*SUMNL
      SUMABSL=QABS(LIMIT)*SUMNL
      DO 2500 I=LIMIT+1,IUP,2
      RR=I*RINC
      R1=RR+RINC
      PROD1=PSD(I)*RR**2
      PROD2=PSD(I+1)*R1**2
      SUMNL=SUMNL+4.*PROD1+2.*PROD2
      SUMA=SUMA+4.*PROD1*RR+2.*PROD2*R1
      SUMD=SUMD+4.*PSD(I)+2.*PSD(I+1)
      SUMSCAL=SUMSCAL+4.*QSCA1(I)*PROD1+2.*QSCA1(I+1)*PROD2
      SUMABSL=SUMABSL+4.*QABS(I)*PROD1+2.*QABS(I+1)*PROD2
 2500 CONTINUE
      LL=IUP-LIMIT-1
      IF ((LL/2)*2.EQ.LL) GOTO 2510
      PROD1=1.5*PSD(IUP+1)*((IUP+1)*RINC)**2
      SUMNL=SUMNL+PROD1
      SUMA=SUMA+PROD1*(IUP+1)*RINC
      SUMD=SUMD+1.5*PSD(IUP+1)
      SUMSCAL=SUMSCAL+QSCA1(IUP+1)*PROD1
      SUMABSL=SUMABSL+QABS(IUP+1)*PROD1
      GOTO 2515
 2510 SUMNL=SUMNL-PROD2
      SUMA=SUMA-PROD2*(IUP+1)*RINC
      SUMD=SUMD-PSD(IUP+1)
      SUMSCAL=SUMSCAL-QSCA1(IUP+1)*PROD2
      SUMABSL=SUMABSL-QABS(IUP+1)*PROD2
 2515 SUMN=SUMNS+SUMNL
      SUM=SUM+SUMD
      IF(SUMD.GT.0.) GOTO2520
      WRITE(*,2054)
      STOP
 2520 XRMS=SQRT(SUMNL/SUMD)
      IF(ITYPE.EQ.9) R=.5*(XRMS/T)**(2./3.)*EXP(-(XRMS/10.)**.5)+1
      SUMSCAL=SUMSCAL/SUMNL
      SUMABSL=SUMABSL/SUMNL
      CALL DIFFPHASE(XRMS,R)
      IF(SUMNS.NE.0) GOTO2550
      SUMSCAS=0.
      SUMABSS=0.
      GOTO2551
 2550 SUMSCAS=SUMSCAS/SUMNS
      SUMABSS=SUMABSS/SUMNS
      DO 2030 II=0,NANG
 2030 SUMISS(II)=SUMISS(II)/SUMNS
 2551 DO 2031 II=0,NANG
 2031 SUMIDL(II)=RI(II)
C CALCULATE PHASE FUNCTION AND EFFICIENCIES FOR REFLECTION AND
C TRANSMISSION COMPONENTS
C
C FIRST THE EFFICIENCIES
      F=SUMNL/SUMN
      F1=1.-F
      AR=1.
      IF (2.*IMM*X0.GT.1.) AR=R
      QS=SUMSCAL*F*R+SUMSCAS*F1
      QA=SUMABSL*F*AR+SUMABSS*F1
      QE=QS+QA
      CALL REFPHASE(M,QR)
      QT=SUMSCAL-1.-QR
      IF(QT.LT.0.) QT=0.
C THEN THE PHASE FUNCTION
      BT=.63661977*LOG(G)
      CT=2.*(BT**2+1.)/(1./G**2+1.)/4./PI
      WRITE(*,1060)
 1060 FORMAT(1X,'ANGLE',2X,'PHASE FUNC.',4X,'SPHERE Is',2X,'DIFFRAC. Id'
     $,2X,'REFLECTION Ir',2X,'TRANS. It',/)
      DO 3000 J=0,NANG
      RAD=PI*(ANGLEL+FLOAT(J)*DELTA)/180.
      RIT=CT*EXP(-BT*RAD)
      RP=SUMISS(J)*F1*SUMSCAS/QS+R*F/QS*(SUMIDL(J)+RI(J)*QR+RIT*QT)
 3000 WRITE(7,1050) RAD*180.D0/PI,RP,SUMISS(J),SUMIDL(J),
     $RI(J),RIT
 1050 FORMAT(1X,F6.2,1X,5(G12.5,1X))
      WRITE(7,1051) 1.,XRMS
 1051 FORMAT(/,1X,'DIFFRATION EFFICIENCY=',F3.1,10X,'XRMS=',F6.2)
      WRITE(7,1052) QR,F
 1052 FORMAT(1X,'REFLECTION EFFICIENCY=',F8.5,6X,'F=',F6.4)
      WRITE(7,1053) QT
 1053 FORMAT(1X,'TRANSMISSION EFFICIENCY=',F8.5)
      WRITE(7,1054) QS,SUMSCAS
 1054 FORMAT(1X,'SCATTERING EFFICIENCY=',F8.5,8X,'SMALL PART.=',F8.5)
      WRITE(7,1055) QA
 1055 FORMAT(1X,'ABSORPTION EFFICIENCY=',F8.5)
      IF(ANGLEU1.EQ.180.) WRITE(7,1056) QE,RP*QS/QE
      IF(ANGLEU.NE.180.) WRITE(7,1058) QE
 1056 FORMAT(1X,'EXTINCTION EFFICIENCY=',F8.5,5X,'LIDAR RATIO=',F12.6)
 1058 FORMAT(1X,'EXTINCTION EFFICIENCY=',F8.5)
      WRITE(7,2050) .25*QE*SUMN/SUM/PI
 2050 FORMAT(1X,'(EXTINCTION COEF)/# PARTICLE=',G15.8,'*LAMBDA**2')
      WRITE(7,2051) .25*QS*SUMN/SUM/PI
 2051 FORMAT(1X,'(SCATTERING COEF)/# PARTICLE=',G15.8,'*LAMBDA**2')
      WRITE(7,2052) .25*QA*SUMN/SUM/PI
 2052 FORMAT(1X,'(ABSORPTION COEF)/# PARTICLE=',G15.8,'*LAMBDA**2')
      IF(ITYPE.NE.9) GOTO900
      WRITE(7,2053) PI*QE/T
      STOP
  900 WRITE(7,2053) QE*SUMN*3.*PI/2./SUMA/VF
 2053 FORMAT(1X,'MASS EXTINCTION COEF.=',G15.8,'/LAMBDA/DENSITY')
 2054 FORMAT(1X,'THERE ARE NO PARTICLES IN THIS SIZE RANGE')
      STOP
      END
C
C
C     ** end of main program **
C
C INTEGRATE THE PHASE FUNCTION FOR SPHERES OVER THE PARTICLE SIZE
C DISTRIBUTION USING SIMPSON'S INTEGRATION
C BY BLAIR EVANS/DREV 
C JANUARY 1988
C -------------
C     Identification des variables (Martin Aube 1998)
C
C        PSD(*) = distribution de taille pour chaque dX
C        M = indice de refraction complexe
C        RINC = increment de la distribution de taille (dX) X=size param
C               N.B. le size param = 2 pi r / lambda
C        IUP = entier representant le bin maximal 
C        ILOW = entier representant le bin minimal
C        ANGLEL = angle minimal pour la fonction de phase
C        ANGLEU = angle maximal pour la fonction de phase
C        DELTA = increment de l'angle pour la fonction de phase
C        NANG = nombre d'angles pour la fonction de phase
C        IDIFF = nombre d'intervalles de taille (bins)
C        
C        
C -------------
      SUBROUTINE SPHEREPHASE(M)
      REAL*4 RI(0:18000),SUMISS(0:18000)
      REAL*8 PSD(5002),DUMR8(20504),QEXT,QSCA,QEXT1,QSCA1,RR,R1
      COMPLEX*16 M
      COMMON /PHASE/ RI
      COMMON /DATA/ ANGLEL,ANGLEU,DELTA,NANG,ILOW,IUP,RINC,ANGLEU1
      COMMON /GLOBALR4/ SUMISS
      COMMON /GLOBALR8/ PSD,DUMR8
      SAVE
c		print *, 'routine SPHEREPHASE'
      PI=3.141592653
      IDIFF=IUP-ILOW
      IF(IDIFF/2*2.EQ.IDIFF) GOTO10 !Simpson's "1/3" rule only works for an
C			even number of intervals (see A&S p. 886) ** mod **
      IUP=IUP+1
C
C     initialisation of PSD quadratures
   10 SUMNS=PSD(ILOW)*ILOW**2*RINC**2
      SUMN=PSD(ILOW)
      SUMA=SUMNS*ILOW*RINC
      RR=ILOW*RINC
      CALL MIEPHASE(RR,M,QEXT,QSCA,QALPHA)
      QEXT1=QEXT*SUMNS
      QSCA1=QSCA*SUMNS
      DO 2001 I=0,NANG
c		print *, 'SPHEREPHASE valeur de RI(',I,')=', RI(I)
 2001 SUMISS(I)=RI(I)*QSCA*SUMNS!** mod ** (originally RI(I)*SUMNS)
C
C     Actual quadrature.
C     For RI (which by comparison with standard results must = phase function / (4*pi) )
C     this means a Simpson's rule approximation to: int { RI * QSCA * x**2 } dx.
C     Below the quadrature loop the program simply calculates:
C     (4*pi) * int { RI * QSCA * PSD * x**2 } dx / int { QSCA * PSD * x**2 } dx
C     (note that dx is never multiplied but it clearly doesnt matter)
C
      DO 2000 I=ILOW+1,IUP,2
      	RR=I*RINC
      	R1=RR+RINC
      	PROD1=PSD(I)*RR**2
      	PROD2=PSD(I+1)*R1**2
      	SUMNS=SUMNS+4.*PROD1+2.*PROD2
      	SUMN=SUMN+4.*PSD(I)+2.*PSD(I+1)
      	SUMA=SUMA+4.*PROD1*RR+2.*PROD2*R1
      	CALL MIEPHASE(RR,M,QEXT,QSCA,QALPHA)
      	QEXT1=QEXT1+4.*QEXT*PROD1
      	QSCA1=QSCA1+4.*QSCA*PROD1
      	DO 2002 II=0,NANG
 2002 	   SUMISS(II)=SUMISS(II)+4.*RI(II)*QSCA*PROD1!** mod ** (originally RI(II)*PROD1)
      	CALL MIEPHASE(R1,M,QEXT,QSCA,QALPHA)
      	QEXT1=QEXT1+2.*QEXT*PROD2
      	QSCA1=QSCA1+2.*QSCA*PROD2
      	DO 2003 II=0,NANG
 2003 	   SUMISS(II)=SUMISS(II)+2.*RI(II)*QSCA*PROD2!** mod ** (originally RI(II)*PROD2)
C
C     	** mod **
      	IF((50*(I/50) - I).EQ.0)THEN
      	   WRITE(*, 4013)I, R1
 4013 	   FORMAT(1X, 'finished Mie computations for size index # ', 
     1     I5, ', size parameter = ',f8.2)
      	END IF
C
 2000 CONTINUE
      SUMNS=SUMNS-PROD2 !subtract out the last even term so that rather
C		than 2 last even terms there is only one (A&S p. 886) ** mod **
      SUMN=SUMN-PSD(IUP)
      SUMA=SUMA+PROD2*R1
      QEXT1=QEXT1-QEXT*PROD2
      QSCA1=QSCA1-QSCA*PROD2
      IF(SUMNS.GT.0.) GOTO14
      WRITE(*,1306)
      STOP
   14 QEXT2=QEXT1/SUMNS
      QSCA2=QSCA1/SUMNS
      SUMA=QEXT1*3.*PI/2./SUMA
      QEXT1=.25*QEXT1/SUMN/PI
      QSCA1=.25*QSCA1/SUMN/PI
      DO 2004 II=0,NANG
      SUMISS(II)=4.*PI*(SUMISS(II)-RI(II)*QSCA*PROD2)/(QSCA2*SUMNS)!** mod ** 
C     (originally SUMISS(II)=(SUMISS(II)-RI(II)*PROD2)/SUMNS)
 2004 WRITE(7,1000) ANGLEL+FLOAT(II)*DELTA,SUMISS(II)
 1000 FORMAT(1X,F6.2,1X,G12.5)
      WRITE(7,1005) QEXT2,QSCA2,QEXT2-QSCA2
 1005 FORMAT(1X,'COMP EXT EFF=',G12.6,1X,'COMP SCA EFF=',G12.6,1X,
     $'COMP ABS EFF=',G12.6)
      IF(ANGLEU1.EQ.180.) WRITE(7,1006) SUMISS(NANG)*QSCA2/QEXT2
C
C     ** mod. **
      WRITE(7,10010) SUMNS,SUMN
10010 FORMAT(1X,'a = int{dN/dx*r**2}dx = ',G12.6/
     $       1X,'(area of all part. = A = [lambda^2/(4*pi)] * a'/
     $       1X,'N = int{dN/dx}dx =  ',G12.6,' part. / unit vol.')
      WRITE(7,1001) QEXT1
      WRITE(7,1003) QSCA1
      WRITE(7,1004) QEXT1-QSCA1
      WRITE(7,1002) SUMA
 1002 FORMAT(1X,'MASS EXT=',G12.6,'/lambda/DENSITY')
 1001 FORMAT(1X,'EXT/PART=',G12.6,'*lambda^2 (ext. cross sect.)')
 1003 FORMAT(1X,'SCA/PART=',G12.6,'*lambda^2 (sca. cross sect.)')
 1004 FORMAT(1X,'ABS/PART=',G12.6,'*lambda^2 (abs. cross sect.)'/
     $       1X,'Cross section / (A / N) should = COMP EFF'/
     $       1X,'Units of A and cross sect. are units of lambda^2')
C     ** mod. **
C
 1006 FORMAT(1X,'LIDAR RATIO=',G12.6)
 1306 FORMAT(1X,'THERE ARE NO PARTICLES IN THIS SIZE RANGE')
      STOP
      END
C PHASE FUNCTION/MIE CODE FOR SPHERICAL PARTICLES
C BY BLAIR EVANS/DREV
C JANUARY 1988
C -------------
C     Identification des variables (Martin Aube 1998)
C
C        M = indice de refraction complexe
C        RINC = increment de la distribution de taille (dX) X=size param
C               N.B. le size param = 2 pi r / lambda
C        IUP = entier representant le bin maximal 
C        ILOW = entier representant le bin minimal
C        ANGLEL = angle minimal pour la fonction de phase
C        ANGLEU = angle maximal pour la fonction de phase
C        DELTA = increment de l'angle pour la fonction de phase
C        NANG = nombre d'angles pour la fonction de phase
C        IDIFF = nombre d'intervalles de taille (bins)
C        X = valeur du param de Mie 
C        
C -------------
      SUBROUTINE MIEPHASE(X,M,QEXT,QSCA,QALPHA)
      REAL*4 RI(0:18000)
      REAL*8 PSI(3500),CHI(3500),ETA2(3500)
      REAL*8 X,P0,SCALE,MABS,C0,YL,T2,QEXT,QSCA
      REAL*8 XL,PI1,PI2,PTMP,PP1,PP2,RAD,TAU,QPH1,QPH2,QPH3,FACT
      REAL*8 DUMMY(15006)
      COMPLEX*16 ETA3(3500),ETA1(7000),A(3500),B(3500),M,ZETA
      COMPLEX*16 CDI,Z,QPHASE1,QPHASE2
      COMMON /PHASE/ RI
      COMMON /DATA/ ANGLEL,ANGLEU,DELTA,NANG,ILOW,IUP,RINC,ANGLEU1
      COMMON /POLAR/ IPARA,IPERP
      COMMON /GLOBALR8/ DUMMY,PSI,CHI,ETA2
      COMMON /GLOBALC16/ ETA3,ETA1,A,B
      SAVE
c		print *, 'routine MIEPHASE'
      DO 111 I=1,3500
  111 CHI(I)=1.E32
      MABS=CDABS(M)
      IF(IPARA.EQ.0.AND.IPERP.EQ.0) GOTO9
      FACT=1./DBLE(IPARA+IPERP)
    9 NL3=1.5*X+10
C CALCULATE PSI(X) FOR ALL ORDERS BY BACKWARD RECURRENCE
C (ABRAMOWITZ & STEGUN P 452)
      IF(X.GT.100) GOTO2
      NL1=2*X+10
      GOTO1
    2 NL1=MAX0(NL3,IDINT(X+4.*X**.533+2))
    1 PSI(NL1+2)=0.D0
      PSI(NL1+1)=1.D0
      KK=0
      DO 20 I=NL1,1,-1
      IF (PSI(I+2).LT.1.E25) GOTO20
      K1=I+2
      KK=KK+1
      PSI(I+1)=PSI(I+1)*1.E-30
      PSI(I+2)=PSI(I+2)*1.E-30
   20 PSI(I)=DBLE(2*I+3)/X*PSI(I+1)-PSI(I+2)
      P0=3.D0/X*PSI(1)-PSI(2)
      SCALE=P0/DSIN(X)
      DO 22 I=1,NL1
   22 PSI(I)=PSI(I)/SCALE
      IF(KK.EQ.0) GOTO999
      DO 23 I=K1+1,NL1
   23 PSI(I)=PSI(I)*1.E-30
  999 P0=DSIN(X)
C CALCULATE ETA1(MX) FOR ALL ORDER BY BACKWARD RECURRENCE
C (KATTAWAR & PLASS APPL. OPT. 6 1377-1382)
      NL2=2.0*MABS*X+10
      Z=M*DCMPLX(X,0D0)
      ETA1(NL2 + 1)=DCMPLX(0.D0,0.D0)! ** mod. ** NL2 + 1 for NL2
      DO 40 I=NL2,1,-1
      CDI=DBLE(I+1)/Z
   40 ETA1(I)=CDI-1.D0/(ETA1(I+1)+CDI)
C CALCULATE ETA2(X) FOR ALL ORDERS BY BACKWARD RECURRENCE
C (KERKER P 67-68)
      ETA2(NL3 + 1)=0.! ** mod. ** NL3 + 1 for NL3
      DO 50 I=NL3,1,-1
   50 ETA2(I)=DBLE(I+1)/X-1./(ETA2(I+1)+DBLE(I+1)/X)
C CALCULATE ETA3(X) AND CHI(X) FOR ALL ORDERS BY FORWARD RECURRENCE
C AND THE MIE SCATTERING COEFFICIENTS
C (KERKER P 67-68)
      C0=DCOS(X)
      CHI(1)=DCOS(X)/X+DSIN(X)
      ETA3(1)=DCMPLX(P0,C0)/DCMPLX(PSI(1),CHI(1))-1.D0/X
      CHI(2)=DBLE(3)/X*CHI(1)-C0
      ETA3(2)=1.D0/(2.D0/X-ETA3(1))-2.D0/X
      ZETA=DCMPLX(PSI(1),CHI(1))
      A(1)=PSI(1)*((ETA1(1)-M*ETA2(1))/(ETA1(1)-M*ETA3(1)))/ZETA
      B(1)=PSI(1)*((ETA2(1)-M*ETA1(1))/(ETA3(1)-M*ETA1(1)))/ZETA
      ZETA=DCMPLX(PSI(2),CHI(2))
      A(2)=PSI(2)*((ETA1(2)-M*ETA2(2))/(ETA1(2)-M*ETA3(2)))/ZETA
      B(2)=PSI(2)*((ETA2(2)-M*ETA1(2))/(ETA3(2)-M*ETA1(2)))/ZETA
      K2=0
      DO 60 I=3,NL3
      ETA3(I)=1.D0/(DBLE(I)/X-ETA3(I-1))-DBLE(I)/X
      IF(K2.EQ.1) GOTO65
      CHI(I)=(2.D0*DBLE(I)-1.D0)/X*CHI(I-1)-CHI(I-2)
      IF(CHI(I).GT.1.E32) K2=1
   65 ZETA=DCMPLX(PSI(I),CHI(I))
      A(I)=PSI(I)*((ETA1(I)-M*ETA2(I))/(ETA1(I)-M*ETA3(I)))/ZETA
      B(I)=PSI(I)*((ETA2(I)-M*ETA1(I))/(ETA3(I)-M*ETA1(I)))/ZETA
   60 CONTINUE
C CALCULATE LEGENDRE FUNCTIONS PI1,PI2,PP1,PP2 AND TAU AT ANGLE THETA
C FOR THETA=0 TO 180 DEGREES
      QEXT=0.D0
      QSCA=0.D0
      I=0
    5 IF (I.LT.X) GOTO6
      IF (I.GE.NL3) GOTO74
      IF (DBLE(A(I)).LE.1.E-17) GOTO74
    6 I=I+1
      QEXT=QEXT+DBLE(2*I+1)*(DBLE(A(I)+B(I)))
      QSCA=QSCA+DBLE(2*I+1)*((CDABS(A(I)))**2+(CDABS(B(I)))**2)
      GOTO5
   74 QEXT=2.D0/X**2*QEXT
      QSCA=2.D0/X**2*QSCA
      QALPHA=4.71238898037/X*QEXT
C
C     ____________________________________________________________
      DO 100 J=0,NANG
      RAD=3.1415926535897939/180.D0*(ANGLEL+DBLE(J)*DELTA)
      XL=DCOS(RAD)
      YL=DSIN(RAD)
      PI1=1.D0
      PI2=3.D0*XL
      PP1=0.D0
      PP2=3.D0
      T2=XL*PI2-YL*YL*3.D0
C AND CALCULATE PHASE FUNCTION (QPHASE1 AND QPHASE2) FOR BOTH
C POLARIZATION STATES AND THE AVERAGE QPH3
      QPHASE1=1.5D0*(A(1)+B(1)*XL)+5.D0/6.D0*(A(2)*PI2+B(2)*T2)
      QPHASE2=1.5D0*(B(1)+A(1)*XL)+5.D0/6.D0*(B(2)*PI2+A(2)*T2)
      I=2
    3 IF (I.LT.X) GOTO4
      IF (I.GE.NL3) GOTO75
      IF (DBLE(A(I)).LE.1.E-17) GOTO75
    4 I=I+1
      PI1=DBLE(2*I-1)/DBLE(I-1)*XL*PI2-DBLE(I)/DBLE(I-1)*PI1
      PP1=DBLE(2*I-1)*PI2+PP1
      TAU=XL*PI1-YL*YL*PP1
      QPHASE1=QPHASE1+DBLE(2*I+1)/DBLE(I*(I+1))*(A(I)*PI1+B(I)*TAU)
      QPHASE2=QPHASE2+DBLE(2*I+1)/DBLE(I*(I+1))*(A(I)*TAU+B(I)*PI1)
      PTMP=PI1
      PI1=PI2
      PI2=PTMP
      PTMP=PP1
      PP1=PP2
      PP2=PTMP
      GOTO3
   75 QPH1=4.D0/X**2*CDABS(QPHASE1)**2
      QPH2=4.D0/X**2*CDABS(QPHASE2)**2
      QPH3=(QPH1*IPARA+QPH2*IPERP)*FACT
  100 RI(J)=QPH3/12.566371/QSCA
C     ____________________________________________________________
C
      RETURN
      END
C INTEGRATE COATED SPHERE PHASE FUNCTION OVER PARTICLE SIZE
C DISTRIBUTION BY SIMPSON'S METHOD
C BY BLAIR EVANS/DREV
C JANUARY 1988
      SUBROUTINE COATPHASE(X,Y,M,M2,CM)
      REAL*4 RI(0:18000),SUMISS(0:18000)
      REAL*8 PSD(5002),DUMR8(20504),RC,RIC,RT,RIT,RN,RIN,QEXT3
      REAL*8 QSCA3,QALPHA3,X3,Y3
      COMPLEX*8 M2
      COMPLEX*16 M,CM
      COMMON /PHASE/ RI
      COMMON /DATA/ ANGLEL,ANGLEU,DELTA,NANG,ILOW,IUP,RINC,ANGLEU1
      COMMON /GLOBALR4/ SUMISS
      COMMON /GLOBALR8/ PSD,DUMR8
      SAVE
c		print *, 'routine COATPHASE'
      PI=3.141592653
      IFL=DINT(DBLE(CM))
      IDIFF=IUP-ILOW
      IF(IDIFF/2*2.EQ.IDIFF) GOTO10
      IUP=IUP+1
   10 C=X
      F=Y
      IF(Y.NE.0.) C=0.
      SUMNS=PSD(ILOW)*ILOW**2*RINC**2
      SUMN=PSD(ILOW)
      SUMA=SUMNS*ILOW*RINC
      Y=ILOW*RINC
      X=F*Y+C
      IF(IFL.EQ.-1) THEN
      CALL COAT(X,Y,M,M2,QEXT,QSCA,QALPHA)
      ELSE
      RC=DBLE(M)
      RIC=DIMAG(M)
      RT=REAL(M2)
      RIT=AIMAG(M2)
      RN=DBLE(CM)
      RIN=DIMAG(CM)
      X3=X
      Y3=Y
      CALL ANISO(RC,RIC,RT,RIT,RN,RIN,X3,Y3,QEXT3,QSCA3,QALPHA3)
      QEXT=QEXT3
      QSCA=QSCA3
      ENDIF
      QEXT1=QEXT*SUMNS
      QSCA1=QSCA*SUMNS
      DO 2001 I=0,NANG
 2001 SUMISS(I)=RI(I)*SUMNS
      DO 2000 I=ILOW+1,IUP,2
      RR=I*RINC
      R1=RR+RINC
      PROD1=PSD(I)*RR**2
      PROD2=PSD(I+1)*R1**2
      SUMNS=SUMNS+4.*PROD1+2.*PROD2
      SUMN=SUMN+4.*PSD(I)+2.*PSD(I+1)
      SUMA=SUMA+4.*PROD1*RR+2.*PROD2*R1
      Y=RR
      X=F*Y+C
      IF(IFL.EQ.-1) THEN
      CALL COAT(X,Y,M,M2,QEXT,QSCA,QALPHA)
      ELSE
      X3=X
      Y3=Y
      CALL ANISO(RC,RIC,RT,RIT,RN,RIN,X3,Y3,QEXT3,QSCA3,QALPHA3)
      QEXT=QEXT3
      QSCA=QSCA3
      ENDIF
      QEXT1=QEXT1+4.*QEXT*PROD1
      QSCA1=QSCA1+4.*QSCA*PROD1
      DO 2002 II=0,NANG
 2002 SUMISS(II)=SUMISS(II)+4.*RI(II)*PROD1
      Y=R1
      X=F*Y+C
      IF(IFL.EQ.-1) THEN
      CALL COAT(X,Y,M,M2,QEXT,QSCA,QALPHA)
      ELSE
      X3=X
      Y3=Y
      CALL ANISO(RC,RIC,RT,RIT,RN,RIN,X3,Y3,QEXT3,QSCA3,QALPHA3)
      QEXT=QEXT3
      QSCA=QSCA3
      ENDIF
      QEXT1=QEXT1+2.*QEXT*PROD2
      QSCA1=QSCA1+2.*QSCA*PROD2
      DO 2003 II=0,NANG
 2003 SUMISS(II)=SUMISS(II)+2.*RI(II)*PROD2
 2000 CONTINUE
      SUMNS=SUMNS-PROD2
      SUMN=SUMN-PSD(IUP)
      SUMA=SUMA-PROD2*RR
      QEXT1=QEXT1-QEXT*PROD2
      QSCA1=QSCA1-QSCA*PROD2
      IF(SUMNS.GT.0.) GOTO14
      WRITE(*,1306)
      STOP
   14 QEXT2=QEXT1/SUMNS
      QSCA2=QSCA1/SUMNS
      SUMA=QEXT1*3.*PI/2./SUMA
      QEXT1=.25*QEXT1/SUMN/PI
      QSCA1=.25*QSCA1/SUMN/PI
      DO 2004 II=0,NANG
      SUMISS(II)=(SUMISS(II)-RI(II)*PROD2)/SUMNS
 2004 WRITE(7,1000) ANGLEL+FLOAT(II)*DELTA,SUMISS(II)
 1000 FORMAT(1X,F6.2,1X,G12.5)
      WRITE(7,1005) QEXT2,QSCA2,QEXT2-QSCA2
 1005 FORMAT(1X,'COMP EXT EFF=',G12.6,1X,'COMP SCA EFF=',G12.6,1X,
     $'COMP ABS EFF=',G12.6)
      IF(ANGLEU1.EQ.180.) WRITE(7,1006) SUMISS(NANG)*QSCA2/QEXT2
      WRITE(7,1001) QEXT1
      WRITE(7,1003) QSCA1
      WRITE(7,1004) QEXT1-QSCA1
      WRITE(7,1002) SUMA
 1002 FORMAT(1X,'MASS EXT=',G12.6,'/LAMBDA/(AVG. DENSITY)')
 1001 FORMAT(1X,'EXT/PART=',G12.6,'*LAMBDA**2')
 1003 FORMAT(1X,'SCA/PART=',G12.6,'*LAMBDA**2')
 1004 FORMAT(1X,'ABS/PART=',G12.6,'*LAMBDA**2')
 1006 FORMAT(1X,'LIDAR RATIO=',G12.6)
 1306 FORMAT(1X,'THERE ARE NO PARTICLES IN THIS SIZE RANGE')
      STOP
      END
C COATED SPHERE MIE CALCULATION (FROM BOHREN AND HUFFMAN 1983)
C MODIFIED FOR CALCULATING PHASE FUNCTION ADDITIONALLY
C BY BLAIR EVANS/DREV
C JANUARY 1988
      SUBROUTINE COAT(X,Y,M,M2,QEXT,QSCA,QALPHA)
      REAL*4 RI(0:18000)
      COMPLEX*8 M2
      COMPLEX*16 A(8750),B(8750),X1,X2,Y2,REFREL
      COMPLEX*16 D1X1,D0X1,D1X2,D0X2,D1Y2,D0Y2,QPHASE1,QPHASE2
      COMPLEX*16 XI0Y,XI1Y,XIY,CHI0Y2,CHI1Y2,CHIY2,CHI0X2,CHI1X2,CHIX2
      COMPLEX*16 CHIPX2,CHIPY2,ANCAP,BNCAP,DNBAR,GNBAR,AN,BN,CRACK,BRACK
      COMPLEX*16 AMESS1,AMESS2,AMESS3,AMESS4
      COMPLEX*16 M
      COMMON /PHASE/ RI
      COMMON /DATA/ ANGLEL,ANGLEU,DELTA,NANG,ILOW,IUP,RINC,ANGLEU1
      COMMON /POLAR/ IPARA,IPERP
      COMMON /GLOBALC16/ A,B
      SAVE
c		print *, 'routine COAT'
      IF(IPARA.EQ.0.AND.IPERP.EQ.0) GOTO1
      FACT=1./FLOAT(IPARA+IPERP)
    1 DEL=1.0E-8
      X1=M*X
      X2=M2*X
      Y2=M2*Y
      YSTOP=Y+4.*Y**.3333+2.
      REFREL=M2/M
      NSTOP=YSTOP
      D0X1=CDCOS(X1)/CDSIN(X1)
      D0X2=CDCOS(X2)/CDSIN(X2)
      D0Y2=CDCOS(Y2)/CDSIN(Y2)
      PSI0Y=COS(Y)
      PSI1Y=SIN(Y)
      CHI0Y=-SIN(Y)
      CHI1Y=COS(Y)
      XI0Y=CMPLX(PSI0Y,-CHI0Y)
      XI1Y=CMPLX(PSI1Y,-CHI1Y)
      CHI0Y2=-CDSIN(Y2)
      CHI1Y2=CDCOS(Y2)
      CHI0X2=-CDSIN(X2)
      CHI1X2=CDCOS(X2)
      QSCA=0.
      QEXT=0.
      N=1
      IFLAG=0
  200 RN=N
      PSIY=(2.*RN-1.)*PSI1Y/Y-PSI0Y
      CHIY=(2.*RN-1.)*CHI1Y/Y-CHI0Y
      XIY=CMPLX(PSIY,-CHIY)
      D1Y2=1./(RN/Y2-D0Y2)-RN/Y2
      IF(IFLAG.EQ.1) GOTO999
      D1X1=1./(RN/X1-D0X1)-RN/X1
      D1X2=1./(RN/X2-D0X2)-RN/X2
      CHIX2=(2.*RN-1.)*CHI1X2/X2-CHI0X2
      CHIY2=(2.*RN-1.)*CHI1Y2/Y2-CHI0Y2
      CHIPX2=CHI1X2-RN*CHIX2/X2
      CHIPY2=CHI1Y2-RN*CHIY2/Y2
      ANCAP=REFREL*D1X1-D1X2
      QPHASE2=REFREL*D1X1*CHIX2-CHIPX2
      IF(CDABS(QPHASE2).LT.1.E-25) IFLAG=1
      IF(QPHASE2.EQ.CMPLX(0.,0.)) QPHASE2=CMPLX(1.E-25,1.E-25)
      ANCAP=ANCAP/QPHASE2
      QPHASE1=CHIX2*D1X2-CHIPX2
      IF(CDABS(QPHASE1).LT.1.E-25) IFLAG=1
      IF(QPHASE1.EQ.CMPLX(0.,0.)) QPHASE1=CMPLX(1.E-25,1.E-25)
      ANCAP=ANCAP/QPHASE1
      BRACK=ANCAP*(CHIY2*D1Y2-CHIPY2)
      BNCAP=REFREL*D1X2-D1X1
      QPHASE2=REFREL*CHIPX2-D1X1*CHIX2
      IF(CDABS(QPHASE2).LT.1.E-25) IFLAG=1
      IF(QPHASE2.EQ.CMPLX(0.,0.)) QPHASE2=CMPLX(1.E-25,1.E-25)
      BNCAP=BNCAP/QPHASE2
      BNCAP=BNCAP/QPHASE1
      CRACK=BNCAP*(CHIY2*D1Y2-CHIPY2)
      AMESS1=BRACK*CHIPY2
      AMESS2=BRACK*CHIY2
      AMESS3=CRACK*CHIPY2
      AMESS4=CRACK*CHIY2
      IF(CDABS(AMESS1).GT.DEL*CDABS(D1Y2)) GOTO999
      IF(CDABS(AMESS2).GT.DEL) GOTO999
      IF(CDABS(AMESS3).GT.DEL*CDABS(D1Y2)) GOTO999
      IF(CDABS(AMESS4).GT.DEL) GOTO999
      BRACK=CMPLX(0.,0.)
      CRACK=CMPLX(0.,0.)
      IFLAG=1
  999 DNBAR=D1Y2-BRACK*CHIPY2
      DNBAR=DNBAR/(1.-BRACK*CHIY2)
      GNBAR=D1Y2-CRACK*CHIPY2
      GNBAR=GNBAR/(1.-CRACK*CHIY2)
      AN=(DNBAR/M2+RN/Y)*PSIY-PSI1Y
      AN=AN/((DNBAR/M2+RN/Y)*XIY-XI1Y)
      A(N)=AN
      BN=(M2*GNBAR+RN/Y)*PSIY-PSI1Y
      BN=BN/((M2*GNBAR+RN/Y)*XIY-XI1Y)
      B(N)=BN
      QSCA=QSCA+(2.*RN+1.)*(CDABS(AN)*CDABS(AN)+CDABS(BN)*CDABS(BN))
      QEXT=QEXT+(2.*RN+1.)*(DBLE(AN)+DBLE(BN))
      PSI0Y=PSI1Y
      PSI1Y=PSIY
      CHI0Y=CHI1Y
      CHI1Y=CHIY
      XI1Y=CMPLX(PSI1Y,-CHI1Y)
      CHI0X2=CHI1X2
      CHI1X2=CHIX2
      CHI0Y2=CHI1Y2
      CHI1Y2=CHIY2
      D0X1=D1X1
      D0X2=D1X2
      D0Y2=D1Y2
      N=N+1
      IF(N-1-NSTOP) 200,300,300
  300 QSCA=(2./(Y*Y))*QSCA
      QEXT=(2./(Y*Y))*QEXT
      QALPHA=4.71238898037*QEXT/Y
      DO 100 J=0,NANG
      RAD=3.14159265/180.*(ANGLEL+FLOAT(J)*DELTA)
      XL=COS(RAD)
      YL=SIN(RAD)
      PI1=1.
      PI2=3.*XL
      PP1=0.
      PP2=3.
      T2=XL*PI2-YL*YL*3.
      QPHASE1=1.5*(A(1)+B(1)*XL)+5./6.*(A(2)*PI2+B(2)*T2)
      QPHASE2=1.5*(B(1)+A(1)*XL)+5./6.*(B(2)*PI2+A(2)*T2)
      I=2
    3 I=I+1
      PI1=FLOAT(2*I-1)/FLOAT(I-1)*XL*PI2-FLOAT(I)/FLOAT(I-1)*PI1
      PP1=FLOAT(2*I-1)*PI2+PP1
      TAU=XL*PI1-YL*YL*PP1
      QPHASE1=QPHASE1+FLOAT(2*I+1)/FLOAT(I*(I+1))*(A(I)*PI1+B(I)*TAU)
      QPHASE2=QPHASE2+FLOAT(2*I+1)/FLOAT(I*(I+1))*(B(I)*PI1+A(I)*TAU)
      PTMP=PI1
      PI1=PI2
      PI2=PTMP
      PTMP=PP1
      PP1=PP2
      PP2=PTMP
      IF(I.LE.NSTOP) GOTO3
      QPH1=4./Y**2*CDABS(QPHASE1)**2
      QPH2=4./Y**2*CDABS(QPHASE2)**2
      QPH3=(QPH1*IPARA+QPH2*IPERP)*FACT
  100 RI(J)=QPH3/12.566371/QSCA
      RETURN
      END
C ANISOTROPIC COATED SPHERE MIE CALCULATION (FROM ROTH AND DIGNAM
C J. OPTICAL SOC. AM. V63 N3 1973) WITH CORRECTIONS.
C BY BLAIR EVANS/DREV
C NOVEMBER 1989
      SUBROUTINE ANISO(RC,RIC,RT,RIT,RN,RIN,X,Y,QEXT,QSCA,QALPHA)
      IMPLICIT REAL*8 (O-Z)
      IMPLICIT COMPLEX*16 (A-H)
      REAL*4 RI(0:18000),RANGLEL,RANGLEU,RDELTA,RINC,ANGLEU1
      COMPLEX*16 M,MT,MN,PSIMX(0:1100),PSIY(0:1100),PSIMTX(0:1100)
      COMPLEX*16 PSIMTY(0:1100),PSIMY(0:1100),AN(1100),BN(1100),LADD
      COMPLEX*16 DUMC16(9795)
      COMMON /PHASE/ RI
      COMMON /DATA/ RANGLEL,RANGLEU,RDELTA,NANG,ILOW,IUP,RINC,ANGLEU1
      COMMON /POLAR/ IPARA,IPERP
      COMMON /GLOBALC16/ PSIMX,PSIY,PSIMTX,PSIMTY,PSIMY,AN,BN,DUMC16
      SAVE
c		print *, 'routine ANISO'
      PI=3.141592653589793238D0
      IF(IPARA.EQ.0.AND.IPERP.EQ.0) GOTO9
      FACT=1./DBLE(IPARA+IPERP)
    9 QSCA=0.D0
      QEXT=0.D0
      MT=DCMPLX(RT,RIT)
      MN=DCMPLX(RN,RIN)
      M=DCMPLX(RC,RIC)
      CMTX=MT*X
      CMX=M*X
      CMTY=MT*Y
      CMY=M*Y
      CKMTX=(PI*CMTX/2.D0)**.5
      CKMTY=(PI*CMTY/2.D0)**.5
      CKMY=(PI*CMY/2.D0)**.5
      CKMX=(PI*CMX/2.D0)**.5
      CKY=(PI*Y/2.D0)**.5
C BACKWARD RECURRENCE FOR HALF INTEGER ORDER BESSEL FUNCTIONS OF
C FIRST KIND + CONVERTING TO RICCATI-BESSEL FUNCTIONS.
      LY=Y+4.05*Y**.333333+2
      LX=LY
C PSI(MT*Y)
      Z1=DBLE(CMTY)
      Z2=DIMAG(CMTY)
      V1=DBLE(LY+1)+.5D0
      V2=0.
      CALL BESSEL(V1,V2,Z1,Z2,PSIMTY(LY+1))
      PSIMTY(LY+1)=CKMTY*CDEXP(PSIMTY(LY+1))
      V1=V1-1.D0
      CALL BESSEL(V1,V2,Z1,Z2,PSIMTY(LY))
      PSIMTY(LY)=CKMTY*CDEXP(PSIMTY(LY))
      DO 10 I=LY-1,0,-1
   10 PSIMTY(I)=DBLE(2*I+3)/CMTY*PSIMTY(I+1)-PSIMTY(I+2)
C PSI(Y)
      Z1=Y
      Z2=0.D0
      V1=DBLE(LY+1)+.5D0
      CALL BESSEL(V1,V2,Z1,Z2,PSIY(LY+1))
      PSIY(LY+1)=CKY*CDEXP(PSIY(LY+1))
      V1=V1-1.D0
      CALL BESSEL(V1,V2,Z1,Z2,PSIY(LY))
      PSIY(LY)=CKY*CDEXP(PSIY(LY))
      DO 20 I=LY-1,0,-1
   20 PSIY(I)=DBLE(2*I+3)/Y*PSIY(I+1)-PSIY(I+2)
C PSI(MTX)
      Z1=DBLE(CMTX)
      Z2=DIMAG(CMTX)
      V1=DBLE(LX+1)+.5D0
      CALL BESSEL(V1,V2,Z1,Z2,PSIMTX(LX+1))
      PSIMTX(LX+1)=CKMTX*CDEXP(PSIMTX(LX+1))
      V1=V1-1.D0
      CALL BESSEL(V1,V2,Z1,Z2,PSIMTX(LX))
      PSIMTX(LX)=CKMTX*CDEXP(PSIMTX(LX))
      DO 30 I=LX-1,0,-1
   30 PSIMTX(I)=DBLE(2*I+3)/CMTX*PSIMTX(I+1)-PSIMTX(I+2)
C PSI(MX)
      Z1=DBLE(CMX)
      Z2=DIMAG(CMX)
      V1=DBLE(LX+1)+.5D0
      CALL BESSEL(V1,V2,Z1,Z2,PSIMX(LX+1))
      PSIMX(LX+1)=CKMX*CDEXP(PSIMX(LX+1))
      V1=V1-1.D0
      CALL BESSEL(V1,V2,Z1,Z2,PSIMX(LX))
      PSIMX(LX)=CKMX*CDEXP(PSIMX(LX))
      DO 40 I=LX-1,0,-1
   40 PSIMX(I)=DBLE(2*I+3)/CMX*PSIMX(I+1)-PSIMX(I+2)
C PSI(MY)
      Z1=DBLE(CMY)
      Z2=DIMAG(CMY)
      V1=DBLE(LY+1)+.5D0
      CALL BESSEL(V1,V2,Z1,Z2,PSIMY(LY+1))
      PSIMY(LY+1)=CKMY*CDEXP(PSIMY(LY+1))
      V1=V1-1.D0
      CALL BESSEL(V1,V2,Z1,Z2,PSIMY(LY))
      PSIMY(LY)=CKMY*CDEXP(PSIMY(LY))
      DO 50 I=LY-1,0,-1
   50 PSIMY(I)=DBLE(2*I+3)/CMY*PSIMY(I+1)-PSIMY(I+2)
C GET INTIAL VALUES FOR NEGATIVE HALF INTEGER ORDER BESSEL FUNCTIONS
C OF FIRST KIND FOR FORWARD RECURRANCE + MAKING THEM RICCATI-BESSEL
C FUNCTIONS.
C CHI(Y)
      CHIY0=-DCMPLX(DCOS(Y),0.D0)
      CHIY1=-DCMPLX(DSIN(Y),0.D0)
C CHI(MTY)
      CMTY0=-CDCOS(CMTY)
      CMTY1=-CDSIN(CMTY)
C CHI(MY)
      CMY0=-CDCOS(CMY)
      CMY1=-CDSIN(CMY)
C CHI(MTX)
      CMTX0=-CDCOS(CMTX)
      CMTX1=-CDSIN(CMTX)
C START OF MAIN LOOP TO CALCULATE MIE COEFFICIENTS. INCLUDES
C CALCULATION OF BESSEL FUNCTIONS OF THE FIRST KIND OF COMPLEX ORDER.
      ZMTX1=DBLE(CMTX)
      ZMTX2=DIMAG(CMTX)
      ZMTY1=DBLE(CMTY)
      ZMTY2=DIMAG(CMTY)
      LL=1
      L=0
      CMTMN2=(MT/MN)**2
  100 L=L+1
      CW=(L*L*CMTMN2+L*CMTMN2+.25)**.5
      V1=DBLE(CW)
      V2=DIMAG(CW)
C CALCULATE NEGATIVE HALF INTEGER ORDER BESSEL FUNCTIONS OF FIRST KIND
C + RICCATI-BESSEL FUNCTIONS.
      CZ=(-2.*L+1)*CHIY0/Y-CHIY1
      CHIY1=CHIY0
      CHIY0=CZ
      CZ=(-2.*L+1)*CMTY0/CMTY-CMTY1
      CMTY1=CMTY0
      CMTY0=CZ
      CZ=(-2.*L+1)*CMY0/CMY-CMY1
      CMY1=CMY0
      CMY0=CZ
      CZ=(-2.*L+1)*CMTX0/CMTX-CMTX1
      CMTX1=CMTX0
      CMTX0=CZ
C CALCULATE POSITIVE AND NEGATIVE COMPLEX ORDER BESSEL FUNCTIONS OF
C FIRST KIND.  THE ORDER BEING CW. MAKE THEM RICCATI-BESSEL LIKE.
C CHI_W(MTX)
      CALL BESSEL(-V1,-V2,ZMTX1,ZMTX2,CWMTX)
      CWMTX=CDLOG(-CKMTX)+CWMTX
C CHI_W(MTY)
      CALL BESSEL(-V1,-V2,ZMTY1,ZMTY2,CWMTY)
      CWMTY=-CKMTY*CDEXP(CWMTY)
C PSI_W(MTX)
      CALL BESSEL(V1,V2,ZMTX1,ZMTX2,BWMTX)
      BWMTX=CKMTX*CDEXP(BWMTX)
C PSI_W(MTY)
      CALL BESSEL(V1,V2,ZMTY1,ZMTY2,BWMTY)
      BWMTY=CKMTY*CDEXP(BWMTY)
C CALCULATE THE DERIVATIVES OF ALL RICCATI-BESSEL FUNCTIONS (INCLUDING
C THOSE OF COMPLEX ORDER.
C PSI'(MTY)
      BMTYP=PSIMTY(L-1)-DBLE(L)*PSIMTY(L)/CMTY
C PSI'(Y)
      BYP=PSIY(L-1)-DBLE(L)*PSIY(L)/Y
C PSI'(MTX)
      BMTXP=PSIMTX(L-1)-DBLE(L)*PSIMTX(L)/CMTX
C PSI'(MX)
      BMXP=PSIMX(L-1)-DBLE(L)*PSIMX(L)/CMX
C PSI'(MY)
      BMYP=PSIMY(L-1)-DBLE(L)*PSIMY(L)/CMY
C CHI'(Y)
      CHIYP=-CHIY1-DBLE(L)*CHIY0/Y
C CHI'(MTY)
      CMTYP=-CMTY1-DBLE(L)*CMTY0/CMTY
C CHI'(MY)
      CMYP=-CMY1-DBLE(L)*CMY0/CMY
C CHI'(MTX)
      CMTXP=-CMTX1-DBLE(L)*CMTX0/CMTX
C PSI_W'(MTX)
      CALL BESSEL(V1-1.,V2,ZMTX1,ZMTX2,BWMTXP)
      BWMTXP=CKMTX*CDEXP(BWMTXP)+(.5D0-CW)*BWMTX/CMTX
C PSI_W'(MTY)
      CALL BESSEL(V1-1.,V2,ZMTY1,ZMTY2,BWMTYP)
      BWMTYP=CKMTY*CDEXP(BWMTYP)+(.5D0-CW)*BWMTY/CMTY
C CHI_W'(MTX)
      CALL BESSEL(-V1-1.,-V2,ZMTX1,ZMTX2,CWMTXP)
      ALX=CDLOG(-CKMTX)+CWMTXP
      ALY=CDLOG((.5D0+CW))+CWMTX-CDLOG(CMTX)
      CWMTXP=LADD(ALX,ALY,1)
C CHI_W'(MTY)
      CALL BESSEL(-V1-1.,-V2,ZMTY1,ZMTY2,CWMTYP)
      CWMTYP=-CKMTY*CDEXP(CWMTYP)+(.5D0+CW)*CWMTY/CMTY
      LL=-LL
C COMPUTE ZETA(Y) AND ZETA(MY) AND THEIR DERIVATIVES.
      CZY=PSIY(L)+CHIY0*DCMPLX(0.D0,1.D0)*LL
C      CZMY=PSIMY(L)+CMY0*DCMPLX(0.D0,1.D0)*LL
      CZYP=BYP+CHIYP*DCMPLX(0.D0,1.D0)*LL
C      CZMYP=BMYP+CMYP*DCMPLX(0.D0,1.D0)*LL
C CALCULATE WRONSKIANS OF PSI, CHI AND ZETA AND THEIR MIXTURES.
C WITH CORRECTIONS (INCLUDING ERRATA J. OPTICAL SOC. AM. V66 N9 P981)
      CPPXP=M*BWMTXP*PSIMX(L)-MT*BWMTX*BMXP
      CCPQXP=CWMTYP*PSIY(L)-MT*CWMTY*BYP
      IF(PSIMX(L).EQ.DCMPLX(0.D0,0.D0).OR.BMXP.EQ.DCMPLX(0.D0,0.D0))
     $THEN
      CCPXP=DCMPLX(0.D0,0.D0)
      ELSE
      ALX=CDLOG(M)+CWMTXP+CDLOG(PSIMX(L))
      ALY=CDLOG(MT)+CWMTX+CDLOG(BMXP)
      CCPXP=CDEXP(LADD(ALX,ALY,-1))
      ENDIF
      CPPQXP=BWMTYP*PSIY(L)-MT*BWMTY*BYP
      CCZQXP=CWMTYP*CZY-MT*CWMTY*CZYP
      CPZQXP=BWMTYP*CZY-MT*BWMTY*CZYP
      CPPXD=MT*BMTXP*PSIMX(L)-M*PSIMTX(L)*BMXP
      CCPQXD=MT*CMTYP*PSIY(L)-CMTY0*BYP
      CCPXD=MT*CMTXP*PSIMX(L)-M*CMTX0*BMXP
      CPPQXD=MT*BMTYP*PSIY(L)-PSIMTY(L)*BYP
      CCZQXD=MT*CMTYP*CZY-CMTY0*CZYP
      CPZQXD=MT*BMTYP*CZY-PSIMTY(L)*CZYP
C CALCULATE THE ANISOTROPIC MIE COEFFICIENTS.
      IF(CPPXP.EQ.DCMPLX(0.D0,0.D0).AND.CCPXP.EQ.DCMPLX(0.D0,0.D0)) THEN
      CRAT=DCMPLX(0.D0,0.D0)
      ELSE
      CRAT=CPPXP/CCPXP
      ENDIF
      IF(CPPXD.EQ.DCMPLX(0.D0,0.D0).AND.CCPXD.EQ.DCMPLX(0.D0,0.D0)) THEN
      CRAT1=DCMPLX(0.D0,0.D0)
      ELSE
      CRAT1=CPPXD/CCPXD
      ENDIF
      AN(L)=(CRAT*CCPQXP-CPPQXP)/(CRAT*CCZQXP-CPZQXP)
      BN(L)=(CRAT1*CCPQXD-CPPQXD)/(CRAT1*CCZQXD-CPZQXD)
C CALCULATE THE EFFICIENCY FACTORS.
      QSCA=QSCA+(2.*DBLE(L)+1.D0)*(CDABS(AN(L))**2+CDABS(BN(L))**2)
      QEXT=QEXT+(2.*DBLE(L)+1.D0)*(DBLE(AN(L))+DBLE(BN(L)))
      IF(L-LY) 100,200,100
  200 QSCA=(2.D0/(Y*Y))*QSCA
      QEXT=(2.D0/(Y*Y))*QEXT
      QALPHA=4.71238898037/Y*QEXT
      DO 101 J=0,NANG
      RAD=3.1415926535897939/180.D0*(RANGLEL+DBLE(J)*RDELTA)
      XL=DCOS(RAD)
      YL=DSIN(RAD)
      PI1=1.D0
      PI2=3.D0*XL
      PP1=0.D0
      PP2=3.D0
      T2=XL*PI2-YL*YL*3.D0
C AND CALCULATE PHASE FUNCTION (CPHA1 AND CPHA2) FOR BOTH
C POLARIZATION STATES AND THE AVERAGE QPH3
      CPHA1=1.5D0*(AN(1)+BN(1)*XL)+5.D0/6.D0*(AN(2)*PI2+BN(2)*T2)
      CPHA2=1.5D0*(BN(1)+AN(1)*XL)+5.D0/6.D0*(BN(2)*PI2+AN(2)*T2)
      I=2
    3 IF (I.LT.Y) GOTO4
      IF (I.GE.LY) GOTO75
      IF (DBLE(AN(I)).LE.1.D-17) GOTO75
    4 I=I+1
      PI1=DBLE(2*I-1)/DBLE(I-1)*XL*PI2-DBLE(I)/DBLE(I-1)*PI1
      PP1=DBLE(2*I-1)*PI2+PP1
      TAU=XL*PI1-YL*YL*PP1
      CPHA1=CPHA1+DBLE(2*I+1)/DBLE(I*(I+1))*(AN(I)*PI1+BN(I)*TAU)
      CPHA2=CPHA2+DBLE(2*I+1)/DBLE(I*(I+1))*(AN(I)*TAU+BN(I)*PI1)
      PTMP=PI1
      PI1=PI2
      PI2=PTMP
      PTMP=PP1
      PP1=PP2
      PP2=PTMP
      GOTO3
   75 QPH1=4.D0/Y**2*CDABS(CPHA1)**2
      QPH2=4.D0/Y**2*CDABS(CPHA2)**2
      QPH3=(QPH1*IPARA+QPH2*IPERP)*FACT
  101 RI(J)=QPH3/12.566371/QSCA
      RETURN
      END
      FUNCTION LADD(ALX,ALY,IS)
      COMPLEX*16 LADD,ALX,ALY
      ILX=DINT(DBLE(ALX))
      ALY=ALY-ILX
      ALX=ALX-ILX
      IF(CDABS(ALY).LT.42.D0) THEN
      LADD=CDLOG(CDEXP(ALX)+IS*CDEXP(ALY))+ILX
      ELSE
      IF(DBLE(ALY).GT.DBLE(ALX)) THEN
      LADD=ALY+ILX
      ELSE
      LADD=ALX+ILX
      ENDIF
      ENDIF
      END
C SUBROUTINE TO CALCULATE BESSEL FUNCTION OF COMPLEX ORDER AND
C ARGUMENT.
      SUBROUTINE BESSEL(V1X,V2,Z3,Z4,LJV)
      REAL*8 V1,V2,Z1,Z2,V3,PI,Z3,Z4,CS2,V1X
      COMPLEX*16 V,Z,JV,RNUM,RDEN,T,RAT,CHI,MU,P,Q,T1,T2,T3,Z64,CS,LJV
      SAVE
c		print *, 'routine BESSEL'
      V1=V1X
      LJV=DCMPLX(0.D0,0.D0)
      IF(V2.NE.0..OR.DINT(V1).NE.V1.OR.V1.GE.0.) GOTO3
      Z3=-Z3
      V1=-V1X
    3 Z1=DABS(Z3)
      Z2=DABS(Z4)
      V=CMPLX(V1,V2)
      Z=CMPLX(Z1,Z2)
      PI=3.1415926535897932D0
      IF(Z1.EQ.0..AND.Z2.EQ.0.) GOTO50
      IF (CDABS(V*V/Z).GT.10..OR.CDABS(Z).LE.15.) GOTO60
C BESSEL FUNCTION OF FIRST KIND COMPLEX ORDER AND ARGUMENT
C FROM A+S ASYMPTOTIC EXPANSIONS 9.2.5
      K=INT(CDABS(V*V/Z))+5
      Z64=64*Z**2
      CHI=Z-(.5D0*V+.25D0)*PI
      MU=4.D0*V*V
      P=DCMPLX(1.D0,0.D0)
      Q=(MU-1.D0)/8/Z
      T3=DCMPLX(1.D0,0.D0)
      T2=(MU-1)/8/Z
      DO 70 I=1,K
      T1=(MU-(4*I-3)**2)*(MU-(4*I-1)**2)/(2*I-1)/(2*I)/Z64*(-T3)
      T3=T1
      T2=(MU-(4*I-1)**2)*(MU-(4*I+1)**2)/(2*I)/(2*I+1)/Z64*(-T2)
      P=P+T1
      Q=Q+T2
   70 CONTINUE
      IF(Z2.GT.345.) GOTO30
      JV=(2/PI/Z)**.5*(P*CDCOS(CHI)-Q*CDSIN(CHI))
      LJV=CDLOG(JV)
      GOTO15
   30 CONTINUE
      CS2=(.25+.5*V1)*PI-Z1
      CS2=CS2-2.*PI*DINT((CS2+DSIGN(PI,CS2))/2./PI)
      CS=DCMPLX(Z2-.5*V2*PI-DLOG(2.D0),CS2)
      LJV=.5*CDLOG(2./PI/Z)+CDLOG(P)+CS+CDLOG((1.D0-Q/P*
     $DCMPLX(0.D0,1.D0)))
      GOTO15
C USE BACKWARD RECURRANCE AND NEUMANN EXPANSION FOR NORMALIZATION
C MAKING SURE THAT ROUTINE IS IN A SAFE REGION.
   60 N=3
      IVT=INT(CDABS(V))+10
      V3=V1
      IF(V1.LT.0.) V3=V1-INT(V1)
      IMAG=.01368804*Z2*Z2+22.*(1.-1.826*EXP(-Z2*Z2/1200.))+.5
      IREAL=.02653561*Z2*Z2-17.6*(1.-EXP(-Z2/21.8))+.5
      IF(V2.LT.IMAG.AND.V1.LT.IREAL) V3=IREAL+V3-INT(V3)+2.
      IF(V3.EQ.0.AND.V2.EQ.0.) V3=V3+1.
   20 CALL CBESSEL(V3,V2,Z1,Z2,LJV,ERR)
C	  WRITE(*,*) LJV,ERR
      IF(ERR.GT.10.0) GOTO10
      V3=V3+IVT
      GOTO20
   10 IDIFF=V3-V1+.1
C IF BACKWARD RECURRANCE WAS NOT INTIALLY IN A GOOD REGION THEN
C USE CONTINUED FRACTIONS TO GET THERE.
      IF(IDIFF.EQ.0) GOTO15
      DO 40 I=1,IDIFF
      V=CMPLX(V3,V2)-I+1
      N=3
      RNUM=2.*V/Z
      RDEN=-2.*(V+1.)/Z
      RAT=RNUM
      RNUM=RDEN+1./RNUM
      RAT=RAT*RNUM/RDEN
   18 T=(-1)**(N+1)*2.*(V+N-1)/Z
      RNUM=T+1./RNUM
      RDEN=T+1./RDEN
      IF(CDABS(RNUM-RDEN).LT.1.E-13) GOTO100
      N=N+1
      RAT=RAT*RNUM/RDEN
      GOTO18
  100 CONTINUE
C     WRITE(*,*) LJV,RAT
   40 LJV=LJV+CDLOG(RAT)
   15 IF(Z1.GT.Z3.AND.Z2.GT.Z4) LJV=LJV+DCMPLX(V1,V2)*PI*
     $DCMPLX(0.D0,1.D0)
      IF(Z1.GT.Z3.AND.Z2.EQ.Z4) LJV=DCONJG(LJV+DCMPLX(V1,V2)
     $*PI*DCMPLX(0.D0,1.D0))
      IF(Z1.EQ.Z3.AND.Z2.GT.Z4) LJV=DCONJG(LJV)
C     WRITE(*,*) LJV
C     IF(ABS(DBLE(LJV)).LT.345.) WRITE(*,*) CDEXP(LJV)
      RETURN
   50 IF(V1.NE.0..OR.V2.NE.0) GOTO55
C     WRITE(*,*) (1.D0,0.D0)
      LJV=0.
      RETURN
C  55 WRITE(*,*) (0.D0,0.D0)
   55 LJV=-1.D150
      RETURN
      END
C SUBROUTINE FOR CALCULATING BESSEL FUNCTIONS OF COMPLEX
C ORDER AND ARGUMENT.  BACKWARD RECURSION AND GENERALIZED
C NEUMANN EXPANSIONS (A+S 9.1.87).  ERR RETURNS ACCURACY
C ESTIMATE IF LESS THAN 10 DIGITS.
      SUBROUTINE CBESSEL(V1,V2,Z1,Z2,JV,ERR)
C     REAL*8 GAM1,V1,V2,Z1,Z2,V3,V4,PI,X,Y,YY,SUMM,ERR
      IMPLICIT REAL*8 (A-H,O-Z)
	  REAL*4 ERR
      COMPLEX*16 V,Z,JV,JV1,JV2,CN,SUM,GAM,T1,T2
	  SAVE
c		print *, 'routine CBESSEL'
      ERR=100.
      PI=3.141592653589793238D0
      V=DCMPLX(V1,V2)
      Z=DCMPLX(Z1,Z2)
      Y=CDABS(V)
      X=CDABS(Z)
C L IS NUMBER OF TERMS IN NEUMANN EXPANSION REQUIRED FOR CONVERGENCE
C (DERIVED EMPIRICALLY).
      L=2*DINT((X+5.5*DLOG10(X)+3.*DSQRT(X)+8.5)/2.)
      IF (L.LT.12) L=12
      IF (Y.GT.5) GOTO30
      V3=DABS(V1+L/2)
      V4=DABS(V2)
      CN=DCMPLX(V3,V4)
      JV1=DCMPLX(0.D0,0.D0)
      JV2=DCMPLX(1.D0,0.D0)
      GAM=(CN-.5D0)*CDLOG(CN)-CN+.5D0*DLOG(6.2831853071795865D0)+
     $1.D0/12/CN-1.D0/360/CN**3+1.D0/1260/CN**5-1.D0/1680/CN**7+
     $1.D0/1188/CN**9-.001917526917D0/CN**11+1.D0/156/CN**13
      GAM1=0.D0
      DO 5 I=1,L/2
    5 GAM1=GAM1+DLOG(DBLE(I))
      IF(V3.GT.V1+L/2) GAM=DCMPLX(0.D0,PI)+DLOG(PI)-GAM-CDLOG(
     $CN*CDSIN(PI*CN))
      IF(V3.GT.V1+L/2.AND.V4.EQ.V2) GAM=DCONJG(GAM)
      IF(V3.EQ.V1+L/2.AND.V4.GT.V2) GAM=DCONJG(GAM)
      T2=DCMPLX(1.D0,0.D0)
      SUM=(V+L)*T2
      CN=V+L+1
      DO 10 I=1,L
      T1=2*(CN-I)/Z
      JV=T1*JV2-JV1
      JV1=JV2
      JV2=JV
      IF (((L-I)/2)*2.NE.(L-I)) GOTO 10
      K=(L-I)/2
      T2=T2*(K+1)/(V+K)
      SUM=SUM+(V+L-I)*JV*T2
   10 CONTINUE
      T2=V*CDLOG(.5*Z)-GAM+GAM1
      JV=CDLOG(JV)-CDLOG(SUM)+T2
      GOTO25
   30 V3=DABS(V1)
      V4=DABS(V2)
      CN=DCMPLX(V3,V4)
      JV1=DCMPLX(0.D0,0.D0)
      JV2=DCMPLX(1.D-20,0.D0)
      GAM=(CN-.5D0)*CDLOG(CN)-CN+.5D0*DLOG(6.2831853071795865D0)+
     $1.D0/12/CN-1.D0/360/CN**3+1.D0/1260/CN**5-1.D0/1680/CN**7
      IF(CDABS(CN).GT.100.) GOTO1
      GAM=GAM+1.D0/1188/CN**9-.001917526917D0/CN**11+1.D0/156/CN**13
    1 IF(V3.GT.V1) GAM=DCMPLX(0.D0,PI)+DLOG(PI)-GAM-CDLOG(CN*
     $CDSIN(PI*CN))
      IF (V3.GT.V1.AND.V4.EQ.V2) GAM=DCONJG(GAM)
      IF (V3.EQ.V1.AND.V4.GT.V2) GAM=DCONJG(GAM)
      SUM=(V+L)*1.E-20
      CN=V+L+1
      FAC=0.
      SUMM=-1.D10
      IFAC=0
      DO 15 I=1,L
      T1=2*(CN-I)/Z
      JV=T1*JV2-JV1
      JV1=JV2
      JV2=JV
      IF (((L-I)/2)*2.NE.(L-I)) GOTO15
      IF (CDABS(JV).GT.1.E-32) GOTO2
      FAC=FAC+1.
      JV1=JV1*1.E30
      JV2=JV2*1.E30
      JV=JV*1.E30
    2 K=(L-I)/2
      T2=(K+1)/(V+K)
      JV1=T2*JV1
      JV2=T2*JV2
      IF (FAC.NE.0.) GOTO15
      SUM=SUM+(V+L-I)*JV*T2
      YY=DLOG(CDABS(SUM))+IFAC*69.077553
      IF(YY-SUMM.LT.-16.118) ERR=17.D0+DLOG10(DEXP(YY-SUMM))
      IF(YY.GT.SUMM) SUMM=YY
      IF (CDABS(SUM).LT.1.E30) GOTO15
      IFAC=IFAC+1
      SUM=SUM*1.E-30
      JV1=JV1*1.E-30
      JV2=JV2*1.E-30
      JV=JV*1.E-30
   15 CONTINUE
C  15 WRITE(*,*) SUM,CN-I,JV
      T2=V*CDLOG(.5*Z)
      JV=(CDLOG(JV/V)-GAM-CDLOG(SUM)+T2-FAC*69.0775527898213D0)
   25 CONTINUE
      RETURN
      END
C INTEGRATES CYLINDER PHASE FUNCTION OVER PARTICLE SIZE DISTRIBUTION
C USING SIMPSON'S METHOD
C BY BLAIR EVANS/DREV
C JANUARY 1988
      SUBROUTINE CYLPHASE(M)
      REAL*4 RI(0:18000),SUMIC(0:18000)
      REAL*8 PSD(5002),DUMR8(20504)
      COMPLEX*16 M
      COMMON /PHASE/ RI
      COMMON /DATA/ ANGLEL,ANGLEU,DELTA,NANG,ILOW,IUP,RINC,ANGLEU1
      COMMON /POLAR/ IPARA,IPERP
	  COMMON /GLOBALR4/ SUMIC
      COMMON /GLOBALR8/ PSD,DUMR8
      SAVE
c		print *, 'routine CYLPHASE'
      PI=3.141592653
      IFLAG=0
      IDIFF=IUP-ILOW
      IF(IDIFF/2*2.EQ.IDIFF) GOTO10
      IUP=IUP+1
   10 WRITE(*,1000)
 1000 FORMAT(1X,'ORIENTATION ANGLE (0=RANDOM) ?')
      READ(*,*) PHIX
      IF(PHIX.NE.0.) GOTO9
      IFLAG=1
      WRITE(*,1100)
      WRITE(*,1101)
      WRITE(*,1102)
      WRITE(*,1103)
 1100 FORMAT(1X,'WHICH POLARIZATION STATE ? 1) PARALLEL')
 1101 FORMAT(1X,'                           2) PERPENDICULAR')
 1102 FORMAT(1X,'                        OR 3) RANDOM')
 1103 FORMAT(1X,'WITH RESPECT TO SCATTERING PLANE')
      READ(*,*) ANS
      IF(ANS.NE.2.) IPARA=1
      IF(ANS.NE.1.) IPERP=1
    9 PHI=PHIX/180.*3.141592653
      SUMNS=PSD(ILOW)*ILOW*RINC
      SUMN=PSD(ILOW)
      RR=ILOW*RINC
      SUMA=SUMNS*RR
      IF(IFLAG.EQ.0) GOTO1
      CALL RANDC(RR,M,QEXT,QSCA,QALPHA)
      GOTO11
    1 CALL CYLINDERPHASE(RR,M,PHI,QEXT,QSCA,QALPHA)
   11 QEXT1=QEXT*SUMNS
      QSCA1=QSCA*SUMNS
      DO 2001 I=0,NANG
 2001 SUMIC(I)=RI(I)*SUMNS
      DO 2000 I=ILOW+1,IUP,2
      RR=I*RINC
      R1=RR+RINC
      PROD1=4.*PSD(I)*RR
      PROD2=2.*PSD(I+1)*R1
      SUMNS=SUMNS+PROD1+PROD2
      SUMN=SUMN+4.*PSD(I)+2.*PSD(I+1)
      SUMA=SUMA+PROD1*RR+PROD2*R1
      IF(IFLAG.EQ.0) GOTO2
      CALL RANDC(RR,M,QEXT,QSCA,QALPHA)
      GOTO12
    2 CALL CYLINDERPHASE(RR,M,PHI,QEXT,QSCA,QALPHA)
   12 QEXT1=QEXT1+QEXT*PROD1
      QSCA1=QSCA1+QSCA*PROD1
      DO 2002 II=0,NANG
 2002 SUMIC(II)=SUMIC(II)+RI(II)*PROD1
      IF(IFLAG.EQ.0) GOTO3
      CALL RANDC(R1,M,QEXT,QSCA,QALPHA)
      GOTO13
    3 CALL CYLINDERPHASE(R1,M,PHI,QEXT,QSCA,QALPHA)
   13 QEXT1=QEXT1+QEXT*PROD2
      QSCA1=QSCA1+QSCA*PROD2
      DO 2003 II=0,NANG
 2003 SUMIC(II)=SUMIC(II)+RI(II)*PROD2
 2000 CONTINUE
      SUMNS=SUMNS-PROD2
      SUMN=SUMN-PSD(IUP)
      SUMA=SUMA-PROD2*RR
      QEXT1=QEXT1-QEXT*PROD2
      QSCA1=QSCA1-QSCA*PROD2
      IF(SUMNS.NE.0.) GOTO15
      WRITE(*,1310)
      STOP
   15 QEXT2=QEXT1/SUMNS
      QSCA2=QSCA1/SUMNS
      SUMA=QEXT1*4./SUMA
      QEXT1=QEXT1/SUMN/PI
      QSCA1=QSCA1/SUMN/PI
      IF(IFLAG.EQ.0) WRITE(*,1201) 2.*PHIX
 1201 FORMAT(1X,'PHASE FUNCTION ON CONE OF APEX ANGLE',1X,F6.2,1X,
     $'DEG.')
      DO 2004 II=0,NANG
      SUMIC(II)=(SUMIC(II)-RI(II)*PROD2)/SUMNS
 2004 WRITE(7,1200) ANGLEL+FLOAT(II)*DELTA,SUMIC(II)
 1200 FORMAT(1X,F6.2,1X,G12.5)
      WRITE(7,1304) QEXT2,QSCA2,QEXT2-QSCA2
 1304 FORMAT(1X,'COMP EXT EFF=',G12.6,1X,'COMP SCA EFF=',G12.6,1X,
     $'COMP ABS EFF=',G12.6)
      IF(QEXT2.GT.0.) GOTO21
      WRITE(7,1305) 0.
      GOTO22
   21 IF(ANGLEU1.EQ.180.AND.PHIX.EQ.0.) WRITE(7,1305) SUMIC(NANG)/QEXT2
      IF(ANGLEU1.EQ.180.AND.PHIX.EQ.90.) WRITE(7,1305) SUMIC(NANG)/QEXT2
   22 WRITE(7,1300) QEXT1
 1300 FORMAT(1X,'EXT/PART=',G12.6,'*LAMBDA')
      WRITE(7,1301) QSCA1
 1301 FORMAT(1X,'SCA/PART=',G12.6,'*LAMBDA')
      WRITE(7,1302) QEXT1-QSCA1
 1302 FORMAT(1X,'ABS/PART=',G12.6,'*LAMBDA')
      WRITE(7,1303) SUMA
 1303 FORMAT(1X,'MASS EXT=',G12.6,'/LAMBDA/DENSITY')
 1305 FORMAT(1X,'LIDAR RATIO=',G12.6)
 1310 FORMAT(1X,'THERE ARE NO PARTICLES IN THIS SIZE RANGE')
      STOP
      END
C INFINITE CYLINDER PHASE FUNCTION PROGRAM (FROM KERKER 1969)
C BY BLAIR EVANS/DREV
C JANUARY 1988
      SUBROUTINE CYLINDERPHASE(X,M,PHI,QEXT,QSCA,QALPHA)
      REAL*4 RI(0:18000),JL(1000),Y(1000),JLP
      COMPLEX*8 SUM11,SUM22,SUM12,JJPL,CNUM,CDEN,CRAT,T
      COMPLEX*8 JJ(6000),Z2,MS,CJ,C1,C2,C3,C4,C5,C6,C7,CSUMA
      COMPLEX*8 TA1,Q1,Q2,Q3,Q4,JJP,CSUMB,T1,T2
      COMPLEX*8 A1(1001),A2(1001),B1(1001)
      COMPLEX*16 M,CALPHA,DELT,H,HP
      COMMON /PHASE/ RI
      COMMON /DATA/ ANGLEL,ANGLEU,DELTA,NANG,ILOW,IUP,RINC,ANGLEU1
C      COMMON /GLOBALR4/ JL,Y,DUMR4
      COMMON /GLOBALC8/ A1,A2,B1
      SAVE
c		print *, 'routine CYLINDERPHASE'
      IFLAG=0
      PHI=1.570796326-PHI
      PI=3.141592653
      CH=SIN(PHI)
      CL=COS(PHI)
      MS=M*M
      CJ=CSQRT(MS-CMPLX(CH**2,0.))
      Z1=X*CL
      Z2=X*CJ
      IF (REAL(CMPLX(0.,1.)*Z2).GT.200.) IFLAG=1
C CALCULATE BESSEL FUNCTION OF FIRST KIND FOR ALL ORDER AT Z1
      NSTOP=Z1+4.*Z1**.333333+6.
      NDELTA=(101.+Z1)**.499
      MST=NSTOP+NDELTA
      MST=(MST/2)*2
      IF(Z1.GT.0.0003) GOTO5
      F=1
      DO 11 L=1,15
      JL(L)=(Z1/2.)**(L-1)/F
   11 F=F*L
      GOTO6
    5 JL(MST+1)=0.0
      JL(MST)=1.E-32
      M1=MST-1
      DO 10 L=1,M1
      ML=MST-L
   10 JL(ML)=2.*FLOAT(ML)*JL(ML+1)/Z1-JL(ML+2)
      ALPHA=JL(1)
      M2=MST-2
      DO 20 L=2,M2,2
   20 ALPHA=ALPHA+2.*JL(L+1)
      DO 30 L=1,M1
   30 JL(L)=JL(L)/ALPHA
C CALCULATE BESSEL FUNCTION OF SECOND KIND FOR ALL ORDERS AT Z1
    6 Y(1)=JL(1)*(ALOG(Z1/2.)+.5772156649)
      M4=MST/2-1
      DO 40 L=1,M4
   40 Y(1)=Y(1)-2.*((-1)**L)*JL(2*L+1)/FLOAT(L)
      Y(1)=.6366197724*Y(1)
      Y(2)=JL(2)*Y(1)-.6366197724/Z1
      Y(2)=Y(2)/JL(1)
      NS=NSTOP-1
      DO 50 L=1,NS
   50 Y(L+2)=2.*FLOAT(L)*Y(L+1)/Z1-Y(L)
      ZZ=CABS(Z2)
      NSTOP=ZZ+4.*ZZ**.333333+4.
      NDELTA=(101.+ZZ)**.499
      MST=NSTOP+NDELTA
      MST=(MST/2)*2
      JJ(MST+1)=0.0
      JJ(MST)=1.0E-32
      M1=MST-1
      IF (IFLAG.EQ.0) GOTO60
C CALCULATE BESSEL FUNCTION OF FIRST KIND FOR ALL ORDERS AT Z2
      DO 105 L=1,M1
      CNUM=2.*L/Z2
      CDEN=-2.*(L+1.)/Z2
      CRAT=CNUM
      CNUM=CDEN+1./CNUM
      CRAT=CRAT*CNUM/CDEN
      N=3
   18 T=(-1)**(N+1)*2.*(L+N-1)/Z2
      CNUM=T+1./CNUM
      CDEN=T+1./CDEN
      N=N+1
      CRAT=CRAT*CNUM/CDEN
      IF(CABS(CNUM-CDEN).GT.1.E-14) GOTO18
  105 JJ(L)=1./(CRAT-L/Z2)
      JJPL=CMPLX(1.,0.)
      GOTO70
   60 CONTINUE
      DO 110 L=1,M1
      ML=MST-L
  110 JJ(ML)=2.*FLOAT(ML)*JJ(ML+1)/Z2-JJ(ML+2)
      CALPHA=JJ(1)
      M2=MST-2
      DO 120 L=2,M2,2
  120 CALPHA=CALPHA+2.*JJ(L+1)
      CALPHA=1.D0/CALPHA
      DO 130 L=1,M1
  130 JJ(L)=JJ(L)*CALPHA
   70 C7=CMPLX(0.,CH/X)
      C3=CJ*CJ
      C4=CMPLX(CL*CL,0.)
      C5=M*CJ
      C6=C3/M
      CSUMA=CMPLX(0.,0.)
      CSUMB=CMPLX(0.,0.)
      CSSCAA=0.
      CSSCAB=0.
      NS=MIN(NS,M1)
      DO 200 L=NS,1,-1
      H=CMPLX(JL(L),-Y(L))
      JJP=JJ(L)*FLOAT(L-1)/Z2-JJ(L+1)
      IF (IFLAG.EQ.1) JJP=JJPL
      JLP=JL(L)*FLOAT(L-1)/Z1-JL(L+1)
      C1=C7*FLOAT(L-1)
      C2=C1/M
      YP=Y(L)*FLOAT(L-1)/Z1-Y(L+1)
      HP=CMPLX(JLP,-YP)
      Q1=(C6*JJ(L)*CL*HP-C5*JJP*C4*H)
      Q2=C2*(1-MS)*JJ(L)*H
      DELT=C3*JJ(L)*(C1*H*Q2-CL*HP*Q1)-C4*H*(C1*JJ(L)*Q2-CJ*JJP*Q1)
      TA1=FLOAT(L-1)*CH*CL*CJ*CJ*(M*M-CMPLX(1.,0.))/M/X*JJ(L)**2
      A1(L)=TA1*2./(PI*Z1*DELT)
      TA1=C3*JJ(L)*(-CL*JLP*Q1+C1*JL(L)*Q2)
      A2(L)=(TA1+C4*JL(L)*(CJ*JJP*Q1-C1*JJ(L)*Q2))/DELT
      Q3=C6*JJ(L)*CL*JLP-C5*JJP*C4*JL(L)
      Q4=C2*JJ(L)*C4*JL(L)-C6*JJ(L)*C1*JL(L)
      TA1=C3*JJ(L)*(C1*H*Q4-CL*HP*Q3)-C4*H*(C1*JJ(L)*Q4-CJ*JJP*Q3)
      B1(L)=TA1/DELT
      CSSCAA=CSSCAA+CABS(A1(L))**2+CABS(B1(L))**2
      CSSCAB=CSSCAB+CABS(A2(L))**2+CABS(A1(L))**2
      CSUMB=CSUMB+B1(L)
  200 CSUMA=CSUMA+A2(L)
      T1=2.*CSUMB-B1(1)
      T2=2.*CSUMA-A2(1)
      T3=2.*(CSSCAA-CABS(A1(1))**2)-CABS(B1(1))**2
      T4=2.*(CSSCAB-CABS(A1(1))**2)-CABS(A2(1))**2
      QSCA1=2.*T3/X
      QSCA2=2.*T4/X
      QEXT1=2.*REAL(T1)/X
      QEXT2=2.*REAL(T2)/X
      QEXT=.5*(QEXT1+QEXT2)
      QSCA=.5*(QSCA1+QSCA2)
      QALPHA=4.*QEXT*CL/X
      DO 450 I=0,NANG
      RAD=PI/180.*(ANGLEL+FLOAT(I)*DELTA)
      SUM11=CMPLX(0.,0.)
      SUM12=CMPLX(0.,0.)
      SUM22=CMPLX(0.,0.)
      DO 400 L=1,NS
      XL=COS((L-1)*RAD)
      YL=SIN((L-1)*RAD)
      SUM11=SUM11+B1(L)*XL
      SUM12=SUM12+A1(L)*YL
  400 SUM22=SUM22+A2(L)*XL
      SUM11=2.*SUM11-B1(1)
      SUM12=2.*SUM12
      SUM22=2.*SUM22-A2(1)
C PHASE FUNCTION FOR RANDOM POLARIZATION
      RI(I)=1./PI/PI/X/QSCA*(.5*(CABS(SUM11)**2+2.*CABS(SUM12)**2+
     $CABS(SUM22)**2))
  450 CONTINUE
      PHI=1.570796326-PHI
      RETURN
      END
C RANDOM ORIENTATION (VARIATION OF COHEN ET AL 1985)
C INFINITE CYLINDER PHASE FUNCTION PROGRAM (FROM KERKER 1969)
C BY BLAIR EVANS/DREV
C JANUARY 1988
      SUBROUTINE RANDC(X,M,QEXT,QSCA,QALPHA)
      REAL*4 RI(0:18000),JL(1000),Y(1000),JLP
      REAL*8 DRATIO
      COMPLEX*8 SUM11,SUM22,SUM12,JJPL,CNUM,CDEN,CRAT,T
      COMPLEX*8 JJ(6000),Z2,MS,CJ,C1,C2,C3,C4,C5,C6,C7,CSUMA
      COMPLEX*8 TA1,Q1,Q2,Q3,Q4,JJP,CSUMB,T1,T2
      COMPLEX*8 A1(1001),A2(1001),B1(1001)
      COMPLEX*16 M,CALPHA,DELT,H,HP
      COMMON /PHASE/ RI
      COMMON /DATA/ ANGLEL,ANGLEU,DELTA,NANG,ILOW,IUP,RINC,ANGLEU1
      COMMON /POLAR/ IPARA,IPERP
C      COMMON /GLOBALR4/ JL,Y,DUMR4
      COMMON /GLOBALC8/ A1,A2,B1
      SAVE
c		print *, 'routine RANDC'
      IF(IPARA.EQ.0.AND.IPERP.EQ.0) GOTO1
      FACT=1./(IPARA+IPERP)
    1 PI=3.141592653
      PI2=PI/2.
      RAD1=PI/180.
      N=360
      IFLAG1=1
      IF(DELTA.LT.0.5) N=180/DELTA
      TWOH=180./N
      H2=TWOH/2.
      CONST=3./(H2*RAD1*2.)
      QEXT1=0.
      QEXT2=0.
      QSCA1=0.
      QSCA2=0.
      IANGL=ANGLEL*500.+.9
      IANGU=ANGLEU*500.+.9
      IDELTA=DELTA*500.+.9
      IPHIP=IANGL
      DO 100 I=IANGL,IANGU,IDELTA
  100 RI(I/5)=0.
      DO 500 IPHI=1,N+1
      DO 510 IA=1,2
      WA=IA
      IF(IPHI.EQ.1.AND.IA.EQ.1) GOTO510
      IF(IPHI.EQ.N+1.AND.IA.EQ.1) WA=.5
      IF(IPHI.EQ.N+1.AND.IA.EQ.2) GOTO510
      IPHI1=((IPHI-1)*TWOH+(IA-1)*H2)*500.+.1
      PHI=PI2-((IPHI-1)/2.*TWOH+(IA-1)*H2/2.)*RAD1
      IFLAG=0
      CH=SIN(PHI)
      CL=COS(PHI)
      MS=M*M
      CJ=CSQRT(MS-CMPLX(CH**2,0.))
      Z1=X*CL
      Z2=X*CJ
      IF (REAL(CMPLX(0.,1.)*Z2).GT.200.) IFLAG=1
      NSTOP=Z1+4.*Z1**.333333+6.
      NDELTA=(101.+Z1)**.499
      MST=NSTOP+NDELTA
      MST=(MST/2)*2
      IF(Z1.GT.0.0003) GOTO5
      F=1.
      DO 11 L=1,15
      JL(L)=(Z1/2.)**(L-1)/F
   11 F=F*L
      GOTO6
    5 JL(MST+1)=0.0
      JL(MST)=1.E-32
      M1=MST-1
      DO 10 L=1,M1
      ML=MST-L
   10 JL(ML)=2.*FLOAT(ML)*JL(ML+1)/Z1-JL(ML+2)
      ALPHA=JL(1)
      M2=MST-2
      DO 20 L=2,M2,2
   20 ALPHA=ALPHA+2.*JL(L+1)
      DO 30 L=1,M1
   30 JL(L)=JL(L)/ALPHA
    6 Y(1)=JL(1)*(ALOG(Z1/2.)+.5772156649)
      M4=MST/2-1
      DO 40 L=1,M4
   40 Y(1)=Y(1)-2.*((-1)**L)*JL(2*L+1)/FLOAT(L)
      Y(1)=.6366197724*Y(1)
      Y(2)=JL(2)*Y(1)-.6366197724/Z1
      Y(2)=Y(2)/JL(1)
      NS=NSTOP-1
      DO 50 L=1,NS
   50 Y(L+2)=2.*FLOAT(L)*Y(L+1)/Z1-Y(L)
      ZZ=CABS(Z2)
      NSTOP=ZZ+4.*ZZ**.333333+4.
      NDELTA=(101.+ZZ)**.499
      MST=NSTOP+NDELTA
      MST=(MST/2)*2
      JJ(MST+1)=0.0
      JJ(MST)=1.0E-32
      M1=MST-1
      IF (IFLAG.EQ.0) GOTO60
      DO 105 L=1,M1
      CNUM=2.*L/Z2
      CDEN=-2.*(L+1.)/Z2
      CRAT=CNUM
      CNUM=CDEN+1./CNUM
      CRAT=CRAT*CNUM/CDEN
      N=3
   18 T=(-1)**(N+1)*2.*(L+N-1)/Z2
      CNUM=T+1./CNUM
      CDEN=T+1./CDEN
      N=N+1
      CRAT=CRAT*CNUM/CDEN
      IF(CABS(CNUM-CDEN).GT.1.E-14) GOTO18
  105 JJ(L)=1./(CRAT-L/Z2)
      JJPL=CMPLX(1.,0.)
      GOTO70
   60 CONTINUE
      DO 110 L=1,M1
      ML=MST-L
  110 JJ(ML)=2.*FLOAT(ML)*JJ(ML+1)/Z2-JJ(ML+2)
      CALPHA=JJ(1)
      M2=MST-2
      DO 120 L=2,M2,2
  120 CALPHA=CALPHA+2.*JJ(L+1)
      CALPHA=1.D0/CALPHA
      DO 130 L=1,M1
  130 JJ(L)=JJ(L)*CALPHA
   70 C7=CMPLX(0.,CH/X)
      C3=CJ*CJ
      C4=CMPLX(CL*CL,0.)
      C5=M*CJ
      C6=C3/M
      CSUMA=CMPLX(0.,0.)
      CSUMB=CMPLX(0.,0.)
C      CSSCAA=CMPLX(0.,0.)
C      CSSCAB=CMPLX(0.,0.)
      CSSCAA=0.0
      CSSCAB=0.0
      DO 200 L=NS,1,-1
      H=CMPLX(JL(L),-Y(L))
      JJP=JJ(L)*FLOAT(L-1)/Z2-JJ(L+1)
      IF (IFLAG.EQ.1) JJP=JJPL
      JLP=JL(L)*FLOAT(L-1)/Z1-JL(L+1)
      C1=C7*FLOAT(L-1)
      C2=C1/M
      YP=Y(L)*FLOAT(L-1)/Z1-Y(L+1)
      HP=CMPLX(JLP,-YP)
      Q1=(C6*JJ(L)*CL*HP-C5*JJP*C4*H)
      Q2=C2*(1-MS)*JJ(L)*H
      DELT=C3*JJ(L)*(C1*H*Q2-CL*HP*Q1)-C4*H*(C1*JJ(L)*Q2-CJ*JJP*Q1)
      TA1=FLOAT(L-1)*CH*CL*CJ*CJ*(M*M-CMPLX(1.,0.))/M/X*JJ(L)**2
      A1(L)=TA1*2./(PI*Z1*DELT)
      TA1=C3*JJ(L)*(-CL*JLP*Q1+C1*JL(L)*Q2)
      A2(L)=(TA1+C4*JL(L)*(CJ*JJP*Q1-C1*JJ(L)*Q2))/DELT
      Q3=C6*JJ(L)*CL*JLP-C5*JJP*C4*JL(L)
      Q4=C2*JJ(L)*C4*JL(L)-C6*JJ(L)*C1*JL(L)
      TA1=C3*JJ(L)*(C1*H*Q4-CL*HP*Q3)-C4*H*(C1*JJ(L)*Q4-CJ*JJP*Q3)
      B1(L)=TA1/DELT
      CSSCAA=CSSCAA+CABS(A1(L))**2+CABS(B1(L))**2
      CSSCAB=CSSCAB+CABS(A2(L))**2+CABS(A1(L))**2
      CSUMB=CSUMB+B1(L)
C  200 CSUMA=CSUMA+A2(L)
      CSUMA=CSUMA+A2(L)
  200 CONTINUE
      T1=2.*CSUMB-B1(1)
      T2=2.*CSUMA-A2(1)
      T3=2.*(CSSCAA-CABS(A1(1))**2)-CABS(B1(1))**2
      T4=2.*(CSSCAB-CABS(A1(1))**2)-CABS(A2(1))**2
      QSCA1=QSCA1+WA*T3/X/CONST
      QSCA2=QSCA2+WA*T4/X/CONST
      QEXT1=QEXT1+WA*REAL(T1)/X/CONST
      QEXT2=QEXT2+WA*REAL(T2)/X/CONST
      IF(IPHI1.LT.IANGL) GOTO510
C LOOP OVER PHASE FUNCTION ANGLES TRANSFORMING FROM INFINITE
C CYLINDER SCATTERING CONE TO SCATTERING ANGLES
      QSCA=(T3+T4)/X
  310 IF(IPHI1.LT.IPHIP+IDELTA.OR.IPHIP.EQ.IANGU) GOTO300
      IPHIP=IPHIP+IDELTA
      IFLAG1=1
      GOTO310
  300 DO 450 I=IANGL,IPHIP,IDELTA
      IF(IFLAG1.EQ.0.OR.I.NE.IPHIP) GOTO305
      IFLAG1=0
      WA=1
      RAD=PI
      IF(IPHI.NE.(N+1)) GOTO302
  301 F=PI*CONST
      GOTO303
  302 RDD=H2/2.*RAD1+RAD1*I/1000.
      F=DATAN((DSIN(DBLE(RDD+RAD1*I/1000.))*DSIN(DBLE(H2/2.*RAD1)))**.5
     1/DCOS(DBLE(RDD)))*CONST
      GOTO303
  305 DRATIO=DSIN(DBLE(RAD1*I/1000.))/CL
      RAD=2.*DASIN(DRATIO)
      F=1./COS(RAD/2.)
  303 SUM11=CMPLX(0.,0.)
      SUM12=CMPLX(0.,0.)
      SUM22=CMPLX(0.,0.)
      DO 400 L=1,NS
      XL=COS((L-1)*RAD)
      YL=SIN((L-1)*RAD)
      SUM11=SUM11+B1(L)*XL
      SUM12=SUM12+A1(L)*YL
  400 SUM22=SUM22+A2(L)*XL
      SUM11=2.*SUM11-B1(1)
      SUM12=2.*SUM12
      SUM22=2.*SUM22-A2(1)
C PHASE FUNCTION
      RI(I/5)=RI(I/5)+WA*F/PI/PI/X*((CABS(SUM11)**2*IPARA+CABS(SUM12)
     $**2*(IPARA+IPERP)+CABS(SUM22)**2*IPERP)*FACT)/QSCA
  450 CONTINUE
  510 CONTINUE
  500 CONTINUE
      QEXT=FACT*(QEXT1*IPARA+QEXT2*IPERP)
      QSCA=FACT*(QSCA1*IPARA+QSCA2*IPERP)
      DO 600 I=0,NANG
      J=(IANGL+I*IDELTA)/5
  600 RI(I)=RI(J)/PI/CONST
      QALPHA=PI*QEXT/X
      PHI=1.570796326-PHI
      RETURN
      END
C INTEGRATES FIBER PHASE FUNCTION OVER PARTICLE SIZE DISTRIBUTION
C USING SIMPSON'S METHOD
C BY BLAIR EVANS/DREV
C JANUARY 1988
      SUBROUTINE FIBERPHASE(A,M)
      REAL*4 RI(0:18000),SUMIF(0:18000)
      REAL*8 PSD(5002),DUMR8(20504)
      COMPLEX*8 Z
      COMPLEX*16 M
      COMMON /PHASE/ RI
      COMMON /DATA/ ANGLEL,ANGLEU,DELTA,NANG,ILOW,IUP,RINC,ANGLEU1
      COMMON /INIT/ Z,EP,EDP,PHI,Q
      COMMON /GLOBALR4/ SUMIF
      COMMON /GLOBALR8/ PSD,DUMR8
      SAVE
c		print *, 'routine FIBERPHASE'
      PI=3.141592653
      IFLAG=0
      IDIFF=IUP-ILOW
      IF(IDIFF/2*2.EQ.IDIFF) GOTO10
      IUP=IUP+1
   10 WRITE(*,1000)
 1000 FORMAT(1X,'ORIENTATION ANGLE (0.=RANDOM) ?')
      READ(*,*) PHIX
      IF(PHIX.EQ.0.) IFLAG=1
      PHI=PHIX/180.*3.141592653
      CALL SETUP(A,M)
      SUMNS=PSD(ILOW)*ILOW*RINC
      SUMN=PSD(ILOW)
      RR=ILOW*RINC
      SUMA=SUMNS*RR
      IF(IFLAG.EQ.0) GOTO1
      CALL RANDF(RR,A,QEXT,QSCA,QALPHA)
      GOTO11
    1 CALL FIBER(RR,A,M,QEXT,QSCA,QALPHA)
   11 QEXT1=QEXT*SUMNS
      QSCA1=QSCA*SUMNS
      DO 2001 I=0,NANG
 2001 SUMIF(I)=RI(I)*SUMNS
      DO 2000 I=ILOW+1,IUP,2
      RR=I*RINC
      R1=RR+RINC
      PROD1=4.*PSD(I)*RR
      PROD2=2.*PSD(I+1)*R1
      SUMNS=SUMNS+PROD1+PROD2
      SUMN=SUMN+4.*PSD(I)+2.*PSD(I+1)
      SUMA=SUMA+PROD1*RR+PROD2*R1
      IF(IFLAG.EQ.0) GOTO2
      CALL RANDF(RR,A,QEXT,QSCA,QALPHA)
      GOTO12
    2 CALL FIBER(RR,A,M,QEXT,QSCA,QALPHA)
   12 QEXT1=QEXT1+QEXT*PROD1
      QSCA1=QSCA1+QSCA*PROD1
      DO 2002 II=0,NANG
 2002 SUMIF(II)=SUMIF(II)+RI(II)*PROD1
      IF(IFLAG.EQ.0) GOTO3
      CALL RANDF(R1,A,QEXT,QSCA,QALPHA)
      GOTO13
    3 CALL FIBER(R1,A,M,QEXT,QSCA,QALPHA)
   13 QEXT1=QEXT1+QEXT*PROD2
      QSCA1=QSCA1+QSCA*PROD2
      DO 2003 II=0,NANG
 2003 SUMIF(II)=SUMIF(II)+RI(II)*PROD2
 2000 CONTINUE
      SUMNS=SUMNS-PROD2
      SUMN=SUMN-PSD(IUP)
      SUMA=SUMA-PROD2*RR
      QEXT1=QEXT1-QEXT*PROD2
      QSCA1=QSCA1-QSCA*PROD2
      IF(SUMNS.GT.0.) GOTO14
      WRITE(*,1307)
      STOP
   14 QEXT2=QEXT1/SUMNS
      QSCA2=QSCA1/SUMNS
      SUMA=QEXT2/A
      IF(IFLAG.EQ.0) SUMA=SUMA*2.*SIN(PHI)
      QEXT1=QEXT1/SUMN/PI
      QSCA1=QSCA1/SUMN/PI
      IF(IFLAG.EQ.0) WRITE(*,1306)
 1306 FORMAT(1X,'PHASE FUNCTION FOR THETA, PHI=0. DEG. FIBER CO-ORDS.')
      DO 2004 II=0,NANG
      SUMIF(II)=(SUMIF(II)-RI(II)*PROD2)/SUMNS
 2004 WRITE(7,1200) ANGLEL+FLOAT(II)*DELTA,SUMIF(II)
 1200 FORMAT(1X,F6.2,1X,G12.5)
      WRITE(7,1304) QEXT2,QSCA2,QEXT2-QSCA2
 1304 FORMAT(1X,'COMP EXT EFF=',G15.8,1X,'COMP SCA EFF=',G15.8,1X,
     $'COMP ABS EFF=',G15.8)
      IF(QEXT2.GT.0.) GOTO 21
      WRITE(7,1305) 0.
      GOTO22
   21 IF(ANGLEU1.EQ.180.AND.IFLAG.EQ.1) WRITE(7,1305) SUMIF(NANG)/QEXT2*
     $QSCA2
      IF(IFLAG.EQ.0.AND.MOD((PHIX-ANGLEL)/DELTA,1.).EQ.0.) WRITE(7,1305)
     $ SUMIF(INT((PHIX-ANGLEL)/DELTA))*QSCA2/QEXT2
   22 WRITE(7,1300) QEXT1
 1300 FORMAT(1X,'EXT/PART=',G15.8,'*LAMBDA')
      WRITE(7,1301) QSCA1
 1301 FORMAT(1X,'SCA/PART=',G15.8,'*LAMBDA')
      WRITE(7,1302) QEXT1-QSCA1
 1302 FORMAT(1X,'ABS/PART=',G15.8,'*LAMBDA')
      WRITE(7,1303) SUMA
 1303 FORMAT(1X,'MASS EXT=',G15.8,'/LAMBDA/DENSITY')
 1305 FORMAT(1X,'LIDAR RATIO=',G15.8)
 1307 FORMAT(1X,'THERE ARE NO PARTICLES IN THIS SIZE RANGE')
      STOP
      END
C FINITE CYLINDER PROGRAM FOR CALCULATING THE PHASE FUNCTION AND
C EFFIENCIES.
C BY BLAIR EVANS/DREV
C JANAURY 1988
      SUBROUTINE FIBER(Y,A,M,QEXT,QSCA,QALPHA)
      REAL*4 RI(0:18000)
      COMPLEX*8 Z,GS,GAMC,GAMS,CN,LC,LS,CT1,LP,LN,L4,CTC2,CTS2
      COMPLEX*8 CTC3,CTS3,TMP,TMN,GC,RLAM,RK,ATERM,U11,U12,U22,CXX
      COMPLEX*16 M
      COMMON /PHASE/ RI
      COMMON /DATA/ ANGLEL,ANGLEU,DELTA,NANG,ILOW,IUP,RINC,ANGLEU1
      COMMON /INIT/ Z,EP,EDP,PHI,Q
C PRECOMPUTE AND STORE SOME CONSTANTS
      SAVE
c		print *, 'routine FIBER'
      X=Y/2.
      IF(Q.GE.1) RETURN
      PI=3.141592653
      PI4=12.56637061
      PI2=9.869604397
      RLN4=1.386294361
      RAD1=90./180.*PI
      IF(X.LE..4.AND.A.LE..4) GOTO300
      IF(X.LE.20..AND.X.GE..75.AND.A.LE..5.AND.X/A.GT.5.) GOTO100
      IF(X.LE.5..OR.A.GT.10..OR.X/A.LT.1.5) GOTO999
C LONG CYLINDERS WITH RADIUS SMALL OR OF ORDER OF WAVELENGTH
C APPROXIMATION DUE TO VAN DE HULST (1957) CH. 15.22
      ANGLELT=ANGLEL
      ANGLEUT=ANGLEU
      NANGT=NANG
      ANGLEL=0.
      ANGLEU=0.
      NANG=0
      PHI=ACOS(Q)
      CALL CYLINDERPHASE(A,M,PHI,QEXT,QSCA,QALPHA)
      CONST=RI(0)
      ANGLEL=ANGLELT
      ANGLEU=ANGLEUT
      NANG=NANGT
      DO 200 I=0,NANG
      RAD=.0174532925*(ANGLEL+FLOAT(I)*DELTA)
      P=COS(RAD)
      IF(ABS(P+Q).GT.1.E-7) GOTO250
      F=1.
      GOTO200
  250 F=(SIN(X*(Q+P))/(X*(Q+P)))**2
  200 RI(I)=CONST*F
      RETURN
C RAYLEIGH SPHERIOD APPROXIMATION (RUCK ET AL 1970)
  300 E=X/A
      CXX=E/SQRT(E*E*SQ*SQ+Q*Q)
      IF(ABS(E-1.).LT.1.E-4) E=1.0001
      IF(E.LT.1) GOTO350
C ASPECT RATIO >1, PROLATE CYLINDER
      E1=E*E-1.
      E12=E1**.5
      RLOG=LOG((E+E12)/(E-E12))
      RIA=1./E1*(.5*E/E12*RLOG-1.)
      RIB=.5*(1.-RIA)
      CN=1./(RIA+1./(CMPLX(EP,EDP)-1.))/3.
      U11=1./(RIB+1./(CMPLX(EP,EDP)-1.))/3.
      SQ=(1.-Q*Q)**.5
      DO 310 I=0,NANG
      RAD=.0174532925*(ANGLEL+FLOAT(I)*DELTA)
      P=COS(RAD)
      SRAD=SIN(RAD)
      U22=SQ*SRAD*CN+Q*P*U11
  310 RI(I)=(CABS(U11)**2 +CABS(U22)**2)*CXX*4./(PI*3.)
C SINCE THE RAYLEIGH APPROXIMATION IS USED MUST FIRST CALCULATE
C ABSORPTION AND SCATTERING EFFIENCIES TO GET EXTINCTION
C (VAN DE HULST 1957)
      QABS=2.*A*CXX*REAL(CMPLX(0.,-1.)*(SQ*SQ*CN+(1.+Q*Q)*U11))
      QSCA=A**4*4.*E*CXX*(CABS(SQ*SQ*CN)**2+CABS((1.+Q*Q)*U11)**2)/3.
      QEXT=QSCA+QABS
      QALPHA=1.5*PI*(SQ*SQ/A+X/(A*A)*Q*Q)*QEXT
      RETURN
C ASPECT RATIO < 1, OBLATE CYLINDER
  350 E=A/X
      E1=E*E-1.
      E12=E1**.5
      RLOG=ASIN(E12/E)
      RIA=.5/E1*(E*E/E12*RLOG-1.)
      RIB=.5*(1.-RIA)
      SQ=(1.-Q*Q)**.5
      CN=1./(RIA+1./(CMPLX(EP,EDP)-1.))/3.
      U11=1./(RIB+1./(CMPLX(EP,EDP)-1.))/3.
      DO 360 I=0,NANG
      RAD=.01745329251*(ANGLEL+FLOAT(I)*DELTA)
      P=COS(RAD)
      SRAD=SIN(RAD)
      U22=SQ*SRAD*CN+P*Q*U11
  360 RI(I)=(CABS(U11)**2+CABS(U22)**2)*CXX*4./(PI*3.)
      QABS=2.*A*CXX*REAL(CMPLX(0.,-1.)*(SQ*SQ*CN+(1.+Q*Q)*U11))
      QSCA=A**4*4./E*CXX*(CABS(SQ*SQ*CN)**2+CABS((1.+Q*Q)*U11)**2)/3.
      QEXT=QSCA+QABS
      QALPHA=1.5*PI*(SQ*SQ/A+X/(A*A)*Q*Q)*QEXT
      RETURN
C ANTENNA APPROXIMATION
C FIRST ORDER VARIATIONAL APPROACH FROM TAI 1952, CASSEDY AND FAINBERG
C 1960 AND PEDERSON ET AL 1983-1985
  100 NAG=NANG
      LOW=0
      X2=2.*X
      SX=SIN(X)
      S2X=SIN(X2)
      SX2=SX*SX
      CX=COS(X)
      C2X=COS(X2)
      CX2=CX*CX
      ZZ=4.*X
      CALL SC(ZZ,CI,SI)
      L4=CMPLX(-CI,SI)
      IF (PHI.NE.RAD1) GOTO 5
C BROADSIDE INCIDENCE WITH NON-ZERO END CURRENT MODIFICATION
      G1=2.*SX
      RLAM=2./A-CMPLX(C2X,S2X)/X
      U11=-RLAM*CX2-CMPLX(0.,1.)*L4
      ZZ=X2
      CALL SC(ZZ,CI,SI)
      LN=CMPLX(-CI,SI)
      ATERM=RLN4+2.*LOG(X2/A)-2.*LN
      U12=-RLAM*CX+ATERM*SX
      U22=-RLAM+X2*ATERM+CMPLX(0.,1.)*2.*(1.-C2X)-2.*S2X
      H1=X+.5*S2X
      RK=(G1*U12-X2*U11-PI4*Z*CMPLX(0.,4.*SX2-H1*X2))/(G1*U22-X2*U12)
      GC=G1-RK*X2
      LC=PI4*Z*CMPLX(0.,1.)*(H1-2.*RK*G1+RK*RK*X2)
      GAMC=U11-2.*RK*U12+RK*RK*U22
      CTS2=CMPLX(0.,0.)
      CTC2=GC/(GAMC-LC)
      SQX=0.
      CQX=1.
      GOTO80
C NON-BROADSIDE ORIENTATIONS
C PRE-CALCULATE MORE CONSTANTS
    5 QX=Q*X
      QX2=2.*QX
      SQX=SIN(QX)
      SQX2=SQX*SQX
      S2QX=SIN(QX2)
      CQX=COS(QX)
      CQX2=CQX*CQX
      C2QX=COS(QX2)
      SS2QX=S2X*S2QX
      Q2N=(1.-Q*Q)
      XCX=Q2N*X
      Q2P=.5*(1./Q+Q)
      SQ2P=Q2P*S2QX
      CQ2P=Q2P*C2QX
C CALCULATE CENTRAL INTEGRALS
      GS=CMPLX(0.,-(2.*SQX2*CX-SQ2P*SX+XCX*SX)/SQRT(Q2N))
      IF(CABS(GS).LT.1.E-6) GS=CMPLX(0.,0.)
      GC=(2.*CQX2*SX-SQ2P*CX-XCX*CX)/SQRT(Q2N)
      T=CQX2*(X+.5*S2X)-2.*(S2X*CQX2-Q*S2QX*CX2)/Q2N+CX2*(QX+.5*S2QX)/Q
      LC=PI4*Z*CMPLX(0.,T)
      T=SQX2*(X-.5*S2X)+2.*(S2X*SQX2-Q*S2QX*SX2)/Q2N+SX2*(QX-.5*S2QX)/Q
      LS=PI4*Z*CMPLX(0.,T)
      CT1=CMPLX(-1.+C2X*C2QX+Q*SS2QX,Q*C2X*S2QX-S2X*C2QX)
      ZZ=X2+QX2
      CALL SC(ZZ,CI,SI)
      LP=CMPLX(-CI,SI)
      ZZ=X2-QX2
      CALL SC(ZZ,CI,SI)
      LN=CMPLX(-CI,SI)
      TMN=LP-LN
      SS2QX2=SS2QX*.5
      Q2NX=Q2N*X
      TMP=1.38629436+2.*LOG(X2/A)-LP-LN
      CTC2=(CQ2P*CX2+SS2QX2)*TMN
      CTS2=(CQ2P*SX2-SS2QX2)*TMN
      CTC3=(CX2*(SQ2P+Q2NX)-S2X*CQX2)*TMP
      CTS3=(-SX2*(SQ2P-Q2NX)+S2X*SQX2)*TMP
      GAMC=CX2*CT1-CTC2+CMPLX(0.,1.)*CTC3+CQX2*L4
      GAMS=SX2*CT1+CTS2+CMPLX(0.,1.)*CTS3+SQX2*L4
      CTS2=GS/(GAMS*CMPLX(0.,-1.)-LS)
      Q2=Q*Q
      CTC2=GC/(GAMC*CMPLX(0.,-1.)-LC)
C COMPUTE PHASE FUNCTION IN FIBER CO-ORDINATES
   80 IF(ANGLEL.NE.0.) GOTO71
      LOW=1
      RI(0)=0.
   71 IF(ANGLEU.NE.180.) GOTO70
      NAG=NANG-1
      RI(NANG)=0.
   70 DO 60 I=LOW,NAG
      RAD=.01745329251*(ANGLEL+FLOAT(I)*DELTA)
      P=COS(RAD)
      IF(ABS(P-Q).GT.1.E-7) GOTO65
      RI(I)=(CABS(GC*CTC2+GS*CTS2))**2/4./PI2
      GOTO60
   65 IF(ABS(P+Q).GT.1.E-7) GOTO66
      RI(I)=(CABS(GC*CTC2-GS*CTS2))**2/4./PI2
      GOTO60
   66 PX=P*X
      P2=P*P
      PS=SIN(RAD)
      PS2=PS*PS
      SPX=SIN(PX)
      CPX=COS(PX)
      W=CQX/PS2*(SX*CPX-P*CX*SPX)-CX/(Q2-P2)*(Q*SQX*CPX-P*CQX*SPX)
      V=SQX/PS2*(P*SX*CPX-CX*SPX)-SX/(Q2-P2)*(P*SQX*CPX-Q*CQX*SPX)
   61 RI(I)=(4.*PS2*(CABS(CTC2*W+CMPLX(0.,1.)*CTS2*V))**2)/4./PI2
   60 CONTINUE
      QEXT=REAL(CMPLX(0.,-1.)*(GC*CTC2-GS*CTS2))/PI
      QSCA=REAL(Z)/ABS(Z)/PI
      QSCA=QEXT-QSCA*((ABS(CTC2))**2*ABS(LC)+(ABS(CTS2))**2*ABS(LS))
      QEXT=PI2*QEXT/A/X/SIN(PHI)
      QSCA=PI2*QSCA/A/X/SIN(PHI)
      QALPHA=QEXT*(4.*SIN(PHI)/A+PI*COS(PHI)/X)
      DO 90 I=LOW,NANG
   90 RI(I)=RI(I)*2.*PI2/A/X/SIN(PHI)/QSCA/2.
      RETURN
  999 WRITE(*,1000)
 1000 FORMAT(1X,'SORRY, THERE ARE NO GOOD APPROXIMATIONS FOR THIS CASE')
      STOP
      END
C RANDOM ORIENTATION
C FINITE CYLINDER PROGRAM FOR CALCULATING THE PHASE FUNCTION AND
C EFFIENCIES.  USES FIRST ORDER VARIATIONAL APPROACH FROM TAI 1952
C AND N.E. PEDERSEN ET AL 1983-1985
C BY BLAIR EVANS/DREV
C JANUARY 1988
      SUBROUTINE RANDF(Y,A,QEXT,QSCA,QALPHA)
      REAL*4 RI(0:18000)
      COMPLEX*8 Z,GS,GAMC,GAMS,LC,LS,CT1,LP,LN,L4,CTC2,CTS2
      COMPLEX*8 CTC3,CTS3,TMP,TMN,GC,G,GA,W,V,AI0,AI1,RATIO,U1
      COMPLEX*8 RLAM,ATERM,U11,U12
      COMMON /PHASE/ RI
      COMMON /DATA/ ANGLEL,ANGLEU,DELTA,NANG,ILOW,IUP,RINC,ANGLEU1
      COMMON /INIT/ Z,EP,EDP,PHI,Q
C PRECOMPUTE AND STORE SOME CONSTANTS
      SAVE
c		print *, 'routine RANDF'
      X=Y/2.
C CHOOSE N,THE INTEGRATION STEP, SO INTEGRATES PEAKS ON SCATTERING CONE
C ACCURATELY
      N=3.*X+10
      PI=3.141592653
      PI4=12.56637061
      PI2=9.869604397
      RLN4=1.386294361
      RAD1=.5*PI
      TWOH=180./N
      H=TWOH/2.
      X2=2.*X
      SX=SIN(X)
      S2X=SIN(X2)
      SX2=SX*SX
      CX=COS(X)
      C2X=COS(X2)
      CX2=CX*CX
      ZZ=4.*X
      CALL SC(ZZ,CI,SI)
      L4=CMPLX(-CI,SI)
      DO 500 I=0,NANG
  500 RI(I)=0.
      QEXT=0.
      QSCA=0.
C LOOP OVER FIBER ORIENTATION ANGLES (SIMPSON'S INTEGRATION)
      IBETAU=N
      IALPHAU=2*N+1
      DO 100 IBETA=1,IBETAU
      DO 150 IB=1,2
      WB=IB
      IF(IBETA.EQ.1.AND.IB.EQ.1) GOTO150
      PHI=.0174532925*((IBETA-1)*TWOH+(IB-1)*H)
      Q=COS(PHI)
      Q2=Q*Q
      SP=SIN(PHI)
C COMPUTE SURFACE IMPEDANCE
      GA=(CMPLX(Q2-EP,EDP))**.5
      G=GA
      GA=GA*A
      U=CABS(GA)
      V=CMPLX(1.,0.)
      AI0=V
      AI1=V
C COMPUTE RATIO OF MODIFIED BESSEL FUNCTIONS
      IF(U.GT.7.5) GOTO15
      W=(GA/2)**2
      LIMIT=4.*SQRT(U)+1.5
      DO 11 I=1,LIMIT
      V=V*W/FLOAT(I*I)
      AI0=AI0+V
   11 AI1=AI1+V/FLOAT(I+1)
      AI1=AI1*GA/2.
      GOTO25
   15 W=V
      XX=8.*GA
      LIMIT=50./LOG(U)**2+1
      DO 20 I=1,LIMIT
      U1=1./(XX*FLOAT(I))
      U2=FLOAT((2*I-1)**2)
      W=W*U2*U1
      V=V*(U2-4.)*U1
      AI0=AI0+W
   20 AI1=AI1+V
   25 RATIO=AI0/AI1
      Z=.15915494*G*RATIO/(A*CMPLX(EDP,EP))
      IF (PHI.NE.RAD1) GOTO 5
C BROADSIDE INCIDENCE WITH ZERO END CURRENT CONDITION
      G1=2.*SX
      RLAM=2./A-CMPLX(C2X,S2X)/X
      U11=-RLAM*CX2-CMPLX(0.,1.)*L4
      ZZ=X2
      CALL SC(ZZ,CI,SI)
      LN=CMPLX(-CI,SI)
      ATERM=RLN4+2.*LOG(X2/A)-2.*LN
      U12=-RLAM*CX+ATERM*SX
      U22=-RLAM+X2*ATERM+CMPLX(0.,1.)*2.*(1.-C2X)-2.*S2X
      H1=X+.5*S2X
      GC=G1-CX*X2
      LC=PI4*Z*CMPLX(0.,1.)*(H1-2.*CX*G1+CX2*X2)
      GAMC=U11-2.*CX*U12+CX2*U22
      CTS2=CMPLX(0.,0.)
      CTC2=GC/(GAMC-LC)
      SQX=0.
      CQX=1.
      QSQX=0.
      QCQX=0.
      GOTO80
C NON-BROADSIDE ORIENTATIONS
C PRE-CALCULATE MORE CONSTANTS
    5 QX=Q*X
      QX2=2.*QX
      SQX=SIN(QX)
      QSQX=Q*SQX
      SQX2=SQX*SQX
      S2QX=SIN(QX2)
      CQX=COS(QX)
      QCQX=Q*CQX
      CQX2=CQX*CQX
      C2QX=COS(QX2)
      SS2QX=S2X*S2QX
      Q2N=(1.-Q*Q)
      IF(Q2N.EQ.0.) Q2N=SP*SP
      XCX=Q2N*X
      Q2P=.5*(1./Q+Q)
      SQ2P=Q2P*S2QX
      CQ2P=Q2P*C2QX
C CALCULATE CENTRAL INTEGRALS
      GS=CMPLX(0.,-(2.*SQX2*CX-SQ2P*SX+XCX*SX)/SQRT(Q2N))
      IF(CABS(GS).LT.1.E-6) GS=CMPLX(0.,0.)
      GC=(2.*CQX2*SX-SQ2P*CX-XCX*CX)/SQRT(Q2N)
      T=CQX2*(X+.5*S2X)-2.*(S2X*CQX2-Q*S2QX*CX2)/Q2N+CX2*(QX+.5*S2QX)/Q
      LC=PI4*Z*CMPLX(0.,T)
      T=SQX2*(X-.5*S2X)+2.*(S2X*SQX2-Q*S2QX*SX2)/Q2N+SX2*(QX-.5*S2QX)/Q
      LS=PI4*Z*CMPLX(0.,T)
      CT1=CMPLX(-1.+C2X*C2QX+Q*SS2QX,Q*C2X*S2QX-S2X*C2QX)
      ZZ=X2+QX2
      CALL SC(ZZ,CI,SI)
      LP=CMPLX(-CI,SI)
      ZZ=X2-QX2
      CALL SC(ZZ,CI,SI)
      LN=CMPLX(-CI,SI)
      TMN=LP-LN
      SS2QX2=SS2QX*.5
      Q2NX=Q2N*X
      TMP=1.38629436+2.*LOG(X2/A)-LP-LN
      CTC2=(CQ2P*CX2+SS2QX2)*TMN
      CTS2=(CQ2P*SX2-SS2QX2)*TMN
      CTC3=(CX2*(SQ2P+Q2NX)-S2X*CQX2)*TMP
      CTS3=(-SX2*(SQ2P-Q2NX)+S2X*SQX2)*TMP
      GAMC=CX2*CT1-CTC2+CMPLX(0.,1.)*CTC3+CQX2*L4
      GAMS=SX2*CT1+CTS2+CMPLX(0.,1.)*CTS3+SQX2*L4
      CTS2=GS/(GAMS*CMPLX(0.,-1.)-LS)
      CTC2=GC/(GAMC*CMPLX(0.,-1.)-LC)
C LOOP OVER PHASE FUNCTION ANGLES INTEGRATING OVER AZIMUTHAL ANGLES
   80 DO 200 ITHETA=0,NANG
      RADTH=PI-.0174532925*(ANGLEL+FLOAT(ITHETA)*DELTA)
      SRTH=SIN(RADTH)
      CRTH=COS(RADTH)
  210 SUMA=0.
      DO 300 IALPHA=1,IALPHAU
      DO 350 IA=1,2
      WA=IA
      IF(IALPHA.EQ.1.AND.IA.EQ.1) WA=.5
      IF(IALPHA.EQ.IALPHAU.AND.IA.EQ.1) WA=.5
      IF(IALPHA.EQ.IALPHAU.AND.IA.EQ.2) GOTO350
      RADAL=.0174532925*((IALPHA-1)*TWOH+(IA-1)*H)
      P=SRTH*SP*COS(RADAL)+CRTH*Q
      IF(P.GE.1..OR.P.LE.-1.) GOTO350
      IF(ABS(P-Q).GT.1.E-7) GOTO65
      SUMA=SUMA+WA*(CABS(GC*CTC2+GS*CTS2))**2/4.
      GOTO350
   65 IF(ABS(P+Q).GT.1.E-7) GOTO66
      SUMA=SUMA+WA*(CABS(GC*CTC2-GS*CTS2))**2/4.
      GOTO350
   66 PX=P*X
      P2=P*P
      PS=(1.-P2)**.5
      PS2=PS*PS
      SPX=SIN(PX)
      CPX=COS(PX)
      W=CQX/PS2*(SX*CPX-P*CX*SPX)-CX/(Q2-P2)*(QSQX*CPX-P*CQX*SPX)
      V=SQX/PS2*(P*SX*CPX-CX*SPX)-SX/(Q2-P2)*(P*SQX*CPX-QCQX*SPX)
      SUMA=SUMA+WA*(PS2*(CABS(CTC2*W+CMPLX(0.,1.)*CTS2*V))**2)
  350 CONTINUE
  300 CONTINUE
  200 RI(ITHETA)=RI(ITHETA)+WB*(2.*SUMA)*H*PI/180./3./PI2
      QEXT=QEXT+WB*REAL(CMPLX(0.,-1.)*(GC*CTC2-GS*CTS2))/PI*H/1080.*4.
      QS=WB*(2.*REAL(Z)/ABS(Z)/PI/PI4)*H*PI/180./3.*4.
      QSCA=QSCA+QS*((ABS(CTC2))**2*ABS(LC)+(ABS(CTS2))**2*ABS(LS))
  150 CONTINUE
  100 CONTINUE
      QSCA=QEXT-QSCA
      QEXT=4.*PI*QEXT/A/X
      QSCA=4.*PI*QSCA/A/X
      QALPHA=QEXT*PI*(1./A+.5/X)
      IF(QSCA.GT.0.) GOTO610
      QSCA=1.E-6*QEXT
      WRITE(*,1000)
 1000 FORMAT(1X,'**** WARNING: PHASE FUNCTION NOT NORMALIZED SINCE QSCA
     $ IS UNRELIABLE ****')
  610 DO 600 I=0,NANG
  600 RI(I)=2.*RI(I)*H/3./720.*4./QSCA*4*PI/A/(X+.5*A)/2./PI
      RETURN
      END
C SUBROUTINE TO ADJUST REFRACTIVE INDEX IF NECESSARY AND CALCULATE
C SURFACE IMPEDANCE FOR FINITE CYLINDER PROGRAM
C BY BLAIR EVANS/DREV
C JANUARY 1988
      SUBROUTINE SETUP(A,M)
      COMPLEX*8 Z,G,GA,W,V,U1,X,AI0,AI1,RATIO
      COMPLEX*16 M
      COMMON /INIT/ Z,EP,EDP,PHI,Q
	  SAVE
c		print *, 'routine SETUP'
      RDFACTOR=1.
      Q=COS(PHI)
      REM=DBLE(M)
      RIM=DBLE(CMPLX(0.,1.)*M)
      EP=REM**2-RIM**2
      EDP=2.*REM*RIM
      WRITE(*,1013)
 1013 FORMAT(1X,'IS CYLINDER RADIUS ~<.6 MICRONS ? (0=YES)')
      READ(*,*) ANS
      IF(ANS.NE.0.) GOTO50
      WRITE(*,1007)
 1007 FORMAT(1X,'WAVELENGTH ?')
      READ(*,*) RLAM
      WRITE(*,1012)
 1012 FORMAT(1X,'WILL ADJUST REFRACTIVE INDEX WITH THE FOLLOWING
     $ INFORMATION:',/)
      WRITE(*,1002)
 1002 FORMAT(1X,'ENTER BULK CONDUCTIVITY IN mho/m')
      READ(*,*) SIGMA
      WRITE(*,1004)
 1004 FORMAT(1X,'ENTER BULK DENSITY IN g/m**3')
      READ(*,*) RHO
      WRITE(*,1005)
 1005 FORMAT(1X,'ENTER MOLECULAR WEIGHT')
      READ(*,*) RMW
      WRITE(*,1006)
 1006 FORMAT(1X,'ENTER NUMBER OF CONDUCTION ELECTRONS PER MOLECULE')
      READ(*,*) C
      RN=RHO/RMW
      RLAMBDA=1.7822477E-12*SIGMA*(1./RN/C)**(2./3.)
      RDFACTOR=1.-.1875*6.2831853/RLAM*RLAMBDA/A
      IF(RDFACTOR.GT.0.) GOTO10
      WRITE(*,1010) A*RLAM*1.591549E9
 1010 FORMAT(1X,'RADIUS=',F9.2,1X,'ANGSTROMS AND IS TOO SMALL FOR')
      WRITE(*,1000)
 1000 FORMAT(1X,'             THIS CORRECTION')
      STOP
   10 EP=1.-RDFACTOR*(1-REM**2+RIM**2)
      EDP=RDFACTOR*2.*RIM*REM
      REM1=SQRT((EP+SQRT(EP**2+EDP**2))/2.)
      ROOT=(-EP+SQRT(EP**2+EDP**2))*.5
      IF(ROOT.GT.0.) GOTO30
      RIM1=0.
      GOTO35
   30 RIM1=SQRT(ROOT)
   35 M=CMPLX(REM1,-RIM1)
   50 GA=(CMPLX(Q**2-EP,EDP))**.5
      G=GA
      GA=GA*A
      U=CABS(GA)
      V=CMPLX(1.,0.)
      AI0=V
      AI1=V
      IF(U.GT.7.5) GOTO15
      W=(GA/2)**2
      LIMIT=4.*SQRT(U)+1.5
      DO 11 I=1,LIMIT
      V=V*W/FLOAT(I*I)
      AI0=AI0+V
   11 AI1=AI1+V/FLOAT(I+1)
      AI1=AI1*GA/2.
      GOTO25
   15 W=V
      X=8.*GA
      LIMIT=50./LOG(U)**2+1
      DO 20 I=1,LIMIT
      U1=1./(X*FLOAT(I))
      U2=FLOAT((2*I-1)**2)
      W=W*U2*U1
      V=V*(U2-4.)*U1
      AI0=AI0+W
   20 AI1=AI1+V
   25 RATIO=AI0/AI1
      Z=.15915494*G*RATIO/(A*CMPLX(EDP,EP))
      RETURN
      END
C CALCULATE SINE AND COSINE INTEGRALS FOLLOWING ABRAMOWITZ AND STEGUN
C BY BLAIR EVANS/DREV
C JANUARY 1988
      SUBROUTINE SC(ZZ,CI,SI)
	  SAVE
c		print *, 'routine SC'
      DATA A1,A2,A3,A4 /38.027264,265.187033,335.67732,38.102495/
      DATA B1,B2,B3,B4 /40.021433,322.624911,570.23628,157.105423/
      DATA C1,C2,C3,C4 /42.242855,302.757865,352.018498,21.821899/
      DATA D1,D2,D3,D4 /48.196927,482.485984,1114.978885,449.690326/
      ZZ2=ZZ*ZZ
      IF(ZZ.GT.1.0) GOTO30
      W=-ZZ2*ZZ/18.
      SI=ZZ+W
      V=-ZZ2/4.
      CI=V
      LIMIT=3.*ZZ+2
      DO 40 I=2,LIMIT
      II=I+I
      I2=I*I*4
      I3=I2*I*2
      W=-W*ZZ2*FLOAT(II-1)/FLOAT(I3+2*I2+II)
      V=-V*ZZ2*FLOAT(II-2)/FLOAT(I3-I2)
      SI=SI+W
   40 CI=CI+V
      RETURN
   30 ZZ4=ZZ2*ZZ2
      ZI2=1./ZZ2
      ZI4=ZI2*ZI2
      F=(ZZ4+A1*ZZ2+A2+A3*ZI2+A4*ZI4)/(ZZ4+B1*ZZ2+B2+B3*ZI2+B4*ZI4)/ZZ
      G=(ZZ4+C1*ZZ2+C2+C3*ZI2+C4*ZI4)/(ZZ4+D1*ZZ2+D2+D3*ZI2+D4*ZI4)/ZZ2
      SZ=SIN(ZZ)
      CZ=COS(ZZ)
      SI=1.57079633-F*CZ-G*SZ
      CI=F*SZ-G*CZ-LOG(ZZ)-.5772156649
      RETURN
      END
C INTEGRATES COATED CYLINDER PHASE FUNCTION OVER PARTICLE SIZE DISTRIBUTION
C USING SIMPSON'S METHOD
C BY BLAIR EVANS/DREV
C JANUARY 1988
      SUBROUTINE COCYLPHASE(X,Y,M)
      REAL*4 RI(0:18000),SUMIC(0:18000)
      REAL*8 PSD(5002),DUMR8(20504)
      COMPLEX*16 M
      COMMON /PHASE/ RI
      COMMON /DATA/ ANGLEL,ANGLEU,DELTA,NANG,ILOW,IUP,RINC,ANGLEU1
      COMMON /POLAR/ IPARA,IPERP
      COMMON /GLOBALR4/ SUMIC
      COMMON /GLOBALR8/ PSD,DUMR8
      SAVE
c		print *, 'routine COCYLPHASE'
      PI=3.141592653
      IFLAG=0
      IDIFF=IUP-ILOW
      IF(IDIFF/2*2.EQ.IDIFF) GOTO10
      IUP=IUP+1
   10 WRITE(*,1000)
 1000 FORMAT(1X,'ORIENTATION ANGLE (0=RANDOM) ?')
      READ(*,*) PHIX
      IF(PHIX.NE.0.) GOTO9
      IFLAG=1
      WRITE(*,1100)
      WRITE(*,1101)
      WRITE(*,1102)
      WRITE(*,1103)
 1100 FORMAT(1X,'WHICH POLARIZATION STATE ? 1) PARALLEL')
 1101 FORMAT(1X,'                           2) PERPENDICULAR')
 1102 FORMAT(1X,'                        OR 3) RANDOM')
 1103 FORMAT(1X,'WHICH RESPECT TO SCATTERING PLANE.')
      READ (*,*) ANS
      IF(ANS.NE.2.) IPARA=1
      IF(ANS.NE.1.) IPERP=1
    9 PHI=PHIX/180.*3.141592653
      C=X
      F=Y
      IF(Y.NE.0.) C=0.
      SUMNS=PSD(ILOW)*ILOW*RINC
      SUMN=PSD(ILOW)
      RR=ILOW*RINC
      SUMA=SUMNS*RR
      X=F*RR+C
      IF(IFLAG.EQ.0) GOTO1
      CALL RANDCC(X,RR,M,QEXT,QSCA,QALPHA)
      GOTO11
    1 CALL COCYL(X,RR,M,PHI,QEXT,QSCA,QALPHA)
   11 QEXT1=QEXT*SUMNS
      QSCA1=QSCA*SUMNS
      DO 2001 I=0,NANG
 2001 SUMIC(I)=RI(I)*SUMNS
      DO 2000 I=ILOW+1,IUP,2
      RR=I*RINC
      R1=RR+RINC
      PROD1=PSD(I)*RR
      PROD2=PSD(I+1)*R1
      SUMNS=SUMNS+4.*PROD1+2.*PROD2
      SUMN=SUMN+4.*PSD(I)+2.*PSD(I+1)
      SUMA=SUMA+4.*PROD1*RR+2.*PROD2*R1
      X=F*RR+C
      IF(IFLAG.EQ.0) GOTO2
      CALL RANDCC(X,RR,M,QEXT,QSCA,QALPHA)
      GOTO12
    2 CALL COCYL(X,RR,M,PHI,QEXT,QSCA,QALPHA)
   12 QEXT1=QEXT1+4.*QEXT*PROD1
      QSCA1=QSCA1+4.*QSCA*PROD1
      DO 2002 II=0,NANG
 2002 SUMIC(II)=SUMIC(II)+4.*RI(II)*PROD1
      X=F*R1+C
      IF(IFLAG.EQ.0) GOTO3
      CALL RANDCC(X,R1,M,QEXT,QSCA,QALPHA)
      GOTO13
    3 CALL COCYL(X,R1,M,PHI,QEXT,QSCA,QALPHA)
   13 QEXT1=QEXT1+2.*QEXT*PROD2
      QSCA1=QSCA1+2.*QSCA*PROD2
      DO 2003 II=0,NANG
 2003 SUMIC(II)=SUMIC(II)+2.*RI(II)*PROD2
 2000 CONTINUE
      SUMNS=SUMNS-PROD2
      SUMN=SUMN-PSD(IUP)
      SUMA=SUMA-PROD2*R1
      QEXT1=QEXT1-QEXT*PROD2
      QSCA1=QSCA1-QSCA*PROD2
      IF(SUMNS.GT.0) GOTO 14
      WRITE(*,1306)
      STOP
   14 QEXT2=QEXT1/SUMNS
      QSCA2=QSCA1/SUMNS
      SUMA=QEXT1*4./SUMA
      QEXT1=QEXT1/SUMN/PI
      QSCA1=QSCA1/SUMN/PI
      IF(IFLAG.EQ.0) WRITE(*,1201) 2.*PHIX
 1201 FORMAT(1X,'PHASE FUNCTION ON CONE OF APEX ANGLE',1X,F6.2,1X,
     $'DEG.')
      DO 2004 II=0,NANG
      SUMIC(II)=(SUMIC(II)-RI(II)*PROD2)/SUMNS
 2004 WRITE(7,1200) ANGLEL+FLOAT(II)*DELTA,SUMIC(II)
 1200 FORMAT(1X,F6.2,1X,G12.5)
      WRITE(7,1304) QEXT2,QSCA2,QEXT2-QSCA2
 1304 FORMAT(1X,'COMP EXT EFF=',G12.6,1X,'COMP SCA EFF=',G12.6,1X,
     $'COMP ABS EFF=',G12.6)
      IF(QEXT2.GT.0.) GOTO21
      WRITE(7,1305) 0.
      GOTO22
   21 IF(ANGLEU1.EQ.180.) WRITE(7,1305) SUMIC(NANG)/QEXT2
   22 WRITE(7,1300) QEXT1
 1300 FORMAT(1X,'EXT/PART=',G12.6,'*LAMBDA')
      WRITE(7,1301) QSCA1
 1301 FORMAT(1X,'SCA/PART=',G12.6,'*LAMBDA')
      WRITE(7,1302) QEXT1-QSCA1
 1302 FORMAT(1X,'ABS/PART=',G12.6,'*LAMBDA')
      WRITE(7,1303) SUMA
 1303 FORMAT(1X,'MASS EXT=',G12.6,'/LAMBDA/DENSITY')
 1305 FORMAT(1X,'LIDAR RATIO=',G12.6)
 1306 FORMAT(1X,'THERE ARE NO PARTICLES IN THIS SIZE RANGE')
      STOP
      END
C COATED PERFECTLY REFLECTING INFINITE CYLINDER
C PHASE FUNCTION PROGRAM (FROM RUCK 1970 WITH CORRECTIONS)
C BY BLAIR EVANS/DREV
C JANUARY 1988
      SUBROUTINE COCYL(X,Z,M,PHI,QEXT,QSCA,QALPHA)
      REAL*4 RI(0:18000),JL(1000),Y(1000),JLP
      COMPLEX*8 SUM11,SUM22,SUM12,YJP,JMP,YMP,CSUMA,Z3
      COMPLEX*8 JJ(6000),JM(6000),YJ(6000),YM(6000),Z2,MS,CJ
      COMPLEX*8 H,HP,JJP,CSUMB,T1,T2,T3,T4
      COMPLEX*8 A1(1001),A2(1001),B1(1001)
      COMPLEX*8 ZN,YN,VN,PN,NN,MN,QN,QNN,CK
      COMPLEX*16 M,CALPHA,DELT
      COMMON /PHASE/ RI
      COMMON /DATA/ ANGLEL,ANGLEU,DELTA,NANG,ILOW,IUP,RINC,ANGLEU1
      COMMON /GLOBALC8/ A1,A2,B1
      SAVE
c		print *, 'routine COCYL'
      PHI=1.570796326-PHI
      PI=3.141592653
      CH=SIN(PHI)
      CL=COS(PHI)
      MS=M*M
      CJ=CSQRT(MS-CMPLX(CH**2,0.))
      QN=CH/Z*(1./(CJ*CJ)-1./(CL*CL))
      Z1=Z*CL
      Z2=Z*CJ
      Z3=X*CJ
C CALCULATE BESSEL FUNCTION OF FIRST KIND FOR ALL ORDERS AT Z1
      ZZ=CABS(Z3)
      NSTOP1=Z1+4.*Z1**.333333+6.
      NDELTA=(101.+Z1)**.499
      MST=NSTOP1+NDELTA
      MST=(MST/2)*2
      IF(Z1.GT.0.0003) GOTO5
      IJL=15
      F=1
      DO 11 L=1,15
      JL(L)=(Z1/2.)**(L-1)/F
   11 F=F*L
      GOTO6
    5 JL(MST+1)=1.E-34
      JL(MST)=1.E-32
      M1=MST-1
      DO 10 L=1,M1
      ML=MST-L
   10 JL(ML)=2.*FLOAT(ML)*JL(ML+1)/Z1-JL(ML+2)
      ALPHA=JL(1)
      M2=MST-2
      DO 20 L=2,M2,2
   20 ALPHA=ALPHA+2.*JL(L+1)
      DO 30 L=1,M1
   30 JL(L)=JL(L)/ALPHA
      IJL=M1
C CALCULATE BESSEL FUNCTION OF SECOND KIND FOR ALL ORDERS AT Z1
    6 Y(1)=JL(1)*(ALOG(Z1/2.)+.5772156649)
      M4=MST/2-1
      DO 40 L=1,M4
   40 Y(1)=Y(1)-2.*((-1)**L)*JL(2*L+1)/FLOAT(L)
      Y(1)=.6366197724*Y(1)
      Y(2)=JL(2)*Y(1)-.6366197724/Z1
      Y(2)=Y(2)/JL(1)
      NS=NSTOP1-1
      DO 50 L=1,NS
   50 Y(L+2)=2.*FLOAT(L)*Y(L+1)/Z1-Y(L)
      IY=NS+2
C CALCULATE BESSEL FUNCTION OF FIRST KIND FOR ALL ORDERS AT Z2
   60 ZZ1=CABS(Z2)
      NSTOP=ZZ1+4.*ZZ1**.333333+4.
      NDELTA=(101.+ZZ1)**.499
      MST=NSTOP+NDELTA
      MST=(MST/2)*2
      IF(ZZ1.GT.0.0003) GOTO70
      IJJ=15
      F=1
      DO 65 L=1,15
      JJ(L)=(Z2/2.)**(L-1)/F
   65 F=F*L
      GOTO80
   70 JJ(MST+1)=0.0
      JJ(MST)=1.0E-32
      M1=MST-1
      DO 110 L=1,M1
      ML=MST-L
  110 JJ(ML)=2.*FLOAT(ML)*JJ(ML+1)/Z2-JJ(ML+2)
      CALPHA=JJ(1)
      M2=MST-2
      DO 120 L=2,M2,2
  120 CALPHA=CALPHA+2.*JJ(L+1)
      DO 130 L=1,M1
  130 JJ(L)=JJ(L)/CALPHA
      IJJ=M1
C CALCULATE BESSEL FUNCTION OF SECOND KIND FOR ALL ORDERS AT Z2
   80 YJ(1)=JJ(1)*(LOG(Z2/2.)+.5772156649)
      M4=MST/2-1
      DO 140 L=1,M4
  140 YJ(1)=YJ(1)-2.*((-1)**L)*JJ(2*L+1)/FLOAT(L)
      YJ(1)=.6366197724*YJ(1)
      YJ(2)=JJ(2)*YJ(1)-.6366197725/Z2
      YJ(2)=YJ(2)/JJ(1)
      DO 150 L=1,NSTOP1-1
  150 YJ(L+2)=2.*FLOAT(L)*YJ(L+1)/Z2-YJ(L)
      IYJ=NSTOP1+1
C CALCULATE BESSEL FUNCTION OF FIRST KIND FOR ALL ORDERS AT Z3
      ZZ=CABS(Z3)
      NSTOP=ZZ+4.*ZZ**.333333+4.
      NDELTA=(101.+ZZ)**.499
      MST=NSTOP+NDELTA
      MST=(MST/2)*2
      IF(ZZ.GT.0.0003) GOTO155
      IJM=15
      IYM=15
      F=1
      JM(1)=1.
      DO 153 L=2,15
      JM(L)=(Z3/2.)**(L-1)/F
      IF(CABS(JM(L)).LT.1.E-30) GOTO153
      YM(L)=-1./JM(L)/PI/(L-1)
  153 F=F*L
      YM(1)=2*LOG(Z3)/PI
      GOTO196
  155 JM(MST+1)=0.0
      JM(MST)=1.0E-32
      M1=MST-1
      DO 160 L=1,M1
      ML=MST-L
  160 JM(ML)=2.*FLOAT(ML)*JM(ML+1)/Z3-JM(ML+2)
      CALPHA=JM(1)
      M2=MST-2
      DO 170 L=2,M2,2
  170 CALPHA=CALPHA+2.*JM(L+1)
      DO 180 L=1,M1
  180 JM(L)=JM(L)/CALPHA
      IJM=M1
C CALCULATE BESSEL FUNCTION OF SECOND KIND FOR ALL ORDERS AT Z3
  185 YM(1)=JM(1)*(LOG(Z3/2.)+.5772156649)
      M4=MST/2-1
      DO 190 L=1,M4
  190 YM(1)=YM(1)-2.*((-1)**L)*JM(2*L+1)/FLOAT(L)
      YM(1)=.6366197724*YM(1)
      YM(2)=JM(2)*YM(1)-.6366197724/Z3
      YM(2)=YM(2)/JM(1)
      NS=NSTOP-1
      DO 195 L=1,NS
  195 YM(L+2)=2.*FLOAT(L)*YM(L+1)/Z3-YM(L)
      IYM=NS+2
  196 CSUMA=CMPLX(0.,0.)
      CSUMB=CMPLX(0.,0.)
      CSSCAA=0.
      CSSCAB=0.
      NS=MAX(NSTOP-1,NSTOP1-1)
      IF(NS.LE.IJL) GOTO700
      DO 750 L=IJL,NS
  750 JL(L)=1.E-32
  700 IF(NS.LE.IY) GOTO701
      DO 751 L=IY,NS
  751 Y(L)=1.E30
  701 IF(NS.LE.IJJ) GOTO702
      DO 752 L=IJJ,NS
  752 JJ(L)=CMPLX(1.E-32,1.E-32)
  702 IF(NS.LE.IYJ) GOTO703
      DO 753 L=IYJ,NS
  753 YJ(L)=CMPLX(1.E28,1.E28)
  703 IF(NS.LE.IJM) GOTO704
      DO 754 L=IJM,NS
  754 JM(L)=CMPLX(1.E-32,1.E-32)
  704 IF(NS.LE.IYM) GOTO705
      DO 755 L=IYM,NS
  755 YM(L)=CMPLX(1.E28,1.E28)
  705 DO 200 L=NS,1,-1
      H=CMPLX(JL(L),Y(L))
      JJP=JJ(L)*FLOAT(L-1)/Z2-JJ(L+1)
      JLP=JL(L)*FLOAT(L-1)/Z1-JL(L+1)
      JMP=JM(L)*FLOAT(L-1)/Z3-JM(L+1)
      YP=Y(L)*FLOAT(L-1)/Z1-Y(L+1)
      YJP=YJ(L)*FLOAT(L-1)/Z2-YJ(L+1)
      YMP=YM(L)*FLOAT(L-1)/Z3-YM(L+1)
      HP=CMPLX(JLP,YP)
      IF(ZZ.GT.1.E-5) GOTO210
      ZN=CJ/(CL*MS)*JJ(L)/JJP
      YN=CJ/CL*JJ(L)/JJP
      GOTO220
  210 CALPHA=JJP*YM(L)-YJP*JM(L)
      ZN=CJ/(CL*MS)*(JJ(L)*YM(L)-YJ(L)*JM(L))/CALPHA
      IF(JJP*YMP-YJP*JMP.NE.CMPLX(0.,0.)) GOTO215
      YN=CMPLX(1.E17,1.E17)
      GOTO220
  215 CALPHA=JJP*YMP-YJP*JMP
      YN=CJ/CL*(JJ(L)*YMP-YJ(L)*JMP)/CALPHA
  220 VN=JL(L)-ZN*JLP
      IF(ALOG10(CABS(YN))+ALOG10(CABS(HP)).GT.34) GOTO200
      PN=H-YN*HP
      NN=H-ZN*HP
      IF(CABS(PN).GT.1.E17.AND.CABS(NN).GT.1.E17) GOTO200
      MN=JL(L)-YN*JLP
      QNN=FLOAT(L-1)*QN
      IF(CABS(QNN*H).GT.1.E17) GOTO200
C CORRECTION OF ERROR IN RUCK 1970, p280-281
      CK=ZN*CL*CL*YN
      IF(PN*NN-CK*(QNN*H)**2.EQ.CMPLX(0.,0.)) GOTO200
      DELT=-1.D0/(PN*NN-CK*(QNN*H)**2)
      A1(L)=-CONJG(2./(PI*Z1)*DELT*QNN*CK/CL)
      A2(L)=CONJG((QNN*QNN*JL(L)*H*CK-MN*NN)*DELT)
      B1(L)=CONJG((QNN*QNN*JL(L)*H*CK-VN*PN)*DELT)
      CSSCAA=CSSCAA+CABS(A1(L))**2+CABS(B1(L))**2
      CSSCAB=CSSCAB+CABS(A2(L))**2+CABS(A1(L))**2
      CSUMB=CSUMB+B1(L)
      CSUMA=CSUMA+A2(L)
  200 CONTINUE
      T1=2.*CSUMB-B1(1)
      T2=2.*CSUMA-A2(1)
      T3=2.*(CSSCAA-CABS(A1(1))**2)-CABS(B1(1))**2
      T4=2.*(CSSCAB-CABS(A1(1))**2)-CABS(A2(1))**2
      QSCA1=2.*T3/Z
      QSCA2=2.*T4/Z
      QEXT1=2.*REAL(T1)/Z
      QEXT2=2.*REAL(T2)/Z
      QEXT=.5*(QEXT1+QEXT2)
      QSCA=.5*(QSCA1+QSCA2)
      QALPHA=4.*QEXT*CL/Z
      DO 450 I=0,NANG
      RAD=PI/180.*(ANGLEL+FLOAT(I)*DELTA)
      SUM11=CMPLX(0.,0.)
      SUM12=CMPLX(0.,0.)
      SUM22=CMPLX(0.,0.)
      DO 400 L=1,NS
      XL=COS((L-1)*RAD)
      YL=SIN((L-1)*RAD)
      SUM11=SUM11+B1(L)*XL
      SUM12=SUM12+A1(L)*YL
  400 SUM22=SUM22+A2(L)*XL
      SUM11=2.*SUM11-B1(1)
      SUM12=2.*SUM12
      SUM22=2.*SUM22-A2(1)
C PHASE FUNCTION FOR RANDOM POLARIZATION
      RI(I)=1./PI/PI/Z/QSCA*(.5*(CABS(SUM11)**2+2.*CABS(SUM12)
     $**2+CABS(SUM22)**2))
  450 CONTINUE
      PHI=1.570796326-PHI
      RETURN
      END
C RANDOM ORIENTATION (VARIATION OF COHEN ET AL 1985)
C COATED PERFECTLY REFLECTING INFINITE CYLINDER
C PHASE FUNCTION PROGRAM (FROM RUCK 1970 WITH CORRECTIONS)
C BY BLAIR EVANS/DREV
C JANUARY 1988
      SUBROUTINE RANDCC(X,Z,M,QEXT,QSCA,QALPHA)
      REAL*4 RI(0:18000),JL(1000),Y(1000),JLP
      REAL*8 DRATIO
      COMPLEX*8 SUM11,SUM22,SUM12,YJP,JMP,YMP,CSUMA,Z3
      COMPLEX*8 JJ(6000),JM(6000),YJ(6000),YM(6000),Z2,MS,CJ
      COMPLEX*8 H,HP,JJP,CSUMB,T1,T2,T3,T4
      COMPLEX*8 A1(1001),A2(1001),B1(1001)
      COMPLEX*8 ZN,YN,VN,PN,NN,MN,QN,QNN,CK
      COMPLEX*16 M,CALPHA,DELT
      COMMON /PHASE/ RI
      COMMON /DATA/ ANGLEL,ANGLEU,DELTA,NANG,ILOW,IUP,RINC,ANGLEU1
      COMMON /POLAR/ IPARA,IPERP
C      COMMON /GLOBALR4/ JL,Y,DUMR4
      COMMON /GLOBALC8/ A1,A2,B1
      SAVE
c		print *, 'routine RANDCC'
      IF(IPARA.EQ.0.AND.IPERP.EQ.0) GOTO1
      FACT=1./(IPARA+IPERP)
    1 PI=3.141592653
      PI2=PI/2.
      RAD1=PI/180.
      MS=M*M
      N=360
      IFLAG1=1
      IF(DELTA.LT.0.5) N=180/DELTA
      TWOH=180./N
      H2=TWOH/2.
      QEXT1=0.
      QEXT2=0.
      QSCA1=0.
      QSCA2=0.
      IANGL=ANGLEL*500.+.9
      IANGU=ANGLEU*500.+.9
      IDELTA=DELTA*500.+.9
      IPHIP=IANGL
      DO 100 I=IANGL,IANGU,IDELTA
  100 RI(I/5)=0.
      DO 500 IPHI=1,N+1
      DO 510 IA=1,2
      WA=IA
      IF(IPHI.EQ.1.AND.IA.EQ.1) GOTO510
      IF(IPHI.EQ.N+1.AND.IA.EQ.1) WA=.5
      IF(IPHI.EQ.N+1.AND.IA.EQ.2) GOTO510
      IPHI1=((IPHI-1)*TWOH+(IA-1)*H2)*500.+.1
      PHI=PI2-((IPHI-1)/2.*TWOH+(IA-1)*H2/2.)*RAD1
      CH=SIN(PHI)
      CL=COS(PHI)
      CJ=CSQRT(MS-CMPLX(CH**2,0.))
      QN=CH/Z*(1./(CJ*CJ)-1./(CL*CL))
      Z1=Z*CL
      Z2=Z*CJ
      Z3=X*CJ
C CALCULATE BESSEL FUNCTION OF FIRST KIND FOR ALL ORDERS AT Z1
      ZZ=CABS(Z3)
      NSTOP1=Z1+4.*Z1**.333333+6.
      NDELTA=(101.+Z1)**.499
      MST=NSTOP1+NDELTA
      MST=(MST/2)*2
      IF(Z1.GT.0.0003) GOTO5
      IJL=15
      F=1
      DO 11 L=1,15
      JL(L)=(Z1/2.)**(L-1)/F
   11 F=F*L
      GOTO6
    5 JL(MST+1)=1.E-34
      JL(MST)=1.E-32
      M1=MST-1
      DO 10 L=1,M1
      ML=MST-L
   10 JL(ML)=2.*FLOAT(ML)*JL(ML+1)/Z1-JL(ML+2)
      ALPHA=JL(1)
      M2=MST-2
      DO 20 L=2,M2,2
   20 ALPHA=ALPHA+2.*JL(L+1)
      DO 30 L=1,M1
   30 JL(L)=JL(L)/ALPHA
      IJL=M1
C CALCULATE BESSEL FUNCTION OF SECOND KIND FOR ALL ORDERS AT Z1
    6 Y(1)=JL(1)*(ALOG(Z1/2.)+.5772156649)
      M4=MST/2-1
      DO 40 L=1,M4
   40 Y(1)=Y(1)-2.*((-1)**L)*JL(2*L+1)/FLOAT(L)
      Y(1)=.6366197724*Y(1)
      Y(2)=JL(2)*Y(1)-.6366197724/Z1
      Y(2)=Y(2)/JL(1)
      NS=NSTOP1-1
      DO 50 L=1,NS
   50 Y(L+2)=2.*FLOAT(L)*Y(L+1)/Z1-Y(L)
      IY=NS+2
C CALCULATE BESSEL FUNCTION OF FIRST KIND FOR ALL ORDERS AT Z2
   60 ZZ1=CABS(Z2)
      NSTOP=ZZ1+4.*ZZ1**.333333+4.
      NDELTA=(101.+ZZ1)**.499
      MST=NSTOP+NDELTA
      MST=(MST/2)*2
      IF(ZZ1.GT.0.0003) GOTO70
      IJJ=15
      F=1
      DO 65 L=1,15
      JJ(L)=(Z2/2.)**(L-1)/F
   65 F=F*L
      GOTO80
   70 JJ(MST+1)=0.0
      JJ(MST)=1.0E-32
      M1=MST-1
      DO 110 L=1,M1
      ML=MST-L
  110 JJ(ML)=2.*FLOAT(ML)*JJ(ML+1)/Z2-JJ(ML+2)
      CALPHA=JJ(1)
      M2=MST-2
      DO 120 L=2,M2,2
  120 CALPHA=CALPHA+2.*JJ(L+1)
      CALPHA=1.D0/CALPHA
      DO 130 L=1,M1
  130 JJ(L)=JJ(L)*CALPHA
      IJJ=M1
C CALCULATE BESSEL FUNCTION OF SECOND KIND FOR ALL ORDERS AT Z2
   80 YJ(1)=JJ(1)*(LOG(Z2/2.)+.5772156649)
      M4=MST/2-1
      DO 140 L=1,M4
  140 YJ(1)=YJ(1)-2.*((-1)**L)*JJ(2*L+1)/FLOAT(L)
      YJ(1)=.6366197724*YJ(1)
      YJ(2)=JJ(2)*YJ(1)-.6366197725/Z2
      YJ(2)=YJ(2)/JJ(1)
      DO 150 L=1,NSTOP1-1
  150 YJ(L+2)=2.*FLOAT(L)*YJ(L+1)/Z2-YJ(L)
      IYJ=NSTOP1+1
C CALCULATE BESSEL FUNCTION OF FIRST KIND FOR ALL ORDERS AT Z3
      ZZ=CABS(Z3)
      NSTOP=ZZ+4.*ZZ**.333333+4.
      NDELTA=(101.+ZZ)**.499
      MST=NSTOP+NDELTA
      MST=(MST/2)*2
      IF(ZZ.GT.0.0003) GOTO155
      IJM=15
      IYM=15
      F=1
      JM(1)=1.
      DO 153 L=2,15
      JM(L)=(Z3/2.)**(L-1)/F
      IF(CABS(JM(L)).LT.1.E-30) GOTO153
      YM(L)=-1./JM(L)/PI/(L-1)
  153 F=F*L
      YM(1)=2*LOG(Z3)/PI
      GOTO196
  155 JM(MST+1)=0.0
      JM(MST)=1.0E-32
      M1=MST-1
      DO 160 L=1,M1
      ML=MST-L
  160 JM(ML)=2.*FLOAT(ML)*JM(ML+1)/Z3-JM(ML+2)
      CALPHA=JM(1)
      M2=MST-2
      DO 170 L=2,M2,2
  170 CALPHA=CALPHA+2.*JM(L+1)
      CALPHA=1.D0/CALPHA
      DO 180 L=1,M1
  180 JM(L)=JM(L)*CALPHA
      IJM=M1
C CALCULATE BESSEL FUNCTION OF SECOND KIND FOR ALL ORDERS AT Z3
  185 YM(1)=JM(1)*(LOG(Z3/2.)+.5772156649)
      M4=MST/2-1
      DO 190 L=1,M4
  190 YM(1)=YM(1)-2.*((-1)**L)*JM(2*L+1)/FLOAT(L)
      YM(1)=.6366197724*YM(1)
      YM(2)=JM(2)*YM(1)-.6366197724/Z3
      YM(2)=YM(2)/JM(1)
      NS=NSTOP-1
      DO 195 L=1,NS
  195 YM(L+2)=2.*FLOAT(L)*YM(L+1)/Z3-YM(L)
      IYM=NS+2
  196 CSUMA=CMPLX(0.,0.)
      CSUMB=CMPLX(0.,0.)
      CSSCAA=0.
      CSSCAB=0.
      NS=MAX(NSTOP-1,NSTOP1-1)
      IF(NS.LE.IJL) GOTO700
      DO 750 L=IJL,NS
  750 JL(L)=1.E-32
  700 IF(NS.LE.IY) GOTO701
      DO 751 L=IY,NS
  751 Y(L)=1.E30
  701 IF(NS.LE.IJJ) GOTO702
      DO 752 L=IJJ,NS
  752 JJ(L)=CMPLX(1.E-32,1.E-32)
  702 IF(NS.LE.IYJ) GOTO703
      DO 753 L=IYJ,NS
  753 YJ(L)=CMPLX(1.E28,1.E28)
  703 IF(NS.LE.IJM) GOTO704
      DO 754 L=IJM,NS
  754 JM(L)=CMPLX(1.E-32,1.E-32)
  704 IF(NS.LE.IYM) GOTO705
      DO 755 L=IYM,NS
  755 YM(L)=CMPLX(1.E28,1.E28)
  705 DO 200 L=NS,1,-1
      H=CMPLX(JL(L),Y(L))
      JJP=JJ(L)*FLOAT(L-1)/Z2-JJ(L+1)
      JLP=JL(L)*FLOAT(L-1)/Z1-JL(L+1)
      JMP=JM(L)*FLOAT(L-1)/Z3-JM(L+1)
      YP=Y(L)*FLOAT(L-1)/Z1-Y(L+1)
      YJP=YJ(L)*FLOAT(L-1)/Z2-YJ(L+1)
      YMP=YM(L)*FLOAT(L-1)/Z3-YM(L+1)
      HP=CMPLX(JLP,YP)
      IF(ZZ.GT.1.E-5) GOTO210
      ZN=CJ/(CL*MS)*JJ(L)/JJP
      YN=CJ/CL*JJ(L)/JJP
      GOTO220
  210 CALPHA=JJP*YM(L)-YJP*JM(L)
      ZN=CJ/(CL*MS)*(JJ(L)*YM(L)-YJ(L)*JM(L))/CALPHA
      IF(JJP*YMP-YJP*JMP.NE.CMPLX(0.,0.)) GOTO215
      YN=CMPLX(1.E17,1.E17)
      GOTO220
  215 CALPHA=(JJP*YMP-YJP*JMP)
      YN=CJ/CL*(JJ(L)*YMP-YJ(L)*JMP)/CALPHA
  220 VN=JL(L)-ZN*JLP
      IF(ALOG10(CABS(YN))+ALOG10(CABS(HP)).GT.34) GOTO200
      PN=H-YN*HP
      NN=H-ZN*HP
      IF(CABS(PN).GT.1.E17.AND.CABS(NN).GT.1.E17) GOTO200
      MN=JL(L)-YN*JLP
      QNN=FLOAT(L-1)*QN
      IF(CABS(QNN*H).GT.1.E17) GOTO200
C CORRECTION OF ERROR IN RUCK 1970, p280-281
      CK=ZN*CL*CL*YN
      IF(PN*NN-CK*(QNN*H)**2.EQ.CMPLX(0.,0.)) GOTO200
      DELT=-1.D0/(PN*NN-CK*(QNN*H)**2)
      A1(L)=-CONJG(2./(PI*Z1)*DELT*QNN*CK/CL)
      A2(L)=CONJG((QNN*QNN*JL(L)*H*CK-MN*NN)*DELT)
      B1(L)=CONJG((QNN*QNN*JL(L)*H*CK-VN*PN)*DELT)
      CSSCAA=CSSCAA+CABS(A1(L))**2+CABS(B1(L))**2
      CSSCAB=CSSCAB+CABS(A2(L))**2+CABS(A1(L))**2
      CSUMB=CSUMB+B1(L)
      CSUMA=CSUMA+A2(L)
  200 CONTINUE
      T1=2.*CSUMB-B1(1)
      T2=2.*CSUMA-A2(1)
      T3=2.*(CSSCAA-CABS(A1(1))**2)-CABS(B1(1))**2
      T4=2.*(CSSCAB-CABS(A1(1))**2)-CABS(A2(1))**2
      QSCA1=QSCA1+WA*H2/3.*RAD1*2.*T3/Z
      QSCA2=QSCA2+WA*H2/3.*RAD1*2.*T4/Z
      QEXT1=QEXT1+WA*H2/3.*RAD1*2.*REAL(T1)/Z
      QEXT2=QEXT2+WA*H2/3.*RAD1*2.*REAL(T2)/Z
      IF(IPHI1.LT.IANGL) GOTO510
C LOOP OVER PHASE FUNCTION ANGLES TRANSFORMING FROM INFINITE
C CYLINDER SCATTERING CONE TO SCATTERING REFERENCE FRAME
      QSCA=(T3+T4)/Z
  310 IF(IPHI1.LT.IPHIP+IDELTA.OR.IPHIP.EQ.IANGU) GOTO300
      IPHIP=IPHIP+IDELTA
      IFLAG1=1
      GOTO310
  300 DO 450 I=IANGL,IPHIP,IDELTA
      IF(IFLAG1.EQ.0..OR.I.NE.IPHIP) GOTO305
      IFLAG1=0
      WA=1
      RAD=PI
      IF(IPHI.NE.(N+1)) GOTO302
  301 F=PI/2.*3./(H2*RAD1)
      GOTO303
  302 RDD=H2/2.*RAD1+RAD1*I/1000.
      F=DATAN((DSIN(DBLE(RDD+RAD1*I/1000.))*DSIN(DBLE(H2/2.*RAD1)))**.5
     $/DCOS(DBLE(RDD)))*3./(H2*RAD1)/2.
      GOTO303
  305 DRATIO=DSIN(DBLE(RAD1*I/1000.))/CL
      RAD=2.*DASIN(DRATIO)
      F=1./COS(RAD/2.)
  303 SUM11=CMPLX(0.,0.)
      SUM12=CMPLX(0.,0.)
      SUM22=CMPLX(0.,0.)
      DO 400 L=1,NS
      XL=COS((L-1)*RAD)
      YL=SIN((L-1)*RAD)
      SUM11=SUM11+B1(L)*XL
      SUM12=SUM12+A1(L)*YL
  400 SUM22=SUM22+A2(L)*XL
      SUM11=2.*SUM11-B1(1)
      SUM12=2.*SUM12
      SUM22=2.*SUM22-A2(1)
C PHASE FUNCTION FOR RANDOM POLARIZATION
      RI(I/5)=RI(I/5)+WA*F/PI/PI/Z*((CABS(SUM11)**2*IPARA+CABS(SUM12)**
     $2*(IPARA+IPERP)+CABS(SUM22)**2*IPERP)*FACT)/QSCA
  450 CONTINUE
  510 CONTINUE
  500 CONTINUE
      DO 600 I=0,NANG
      J=(IANGL+I*IDELTA)/5
  600 RI(I)=RI(J)*H2/3.*RAD1*2./PI
      QEXT=FACT*(QEXT1*IPARA+QEXT2*IPERP)
      QSCA=FACT*(QSCA1*IPARA+QSCA2*IPERP)
      QALPHA=PI*QEXT/Z
      PHI=1.570796326-PHI
      RETURN
      END
C MIE CODE FOR SPHERICAL PARTICLES
C BY BLAIR EVANS/DREV
C JANUARY 1988
      SUBROUTINE QMIE(M)
      REAL*8 PSI(2004),CHI(2004),ETA2(2004),QSCA(5002),QABS(5002)
      REAL*8 QEXT,X,P0,SCALE,MABS,C0,QSC1
      REAL*8 DUMMY(5002),DUMR8(4488)
      COMPLEX*16 ETA3(2004),ETA1(4008),A(2004),B(2004),M,ZETA
      COMPLEX*16 CDI,Z,DUMC16(7480)
      COMMON /DATA/ ANGLEL,ANGLEU,DELTA,NANG,ILOW,IUP,RINC,ANGLEU1
      COMMON /GLOBALR8/ DUMMY,QSCA,QABS,PSI,CHI,ETA2,DUMR8
      COMMON /GLOBALC16/ ETA3,ETA1,A,B,DUMC16
      SAVE
c		print *, 'routine QMIE'
      DO 111 I=1,2004
  111 CHI(I)=1.E32
      DO 10 I=1,IUP+1
      QSCA(I)=0.D0
   10 QABS(I)=0.D0
      RES=RINC
      XX=(IUP+1)*RINC
      XL=ILOW*RINC
      MABS=CDABS(M)
      IX=XX/RES+.1
      IL=XL/RES+.1
      DO 30 K=IL,IX
      QEXT=0.D0
      QSC1=0.D0
      X=DBLE(K)*RES
      NL3=1.5*X+10
C CALCULATE PSI(X) FOR ALL ORDERS BY BACKWARD RECURRENCE
C (ABRAMOWITZ & STEGUN P 452)
      IF(X.GT.100) GOTO2
      NL1=2*X+10
      GOTO1
    2 NL1=MAX0(NL3,IDINT(X+4.*X**.533+2))
    1 PSI(NL1+2)=0.D0
      PSI(NL1+1)=1.D0
      KK=0
      DO 20 I=NL1,1,-1
      IF (PSI(I+2).LT.1.E30) GOTO20
      K1=I+2
      KK=KK+1
      PSI(I+1)=PSI(I+1)*1.E-30
      PSI(I+2)=PSI(I+2)*1.E-30
   20 PSI(I)=DBLE(2*I+3)/X*PSI(I+1)-PSI(I+2)
      P0=3.D0/X*PSI(1)-PSI(2)
      SCALE=P0/DSIN(X)
      DO 22 I=1,NL1
   22 PSI(I)=PSI(I)/SCALE
      IF(KK.EQ.0) GOTO999
      DO 23 I=K1+1,NL1
   23 PSI(I)=PSI(I)*1.E-30
  999 P0=DSIN(X)
C CALCULATE ETA1(MX) FOR ALL ORDER BY BACKWARD RECURRENCE
C (KATTAWAR & PLASS APPL. OPT. 6 1377-1382)
      NL2=2.0*MABS*X+10
      Z=M*DCMPLX(X,0D0)
      ETA1(NL2)=DCMPLX(0.D0,0.D0)
      DO 40 I=NL2,1,-1
      CDI=DBLE(I+1)/Z
   40 ETA1(I)=CDI-1.D0/(ETA1(I+1)+CDI)
C CALCULATE ETA2(X) FOR ALL ORDERS BY BACKWARD RECURRENCE
C (KERKER P 67-68)
      ETA2(NL3)=0.
      DO 50 I=NL3,1,-1
   50 ETA2(I)=DBLE(I+1)/X-1./(ETA2(I+1)+DBLE(I+1)/X)
C CALCULATE ETA3(X) AND CHI(X) FOR ALL ORDERS BY FORWARD RECURRENCE
C AND THE MIE SCATTERING COEFFICIENTS
C (KERKER P 67-68)
      C0=DCOS(X)
      CHI(1)=DCOS(X)/X+DSIN(X)
      ETA3(1)=DCMPLX(P0,C0)/DCMPLX(PSI(1),CHI(1))-1.D0/X
      CHI(2)=DBLE(3)/X*CHI(1)-C0
      ETA3(2)=1.D0/(2.D0/X-ETA3(1))-2.D0/X
      ZETA=DCMPLX(PSI(1),CHI(1))
      A(1)=PSI(1)*((ETA1(1)-M*ETA2(1))/(ETA1(1)-M*ETA3(1)))/ZETA
      B(1)=PSI(1)*((ETA2(1)-M*ETA1(1))/(ETA3(1)-M*ETA1(1)))/ZETA
      ZETA=DCMPLX(PSI(2),CHI(2))
      A(2)=PSI(2)*((ETA1(2)-M*ETA2(2))/(ETA1(2)-M*ETA3(2)))/ZETA
      B(2)=PSI(2)*((ETA2(2)-M*ETA1(2))/(ETA3(2)-M*ETA1(2)))/ZETA
      K2=0
      DO 60 I=3,NL3
      ETA3(I)=1.D0/(DBLE(I)/X-ETA3(I-1))-DBLE(I)/X
      IF(K2.EQ.1) GOTO65
      CHI(I)=(2.D0*DBLE(I)-1.D0)/X*CHI(I-1)-CHI(I-2)
      IF(CHI(I).GT.1.E32) K2=1
   65 ZETA=DCMPLX(PSI(I),CHI(I))
      A(I)=PSI(I)*((ETA1(I)-M*ETA2(I))/(ETA1(I)-M*ETA3(I)))/ZETA
      B(I)=PSI(I)*((ETA2(I)-M*ETA1(I))/(ETA3(I)-M*ETA1(I)))/ZETA
   60 CONTINUE
      I=0
    3 IF (I.LT.X) GOTO4
      IF (I.GE.NL3) GOTO75
      IF (DBLE(A(I)).LE.1.E-17) GOTO75
    4 I=I+1
      QEXT=QEXT+DBLE(2*I+1)*(DBLE(A(I)+B(I)))
      QSC1=QSC1+DBLE(2*I+1)*(CDABS(A(I))**2+CDABS(B(I))**2)
      GOTO3
   75 QEXT=2.D0/X**2*QEXT
      QSC1=2.D0/X**2*QSC1
      QSCA(K)=QSC1
   30 QABS(K)=QEXT-QSC1
      RETURN
      END
C SUBROUTINE TO CALCULATE DIFFRACTION COMPONENT OF PHASE FUNCTION
C FOR IRREGULARLY SHAPED PARTICLES (FROM POLLACK AND CUZZI 1979)
C BY BLAIR EVANS/DREV
C JANUARY 1988
      SUBROUTINE DIFFPHASE(X,R)
      REAL*4 RI(0:18000)
      COMMON /PHASE/RI
      COMMON /DATA/ ANGLEL,ANGLEU,DELTA,NANG,ILOW,IUP,RINC,ANGLEU1
	  SAVE
c		print *, 'routine DIFFPHASE'
      DO 60 I=0,18000
   60 RI(I)=0.      
C COEFFICENTS USED TO CALCULATE BESSEL FUNCTION J1/Z (A+S 9.4.4 FOR
C Z<=3 AND HODKINSON AND GREENLEAVES APPROX. FOR Z>3)
      A1=-.56249985
      A2=.21093573
      A3=-.03954289
      A4=.00443319
      A5=-.00031761
      A6=.00001109
      XBAR=X*R
C UNNORMALIZED PHASE FUNCTION USING POLYNOMIAL APPROXIMATION FOR THE
C BESSEL FUNCTION J1
      DO 10 I=0,9000
      RAD=FLOAT(I)*.00017453293
      RK=.5*(1.+(COS(RAD))**2)
      Z=XBAR*SIN(RAD)
      IF(Z.GT.3) GOTO20
      Z3=Z/3.
      RJ1Z=.5+A1*Z3**2+A2*Z3**4+A3*Z3**6+A4*Z3**8+A5*Z3**10+A6*Z3**12
      RI(I)=4.*XBAR**2*(RJ1Z)**2
      GOTO10
   20 RI(I)=.20264237*(SIN(Z-.78539816))**2/Z/(SIN(RAD))**2
   10 RI(I)=RI(I)*RK
C INTEGRATE THE PHASE FUNCTION OVER THE SOLID ANGLE IN ORDER TO NORMALIZE
      SUM=0.
      DO 30 I=1,8999,2
      RAD=FLOAT(I)*.00017453293
      RAD1=RAD+.00017453293
   30 SUM=SUM+2.*RI(I)*SIN(RAD)+RI(I+1)*SIN(RAD1)
      SUM=(SUM+RI(9000))/300.*3.14159265/90.
      CD=2./SUM/12.56637061
C COMPUTE NORMALIZED PHASE FUNCTION OVER REQUIRED RANGE
      K=(100.*ANGLEL+.0001)
      L=(100.*DELTA+.0001)
      DO 40 I=0,NANG
      J=K+I*L
   40 RI(I)=CD*RI(J)
      RETURN
      END
C SUBROUTINE TO CALCULATE REFLECTION COMPONENT OF PHASE FUNCTION
C FOR IRREGULARLY SHAPED PARTICLES (FROM POLLACK AND CUZZI 1979)
C BY BLAIR EVANS/DREV
C JANUARY 1988
      SUBROUTINE REFPHASE(M,QR)
      REAL*4 RI(0:18000)
      COMPLEX*16 M
      COMMON /PHASE/RI
      COMMON /DATA/ ANGLEL,ANGLEU,DELTA,NANG,ILOW,IUP,RINC,ANGLEU1
	  SAVE
c		print *, 'routine REFPHASE'
      RM2=CDABS(M)**2
C UNNORMALIZED REFLECTION PHASE FUNCTION
      DO 10 I=0,18000
      RAD2=I*3.14159265/36000.
      S=SIN(RAD2)
      S2=S**2
      T=(ABS(RM2-1.+S2))**.5
   10 RI(I)=.5*(((S-T)/(S+T))**2+((RM2*S-T)/(RM2*S+T))**2)
C CALCULATE INTEGRAL OVER SOLID ANGLE IN ORDER TO NORMALIZE
      SUM=0.
      DO 20 I=1,17999,2
      RAD=I/18000.*3.14159265
      RAD1=(I+1.)/18000.*3.14159265
   20 SUM=SUM+4.*RI(I)*SIN(RAD)+2.*RI(I+1)*SIN(RAD1)
      SUM=SUM/300.*3.14159265/180.
      CR=2./SUM
      QR=1./CR
      CR=CR/12.56637061
C COMPUTE NORMALIZED PHASE FUNCTION OVER REQUIRED RANGE
      K=(100.*ANGLEL+.0001)
      L=(100.*DELTA+.0001)
      DO 30 I=0,NANG
      J=K+I*L
   30 RI(I)=CR*RI(J)
      RETURN
      END
