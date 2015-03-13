      PARAMETER (IAE1=1, NTP=6, ILG=12, ILEV=1, ISIZE=12)
      PARAMETER (NTR=ISIZE*NTP+IAE1-1)
      PARAMETER (NPTS=ILG*(ILEV+1))
      PARAMETER (NTYPES=6)

      REAL RHROW(ILG,ILEV+1), THROW(ILG,ILEV+1)
      REAL RHSIZE(ILG,ILEV,ISIZE),RHOP(ILG,ILEV,ISIZE), RHOP0(NTP)
      REAL AEROSIZE (2,ISIZE),RGRID(ILG,ILEV,NTR),TOTMAS(ILG,ILEV,ISIZE)
      REAL AERONUM(ILG,ILEV,ISIZE)
      REAL BINLIM(ISIZE+1)
      REAL FRACT(NTYPES),MMR

      REAL AVESIZE(ISIZE)
      REAL FMO(ILG,ILEV,ISIZE,NTP),FMSO(ILG,ILEV,ISIZE)
      REAL PHIAW1(ILG,ILEV,ISIZE),AMW(ILG,ILEV,ISIZE)
      REAL ANU(ILG,ILEV,ISIZE),A(ILG,ILEV,ISIZE)
      REAL BMIX(ILG,ILEV,ISIZE),FR1(ILG,ILEV,ISIZE)
      REAL FC(ILG,ILEV,ISIZE),FRC(ILG,ILEV,ISIZE)
      REAL AWX(ILG,ILEV,ISIZE,2),PHIX(ILG,ILEV,ISIZE)
      REAL DELIQS(ILG,ILEV,ISIZE),RECRYS(ILG,ILEV,ISIZE)

      CHARACTER*12 AERONAME(NTP)
      character*30 nommie
      REAL DENSITY(NTYPES)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DEBUT DES MODIFICATIONS JDG 2002-01-31

      INTEGER ii, iii, NTP2,binsw,numbin,FILELEN

      ii=1
      iii=1


C FIN DES MODIFICATIONS JDG 2002-01-31
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  SET THE RELATIVE HUMIDITIES
      DATA RHROW/NPTS*0.3/

C  SET THE TEMPERATURE
      DATA THROW/NPTS*273./

C  SET THE INPUT AEROSOL TYPES AND THEIR INPUT ORDER
C  THE FOLLOWING TYPES ARE AVAILABLE
c  MAKE SURE THAT NTP = NUMBER OF INPUT TYPES
C     1. AMMONIUM SULFATE (SULPHATE)
C     2. SEA SALT (SEA-SALT)
C     3. ORGANIC CARBON (OMCARBON)
C     4. AMMONIUM NITRATE (NITRATES)
C     5. BLACK CARBON (BLCARBON)
C     6. SOIL DUST (SOILDUST)
CCCCCCCCCCCCCCCCCCCCCCCC
C Changement des noms de particules SC JDG 2002-02-11
cccccccccccccccccccccccccc
C      DATA AERONAME/'SULFATE','SEA-SALT'/
      DATA AERONAME/'SULFATE','SEASALT','NITRATE',
     1            'BLACKCARBON','SOILDUST','OMCARBON'/

C  SET THE MASS FRACTIONS (HAS TO FOLLOW AERONAME ORDER GIVEN ABOVE):
C  SUM OF MASS FRACTIONS MUST BE 1.0
c      DATA FRACT/0.5, 0.5, 0.0, 0.0, 0.0, 0.0/

C  SET THE DRY DENSITIES OF EACH SPECIES (HAS TO FOLLOW AERONAME ORDER)
      DATA DENSITY/1760., 2240., 1725., 2300., 2500., 1760./
C      DATA DENSITY/1000., 2240., 1725., 2300., 2500., 1000./

C  BINLIM ARE THE DRY RADIUS LIMITS (MICRONS) OF THE SIZE BINS
      DATA BINLIM/0.005,0.01,0.02,0.04,0.08,0.16,0.32,0.64,1.28,2.56,
     1            5.12,10.24,20.48/
      Print*,'Bin limits definition (0=user defined, 1=Canadian Aerosol Module 
     1standard)?'
      read(*,*) binsw 
      if (binsw.eq.0) then
         print*,'Enter .bns.bmi root file name:'
	 read(*,*) nommie
	 FILELEN=INDEX(nommie,' ')-1
	 nommie=nommie(1:FILELEN)//'.bns.bmi'
         open(unit=1,file=nommie,status='old')
	    read(1,*)
	    read(1,*)
	    read(1,*) numbin
	    do i=1,numbin
	      read(1,*) n, BINLIM(n), BINLIM(n+1)
	    enddo
	 close(unit=1)
      endif

C  SET LONGITUDE ARRAY LIMITS
      IL1=1
      IL2=ILG

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DEBUT DES MODIFICATIONS SC JDG 2002-02-11
C  INITIALIZE RELATIVE HUMIDITY ARRAY
      DO L=1,ILEV+1
            RHROW(IL1,L)=0.1
            RHROW(IL1+1,L)=0.2
            RHROW(IL1+2,L)=0.3
            RHROW(IL1+3,L)=0.4
            RHROW(IL1+4,L)=0.5
            RHROW(IL1+5,L)=0.6
            RHROW(IL1+6,L)=0.7
            RHROW(IL1+7,L)=0.8
            RHROW(IL1+8,L)=0.9
            RHROW(IL1+9,L)=0.95
            RHROW(IL1+10,L)=0.98
            RHROW(IL1+11,L)=0.99
      END DO

C FIN DES MODIFICATIONS SC JDG 2002-02-11
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INITIALIZE AEROSOL SIZE BINS
      DO N=1,ISIZE
         AEROSIZE(1,N)=BINLIM(N)*1.0E-6
         AEROSIZE(2,N)=BINLIM(N+1)*1.0E-6
         AVESIZE(N)=(AEROSIZE(1,N)+AEROSIZE(2,N))/2.0
      END DO

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DEBUT DES MODIFICATIONS DU FICHIER JDG 2002-01-31

      OPEN (unit=10,FILE='humout')
      write(10,*) NTP,' 13 ',ISIZE,' * N types, N RH, N bins *'


C
C
C  Begining of the particle type loop
C
C
       NTP2=NTP
       do iii=1,NTP2
          if (iii.eq.1) then
             FRACT(1)=1.0
             FRACT(2)=0.0
             FRACT(3)=0.0
             FRACT(4)=0.0
             FRACT(5)=0.0
	     FRACT(6)=0.0
          elseif (iii.eq.2) then
             FRACT(1)=0.0
             FRACT(2)=1.0
             FRACT(3)=0.0
             FRACT(4)=0.0
             FRACT(5)=0.0
	     FRACT(6)=0.0
          elseif (iii.eq.3) then
             FRACT(1)=0.0
             FRACT(2)=0.0
             FRACT(3)=1.0
             FRACT(4)=0.0
             FRACT(5)=0.0
	     FRACT(6)=0.0
          elseif (iii.eq.4) then
             FRACT(1)=0.0
             FRACT(2)=0.0
             FRACT(3)=0.0
             FRACT(4)=1.0
             FRACT(5)=0.0
	     FRACT(6)=0.0
          elseif (iii.eq.5) then
             FRACT(1)=0.0
             FRACT(2)=0.0
             FRACT(3)=0.0
             FRACT(4)=0.0
             FRACT(5)=1.0
	     FRACT(6)=0.0
          elseif (iii.eq.6) then
	     FRACT(1)=0.0
             FRACT(2)=0.0
             FRACT(3)=0.0
             FRACT(4)=0.0
             FRACT(5)=0.0
	     FRACT(6)=1.0
          endif



C FIN DES MODIFICATIONS JDG 2002-01-31
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  MAKE SURE THE SUM OF MASS FRACTIONS EQUALS ONE
C  SET UP DENSITY ARRAY:  RHOP0
      FRACTOT=0
      DO NT=1,NTP
         FRACTOT=FRACTOT + FRACT(NT)
         RHOP0(NT)=DENSITY(NT)
      END DO
      IF (FRACTOT .NE. 1.0) THEN
         WRITE(6,*) 'WARNING: SUM OF MASS FRACTIONS NOT = 1'
      END IF

C  SETUP RGRID
C  MMR = TOTAL AEROSOL MASS MIXING RATIO (SAME FOR ALL SIZE BINS)
      MMR=1.0E-8
      DO NT=1,NTP
         DO N=1,ISIZE
            NO=N+ISIZE*(NT-1)+(IAE1-1)
            DO L=1,ILEV
               DO I=IL1,IL2
                  RGRID(I,L,NO)=MMR*FRACT(NT)
               END DO
            END DO
         END DO
      END DO
      
      CALL AEROPROP(       NTR,      NTP,      ILG,     ILEV,      IL1,
     1                     IL2,    ISIZE,   RHSIZE, AEROSIZE,     RHOP,
     2                   RHOP0,    RHROW,    THROW,    RGRID,  AERONUM,
     3                  TOTMAS,     PHIX,     IAE1, AERONAME,
     4                     FMO,     FMSO,   PHIAW1,      AMW,
     5                     ANU,        A,     BMIX,      FR1,       FC,
     6                     FRC,      AWX,   DELIQS,   RECRYS)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DEBUT DES MODIFICATIONS JDG 2002-01-31


      WRITE(10,*) AERONAME(iii), iii
         WRITE(10,*) '0.00000000  * RELATIVE HUMIDITY *'
         WRITE(10,*) 'Bin Number, Radius Ratio, Final Density'
         do 2001 ii=1,ISIZE
            WRITE(10,*) ii,' 1.00000000   ',DENSITY(iii)
 2001    continue
      DO M=1,ILG
         WRITE(10,*) RHROW(M,2)*100., ' * RELATIVE HUMIDITY *'
         WRITE(10,*) 'Bin Number, Radius Ratio, Final Density'
         do 2000 ii=1,ISIZE
            WRITE(10,*) ii,RHSIZE(M,1,ii)/AVESIZE(ii),RHOP(M,1,ii)
 2000    continue
      END DO



       ENDDO

      CLOSE(UNIT=10)


C FIN DES MODIFICATIONS JDG 2002-01-31
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      OPEN(26, FILE='AEROPROP.OUT')
      WRITE(6,*) 'MASS FRACTIONS:'
      WRITE(26,*) 'MASS FRACTIONS:'
      DO M=1,NTP
         WRITE(6,930) AERONAME(M), FRACT(M)
         WRITE(26,930) AERONAME(M), FRACT(M)
 930     FORMAT(1X,A8,2X,F6.3) 
      END DO
      DO M=1,ILG 
         WRITE(6,*) 'RELATIVE HUMIDITY = ',RHROW(M,2)*100.
         WRITE(6,*) 'RADIUS RATIO FOR 12 SIZE BINS:'
         WRITE(6,940) (RHSIZE(M,1,I)/AVESIZE(I),I=1,ISIZE)
 940     FORMAT(3(1X,4F11.6,/))
         WRITE(26,*) 'RELATIVE HUMIDITY = ',RHROW(M,2)*100.
         WRITE(26,*) 'RADIUS RATIO FOR 12 SIZE BINS:'
         WRITE(26,940) (RHSIZE(M,1,I)/AVESIZE(I),I=1,ISIZE)
C         WRITE(6,*) 'FINAL DENSITY FOR 12 SIZE BINS:'
C         WRITE(6,*) (RHOP(M,1,I),I=1,ISIZE)
      END DO
C      WRITE(6,*) 'RELATIVE HUMIDITY = '
C      WRITE(6,*) RHROW(ILG,2)*100.
C      WRITE(6,*) 'RADIUS RATIO FOR 12 SIZE BINS (I=ILG,L=ILEV):'
C      WRITE(6,*) (RHSIZE(ILG,ILEV,I)/AVESIZE(I),I=1,ISIZE)

      END

C***********************************************************************


      SUBROUTINE AEROPROP (NTR,      NTP,      ILG,     ILEV,      IL1,
     1                     IL2,    ISIZE,   RHSIZE, AEROSIZE,     RHOP,
     2                   RHOP0,    RHROW,    THROW,    RGRID,  AERONUM,
     3                  TOTMAS,     PHIX,     IAE1, AERONAME,   
     4                     FMO,     FMSO,   PHIAW1,      AMW,
     5                     ANU,        A,     BMIX,      FR1,       FC,
     6                     FRC,      AWX,   DELIQS,   RECRYS)
C-----------------------------------------------------------------------
C     PURPOSE:
C     --------
C     AEROSOL PROPERTY CALCULATION                                     
C
C     HISTORY:
C     --------
C
C     * MAY  5/99 - G. LESINS   ADDED ORGANIC AEROSOL TYPE - TREATED AS SULPHATE
C     * JUN 19/98 - G. LESINS   ADDED NITRATE, BC, DUST
C     *                         ADDED DELIQUESCENCE & RECRYST DATA
C     * MAR 13/98 - G. LESINS   MODIFIED PARAMETERIZATION
C     *                         VALID FOR ALL SIZES AND RH
C     *                         USES EMPIRICAL OSMOTIC COEFFICIENTS
C     * JUN 25/97 - S.L. GONG   COMBINE PREVIOUS AERODEN AND GROWTH
C     *                         ROUTINES TO COMPUTE THE AEROSOL
C     *                         AMBIENT PROPERTIES.
C     *
C     * JAN 19/96 - S.L. GONG   VECTORIZED THE WHOLE PROGRAM AND ADD
C     *                         WORKING SPACES.
C     *
C     * DEC  5/94 - S.L. GONG   FIRST VERSION
C     *
C
C     ARGUMENT LIST
C     NTR - TOTAL NUMBER OF TRACE SUBSTANCES (GASES AND AEROSOLS)
C     NTP - NUMBER OF AEROSOL TYPES
C     ILG - NUMBER OF LONGITUDE GRID POINTS
C     ILEV - NUMBER OF VERTICAL LEVELS
C     IL1 - MINIMUM INDEX FOR ILG
C     IL2 - MAXIMUM INDEX FOR ILG
C     ISIZE - NUMBER OF SIZE BINS
C     RHSIZE - WET RADIUS ***OUTPUT***
C     AEROSIZE - DRY RADIUS
C     RHOP - FINAL WET DENSITY ***OUTPUT***
C     RHOP0 - DENSITY FOR EACH AEROSOL TYPE
C     RHROW - RELATIVE HUMIDITY
C     THROW - TEMP
C     RGRID - MASS MIXING RATIO FOR EACH TRACE SUBSTANCE
C     AERONUM - NUMBER CONCONCENRATION (#/KG)  ***OUTPUT***
C     TOTMAS - TOTAL MASS MIXING RATIO FOR EACH BIN (ALL cOMPONENTS) 
C                 ***OUTPUT***
C     IAE1 - INDEX OF FIRST AEROSOL IN TRACE SUBSTANCE LIST (NTR)
C     AERONAME - NAME OF EACH AEROSOL TYPE
C     THE REMAINING ARGUMENTS ARE WORKING ARRAYS ADDED BY LESINS
C     PHIX - OSMOTIC COEFFICIENT
C     FMO - MASS FRACTION OF EACH DRY COMPONENT
C     FMSO - MASS FRACTION OF TOTAL SOLUBLE PART
C     PHIAW1 - OSMOTIC COEFFICIENT AT WATER ACTIVITY=1
C     AMW - AVERAGE MOLECULAR WEIGHT OF SOLUBLE PART
C     ANU - AVERAGE NU (ION NUMBER) OF SOLUBLE PART
C     A - SURFACE TENSION COEFFICIENT IN KOHLER EQUATION
C     BMIX - SOLUTE COEFFICIENT IN KOHLER EQUATION
C     FR1 - ESTIMATED RADIUS RATIO AT RELATIVE HUMIDITY=1
C     FC - CRITICAL RELATIVE HUMIDITY (SUPERSATURATED)
C     FRC - CRITICAL RADIUS RATIO
C     AWX - ITERATED VALUE FOR WATER ACTIVITY
C     DELIQS - AVERAGE DELIQUESCENCE POINTS FOR MIXTURE
C     RECRYS - AVERAGE RECRYSTALLIZATION POINTS FOR MIXTURE
C
C     *
C-----------------------------------------------------------------------     
C
C IN THE PARAMETER STATEMENT:
C   NUMSOL = TOTAL NUMBER OF SOLUBLE AEROSOL TYPES POSSIBLE
C   NUMINS = TOTAL NUMBER OF INSOLUBLE AEROSOL TYPES POSSIBLE
      PARAMETER (NUMSOL=4, NUMINS=2, NUMTYPES=NUMSOL+NUMINS)
      REAL RHROW(ILG,ILEV+1), THROW(ILG,ILEV+1)
      REAL RHSIZE(ILG,ILEV,ISIZE),RHOP(ILG,ILEV,ISIZE), RHOP0(NTP)
      REAL AEROSIZE (2,ISIZE),RGRID(ILG,ILEV,NTR),TOTMAS(ILG,ILEV,ISIZE)
      REAL AERONUM(ILG,ILEV,ISIZE)
      CHARACTER*12 AERONAME(NTP), SORTNAME(NUMTYPES)
      
C FOLLOWING SECTION CONTAINS DATA ARRAYS FOR ALL AEROSOL TYPES
      REAL NU(NUMTYPES),MW(NUMTYPES),PHIK(NUMTYPES),B(NUMTYPES)
      REAL DELIQ(NUMTYPES),RECRY(NUMTYPES)
      REAL PHIT(11,NUMSOL)
      REAL NU_(NUMTYPES),MW_(NUMTYPES)
      REAL DELIQ_(NUMTYPES),RECRY_(NUMTYPES)
      REAL PHIT_(11,NUMSOL)
      CHARACTER*12NAMSOL(NUMSOL), NAMINS(NUMINS)

C FOLLOWING SECTION CONTAINS WORKING ARRAYS   
      REAL AVESIZE(25)
      REAL FMO(ILG,ILEV,ISIZE,NTP),FMSO(ILG,ILEV,ISIZE)
      REAL PHIAW1(ILG,ILEV,ISIZE),AMW(ILG,ILEV,ISIZE)
      REAL ANU(ILG,ILEV,ISIZE),A(ILG,ILEV,ISIZE)
      REAL BMIX(ILG,ILEV,ISIZE),FR1(ILG,ILEV,ISIZE)
      REAL FC(ILG,ILEV,ISIZE),FRC(ILG,ILEV,ISIZE)
      REAL AWX(ILG,ILEV,ISIZE,2), PHIX(ILG,ILEV,ISIZE)
      REAL DELIQS(ILG,ILEV,ISIZE),RECRYS(ILG,ILEV,ISIZE)

      REAL MWW,RW,DENW,SFCTEN
      REAL*8 R,Q,D
      INTEGER NTPS, NUMITER, SORTNUM(NUMTYPES), LN(NUMTYPES)
      LOGICAL DELIQCRY

C AEROSOL TYPE DATA ARE IN THE FOLLOWING ORDER:
C  1. SEA SALT
C  2. (NH4)2SO4
C  3. NH4NO3
C  4. ORGANIC
C  5. BLACK CARBON
C  6. SOIL DUST
C
C NAMSOL ARE THE PERMITTED SOLUBLE AEROSOL NAMES
      DATA NAMSOL/'SEASALT','SULFATE','NITRATE','OMCARBON'/
c NAMINS ARE THE PERMITTED INSOLUBLE AEROSOL NAMES
      DATA NAMINS/'BLACKCARBON','SOILDUST'/

C MW ARE THE MOLECULAR WEIGHTS OF THE DRY AEROSOL COMPONENTS
      DATA MW_ /67.180, 132.1342, 80.0435, 132.1342, 12.011, 60.08/
C
C NU ARE THE IONS PER SOLUTE MOLECULE
      DATA NU_ /2.165, 3.0, 2.0, 3.0, 0., 0./

C DELIQ IS THE DELIQUESCENCE RELATIVE HUMIDITY
C RECRY IS THE RECRYSTALLIZATION RELATIVE HUMIDITY
      DATA DELIQ_/0.74, 0.80, 0.62, 0.80, 1.1, 1.1/
      DATA RECRY_/0.45, 0.37, 0.25, 0.37, 1.1, 1.1/
C
C MWW IS THE MOLECULAR WEIGHT OF WATER
      DATA MWW/18.015/
C
C RW IS THE GAS CONSTANT FOR WATER VAPOUR ( J/(KG K) )
      DATA RW/461.51/
C
C DENW IS THE DENSITY OF WATER ( KG / M^3 )
      DATA DENW/1000./
C
C SFCTEN IS THE SURFACE TENSION BETWEEN WATER AND AIR ( J / M^2 )
      DATA SFCTEN/0.076/
C
C NTPS IS THE INDEX NUMBER OF THE LAST SOLUBLE AEROSOL TYPE (<= NTP)
C IF NTPS=NTP THEN THERE ARE NO INSOLUBLE COMPONENTS
C
C NUMITER IS THE NUMBER OF ITERATIONS PERFORMED FOR KOHLER EQUATION
C NUMITER=2 SHOULD GIVE BETTER THAN 1% ACCURACY
C NUMITER MUST NOT EXCEED THE DECLARED SIZE OF THE LAST INDEX IN AWX
      DATA NUMITER/2/
C
C OCFACTOR IS A FACTOR USED TO MULTIPLY THE ORGANIC CARBON OSMOTIC
C   COEFFICIENTS TO FORCE A DIFFERENCE FROM SULPHATE
      DATA OCFACTOR/1./
C
C PHIT(J,I) is the phi(aw) table of coefficients
C I is the solute index:
C     1 = sea salt
C     2 = (NH4)2SO4
C     3 = NH4NO3
C     4 = organic
C I MUST BE DIMENSIONED TO EQUAL NTPS
C J is the data :
C  1 = minimum aw for polynomial fit
C  2 = aw break point between two polynomials
C  3 = x^3 coef for aw > awbreak
C  4 = x^2 coef
C  5 = x^1 coef
C  6 = x^0 coef
C  7 = x^4 coef for aw <= awbreak
C  8 = x^3 coef
C  9 = x^2 coef
C  10= x^1 coef
C  11= x^0 coef
      DATA PHIT_/0.44,0.92, 410.74729, -1138.2693, 1049.2792,-320.74562,
     1 -5.79690208, 17.7685336, -22.5253540, 11.8087027, -0.48210984,
     2 0.39, 0.92, 457.060777, -1280.47495, 1194.81750, -370.739425,
     3 -1.62440470, 4.07342346, -5.61205075, 3.873682106, -0.216021389,
     4 0.275, 0.81, 7.6174049, -19.354181, 17.103802, -4.5561686,
     5 -1.1108526, 3.7035588, -5.1408203, 4.0788267, -0.77326108,
     6 0.39, 0.92, 457.060777, -1280.47495, 1194.81750, -370.739425,
     7 -1.62440470, 4.07342346, -5.61205075, 3.873682106, -0.216021389/
C
C  DELIQCRY IS A LOGICAL SWITCH TO HANDLE SIZE IF RH IS BETWEEN
C    CRYSTALLIZATION AND DELIQUESCENCE.  IF =.TRUE. THEN FINAL SIZE IS
C    WEIGHTED AVERAGE BETWEEN DRY AND DELIQUESCED SIZES.  IF =.FALSE.
C    THEN FINAL SIZE IS THE FULLY DELIQUESCED SIZE.
      DATA DELIQCRY/.TRUE./
C
      CALL FILZRO(RHOP, ILG, ILEV, ISIZE)
      CALL FILZRO(TOTMAS,ILG, ILEV, ISIZE)
      CALL FILZRO(FMSO,ILG,ILEV,ISIZE)
      CALL FILZRO(PHIAW1,ILG,ILEV,ISIZE)
      CALL FILZRO(AMW,ILG,ILEV,ISIZE)
      CALL FILZRO(ANU,ILG,ILEV,ISIZE)
      CALL FILZRO(DELIQS,ILG,ILEV,ISIZE)
      CALL FILZRO(RECRYS,ILG,ILEV,ISIZE)
      CALL FILZRO2(FMO,ILG,ILEV,ISIZE,NTP)
      CUB=1./3.

C        SORT THE AEROSOL NAMES (SOLUBLES FIRST, THEN INSOLUBLES)
      INEXT=1
      ILAST=NTP
      DO NT=1,NTP
         IFOUND=0
         DO J=1,NUMSOL
            IF (AERONAME(NT) .EQ. NAMSOL(J)) THEN
               SORTNAME(INEXT) = AERONAME(NT)
               SORTNUM(INEXT) = J
               LN(INEXT) = NT
               INEXT = INEXT + 1
               IFOUND = 1
            ENDIF
         ENDDO
         IF (IFOUND .EQ. 0) THEN
            DO J=1,NUMINS
               IF (AERONAME(NT) .EQ. NAMINS(J)) THEN
                  SORTNAME(ILAST) = AERONAME(NT)
                  SORTNUM(ILAST) = J + NUMSOL
                  LN(ILAST) = NT
                  ILAST = ILAST - 1
                  IFOUND = 1
               ENDIF
            ENDDO
         ENDIF
         IF (IFOUND .EQ. 0) THEN
            WRITE(6,*) '***ERROR*** UNKNOWN AEROSOL NAME'
            STOP
         ENDIF
         IF (NT .EQ. NTP) NTPS = INEXT - 1
      ENDDO
C
C      DO NT=1,NTP
C         WRITE(6,600) AERONAME(NT),LN(NT),SORTNAME(NT),SORTNUM(NT)
C 600     FORMAT(1X,A8,5X,I1,5X,A8,5X,I1)
C      ENDDO
C
C
C  ADJUST OSMOTIC COEFFICIENTS FOR ORGANIC CARBON
      DO J=3,11
         PHIT_(J,4)=OCFACTOR*PHIT_(J,4)
      ENDDO

C  ARRANGE THE DATA ARRAYS IN THE CORRECT ORDER
      DO NT=1,NTP
         MW(NT)=MW_(SORTNUM(NT))
         NU(NT)=NU_(SORTNUM(NT))
         DELIQ(NT)=DELIQ_(SORTNUM(NT))
         RECRY(NT)=RECRY_(SORTNUM(NT))
      ENDDO
      DO NT=1,NTPS
         DO J=1,11
            PHIT(J,NT)=PHIT_(J,SORTNUM(NT))
         ENDDO
      ENDDO

C
C     * TOTAL DRY MASS MIXING RATIO & DRY AEROSOL COMPOSITE DENSITY
C       OF AEROSOL IN EACH BIN
C
C                    m1+m2+m3
C       RHO = -------------------------
C              m1/rho1+m2/rho2+m3/rho3
c
      DO NT=1,NTP
         NT0=LN(NT)
         DO N=1,ISIZE
            NO= N+ISIZE*(NT0-1)+(IAE1-1)
            DO L=1,ILEV
              DO I=IL1,IL2
                 TRAMASS=AMAX1(1.0E-33, RGRID(I,L,NO))
                 TOTMAS(I,L,N)=TOTMAS(I,L,N)+TRAMASS
                 RHOP(I,L,N)=RHOP(I,L,N)+TRAMASS/RHOP0(NT0)
              END DO
            END DO
         END DO
      END DO
C
C COMPUTE THE MASS FRACTION OF EACH DRY AEROSOL COMPONENT, FMO
C
      DO NT=1,NTP
         NT0=LN(NT)
         DO N=1,ISIZE
            NO= N+ISIZE*(NT0-1)+(IAE1-1)
            DO L=1,ILEV
              DO I=IL1,IL2
                 TRAMASS=AMAX1(1.0E-33, RGRID(I,L,NO))
                 FMO(I,L,N,NT)=TRAMASS/TOTMAS(I,L,N)
              END DO
            END DO
         END DO
      END DO
C
C      COMPUTE PHI AT AW=1 FOR EACH SOLUTE
C      COMPUTE KOHLER B FACTOR FOR EACH SOLUTE
C
      DO NT=1,NTPS
         PHIK(NT)=PHIT(3,NT)+PHIT(4,NT)+PHIT(5,NT)+PHIT(6,NT)
         B(NT)=NU(NT)*PHIK(NT)*MWW*RHOP0(LN(NT))/(MW(NT)*DENW)
      END DO
C
C COMPUTE THE SOLUTE MASS FRACTION, FMSO
C COMPUTE AVERAGE NU AND AVERAGE MOLECULAR WEIGHT OF SOLUBLE PART
C COMPUTE PHI AT AW=1 FOR THE MIXED AEROSOL
C COMPUTE AVERAGE DELIQUESCENCE AND RECRYSTALLIZATION POINTS
      DO NT=1,NTPS
      DO N=1,ISIZE
         DO L=1,ILEV
           DO I=IL1,IL2
              FMSO(I,L,N)=FMSO(I,L,N)+FMO(I,L,N,NT)
              AMW(I,L,N)=AMW(I,L,N)+FMO(I,L,N,NT)/MW(NT)
              ANU(I,L,N)=ANU(I,L,N)+NU(NT)*FMO(I,L,N,NT)/MW(NT)
              PHIAW1(I,L,N)=PHIAW1(I,L,N)+PHIK(NT)*NU(NT)*FMO(I,L,N,NT)
     1                        /MW(NT)
              DELIQS(I,L,N)=DELIQS(I,L,N)+FMO(I,L,N,NT)*DELIQ(NT)
              RECRYS(I,L,N)=RECRYS(I,L,N)+FMO(I,L,N,NT)*RECRY(NT)
           END DO
         END DO
      END DO
      END DO
C
C COMPUTE THE AVERAGE DRY AEROSOL RADIUS, AVESIZE
C
      DO N=1,ISIZE
         AVESIZE(N)=(AEROSIZE(1,N)+AEROSIZE(2,N))/2.0
      END DO
C
      DO N=1,ISIZE
         RWI=(AEROSIZE(1,N)+AEROSIZE(2,N))/2.0
         DO L=1,ILEV
           DO I=IL1,IL2
              RHOP(I,L,N)=TOTMAS(I,L,N)/RHOP(I,L,N)
              IF (FMSO(I,L,N) .NE. 0.) THEN
                 AMW(I,L,N)=FMSO(I,L,N)/AMW(I,L,N)
                 ANU(I,L,N)=ANU(I,L,N)*AMW(I,L,N)/FMSO(I,L,N)
                 PHIAW1(I,L,N)=PHIAW1(I,L,N)*AMW(I,L,N)/(FMSO(I,L,N)*
     1                        ANU(I,L,N))
                 DELIQS(I,L,N)=DELIQS(I,L,N)/FMSO(I,L,N)
                 RECRYS(I,L,N)=RECRYS(I,L,N)/FMSO(I,L,N)
              ELSE
                 DELIQS(I,L,N)=1.1
                 RECRYS(I,L,N)=1.1
              END IF
C
C     * AEROSOL NUMBER CONCENTRATION (#/KG_AIR)
C      NOTE:  4.189 = 4 * PI / 3
C
              AERONUM(I,L,N)=TOTMAS(I,L,N)/(4.189*
     1                                   RWI*RWI*RWI*RHOP(I,L,N))
           END DO
         END DO
      END DO

C      WRITE(6,*) 'FMO:'
C      WRITE(6,*) (FMO(1,1,1,M),M=1,NTP)
C      WRITE(6,*) 'FMSO= ',FMSO(1,1,1)
C      WRITE(6,*) 'MW:'
C      WRITE(6,*) (MW(M),M=1,NTP)
C      WRITE(6,*) 'AMW= ',AMW(1,1,1)
C      WRITE(6,*) 'NU:'
C      WRITE(6,*) (NU(M),M=1,NTP)
C      WRITE(6,*) 'ANU= ',ANU(1,1,1)
C      WRITE(6,*) 'RHOP0:'
C      WRITE(6,*) (RHOP0(LN(M)),M=1,NTP)

C
C  COMPUTE KOHLER A' FACTOR AND B FACTOR FOR MIXTURE
C  COMPUTE RADIUS RATIO AT RH=1 (FR1)
C  COMPUTE CRITICAL RADUS RATIO (FRC)
C  COMPUTE CRITICAL RELATIVE HUMIDITY (FC)
C
      DO N=1,ISIZE
         DO L=1,ILEV
           DO I=IL1,IL2
              A(I,L,N)=2.0*SFCTEN/(DENW*RW*THROW(I,L+1)*AVESIZE(N))
              IF (AMW(I,L,N) .NE. 0) THEN
                 BMIX(I,L,N)=ANU(I,L,N)*PHIAW1(I,L,N)*MWW*RHOP(I,L,N)*
     1                     FMSO(I,L,N)/(AMW(I,L,N)*DENW)
              ELSE
                 BMIX(I,L,N)=0.0
              END IF
              Q=-BMIX(I,L,N)/(3.0*A(I,L,N))
C              R=0.5
              D=Q*Q*Q+0.25
              IF(D .LT. 0.0 .AND. Q .LT. 0.0) THEN
                 THETA=ACOS(0.5/SQRT(-Q*Q*Q))
                 FR1(I,L,N)=2.0*SQRT(-Q)*COS(THETA/3.0)
              ELSE
                 DSR=SQRT(D)
                 FR1(I,L,N)=(0.5+DSR)**CUB+(0.5-DSR)**CUB
              END IF

C*******************
C  THIS SECTION CAN BE COMMENTED OUT IF YOU ARE NOT INTERESTED
C    IN COMPUTING THE CRITICAL RADIUS AND CRITICAL RELATIVE HUMIDITY

              Q=Q*3.0
C              R=1.0
              D=Q*Q*Q+1.0
              IF(D .LT. 0.0 .AND. Q .LT. 0.0) THEN
                 THETA=ACOS(1.0/SQRT(-Q*Q*Q))
                 FRC(I,L,N)=2.0*SQRT(-Q)*COS(THETA/3.0)
              ELSE
                 DSR=SQRT(D)
                 FRC(I,L,N)=(1.0+DSR)**CUB+(1.0-DSR)**CUB
              END IF
              FRCTEST=FRC(I,L,N)**3.0
              IF (FRCTEST .EQ. 1.0) THEN
                 FC(I,L,N)=1.0
              ELSE
                 FC(I,L,N)=EXP(A(I,L,N)/FRC(I,L,N)-BMIX(I,L,N)/
     1                  (FRCTEST-1.0))
              END IF
C*********************
C
           END DO
         END DO
      END DO
C
C SOLVE FOR THE WET RADIUS AFTER WATER UPTAKE
      DO ITER=1,NUMITER
         CALL FILZRO(PHIX, ILG,ILEV,ISIZE)
         DO N=1,ISIZE
         DO NT=1,NTPS
            DO L=1,ILEV
               DO I=IL1,IL2
                 RH=AMIN1(1.0,AMAX1(0.0,RHROW(I,L+1)))
c                 IF(RH .GT. 1.0) RH=1.0
                 IF(ITER .EQ. 1) THEN
                    AWX(I,L,N,ITER)=RH*EXP(-A(I,L,N)/(0.8*FR1(I,L,N)))
                    IF(AWX(I,L,N,ITER) .GT. 1.0) AWX(I,L,N,ITER)=1.0
                    AWX1=AWX(I,L,N,ITER)
                 ELSE
                    AWX1=AWX(I,L,N,ITER-1)
                 END IF
                 IF(AWX1 .GT. PHIT(2,NT)) THEN
                   PHIX(I,L,N)=PHIX(I,L,N)+(PHIT(3,NT)*AWX1*AWX1*AWX1 +
     1                  PHIT(4,NT)*AWX1*AWX1 +PHIT(5,NT)*AWX1 +
     2                   PHIT(6,NT))*NU(NT)*FMO(I,L,N,NT)/MW(NT)
                 ELSE
                   PHIX(I,L,N)=PHIX(I,L,N)+(PHIT(7,NT)*AWX1*AWX1*AWX1
     1                             *AWX1+PHIT(8,NT)*AWX1*AWX1*AWX1 +
     2                      PHIT(9,NT)*AWX1*AWX1 + PHIT(10,NT)*AWX1 +
     3                      PHIT(11,NT))*NU(NT)*FMO(I,L,N,NT)/MW(NT)
                 END IF
               END DO
            END DO
         END DO

            DO L=1,ILEV
              DO I=IL1,IL2
                 RH=AMIN1(1.0,AMAX1(0.0,RHROW(I,L+1)))
                 IF(ITER .EQ. 1) THEN
                    AWX1=AWX(I,L,N,ITER)
                 ELSE
                    AWX1=AWX(I,L,N,ITER-1)
                 END IF
                 IF (FMSO(I,L,N) .NE. 0.) THEN
                    PHIX(I,L,N)=PHIX(I,L,N)*AMW(I,L,N)/(FMSO(I,L,N)
     1                                       *ANU(I,L,N))
                    FRX1=(1.0-ANU(I,L,N)*PHIX(I,L,N)*MWW*RHOP(I,L,N)*
     1                  FMSO(I,L,N)/(AMW(I,L,N)*DENW*LOG(AWX1)))**CUB
                 ELSE
                    PHIX(I,L,N)=0.
                    FRX1=1.0
                 END IF
C
C*********************
C  THIS SECTION GREATLY IMPROVES ACCURACY FOR RH ~ 1
C  IT CAN BE COMMENTED OUT IF SPEED IS MORE IMPORTANT THAN ACCURACY
C
                 IF(RH .GT. 0.98 .AND. FRX1 .GT. 1.0) THEN
                    FRX3=FRX1*FRX1*FRX1
                    BPR=ANU(I,L,N)*PHIX(I,L,N)*MWW*RHOP(I,L,N)*
     1                   FMSO(I,L,N)*FRX3/(AMW(I,L,N)*DENW*(FRX3-1.0))
                    Q=-A(I,L,N)/(3.0*BPR)
                    R=-LOG(RH)/(2.0*BPR)
                    D=Q*Q*Q+R*R
                    IF(D .LT. 0.0 .AND. Q .LT. 0.0) THEN
                       THETA=ACOS(R/SQRT(-Q*Q*Q))
                       FRX1=1.0/(2.0*SQRT(-Q)*COS(THETA/3.0))
                    ELSE
                       vv=ABS(R+SQRT(D))
                        dd=ABS(R-SQRT(D))
                       FRX1=1.0/((vv)**CUB + (dd)**CUB)
                    END IF
                 END IF
C**********************
C
                 AWX(I,L,N,ITER)=RH*EXP(-A(I,L,N)/FRX1)
                 RHSIZE(I,L,N)=AVESIZE(N)*FRX1
              END DO
            END DO
         END DO
      END DO
C
C ADJUST SIZE IF RH < DELIQUESCENCE POINT
C COMPUTE THE MEAN DENSITY OF THE WET AEROSOL
C
      DO N=1,ISIZE
         DO L=1,ILEV
           DO I=IL1,IL2
              RH=RHROW(I,L+1)
              IF( RH .LT. DELIQS(I,L,N)) THEN
                 IF( RH .LT. RECRYS(I,L,N)) THEN
                    RHSIZE(I,L,N)=AVESIZE(N)
                 ELSE
                    IF( DELIQCRY ) RHSIZE(I,L,N)=AVESIZE(N)+
     1                 (RHSIZE(I,L,N)-AVESIZE(N))*
     2                 (RH-RECRYS(I,L,N))/(DELIQS(I,L,N)-RECRYS(I,L,N))
                 END IF
              END IF
              FF=AVESIZE(N)/RHSIZE(I,L,N)
              RHOP(I,L,N)=DENW+AMAX1(0.,FF*FF*FF*(RHOP(I,L,N)-DENW))
           END DO
         END DO
      END DO
C
C

      RETURN
      END

C***********************************************************************

C  SUBROUTINE FILZRO ZEROS ARRAY A
C  THIS SUBROUTINE IS NOT NEEDED IN THE AES VERSION
      SUBROUTINE FILZRO(A, ILG, ILEV, ISIZE)
      REAL A(ILG,ILEV,ISIZE)
      DO N=1,ISIZE
         DO L=1,ILEV
            DO I=1,ILG
               A(I,L,N)=0.0
            END DO
         END DO
      END DO
      RETURN
      END

      SUBROUTINE FILZRO2(A, ILG, ILEV, ISIZE, NTP)
      REAL A(ILG,ILEV,ISIZE,NTP)
      DO M=1,NTP
        DO N=1,ISIZE
           DO L=1,ILEV
              DO I=1,ILG
                 A(I,L,N,M)=0.0
              END DO
           END DO
        END DO
      END DO
      RETURN
      END



