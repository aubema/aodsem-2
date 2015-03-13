C      PROGRAMME  DE  FIT GFB
C
C      VERSION 1.1
C
C
C      LOGICIEL POUR L'AJUSTEMENT DE N GAUSSIENNES
C
C      I/O ADAPTE AU SYSTEME DE TRAITEMENT D'IMAGES ASTRONOMIQUE
C      IRAF DE KPNO
C 
C      REFERENCES:DATA REDUCTION AND ERROR ANALYSIS FOR THE PHYSICAL
C                 SCIENCES, PHILIP R. BEVINGTON, McGraw-Hill, 1969
c      UNIVERSITE LAVAL  1 mai 1991
C
C
C      COPYRIGHT MARTIN AUBE 1991
C      PERMISSION DE DIFFUSER ET MODIFIER A DES FINS NON LUCRATIVES
C
C
C      PROGRAMME PRINCIPAL
C      LE LOGICIEL PERMET DE DECOMPOSER UN PROFIL DE RAIE ET LA
C      COMPOSANTE CONTINUE VOISINE EN UN CONTINU DE LA FORME:
C      A1 + A2 * X + A3 X**2
C      ET UNE RAIE COMPOSEE DE PLUSIEURS GAUSSIENNES.
C      L'UTILISATEUR DEVRAS SELECTIONNER UNE SECTION UNIDIMENTIONNELLE
C      DU SPECTRE DE DIMENSION REDUITE LOCALISEE AU VOISINAGE
C      DE LA RAIE D'INTERET OU UN FICHIER TEXTE x-y OU y.
C
C
C      LES DIFFERENTS LOGICIELS
C
C
C          LES LOGICIELS gf* POUR L'AJUSTEMENT DE PLUSIEURS GAU-
C      SSIENNES NE SONT PAS TOTATEMENT INTEGRES A L'ENVIRONNEMENT
C      IRAF EN CE SENS QU'ILS NE FONT PAS APPEL A EDITPARAMETERS
C      POUR L'ENTREE DES PARAMETRES.  LES LOGICIELS SONT INTERACTIFS.
C      
C
C 
C
C
C     SORTIE:  LA SORTIE EST SECTIONNEE EN QUATRE FICHIERS
C               A) FICHIER resfit.dat (parametres, etc)
C               B) FICHIER resfit.res (x, y-yfit i.e. residu)
C               C) FICHIER resfit.fit (x, yfit)
C               D) FICHIER resfit.xy  (x, y)
C
C =============================================================
C
C     DONNEES INITIALES
C
C
      PROGRAM GFB
      DIMENSION X(2048), Y(2048), SIGMAY(2048), A(63), DELTAA(63)
     +, SIGMAA(63), YFIT(2048), DFLAGA(63)
      REAL  MEMCHI, mmi, DFLAGA, A, DELTAA, DY,fact
      CHARACTER*40 IMAGE
      CHARACTER*18 , NOMFI, RESULT
      CHARACTER*6 FILE, QUE3
      CHARACTER*1  QUE2
      INTEGER   NGAUS, NORDP,n0,ngaussmax
      FILE='resfit'
      RESULT='resfit.dat'
      MODE=-1
      DO 5, K=1,60
        DFLAGA(K)=1.
  5   CONTINUE
      OPEN(UNIT=3,FILE='gaufit.in',STATUS='unknown')
        READ(3,10) NOMFI
        READ(3,*) n0
        READ(3,*) NPTS
        READ(3,*) ngaussmax
        READ(3,*) fact
      CLOSE(UNIT=3)
   10 FORMAT(A)
C
C      LECTURE DU FICHIER PROFIL
C
   15 FORMAT (I3)
      OPEN(UNIT=1,FILE=NOMFI,STATUS='unknown')
       mmi=0.
       DO 20 M=1, NPTS
       READ (1,*,end=22) X(M), Y(M)
       mmi=mmi+1.
  20  CONTINUE
 22   CLOSE(UNIT=1)
      if (mmi.lt.NPTS) then
        NPTS=mmi
      endif
C
C     ROUTINE POUR L'ACCES AUX FONCTIONS GRAPHIQUES DE IRAF
C
      OPEN(UNIT=2,FILE='gaufit.gfx',STATUS='unknown')
        WRITE(2,*) 'plot'
        WRITE(2,*) 'graph(title="DATA + FIT",input="resfit.fit,resfit.xy
     +",wx1=0,wx2=0, wy1=0, wy2=0,vx1=0,vx2=1,'
        WRITE(2,*) 'vy1=0,vy2=1,pointmode=no,append=no,>G"gaufita.mos")'
        write(2,*) 'graph (title="COMPOSANTES",input="@gaufit.gau",wy1=0
     +,wy2=0,vx1=0,vx2=1,vy1=0,vy2=1,append=no,'
        write(2,*) 'wx1=',x0,',wx2=',(npts-1)*x0d+x0,',pointmode=no,>G"g
     +aufitc.mos")'
        WRITE(2,*)'graph(title="  RESIDUS  ",vx1=0,vx2=1,vy1=0,vy2=1,inp
     +ut="resfit.res",append=no,wx1=0,wx2=0,'
        WRITE(2,*) 'wy1=0,wy2=0,pointmode=no,>G"gaufitb.mos")'
        WRITE(2,*) 'gkimos(input="gaufita.mos,gaufitc.mos,gaufitb.mos",n
     +y=3,nx=1,rotate=no,interactive=yes,fill=yes)'
        WRITE(2,*) 'del gaufita.mos,gaufitb.mos,gaufitc.mos verify=no'
      CLOSE(UNIT=2)
C
C
C     PARAMETRES INITIAUX POUR LE FIT
C
C
C     STATISTIQUE
C
      CALL STATi( X, Y, Y1, Y2, X1, X2, Y1MEM, DY, YMAX, NPTS)
C
C     NORMALISATION DES VARIABLES
C
      CALL NORM (Y, Y1, Y2, YMAX, DY, NPTS)
C
      PRINT*
      PRINT*
  27  PRINT*,'NOMBRE DE PROFILS GAUSSIENS  (MAX ',ngaussmax,')?' 
      READ(*,30,err=27) NGAUS
      IF (NGAUS.GT.ngaussmax) THEN
         GOTO 27
      ENDIF
  29  PRINT*,'ORDRE DU POLYNOME REPRESENTANT LE CONTINU (MAX 2)?'
      READ(*,30,err=29) NORDP
      IF (NORDP.EQ.-1) THEN
        PRINT*
        PRINT*,'POLYNOME CHOISI: 0. EN TOUS POINTS.'
      ELSEIF (NORDP.EQ.0) THEN
        PRINT*
        PRINT*,'POLYNOME CHOISI: A(1) (TERME CONSTANT)'
      ELSEIF (NORDP.EQ.1) THEN
        PRINT*
        PRINT*,'POLYNOME CHOISI: A(1) + A(2) X'
      ELSEIF (NORDP.EQ.2) THEN
        PRINT*
        PRINT*,'POLYNOME CHOISI: A(1) + A(2) X + A(3) X^2'
      ELSE
        PRINT*,'(-1,0,1,2)'
        GOTO 29
      ENDIF
      NTERMS=3.*NGAUS+NORDP+1.
   30 FORMAT(I3)
      PRINT*
   31 PRINT*,'DESIREZ-VOUS EXERCER DES CONTRAINTES? (o/[n])',QUE2
      READ 10, QUE2
      PRINT*
      IF  ((QUE2.eq.'O').OR.(QUE2.eq.'o')) THEN
        QUE2='o'
      ELSEIF ((QUE2.eq.'').OR.(QUE2.eq.'n')) THEN
        QUE2='n'
      ELSE
        GOTO 31
      ENDIF
       IF (QUE2.EQ.'o') THEN
         PRINT 55
         PRINT 54
         PRINT 56
         PRINT 76
         PRINT 59
         PRINT 51
         PRINT 55
       ELSE
         PRINT 58
         PRINT 57
         PRINT 58
       ENDIF
      LL=0.
      DO 45 L=1,NGAUS
       LL=LL+1.
       A(LL)=0.
       IF (QUE2.EQ.'o') THEN
         READ*, A(LL+1), DFLAGA(LL+1), A(LL+2), DFLAGA(LL+2)
       ELSE
         READ*, A(LL+1), A(LL+2)
       ENDIF
   54  FORMAT('| POSITION   | CONTRAINTE  | SIGMA       | CONTRAINTE  |'
     +)
   55  FORMAT('|------------|-------------|-------------|-------------|'
     +)
   56  FORMAT('|            | oui -> 0    |             | oui -> 0    |'
     +)
   76  FORMAT('|            | non -> 1    |             | non -> 1    |'
     +)
   59  FORMAT('|            | valeur -> 2 |             | valeur -> 2 |'
     +)
   51  FORMAT('|            |prededente   |             |precedente   |'
     +)
   57  FORMAT('| POSITION   | SIGMA       |')
   58  FORMAT('|------------|-------------|')
c       PRINT 52, L
c       PRINT 53
c   52  FORMAT('AMPLITUDE DE LA GAUSSIENNE #',I2,' ?')
c   53  FORMAT('entrez 0 si inconnue.')
c       IF (QUE2.EQ.'o') THEN
c         READ*, A(LL), DFLAGA(LL)
c       ELSE
c         READ*, A(LL)
c       ENDIF
c       IF (Y1MEM.LT.0.) THEN
c         A(LL)=-2.*Y1MEM+A(LL)
c       ENDIF
       A(LL)=A(LL)/ABS(Y1MEM)
       DELTAA(LL)=A(LL)*DFLAGA(LL)/26.
       IF (A(LL).EQ.0.) THEN
         A(LL)=10.1
         DELTAA(LL)=10.
       ENDIF
       DELTAA(LL+1)=.51*DFLAGA(LL+1)
       DELTAA(LL+2)=DFLAGA(LL+2)*.07
       IF (DFLAGA(LL+2).EQ.2.) THEN
         DELTAA(LL+2)=0.
       ENDIF
       LL=LL+2.
   45 CONTINUE
      LL=LL+1
      A(LL)=Y1-X1*(Y1-Y2)/(X1-X2)
      A(LL+1)=(Y1-Y2)/(X1-X2)
      A(LL+2)=0.
      DELTAA(LL)=YMAX/10.
      DELTAA(LL+1)=.1
      DELTAA(LL+2)=.1
      if (nordp.eq.0) then
        a(ll+1)=0.
      elseif (nordp.eq.-1) then
        a(ll)=0.
        a(ll+1)=0.
      endif
      MEMCHI=1.E10
C
C     DEBUT DE LA ROUTINE DE FIT
C
      PRINT*
      PRINT*
      PRINT*,'AJUSTEMENT EN COURS ...'
      DO 70 K=1,100
   60 CALL GRIDLS (X, Y, SIGMAY, NPTS, NTERMS, MODE, A,
     + DELTAA, SIGMAA, YFIT, CHISQR, NGAUS, NORDP, DFLAGA)
      IF ((ABS(MEMCHI-CHISQR))-((DY/8.)**2.)) 72, 62, 62
  62  MEMCHI=CHISQR
  70  CONTINUE
C
C     MODULE D'IMPRESSION DES RESULTATS
C
  72   PRINT*,'AJUSTEMENT TERMINE !'
       PRINT*
       PRINT*
       CALL IMPRI (RESULT, NOMFI, NPTS, Y1MEM, NGAUS, NORDP, YFIT,
     + Y, X, A, NTERMS, FILE, IMAGE)             
          END
C
C
C =============================================================
C
C     ROUTINE D'IMPRESSION
C
       SUBROUTINE IMPRI (RESULT, NOMFI, NPTS, Y1MEM, NGAUS, NORDP,
     + YFIT, Y, X, A, NTERMS, FILE, IMAGE)
       DIMENSION X(2048), Y(2048), YFIT(2048), A(60), profil(21),
     + b(3), xx(2048)
       CHARACTER*28 IMAGE
       CHARACTER*18 NOMFI, RESULT, FILERES, FILEFIT
       CHARACTER*6 FILE
       character*10 profil
       INTEGER NGAUS, NORDP, NPTS, NTERMS,count
       REAL X, Y, A, YFIT, Y1MEM, b, xx
      profil(1)='gaufit.p01'
      profil(2)='gaufit.p02'
      profil(3)='gaufit.p03'
      profil(4)='gaufit.p04'
      profil(5)='gaufit.p05'
      profil(6)='gaufit.p06'
      profil(7)='gaufit.p07'
      profil(8)='gaufit.p08'
      profil(9)='gaufit.p09'
      profil(10)='gaufit.p10'
      profil(11)='gaufit.p11'
      profil(12)='gaufit.p12'
      profil(13)='gaufit.p13'
      profil(14)='gaufit.p14'
      profil(15)='gaufit.p15'
      profil(16)='gaufit.p16'
      profil(17)='gaufit.p17'
      profil(18)='gaufit.p18'
      profil(19)='gaufit.p19'
      profil(20)='gaufit.p20'
      profil(21)='gaufit.p21'
      RESI=0.
      DO 11 L=1,NPTS
        RESI=RESI+ABS(Y(L)-YFIT(L))
  11  CONTINUE
      RESI=RESI/NPTS
       OPEN(UNIT=2,FILE=RESULT,STATUS='unknown')
       WRITE(2,*) '#FICHIER resfit.dat POUR ', IMAGE,' -> ', NOMFI
       WRITE(2,*) '#NOMBRE DE POINTS=',NPTS
       WRITE(2,*) '#NOMBRE DE PROFILS GAUSSIENS=',NGAUS
       WRITE(2,*) '#ORDRE DU POLYNOME=',NORDP
       WRITE(2,*) '#RESIDU MOYEN=', RESI*Y1MEM
C
C     RENORMALISATION
C
       DO 75 K=1,NPTS
        Y(K)=ABS(Y1MEM)*Y(K)
        YFIT(K)=ABS(Y1MEM)*YFIT(K)
c        IF (Y1MEM.LT.0.) THEN
c          Y(K)=2.*Y1MEM+Y(K)
c          YFIT(K)=2.*Y1MEM+YFIT(K)
c       ENDIF
  75   CONTINUE
       LL=0
       DO 77 I=1,NGAUS
         LL=LL+1
         A(LL)=ABS(Y1MEM)*A(LL)
         LL=LL+2
  77   CONTINUE
       LL=LL+1
       A(LL)=ABS(Y1MEM)*A(LL)
c       IF (Y1MEM.LT.0.) THEN
c         A(LL)=2.*Y1MEM+A(LL)
c       ENDIF
       A(LL+1)=ABS(Y1MEM)*A(LL+1)
       A(LL+2)=ABS(Y1MEM)*A(LL+2)
       WRITE(2,*)
       WRITE(2,*)
       WRITE(2,85)
       WRITE(2,*)
       KL=1
       DO 80 K=1,NGAUS
        WRITE(2,92) A(KL),A(KL+1), A(KL+2), A(KL+2)*2.3548, A(KL)*2.5066
     +*A(KL+2)
        KL=KL+3
  80   CONTINUE
        WRITE(2,*)
       WRITE(2,*)
       WRITE(2,86)
       WRITE(2,*)
       WRITE(2,88)
       WRITE(2,*)
       WRITE(2,93) A(KL), A(KL+1), A(KL+2)
      CLOSE(UNIT=2)
  85  FORMAT('# AMPLITUDE     POSITION      SIGMA         FWHM             
     +FLUX TOTAL  ')
  86  FORMAT('#        POLYNOME POUR LE CONTINU          ')
  88  FORMAT('# CONSTANTE     LINEAIRE      QUADRATIQUE  ')
  92  format(2x,g11.4,3x,g11.6,3x,g11.4,3x,g11.4,3x,g11.4)
  93  format('#',1x,g11.4,3x,g11.4,3x,g11.4)
      FILERES=FILE//'.res'
      OPEN(UNIT=3,FILE=FILERES,STATUS='unknown')
        DO 90 K=1,NPTS
          WRITE(3,*) X(K), Y(K)-YFIT(K)
  90    CONTINUE
      CLOSE(UNIT=3)
      FILEFIT=FILE//'.fit'
      OPEN(UNIT=4,FILE=FILEFIT,STATUS='unknown')
        DO 98 K=1,NPTS
           WRITE(4,*) X(K), YFIT(K)
  98    CONTINUE
      CLOSE(UNIT=4)
      OPEN(UNIT=1,FILE='resfit.xy',STATUS='unknown')
        DO 99 K=1,NPTS
          WRITE(1,*) X(K), Y(K)
  99    CONTINUE
      CLOSE(UNIT=1)
C    
C     CREATION DES COURBES POUR LES COMPOSANTES
C
       k=1
       if (ngaus.eq.20) then
          count=20
       else 
          count=ngaus+1
       endif
       do 500, l=1,count
          open(unit=1,file=profil(l),status='unknown')
          ngau=1
          nord=-1
          i=1
          b(1)=a(k)
          b(2)=a(k+1)
          b(3)=a(k+2)
          xx(1)=b(2)-6*b(3)
          if (l.eq.ngaus+1) then           
            do 505, kk=1,NPTS
              ngau=0
              nord=nordp
              y(i)=functn(x,i,b,ngau,nord)
             write(1,*) x(i), y(i)
              i=i+1
  505       continue
          else            
            do 510, kk=1,48
              y(i)=functn(xx,i,b,ngau,nord)
              write(1,*) xx(i), y(i)
              i=i+1
              xx(i)=xx(i-1)+b(3)/4.
  510       continue 
          endif
          close(unit=1) 
          k=k+3 
  500     continue
      RETURN
      END
C
C
C =============================================================
C
C      ROUTINE DE FIT
C
C      BUT
C       FAIRE UN FIT PAR LES MOINDRES CARRES AVEC UNE FONCTION
C       SPECIFIEE DANS LA SOUS-ROUTINE FUNCTN
C
C      UTILISATION
C       CALL GRIDLS (X, Y, SIGMAY, NPTS, NTERMS, MODE, A, DELTAA,
C          SIGMAA, YFIT, CHISQR, NGAUS, NORDP, DFLAGA)
C
C      PARAMETRES
C       X  -VECTEUR DE COORDONNEE (PIXEL)
C       Y  -VECTEUR DE LA FONCTION (ADU)
C       SIGMAY  -VECTEUR DE DEVIATION STANDARD POUR LES Y
C       NPTS  -NOMBRE DE PAIRES DE POINTS DE DATA
C       NTERMS  -NOMBRE DE PARAMETRES DE LA FONCTION
C       MODE  -POIDS POUR CHAQUE POINTS UTILE DANS LA RESOLUTION
C              PAR LES MOINDRES CARRES
C                +1 (INSTRUMENTAL)  POIDS(I)=1./SIGMAY(I)**2
C                 0 (AUCUN POIDS SPECIFIQUES)
C                -1 (STATISTIQUE)   POIDS(I)=1/Y(I)
C       A  - MATRICE DE PARAMETRES
C       DELTAA  - VECTEUR POUR L'INCREMENT DES PARAMETRES
C       SIGMAA  - VECTEUR DE DEVIATION STANDARD
C       YFIT  -VECTEUR DES VALEURS DE Y CALCULEES
C       CHISQR  -CHI CARRE POUR LE FIT
C       DFLAGA  - VECTEUR PERMETTANT D'IMPOSER DES CONTRAINTES
C                 SUR LES PARAMETRES
C
C      SOUS-ROUTINES REQUISES
C       FUNCTN (X, I, A, NGAUSDB, NGAUS, NORDP)
C           EVALUE LA VALEUR DE LA FONCTION POUR X
C       FCHISQ (Y, SIGMAY, NPTS, NFREE, MODE, YFIT)
C           EVALUE CHI CARRE
C      COMMENTAIRES
C        DELTAA EST EDITEE AUTOMATIQUEMENT AU COURS DU PROGRAMME
C
      SUBROUTINE GRIDLS (X, Y, SIGMAY, NPTS, NTERMS, MODE, A, DELTAA,
     + SIGMAA, YFIT, CHISQR, NGAUS, NORDP, DFLAGA)
      DIMENSION X(2048), Y(2048), SIGMAY(2048), A(60), DELTAA(60),
     + SIGMAA(60), YFIT(2048), DFLAGA(60)
      REAL DELTA, DELTAA, CHISQR, FREE, CHISQ1, CHISQ2, CHISQ3, FN,
     + SAVE, DFLAGA, A
      INTEGER MEMCHI12
   11 NFREE= NPTS-NTERMS
      FREE=NFREE
      CHISQR=0.
      IF (NFREE) 100, 100, 20
   20 DO 90 J=1, NTERMS
C
C     EVALUATION DE CHI CARRE AUX DEUX PREMIERS PTS
C
      IF (DFLAGA(J).EQ.2.) THEN
         A(J)=A(J-3)
      ELSE  
        IF (DELTAA(J).EQ.0.) THEN
          GOTO 90
        ENDIF
      ENDIF
   21 DO 22 I=1, NPTS
   22 YFIT(I)=FUNCTN (X, I, A, NGAUS, NORDP)
   23 CHISQ1= FCHISQ (Y, SIGMAY, NPTS, NFREE, MODE, YFIT)
      FN=0.
      DELTA=DELTAA(J)
      MEMCHI12=0
   41 A(J)=A(J)+DELTA
      MEMCHI12=MEMCHI12+1
      IF (MEMCHI12.EQ.3)  THEN
         A(J)=A(J)-DELTA
         GO TO 90
      ENDIF
        DO 43 I=1, NPTS
   43 YFIT(I) =FUNCTN (X, I, A, NGAUS, NORDP)
   44 CHISQ2= FCHISQ (Y, SIGMAY, NPTS, NFREE, MODE, YFIT)
   45 IF (CHISQ1-CHISQ2) 51, 41, 61
C
C     RENVERSE LA DIRECTION DE LA RECHERCHE (SI CHI CARRE AUGMENTE)
C
   51 DELTA =-DELTA
      A(J) = A(J) +DELTA
      DO 54 I=1, NPTS
   54 YFIT(I)=FUNCTN (X, I, A, NGAUS, NORDP)
      SAVE =CHISQ1
      CHISQ1=CHISQ2
   57 CHISQ2=SAVE
C
C     INCREMENTE A(J) JUSQU'A CE QUE CHI CARRE AUGMENTE
C
   61 FN=FN+1.
      A(J)=A(J)+DELTA
      DO 64 I=1, NPTS
   64 YFIT(I)=FUNCTN (X, I, A, NGAUS, NORDP)
      CHISQ3= FCHISQ (Y, SIGMAY, NPTS, NFREE, MODE, YFIT)
   66 IF (CHISQ3-CHISQ2) 71, 81, 81
   71 CHISQ1= CHISQ2
      CHISQ2=CHISQ3
      GO TO 61
C
C     RECHERCHE DU MINIMUM DE LA PARABOLE DEFINIE PAR LES TROIS
C     POINTS PRECEDENTS
C
   81 IF ((CHISQ3-CHISQ2).EQ.0.) THEN
         GO TO 82
      ENDIF
      IF ((CHISQ2-CHISQ3).EQ.(CHISQ1-CHISQ2)) THEN
          GO TO 82
      ENDIF
      DELTA=DELTA*(1./(1.+(CHISQ1-CHISQ2)/(CHISQ3-CHISQ2))+0.5)
   82 A(J)=A(J)-DELTA
   83 SIGMAA(J)=DELTAA(J)*SQRT(2./(FREE*(CHISQ3-2.*CHISQ2+CHISQ1)))
   84 DELTAA(J)=DELTAA(J)*FN/3.
   90 CONTINUE
C
C     EVALUATION DU FIT ET CHI CARRE POUR LES PARAMETRES FINAUX
C
   91 DO 92 I=1, NPTS
   92 YFIT(I)=FUNCTN(X, I, A, NGAUS, NORDP)
   93 CHISQR=FCHISQ(Y, SIGMAY, NPTS, NFREE, MODE, YFIT)
  100 RETURN
      END
C
C
C =============================================================
C
C     FONCTION FUNCTN
C
C
      FUNCTION FUNCTN (X, I, A, NGAUS, NORDP)
      DIMENSION X(2048), A(60), Z(60), Z2(60)
      REAL A, FUNCTN
      LL=1
      FUNCTN=0.
      XI=X(I)
      DO 200 L=1,NGAUS
       Z(L)=(XI-A(LL+1))/A(LL+2)
       Z2(L)=Z(L)**2
       IF (Z2(L)-50.) 116, 199, 199
  116  FUNCTN=FUNCTN+A(LL)*EXP(-Z2(L)/2.)
  199  LL=LL+3
  200 CONTINUE
      IF (NORDP.EQ.0) THEN
        FUNCTN=FUNCTN+A(LL)
      ELSEIF (NORDP.EQ.1) THEN
        FUNCTN=FUNCTN+A(LL)+A(LL+1)*XI
      ELSEIF (NORDP.EQ.2) THEN
        FUNCTN=FUNCTN+A(LL)+A(LL+1)*XI+A(LL+2)*XI*XI
      ENDIF
      RETURN
      END
C
C
C =============================================================
C
C     FONCTION  FCHISQ
C
C
C     EVALUATION DE CHI CARRE POUR LE FIT DES DONNEES
C     FCHISQ=SOMME((Y-YFIT)**2/SIGMA**2)/NFREE
C
C
      FUNCTION FCHISQ (Y, SIGMAY, NPTS, NFREE, MODE, YFIT)
      DIMENSION Y(2048), SIGMAY(2048), YFIT(2048)
      REAL CHISQ, FCHISQ, WEIGHT, FREE
   11 CHISQ=0.
   12 IF (NFREE) 13, 13, 20
   13 FCHISQ=0.
      GO TO 40
C
C      SOMMATION DES CHI CARRES
C
   20 DO 30 I=1, NPTS
   21 IF (MODE) 22, 27, 29
   22 IF (Y(I)) 25, 27, 23
   23 WEIGHT=1/Y(I)
      GO TO 30
   25 WEIGHT=-1/Y(I)
      GO TO 30
   27 WEIGHT=1.
      GO TO 30
   29 WEIGHT=1./SIGMAY(I)**2
   30 CHISQ=CHISQ+WEIGHT*(Y(I)-YFIT(I))**2
C
C       DIVISION PAR LE NOMBRE DE DEGRES DE LIBERTE (NFREE)
C
   31 FREE=NFREE
   32 FCHISQ=CHISQ/FREE
   40 RETURN
      END
C
C
C =============================================================
C
C     NORMALISATION DES VARIABLES
C
C
      SUBROUTINE NORM (Y, Y1, Y2, YMAX, DY, NPTS)
      DIMENSION Y(2048)
      REAL Y1,Y2,YMAX
      INTEGER L,NPTS
      DO 26 L=1,NPTS
              Y(L)=(Y(L)/Y1)
 26   CONTINUE
      Y2=Y2/Y1
      YMAX=YMAX/Y1
      DY=DY/Y1
      Y1=1.
      RETURN
      END
C
C
C =============================================================
C
C    STATISTIQUE SUR Y(X)
C
C    MAXIMUM, MINIMUM
C
      SUBROUTINE STATi( X, Y, Y1, Y2, X1, X2, Y1MEM, DY, YMAX, NPTS)
      DIMENSION X(2048), Y(2048)
      REAL Y1, Y1MEM, DY, YMAX, XMAX, XMIN
      XMAX=0.
      YMAX=0.
      DO 24 L=1, NPTS
      IF (YMAX.LT.Y(L)) THEN
        YMAX=Y(L)
      ENDIF
      if (XMAX.lt.x(l)) then
        XMAX=x(l)
      endif     
  24  CONTINUE
      XMIN=XMAX
      do 25 l=1,NPTS
        if (x(l).lt.XMIN) then
          XMIN=x(l)
        endif
  25  continue
C
C     CONTINU ADJACENT, BRUIT
C
      X1=(X(1)+X(2)+X(3)+X(4))/4.
      Y1=(Y(1)+Y(2)+Y(3)+Y(4))/4.
      Y1MEM=Y1
      X2=(X(NPTS-3)+X(NPTS-2)+X(NPTS-1)+X(NPTS))/4.
      Y2=(Y(NPTS-3)+Y(NPTS-2)+Y(NPTS-1)+Y(NPTS))/4.
      DY=(ABS(Y(1)-Y(2))+ABS(Y(2)-Y(3))+ABS(Y(3)-Y(4))+ABS(Y(NPTS-3)
     +-Y(NPTS-2))+ABS(Y(NPTS-2)-Y(NPTS-1))+ABS(Y(NPTS-1)-Y(NPTS)))/6.
c      IF (Y1.LT.0.) THEN
c        DO 27 L=1,NPTS
c          Y(L)=-2.*Y1+Y(L)
c 27     CONTINUE
c        Y2=-2.*Y1+Y2
c        YMAX=-2.*Y1+YMAX
c        Y1=-Y1
c      ENDIF
      RETURN
      END
