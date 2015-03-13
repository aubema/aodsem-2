C      PROGRAMME  DE  FIT GFA
C
C      VERSION 1.1
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
C     SORTIE:  LA SORTIE EST SECTIONNEE EN QUATRE FICHIERS
C               A) FICHIER resfit.dat (parametres, etc)
C               B) FICHIER resfit.res (x, y-yfit i.e. residu)
C               C) FICHIER resfit.fit (x, yfit)
C               D) FICHIER resfit.xy  (x, y)
C
C =============================================================
C
C
C      DONNEES INITIALES
C
      PROGRAM GFA
      real X(2048), Y(2048), SIGMAY(2048), A(768), DELTAA(768)
     + ,y1,y2,x1,x2,y1mem,ymax,xl(2048),yl(2048)
     +, SIGMAA(768), YFIT(2048), DDY(2048), MAXI(1000), DFLAGA(768)
      REAL  MEMCHI, DDYMEM,   DY,fact ,xmaxm(1000),ymaxm(1000),
     +dmaxm(1000),dmaxi(1000),ymaxi(1000),moyenne(1000)
     
      CHARACTER*40 IMAGE
      CHARACTER*18 , NOMFI, RESULT
      CHARACTER*6 FILE, QUE3
      INTEGER   NGAUS, NORDP, LLL,npts,n0,k,kk,fe,fenetre,liss,nf
      integer npics, mmi
      FILE='resfit'
      RESULT='resfit.dat'
      MODE=-1
      DO 5, K=1,768
        DFLAGA(K)=1.
  5   CONTINUE
      OPEN(2,FILE='gaufit.in',STATUS='unknown')
        READ(2,10) NOMFI
        read(2,*) n0
        READ(2,*) nf
	read(2,*) ngaussmax
	READ(2,*) fact
      CLOSE(2)
      if (ngaussmax.gt.256) then
        print*,'Trop de gaussiennes (max=256)!!!!!!!!!!!!'
        stop
      endif
        npts=nf-n0
   10 FORMAT(A)
C
C      LECTURE DU FICHIER PROFIL
C
	
   15 FORMAT (I3)
      OPEN(1, FILE=NOMFI,STATUS='unknown')
       mmi=0
       DO 20 M=1, nf
         if (m.ge.n0) then
           mmi=mmi+1
           READ (1,*,end=22) X(Mmi), Y(Mmi)

         else 
           read (1,*,end=22)
         endif
         xl(Mmi)=X(mmi)
         yl(mmi)=y(mmi)

 20    CONTINUE
 22   CLOSE(1)
      if (mmi.lt.npts) then
        npts=mmi
      endif
C
C     LISSAGE
C      
         CALL LISSAGE( Xl, Yl, NPTS)
C         
C       STATISTIQUE SUR Y(X)
C
  323  CALL STATi( Xl, Yl, Y1, Y2, X1, X2, Y1MEM, DY, YMAX, NPTS)
C
C
C     NORMALISSATION DES VARIABLES
C
      CALL NORM (Yl, Y1, Y2, YMAX, DY, NPTS)
C
C
C       DERIVEE  identification des principaux maximums
C
c      print*,'Recherche du nombre de raies...'
      DDYMEM=DY
      LLL=0
      do k=1,npts
             ymaxi(k)=0.
             dmaxi(k)=0.
             ddy(k)=0.
      enddo
      DO 28 K=3,NPTS-3
         fenetre=100
         moyenne(k)=0.
         if ((k.gt.fenetre-3).and.(k.lt.npts-fenetre+3)) then
             do fe=k-fenetre,k+fenetre
                moyenne(k)=yl(fe)+moyenne(k)
             enddo
             moyenne(k)=moyenne(k)/(2.*real(fenetre)+1.)
             if (yl(k)/moyenne(k).gt.fact*dy*sqrt(2.*real(fenetre)+1))
     +       then
                IF ((ABS(Yl(K-2)).LT.ABS(Yl(K-1)-fact*DY)).AND.
     +          (ABS(Yl(K+2)).LT.(ABS(Yl(K+1))-fact*DY))) THEN
                   LLL=LLL+1
c                   if (maxi(LLL-1).ne.xl(k-1)) then
c                   if (maxi(LLL-2).ne.xl(k-2)) then
                     dmaxi(LLL)=yl(k)/moyenne(k)
                     ymaxi(LLL)=yl(k)
                     maxi(LLL)=xl(k)
c                   endif
c                   endif
                endif
             endif

          endif
          DDY(K)=(Yl(K+1)-Yl(K))/(Xl(K+1)-Xl(K))

      DDYMEM=DDY(K)
 28   CONTINUE
c
c  selection des maximums les plus importants et classement par ordre
c  d'importance
c
      do kk=1,ngaussmax
        xmaxm(kk)=0.
        ymaxm(kk)=0.
        dmaxm(kk)=0.

      enddo
      npics=0
      do kk=1,ngaussmax

          do k=1,lll
            
            if ((dmaxi(k).gt.dmaxm(kk)).and.(dmaxi(k).gt.0.)) then
              if (kk.gt.1) then
                if (dmaxi(k).lt.dmaxm(kk-1)) then
      
     
                  if (abs(MAXI(k)-xmaxm(kk-1)).gt.5.) then
c  eliminer les max plus proches que 5
c                
                  ymaxm(kk)=ymaxi(k)
                  xmaxm(kk)=MAXI(k)
                  dmaxm(kk)=dmaxi(k)

                  endif
  
                  
                endif
               else
                  ymaxm(kk)=ymaxi(k)
                  xmaxm(kk)=MAXI(k)
                  dmaxm(kk)=dmaxi(k)

               endif

            endif
          enddo
c
c Compter le nombre de raies trouvees
c
          if (xmaxm(kk).gt.0.) then
            print*,'Longueur d onde raie #',npics,' :',xmaxm(kk)
            npics=npics+1
          endif
       enddo
              
          if (ngaussmax.gt.npics) ngaussmax=npics
C
C       STATISTIQUE SUR Y(X)
C
  23  CALL STATi( X, Y, Y1, Y2, X1, X2, Y1MEM, DY, YMAX, NPTS)
C
C
C     NORMALISSATION DES VARIABLES
C
      CALL NORM (Y, Y1, Y2, YMAX, DY, NPTS)


C
C        FIT PRELIMINAIRE AUTOMATIQUE
C
        L=0
c        NGAUS=LLL
        
        NGAUS=ngaussmax

        NTERMS=3*NGAUS+3
        NORDP=2
        DO 31 K=1,NGAUS
          L=L+1
          A(L+1)=xmaxm(K)
          DELTAA(L+1)=.5
          A(L)=y(K)
          DELTAA(L)=1.
          A(L+2)=.11
          DELTAA(L+2)=.023
          L=L+2
  31    CONTINUE
          L=L+1
          A(L)=Y1-X1*(Y1-Y2)/(X1-X2)
          DELTAA(L)=.1
          A(L+1)=(Y1-Y2)/(X1-X2)
          DELTAA(L+1)=.1
          A(L+2)=0.
          DELTAA(L+2)=.1
          MEMCHI=1.E10
C
C   APPEL DE LA ROUTINE DE FIT POUR LE FIT PRELIMINAIRE
C
c      PRINT*
c      PRINT*
c      PRINT*,'AJUSTEMENT DE ',LLL,' GAUSSIENNES EN COURS ...'
          DO 35, K=1,100
             CALL GRIDLS (X, Y, SIGMAY, NPTS, NTERMS, MODE, A,
     +       DELTAA, SIGMAA, YFIT, CHISQR, NGAUS, NORDP, DFLAGA)
  36      IF ((ABS(MEMCHI-CHISQR))-((DY*0.01)**2.)) 38, 37, 37
  37      MEMCHI=CHISQR
  35      CONTINUE
C
C      IMPRESSION DES RESULTATS
C
  38  CALL IMPRI (RESULT, NOMFI, NPTS, Y1MEM, NGAUS, NORDP, YFIT,
     +Y, X, A, NTERMS, FILE, IMAGE)
      OPEN(UNIT=2,FILE='gaufit.oto',STATUS='unknown')
          WRITE(2,*) 'NOMBRE DE GAUSSIENNES:', ngaus
          WRITE(2,*)
          WRITE(2,42)
          WRITE(2,*)
          WRITE(2,41)
          WRITE(2,40)
          WRITE(2,41)
          LL=1
	DO 46, K=1,NGAUS
          WRITE(2,*) A(LL+1), A(LL+2)
	LL=LL+3
  46    CONTINUE
	CLOSE(3)
  40  FORMAT('| POSITION   | SIGMA       |')
  41  FORMAT('|------------|-------------|')
  42  FORMAT('RESULTAT DE L AJUSTEMENT AUTOMATISE')
      END
C
C
C =============================================================
C
C     ROUTINE D'IMPRESSION
C
       SUBROUTINE IMPRI (RESULT, NOMFI, NPTS, Y1MEM, NGAUS, NORDP,
     + YFIT, Y, X, A, NTERMS, FILE, IMAGE)
       real X(2048), Y(2048), YFIT(2048), A(768),
     + b(3), xx(2048)
       CHARACTER*28 IMAGE
       CHARACTER*18 NOMFI, RESULT, FILERES, FILEFIT
       CHARACTER*6 FILE
       INTEGER NGAUS, NORDP, NPTS, NTERMS
       REAL  Y1MEM
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
       WRITE(2,*) '#RESIDU MOYEN=', RESI*abs(Y1MEM)
C
C     RENORMALISATION
C
       DO 75 K=1,NPTS
        Y(K)=Y1MEM*Y(K)
        YFIT(K)=Y1MEM*YFIT(K)
c        IF (Y1MEM.LT.0.) THEN
c          Y(K)=2.*Y1MEM+Y(K)
c          YFIT(K)=2.*Y1MEM+YFIT(K)
c        ENDIF
  75   CONTINUE
       LL=0
       DO 77 I=1,NGAUS
         LL=LL+1
         A(LL)=Y1MEM*A(LL)
         LL=LL+2
  77   CONTINUE
       LL=LL+1
       A(LL)=Y1MEM*A(LL)
c       IF (Y1MEM.LT.0.) THEN
c         A(LL)=2.*Y1MEM+A(LL)
c       ENDIF
       A(LL+1)=Y1MEM*A(LL+1)
       A(LL+2)=Y1MEM*A(LL+2)
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
      DIMENSION X(2048), Y(2048), SIGMAY(2048), A(768), DELTAA(768),
     + SIGMAA(768), YFIT(2048), DFLAGA(768)
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
      DIMENSION X(2048), A(768), Z(768), Z2(768)
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
      real X(2048), Y(2048), x1,x2,y2
      REAL Y1, Y1MEM, DY, YMAX, XMAX, XMIN
      integer npts
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
C
C
C =============================================================
C
C	LISSAGE
C
C
      SUBROUTINE LISSAGE (X, Y, NPTS)
      
      REAL tot,ny(2048),x(2048),y(2048)

      INTEGER a,b,h,k,npts
      do i = 1,3
      print*,"LISSAGE"
        a = 1
        b = 11
        do k = 6,npts-5
          tot=0.
          do h = a,b
            tot=tot+y(h)
          enddo
          ny(k)=tot/11.
          a=a+1
          b=b+1
        enddo

        do k = 1,5
          ny(k)=ny(6)
        enddo
        do k = npts-4,npts
          ny(k)=ny(npts-5)
        enddo
      
        do k = 1,npts
	   y(k)=ny(k)
        enddo
      enddo

      open(22,file='temp.bidon',status='unknown')
	do k = 1,npts
         write(22,*) x(k), y(k)
	enddo
      close(22)
      RETURN
      END












