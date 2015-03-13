C      PROGRAMME  DE  FIT GF0
C 
C      VERSION 1.0
C
C
C      LOGICIEL POUR L'AJUSTEMENT DE N GAUSSIENNES
C
C      I/O ADAPTE AU SYSTEME DE TRAITEMENT D'IMAGES ASTRONOMIQUE
C      IRAF DE KPNO
C 
C      REFERENCES:DATA REDUCTION AND ERROR ANALYSIS FOR THE PHYSICAL
C                 SCIENCES, PHILIP R. BEVINGTON, McGraw-Hill, 1969
c      UNIVERSITE LAVAL  10 mai 1991
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
C      1) gfaube
C
C          LE FICHIER DE COMMANDE gfaube GERE L'EXECUTION GLOBALE
C      DES TROIS LOGICIELS (gf0.f, gfa.f, gfb.f). IL SERT AUSSI 
C      D'INTERFACE POUR L'ACCES AUX COMMANDES DE GESTION D'IMAGE
C      ET AUX COMMANDES GRAPHIQUES DE IRAF.  CE FICHIER EST EXECUTE
C      EN TAPANT cl<gfaube .  CE LOGICIEL EFFACE APRES USAGE TOUS 
C      LES FICHIERS TEMPORAIRES gaufit.* . 
C
C
C      2) gf0.f
C
C          LE LOGICIEL gf0.f PERMET A L'UTILISATEUR DE FOURNIR DES 
C      PARAMETRES NECESSAIRES A L'EXECUTION DE gfa.f et gfb.f.  IL 
C      CREE LE FICHIER DE COMMANDE gaufit.img PERMETTANT LA CONVERSION
C      D'UNE IMAGE OU D'UNE SECTION D'IMAGE EN FICHIER TEXTE.
C      EN MODE D'ENTREE TEXTE, ON DOIT SPECIFIER LE FORMAT DU FICHIER
C      D'ENTREE.  IL Y A DEUX FORMATS SUPPORTES PAR LE LOGICIEL:
C
C      1) x-y       1.2       2.1e-5
C                   1.9103    3.01e-6
C                   2.1       3.1e-6
C                   3.2       4e-6
C                   1.24      6e-6
C                  100.1     1.1e-5
C                    .          .
C                    .          .
C                    .          .
C 
C      2) y         2.1e-5      LA VALEUR DE x EST OBTENUE EN DEFI-
C                   3.2e-5      NISSANT UNE VALEUR INITIALE ET UN 
C                   3.67e-5     INCREMENT.  
C                     .
C                     .
C                     . 
C
C       EN MODE IMAGE, ON ADOPTE LE FORMAT y. LES VALEURS DE x SONT 
C       DONNEES EN PIXEL.  LA VALEUR INITIALE EST 1 ET L'INCREMENT 
C       VAUT 1.  SEULES LES SECTIONS D'IMAGES UNIDIMENTIONNELLES
C       SONT RECONNUES PAR LE LOGICIEL.  SI L'UTILISATEUR SELECTIONNE
C       UNE SECTION D'IMAGE 2-D, SEUL LA PREMIERE LIGNE EST CONSIDE-
C       REE.
C
C           LA LONGUEUR DE L'IMAGE OU DU FICHIER EST SPECIFIEE PAR
C       L'USAGER.  LE CHOIX D'UNE LONGUEUR NULLE OU D'UNE LONGUEUR
C       SUPERIEURE A LA LONGUEUR DU FICHIER EST INTERPRETEE COMME
C       LE CHOIX DE LA LONGUEUR TOTALE DU FICHIER.   
C
C
C      3) gfa.f
C
C          LE LOGICIEL gfa.f EFFECTUE UN FIT D'UNE GAUSSIENNE SUR 
C      CHAQUE MAXIMUMS IDENTIFIES PAR LA SECTION DE DERIVEE DE Y(I).
C      LE CRITERE DE RECHERCHE DES MAXIMUMS EST RELIE AU RAPPORT
C      SIGNAL SUR BRUIT C'EST POURQUOI LES RAIES FAIBLES NE SONT 
C      GENERALEMENT PAS IDENTIFIEES.  LE CONTINU SPECTRAL EST 
C      AJUSTE PAR UN POLYNOME QUADRATIQUE (NORDP=2). LE BRUIT
C      EST EVALUE A PARTIR DES 4 PREMIERS ET DES 4 DERNIERS PIXELS
C      DU FICHIER D'ENTREE.  IL EST DONC IMPORTANT DE S'ASSURER
C      QU'AUCUNES RAIES N'EST PRESENTE DANS CES REGIONS.  CET ESTI-
C      ME DU BRUIT PERMET L'IDENTIFICATION AUTOMATIQUE DES MAXIMUMS
C      AINSI QUE L'ARRET DE L'ITERATION DE LA PROCEDURE D'AJUSTE-
C      MENT (dans gfa.f et gfb.f).
C  
C
C      4) gfb.f
C
C          LE LOGICIEL gfb.f EFFECTUE LE FIT FINAL.  L'UTILISATEUR
C      PEUT AJOUTER OU ENLEVER DES PROFILS GAUSSIENS.  IL EST AUSSI
C      POSSIBLE D'IMPOSER DES CONTRAINTES SUR L'UN OU L'AUTRE DES
C      PARAMETRES DE CHAQUE GAUSSIENNE.  L'UTILISATEUR N'A PAS A 
C      SPECIFIER L'AMPLITUDE DES GAUSSIENNES.  ADVENANT LE CAS OU
C      ON DESIRE LE SPECIFIER, IL FAUT RETIRER LES COMMENTAIRES 
C      DANS LA SECTION "PARAMETRES INITIAUX POUR LE FIT". 
C      L'UTILISATEUR DOIT SPECIFIER L'ORDRE DU POLYNOME POUR L'AJUS-
C      TEMENT DU CONTINU SPECTRAL (ordre maximum=2, -1 correspond
C      a aucun polynome). 
C      CE LOGICIEL EST AUSSI RESPONSABLE DE LA CREATION DU FICHIER 
C      DE COMMANDE  gaufit.gfx PERMETTANT L'ACCES AUX FONCTIONS GRA-
C      PHIQUES DE IRAF.
C      LE NOMBRE DE PROFILS EST ARBITRAIREMENT LIMITE A 20 POUR
C      MODIFIER CE PARAMETRE IL FAUT AUGMENTER LA DIMENSION DE
C      A, DELTAA, SIGMAA, DFLAGA, ET AUGMENTER LA LIMITE DU NOMBRE
C      DE PROFILS (PRES DE LA LIGNE 27).
C 
C
C
C     SORTIE:  LA SORTIE EST SECTIONNEE EN QUATRE FICHIERS
C
C               A) FICHIER resfit.dat (parametres, etc)
C               B) FICHIER resfit.res (x, y-yfit i.e. residu)
C               C) FICHIER resfit.fit (x, yfit)
C               D) FICHIER resfit.xy  (x, y)
C
C =============================================================
C
C    DONNES INITIALES
C
C
      PROGRAM GF0
      REAL  x0, x0d, x, y
      CHARACTER*40 , NOMFI, IMAGE, imag
      CHARACTER*6  QUE3
      CHARACTER*1 QUE1, imtst
      INTEGER NPTS
      IMAGE='UN FICHIER TEXTE'
      x0=0.
      x0d=0.
  19  PRINT*,'TYPE D ENTREE (IMAGE/TEXTE) ([i]/t) ?'
      READ(*,10,err=19) QUE1
      IF ((QUE1.EQ.'i').or.(QUE1.EQ.'')) THEN
  2     PRINT*,'SECTION D IMAGE:'
        READ 10, IMAGE
        kk=0
        do 11 k=1,len(image)
          kk=kk+1
          imtst=image(kk:kk)
        if ((imtst.eq.' ').or.(imtst.eq.'[')) then
          imag=image(1:kk-1)//'.imh'
          kk=kk-1
        endif
   11   continue
        if (image(kk-3:kk).eq.'.imh') then
          imag=image(1:kk)
        endif
        OPEN(UNIT=1,FILE=imag,STATUS='unknown',ERR=2)
        CLOSE(UNIT=1)
        OPEN(UNIT=2,FILE='gaufit.img',STATUS='unknown')
          WRITE(2,*) 'dataio'
          WRITE(2,21) IMAGE
        CLOSE(UNIT=2)
        NOMFI='gaufit.txt'
        QUE3='y'
        PRINT*,'VALEUR INITIALE SELON X ?'
        READ*, x0
        PRINT*,'INCREMENT SELON X ?'
        READ*, x0d
        if (x0d.eq.0) then
          x0d=1.
        endif     
  20    PRINT*,'LONGUEUR DE L IMAGE EN PIXEL ([max] 2048) ?'
        READ(*,15,err=20) NPTS
        IF (NPTS.GT.2048) THEN
          GOTO 20
        ELSEIF (NPTS.EQ.0) THEN 
          npts=2048
        ENDIF
        OPEN(UNIT=1,FILE='gaufit.in',STATUS='unknown')
          WRITE(1,10) NOMFI
          WRITE(1,10) IMAGE
          WRITE(1,*) NPTS
          WRITE(1,10) QUE3
          WRITE(1,*) x0
          WRITE(1,*) x0d
        CLOSE(UNIT=1)
      ELSEIF (QUE1.EQ.'t') THEN
   17   PRINT*,'NOM DU FICHIER DE PROFIL DE RAIE ?'
        READ 10, NOMFI
        OPEN(UNIT=1,FILE=NOMFI,STATUS='unknown',ERR=17)
        nlign=0
        do 7 k=1,2049
          read(1,*,end=8) x
        nlign=nlign+1
    7   continue
    8   CLOSE(UNIT=1)
   10   FORMAT(A)
        OPEN(UNIT=3,FILE='gaufit.img',STATUS='unknown')
          WRITE(3,*) 'plot'
        CLOSE(UNIT=3)
        OPEN(UNIT=2,FILE='gaufit.txt',STATUS='unknown')
          WRITE(2,*) 'NONO'
        CLOSE(UNIT=2)
   12   PRINT*,'LONGUEUR DE L IMAGE EN PIXEL ([max] 2048) ?'
        READ(*,15,ERR=12) NPTS
        IF (NPTS.GT.2048) THEN
          GOTO 12
        ELSEIF (NPTS.EQ.0) THEN 
          npts=2048
        ENDIF
   16   PRINT*,'TYPE DE DONNEES (x-y) OU ([y]) ?'
        READ 10, QUE3
          IF (QUE3.EQ.'') THEN 
            QUE3='y'
          ENDIF          
          IF ((QUE3.EQ.'x-y').or.(QUE3.EQ.'y')) THEN
            IF (QUE3.EQ.'y') THEN
              PRINT*,'VALEUR INITIALE SELON X ?'
              READ*, x0
              PRINT*,'INCREMENT SELON X ?'
              READ*, x0d
              if (x0d.eq.0) then
                x0d=1.
              endif   
            elseif (que3.eq.'x-y') then
              open(unit=1,file=nomfi,status='unknown')
                do 26 k=1,nlign               
                read(1,*,err=16) x, y
   26           continue
              close(unit=1)
            ENDIF
          ELSE
           GOTO 16
          ENDIF
        OPEN(UNIT=1,FILE='gaufit.in',STATUS='unknown')
          WRITE(1,10) NOMFI
          WRITE(1,10) IMAGE
          WRITE(1,*) NPTS
          WRITE(1,10) QUE3
          WRITE(1,*) x0
          WRITE(1,*) x0d
        CLOSE(UNIT=1)
      ELSE 
        GOTO 19
      ENDIF
   15 FORMAT (I4)
   21 FORMAT('wtexti header=no maxline=7 output=gaufit.txt input=',A)
      END
