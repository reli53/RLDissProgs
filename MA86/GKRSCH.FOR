C                                                                       00010002
C       ##################################################              00020002
C       ####   MODELL: MA86                           ####              00030002
C       ####   NAME DES PROGRAMMS:  MA86(GKRSCH)      ####              00040004
C       ####      LAPLACE(RHON) = RHON                ####              00050002
C       ####      LAPLACE(PHIA) = RHON - 1            ####              00060002
C       ####      R.B. : RHON = C                     ####              00070002
C       ####           : PHIA = - GAMMA * KAPPA       ####              00080002
C       ####             (KAPPA = 2* MITTL. KRUEMMUNG)####              00090002
C       ####      VA    =  GRAD( PHIA )               ####              00100002
C       ####             (RHOA AUF 1 SKALIERT !)      ####              00110002
C       ##################################################              00120002
C        2 DIMENSIONAL UND UNENDLICH LANGER STREIFEN BZW.               00130002
C        3 DIMENSIONAL UND ZWEI UNENDLICH GROSSE PLATTEN.               00140002
C        AUSGERECHNET WIRD DER (VERMUTLICHE) KRITISCHE PARAMETER        00150002
C        UND ZUR SICHERHEIT WERDEN DIE MAXIMALEN EIGENWERTE             00160002
C        MLAMB BEI DER DAZUGEHOERNDEN WELLENZAHL MK BESTIMMT.           00170002
C                                                                       00180002
      DOUBLE PRECISION RHOA,C   ,GKR ,B0  ,LA  ,LS  ,K                  00190000
      DOUBLE PRECISION MLA ,MLS ,MKA ,MKS                               00200000
      DOUBLE PRECISION DT  ,DS  ,DC  ,H1  ,H2  ,H3   ,H4                00210000
      RHOA=100.D+00                                                     00220000
      DO 2200 B0=1.D-01 , 1.D+01, 1.D-01                                00230000
         DT=DTANH(B0)                                                   00240000
         DS=DSINH(B0)                                                   00250000
         DC=DCOSH(B0)                                                   00260000
         C=B0/DT                                                        00270000
C   >>>  GKR = VERMUTLICH KRITISCHES GAMMA , BESTIMMT AUS DER           00280001
C   >>>  GLEICHUNG (D(LAMBDA))**2/D(K**2) = 0 , K=0 , ANTISYMMETR.      00290001
         GKR=B0*B0*2.D+00/3.D+00/RHOA                                   00300000
         GKR=B0/2.D+00*(GKR+B0/RHOA*(B0-DC*DS)/DS/DS)                   00310000
         WRITE (6,51) B0                                                00320001
   51    FORMAT (1X,'B0 = ',1(D17.10,2X))                               00330001
         MLA=-1.D+10                                                    00340000
         MLS=MLA                                                        00350000
         MKA=0.                                                         00360000
         MKS=0.                                                         00370000
C  >>>   MLA = MAXIMALES LAMBDA (ANTISYMMETR. MODE)                     00380001
C  >>>   MLS = MAXIMALES LAMBDA (    SYMMETR. MODE)                     00390001
C  >>>   MKA ,MKS SIND DIE DAZUGEHOERENDEN WELLENZAHLEN K .             00400001
         DO 2100 K=1.D-02, .5D+02 , 1.D-02                              00410001
            H1=(B0-RHOA*GKR*K*K)*K                                      00420000
            H2=DSQRT(1.+K*K)*B0                                         00430000
            H3=DTANH(K*B0)                                              00440000
            H4=DTANH(H2)                                                00450000
            IF (K .GT. 1.D-10) THEN                                     00460001
               LA=(C-1.+H1/H3-H2/H4)/RHOA                               00470001
               LS=(C-1.+H1*H3-H2*H4)/RHOA                               00480001
            ELSE                                                        00490001
               LA=0.D0                                                  00500001
               LS=-1.D+10                                               00510001
            ENDIF                                                       00520001
C           WRITE ( 6,50) K,LA,LS                                       00530001
            MLA=DMAX1(LA,MLA)                                           00540000
            MLS=DMAX1(LS,MLS)                                           00550000
            IF (MLS .GT. LS) GOTO 2050                                  00560000
               MKS=K                                                    00570000
 2050       IF (MLA .GT. LA) GOTO 2100                                  00580000
               MKA=K                                                    00590000
 2100    CONTINUE                                                       00600000
      WRITE (6,50) MLA,MKA,MLS,MKS                                      00610000
   50 FORMAT (1X,4(D17.10,2X))                                          00620000
 2200 CONTINUE                                                          00630000
      END                                                               00640000
