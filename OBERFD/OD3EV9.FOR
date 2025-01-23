C/D14KH    JOB RACF,CLASS=H,
C/    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=B06B
C/E2          EXEC FORTCLG,REGION.FORT=3000K,REGION.GO=5000K
C/FORT.SYSIN    DD *
C
C
C       BESTE VERSION :   31.01.1991 (R.LIERMANN)
C
C         MIT NORMIERTE LEGENDRE-POLYNOME (VERY EASY , VERY GOOD)
C
C       ##################################################
C       ####     EIGENWERTE DES LOKALEN               ####
C       ####       OBERFLAECHEN-DYNAMIK-MODELLS (3D)  ####
C       ####     V(KM)=VA1*(KM-K00) + VA2*(KM-K00)**2 ####
C       ####          + VA3*(KM-K00)**3               ####
C       ####   KM=SUMME DER BEIDEN HAUPTKRUEMMUNGEN   ####
C       ####                                          ####
C       ####   DB/DT =                                ####
C       ####    (V(KM)+GAMMA*(LAPLACE-BELTRAMI)(KM))* ####
C       ####            *DSQRT(G)/B                   ####
C       ####       ( IN KUGELKOORDINATEN)             ####
C       ####     STEADY - STATE BERECHNUNG DURCH      ####
C       ####  LOESEN DER GLEICHUNG IN KUGELKOORDINATEN####
C       ####                                          ####
C       ####   NAME DES PROGRAMMES :         (OD3EV9) ####
C       ##################################################
C
C       LITERATUR: BROWER, KESSLER, KOPLIK AND LEVINE
C       ACHTUNG:  GEAENDERTES NULLSTELLENSUCHPROGRAMM
C                 ( OHNE NEWTON-VERFAHREN , DAFUER
C                   EIN MINIMUN-SUCH-VERFAHREN DURCH
C                   INTERVALL-EINSCHACHTELUNG )
C
C
      IMPLICIT REAL*8 (A-H,K,O-Z)
C
      DOUBLE PRECISION  BK( 65),BKVOR( 65),BTH( 65),VN( 65)
      DOUBLE PRECISION  TCOS( 65),TSIN( 65)
      DOUBLE PRECISION  AL0( 65, 65), AL1( 65, 65) ,AL2( 65, 65)
      DOUBLE PRECISION                AL3( 65, 65) ,AL4( 65, 65)
      DOUBLE PRECISION  PI, DTH , EPS1
      DOUBLE PRECISION  XLBK , XKAP
      DOUBLE PRECISION  B0S ,B0S2,B0S3,B0S4,B0S5,B0S6,B0S7,B0S8,B0S9
      DOUBLE PRECISION  B0S10,B0S11,B0S12
      DOUBLE PRECISION  G0S ,G0S2,G0S3,G0S4,G0SW
      DOUBLE PRECISION  B1S ,B2S ,B3S ,B4S ,B2S2,B2S3
      DOUBLE PRECISION  KSA  ,KSB  ,KSC  ,KSD  ,KSE
      DOUBLE PRECISION  KSAT ,KSBT ,KSCT ,KSDT ,KSET
      DOUBLE PRECISION  KSATT,KSBTT,KSCTT,KSDTT,KSETT
      DOUBLE PRECISION  KSTA ,KSTB ,KSTC ,KSTD ,KSTF ,KSTG
      DOUBLE PRECISION  KSTTA,KSTTB,KSTTC,KSTTD,KSTTF,KSTTG,KSTTH,KSTTI
      DOUBLE PRECISION  KSFFA,KSFFB,KSFFC,KSFFD
      DOUBLE PRECISION  K0A  ,K0B  ,K0C
      DOUBLE PRECISION  K0AT ,K0BT ,K0CT
      DOUBLE PRECISION  K0ATT,K0BTT,K0CTT
      DOUBLE PRECISION  K0    ,K0T   ,K0TT
      DOUBLE PRECISION  B0    ,B0T   ,B0TT  ,B0TTT ,B0TTTT
      DOUBLE PRECISION  COSTH ,SINTH
      DOUBLE PRECISION  RPLM(0:4,1:800)
CCC   DOUBLE PRECISION  RPLMEM(0:4,0:8,1:48,1:800)
CC    DOUBLE PRECISION  RPLMEM(0:4,0:4,1:16,1:200)
      DOUBLE PRECISION  RPLMEM(0:4,0:5,1:16,1:200)
      DOUBLE PRECISION  EVM(0:8,1:48,1:48)
      DOUBLE PRECISION  ERMEM( 432)
      DOUBLE PRECISION  APL0( 65,800), APL1( 65,800),APL2( 65,800)
      DOUBLE PRECISION              APL3( 65,800),APL4( 65,800)
C
      INTEGER           N,N4,NG,    CPUSEC
      INTEGER           NCOUNT,NCC,NCCFP,NCC1
      INTEGER           K,J,L,M,MS
      INTEGER           MMAX,LMTOT,LMC,LMCS, MODE
      CHARACTER*30      FNW1,FNW2,FNW3
      CHARACTER*1       ITFLAG
C
      INTEGER           ICON
C
      DOUBLE PRECISION  A( 48, 48)
      DOUBLE PRECISION  EV( 48, 48)
      DOUBLE PRECISION  EI( 48),ER( 48)
      DOUBLE PRECISION  VX( 48)
C
      DOUBLE PRECISION  ERSORT(48)
C
      DOUBLE PRECISION  FNV0,FNV1,VA1,VA2,VA3,K00,X1
      FNV0(VA1,VA2,VA3,K00,X1)
     *                 = VA1*(X1-K00)+VA2*(X1-K00)*(X1-K00)
     *                     + VA3*(X1-K00)*(X1-K00)*(X1-K00)
      FNV1(VA1,VA2,VA3,K00,X1)
     *                 = VA1 + VA2*2.D0*(X1-K00)
     *                     + VA3* 3.D0 * (X1-K00)*(X1-K00)
C
C
C       **************************************************
C       ****      PARAMETER PARAMETER PARAMETER       ****
C       **************************************************
C
                 FNW1  = 'D14K.TES1.DATA'
                 FNW2  = 'D14K.TES2.DATA'
                 FNW3  = 'D14K.TES3.DATA'
CC               FNW1  = 'B06B.RLOD3G.DATA'
CC               FNW2  = 'B06B.RLOD3E.DATA'
                 K00 =          2.000000D0
                 VA1   =        1.D0
                 VA2   =        0.D0
                 VA3   =       -1.D0
                 B0    =        2.D0/K00
                 GAMMA2=B0*B0*FNV1(VA1,VA2,VA3,K00,K00)/2.D0/3.D0
                 NG    =         2
                 NW    =         0
                 N4    =        21
                 N     =       N4*4
                 EPS1  =         1.D-12
                 LMTOT =        16
                 MMAX  =         5
                 NPTEV =       4
C   ----->    0<=MMAX<=8 , 1<= NPTEV <= 800  , 1<= LMTOT <=48
C   ----->    1<=N4 <= 64
C ----> LMTOT X LMTOT IST DIE GROESSE DER EIGENWERT-MATRIX
C ----> NPTEV IST DIE ANZAHL DER STUETZPUNKTE ZWISCHEN 0 UND PI.
C ----> MMAX  ES WERDEN DIE M VON 0 BIS MMAX BETRACHTET.
C                SD=0 : REDUZ. AUSGABE AUF BILDSCHIRM
C                N  =  ANZAHL DER STUETZPKTE FUER L=2 PERIODEN
C                N  =  2*L*N MUSS <= 2048 = 2**10 SEIN
C                GAMMA = KOEFFIZIENT DER OBERFLAECHENSPANNUNG
C                B = STEADY-STATE RADIUS
C    ERGEBNISSE: VA1=1.D0 , VA2=0.D0 VA3=........
C    ..VA3..K00.....GAMMA.....B(TH=0)...B(TH=PI/2)...FACT.....N4.
C     -1.0  2.0   1/6-0.020     0.9019    1.053     0.01*COS  20
C     -1.0  2.0   1/6-0.010     0.9378    1.032     0.01*COS  20
C     -1.0  2.0   1/6-0.005     0.9626    1.019     0.01*COS  20
C     -1.0  2.0   0.1664        0.9973    1.001      ?        20
C     -1.0  2.0   1/6-0.000     0.9999    1.000     0.01*COS  20
C     -1.0  2.0   1/6+0.002     1.031     0.9847    0.01*COS  20
C     -1.0  2.0   1/6+0.0021    1.034     0.9832    0.01*COS  20
C     -1.0  2.0   1/6+0.0022    1.037     0.9813    0.01*COS  20
C     -1.0  2.0   1/6+0.0023    1.043     0.9786    0.01*COS  20
C     -1.0  2.0   0.1688        1.066      ?         ?        20
C     -1.0  2.0   0.1682        1.079      ?         ?        20
C     -1.0  2.0   0.1663        1.105     0.9452     ?        20
C     -1.0  2.0   0.1380        1.2..     0.83..     ?        20
C
C
C     **********************************************************
C     ***     BEMERKUNGEN                                    ***
C     **********************************************************
C
C     DIE GROESSEN
C          K0    ,K0T   ,K0TT  ,      ,KS    ,KST   ,KSTT  ,KSFF
C          FLB0T ,FLB0TT,FLB0FF,      ,FLBSKA,FLBSKB,FLBSKC,FLBSKD
C          FLB0K0,      ,FACTT ,      ,GSVBS ,GSVBST,      ,G0
C          V0K0  ,VSK0  ,      ,B0PT  ,      ,      ,      ,
C     SIND UEBERPRUEFT WORDEN UND SIND RICHTIG !!!!!!
C
C
C     **********************************************************
C     ***     KONSTANTEN                                     ***
C     **********************************************************
C
      ZERO = 0.D0
      ONE  = 1.D0
      C2P  = 2.D0
      C3P  = 3.D0
      C4P  = C2P + C2P
      C5P  = C2P + C3P
      C6P  = C3P + C3P
      C7P  = C3P + C4P
      C8P  = C4P + C4P
      C9P  = C4P + C5P
      C10P = C5P + C5P
      C12P = C6P + C6P
      C14P = C7P + C7P
      C15P = C8P + C7P
      C18P = C9P + C9P
      C45P = C18P + C18P + C9P
      C50P = C45P + C5P
      C54P = C18P + C18P + C18P
      C56P = C54P + C2P
      C70P = C56P + C14P
      C105P= C54P + C56P - C5P
      PI=C4P*DATAN(ONE)
      DTH =N4
      DTH= PI/(C2P*DTH)
C     **********************************************************
C     *** TABELLE FUER COSINUS UND SINUS                     ***
C     **********************************************************
      WRITE(6,*) ' ANLEGEN VON TABELLEN FUER COSINUS UND SINUS '
      DO 111 I=1,N4+1
        HILF1=I-1
        TCOS(I)=DCOS( HILF1* DTH)
        TSIN(I)=DSIN( HILF1* DTH)
  111 CONTINUE
        TCOS(N4+1)=ZERO
        TSIN(   1)=ZERO
        TCOS(   1)=ONE
        TSIN(N4+1)=ONE
C
C     **********************************************************
C     *** TABELLE FUER P_L(COS(THETA)) FUER STAEDY-STATE SUCHE**
C     **********************************************************
      WRITE(6,*) ' ANLEGEN VON TABELLEN FUER P_L (COS (THETA) )'
      DO 120 I=1,N4+1
       HI=I-1
       DO 122 J=1,N4+1
        IF (J .EQ. N4+1 .AND. NG .EQ. 2) THEN
           AL0(I,N4+1)=ZERO
           AL1(I,N4+1)=ZERO
           AL2(I,N4+1)=ZERO
           AL3(I,N4+1)=ZERO
           AL4(I,N4+1)=ZERO
           GOTO 122
        ENDIF
C ----> DIE ABLEITUNGEN NACH THETA VON  P_L( COS(THETA) )
        CALL DNLPDT(J+J-2,0,0,TCOS(I),H3,ICON)
        AL0(I,J)=H3
        CALL DNLPDT(J+J-2,0,1,TCOS(I),H3,ICON)
        AL1(I,J)=H3
        CALL DNLPDT(J+J-2,0,2,TCOS(I),H3,ICON)
        AL2(I,J)=H3
        CALL DNLPDT(J+J-2,0,3,TCOS(I),H3,ICON)
        AL3(I,J)=H3
        CALL DNLPDT(J+J-2,0,4,TCOS(I),H3,ICON)
        AL4(I,J)=H3
  122  CONTINUE
  120 CONTINUE
C
C     **********************************************************
C     *** TABELLE FUER P_L(COS(THETA)) FUER EIGENWERTBERECHN. **
C     **********************************************************
      WRITE(6,*) ' ANLEGEN VON TABELLEN FUER P_(L,M) (COS (THETA) )'
      H111 = ONE / NPTEV
      DTHEV = PI * H111
C
      DO 133 NPC  = 1, NPTEV
       H111 = NPC
       THETA = H111 * DTHEV - DTHEV/ C2P
       COSTH=DCOS(THETA)
C
       DO 133  J=1, N4
        CALL DNLPDT(J+J-2,0,0,COSTH,H3,ICON)
        APL0(J,NPC) = H3
        CALL DNLPDT(J+J-2,0,1,COSTH,H3,ICON)
        APL1(J,NPC) = H3
        CALL DNLPDT(J+J-2,0,2,COSTH,H3,ICON)
        APL2(J,NPC) = H3
        CALL DNLPDT(J+J-2,0,3,COSTH,H3,ICON)
        APL3(J,NPC) = H3
        CALL DNLPDT(J+J-2,0,4,COSTH,H3,ICON)
        APL4(J,NPC) = H3
  133 CONTINUE
C
      DO 138 NPC  = 1, NPTEV
       H111 = NPC
       THETA = H111 * DTHEV - DTHEV/ C2P
       COSTH=DCOS(THETA)
      DO 138 M=0,MMAX
      DO 138 LMC=1,LMTOT
        L=LMC-1+IABS(M)
      DO 138 J=0,4
          CALL DNLPDT(L,M,J,COSTH,H3,ICON)
          RPLMEM(J,M,LMC,NPC) = H3
  138 CONTINUE
C
C     **********************************************************
C     *** OEFFNEN DER DATEIEN UND SCHREIBEN DER PARAMETER    ***
C     **********************************************************
C
      NCOUNT=0
C     >>>> NCOUNT IST DER EIGENWERT-ZAEHLER.
      IF(NW .EQ. 1) OPEN(22,FILE=FNW1)
      IF(NW .EQ. 1) REWIND 22
      IF(NW .EQ. 1) OPEN(23,FILE=FNW2)
      IF(NW .EQ. 1) REWIND 23
      IF(NW .EQ. 1) OPEN(24,FILE=FNW3)
      IF(NW .EQ. 1) REWIND 24
      IF (NW .EQ. 1) THEN
C        WRITE(22,1118) NCOUNT
         WRITE(22,1116) '   N4=  ',N4,  '  EPS1= ', EPS1,' MMAX= ',MMAX
     &                ,' LMTOT= ',LMTOT,' NPTEV= ',NPTEV
         WRITE(22,1112) 'GAMMA','A(2)','B(TH=0)','B(TH=PI/2)'
     &                         ,'NCC','SVNQU'
C
         WRITE(23,1116) '   N4=  ',N4,  '  EPS1= ', EPS1,' MMAX= ',MMAX
     &                ,' LMTOT= ',LMTOT,' NPTEV= ',NPTEV
         WRITE(23,1112) ' A(2)','EIGENWERT','MODE M','SVNQUT'
         WRITE(24,1112) 'GAMMA','A(2)','B(TH=0)','B(TH=PI/2)'
         WRITE(24,*) ' DANACH KOMMEN DIE BK(I), I=1,N4+1 '
 1112    FORMAT(4(1X,A12),1(1X,A8),1(1X,A12))
 1116    FORMAT(1X,A8,I4,A8,1PE12.5,3(A8,I4))
 1118    FORMAT(5X,I8)
       ENDIF
C
C     **********************************************************
C     *** STARTKURVE UND STARTGAMMA
C     **********************************************************
      WRITE(6,*) ' BEGINN DER ITERATIONEN      '
C
        DO 112 I=1,N4+1
          BK(I)=0.D0
  112   CONTINUE
        BK(1)=B0 *DSQRT(C2P)
        BK(2)=  .010D0 * DSQRT(C2P/C5P)
CC      BK(2)= -.010D0 * DSQRT(C2P/C5P)
C   >>>> P_{0,0} (NORMIERT) = DSQRT(1/2) * P_{0,0} (UNNORMIERT)
C   >>>> P_{2,0} (NORMIERT) = DSQRT(5/2) * P_{2,0} (UNNORMIERT)
C
C     GAMMAK = GAMMA2 - ZERO* (ONE/C6P)+(ONE/C8P)
C     GAMMAK = GAMMA2 - .0000D0
      GAMMAK = 0.1667D0
CC    GAMMAK = 0.1665D0
CC    BK(1) = 1.34948D0
CC    BK(2) = 0.19416D0
CC    BK(3) = 0.01336D0
CC    BK(4) =-0.00211D0
CC    BK(5) =-0.00041D0
CC    GAMMAK= 0.12078D0
CC    BK(1) = 1.32865D0
CC    BK(2) = 0.22073D0
CC    BK(3) = 0.01671D0
CC    BK(4) =-0.00328D0
CC    BK(5) =-0.00064D0
CC    GAMMAK= 0.10733D0
      IF (NG .EQ. 2) BK(N4+1)=GAMMAK
      DO 222 I=1,N4+1
        IF (NG .EQ. 2 .AND. I .EQ. 2) GOTO 222
        BKVOR(I)=BK(I)
  222 CONTINUE
C
C     **********************************************************
C     ***  HIER FAENGT DER GROSSE LOOP AN
C     **********************************************************
C
      DO 9000 VARIA =  0.0100D0 ,  2.0001D0 ,  0.0200D0
CC    DO 9000 VARIA =  0.0010D0 ,  1.0001D0 ,  0.0020D0
CC    DO 9000 VARIA = -0.0005D0 , -0.5000D0 , -0.0010D0
C        IF (VARIA .GT. ZERO) GOTO 4999
C
C
C
C
        BK(2)=  VARIA  * DSQRT(C2P/C5P)
CC      IF (BK(2) .LT. 0.22072D0) GOTO 9000
      DO 322 I=1,N4+1
        IF (NG .EQ. 2 .AND. I .EQ. 2) GOTO 322
        H11 = BK(I)
        BK(I)= BK(I) + (BK(I) - BKVOR(I))
        BKVOR(I)=H11
  322 CONTINUE
        NCC=0
        NCCFP=0
        NCC1=0
  109   CONTINUE
        NCCFP=NCCFP+1
        IF (NCCFP-1 .EQ. 1000 )  NCCFP=1
        SVNQUA=1.D50
        SVNQ99=-1.D50
        IACTM = 0
C
 1778 NCC=NCC+1
      IACT = IACTM + 1
      IF (IACT .EQ. 2) IACT =IACT + 1
      IF (IACT .EQ. N4+2) IACT = 1
      IACTM = IACT
C
      IF (IACT .EQ. N4+1) DELDK = .0010D0*0.5D0**1
      IF (IACT .LT. N4+1) DELDK = .0010D0*0.5D0**IACT
      DELDKM = DELDK
      NCC9 = 0
C
      ITFLAG='1'
      BKMEM = BK(IACT)
C
 1777 CONTINUE
      SVNQUN=ZERO
      NCC9 = NCC9 + 1
      GAMMAK = BK(N4+1)
      DO 140 I=1,N4+1
       B0S = ZERO
       B1S = ZERO
       B2S = ZERO
       B3S = ZERO
       B4S = ZERO
C ----> DIE BERECHNUNG VON B(I) = SUM( BK(J) * P_{J+J-2}(TCOS(I)) )
       DO 130 J=1,N4
        B0S =B0S +BK(J)*AL0(I,J)
        B1S =B1S +BK(J)*AL1(I,J)
        B2S =B2S +BK(J)*AL2(I,J)
        B3S =B3S +BK(J)*AL3(I,J)
        B4S =B4S +BK(J)*AL4(I,J)
  130  CONTINUE
        G0S = B0S * B0S + B1S * B1S
        G0S2= G0S * G0S
        G0S3= G0S * G0S2
        G0S4= G0S2* G0S2
        G0SW= DSQRT(G0S)
        B0S2= B0S * B0S
        B0S3= B0S * B0S2
        B0S4= B0S2* B0S2
        B0S5= B0S2* B0S3
        B0S6= B0S3* B0S3
        B0S7= B0S3* B0S4
        B0S8= B0S4* B0S4
        B0S9= B0S4* B0S5
        B0S10= B0S5* B0S5
        B0S11= B0S5* B0S6
        B0S12= B0S6* B0S6
        B2S2= B2S * B2S
        B2S3= B2S * B2S2
        IF (I .EQ. 1) GOTO 144
C
C    XLBK  =LAPL.BELTR.OPER. AUF (2*MITTL.KRUEMM.) ANGEWENDET
C
        H1 = -           B4S *B0S4*G0S2 + C10P*B3S *B2S *B1S *B0S4*G0S
     &       + C10P*B3S *B1S *B0S5*G0S  - C6P *B3S *B1S *B0S3*G0S2
     &       + C18P*B2S3*B0S6           - C15P*B2S3*B0S4*G0S
     &       + C54P*B2S2*B0S7           - C70P*B2S2*B0S5*G0S
     &       + C18P*B2S2*B0S3*G0S2      + C54P*B2S *B0S8
     &      - C105P*B2S *B0S6*G0S       + C56P*B2S *B0S4*G0S2
     &       - C5P *B2S *B0S2*G0S3      + C18P*B0S9
     &       - C50P*B0S7*G0S            + C45P*B0S5*G0S2
     &       - C12P*B0S3*G0S3           -      B0S *G0S4
        HLB1= H1  / ( G0S4*B0S3*G0SW )
C
        H2 = - C2P *B3S *B0S4*G0S2      + C7P *B2S2*B1S *B0S4*G0S
     &       + C14P*B2S *B1S *B0S5*G0S  - C6P *B2S *B1S *B0S3*G0S2
     &       + C7P *B1S *B0S6*G0S       - C8P *B1S *B0S4*G0S2
     &       -      B1S *B0S2*G0S3      -      B1S *G0S4
        H2 =H2 * TCOS(I) / TSIN(I)
        HLB2= H2 / ( G0S4*B0S3*G0SW )
C
        H3 =   C2P *B2S *B0S4*G0S2      -      B2S *B0S2*G0S3
     &       + C2P *B0S5*G0S2           - C2P *B0S3*G0S3
     &       + (-   B1S *B0S2*G0S3 ) * TCOS(I) /TSIN(I)
        H3 =  H3 *TCOS(I)*TCOS(I) /  ( TSIN(I)*TSIN(I) )
        HLB3= H3 / ( G0S4*B0S3*G0SW )
C
        XLBK=  HLB1 + HLB2 + HLB3
C
C
C     XKAP    = 2*MITTLERE KRUEMMUNG
C
        H8 = -      B0S3      + C3P *B0S *G0S  -    B2S *B0S2
        HKA1= H8  / ( B0S *G0S *G0SW )
        H9 = -      B1S *G0S
        H9 =    H9  * TCOS(I) / TSIN(I)
        HKA2=  H9  / ( B0S *G0S *G0SW )
C
        XKAP=   HKA1 + HKA2
C
  144   CONTINUE
        IF (I .GT. 1) GOTO 145
C
C     XLBK = LAPL.BELTR.OPERATOR AUF (2*MITTL.KRUEMM.) ANGEWANDT
C
        H1   = -        B4S *B0S8      +  C3P *B2S3*B0S6
     &         +   C2P *B2S2*B0S7
        HLB1 = H1 / B0S12
        H2   = -   C2P *B4S *B0S8      +  C7P *B2S3*B0S6
     &         +   C8P *B2S2*B0S7      -  C3P *B2S *B0S8
        HLB2 = H2 / B0S12
        H3   =   ONE/C3P *B4S *B0S8  -       C2P *B2S3*B0S6
     &         -     C2P *B2S2*B0S7  +   ONE/C3P *B2S *B0S8
        HLB3 = H3 / B0S12
        XLBK=HLB1 + HLB2 + HLB3
C
C     XKAP     = 2*MITTLERE KRUEMMUNG
C
        H8 =    C2P *B0S3   -      B2S *B0S2
        HKA1=  H8 /  B0S4
        H9 =  -      B2S *B0S2
        HKA2=  H9 /  B0S4
        XKAP=HKA1 + HKA2
C
  145   CONTINUE
C
        VN(I) =  (GAMMAK*XLBK+FNV0(VA1,VA2,VA3,K00,XKAP)) *G0SW/B0S
        SVNQUN=SVNQUN+(VN(I)*VN(I))
        BTH(I) = B0S
  140 CONTINUE
C
CC    IF (VARIA .GT. ZERO) GOTO 4993
CCC   WRITE (6,*)  ITFLAG,BK(IACT) , SVNQUN,DELDK
      IF (DELDK  .LT. 1.D-05*DELDKM) GOTO 1912
      IF (NCC9   .GE.  50   ) GOTO 1912
      IF (ITFLAG .EQ. '1') THEN
          BKC = BK(IACT)
          SVNQUC = SVNQUN
          BK(IACT) = BK(IACT) + DELDK
          ITFLAG = '2'
          GOTO 1777
      ENDIF
C
      IF (ITFLAG .EQ. '2') THEN
          BKR = BK(IACT)
          SVNQUR = SVNQUN
          BK(IACT) = BK(IACT) - DELDK - DELDK
          ITFLAG = '3'
          GOTO 1777
      ENDIF
C
      IF (ITFLAG .EQ. '3') THEN
         BKL = BK(IACT)
         SVNQUL = SVNQUN
      ENDIF
      IF (ITFLAG .EQ. 'L') THEN
         BKL = BK(IACT)
         SVNQUL = SVNQUN
      ENDIF
      IF (ITFLAG .EQ. 'R') THEN
         BKR = BK(IACT)
         SVNQUR = SVNQUN
      ENDIF
      IF (ITFLAG .EQ. 'C') THEN
         BKC = BK(IACT)
         SVNQUC = SVNQUN
CC       IF (SVNQ99 .EQ. SVNQUC)   WRITE(6,*) BKL,BKC,BKR
CC       IF (SVNQ99 .EQ. SVNQUC)   WRITE(6,*) SVNQUL,SVNQUC,SVNQUR
         IF (SVNQ99 .EQ. SVNQUC) GOTO 1912
      ENDIF
CC      WRITE(6,*) ITFLAG,NCC9
CC      WRITE(6,*) BKL,BKC,BKR
C
        ITFLAG='C'
         IF (SVNQUL .LT. SVNQUC .AND. SVNQUL .LT. SVNQUR) ITFLAG='L'
         IF (SVNQUR .LT. SVNQUC .AND. SVNQUR .LT. SVNQUL) ITFLAG='R'
 1451    IF (ITFLAG .EQ. 'L') THEN
             BKR = BKC
             BKC = BKL
             SVNQUR = SVNQUC
             SVNQUC = SVNQUL
             BK(IACT) = BKC - DELDK
             GOTO 1777
          ENDIF
          IF (ITFLAG .EQ. 'R') THEN
             BKL = BKC
             BKC = BKR
             SVNQUL = SVNQUC
             SVNQUC = SVNQUR
             BK(IACT) = BKC + DELDK
             GOTO 1777
          ENDIF
        H1 = BKC - BKL
        H2 = BKR - BKC
        DA2L = 2.D0/(H1*(H1+H2))
        DA2R = 2.D0/(H2*(H1+H2))
        DA2C = - DA2L - DA2R
        DA1L = -H2/(H1*(H1+H2))
        DA1R =  H1/(H2*(H1+H2))
        DA1C = - DA1L - DA1R
        SVNQU2 = SVNQUL*DA2L + SVNQUC*DA2C + SVNQUR*DA2R
        SVNQU1 = SVNQUL*DA1L + SVNQUC*DA1C + SVNQUR*DA1R
        IF (DABS(SVNQU2) .GT. 1.D-70) THEN
            BK(IACT)   = BKC - SVNQU1/ SVNQU2
        ELSE
            BK(IACT) = BKC
        ENDIF
        IF (BK(IACT) .GE. BKL .AND. BK(IACT) .LE. BKR) THEN
          SVNQ99 = SVNQUC
CC        IF (BK(IACT) .LT. BKC - (BKC-BKL)*.1D0 .OR.
CC   &        BK(IACT) .GT. BKC + (BKR-BKC)*.1D0) THEN
             IF (BK(IACT) .LT. BKC) THEN
                BKR = BKC
                SVNQUR = SVNQUC
             ELSE
                BKL = BKC
                SVNQUL = SVNQUC
             ENDIF
CC           DELDK = DMIN1( DABS(H1) , DABS(H2) )
CC        ELSE
CC           ITFLAG = '1'
             DELDK = DELDK*.050D0
CC        ENDIF
          GOTO 1777
        ELSE
          IF (SVNQU1   .LE. 0.D0) ITFLAG='R'
          IF (SVNQU1   .GT. 0.D0) ITFLAG='L'
          GOTO 1451
        ENDIF
C
 1912 CONTINUE
      IF(SVNQUN .LT. SVNQUA ) THEN
          SVNQUA = SVNQUN
       ELSE
          BK(IACT) = BKMEM
      ENDIF
      IF (MOD(NCC-1, 67) .EQ. 0)
     &WRITE(6,1219) NCC,IACT,NCC9 , SVNQUA , BK(IACT) ,DELDK
 1219 FORMAT(1X,'NCC=',I5,' IACT=',I3,' NCC9=',I3,' QU=',1PE10.3
     &                ,' BK(IACT)=',1PE11.4,' DELDK=',1PE10.3)
C
      IF (SVNQUA .GT. EPS1 .AND. NCC .LT. 20000) GOTO 1778
C
C
C
C
 198  CONTINUE
      WRITE(6,*) ' '
      IF (SVNQUN .LE. EPS1) THEN
           WRITE(6,1122) ' S*O*L*U*T*I*O*N : ',NCC
     &              ,' GAMMA=',GAMMAK,' BK(2)=',BK(2)
           WRITE(6,1123) (BK(I),I=1,N4+1)
      ELSE
           WRITE(6,1122) '   S O L U T I O N : ',NCC
     &              ,' GAMMA=',GAMMAK,' BK(2)=',BK(2)
           WRITE(6,1123) (BK(I),I=1,N4+1)
      ENDIF
C
 1122 FORMAT(1X,A22,1X,I8,2(1X,A6,F12.7))
 1123 FORMAT(8(1X,F9.5))
C     IF (NCCFP .EQ. 1) WRITE (6,*) ' GAMMA=',GAMMAK
C       IF (NCCFP .EQ. 1)
C    &     WRITE(6,1005) 'NCC=', NCC , ' I=', I, 'DBK=', INHB(I)
C    &               ,   ' BK=', BK(I), ' VN=', VN(I), 'BTH=', BTH(I)
 1005 FORMAT(1X,A4,I6,1X,A3,I4,4(1X,A4,1PE10.3))
C4993 CONTINUE
C     DO 4994 I=1,N4+1
C        WRITE(24,*)   BTH(I)*TCOS(I) , BTH(I)*TSIN(I)
C4994 CONTINUE
C     DO 4995 I=N4 , 1 , -1
C        WRITE(24,*)  -BTH(I)*TCOS(I) , BTH(I)*TSIN(I)
C4995 CONTINUE
C     DO 4996 I=2,N4+1
C        WRITE(24,*)  -BTH(I)*TCOS(I) ,-BTH(I)*TSIN(I)
C4996 CONTINUE
C     DO 4997 I=N4 ,1 , -1
C        WRITE(24,*)   BTH(I)*TCOS(I) ,-BTH(I)*TSIN(I)
C4997 CONTINUE
C
C
C ---->  LOESUNG GEFUNDEN.
      IF (NW .EQ. 1) THEN
C        WRITE(22,1111) BK(N4+1),BK(2),BTH(1),BTH(N4+1),NCC,SVNQUN
         WRITE(22,1111) GAMMAK  ,BK(2),BTH(1),BTH(N4+1),NCC,SVNQUN
         WRITE(24,1111) GAMMAK  ,BK(2),BTH(1),BTH(N4+1),NCC,SVNQUN
         WRITE(24,1123) (BK(I),I=1,N4+1)
 1111    FORMAT(4(1X,1PE12.5),1(1X,I8),1(1X,1PE12.5))
      ENDIF
C
C
C ---->  EIGENWERTE BERECHNEN
C ---->                       EIGENWERTE BERECHNEN
C ---->                                            EIGENWERTE BERECHNEN
C4999 CONTINUE
C
C
C
C ----> DELETE EIGENVALUE - MATRIX
      DO 5005 M=0,MMAX
      DO 5005 LMC =1,LMTOT
      DO 5005 LMCS=1,LMTOT
        EVM(M,LMC,LMCS) = ZERO
 5005 CONTINUE
C
C
C ----> START LOOP OVER THE THETA'S
      DO 5999 NPC  = 1, NPTEV
       H111 = NPC
       THETA = H111 * DTHEV - DTHEV/ C2P
       COSTH = DCOS(THETA)
       SINTH = DSIN(THETA)
       SVNQUT= ZERO
C
C ----> THE VALUES FOR B0,B0T,B0TT,B0TTT,B0TTTT
C
C      B0    = ONE
       B0    = ZERO
       B0T   = ZERO
       B0TT  = ZERO
       B0TTT = ZERO
       B0TTTT= ZERO
       DO 5010 J=1, N4
        B0    = B0     + BK(J) * APL0(J,NPC)
        B0T   = B0T    + BK(J) * APL1(J,NPC)
        B0TT  = B0TT   + BK(J) * APL2(J,NPC)
        B0TTT = B0TTT  + BK(J) * APL3(J,NPC)
        B0TTTT= B0TTTT + BK(J) * APL4(J,NPC)
 5010 CONTINUE
C
C     WRITE(6,*) ' B0(',THETA,') = ',B0
C
      SINTH2 = SINTH * SINTH
      SINTH3 = SINTH * SINTH2
      SINTH4 = SINTH2* SINTH2
      B02  = B0 * B0
      B03  = B0 * B02
      B04  = B02 * B02
      B05  = B02 * B03
      B0T2 = B0T* B0T
      B0T3 = B0T* B0T2
      B0T4 = B0T2* B0T2
      B0T5 = B0T2* B0T3
      B0TT2= B0TT *B0TT
C
C
C ----> KS = KAPPA (SCHLANGE)
C ----> KST = ABLEITUNG VON KS NACH THETA
C ----> KSTT = 2-TE ABLEITUNG VON KS NACH THETA
C ----> KSFF = 2-TE ABLEITUNG VON KS NACH PHI
      HA1 =           (    -B0TT   *B0T2 *B02
     &                     + C2P*B0TT   *B04
     &                     - C5P *B0T2*B03
     &                     - C2P *B05 )
      HA2 =                (      B0T5
     &                    +C3P *B0T3 *B02
     &                    +C2P *B0T*B04          )
      KSA = SINTH2    * HA1
     &     + SINTH*COSTH * HA2
      HB1 =            (  C3P*B0TT *B0T*B03
     &                   -C3P*B0T3 * B02 )
      HB2 =               (   -B0T2*B03
     &                      -B05             )
      KSB = SINTH2 * HB1
     &     + SINTH*COSTH *HB2
      HC1 =           ( -B0T2*B03 -      B05)
      KSC =  SINTH2 * HC1
      KSD = - B0T4 *B0 - C2P*B0T2*B03 - B05
      KSE = DSQRT(B0T2+B02) *SINTH2 *B02 *(B0T2+B02)**2
C     WRITE(6,*) ' THETA=', THETA, 'KSE =', KSE , 'B0 =', B0,'B0T=',B0T
C --> KS = ( KSA *BS + KSB *BST + KSC *BSTT + KSD*BSFF )  / KSE
C
      HA1T =                   (   -B0TTT  *B0T2 *B02
     &                             -B0TT   *B0T    *B02 *C2P * B0TT
     &                             -B0TT   *B0T2 *B0      *C2P * B0T
     &                       +C2P*B0TTT *B04
     &                       +C2P*B0TT  *B03 *C4P * B0T
     &                       -C5P *B0T  *B03 *C2P * B0TT
     &                       -C5P*B0T2*B02 *C3P * B0T
     &                      -C2P  *B04 * C5P * B0T )
      HA2T =                      (   B0T4     * C5P * B0TT
     &                            +C3P*B0T2 *B02 * C3P * B0TT
     &                            +C3P*B0T3 *B0     * C2P * B0T
     &                            +C2P *B0TT *B04
     &                            +C2P *B0T  *B03 * C4P * B0T )
      KSAT = C2P *SINTH * COSTH * HA1
     &    +(ONE - C2P*SINTH2) *  HA2
      KSAT = KSAT + SINTH2 * HA1T
     &            + SINTH * COSTH*HA2T
C
      HB1T =                  (  C3P *B0TTT*B0T *B03
     &                          +C3P *B0TT *B0TT*B03
     &                          +C3P *B0TT *B0T *B02 * C3P * B0T
     &                          -C3P *B0T2 * B02 * C3P * B0TT
     &                          -C3P *B0T3 * B0     * C2P * B0T )
      HB2T =                ( -B0T    *B03     * C2P * B0TT
     &                         - B0T2*B02       * C3P * B0T
     &                         - B04 * C5P * B0T      )
      KSBT = C2P *COSTH*SINTH *HB1
     &     +(ONE -C2P*SINTH2) * HB2
      KSBT = KSBT + SINTH2 *HB1T
     &         +SINTH*COSTH* HB2T
C
      HC1T =                ( -B0T   *B03    * C2P * B0TT
     &                       - B0T2*B02      * C3P * B0T
     &                       - B04    * C5P * B0T     )
      KSCT =  SINTH  *C2P *COSTH * HC1
      KSCT = KSCT +SINTH2 * HC1T
C
      KSDT=  (     - B0T3 *B0     * C4P * B0TT
     &             - B0T4 *B0T
     &                   - C2P*B0T  *B03    * C2P * B0TT
     &                   - C2P*B0T2*B02     * C3P * B0T
     &                   -  B04    *C5P * B0T  )
C
      KSET = DSQRT(B0T2+B02) * (
     &          SINTH    *B02 *(B0T2+B02)**2 * C2P *COSTH
     &         +SINTH2 *B0      *(B0T2+B02)**2 * C2P * B0T
     &         +SINTH2 *B02 *(B0T2+B02) *C5P*(B0T*B0TT+B0*B0T) )
C --> KS = ( KSA *BS + KSB *BST + KSC *BSTT + KSD*BSFF )  / KSE
      KSTA =        KSAT    - KSA * KSET / KSE
      KSTB = KSA  + KSBT    - KSB * KSET / KSE
      KSTC = KSB  + KSCT    - KSC * KSET / KSE
      KSTD = KSC
      KSTF =        KSDT    - KSD * KSET / KSE
      KSTG = KSD
C --> KST= ( KSTA *BS + KSTB *BST + KSTC *BSTT + KSTD*BSTTT
C    &                + KSTF *BSFF  + KSTG*BSTFF )  / KSE
C
      KSATT= (C2P-C4P*SINTH2)* HA1
     &    +(-C4P *SINTH*COSTH) * HA2
      KSATT= KSATT+ C4P *SINTH*COSTH * HA1T
     &             +(C2P - C4P*SINTH2) * HA2T
      KSATT= KSATT+ SINTH2 * (     -B0TTTT *B0T2 *B02
     &                             -B0TTT  *B0T    *B02    * C2P * B0TT
     &                             -B0TTT  *B0T2 *B0       * C2P * B0T
     &                             -B0TT   *B0T  *B02 *C2P * C2P*B0TTT
     &                             -B0TT2*B0TT *B02 *C2P
     &                             -B0TT2*B0T    *B0  *C2P * C2P * B0T
     &                             -B0TTT *B0T3 *B0 *C2P
     &                             -B0TT  *B0T2 *B0 *C2P * C3P * B0TT
     &                             -B0TT  *B0T3 *B0T*C2P )
      KSATT=KSATT+SINTH2 * (    C2P*B0TTTT *B04
     &                       +  C2P*B0TTT *B03 * C4P * B0T
     &                       -C2P* B0TT *B03 * B0TT
     &                       -C2P* B0T *B02 * B0TT * C3P * B0T
     &                       -C2P* B0T *B03 * B0TTT
     &                       -C5P*B0T2 *B02 *C3P * C3P * B0TT
     &                       -C5P*B0T3 *B0     *C3P * C2P * B0T
     &                      -C2P  *B03 * C5P * B0T * C4P * B0T
     &                      -C2P  *B04 * C5P * B0TT       )
      KSATT= KSATT+ SINTH*COSTH*(   B0T4     * C5P * B0TTT
     &                      +    B0T3     * C5P * B0TT * C4P * B0TT
     &                      +C3P*B0T   *B02 * C3P * B0TT   * C2P *B0TT
     &                      +C3P*B0T2 *B0     * C3P * B0TT * C2P * B0T
     &                      +C3P*B0T2 *B02 * C3P * B0TTT
     &                      +C3P*B0T3 *B0     * C2P * C4P * B0TT
     &                      +C3P*B0T4 *B0T    * C2P
     &                      +C2P *B0TTT*B04
     &                      +C2P *B0TT *B03 * C4P* B0T
     &                      +C2P *B0T     *B03 * C4P * C2P *B0TT
     &                      +C2P *B0T2    *B02 * C4P * C3P * B0T  )
C
      KSBTT= (C2P-C4P*SINTH2) *HB1
     &     +(-C4P*COSTH*SINTH) * HB2
      KSBTT= KSBTT + C4P *COSTH*SINTH * HB1T
     &     +(C2P -C4P*SINTH2) * HB2T
      KSBTT= KSBTT+ SINTH2 *(    C3P *B0TTTT*B0T *B03
     &                          +C3P *B0TTT *B0TT*B03
     &                          +C3P *B0TTT *B0T *B02 * C3P * B0T
     &                          +C3P *B0TT     *B03 * C2P * B0TTT
     &                          +C3P *B0TT2    *B02 * C3P * B0T
     &                          +C3P *B0TTT*B0T2 *B02 *C3P
     &                          +C3P *B0TT *B0T   *B02 *C3P * C2P*B0TT
     &                          +C3P *B0TT *B0T2 *B0   *C3P * C2P*B0T )
      KSBTT= KSBTT+ SINTH2 *( -C3P *B0T     * B02 * C3P *B0TT *C2P*B0TT
     &                          -C3P *B0T2 * B0    * C3P *B0TT *C2P*B0T
     &                          -C3P *B0T2 * B02 * C3P *B0TTT
     &                          -C3P *B0T3 * B0   * C2P *C4P * B0TT
     &                          -C3P *B0T4 * B0T * C2P       )
      KSBTT = KSBTT
     &         +SINTH*COSTH*( -B0TT   *B03     * C2P * B0TT
     &                         -B0T   *B02     * C2P * B0TT * C3P * B0T
     &                         -B0T   *B03     * C2P * B0TTT
     &                         - B0T2*B02       * C3P * C3P * B0TT
     &                         - B0T3*B0        * C3P * C2P * B0T
     &                         - B03 * C5P * B0T * C4P*B0T
     &                         - B04 * C5P * B0TT     )
C
      KSCTT=  (C2P-C4P*SINTH2)  *HC1
     &     +  C4P* SINTH *COSTH *HC1T
      KSCTT= KSCTT+SINTH2 *( -B0TT    *B03    * C2P * B0TT
     &                        -B0T   *B02    * C2P * B0TT * C3P * B0T
     &                        -B0T   *B03    * C2P * B0TTT
     &                       - B0T2*B02      * C3P * C3P * B0TT
     &                       - B0T3*B0       * C3P * C2P * B0T
     &                       - B03    * C5P * B0T * C4P * B0T
     &                       - B04    * C5P * B0TT    )
C
      KSDTT=        - B0T2 *B0     * C4P * B0TT *C3P * B0TT
     &             - B0T3 *B0T    * C4P * B0TT
     &             - B0T3 *B0     * C4P * B0TTT
     &             - B0T4 * C5P * B0TT
      KSDTT=KSDTT        - C2P*B0TT *B03    * C2P * B0TT
     &                   - C2P*B0T  *B02    * C2P * B0TT * C3P * B0T
     &                   - C2P*B0T  *B03    * C2P * B0TTT
     &                   - C2P*B0T2*B02     * C3P *C3P * B0TT
     &                   - C2P*B0T3*B0      * C3P *C2P * B0T
     &                   -  B03    *C5P * B0T * C4P * B0T
     &                   -  B04    *C5P * B0TT
C
      KSETT = DSQRT(B0T2+B02) * (
     &         + C2P *(ONE - C2P*SINTH2) *B02 *(B0T2+B02)**2
     &         + C2P *SINTH*COSTH *B0    *(B0T2+B02)**2 * C2P * B0T
     &         +C2P*SINTH*COSTH *B0   *(B0T2+B02)**2 * C2P * B0T
     &         +SINTH2 *B0T     *(B0T2+B02)**2 * C2P * B0T
     &         +SINTH2 *B0      *(B0T2+B02)**2 * C2P * B0TT
     &   +C2P*SINTH*COSTH *B02 *(B0T2+B02) *C5P*(B0T*B0TT+B0*B0T)
     &   +SINTH2 *B0 *C2P*B0T   *(B0T2+B02) *C5P*(B0T*B0TT+B0*B0T)
     &   +SINTH2 *B02 *(B0T2+B02) *C5P*(B0TT*B0TT+B0T*B0T)
     &   +SINTH2 *B02 *(B0T2+B02) *C5P*(B0T*B0TTT+B0*B0TT) )
      KSETT = KSETT + DSQRT(B0T2+B02) * (
     &         + C2P *SINTH*COSTH *B02 *(B0T2+B02)    *C5P
     &         +SINTH2 *B0      *(B0T2+B02)    * C2P * B0T * C5P
     &         +SINTH2 *B02 *    C5P * C3P  *(B0T*B0TT+B0*B0T) )
     &             * (B0T*B0TT+B0*B0T)
C --> KS = ( KSA *BS + KSB *BST + KSC *BSTT + KSD*BSFF )  / KSE
C --> KST= ( KSTA *BS + KSTB *BST + KSTC *BSTT + KSTD*BSTTT
C    &                + KSTF *BSFF  + KSTG*BSTFF )  / KSE
C --> KSTT= ( KSTTA *BS    + KSTTB*BST  + KSTTC *BSTT + KSTTD*BSTTT
C    &      + KSTTF*BSTTTT + KSTTG*BSFF + KSTTH*BSTFF + KSTTI*BSTTFF )
C    &                                        /  KSE
      KSTTA=(KSATT*KSE - C2P*KSET*(KSAT -KSET*KSA/KSE) -KSA*KSETT)/KSE
     &       +  ZERO                             + ZERO
      KSTTB=(KSBTT*KSE - C2P*KSET*(KSBT -KSET*KSB/KSE) -KSB*KSETT)/KSE
     &       + C2P* (KSAT - KSA * KSET / KSE)    + ZERO
      KSTTC=(KSCTT*KSE - C2P*KSET*(KSCT -KSET*KSC/KSE) -KSC*KSETT)/KSE
     &       + C2P* (KSBT - KSB * KSET / KSE)    + KSA
      KSTTD=    ZERO
     &       + C2P* (KSCT - KSC * KSET / KSE)    + KSB
      KSTTF=    ZERO
     &       +  ZERO                             + KSC
      KSTTG=(KSDTT*KSE - C2P*KSET*(KSDT -KSET*KSD/KSE) -KSD*KSETT)/KSE
     &       +  ZERO                             + ZERO
      KSTTH=    ZERO
     &       + C2P* (KSDT - KSD * KSET / KSE)    + ZERO
      KSTTI=    ZERO
     &       +  ZERO                             + KSD
C --> KS = ( KSA *BS + KSB *BST + KSC *BSTT + KSD*BSFF )  / KSE
C --> KST= ( KSTA *BS + KSTB *BST + KSTC *BSTT + KSTD*BSTTT
C    &                + KSTF *BSFF  + KSTG*BSTFF )  / KSE
C --> KSTT= ( KSTTA *BS    + KSTTB*BST  + KSTTC *BSTT + KSTTD*BSTTT
C    &      + KSTTF*BSTTTT + KSTTG*BSFF + KSTTH*BSTFF + KSTTI*BSTTFF )
C    &                                        /  KSE
C --> KSFF = ( KSFFA *BSFF + KSFFB *BSTFF
C    &                + KSFFC *BSTTFF + KSFFD*BSFFFF )  / KSE
      KSFFA = KSA
      KSFFB = KSB
      KSFFC = KSC
      KSFFD = KSD
C     WRITE(6,1789) THETA,  KSTA,KSTB,KSTC,KSTD,KSTF,KSTG
C1789 FORMAT(8(1X,1PE9.2))
C
C
C -----> K0 = KAPPA (NULL)
C -----> K0T = ABLEITUNG VON K0 NACH THETA
C -----> K0TT = 2-TE ABLEITUNG VON K0 NACH THETA
C
      K0C=  DSQRT(B0T2+B02)*SINTH *B0 *(B0T2+B02)
      H0A1 =        ( -  B0TT*B02 + C3P* B0T2*B0 + C2P*B03)
      K0A=  SINTH * H0A1
      H0B1 =        ( -  B0T3 -     B0T*B02 )
      K0B=  COSTH * H0B1
C --> K0 = (K0A + K0B ) / K0C
      K0 = (K0A + K0B ) / K0C
C --> K0 = (K0A + K0B ) / K0C
      H0A1T=      (   -  B0TTT *B02
     &                -  B0TT  *B0  * C2P *  B0T
     &                + C3P* B0T    *B0 * C2P * B0TT
     &                + C3P* B0T2 *B0T
     &                + C2P*B02 * C3P * B0T )
      K0AT =  COSTH * H0A1 + SINTH * H0A1T
      H0B1T=        ( -  B0T2 * C3P * B0TT
     &                -   B0TT  *B02
     &                -   B0T   *B0    * C2P* B0T )
      K0BT =  -SINTH * H0B1 + COSTH * H0B1T
      K0CT =  DSQRT(B0T2+B02) * (
     &                 COSTH *B0  *(B0T2+B02)
     &       +         SINTH *B0T *(B0T2+B02)
     &       +         SINTH *B0  *C3P * (B0T*B0TT + B0*B0T) )
C ----> K0T
      K0T = ( K0AT - K0A * K0CT / K0C ) / K0C
     &    + ( K0BT - K0B * K0CT / K0C ) / K0C
C ----> K0T
C
      K0ATT = SINTH *  (   -  B0TTTT*B02
     &                -  B0TTT *B0   * C2P * B0T
     &                -  B0TTT *B0  * C2P *  B0T
     &                -  B0TT  *B0T * C2P *  B0T
     &                -  B0TT  *B0  * C2P *  B0TT
     &                + C3P* B0TT   *B0 * C2P * B0TT
     &                + C3P* B0T    *B0T* C2P * B0TT
     &                + C3P* B0T    *B0 * C2P * B0TTT
     &                + C3P* B0T2 * C3P * B0TT
     &                + C2P*B0    * C3P * B0T * C2P * B0T
     &                + C2P*B02 * C3P * B0TT     )
      K0ATT= K0ATT -  SINTH * H0A1 + C2P * COSTH * H0A1T
      K0BTT= COSTH* ( -  B0T    * C3P * B0TT * C2P * B0TT
     &                -  B0T2 * C3P * B0TTT
     &                -   B0TTT *B02
     &                -   B0TT  *B0   * C2P * B0T
     &                -   B0TT  *B0    * C2P* B0T
     &                -   B0T   *B0T   * C2P* B0T
     &                -   B0T   *B0    * C2P* B0TT   )
      K0BTT = K0BTT  -COSTH * H0B1 - C2P *SINTH * H0B1T
      K0CTT=  DSQRT(B0T2+B02) * (
     &       -         SINTH *B0  *(B0T2+B02)
     &       +         COSTH *B0T *(B0T2+B02)
     &       +         COSTH *B0T *(B0T2+B02)
     &       +         SINTH *B0TT*(B0T2+B02)
     &       +         COSTH *B0  *C3P * (B0T*B0TT + B0*B0T)
     &       +         SINTH *B0T *C3P * (B0T*B0TT + B0*B0T)
     &       +         SINTH *B0  *C3P * (B0TT*B0TT + B0T*B0T )
     &       +         SINTH *B0  *C3P * (B0T *B0TTT+ B0 *B0TT) )
      K0CTT = K0CTT +   DSQRT(B0T2+B02) * (
     &                 COSTH *B0  *(B0T2+B02) *C3P
     &       +         SINTH *B0T *(B0T2+B02) * C3P
     &       +         SINTH *B0  *C3P * (B0T*B0TT + B0*B0T) )
     &               *  (B0T*B0TT + B0*B0T) / (B0T2 + B02)
C
C ----> K0TT
      K0TT =(K0ATT*K0C  - C2P*K0CT*(K0AT -K0CT*K0A/K0C) -K0A*K0CTT)
     &     +(K0BTT*K0C  - C2P*K0CT*(K0BT -K0CT*K0B/K0C) -K0B*K0CTT)
      K0TT = K0TT / ( K0C * K0C)
C ----> K0TT
C
C
      G0 = B02 * SINTH2 * (B02 + B0T2)
      FLB0T = B02 * SINTH3 * (
     &            SINTH * ( B0 * B0T3 - B02 * B0T * B0TT)
     &           +COSTH * ( B02 * B0T2 + B04                ) )
      FLB0T = FLB0T / G0 / G0
      FLB0TT= B02 * SINTH2 / G0
      FLB0FF= (B02 + B0T2)      / G0
C
C----> LBS ( K0)
C  LBS(K0) = FLBSKA * BS + FLBSKB * BST + FLBSKC * BSTT + FLBSKD * BSFF
C----> LBS ( K0)
      FLB0K0= FLB0T * K0T + FLB0TT * K0TT
      GSVBS  = C2P*G0 / B0 + C2P * B03 * SINTH2
      GSVBST = C2P * B02 * B0T    * SINTH2
      FLBSKA = - C2P* FLB0K0 * GSVBS / G0
     &      + (   SINTH2 *(B02 * GSVBS + G0*C2P*B0) * K0TT
     &          + SINTH4 *(C3P*B02*B0T3 - C4P*B03*B0T*B0TT)*K0T
     &          + SINTH3 * COSTH*(C4P*B03*B0T2 + C6P*B05)          *K0T
     &         )  / G0 / G0
      FLBSKB = - C2P* FLB0K0 * GSVBST/ G0
     &      + (   SINTH2 *(B02 * GSVBST               ) * K0TT
     &          + SINTH4 *(C3P*B03*B0T2 -           B04      *B0TT)*K0T
     &          + SINTH3 * COSTH*( C2P*B04 * B0T                )  *K0T
     &         )  / G0 / G0
      FLBSKC = ( - B04 * B0T ) * K0T
     &       * SINTH4 / G0 / G0
      FLBSKD = (- B02 * B0T3 - B04 * B0T ) * K0T
     &       * SINTH2 / G0 / G0
C
          FACTT=  DSQRT( B02 + B0T2      ) / B0
          VSK0 = FNV1(VA1,VA2,VA3,K00,K0)
          V0K0 = FNV0(VA1,VA2,VA3,K00,K0)
          B0PT = FACTT*(V0K0+GAMMAK*FLB0K0)
          SVNQUT=SVNQUT + B0PT * B0PT
C
C
C
C ----> STORE THE P_{L,M} FOR FIXED THETA
C
      DO 5556 M=0,MMAX
C
      DO 5110 LMC=1,LMTOT
        L=LMC-1+IABS(M)
        DO 5109 J=0,4
CC        CALL DNLPDT(L,M,J,COSTH,H3,ICON)
          H33 = RPLMEM(J,M,LMC,NPC)
          RPLM(J,LMC) = H33
CC        RPLM(J,LMC) = H3
CC        IF (NPC .LT. 2) WRITE(6,*) H3,H33,J,M,LMC
 5109   CONTINUE
 5110 CONTINUE
C     IF (M.EQ. 0)  WRITE(6,*) (RPLM(0,K),K=1,4)
C
C ----> START OF THE EIGENVALUE-MATRIX-LOOP
C ----> START OF THE EIGENVALUE-MATRIX-LOOP
      DO 5210 LMC =1,LMTOT
C       L = LMC - 1 + IABS(M)
        MS=M
      DO 5209 LMCS=1,LMTOT
C       LS = LMCS - 1 + IABS(MS)
C
C --> KS = ( KSA *BS + KSB *BST + KSC *BSTT + KSD*BSFF )  / KSE
C --> KST= ( KSTA *BS + KSTB *BST + KSTC *BSTT + KSTD*BSTTT
C    &                + KSTF *BSFF  + KSTG*BSTFF )  / KSE
C --> KSTT= ( KSTTA *BS    + KSTTB*BST  + KSTTC *BSTT + KSTTD*BSTTT
C    &      + KSTTF*BSTTTT + KSTTG*BSFF + KSTTH*BSTFF + KSTTI*BSTTFF )
C    &                                        /  KSE
C --> KSFF = ( KSFFA *BSFF + KSFFB *BSTFF
C    &                + KSFFC *BSTTFF + KSFFD*BSFFFF )  / KSE
C
          RMS  = MS
          H3 = ZERO
          H3            = H3             + FACTT * VSK0 *
     &     ( KSA *RPLM(0,LMCS)
     &     + KSB *RPLM(1,LMCS)
     &     + KSC *RPLM(2,LMCS)
     &     - KSD *RPLM(0,LMCS) * RMS * RMS )  / KSE   * RPLM(0,LMC)
C
          H3   = H3   + FACTT * GAMMAK * FLB0T *
     &     ( KSTA *RPLM(0,LMCS)
     &     + KSTB *RPLM(1,LMCS)
     &     + KSTC *RPLM(2,LMCS)
     &     + KSTD *RPLM(3,LMCS)
     &     - KSTF *RPLM(0,LMCS) * RMS * RMS
     &     - KSTG *RPLM(1,LMCS) * RMS * RMS ) / KSE * RPLM(0,LMC)
          H3   = H3   + FACTT * GAMMAK * FLB0TT*
     &     ( KSTTA *RPLM(0,LMCS)
     &     + KSTTB *RPLM(1,LMCS)
     &     + KSTTC *RPLM(2,LMCS)
     &     + KSTTD *RPLM(3,LMCS)
     &     + KSTTF *RPLM(4,LMCS)
     &     - KSTTG *RPLM(0,LMCS) * RMS * RMS
     &     - KSTTH *RPLM(1,LMCS) * RMS * RMS
     &     - KSTTI *RPLM(2,LMCS) * RMS * RMS ) / KSE * RPLM(0,LMC)
          H3   = H3   + FACTT * GAMMAK * FLB0FF*
     &     (-KSFFA *RPLM(0,LMCS) *RMS *RMS
     &     - KSFFB *RPLM(1,LMCS) *RMS *RMS
     &     - KSFFC *RPLM(2,LMCS) *RMS *RMS
     &     + KSFFD *RPLM(0,LMCS)*RMS*RMS*RMS*RMS ) / KSE* RPLM(0,LMC)
C
          H3   = H3   + FACTT * GAMMAK *
     &     ( FLBSKA *RPLM(0,LMCS)
     &     + FLBSKB *RPLM(1,LMCS)
     &     + FLBSKC *RPLM(2,LMCS)
     &     - FLBSKD *RPLM(0,LMCS) * RMS * RMS )   * RPLM(0,LMC)
C
          H3   = H3   + B0PT *
     &     (  (GSVBS   /C2P /G0  - C2P / B0) * RPLM(0,LMCS)
     &       +(GSVBST  /C2P /G0  ) * RPLM(1,LMCS)   ) * RPLM(0,LMC)
C
C   ----->    TEST
C             H3 = RPLM(0,LMCS) * RPLM(0,LMC)
C   ----->    TEST
          EVM(M,LMC,LMCS) = EVM(M,LMC,LMCS) + H3 * SINTH * DTHEV
C
 5209 CONTINUE
 5210 CONTINUE
C
 5556 CONTINUE
C
 5999 CONTINUE
C
C
      DO 7556 M=0,MMAX
C
C     DO 6100  LMC =1,LMTOT
C       WRITE (6,6110) LMC , LMTOT
C       WRITE (6,6111) (EVM(M,LMC,I) , I=1,LMTOT )
C6100 CONTINUE
C6110 FORMAT (1X,'ZEILE =',I4,' VON ',I4, ' ZEILEN ')
C6111 FORMAT (6(1X,1PE12.5))
C
C       CALL DEIG1(A,K,N,MODE,ER,EI,EV,VX,ICON)
C
C       K        : K MUSS DIE DEFINIERTE DIMENSION DER MATRIZEN SEIN
C                : (ALSO STETS N<=K)
C       N        : GROESSE DER MATRIX , MIT DER GERECHNET WIRD
C       MODE = 1 : AUSBALANZIEREN DER MATRIX , D.H. ABSOLUTE SUMME
C                  UEBER I-TE ZEILE = ABSL. SUMME UEBER I-TE SPALTE
C       MODE<>1  : KEIN AUSBALANZIEREN
C       A(K,K)   : A = MATRIX, BZGL. DER DIE EIGENWERTE BERECHNET
C                  WERDEN. ACHTUNG: MATRIX A WIRD VERAENDERT !!!!
C       ER(K)    : REALTEILE DER EIGENWERTE
C       EI(K)    : IMAGINAERTEILE DER EIGENWERTE
C       EV(K,K)  : EV(.,I) IST EIGENVEKTOR ZUM I-TEN EIGENWERT
C       VX(K)    : ARBEITSVEKTOR FUER EIG1(.....)
C       ICON     : KONTROLLPARAMETER FUER EIG1(.....)
C                : ICON = 0     NO ERROR
C                : ICON = 10000 N=1
C                : ICON = 20000 KEINE REDUKT. AUF DREIECKSMATR. MOEGL.
C                : ICON = 30000 N<1 ODER K<N
C
      DO 7000 LMC =1,LMTOT
      DO 7000 LMCS=1,LMTOT
        A(LMC,LMCS)=EVM(M,LMC,LMCS)
 7000 CONTINUE
C
      MODE = 1
      CALL DEIG1(A, 48,LMTOT,MODE,ER,EI,EV,VX,ICON)
      CALL SORTIE(ER,ERSORT,LMTOT)
      DO 7152 K=1,LMTOT
        ERMEM(K+M*(MMAX+1))=ERSORT(K)
C       WRITE(6,*) ER(K),ERSORT(K)
 7152 CONTINUE
C
      IF (NW .EQ. 1) THEN
        DO 7200 LCM = 1 , MIN(MMAX+1 -IABS(M),LMTOT)
C       DO 7200 LCM = 1 , MIN(MMAX+2 -IABS(M),LMTOT)
          WRITE(23,7211) BK(2), ERSORT(LCM),M,SVNQUT
C         WRITE(6 ,7211) BK(2), ERSORT(LCM),M,SVNQUT
 7200   CONTINUE
      ENDIF
 7211 FORMAT(1X,1PE12.5,1X,1PE12.5,1X,I8,1X,1PE12.5)
C
 7556 CONTINUE
C
C
C
C
      CALL CLOCK(CPUSEC)
      WRITE(6,7840) 'MAX. EV VON M=0:',ERMEM(1),'CPU-TIME(SEC):',CPUSEC
      WRITE(6,*) ' SVNQUT = ',SVNQUT,' SVNQU =',SVNQUN
 7840 FORMAT(1X,A20,1PE12.5,A16,I8)
CC    WRITE(6,7846) ' MAX. EIGENWERTE          CPU-TIME(SEC): ', CPUSEC
CC    DO 7842 M=0,MIN(4,MMAX)
CC        WRITE(6,7847) M,(ERMEM(K+M*(MMAX+1)),K=1, MIN(5,LMTOT) )
 7842 CONTINUE
 7846 FORMAT(1X,A40,I8)
 7847 FORMAT(1X,I4,5(1X,1PE12.5))

C
C
      IF (CPUSEC .GT. 100*60) STOP
CC    IF (CPUSEC .GT.   300 ) STOP
CC    IF (CPUSEC .GT.  10*60) STOP
C
 9000 CONTINUE
      IF(NW .EQ. 1) CLOSE(22,STATUS='KEEP')
      IF(NW .EQ. 1) CLOSE(23,STATUS='KEEP')
      IF(NW .EQ. 1) CLOSE(24,STATUS='KEEP')
      STOP
      END
C >>>> SUBROUTINES
      SUBROUTINE  DNLP(L,M,X,RET,ICON)
C  ---> THIS SUBROUTINE COMPUTE THE VALUES NORMALIZED
C  ---> LEGENDRE-POLYNOMES  P_{L,M} (X)     WITH
C  ---> -1.D0<=X<=1.D0 AND L=0,1,2,3,4,.... AND M=-L,-L+1,.....,L-1,L
      REAL*8  H1,H2,H3,HI, X,RET,WX, DKOLM, DKOLMM ,HM,X99
      INTEGER L,M,I,ICON,IAM
      RET=0.0D0
      IAM=IABS(M)
      ICON=0
      IF (L .LT. 0 .OR. IAM .GT. L) ICON=20000
      IF (X .GT. 1.0D0 .OR. X .LT. -1.0D0) ICON=30000
      IF (ICON .GT. 0) GOTO 99
      H1=DSQRT(0.5D0)
C  ------>   H1 IS THE VALUE OF P_{ 0 , 0 } (X)
      IF (L.EQ.0) THEN
        RET=H1
        GOTO 99
      ENDIF
      IF (M.NE.0) THEN
        H2= 1.D0 - X*X
        WX=0.D0
        IF (H2 .GT. 0.D0) WX=DSQRT(H2)
        DO 109 I=1,IAM
          HI=I
          H2= -WX*DSQRT( (HI+HI+1.D0) / (HI+HI) ) *H1
          H1=H2
  109   CONTINUE
        IF (M .LT. 0) H1=DABS(H1)
      ENDIF
C  ------>   H1 IS THE VALUE OF P_{ |M| , M } (X)
      IF (L .EQ. IAM) THEN
         RET=H1
         GOTO 99
      ENDIF
      H2=H1
      H1=0.D0
      HM=M
      DO 100 I=IAM+1,L
        HI=I
        X99= (HI+HI+1.D0)*(HI+HI-1.D0) / ( HI*HI - HM*HM)
        DKOLM=0.D0
        IF (X99.GT.0.D0) DKOLM=DSQRT(X99)
        X99= ((HI-1.D0)*(HI-1.D0) -HM*HM)/ ( HI*HI - HM*HM)
        X99= X99* (HI+HI+1.D0) / (HI+HI-3.D0)
        DKOLMM=0.D0
        IF (X99.GT.0.D0) DKOLMM=DSQRT(X99)
        H3= X* DKOLM *H2 - DKOLMM *H1
        H1=H2
        H2=H3
  100 CONTINUE
C  ------>   H3 IS THE VALUE OF P_{ L , M } (X)
      RET=H3
   99 CONTINUE
      RETURN
      END
      SUBROUTINE  DNLPDT(L,M,K,X,RET,ICON)
C  ---> THIS SUBROUTINE COMPUTE THE K-TH DERIVATION (K <=1000) TO THETA
C  ---> (X=COS(THETA)) OF THE LEGENDRE-POLYNOMES  P_{L,M} (X)   WITH
C  ---> -1.D0<=X<=1.D0 AND L=0,1,2,3,4,.... AND M=-L,-L+1,.....,L-1,L
      REAL*8  H3,HL, X,RET,HI, DKOMP, DKOMM,X99
      REAL*8  H1(-1000:1000)
      INTEGER L,M,K,I,J,ICON,IAM
      RET=0.0D0
      IAM=IABS(M)
      ICON=0
      IF (L .LT. 0 .OR. IAM .GT. L ) ICON=20000
      IF (K.LT.0 .OR. K.GT.1000) ICON=20000
      IF (X .GT. 1.0D0 .OR. X .LT. -1.0D0) ICON=30000
      IF (ICON .GT. 0) GOTO 99
      IF (K .EQ. 0) THEN
         CALL DNLP(L,M,X,H3,ICON)
         RET=H3
         GOTO 99
      ENDIF
      IF (L.EQ.0) THEN
         RET=0.D0
         GOTO 99
      ENDIF
      DO 100 I=M-K , M+K  ,2
         IF (I .LT. -L .OR. I.GT. L) GOTO 100
         CALL DNLP(L,I,X,H3,ICON)
         H1(I-M)=H3
  100 CONTINUE
      HL=L
      DO 102 J=1,K
      DO 102 I=M-K+J  ,  M+K-J  , 2
         IF (I .LT. -L .OR. I.GT. L) GOTO 102
         HI=I
         X99= (HL - HI)*(HL+HI+1.D0)
         DKOMP=0.D0
         IF (X99.GT.0.D0) DKOMP=DSQRT(X99)*.5D0
         X99= (HL + HI)*(HL-HI+1.D0)
         DKOMM=0.D0
         IF (X99.GT.0.D0) DKOMM=DSQRT(X99)*.5D0
         H1(I-M)= DKOMP*H1(I-M+1) - DKOMM*H1(I-M-1)
  102 CONTINUE
      RET=H1(0)
   99 CONTINUE
      RETURN
      END
        SUBROUTINE SORTIE(IN,OUT,N)
C       >>>> SORTIEREN DER GROESSE NACH.
C       >>>> INPUT : IN , N  (WIRD NICHT VERAENDERT)
C       >>>> OUTPUT: OUT
        DOUBLE PRECISION IN(  48), OUT( 48) ,H1
        INTEGER N,I,J,K
        IF (N .LE. 0) RETURN
        IF (N .EQ. 1) OUT(1)=IN(1)
        IF (N .EQ. 1) RETURN
C
C      **** UMSPEICHERN VON IN NACH OUT
        DO 1000 I=1,N
           OUT(I)=IN(I)
 1000   CONTINUE
C
C      **** SORTIEREN DER GROESSE NACH
        DO 3000 J=1,N-1
           H1=OUT(J)
           K=J
           DO 2000 I=J+1,N
              IF (OUT(I) .GT. H1) THEN
                  H1=OUT(I)
                  K =I
              ENDIF
 2000      CONTINUE
           H1=OUT(J)
           OUT(J)=OUT(K)
           OUT(K)=H1
 3000   CONTINUE
        RETURN
C       END
C/LKED.SYSLIB DD DSN=SYS1.FORTLIB,DISP=SHR
C/            DD DSN=SYS1.SSL2,DISP=SHR
C/
      END
