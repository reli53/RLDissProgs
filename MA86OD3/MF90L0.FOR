C
C -------------------------------------------------------------------
C ----     NAEHRSTOFFMODELL:                                    -----
C ----     D(ROHN)/DT = D1 *( LAPLACE(ROHN) - ROHN)             -----
C ----     RHON (AM RANDE) = TAU                                -----
C ----     LAPLACE(GPOT) = ROHN - 1                             -----
C ----     GPOT (AM RANDE) = - GAMMA * KAPPA                    -----
C ----     KAPPA = SUMME DER BEIDEN HAUPTKRUEMMUNGEN            -----
C ----     SEI S(T,T,THETA,FI) = R - B(T,THETA,FI)              -----
C ----     D(B(T,THETA,FI))/DT =GRAD(GPOT)*GRAD(R-B(T,THETA,FI))-----
C ----                                                          -----
C ----     HIER: BEWEGUNGEN FUER L=0 (3 DIMENSIONAL)            -----
C ----           ( LANDAU-TRANSFORMATION AUF EINHEITSKUGEL )    -----
C ----           ( IMPLIZITES LOESEN DER RHON-GLEICHUNG         -----
C ----             UND EXPLITES LOESEN DER BEWEGUNGS-           -----
C ----             GLEICHUNG DES RANDES )                       -----
C ----           ( NICHT-AEQUIDISTANTE WAHL DER STUETZPUNKTE    -----
C ----             MOEGLICH (IST AUCH BESSER) )                 -----
C ----                                                          -----
C ----  DATE:  24.06.1990                    REINHARD LIERMANN  -----
C -------------------------------------------------------------------
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DOUBLE PRECISION RHONI( 81),GPOT( 81),MATA( 81, 81),INHB( 81)
      DOUBLE PRECISION IS,VW( 81),IP( 81)
      DOUBLE PRECISION R( 81),DELR( 80)
      DOUBLE PRECISION AR1L( 80),AR1C( 80),AR1R( 80)
      DOUBLE PRECISION AR2L( 80),AR2C( 80),AR2R( 80)
      CHARACTER*1 PR
      CHARACTER*30 DSW
C
      B0 = 4.73330D+00
      D1 =  0.290D+00
CC    D1 =  1.00D+01
      GAMMA = 2.35000D0
      TAU   =  2.0D0
      PI = DATAN(1.D0) * 4.D0
      NEND = 31
      DELT = 0.0500D0
      TEND = 1.0D+04
      TPRT = 0.25D0
      T    = -DELT
      T99  = -DELT
      B    = B0 +0.010D0
CC    B    = B0 +0.000D0
      DSW  = 'D14K.TES6.DATA'
      NPRT = 9
C ----> NPRT = 1 : PRINT BEI MINIMA UND MAXIMA
C ----> NPRT = 2 : PRINT JEDEN ZEITSCHRITT
C ----> NPRT = 9 : PRINT ON DATASET DSW
C
C    STATIONAERE LOSUNGEN  (FUER R(I) = DSIN( (I-1)/(NEND-1) * PI/2 ) )
C     TAU     GAMMA   NEND      B0(NUMERISCH)      B0(ANALYTISCH)
C     2.0     5.0      31         4.738               4.7333
C     2.0     5.0      21         4.7435              4.7333
C     2.0     5.0      41         4.7360              4.7333
C     2.0     5.0      61         4.7345              4.7333
C
C    STATIONAERE LOSUNGEN  (FUER R(I) = (I-1)/(NEND-1)  )
C     TAU     GAMMA   NEND      B0(NUMERISCH)      B0(ANALYTISCH)
C     2.0     5.0      31         4.69043             4.7333
C     2.0     5.0      51         4.71717             4.7333
C
C    EIGENWERTE DER ANALYTISCHEN STATIONAREN KUGEL
C    (EIN EIGENWERT FUER L=1 IST STETS IDENTISCH 0)
C     TAU     GAMMA   D1     B0(ANALYTISCH)
C     2.0    10.0     0.305     4.7333
C     EIGENWERTE: L=0: 0.00049 +/- I * 0.3743      PERIODE: 17....
C                 ALLE ANDEREN EIGENWERTE: RE(LAMBDA) < 0
C
C    EIGENWERTE DER ANALYTISCHEN STATIONAREN KUGEL
C    (EIN EIGENWERT FUER L=1 IST STETS IDENTISCH 0)
C     TAU     GAMMA   D1     B0(ANALYTISCH)
C     2.0    10.0     0.29      4.7333
C     EIGENWERTE: L=0: 0.0142 +/- I * 0.3654      PERIODE: 17.2
C                 ALLE ANDEREN EIGENWERTE: RE(LAMBDA) < 0
C
C     TAU     GAMMA   D1     B0(ANALYTISCH)
C     2.0    10.0     0.25      4.7333
C     EIGENWERTE: L=0: 0.0542 +/- I * 0.339       PERIODE: 18.3
C                 L=1: 0.0352
C                 ALLE ANDEREN EIGENWERTE: RE(LAMBDA) < 0
C
C     TAU     GAMMA   D1     B0(ANALYTISCH)
C     2.0    10.0    >0.306     4.7333
C     EIGENWERTE: ALLE ANDEREN EIGENWERTE: RE(LAMBDA) < 0
C
C    GEFUNDENE OSZILLATIONEN
C     TAU    D1    GAMMA NEND   B(MIN)   B(MAX)   B0(NUM)  PERIODE
C     2.0   0.25   5.0    21    3.175    8.217    4.7435   17.0
C     2.0   0.29   5.0    21    3.743    6.062    4.7435   17.29
C     2.0   0.30   5.0    21    4.06     5.54     4.7435    ?
C     2.0   0.305  5.0    21    4.36     5.16     4.7435   17....
C     2.0   0.307  5.0    21   >4.52    <4.98     4.7435   17.25
C     2.0   0.29   5.0    51    3.7562   5.9751   4.71717  17.35
C     2.0   0.25   5.0    31    3.086    8.102    4.69043  19.3
C
C    STABILE STATIONAERE LOESUNG
C     TAU    D1    GAMMA NEND     B0(NUM)  STOERUNG
C     2.0   0.31   5.0    31      4.69043   STARK
C
C >>>>> DISKRETISIERUNG VON (0,1)
      R(1)    = 0.D0
      R(NEND) = 1.D0
      H2 = NEND -1
      DO 1400 I=2,NEND-1
         H1=I -1
         HR=H1/H2
CC       R(I) = 0.80D0 *DSQRT( 1.D0 - (1.D0-HR)**2 )  + 0.20D0 * HR
         R(I) = 1.00D0 *DSIN(PI*.5D0*HR )             + 0.00D0 * HR
CC       R(I) = HR
 1400 CONTINUE
C
      WRITE(6, 57) 'R(',1,')=',R(1)
      DO 1405 I=1,NEND-1
        DELR(I)=R(I+1) - R(I)
        WRITE(6, 57) 'R(',I+1,')=',R(I+1),'DELR(',I,')=',DELR(I)
 1405 CONTINUE
   57 FORMAT(1X,2(4X,A6,I4,A2,1X,F10.5))
      AR2R(1) = 2.D0 / (DELR(1)*DELR(1))
      AR2C(1) = -AR2R(1)
      DO 1410 I=2,NEND-1
         H1 = DELR(I-1)
         H2 = DELR(I)
         AR2L(I) = 2.D0/ (H1*(H1+H2))
         AR2R(I) = 2.D0/ (H2*(H1+H2))
         AR2C(I) = - AR2L(I) - AR2R(I)
         AR1L(I) = - H2/ (H1*(H1+H2))
         AR1R(I) = + H1/ (H2*(H1+H2))
         AR1C(I) = - AR1L(I) - AR1R(I)
 1410 CONTINUE
      H1 = DELR(NEND-2)
      H2 = DELR(NEND-1)
      AR1BLL =  + H2 / ( H1*(H1+H2) )
      AR1BL  =  - (H1+H2) / ( H1*H2 )
      AR1BC  =  - AR1BL -AR1BLL
C
C >>>> STARTVERTEILUNG VON RHON
      CALL SBIN(0,B,XX)
      DO 1000 I=1,NEND
         H1 = R(I)*B
         CALL SBIN(0,H1,YY)
CC       WRITE (6,*) YY,XX
         RHONI(I) = TAU*YY/XX
 1000 CONTINUE
      CALL SBIN(1,B,YY)
CC    BPKT = TAU*YY/XX - B/3.D0
      BPKT = 0.D0
C
      IF (NPRT .EQ. 9) THEN
         OPEN (23 ,FILE = DSW)
         REWIND 23
         WRITE(23,46) 'TAU=',TAU,'B0=',B0,'D1=',D1,'GAMMA=',GAMMA
         WRITE(23,47) 'NEND=',NEND,'DELT=',DELT
      ENDIF
   46 FORMAT(4(1X,A6,1PE12.5))
   47 FORMAT(1X,A6,I6,6X,3(1X,A6,1PE12.5))
C
C   >>>>>> HIER FAENGT DIE ZEITSCHLEIFE AN
C
 9999 T = T + DELT
      T99 = T99 + DELT
      DO 1990 I = 1,NEND
      DO 1990 J = 1,NEND
        MATA(I,J) = 0.0D0
 1990 CONTINUE
C
      DO 2000 I = 1,NEND-1
        INHB(I) = RHONI(I)
        MATA(I,I) = MATA(I,I) + 1.D0
 2000 CONTINUE
      INHB(NEND)=TAU
      MATA(NEND,NEND) = 1.D0
      H2 = - DELT* ( D1 * 3.D0* ( AR2C(1) )/B/B - D1 )
      MATA(1,1) = MATA(1,1) + H2
      DO 2005 I = 2,NEND-1
        H2 = - DELT* ( D1  * (   AR2C(I)
     &                   + 2.D0/R(I)*AR1C(I)  )/B/B -D1
     &               + R(I)/B * BPKT *  AR1C(I)           )
        MATA(I,I)= MATA(I,I) + H2
 2005 CONTINUE
      H2 = - DELT* ( D1 * 3.D0* ( AR2R(1)   )/B/B      )
      MATA(1,2) = MATA(1,2) + H2
      DO 2010 I = 2,NEND-1
        H2 = - DELT* ( D1 * ( AR2R(I)
     &                 + 2.D0/R(I) * AR1R(I)        )/B/B
     &               + R(I)/B * BPKT *  AR1R(I)           )
        MATA(I,I+1)= MATA(I,I+1) + H2
 2010 CONTINUE
      DO 2015 I = 2,NEND-1
        H2 = - DELT* ( D1 * ( AR2L(I)
     &                 + 2.D0/R(I) * AR1L(I)        )/B/B
     &               + R(I)/B * BPKT * AR1L(I)           )
        MATA(I,I-1)= MATA(I,I-1) + H2
 2015 CONTINUE
C
CC    DO 2039 I = 1,NEND
CC       WRITE (6,112) ' A(',I,')',(MATA(I,J),J=1,NEND)
C2039 CONTINUE
CC    WRITE (6,112) 'IN(',I+1,')',(INHB(J),J=1,NEND)
CC112 FORMAT(1X,A3,I2,A2,5(1X,1PE12.5))
C
      EPSZ = 1.D-10
      ISW  = 1
      CALL DLAX(MATA, 81,NEND,INHB,EPSZ,ISW,IS,VW,IP,ICON)
      IF (ICON .NE. 0) WRITE (6,*) ' ICON =', ICON
      DO 2200 I=1,NEND
        RHONI(I) = INHB(I)
CC      WRITE (6,   11) I,R(I), RHONI(I)
 2200 CONTINUE
C
      DO 2990 I = 1,NEND
      DO 2990 J = 1,NEND
        MATA(I,J) = 0.0D0
 2990 CONTINUE
C
      DO 3000 I = 1,NEND-1
        INHB(I) = B*B*(RHONI(I) - 1.D0)
 3000 CONTINUE
      INHB(NEND) = - GAMMA * 2.D0/B
      MATA(NEND,NEND) = 1.D0
      H2 =          3.D0*  AR2C(1)
      MATA(1,1) = MATA(1,1) + H2
      DO 3005 I = 2,NEND-1
        H2 =   1.D0*  AR2C(I) + 2.D0 / R(I) * AR1C(I)
        MATA(I,I)= MATA(I,I) + H2
 3005 CONTINUE
      H2 =         3.D0*  AR2R(1)
      MATA(1,2) = MATA(1,2) + H2
      DO 3010 I = 2,NEND-1
        H2 =   1.D0*  AR2R(I) + 2.D0 / R(I) * AR1R(I)
        MATA(I,I+1)= MATA(I,I+1) + H2
 3010 CONTINUE
      DO 3015 I = 2,NEND-1
        H2 =   1.D0*  AR2L(I) + 2.D0 / R(I) * AR1L(I)
        MATA(I,I-1)= MATA(I,I-1) + H2
 3015 CONTINUE
C
CC    DO 3039 I = 1,NEND
CC       WRITE (6,112) ' A(',I,')',(MATA(I,J),J=1,NEND)
C3039 CONTINUE
CC    WRITE (6,112) 'IN(',I,')',(INHB(J),J=1,NEND)
CC112 FORMAT(1X,A3,I2,A2,5(1X,1PE12.5))
C
      EPSZ = 1.D-40
      ISW  = 1
      CALL DLAX(MATA, 81,NEND,INHB,EPSZ,ISW,IS,VW,IP,ICON)
      IF (ICON .NE. 0) WRITE (6,*) ' ICON =', ICON
      DO 3200 I=1,NEND
        GPOT(I) = INHB(I)
CC      WRITE (6,   11) I,R(I), RHONI(I),GPOT(I)
 3200 CONTINUE
C
      PR = 'N'
      IF (NPRT .EQ. 9 .AND. (T99 .GE. TPRT-DELT*.01D0
     &                       .OR. T .LT. DELT*.01D0  )  ) THEN
            WRITE (23, 77) T,B,BPKT,RHONI(1),RHONI(NEND-1),RHONI(NEND)
            T99 = 0.D0
      ENDIF
      IF (NPRT .EQ. 1 .OR. NPRT .EQ. 9) THEN
         IF (T .LT. DELT*.01D0) PR='Y'
         IF ( BPKTA*BPKT .LE. 0.D0) PR='Y'
CC       IF ( (B-B0)*(BALT-B0) .LE. 0.D0) PR='Y'
      ENDIF
      IF (NPRT .EQ. 2) PR='Y'
      IF (PR .EQ. 'Y') WRITE(6, 27) T,B,BPKT
      IF (PR .EQ. 'Y' .AND. NPRT .NE. 2) THEN
        DO 6112 I=1,NEND
CC        WRITE (6,   11) I,R(I), RHONI(I),GPOT(I)+GAMMA*2.D0/B
 6112   CONTINUE
      ENDIF
   11 FORMAT (1X,I4,1X,F10.5,2(1X,1PE12.5))
   27 FORMAT (1X,F10.5,3(1X,1PE12.5))
   77 FORMAT (7(1X,F10.5))
C
      BPKTA = BPKT
      BALT  = B
C  ----> 3 - PUNKTE ABSCHAETZUNG DER 1. ABLEITUNG AM RANDE
      BPKT = 1.D0 /B * ( AR1BC*GPOT(NEND)
     &         + AR1BL*GPOT(NEND-1) + AR1BLL*GPOT(NEND-2)  )
C  ----> 2 - PUNKTE ABSCHAETZUNG DER 1. ABLEITUNG AM RANDE
CCC   BPKT = 1.D0 /B * ( GPOT(NEND) - GPOT(NEND-1) )/ DELR(NEND-1)
      B = B + DELT * BPKT
C
      IF (T .LT. TEND) GOTO 9999
C
C
C
      CLOSE(23 ,STATUS='KEEP')
      STOP
      END
C
C  ------------   SUBROUTINES ---------------
C
C
C
      SUBROUTINE SBJN(L,X,RET)
C   ---- SPHAERISCHE BESSELFUNKTIONEN (1. ART)
      DOUBLE PRECISION X,RET,PI,NY,BJ
      INTEGER*4        L,ICON
      PI = DATAN(1.D0) * 4.D0
      NY = L
      NY = NY + .5D0
      CALL DBJR(X,NY,BJ,ICON)
      IF (X .GT. 0.D0) RET = BJ * DSQRT( PI * .5D0 / X)
      IF (X .EQ. 0.D0) THEN
         IF (L .GT. 0) RET = 0.D0
         IF (L .EQ. 0) RET = 1.D0
      ENDIF
      RETURN
      END
C
      SUBROUTINE SBIN(L,X,RET)
C   ---- MODIFIZIERTE SPHAERISCHE BESSELFUNKTIONEN (1. ART)
      DOUBLE PRECISION X,RET,PI,NY,BI
      INTEGER*4        L,ICON
      PI = DATAN(1.D0) * 4.D0
      NY = L
      NY = NY + .5D0
      CALL DBIR(X,NY,BI,ICON)
      IF (X .GT. 0.D0) RET = BI * DSQRT( PI * .5D0 / X)
      IF (X .EQ. 0.D0) THEN
         IF (L .GT. 0) RET = 0.D0
         IF (L .EQ. 0) RET = 1.D0
      ENDIF
      RETURN
      END
