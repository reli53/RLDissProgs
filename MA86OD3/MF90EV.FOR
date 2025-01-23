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
C ----     HIER: BERECHNUNG DER EIGENWERTE  (3 DIMENSIONAL)     -----
C ----                                                          -----
C ----  DATE:  20.06.1990                    REINHARD LIERMANN  -----
C -------------------------------------------------------------------
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DOUBLE PRECISION ZEROS(0:50,1:50),EVR(51),EVI(51),EV(51,51)
      DOUBLE PRECISION M1,M0
      INTEGER*4 L,LEND,N,NEND,NST,NED,NUMBER(51),NPRT
      CHARACTER*30 DSW
C
      B0 = 4.7333D0
      D1 = 10.00D+00
      GAMMA =   2.35D0
C
      LEND = 10
      NEND = 20
C
CCC   NPRT = 1
      NPRT = 2
      IF (NPRT .EQ. 2) DSW='D14K.TES4.DATA'
      IF (NPRT .EQ. 2) OPEN(23,FILE=DSW)
      IF (NPRT .EQ. 2) REWIND 23
C
      L=-1
 1000 L=L+1
      CALL ZERJL(L,NEND,ZEROS)
      IF (L .LT. LEND) GOTO 1000
C
      WRITE (6,*) ' THE ZEROES OF THE SPHERICAL BESSEL FUNCTIONS OF THE
     &FIRST KIND:'
      DO 2000 L=0,LEND
        NST =1
        NED = MIN(NEND,5)
        WRITE ( 6,  11) 'L=',L,(ZEROS(L,N),N=NST,NED)
        NST = NED + 1
        NED = MIN(NEND,10)
        WRITE ( 6,  12)   (ZEROS(L,N),N=NST,NED)
 2000 CONTINUE
   11 FORMAT(1X,A2,I4,5(1X,1PE12.5))
   12 FORMAT(1X,2X,4X,5(1X,1PE12.5))
C
      IF (NPRT .EQ. 2) THEN
        CALL SBIN(1,B0,M1)
        CALL SBIN(0,B0,M0)
        TAU = B0*M0/M1/3.D0
        WRITE(23 , 36) 'TAU=',TAU,'B0=', B0,'D1=', D1 ,'GAMMA=', GAMMA
        WRITE(23 ,236) 'D1','RE(LAMBDA)','IM(LAMBDA)','MODE'
      ENDIF
C
      DO 3000 VAR   =  0.5010D0 ,  1.00D0, 0.0020D0
CC    DO 3000 VAR   =  0.00D0 ,   10.D0,   1.00D0
CC    DO 3000 VAR   =  0.305D0 ,   0.309D0,  0.001D0
CC      D1 = VAR
        D1 = 1.D0 / VAR
CC      B0 = VAR
CC      D1 = 0.1D0*10.D0**VAR
CC      GAMMA = VAR
C
      CALL SBIN(1,B0,M1)
      CALL SBIN(0,B0,M0)
      TAU = B0*M0/M1/3.D0
      IF (NPRT .EQ. 1) THEN
        WRITE(6,*) ' '
        WRITE( 6 , 36) 'TAU=',TAU,'B0=', B0,'D1=', D1 ,'GAMMA=', GAMMA
      ENDIF
   36 FORMAT(1X,4(1X,A6,1PE12.5))
  236 FORMAT(4(1X,A12))
C
C
        L=-1
 3002   L = L+1
        CALL EIGV(L,NEND,ZEROS,B0,GAMMA,D1,EVR,EVI,EV)
        CALL SORTIE (NEND+1,EVR,NUMBER)
C    ------> EIGENWERTE (DER GROESSE NACH SORTIERT)
        DO 3010 N =1 ,MIN(11,NEND+1)
          IF (NPRT .EQ. 1)
     &    WRITE ( 6 , 17) L,'- MODE',EVR(NUMBER(N))/D1,EVI(NUMBER(N))
          IF (EVR(NUMBER(N))/D1 .GT. -6.D0 .AND. NPRT .EQ. 2)
     &     WRITE (23 ,217) 1.D0/D1,EVR(NUMBER(N))/D1,EVI(NUMBER(N)),L
 3010   CONTINUE
C    ------> EIGENVEKTOREN ZU DEN EIGENWERTEN
        DO 3030 N = 1 , NEND+1
CC        IF ( L.GT.0) GOTO 3030
CCCC      WRITE(6 , 20) (EV(N,NUMBER(I)),I=1,MIN(6,NEND+1))
 3030   CONTINUE
        IF (L. LT. LEND) GOTO 3002
 3000 CONTINUE
   17 FORMAT (1X,6X,I4,1X,A6,4X,2(1X, 1PE12.5))
  217 FORMAT (3(1X,F12.5),1X,I12)
   20 FORMAT (1X,6(1X, 1PE12.5))
      STOP
      END
C
C  ------------   SUBROUTINES ---------------
C
      SUBROUTINE EIGV(L,NEND,ZEROS,B0,GAMMA,D1,EVR,EVI,EV)
C  ------- EIGENWERTE BERECHNEN   --------------
      DOUBLE PRECISION ZEROS(0:50,1:50),EVR(51),EVI(51)
      DOUBLE PRECISION MATA(51,51),HL,ML,ML1,M1,M2,JL1(50),XX,YY
      DOUBLE PRECISION B0,GAMMA,D1
      DOUBLE PRECISION VW(51) , EV(51,51)
      INTEGER          MODE,ICON
      INTEGER*4        L,NEND,I,J
      HL = L
      DO 1000 I = 1 , NEND+1
      DO 1000 J = 1 , NEND+1
        MATA(I,J) = 0.D0
 1000 CONTINUE
      CALL SBIN(L,B0,ML)
      CALL SBIN(L+1,B0,ML1)
      CALL SBIN(1,B0,M1)
      CALL SBIN(2,B0,M2)
      DO 1010 I = 1,NEND
         CALL SBJN(L+1,ZEROS(L,I),JL1(I))
 1010 CONTINUE
C
      MATA(1 , 1)= B0*(M2/M1 - ML1/ML)/3.D0
     &             - GAMMA*HL*(HL-1.D0)*(HL+2.D0) /B0/B0/B0
      DO 1030 I=1,NEND
        MATA(1, I + 1) = B0*JL1(I)/ZEROS(L,I)
 1030 CONTINUE
      DO 1050 I=1,NEND
        XX = ZEROS(L,I)
        MATA(I+1,I+1) = - D1 * (1.D0 + XX*XX/B0/B0)
        YY = (XX * JL1(I) * B0*B0 / (XX*XX + B0*B0)) *B0/3.D0
        YY = YY * 2.D0 / (B0*B0 * JL1(I)*JL1(I) )
        MATA(I+1,  1) = YY * MATA(1 , 1)
        DO 1050 J = 1,NEND
          MATA(I+1,J+1) = MATA(I+1,J+1) + YY*B0*JL1(J)/ZEROS(L,J)
 1050 CONTINUE
C
      MODE = 1
      CALL DEIG1(MATA, 51, NEND+1, MODE, EVR, EVI, EV, VW, ICON)
      RETURN
      END
C
      SUBROUTINE ZERJL(L,NEND,ZEROS)
C  ------- NULLSTELLEN DER SPHAERISCHE BESSELFUNKTIONEN (1.ART) ------
      DOUBLE PRECISION ZEROS(0:50,1:50)
      DOUBLE PRECISION XSTART,XEND,XSTEP,F1,F2,XDEL, XOLD
      INTEGER*4        L,N,NEND
      XSTART = 1.0D-04
      XEND   = 1.0D+05
      XSTEP  = 0.1D+00
      XDEL = XSTEP
      X = XSTART
      N=0
   80 CALL  SBJN(L,X,F1)
      XOLD = X
      N=N+1
   90 X = X + XDEL
      CALL  SBJN(L,X,F2)
      IF ( F1*F2 .LE. 0.D0) XDEL = -XDEL*.5D0
CC    WRITE ( 6, 21) XOLD,X,F1,F2
CC 21 FORMAT(1X,4(1PE12.5))
      F1 = F2
      IF (X .NE. XOLD) THEN
         XOLD = X
         GOTO 90
      ENDIF
      ZEROS(L,N) = X
      X = X + XSTEP*.1D0
      XDEL = XSTEP
      IF (N .LT. NEND) GOTO 80
      RETURN
      END
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
         IF (L. EQ. 0) RET = 1.D0
         IF (L. GT. 0) RET = 0.D0
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
         IF (L. EQ. 0) RET = 1.D0
         IF (L. GT. 0) RET = 0.D0
      ENDIF
      RETURN
      END
C
      SUBROUTINE SORTIE(NEND,EVR,NUMBER)
C   ---- SORTIEREN -------
      DOUBLE PRECISION EVR(51),EV(51),XX
      INTEGER*4        NEND,NUMBER(51),I,J,N
      DO 100 I=1,NEND
        EV(I)=EVR(I)
        NUMBER(I)=I
  100 CONTINUE
      DO 300 I=1,NEND-1
      DO 300 J=I+1,NEND
        IF (EV(I) .LT. EV(J)) THEN
             XX = EV(J)
             EV(J) = EV(I)
             EV(I) = XX
             N = NUMBER(J)
             NUMBER(J) = NUMBER(I)
             NUMBER(I) = N
        ENDIF
  300 CONTINUE
      RETURN
      END
