C
C  ******************************************************************
C  ****    BIFURKATONISANALYSE (BIS 3. ORDNUNG) VON              ****
C  ****    VN = V(K) + GAMMA*(LAP-BELTRAMIE)(K)                  ****
C  ****       MIT K = SUMME DER BEIDEN HAUPTKRUEMMUNGEN          ****
C  ****                                                          ****
C  ****       R. LIERMANN              DATE: 10.05.1990          ****
C  ******************************************************************
C
      REAL*8 B0,K00,VA1,GAMM2,GAMMA,A,K0,  VS,VSS,VSSS
      REAL*8 F1,F2,F3,F4    ,PI, W2O5
      REAL*8 VARIA
      CHARACTER FNW*30
C
C
CC    V(K) = (K - K00) + VA1*(K - K00)**3
CC             VS = 1   //  VSS = 0  // VSSS = 6 * VA1
CC             K0 = K00 //  B0 = 2/K0  // GAMM2 = VS *B0*B0 / (2*(2+1))
CCC   V(K) = (K - K00) + VA1*(K - K00)**2
CCC            VS = 1   //  VSS = 2*VA1  // VSSS = 0
CCC            K0 = K00 //  B0 = 2/K0  // GAMM2 = VS *B0*B0 / (2*(2+1))
C     V(K) = (K - K00)
C              VS = 1   //  VSS = 0   // VSSS = 0
C              K0 = K00 //  B0 = 2/K0  // GAMM2 = VS *B0*B0 / (2*(2+1))
C
C
      FNW   =   'D14K.OD3BIF.DATA'
      K00   =   2.D0
      VA1   =  -0.D0
      K0    =   K00
      B0    =   2.D0/K0
      VS    =   1.D0
CC    VSS   =   0.D0
CC    VSSS  =   VA1*6.D0
      VSS   =   2.D0*VA1
      VSSS  =   0.D0
      GAMM2 =   VS * B0*B0 / ( 2.D0*(2.D0 + 1.D0) )
      PI    =   4.D0*DATAN(1.D0)
      W2O5 =   DSQRT(2.D0/5.D0)
C
      OPEN(22,FILE=FNW)
      REWIND 22
C
      F1    = -24.D0/(B0*B0*B0*B0)
      F2    =  DSQRT(10.D0)*( 8.D0*VS/B0 + 16.D0*VSS/(2.D0*B0*B0) )
     &                          / (B0*B0*7.D0)
      F3    =  DSQRT(10.D0)*108.D0 / (7.D0*B0*B0*B0*B0*B0)
      F4    =  ( (2.D0**3 * 43.D0 *VS) / (3.D0 * 7.D0**2 *B0)
     &         + (2.D0**6 * 5.D0**2 *VSS) / (3.D0 * 7.D0**2 *B0*B0*2.D0)
     &         - (2.D0**7 * 47.D0  ) / (3.D0 * 7.D0**2 )
     &                     * ( VSS / (2.D0*B0*B0) )**2 * (B0/VS)
     &         +  ( 2.D0**6 * VSSS) / (2.D0*3.D0*B0*B0*B0)
     &         ) *15.D0/( 14.D0 * B0*B0*B0)
C
C
CC    WRITE( 6, 112) ' GAMMA ', 'A VON P_(2,0) (NORMIERT) '
      WRITE(22, 112) ' GAMMA ', 'A VON P_(2,0) (NORMIERT) '
C
C
      DO 100 VARIA=-1.D0 , 1.D0 , 0.0010D0
CC    DO 100 VARIA= 0.D0 , 1.D0 , 0.0010D0
        A = VARIA*W2O5
        GAMMA= ( F2*A + F4*A*A) / (-F1 - F3*A)  + GAMM2
CC      WRITE( 6, 111) GAMMA,A
        WRITE(22, 111) GAMMA,A
  100 CONTINUE
C
  111 FORMAT(6(1X,1PE12.5))
  112 FORMAT(1X,A12,1X,A25)
C
      CLOSE(22,STATUS='KEEP')
C
      STOP
      END
