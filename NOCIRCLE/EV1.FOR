//D14KE    JOB RACF,CLASS=A,                                            00010052
//    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K                             00020035
//E2          EXEC  FORTCLG,REGION.FORT=1024K,REGION.GO=2048K           00030045
//FORT.SYSIN    DD *                                                    00040035
C                                                                       00050000
C       ##################################################              00060000
C       ####          EIGENWERTE BERECHNEN            ####              00070000
C       ####  NAME DES PROGRAMMS:  NOCIRCLE(EV1)      ####              00080049
C       ##################################################              00090000
C                                                                       00100000
C                                                                       00110000
        INTEGER           L,N8,N7,N6,N5                                 00120051
        DOUBLE PRECISION  GAMMA,B,K0MIN                                 00130056
        DOUBLE PRECISION  MC(0:100,0:100),MS(0:100,0:100)               00140050
C                                                                       00150035
        INTEGER           K,N,ICON,MODE,J,I                             00160018
        DOUBLE PRECISION  A(  100,  100)                                00170050
        DOUBLE PRECISION  EV(  100,  100)                               00180050
        DOUBLE PRECISION  EI(  100),ER(  100)                           00190050
        DOUBLE PRECISION  VW(  100)                                     00200050
        CHARACTER         FNR*30 , FN1*60                               00210057
C                                                                       00220038
C       **************************************************              00230000
C       ****      PARAMETER PARAMETER PARAMETER       ****              00240000
C       **************************************************              00250000
C                                                                       00260000
        FNR ='F159.M022.DATA'                                           00270062
        N5=10                                                           00280051
        K=100                                                           00290057
        MODE = 1                                                        00300057
C                                                                       00310057
C       EIGENWERTE DER UNTERMATRIZEN DER GROESSE N5,2*N5,3*N5,...       00320051
C       WERDEN BERECHNET                                                00330051
C       CALL DEIG1(A,K,N,MODE,ER,EI,EV,VW,ICON)                         00340059
C                                                                       00350031
C       K        : K MUSS DIE DEFINIERTE DIMENSION DER MATRIZEN SEIN    00360031
C                : (ALSO STETS N<=K)                                    00370031
C       N        : GROESSE DER MATRIX , MIT DER GERECHNET WIRD          00380058
C       MODE = 1 : AUSBALANZIEREN DER MATRIX , D.H. ABSOLUTE SUMME      00390031
C                  UEBER I-TE ZEILE = ABSL. SUMME UEBER I-TE SPALTE     00400031
C       MODE<>1  : KEIN AUSBALANZIEREN                                  00410032
C       A(K,K)   : A = MATRIX, BZGL. DER DIE EIGENWERTE BERECHNET       00420058
C                  WERDEN. ACHTUNG: MATRIX A WIRD VERAENDERT !!!!       00430038
C       ER(K)    : REALTEILE DER EIGENWERTE                             00440058
C       EI(K)    : IMAGINAERTEILE DER EIGENWERTE                        00450058
C       EV(K,K)  : EV(.,I) IST EIGENVEKTOR ZUM I-TEN EIGENWERT          00460058
C       VW(K)    : ARBEITSVEKTOR FUER EIG1(.....)                       00470058
C       ICON     : KONTROLLPARAMETER FUER EIG1(.....)                   00480032
C                : ICON = 0     NO ERROR                                00490032
C                : ICON = 10000 N=1                                     00500031
C                : ICON = 20000 KEINE REDUKT. AUF DREIECKSMATR. MOEGL.  00510032
C                : ICON = 30000 N<1 ODER K<N                            00520032
C                                                                       00530003
C       ***************************************************             00540041
C       ****   LESEN DER DATEN                         ****             00550041
C       ***************************************************             00560041
C                                                                       00570041
        OPEN (21,FILE=FNR)                                              00580057
        REWIND 21                                                       00590035
   40   FORMAT (1X,7X       ,I6)                                        00600056
   41   FORMAT (1X,7X       ,D23.16)                                    00610060
   49   FORMAT (1X,13X,D22.15,1X,13X,D22.15)                            00620060
        READ (21,40) L                                                  00630056
        READ (21,41) GAMMA                                              00640056
        READ (21,41) B                                                  00650056
        READ (21,41) K0MIN                                              00660056
        READ (21,40) N                                                  00670056
        READ (21,40) N8                                                 00680056
        DO 30 J=0,N8                                                    00690035
        DO 30 I=0,N8                                                    00700035
           READ (21,49) MC(I,J),MS(I,J)                                 00710056
   30   CONTINUE                                                        00720035
C                                                                       00730035
        CLOSE (21,STATUS='KEEP')                                        00740035
C                                                                       00750035
C       ***************************************************             00760038
C       ****  BERECHNEN UND PRINTEN DER EIGENWERTE     ****             00770041
C       ***************************************************             00780038
C                                                                       00790038
   50   FORMAT (1X,'L =    ',I6)                                        00800056
   51   FORMAT (1X,'GAMMA =',D23.16)                                    00810060
   52   FORMAT (1X,'B     =',D23.16)                                    00820060
   53   FORMAT (1X,'K0MIN =',D23.16)                                    00830060
   54   FORMAT (1X,'N     =',I6)                                        00840056
   57   FORMAT (1X)                                                     00850056
   58   FORMAT (1X,'NAME DES DATENSATZES: ',A30)                        00860056
        WRITE ( 6,57)                                                   00870056
        WRITE ( 6,57)                                                   00880056
        WRITE ( 6,58) FNR                                               00890061
        WRITE ( 6,50) L                                                 00900056
        WRITE ( 6,51) GAMMA                                             00910056
        WRITE ( 6,52) B                                                 00920056
        WRITE ( 6,53) K0MIN                                             00930056
        WRITE ( 6,54) N                                                 00940056
        WRITE ( 6,57)                                                   00950056
        IF (N8 .GT. 99) THEN N8=99                                      00960040
        IF (N5 .GT. N8)  N5=N8                                          00970054
C                                                                       00980054
        DO 555 N6=N5,N8,N5                                              00990051
C                                                                       01000040
        DO 100 I=0,N6                                                   01010048
        DO 100 J=0,N6                                                   01020048
           A(I+1,J+1)=MC(I,J)                                           01030041
  100   CONTINUE                                                        01040041
C                                                                       01050025
        N7 = N6+1                                                       01060048
        CALL DEIG1(A,100,N7  ,MODE,ER,EI,EV,VW,ICON)                    01070050
C                                                                       01080031
        FN1='EIGENWERTE DER COS-MODEN        '                          01090041
        CALL PREV(FN1,ER,EI,N7)                                         01100041
C                                                                       01110003
        DO 110 I=1,N6                                                   01120048
        DO 110 J=1,N6                                                   01130048
           A(I,J) = MS(I,J)                                             01140041
  110   CONTINUE                                                        01150041
C                                                                       01160041
        N7 = N6                                                         01170048
C                                                                       01180041
        CALL DEIG1(A,100,N7  ,MODE,ER,EI,EV,VW,ICON)                    01190050
C                                                                       01200041
        FN1='EIGENWERTE DER SIN-MODEN        '                          01210041
        CALL PREV(FN1,ER,EI,N7)                                         01220041
C                                                                       01230038
  555   CONTINUE                                                        01240048
C                                                                       01250000
        END                                                             01260037
C                                                                       01270041
C                                                                       01280041
C       ***************************************************             01290041
C       ****           SUBROUTINES                     ****             01300041
C       ***************************************************             01310041
C                                                                       01320041
       SUBROUTINE PREV(FN1,ER,EI,N7)                                    01330042
       CHARACTER FN1*60                                                 01340041
       DOUBLE PRECISION  ER(100),EI(100)                                01350050
       INTEGER N7,I                                                     01360041
 2222  FORMAT (1X,A60)                                                  01370041
 2228  FORMAT (1X,'MATRIXGROESSE =',I3)                                 01380041
 2233  FORMAT (1X,I3,5X,D17.10,5X,D17.10)                               01390060
       WRITE ( 6,2222) FN1                                              01400041
       WRITE ( 6,2228) N7                                               01410041
       DO 2311 I=1,N7                                                   01420041
          WRITE ( 6,2233) I,ER(I),EI(I)                                 01430041
 2311  CONTINUE                                                         01440041
       RETURN                                                           01450041
C                                                                       01460041
       END                                                              01470041
//LKED.SYSLIB   DD  DSN=SYS1.SSL2,DISP=SHR                              01480035
//              DD  DSN=SYS1.FORTLIB,DISP=SHR                           01490035
//GO.SYSIN DD *                                                         01500035
/*                                                                      01510035
/*      EXEC DRUCKE                                                     01520035
/*      END                                                             01530035
