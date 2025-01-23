//D14KG    JOB RACF,CLASS=A,                                            00010016
//    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K                             00020000
//E2          EXEC  FORTCLG,REGION.FORT=1024K,REGION.GO=2048K           00030000
//FORT.SYSIN    DD *                                                    00040000
C                                                                       00050000
C       ##################################################              00060000
C       ####          EIGENWERTE BERECHNEN            ####              00070000
C       ####   NAME DES PROGRAMMS:  NOCIRCLE(EV2)     ####              00080003
C       ##################################################              00090000
C                                                                       00100000
C                                                                       00110000
        INTEGER           L,N8,N7,N6,N5                                 00120004
        DOUBLE PRECISION  GAMMA,B,K0MIN                                 00130011
        DOUBLE PRECISION  MC(0:100,0:100),MS(0:100,0:100)               00140004
        DOUBLE PRECISION  H1                                            00150004
C                                                                       00160000
        INTEGER           K,N,ICON,MODE,J,I                             00170000
        DOUBLE PRECISION  A(  100,  100)                                00180004
        DOUBLE PRECISION  EV(  100,  100)                               00190004
        DOUBLE PRECISION  EI(  100),ER(  100)                           00200004
        DOUBLE PRECISION  VW(  100)                                     00210004
        CHARACTER         FNR*30 , FN1*60                               00220011
C                                                                       00230000
C       **************************************************              00240000
C       ****      PARAMETER PARAMETER PARAMETER       ****              00250000
C       **************************************************              00260000
C                                                                       00270000
        FNR ='D14K.M022.DATA'                                           00280018
        N5=10                                                           00290004
        K=100                                                           00300011
        MODE = 1                                                        00310011
C                                                                       00320011
C       DIE EIGENWERTE DER MATRIZEN DER GROESSE N5,2*N5,3*N5,....       00330004
C       WERDEN BERCHNET.                                                00340015
C       AUFRUF DES UNTERPROGRAMMES MIT:                                 00350015
C       **************************************************************  00360015
C       CALL DEIG1(A,K,N,MODE,ER,EI,EV,VW,ICON)                         00370015
C                                                                       00380000
C       K        : K MUSS DIE DEFINIERTE DIMENSION DER MATRIZEN SEIN    00390000
C                : (ALSO STETS N<=K)                                    00400000
C       N        : GROESSE DER MATRIX , MIT DER GERECHNET WIRD          00410013
C       MODE = 1 : AUSBALANZIEREN DER MATRIX , D.H. ABSOLUTE SUMME      00420000
C                  UEBER I-TE ZEILE = ABSL. SUMME UEBER I-TE SPALTE     00430000
C       MODE<>1  : KEIN AUSBALANZIEREN                                  00440000
C       A(K,K)   : A = MATRIX, BZGL. DER DIE EIGENWERTE BERECHNET       00450013
C                  WERDEN. ACHTUNG: MATRIX A WIRD VERAENDERT !!!!       00460000
C       ER(K)    : REALTEILE DER EIGENWERTE                             00470013
C       EI(K)    : IMAGINAERTEILE DER EIGENWERTE                        00480013
C       EV(K,K)  : EV(.,I) IST EIGENVEKTOR ZUM I-TEN EIGENWERT          00490013
C       VW(K)    : ARBEITSVEKTOR FUER EIG1(.....)                       00500013
C       ICON     : KONTROLLPARAMETER FUER EIG1(.....)                   00510000
C                : ICON = 0     NO ERROR                                00520000
C                : ICON = 10000 N=1                                     00530000
C                : ICON = 20000 KEINE REDUKT. AUF DREIECKSMATR. MOEGL.  00540000
C                : ICON = 30000 N<1 ODER K<N                            00550000
C       **************************************************************  00560015
C                                                                       00570000
C       ***************************************************             00580000
C       ****   LESEN DER DATEN                         ****             00590000
C       ***************************************************             00600000
C                                                                       00610000
        OPEN (21,FILE=FNR)                                              00620011
        REWIND 21                                                       00630000
   40   FORMAT (1X,7X       ,I6)                                        00640008
   41   FORMAT (1X,7X       ,D23.16)                                    00650014
   49   FORMAT (1X,13X,D22.15,1X,13X,D22.15)                            00660014
        READ (21,40) L                                                  00670008
        READ (21,41) GAMMA                                              00680008
        READ (21,41) B                                                  00690008
        READ (21,41) K0MIN                                              00700008
        READ (21,40) N                                                  00710008
        READ (21,40) N8                                                 00720008
        DO 30 J=0,N8                                                    00730000
        DO 30 I=0,N8                                                    00740000
           READ (21,49) MC(I,J),MS(I,J)                                 00750008
   30   CONTINUE                                                        00760000
C                                                                       00770000
        CLOSE (21,STATUS='KEEP')                                        00780000
C                                                                       00790000
C       ***************************************************             00800000
C       ****  BERECHNEN UND PRINTEN DER EIGENWERTE     ****             00810000
C       ***************************************************             00820000
C                                                                       00830000
   50   FORMAT (1X,'L     =',I6)                                        00840008
   51   FORMAT (1X,'GAMMA =',D23.16)                                    00850014
   52   FORMAT (1X,'B     =',D23.16)                                    00860014
   53   FORMAT (1X,'K0MIN =',D23.16)                                    00870014
   54   FORMAT (1X,'N     =',I6)                                        00880010
   57   FORMAT (1X)                                                     00890008
   58   FORMAT (1X,'NAME DES DATENSATZES: ',A30)                        00900008
        WRITE ( 6,57)                                                   00910008
        WRITE ( 6,57)                                                   00920008
        WRITE ( 6,58) FNR                                               00930015
        WRITE ( 6,50) L                                                 00940008
        WRITE ( 6,51) GAMMA                                             00950008
        WRITE ( 6,52) B                                                 00960008
        WRITE ( 6,53) K0MIN                                             00970008
        WRITE ( 6,54) N                                                 00980008
        WRITE ( 6,57)                                                   00990008
C                                                                       01000004
        IF (N8 .GT. 99) THEN N8=99                                      01010000
        IF (N5 .GT. N8)  N5=N8                                          01020005
C                                                                       01030005
        DO 555 N6=N5,N8,N5                                              01040004
C                                                                       01050000
        H1=N6                                                           01060001
        N7=  INT(H1*.5+.01)                                             01070000
        DO 100 I=0,N7                                                   01080000
        DO 100 J=0,N7                                                   01090000
           A(I+1,J+1)=MC(I+I,J+J)                                       01100000
  100   CONTINUE                                                        01110000
C                                                                       01120000
        N7 = N7+1                                                       01130000
        CALL DEIG1(A,100,N7  ,MODE,ER,EI,EV,VW,ICON)                    01140004
C                                                                       01150000
        FN1='EIGENWERTE DER GERADEN COS-MODEN        '                  01160000
        CALL PREV(FN1,ER,EI,N7)                                         01170000
C                                                                       01180000
        H1=N6                                                           01190001
        N7=  INT(H1*.5-.01)                                             01200000
        DO 120 I=0,N7                                                   01210000
        DO 120 J=0,N7                                                   01220000
           A(I+1,J+1)=MC(I+I+1,J+J+1)                                   01230000
  120   CONTINUE                                                        01240000
C                                                                       01250000
        N7 = N7+1                                                       01260000
        CALL DEIG1(A,100,N7  ,MODE,ER,EI,EV,VW,ICON)                    01270004
C                                                                       01280000
        FN1='EIGENWERTE DER UNGERADEN COS-MODEN        '                01290000
        CALL PREV(FN1,ER,EI,N7)                                         01300000
C                                                                       01310000
        H1=N6                                                           01320001
        N7=  INT(H1*.5+.01)                                             01330000
        DO 140 I=1,N7                                                   01340000
        DO 140 J=1,N7                                                   01350000
           A(I,J) = MS(I+I  ,J+J  )                                     01360000
  140   CONTINUE                                                        01370000
C                                                                       01380000
        N7 = N7                                                         01390000
C                                                                       01400000
        CALL DEIG1(A,100,N7  ,MODE,ER,EI,EV,VW,ICON)                    01410004
C                                                                       01420000
        FN1='EIGENWERTE DER GERADEN SIN-MODEN        '                  01430000
        CALL PREV(FN1,ER,EI,N7)                                         01440000
C                                                                       01450000
        H1=N6                                                           01460001
        N7=  INT(H1*.5+.51)                                             01470000
        DO 160 I=1,N7                                                   01480000
        DO 160 J=1,N7                                                   01490000
           A(I,J) = MS(I+I-1,J+J-1)                                     01500000
  160   CONTINUE                                                        01510000
C                                                                       01520000
        N7 = N7                                                         01530000
C                                                                       01540000
        CALL DEIG1(A,100,N7  ,MODE,ER,EI,EV,VW,ICON)                    01550004
C                                                                       01560000
        FN1='EIGENWERTE DER UNGERADEN SIN-MODEN        '                01570000
        CALL PREV(FN1,ER,EI,N7)                                         01580000
C                                                                       01590000
  555   CONTINUE                                                        01600001
C                                                                       01610000
        END                                                             01620000
C                                                                       01630000
C                                                                       01640000
C       ***************************************************             01650000
C       ****           SUBROUTINES                     ****             01660000
C       ***************************************************             01670000
C                                                                       01680000
       SUBROUTINE PREV(FN1,ER,EI,N7)                                    01690000
       CHARACTER FN1*60                                                 01700000
       DOUBLE PRECISION  ER(100),EI(100)                                01710004
       INTEGER N7,I                                                     01720000
 2222  FORMAT (1X,A60)                                                  01730000
 2228  FORMAT (1X,'MATRIXGROESSE =',I3)                                 01740000
 2233  FORMAT (1X,I3,5X,D17.10,5X,D17.10)                               01750015
       WRITE ( 6,2222) FN1                                              01760000
       WRITE ( 6,2228) N7                                               01770000
       DO 2311 I=1,N7                                                   01780000
          WRITE ( 6,2233) I,ER(I),EI(I)                                 01790000
 2311  CONTINUE                                                         01800000
       RETURN                                                           01810000
C                                                                       01820000
       END                                                              01830000
//LKED.SYSLIB   DD  DSN=SYS1.SSL2,DISP=SHR                              01840000
//              DD  DSN=SYS1.FORTLIB,DISP=SHR                           01850000
//GO.SYSIN DD *                                                         01860000
/*                                                                      01870000
/*      EXEC DRUCKE                                                     01880000
/*      END                                                             01890000
