C/D14KS    JOB RACF,CLASS=C,                                            00010003
C/    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K                             00020003
C/E2          EXEC  FORTCLG,REGION.FORT=1024K,REGION.GO=2048K           00030003
C/FORT.SYSIN    DD *                                                    00040003
C                                                                       00050002
C       ##################################################              00060005
C       ####   MODELL: MA86                           ####              00070005
C       ####   NAME DES PROGRAMMS:  MA86(D2KS)        ####              00080005
C       ####      LAPLACE(RHON) = RHON                ####              00090005
C       ####      LAPLACE(PHIA) = RHON - 1            ####              00100005
C       ####      R.B. : RHON = C                     ####              00110005
C       ####           : PHIA = - GAMMA * KAPPA       ####              00120005
C       ####             (KAPPA = 2* MITTL. KRUEMMUNG)####              00130005
C       ####      VA    =  GRAD( PHIA )               ####              00140005
C       ####             (RHOA AUF 1 SKALIERT !)      ####              00150005
C       ##################################################              00160005
C       2-DIM UND KREISSYMMETRIE                                        00170005
C       AUFGABE: DIENT ZUM ZEIGEN, DASS DER NUMERISCHE STEADY STATE     00180005
C                IN DER NAEHE DES ANALYTISCHEN STEADY STATES LIEGT.     00190005
C        DATEN WERDEN IN    NICHT        ABGESPEICHERT.                 00200005
C        GRAPHIK-PROGRAMM:     WIRD NICHT BENOETIGT                     00210005
C                                                                       00220005
        INTEGER           I9,   I,N,ICNTL,COUNT,COUN9,CPRIN             00230002
        DOUBLE PRECISION  GAMMA    ,C,DELT ,T9                          00240002
        DOUBLE PRECISION  B      ,BPKT      ,BSTRT                      00250002
        DOUBLE PRECISION  R(21),DR(21),DR1(21),DR2(21)                  00260002
        DOUBLE PRECISION  PI,H1,H2,H3                                   00270002
        DOUBLE PRECISION  FB                                            00280002
C                                                                       00290002
        DOUBLE PRECISION  RHON(401),GPOT(401)                           00300002
        DOUBLE PRECISION  RHOA                                          00310000
C                                                                       00320002
        DOUBLE PRECISION  A(401,401),BL(401)                            00330002
        DOUBLE PRECISION  VW(401),IP(401)                               00340002
        DOUBLE PRECISION  EPSZ,IS                                       00350002
        INTEGER           ICON,ISW                                      00360002
C       CHARACTER         FNR*30 , FN1*60                               00370002
C                                                                       00380002
C       **************************************************              00390002
C       ****      PARAMETER PARAMETER PARAMETER       ****              00400002
C       **************************************************              00410000
C                                                                       00420002
C       FNR ='D14K.XXXX.DATA'                                           00430003
        C         =     3.26250D+00                                     00440004
        GAMMA     =      .01000D+00                                     00450002
        RHOA      =     1.00000D+00                                     00460003
        I9        =     9                                               00470003
        R(0)      =      .0                                             00480002
        R(1)      =      .2000D0                                        00490003
        R(2)      =      .4000D0                                        00500003
        R(3)      =      .6000D0                                        00510003
        R(4)      =      .7000D0                                        00520003
        R(5)      =      .8000D0                                        00530003
        R(6)      =      .9000D0                                        00540003
        R(I9-2)   =      .9500D0                                        00550003
        R(I9-1)   =      .9800D0                                        00560003
        R(I9)     =     1.                                              00570002
        BSTRT     =     6.00000D0                                       00580004
        DELT      =     0.02000D0                                       00590003
        CEND      = 40000                                               00600004
        CPRIN     =   500                                               00610002
        EPSZ      =     1.D-08                                          00620002
        ISW       =     1                                               00630002
C                                                                       00640000
        H1=1.0D+00                                                      00650002
        PI=4.0D+00*DATAN(H1)                                            00660002
C       H1=I9                                                           00670002
C       DO 120 I=2,  I9-3,1                                             00680002
C          H2=I                                                         00690002
C          R(I)=(R(I9-2)-R(1))/(H1-3.) * (H2-1.) +R(1)                  00700002
C 120   CONTINUE                                                        00710002
        WRITE (6,40) 0,0.00D+00                                         00720002
        DO 130 I=1,  I9,1                                               00730000
           DR(I)=R(I)-R(I-1)                                            00740002
           WRITE (6,40) I,R(I),DR(I)                                    00750002
  130   CONTINUE                                                        00760002
        DO 136 I=1,  I9-1,1                                             00770002
           DR1(I)=1./DR(I)/(DR(I+1)+DR(I))                              00780002
           DR2(I)=1./DR(I+1)/(DR(I+1)+DR(I))                            00790002
  136   CONTINUE                                                        00800002
        B   =BSTRT                                                      00810002
        ICNTL=0                                                         00820002
        COUNT=-1                                                        00830000
        COUN9=CPRIN-1                                                   00840002
        T9   =-DELT                                                     00850002
C                                                                       00860002
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<          00870002
C       >>>>     ZEITSCHLEIFE   (ANFANG)                  <<<<          00880002
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<          00890002
C                                                                       00900002
 2000   CONTINUE                                                        00910002
        COUNT=COUNT+1                                                   00920002
        COUN9=COUN9+1                                                   00930000
        T9   =T9   +DELT                                                00940002
C                                                                       00950002
C                                                                       00960002
C       ***************************************************             00970002
C       ****   AUFSTELLEN DER MATRIX A( , )            ****             00980002
C       ***************************************************             00990002
C                                                                       01000002
 2100   CONTINUE                                                        01010002
        ICNTL=ICNTL+1                                                   01020002
        DO 300 I=1,I9 ,1                                                01030000
           BL(I)=0.                                                     01040002
        DO 300 N=1,I9,1                                                 01050002
           A(I,N)=0.                                                    01060002
  300   CONTINUE                                                        01070002
C       >>>>>>>>>>>MITTLERER PUNKT<<<<<<<<<<                            01080002
        DO 320 I=1,  I9-1,1                                             01090002
         A(I,I)=-      2.*(DR1(I)+DR2(I))/B/B   +                       01100002
     *     (DR(I+1)*DR1(I)-DR(I)*DR2(I))/R(I)/B/B                       01110002
  320   CONTINUE                                                        01120002
C       >>>>>>>>>>>PUNKT DARUEBER<<<<<<<<<<                             01130000
        DO 340 I=1,  I9-2 ,1                                            01140002
         A(I,I+1)=       2.* DR2(I)/B/B                                 01150002
     *    +(DR(I)*DR2(I))/R(I)/B/B                                      01160002
  340   CONTINUE                                                        01170002
C       >>>>>>>>>>>PUNKT DARUNTER<<<<<<<<<<                             01180002
        DO 360 I=2,  I9-1,1                                             01190002
         A(I,I-1)= 2.* DR1(I)/B/B                                       01200002
     *    -      (DR(I+1)*DR1(I))/R(I)/B/B                              01210002
  360   CONTINUE                                                        01220002
        A(1, I9)=(2.*DR1(1)-DR(2)*DR1(1)/R(1))/B/B                      01230000
C       >>>>>>>>>>>  NULLPUNKT  <<<<<<<<<<<<<                           01240002
        A(I9,I9)=-4./DR(1)/DR(1)/B/B                                    01250002
        A(I9,1)= 4./DR(1)/DR(1)/B/B                                     01260002
C                                                                       01270002
        DO 560 I=1,I9,1                                                 01280002
         IF (ICNTL .EQ. 1)   A(I,I)=A(I,I) - 1.                         01290002
  560   CONTINUE                                                        01300002
C       ***************************************************             01310002
C       ****   AUFSTELLEN DER MATRIX BL(  )            ****             01320002
C       ***************************************************             01330000
C                                                                       01340002
           IF (ICNTL .EQ. 1)   FB=C                                     01350002
           IF (ICNTL .EQ. 2)   FB=-GAMMA/B                              01360002
C                                                                       01370002
C       >>>>>>>>  RANDPUNKT DARUEBER  <<<<<<<                           01380002
        I=I9-1                                                          01390002
         H1=(      2.* DR2(I)                                           01400002
     *    +      (DR(I)*DR2(I))/R(I))/B/B   * FB                        01410002
         BL(I)=-H1                                                      01420002
        DO 610 I=1, I9,1                                                01430000
           IF (ICNTL .EQ. 2)   BL(I)=BL(I)+(RHON(I)-1.)/RHOA            01440002
  610   CONTINUE                                                        01450002
C                                                                       01460002
C                                                                       01470002
C                                                                       01480002
        CALL DLAX(A,401,I9,BL,EPSZ,ISW,IS,VW,IP,ICON)                   01490002
C                                                                       01500002
        DO 820 I=1,I9,1                                                 01510002
           IF (ICNTL .EQ. 1)   RHON(I)=BL(I)                            01520002
           IF (ICNTL .EQ. 2)   GPOT(I)=BL(I)                            01530000
  820   CONTINUE                                                        01540002
        IF (ICNTL. EQ. 1) GOTO 2100                                     01550002
C                                                                       01560002
        IF (COUN9 .EQ. CPRIN) THEN                                      01570002
           WRITE (6,42) T9,COUNT                                        01580002
           WRITE (6,40) 0,GPOT(I9),RHON(I9)                             01590002
           DO 7777 I=1, I9-1,1                                          01600002
              WRITE (6,40) I,GPOT(I),RHON(I)                            01610002
 7777      CONTINUE                                                     01620002
           WRITE (6,40) I9,-GAMMA/B,C                                   01630000
        ENDIF                                                           01640002
   40   FORMAT (1X,I8,2X,D23.16,2X,D23.16)                              01650002
   42   FORMAT (1X,'T = ',D23.16,2X,'SCHRITT = ',I8)                    01660002
C                                                                       01670002
C       ******************************************************          01680002
C       ****         BEWEGUNG DES RANDES          ****                  01690002
C       ******************************************************          01700002
C                                                                       01710002
           H3=-GAMMA/B                                                  01720002
           H1=GPOT(I9-1)                                                01730000
           H2=GPOT(I9-2)                                                01740002
           BPKT=       ( (H3-H1)*(DR(I9)+DR(I9-1))/DR(I9)/DR(I9-1)      01750002
     *       -(H3-H2)*DR(I9)/DR(I9-1)/(DR(I9)+DR(I9-1)) )/B             01760002
           IF (COUN9 .EQ. CPRIN) WRITE (6,40) 1,B   ,BPKT               01770002
           B   =B   +BPKT   *DELT                                       01780002
        ICNTL=0                                                         01790002
        IF (COUN9 .EQ. CPRIN) COUN9=0                                   01800002
        IF (COUNT .EQ. CEND) GOTO 9000                                  01810002
        GOTO 2000                                                       01820002
 9000   CONTINUE                                                        01830003
C       END                                                             01840003
C/LKED.SYSLIB   DD  DSN=SYS1.SSL2,DISP=SHR                              01850003
C/              DD  DSN=SYS1.FORTLIB,DISP=SHR                           01860003
C/GO.SYSIN DD *                                                         01870003
C*                                                                      01880003
C*      EXEC DRUCKE                                                     01890003
C*      END                                                             01900003
        END                                                             01910003
