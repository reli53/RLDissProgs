C                                                                       00010001
C                                                                       00020001
C     ******************************************************************00030001
C     ******************************************************************00040001
C     ****                                                          ****00050001
C     ****      MA86(PIC2XY) (GRAPHIC-PROGRAMM ZU MA86(D2XY))       ****00060001
C     ****                                                          ****00070001
C     ******************************************************************00080001
C     ******************************************************************00090001
C                                                                       00100000
C                                                                       00110001
      INTEGER K1                                                        00120001
      INTEGER I,N,N9(0:21),I9,COUNT,L                                   00130001
      DOUBLE PRECISION XMIN,XMAX,YMIN,YMAX,X0,Y0,VL,PS                  00140001
      DOUBLE PRECISION H1,H2,H3,H4,PI,T, GAMMA,C,RHOA,DELT              00150001
      DOUBLE PRECISION RHON(0:400),GPOT(0:400),VX(0:400),VY(0:400)      00160001
      DOUBLE PRECISION X(0:21),Y(0:21),  B(0:81),BPKT(0:81)             00170001
      DOUBLE PRECISION TOP,TOPPKT                                       00180001
      CHARACTER PROG*1,FN*30,SCR*2,CNT*3                                00190001
C                                                                       00200000
C                                                                       00210001
C                                                                       00220001
      PI=4.*DATAN(1.0D+00)                                              00230001
      FN= 'D14K.TEST.DATA'                                              00240001
C     ***********        INPUT OF THE TERMINAL-NUMBER        ***********00250001
      CALL INITT (960)                                                  00260001
      CALL WRITEK (1,' ')                                               00270001
      CALL NEWLIN                                                       00280001
      CALL WRITEK (34,'WHICH TEKTRONIX-TERMINAL IS USED ?')             00290001
      CALL NEWLIN                                                       00300000
      CALL WRITEK (34,'**********************************')             00310001
      CALL NEWLIN                                                       00320001
      CALL WRITEK (1,' ')                                               00330001
      CALL NEWLIN                                                       00340001
      CALL WRITEK (34,'ENTER: DEFAULT  IF TEKTRONIX 4012 ')             00350001
      CALL NEWLIN                                                       00360001
      CALL WRITEK (34,'            T2  IF TEKTRONIX 4014 ')             00370001
      CALL NEWLIN                                                       00380001
      SCR='T1'                                                          00390001
      READ (5,'(A2)') SCR                                               00400000
C                                                                       00410001
C     ***********  BILDSCHIRMPARAMETER *********                        00420001
C                                                                       00430001
      XMIN= -    .10D+00                                                00440001
      XMAX=     7.                                                      00450001
      YMIN= -    .10D+00                                                00460001
      YMAX=     7.                                                      00470001
      VL  =      .7500D+00                                              00480001
      PS  =      .0200D+00                                              00490001
C                                                                       00500000
  555 IF (SCR .NE. 'T2') THEN                                           00510001
         K1=1                                                           00520001
         CALL SETBUF (3)                                                00530001
         CALL TERM (1,1024)                                             00540001
         CALL ERASE                                                     00550001
         CALL WRITEK (1,' ')                                            00560001
         CALL WRITEK (33,'*********************************')           00570001
         CALL WRITEK (33,'*                               *')           00580001
         CALL WRITEK (33,'* WELCOME TO THE TEKTRONIX 4012 *')           00590001
         CALL WRITEK (33,'*                               *')           00600000
         CALL WRITEK (33,'*********************************')           00610001
      ELSE                                                              00620001
         K1=4                                                           00630001
         CALL SETBUF (3)                                                00640001
         CALL TERM (3,4096)                                             00650001
         CALL ERASE                                                     00660001
         CALL WRITEK (1,' ')                                            00670001
         CALL WRITEK (33,'*********************************')           00680001
         CALL WRITEK (33,'*                               *')           00690001
         CALL WRITEK (33,'* WELCOME TO THE TEKTRONIX 4014 *')           00700000
         CALL WRITEK (33,'*                               *')           00710001
         CALL WRITEK (33,'*********************************')           00720001
      END IF                                                            00730001
C                                                                       00740001
C     >>>>>>>>>>> INPUT OF DATA-SET-NAME AND OPEN DATA-SET <<<<<<<      00750001
      WRITE (6,*) ' '                                                   00760001
      WRITE (6,*) ' '                                                   00770001
      WRITE (6,*) ' '                                                   00780001
      WRITE (6,*) '       EINGABE DER DATEN-DATEI'                      00790001
      WRITE (6,*) '=========================================='          00800000
      WRITE (6,*) ' '                                                   00810001
      WRITE (6,*) ' '                                                   00820001
  211 FORMAT (' YOU CAN CHOOSE: DEFAULT   ',A30)                        00830001
      WRITE (6,211) FN                                                  00840001
      WRITE (6,*) ' '                                                   00850001
      WRITE (6,*) '                    (1)   D14K.TES1.DATA'            00860001
      WRITE (6,*) '                    (2)   D14K.TES2.DATA'            00870001
      WRITE (6,*) '                    (3)   D14K.TES3.DATA'            00880001
      WRITE (6,*) '                    (4)   D145.TES4.DATA'            00890001
      WRITE (6,*) '                    (5)   D14K.TES5.DATA'            00900001
      WRITE (6,*) '                    (6)   D14K.TES6.DATA'            00910001
      WRITE (6,*) '                    (0)       .    .    '            00920001
      WRITE (6,*) ' '                                                   00930001
      PROG =' '                                                         00940001
      READ (5,'(A1)') PROG                                              00950001
      IF (PROG .EQ. '1') FN= 'D14K.TES1.DATA'                           00960001
      IF (PROG .EQ. '2') FN= 'D14K.TES2.DATA'                           00970001
      IF (PROG .EQ. '3') FN= 'D14K.TES3.DATA'                           00980001
      IF (PROG .EQ. '4') FN= 'D14K.TES4.DATA'                           00990001
      IF (PROG .EQ. '5') FN= 'D14K.TES5.DATA'                           01000001
      IF (PROG .EQ. '6') FN= 'D14K.TES6.DATA'                           01010001
      IF (PROG .EQ. '0') GOTO 853                                       01020001
      GOTO 666                                                          01030001
  853 WRITE (6,*) ' '                                                   01040001
      WRITE (6,*) ' NAME DER DATEN-DATEI :'                             01050001
      READ (5,'(A20)') FN                                               01060001
  666 OPEN(21,FILE=FN)                                                  01070001
C                                                                       01080001
C                                                                       01090001
C     >>>>>> AENDERN DER BILDSCHIRMPARAMETER <<<<<                      01100000
  988 CALL ERASE                                                        01110001
      CALL ANMODE                                                       01120001
      WRITE (6,*) ' '                                                   01130001
      WRITE (6,*) ' '                                                   01140001
      WRITE (6,*) ' '                                                   01150001
      WRITE (6,*) '         MAX. AND MIN. VALUES      '                 01160001
      WRITE (6,*) '=========================================='          01170001
      WRITE (6,*) ' '                                                   01180001
  221 FORMAT ('  (B)  BOUNDARY        :  XMIN=',F10.5,'    XMAX='F10.5) 01190001
  222 FORMAT ('                       :  YMIN=',F10.5,'    YMAX='F10.5) 01200001
  223 FORMAT ('  (L)  LENGTH OF V-VEK.:    VL=',F10.5)                  01210000
  224 FORMAT ('  (P)  POINT SIZE      :    PS=',F10.5)                  01220001
      WRITE (6,221) XMIN,XMAX                                           01230001
      WRITE (6,222) YMIN,YMAX                                           01240001
      WRITE (6,223) VL                                                  01250001
      WRITE (6,224) PS                                                  01260001
      WRITE (6,*) ' '                                                   01270001
      WRITE (6,*) ' '                                                   01280001
      WRITE (6,*) 'CHANGE SCALE-PARAMETER ?  (DEFAULT = NO CHANGE)'     01290001
      WRITE (6,*) ' '                                                   01300001
      PROG=' '                                                          01310000
      READ (5,'(A1)') PROG                                              01320001
      IF (PROG .EQ. ' ') GOTO 999                                       01330001
      IF (PROG .EQ. 'B') THEN                                           01340001
            WRITE (6,*) 'VIER WERTE (DURCH KOMMATA GETRENNT)'           01350001
            READ (5,*) H1,H2,H3,H4                                      01360001
            XMIN=H1                                                     01370001
            XMAX=H2                                                     01380001
            YMIN=H3                                                     01390001
            YMAX=H4                                                     01400001
      ENDIF                                                             01410000
      IF (PROG .EQ. 'L') THEN                                           01420001
            WRITE (6,*) 'EIN WERT'                                      01430001
            READ (5,*) H1                                               01440001
            VL  =H1                                                     01450001
      ENDIF                                                             01460001
      IF (PROG .EQ. 'P') THEN                                           01470001
            WRITE (6,*) 'EIN WERT'                                      01480001
            READ (5,*) H1                                               01490001
            PS  =H1                                                     01500001
      ENDIF                                                             01510000
      GOTO 988                                                          01520001
C                                                                       01530001
C     >>>>>>>>>> INPUT DATA AND PLOT DATA <<<<<<<<<<                    01540001
  999 CONTINUE                                                          01550001
      REWIND 21                                                         01560001
   30 FORMAT (1X,A3)                                                    01570001
   31 FORMAT (1X,8X,D23.16,2(10X,D23.16))                               01580001
   32 FORMAT (1X,8X,I3)                                                 01590001
   35 FORMAT (1X,I3,4(2X,D23.16))                                       01600001
   40 FORMAT (1X,8X,I8,15X,2(10X,D23.16))                               01610000
   41 FORMAT (1X,I3,2X,I3,4(2X,D23.16))                                 01620001
   42 FORMAT (1X,I3,2(2X,D23.16))                                       01630001
      READ (21, 30) CNT                                                 01640001
      READ (21, 31) GAMMA,RHOA,C                                        01650001
      READ (21, 32) I9                                                  01660001
      DO 881 I=0,I9,1                                                   01670001
         READ (21, 35) N,X(I),H1,Y(I),H1                                01680001
  881 CONTINUE                                                          01690001
C                                                                       01700001
      N9(0)=0                                                           01710000
      DO 1854 I=1,I9,1                                                  01720001
        N9(I)=N9(I-1)+I9-I+2                                            01730001
 1854 CONTINUE                                                          01740001
C                                                                       01750001
 1991 READ (21,40) COUNT,T,DELT                                         01760001
      DO 1855 I=1,N9(I9)+1 ,1                                           01770001
         READ (21,41) L,L,RHON(I),GPOT(I),VX(I),VY(I)                   01780001
 1855 CONTINUE                                                          01790001
      DO 1856 I=0,I9-1,1                                                01800001
         READ (21,42) L,B(I),BPKT(I)                                    01810000
 1856 CONTINUE                                                          01820001
      READ (21,42) L,TOP,TOPPKT                                         01830001
C                                                                       01840001
      CALL TWINDO (250*K1, 950*K1, 50*K1,750*K1)                        01850001
      CALL DWINDO (XMIN,XMAX,YMIN,YMAX)                                 01860001
      CALL ERASE                                                        01870001
      CALL MOVEA (XMIN,YMIN)                                            01880001
      CALL DRAWA (XMAX,YMIN)                                            01890001
      CALL DRAWA (XMAX,YMAX)                                            01900001
      CALL DRAWA (XMIN,YMAX)                                            01910000
      CALL DRAWA (XMIN,YMIN)                                            01920001
      CALL MOVABS (0, 730*K1)                                           01930001
      CALL ANMODE                                                       01940001
  231 FORMAT ('  XMIN =',F8.2)                                          01950001
  232 FORMAT ('  XMAX =',F8.2)                                          01960001
  233 FORMAT ('  YMIN =',F8.2)                                          01970001
  234 FORMAT ('  YMAX =',F8.2)                                          01980001
  251 FORMAT ('     C =',F8.2)                                          01990001
  252 FORMAT (' GAMMA =',F8.4)                                          02000001
  253 FORMAT ('  RHOA =',F8.2)                                          02010000
  261 FORMAT ('     T =',F8.2)                                          02020001
  262 FORMAT ('  DELT =',F8.4)                                          02030001
      WRITE (6,*) 'ABSCISSA:'                                           02040001
      WRITE (6,231) XMIN                                                02050001
      WRITE (6,232) XMAX                                                02060001
      WRITE (6,*) ' '                                                   02070001
      WRITE (6,*) 'ORDINATE:'                                           02080001
      WRITE (6,233) YMIN                                                02090001
      WRITE (6,234) YMAX                                                02100001
      WRITE (6,*) ' '                                                   02110000
      WRITE (6,251) C                                                   02120001
      WRITE (6,252) GAMMA                                               02130001
      WRITE (6,253) RHOA                                                02140001
      WRITE (6,*) ' '                                                   02150001
      WRITE (6,261) T                                                   02160001
      WRITE (6,262) DELT                                                02170001
C                                                                       02180001
      X0=0.                                                             02190001
      Y0=0.                                                             02200001
      CALL MOVEA (X0,Y0+B(0))                                           02210000
      DO 1858 I=1,I9-1,1                                                02220001
         CALL DRAWA (X0+X(I)*TOP , Y0+B(I))                             02230001
 1858 CONTINUE                                                          02240001
      CALL DRAWA (X0+TOP,0.)                                            02250001
      CALL MOVEA (X0,Y0)                                                02260001
      CALL DRAWA (X0,Y0)                                                02270001
      CALL MOVEA (X0+PS,Y0)                                             02280001
      DO 1897 L=0,40 ,10                                                02290001
         H4=L                                                           02300001
C        CALL DRAWA (X0,Y0)                                             02310000
         CALL DRAWA (X0+DCOS(PI*H4/20.)*PS,                             02320001
     *               Y0+DSIN(PI*H4/20.)*PS)                             02330001
 1897    CONTINUE                                                       02340001
C                                                                       02350001
      DO 1921 I=0,I9,1                                                  02360001
      DO 1921 N=0,I9-I,1                                                02370001
         K=N9(I)+N+1                                                    02380001
         H1=X(I)*TOP                                                    02390001
         H2=0.                                                          02400001
         IF (I .LT. I9) H2=Y(N)*B(I)/Y(I9-I)                            02410000
         H3=1.D+00                                                      02420001
         CALL MOVEA (X0+H3*H1,Y0+H3*H2)                                 02430001
         CALL DRAWA (X0+H3*H1,Y0+H3*H2)                                 02440001
         CALL MOVEA (X0+H3*H1+PS,Y0+H3*H2)                              02450001
         DO 1923 L=0,40 ,10                                             02460001
            H4=L                                                        02470001
C           CALL DRAWA (X0+H3*H1,Y0+H3*H2)                              02480001
            CALL DRAWA (X0+H3*H1+DCOS(PI*H4/20.)*PS,                    02490001
     *                  Y0+H3*H2+DSIN(PI*H4/20.)*PS)                    02500001
 1923    CONTINUE                                                       02510000
         CALL MOVEA (X0+H3*H1,Y0+H3*H2)                                 02520001
         CALL DRAWA (X0+H3*H1+VX(K)*VL,Y0+H3*H2+VY(K)*VL)               02530001
 1921 CONTINUE                                                          02540001
C                                                                       02550001
         CALL VCURSR (ICH,XDUMMY,YDUMMY)                                02560001
C           A=65 B=66 C=67 .......                                      02570001
C           L=76 M=77 N=78 .......                                      02580001
C         N : NAECHSTE KURVE   /    E : ENDE                            02590001
C         P : BILDSCHIRMPARAMETER AENDERN                               02600001
C         D : ANDERER DATENSATZ                                         02610000
C                                                                       02620001
C                                                                       02630001
         IF (ICH .EQ. 65 .OR. ICH .EQ. 97) GOTO 999                     02640001
         IF (ICH .EQ. 69 .OR. ICH .EQ. 101) GOTO 9999                   02650001
         IF (ICH .EQ. 68 .OR. ICH .EQ. 100) GOTO 555                    02660001
         IF (ICH .EQ. 80 .OR. ICH .EQ. 112) GOTO 988                    02670001
         IF (ICH .EQ. 78 .OR. ICH .EQ. 110) GOTO 1991                   02680001
         GOTO 1991                                                      02690001
C                                                                       02700001
C                                                                       02710000
 9999 STOP                                                              02720001
      END                                                               02730001
