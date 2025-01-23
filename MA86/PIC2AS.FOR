C                                                                       00010010
C                                                                       00020010
C     ******************************************************************00030010
C     ******************************************************************00040010
C     ****                                                          ****00050010
C     ****      MA86(PIC2AS) (GRAPHIC-PROGRAMM ZU MA86(D2AS))       ****00060010
C     ****                                                          ****00070010
C     ******************************************************************00080010
C     ******************************************************************00090010
C                                                                       00100000
C                                                                       00110010
      INTEGER K1                                                        00120010
      INTEGER I,N,N9,I9,COUNT,L                                         00130010
      DOUBLE PRECISION XMIN,XMAX,YMIN,YMAX,X0,Y0,VL,PS                  00140010
      DOUBLE PRECISION H1,H2,H3,H4,PI,T, GAMMA,C,RHOA,DELT,DFI          00150010
      DOUBLE PRECISION RHON(0:400),GPOT(0:400),VR(0:400),VFI(0:400)     00160010
      DOUBLE PRECISION R(0:21),  B(0:81),BPKT(0:81)                     00170010
      CHARACTER PROG*1,FN*30,SCR*2,CNT*3                                00180010
C                                                                       00190010
C                                                                       00200000
C                                                                       00210010
      PI=4.*DATAN(1.0D+00)                                              00220010
      FN= 'D14K.TEST.DATA'                                              00230011
C     ***********        INPUT OF THE TERMINAL-NUMBER        ***********00240010
      CALL INITT (960)                                                  00250010
      CALL WRITEK (1,' ')                                               00260010
      CALL NEWLIN                                                       00270010
      CALL WRITEK (34,'WHICH TEKTRONIX-TERMINAL IS USED ?')             00280010
      CALL NEWLIN                                                       00290010
      CALL WRITEK (34,'**********************************')             00300000
      CALL NEWLIN                                                       00310010
      CALL WRITEK (1,' ')                                               00320010
      CALL NEWLIN                                                       00330010
      CALL WRITEK (34,'ENTER: DEFAULT  IF TEKTRONIX 4012 ')             00340010
      CALL NEWLIN                                                       00350010
      CALL WRITEK (34,'            T2  IF TEKTRONIX 4014 ')             00360010
      CALL NEWLIN                                                       00370010
      SCR='T1'                                                          00380010
      READ (5,'(A2)') SCR                                               00390010
C                                                                       00400000
C                                                                       00410010
C     ***********  BILDSCHIRMPARAMETER *********                        00420010
C                                                                       00430010
      XMIN= -    .10D+00                                                00440010
      XMAX=     7.                                                      00450010
      YMIN= -    .10D+00                                                00460010
      YMAX=     7.                                                      00470010
      VL  =      .75D0                                                  00480011
      PS  =      .0200D+00                                              00490010
C                                                                       00500000
C                                                                       00510010
  555 IF (SCR .NE. 'T2') THEN                                           00520010
         K1=1                                                           00530010
         CALL SETBUF (3)                                                00540010
         CALL TERM (1,1024)                                             00550010
         CALL ERASE                                                     00560010
         CALL WRITEK (1,' ')                                            00570010
         CALL WRITEK (33,'*********************************')           00580010
         CALL WRITEK (33,'*                               *')           00590010
         CALL WRITEK (33,'* WELCOME TO THE TEKTRONIX 4012 *')           00600000
         CALL WRITEK (33,'*                               *')           00610010
         CALL WRITEK (33,'*********************************')           00620010
      ELSE                                                              00630010
         K1=4                                                           00640010
         CALL SETBUF (3)                                                00650010
         CALL TERM (3,4096)                                             00660010
         CALL ERASE                                                     00670010
         CALL WRITEK (1,' ')                                            00680010
         CALL WRITEK (33,'*********************************')           00690010
         CALL WRITEK (33,'*                               *')           00700000
         CALL WRITEK (33,'* WELCOME TO THE TEKTRONIX 4014 *')           00710010
         CALL WRITEK (33,'*                               *')           00720010
         CALL WRITEK (33,'*********************************')           00730010
      END IF                                                            00740010
C                                                                       00750010
C     >>>>>>>>>>> INPUT OF DATA-SET-NAME AND OPEN DATA-SET <<<<<<<      00760010
      WRITE (6,*) ' '                                                   00770010
      WRITE (6,*) ' '                                                   00780010
      WRITE (6,*) ' '                                                   00790010
      WRITE (6,*) '       EINGABE DER DATEN-DATEI'                      00800000
      WRITE (6,*) '=========================================='          00810010
      WRITE (6,*) ' '                                                   00820010
      WRITE (6,*) ' '                                                   00830010
  211 FORMAT (' YOU CAN CHOOSE: DEFAULT   ',A30)                        00840010
      WRITE (6,211) FN                                                  00850010
      WRITE (6,*) ' '                                                   00860010
      WRITE (6,*) '                    (1)   D14K.TES1.DATA'            00870011
      WRITE (6,*) '                    (2)   D14K.TES2.DATA'            00880011
      WRITE (6,*) '                    (3)   D14K.TES3.DATA'            00890011
      WRITE (6,*) '                    (4)   D14K.TES4.DATA'            00900011
      WRITE (6,*) '                    (5)   D14K.TES5.DATA'            00910011
      WRITE (6,*) '                    (0)       .    .    '            00920011
      WRITE (6,*) ' '                                                   00930010
      PROG =' '                                                         00940010
      READ (5,'(A1)') PROG                                              00950010
      IF (PROG .EQ. '1') FN= 'D14K.TES1.DATA'                           00960011
      IF (PROG .EQ. '2') FN= 'D14K.TES2.DATA'                           00970011
      IF (PROG .EQ. '3') FN= 'D14K.TES3.DATA'                           00980011
      IF (PROG .EQ. '4') FN= 'D145.TES4.DATA'                           00990011
      IF (PROG .EQ. '5') FN= 'D14K.TES5.DATA'                           01000011
      IF (PROG .EQ. '0') GOTO 853                                       01010010
      GOTO 666                                                          01020010
  853 WRITE (6,*) ' '                                                   01030010
      WRITE (6,*) ' NAME DER DATEN-DATEI :'                             01040010
      READ (5,'(A20)') FN                                               01050010
  666 OPEN(21,FILE=FN)                                                  01060010
C                                                                       01070010
C     >>>>> AENDERN DER BILSCHIRMPARAMETER <<<<<<                       01080010
  988 CONTINUE                                                          01090010
      CALL ERASE                                                        01100000
      CALL ANMODE                                                       01110011
      WRITE (6,*) ' '                                                   01120010
      WRITE (6,*) ' '                                                   01130010
      WRITE (6,*) ' '                                                   01140010
      WRITE (6,*) '         MAX. AND MIN. VALUES      '                 01150010
      WRITE (6,*) '=========================================='          01160010
      WRITE (6,*) ' '                                                   01170010
  221 FORMAT ('  (B)  BOUNDARY        :  XMIN=',F10.5,'    XMAX='F10.5) 01180010
  222 FORMAT ('                       :  YMIN=',F10.5,'    YMAX='F10.5) 01190010
  223 FORMAT ('  (L)  LENGTH OF V-VEK.:    VL=',F10.5)                  01200010
  224 FORMAT ('  (P)  POINT SIZE      :    PS=',F10.5)                  01210000
      WRITE (6,221) XMIN,XMAX                                           01220010
      WRITE (6,222) YMIN,YMAX                                           01230010
      WRITE (6,223) VL                                                  01240010
      WRITE (6,224) PS                                                  01250010
      WRITE (6,*) ' '                                                   01260010
      WRITE (6,*) ' '                                                   01270010
      WRITE (6,*) 'CHANGE SCALE-PARAMETER ?  (DEFAULT = NO CHANGE)'     01280010
      WRITE (6,*) ' '                                                   01290010
      PROG=' '                                                          01300010
      READ (5,'(A1)') PROG                                              01310000
      IF (PROG .EQ. ' ') GOTO 999                                       01320010
      IF (PROG .EQ. 'B') THEN                                           01330010
            WRITE (6,*) 'VIER WERTE (DURCH KOMMATA GETRENNT)'           01340010
            READ (5,*) H1,H2,H3,H4                                      01350010
            XMIN=H1                                                     01360010
            XMAX=H2                                                     01370010
            YMIN=H3                                                     01380010
            YMAX=H4                                                     01390010
      ENDIF                                                             01400010
      IF (PROG .EQ. 'L') THEN                                           01410000
            WRITE (6,*) 'EIN WERT'                                      01420010
            READ (5,*) H1                                               01430010
            VL  =H1                                                     01440010
      ENDIF                                                             01450010
      IF (PROG .EQ. 'P') THEN                                           01460010
            WRITE (6,*) 'EIN WERT'                                      01470010
            READ (5,*) H1                                               01480010
            PS  =H1                                                     01490010
      ENDIF                                                             01500010
      GOTO 988                                                          01510000
C                                                                       01520010
C     >>>>>>>>>> INPUT DATA AND PLOT DATA <<<<<<<<<<                    01530010
  999 CONTINUE                                                          01540010
      REWIND 21                                                         01550010
   30 FORMAT (1X,A3)                                                    01560010
   31 FORMAT (1X,8X,D23.16,2(10X,D23.16))                               01570010
   32 FORMAT (1X,8X,I3,20X,10X,I3,20X,10X,D23.16)                       01580010
   35 FORMAT (1X,I3,2(2X,D23.16))                                       01590010
   40 FORMAT (1X,8X,I8,15X,2(10X,D23.16))                               01600010
   41 FORMAT (1X,I3,2X,I3,4(2X,D23.16))                                 01610000
   42 FORMAT (1X,I3,2(2X,D23.16))                                       01620010
      READ (21, 30) CNT                                                 01630010
      READ (21, 31) GAMMA,RHOA,C                                        01640010
      READ (21, 32) N9,I9,DFI                                           01650010
      DO 881 I=0,I9,1                                                   01660010
         READ (21, 35) N,R(I)                                           01670010
  881 CONTINUE                                                          01680010
C                                                                       01690010
 1991 READ (21,40) COUNT,T,DELT                                         01700010
      DO 1855 I=0,N9*I9  ,1                                             01710000
         READ (21,41) L,L,RHON(I),GPOT(I),VR(I),VFI(I)                  01720010
 1855 CONTINUE                                                          01730010
      DO 1856 N=1,N9,1                                                  01740010
         READ (21,42) L,B(N),BPKT(N)                                    01750010
 1856 CONTINUE                                                          01760010
C                                                                       01770010
      CALL TWINDO (250*K1, 950*K1, 50*K1,750*K1)                        01780010
      CALL DWINDO (XMIN,XMAX,YMIN,YMAX)                                 01790010
      CALL ERASE                                                        01800010
      CALL MOVEA (XMIN,YMIN)                                            01810000
      CALL DRAWA (XMAX,YMIN)                                            01820010
      CALL DRAWA (XMAX,YMAX)                                            01830010
      CALL DRAWA (XMIN,YMAX)                                            01840010
      CALL DRAWA (XMIN,YMIN)                                            01850010
      CALL MOVABS (0, 730*K1)                                           01860010
      CALL ANMODE                                                       01870010
  231 FORMAT ('  XMIN =',F8.2)                                          01880010
  232 FORMAT ('  XMAX =',F8.2)                                          01890010
  233 FORMAT ('  YMIN =',F8.2)                                          01900010
  234 FORMAT ('  YMAX =',F8.2)                                          01910000
  251 FORMAT ('     C =',F8.2)                                          01920010
  252 FORMAT (' GAMMA =',F8.4)                                          01930010
  253 FORMAT ('  RHOA =',F8.2)                                          01940010
  261 FORMAT ('     T =',F8.2)                                          01950010
  262 FORMAT ('  DELT =',F8.4)                                          01960010
      WRITE (6,*) 'ABSCISSA:'                                           01970010
      WRITE (6,231) XMIN                                                01980010
      WRITE (6,232) XMAX                                                01990010
      WRITE (6,*) ' '                                                   02000010
      WRITE (6,*) 'ORDINATE:'                                           02010000
      WRITE (6,233) YMIN                                                02020010
      WRITE (6,234) YMAX                                                02030010
      WRITE (6,*) ' '                                                   02040010
      WRITE (6,251) C                                                   02050010
      WRITE (6,252) GAMMA                                               02060010
      WRITE (6,253) RHOA                                                02070010
      WRITE (6,*) ' '                                                   02080010
      WRITE (6,261) T                                                   02090010
      WRITE (6,262) DELT                                                02100010
C                                                                       02110000
      X0=0.                                                             02120010
      Y0=0.                                                             02130010
      CALL MOVEA (X0+B(1),Y0)                                           02140010
      DO 1858 I=1,N9,1                                                  02150010
         H4=I-1                                                         02160010
         CALL DRAWA (X0+B(I)*DCOS(DFI*H4) , Y0+B(I)*DSIN(DFI*H4))       02170010
 1858 CONTINUE                                                          02180010
      CALL MOVEA (X0,Y0)                                                02190010
      CALL DRAWA (X0,Y0)                                                02200010
      CALL MOVEA (X0+PS,Y0)                                             02210000
      DO 1897 L=0,40 ,10                                                02220010
         H4=L                                                           02230010
C        CALL DRAWA (X0,Y0)                                             02240010
         CALL DRAWA (X0+DCOS(PI*H4/20.)*PS,                             02250010
     *               Y0+DSIN(PI*H4/20.)*PS)                             02260010
 1897    CONTINUE                                                       02270010
C                                                                       02280010
      DO 1921 I=1,I9,1                                                  02290010
      DO 1921 N=1,N9,1                                                  02300010
         IF (R(I).GT.1.0000001D0) GOTO 1921                             02310000
         H4=N-1                                                         02320010
         H1=DCOS(DFI*H4)                                                02330010
         H2=DSIN(DFI*H4)                                                02340010
         H3=R(I)*B(N)                                                   02350010
         CALL MOVEA (X0+H3*H1,Y0+H3*H2)                                 02360010
         CALL DRAWA (X0+H3*H1,Y0+H3*H2)                                 02370010
         CALL MOVEA (X0+H3*H1+PS,Y0+H3*H2)                              02380010
         DO 1923 L=0,40 ,10                                             02390010
            H4=L                                                        02400010
C           CALL DRAWA (X0+H3*H1,Y0+H3*H2)                              02410000
            CALL DRAWA (X0+H3*H1+DCOS(PI*H4/20.)*PS,                    02420010
     *                  Y0+H3*H2+DSIN(PI*H4/20.)*PS)                    02430010
 1923    CONTINUE                                                       02440010
         CALL MOVEA (X0+H3*H1,Y0+H3*H2)                                 02450010
         CALL DRAWA (X0+H3*H1+(VR(N+N9*(I-1))*H1-VFI(N+N9*(I-1))*H2)*VL,02460010
     *               Y0+H3*H2+(VR(N+N9*(I-1))*H2+VFI(N+N9*(I-1))*H1)*VL)02470010
 1921 CONTINUE                                                          02480010
C                                                                       02490010
         CALL VCURSR (ICH,XDUMMY,YDUMMY)                                02500010
C           A=65 B=66 C=67 .......                                      02510000
C           L=76 M=77 N=78 .......                                      02520010
C         N : NAECHSTE KURVE   /    E : ENDE                            02530010
C         P : BILDSCHIRMPARAMETER AENDERN                               02540010
C         D : ANDERER DATENSATZ                                         02550010
C                                                                       02560010
C                                                                       02570010
         IF (ICH .EQ. 65 .OR. ICH .EQ. 97) GOTO 999                     02580010
         IF (ICH .EQ. 69 .OR. ICH .EQ. 101) GOTO 9999                   02590010
         IF (ICH .EQ. 68 .OR. ICH .EQ. 100) GOTO 555                    02600010
         IF (ICH .EQ. 80 .OR. ICH .EQ. 112) GOTO 988                    02610000
         IF (ICH .EQ. 78 .OR. ICH .EQ. 110) GOTO 1991                   02620010
         GOTO 1991                                                      02630010
C                                                                       02640010
C                                                                       02650010
 9999 STOP                                                              02660010
      END                                                               02670010
