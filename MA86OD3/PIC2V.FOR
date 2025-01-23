C                                                                       00000100
C                                                                       00000200
C     ******************************************************************00000300
C     ******************************************************************00000400
C     ****                                                          ****00000500
C     ****            P I C  2 V       (GRAPHIC-PROGRAMM)           ****00000600
C     ****                                                          ****00000700
C     ******************************************************************00000800
C     ******************************************************************00000900
C                                                                       00001000
C                                                                       00001100
      INTEGER I,J,K1,NCOUNT                                             00001200
      DOUBLE PRECISION XMIN,XMAX,YMIN,YMAX                              00001300
      DOUBLE PRECISION H1,H2      ,PI                                   00001400
      DOUBLE PRECISION XPMAX,YPMAX,XPMIN,YPMIN                          00001500
      DOUBLE PRECISION XPP,YPP                                          00001600
      DOUBLE PRECISION XP(32000),YP(32000)                              00001700
      CHARACTER PROG*1,FN*30,SCR*2,SC*1                                 00001800
C                                                                       00001900
C                                                                       00002000
C                                                                       00002100
      PI=4.D0*DATAN(1.D0)                                               00002200
C     ***********        INPUT OF THE TERMINAL-NUMBER        ***********00002300
      CALL INITT (960)                                                  00002400
      CALL WRITEK (1,' ')                                               00002500
      CALL NEWLIN                                                       00002600
      CALL WRITEK (34,'WHICH TEKTRONIX-TERMINAL IS USED ?')             00002700
      CALL NEWLIN                                                       00002800
      CALL WRITEK (34,'**********************************')             00002900
      CALL NEWLIN                                                       00003000
      CALL WRITEK (1,' ')                                               00003100
      CALL NEWLIN                                                       00003200
      CALL WRITEK (34,'ENTER: DEFAULT  IF TEKTRONIX 4012 ')             00003300
      CALL NEWLIN                                                       00003400
      CALL WRITEK (34,'            T2  IF TEKTRONIX 4014 ')             00003500
      CALL NEWLIN                                                       00003600
      SCR='T1'                                                          00003700
      READ (5,'(A2)') SCR                                               00003800
      IF (SCR .NE. 'T2') THEN                                           00003900
         K1=1                                                           00004000
         CALL SETBUF (3)                                                00004100
         CALL TERM (1,1024)                                             00004200
         CALL ERASE                                                     00004300
         CALL WRITEK (1,' ')                                            00004400
         CALL WRITEK (33,'*********************************')           00004500
         CALL WRITEK (33,'*                               *')           00004600
         CALL WRITEK (33,'* WELCOME TO THE TEKTRONIX 4012 *')           00004700
         CALL WRITEK (33,'*                               *')           00004800
         CALL WRITEK (33,'*********************************')           00004900
      ELSE                                                              00005000
         K1=4                                                           00005100
         CALL SETBUF (3)                                                00005200
         CALL TERM (3,4096)                                             00005300
         CALL ERASE                                                     00005400
         CALL WRITEK (1,' ')                                            00005500
         CALL WRITEK (33,'*********************************')           00005600
         CALL WRITEK (33,'*                               *')           00005700
         CALL WRITEK (33,'* WELCOME TO THE TEKTRONIX 4014 *')           00005800
         CALL WRITEK (33,'*                               *')           00005900
         CALL WRITEK (33,'*********************************')           00006000
      END IF                                                            00006100
  555 CALL MOVABS (0,660*K1)                                            00006200
      CALL ANMODE                                                       00006300
      WRITE (6,*) ' '                                                   00006400
      WRITE (6,*) ' '                                                   00006500
      WRITE (6,*) ' '                                                   00006600
      WRITE (6,*) '       EINGABE DER DATEN-DATEI'                      00006700
      WRITE (6,*) '=========================================='          00006800
      WRITE (6,*) ' '                                                   00006900
  137 FORMAT (' YOU CAN CHOOSE: DEFAULT   ',A30)                        00007000
      FN= '    .    .    '                                              00007100
      WRITE (6,137) FN                                                  00007200
      WRITE (6,*) ' '                                                   00007300
      WRITE (6,*) '                    (1)   D14K.TES1.DATA'            00007400
      WRITE (6,*) '                    (2)   D14K.TES2.DATA'            00007500
      WRITE (6,*) '                    (3)   D14K.TES3.DATA'            00007600
      WRITE (6,*) '                    (4)   D14K.TES4.DATA'            00007700
      WRITE (6,*) '                    (5)   D14K.TES5.DATA'            00007800
      WRITE (6,*) '                    (6)   D14K.L0#1.DATA'            00007900
      WRITE (6,*) '                    (7)   D14K.L2#1.DATA'            00008000
      WRITE (6,*) '                    (8)   D14K.L3#1.DATA'            00008100
      WRITE (6,*) ' '                                                   00008200
      PROG =' '                                                         00008300
      READ (5,'(A1)') PROG                                              00008400
      IF (PROG .EQ. '1') FN= 'D14K.TES1.DATA'                           00008500
      IF (PROG .EQ. '2') FN= 'D14K.TES2.DATA'                           00008600
      IF (PROG .EQ. '3') FN= 'D14K.TES3.DATA'                           00008700
      IF (PROG .EQ. '4') FN= 'D14K.TES4.DATA'                           00008800
      IF (PROG .EQ. '5') FN= 'D14K.TES5.DATA'                           00008900
      IF (PROG .EQ. '6') FN= 'D14K.L0#1.DATA'                           00009000
      IF (PROG .EQ. '7') FN= 'D14K.L2#1.DATA'                           00009100
      IF (PROG .EQ. '8') FN= 'D14K.L3#1.DATA'                           00009200
      IF (PROG .EQ. ' ') GOTO 853                                       00009300
      GOTO 666                                                          00009400
  853 WRITE (6,*) ' '                                                   00009500
      WRITE (6,*) ' NAME DER DATEN-DATEI :'                             00009600
      READ (5,'(A20)') FN                                               00009700
C                                                                       00009800
C     ***********  CHOOSE THE SCALES  **********************************00009900
  666 CONTINUE                                                          00010000
      CALL ERASE                                                        00010100
      CALL MOVABS (0,750*K1)                                            00010200
      CALL ANMODE                                                       00010300
      WRITE (6,*) '                             SCALES        '         00010400
      WRITE (6,*) '===================================================='00010500
      WRITE (6,*) ' '                                                   00010600
      WRITE (6,*) ' YOU CAN CHOOSE: '                                   00010700
      WRITE (6,*) ' '                                                   00010800
      WRITE (6,*) '               X-SCALE                   Y-SCALE    '00010900
      WRITE (6,*) 'DEFAULT          X                         Y        '00011000
      WRITE (6,*) ' (1)             X               SIGN(Y) * ]Y]**.5  '00011100
      WRITE (6,*) ' (2)             X               SIGN(Y) * ]Y]**.25 '00011200
      WRITE (6,*) ' (3)             X           X * SIGN(Y) * ]Y]**.5  '00011300
      WRITE (6,*) ' (4)             X           X * SIGN(Y) * ]Y]**.25 '00011400
      WRITE (6,*) ' (5)    SIGN(X) / ]X]**.5                  Y        '00011500
      WRITE (6,*) ' (6)    SIGN(X) / ]X]**.5        SIGN(Y) * ]Y]**.5  '00011600
      WRITE (6,*) ' '                                                   00011700
      SC =' '                                                           00011800
      READ (5,'(A1)') SC                                                00011900
C                                                                       00012000
C     ***********  INPUT OF THE DATA ***********                        00012100
  777 OPEN(21,FILE=FN)                                                  00012200
      REWIND 21                                                         00012300
  200 FORMAT (1X,I8)                                                    00012400
  202 FORMAT (5(1X,D23.14))                                             00012500
C     READ (21,200) NCOUNT                                              00012600
      READ (21, * ) NCOUNT                                              00012610
      DO 1100 I=1,NCOUNT                                                00012700
C        READ (21,202) XP(I),YP(I)                                      00012800
         READ (21, * ) XP(I),YP(I)                                      00012810
 1100 CONTINUE                                                          00012900
 1101 CLOSE(21,STATUS='KEEP')                                           00013000
C                                                                       00013100
      DO 1005 I=1,NCOUNT                                                00013200
         IF (SC .EQ. ' ') GOTO 1005                                     00013300
         IF (SC .EQ. '1' .OR. SC .EQ. '3' .OR. SC .EQ. '6') THEN        00013400
            YP(I)=VORZEI( YP(I) ) * POTABS( YP(I) , .5D0 )              00013500
         ENDIF                                                          00013600
         IF (SC .EQ. '2' .OR. SC .EQ. '4') THEN                         00013700
            YP(I)=VORZEI( YP(I) ) * POTABS( YP(I) , .25D0 )             00013800
         ENDIF                                                          00013900
         IF (SC .EQ. '3' .OR. SC .EQ. '4') THEN                         00014000
            YP(I)= XP(I) * YP(I)                                        00014100
         ENDIF                                                          00014200
         IF (SC .EQ. '5' .OR. SC .EQ. '6') THEN                         00014300
            XP(I)=VORZEI( XP(I) ) * POTABS( XP(I) , .5D0 )              00014400
         ENDIF                                                          00014500
 1005 CONTINUE                                                          00014600
C                                                                       00014700
      XMIN=XP(1)                                                        00014800
      XMAX=XP(1)                                                        00014900
      YMIN=YP(1)                                                        00015000
      YMAX=YP(1)                                                        00015100
      DO 1010 I=1, NCOUNT                                               00015200
            XMIN=DMIN1(XMIN,XP(I))                                      00015300
            XMAX=DMAX1(XMAX,XP(I))                                      00015400
            YMIN=DMIN1(YMIN,YP(I))                                      00015500
            YMAX=DMAX1(YMAX,YP(I))                                      00015600
 1010 CONTINUE                                                          00015700
C                                                                       00015800
C                                                                       00015900
  988 CALL ERASE                                                        00016000
      CALL MOVABS (0,750*K1)                                            00016100
      CALL ANMODE                                                       00016200
      WRITE (6,*) '         MAX. AND MIN. VALUES      '                 00016300
      WRITE (6,*) '=========================================='          00016400
  989 WRITE (6,*) ' '                                                   00016500
  251 FORMAT ('  X                  :   XMIN=',F10.5,'    XMAX='F10.5)  00016600
  252 FORMAT ('  Y                  :   YMIN=',F10.5,'    YMAX='F10.5)  00016700
      WRITE (6,251) XMIN,XMAX                                           00016800
      WRITE (6,252) YMIN,YMAX                                           00016900
      WRITE (6,*) ' '                                                   00017000
      WRITE (6,*) 'CHANGE THE AXES-VALUES?  (DEFAULT = NO CHANGE)'      00017200
      PROG=' '                                                          00017400
      READ (5,'(A1)') PROG                                              00017500
      IF (PROG .EQ. ' ') GOTO 999                                       00017600
      WRITE (6,*) 'ZWEI WERTE (DURCH KOMMATA GETRENNT)'                 00017700
      READ (5,*) H1,H2                                                  00017800
      IF (PROG .EQ. 'X') THEN                                           00017900
            XMIN=H1                                                     00018000
            XMAX=H2                                                     00018100
      ENDIF                                                             00018200
      IF (PROG .EQ. 'Y') THEN                                           00018300
            YMIN=H1                                                     00018400
            YMAX=H2                                                     00018500
      ENDIF                                                             00018600
      GOTO 989                                                          00018700
C                                                                       00018800
C                                                                       00018900
  999 CONTINUE                                                          00019000
      XPMAX=XMAX                                                        00019100
      YPMAX=YMAX                                                        00019200
      XPMIN=XMIN                                                        00019300
      YPMIN=YMIN                                                        00019400
      H1=(XPMAX-XPMIN)/1500.D0                                          00019500
      H2=(YPMAX-YPMIN)/1500.D0                                          00019600
C                                                                       00019700
      CALL TWINDO (250*K1, 950*K1, 50*K1,750*K1)                        00019800
      CALL DWINDO (XPMIN,XPMAX,YPMIN,YPMAX)                             00019900
      CALL ERASE                                                        00020000
      CALL MOVEA (XPMIN,YPMIN)                                          00020100
      CALL DRAWA (XPMAX,YPMIN)                                          00020200
      CALL DRAWA (XPMAX,YPMAX)                                          00020300
      CALL DRAWA (XPMIN,YPMAX)                                          00020400
      CALL DRAWA (XPMIN,YPMIN)                                          00020500
      CALL MOVABS (0, 730*K1)                                           00020600
      CALL ANMODE                                                       00020700
  210 FORMAT (' XMIN =',F9.3)                                           00020800
  220 FORMAT (' XMAX =',F9.3)                                           00020900
  230 FORMAT (' YMIN =',F9.3)                                           00021000
  240 FORMAT (' YMAX =',F9.3)                                           00021100
      WRITE (6,*) 'ABSCISSA:'                                           00021200
      WRITE (6,210) XPMIN                                               00021300
      WRITE (6,220) XPMAX                                               00021400
      WRITE (6,*) ' '                                                   00021500
      WRITE (6,*) 'ORDINATE:'                                           00021600
      WRITE (6,230) YPMIN                                               00021700
      WRITE (6,240) YPMAX                                               00021800
      WRITE (6,*) ' '                                                   00021900
      WRITE (6,*) ' '                                                   00022000
      WRITE (6,*) 'WHEN THE CURSOR'                                     00022100
      WRITE (6,*) '   APPEARS:'                                         00022200
      WRITE (6,*) 'S:START AGAIN'                                       00022300
      WRITE (6,*) 'A:CHANGE AXES'                                       00022400
      WRITE (6,*) 'C:CHANGE SCALES'                                     00022500
      WRITE (6,*) 'E:TERMINATE'                                         00022600
      WRITE (6,*) ' '                                                   00022700
C     WRITE (6,*) 'WHEN THE BELL SOUNDS: YOU CAN DECIDE TO GENERATE A HA00022800
C    *RDCOPY'                                                           00022900
C                                                                       00023000
      DO 2100 J=1,NCOUNT                                                00023100
         XPP=XP(J)                                                      00023200
         YPP=YP(J)                                                      00023300
C                                                                       00023400
         CALL MOVEA (XPP,YPP)                                           00023500
         CALL DRAWA (XPP,YPP)                                           00023600
C        >>>> ' VERDICKEN ' DER PUNKTE                                  00023700
         CALL MOVEA (XPP+H1  ,YPP       )                               00023800
         CALL DRAWA (XPP     ,YPP+H2    )                               00023900
         CALL DRAWA (XPP-H1  ,YPP       )                               00024000
         CALL DRAWA (XPP     ,YPP-H2    )                               00024100
         CALL DRAWA (XPP+H1  ,YPP       )                               00024200
         IF (J .EQ. NCOUNT) CALL BELL                                   00024300
 2100 CONTINUE                                                          00024400
         CALL VCURSR (ICH,XDUMMY,YDUMMY)                                00024500
C        >>>> 1= 48             A = 65 /  97     P = 80 / 113           00024600
C        >>>> E= 69 / 101       S = 83 / 115     C = 67 /  99           00024700
         IF (ICH .EQ. 69 .OR. ICH .EQ. 101) THEN                        00024800
            CALL ERASE                                                  00024900
            GOTO 9999                                                   00025000
         END IF                                                         00025100
         IF (ICH .EQ. 65 .OR. ICH .EQ.  97) GOTO 988                    00025200
         IF (ICH .EQ. 67 .OR. ICH .EQ.  99) GOTO 666                    00025300
         IF (ICH .EQ. 83 .OR. ICH .EQ. 115) THEN                        00025400
            CALL ERASE                                                  00025500
            GOTO 555                                                    00025600
         END IF                                                         00025700
C                                                                       00025800
 9999 STOP                                                              00025900
      END                                                               00026000
C                                                                       00026100
C     ***************  SUBROUTINES ********************                 00026200
      DOUBLE PRECISION FUNCTION VORZEI(DI)                              00026300
      DOUBLE PRECISION DI                                               00026400
      IF (DI .EQ. 0.0D0) THEN                                           00026500
         VORZEI=0.D0                                                    00026600
      ELSE                                                              00026700
         VORZEI=1.0D0                                                   00026800
         IF (DI .LT. 0.0D0) VORZEI=-1.D0                                00026900
      ENDIF                                                             00027000
      RETURN                                                            00027100
      END                                                               00027200
C                                                                       00027300
C                                                                       00027400
      DOUBLE PRECISION FUNCTION POTABS(DI,POT)                          00027500
      DOUBLE PRECISION DI,POT                                           00027600
      POTABS=( DABS(DI) ) ** POT                                        00027700
      RETURN                                                            00027800
      END                                                               00027900
