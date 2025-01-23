C                                                                       00000010
C                                                                       00000020
C     ******************************************************************00000030
C     ******************************************************************00000040
C     ****                                                          ****00000050
C     ****      MA86(PIC2SC) (GRAPHIC-PROGRAMM ZU MA86(D2SCHL))     ****00000060
C     ****                                                          ****00000070
C     ******************************************************************00000080
C     ******************************************************************00000090
C                                                                       00000100
C                                                                       00000110
      INTEGER K1                                                        00000120
      INTEGER I,J,X9,Y9,N ,COUNT,L                                      00000130
      DOUBLE PRECISION XMIN,XMAX,YMIN,YMAX,X0,Y0,VL,PS                  00000140
      DOUBLE PRECISION H1,H2,H3,H4,PI,T, GAMMA,C,RHOA,DELT              00000150
      DOUBLE PRECISION RHON(  400),GPOT(  400),VX(  400),VY(  400)      00000160
      DOUBLE PRECISION X(0:21),Y(0:21),LX                               00000170
      DOUBLE PRECISION BO(21),BOPKT(21), BU(21),BUPKT(21)               00000180
      CHARACTER PROG*1,FN*30,SCR*2,CNT*3                                00000190
C                                                                       00000200
C                                                                       00000210
C                                                                       00000220
      PI=4.*DATAN(1.0D+00)                                              00000230
      FN= 'D14K.TEST.DATA'                                              00000240
C     ***********        INPUT OF THE TERMINAL-NUMBER        ***********00000250
      CALL INITT (960)                                                  00000260
      CALL WRITEK (1,' ')                                               00000270
      CALL NEWLIN                                                       00000280
      CALL WRITEK (34,'WHICH TEKTRONIX-TERMINAL IS USED ?')             00000290
      CALL NEWLIN                                                       00000300
      CALL WRITEK (34,'**********************************')             00000310
      CALL NEWLIN                                                       00000320
      CALL WRITEK (1,' ')                                               00000330
      CALL NEWLIN                                                       00000340
      CALL WRITEK (34,'ENTER: DEFAULT  IF TEKTRONIX 4012 ')             00000350
      CALL NEWLIN                                                       00000360
      CALL WRITEK (34,'            T2  IF TEKTRONIX 4014 ')             00000370
      CALL NEWLIN                                                       00000380
      SCR='T1'                                                          00000390
      READ (5,'(A2)') SCR                                               00000400
  555 IF (SCR .NE. 'T2') THEN                                           00000410
         K1=1                                                           00000420
         CALL SETBUF (3)                                                00000430
         CALL TERM (1,1024)                                             00000440
         CALL ERASE                                                     00000450
         CALL WRITEK (1,' ')                                            00000460
         CALL WRITEK (33,'*********************************')           00000470
         CALL WRITEK (33,'*                               *')           00000480
         CALL WRITEK (33,'* WELCOME TO THE TEKTRONIX 4012 *')           00000490
         CALL WRITEK (33,'*                               *')           00000500
         CALL WRITEK (33,'*********************************')           00000510
      ELSE                                                              00000520
         K1=4                                                           00000530
         CALL SETBUF (3)                                                00000540
         CALL TERM (3,4096)                                             00000550
         CALL ERASE                                                     00000560
         CALL WRITEK (1,' ')                                            00000570
         CALL WRITEK (33,'*********************************')           00000580
         CALL WRITEK (33,'*                               *')           00000590
         CALL WRITEK (33,'* WELCOME TO THE TEKTRONIX 4014 *')           00000600
         CALL WRITEK (33,'*                               *')           00000610
         CALL WRITEK (33,'*********************************')           00000620
      END IF                                                            00000630
C                                                                       00000640
C     >>>>>>>>>>> INPUT OF DATA-SET-NAME AND OPEN DATA-SET <<<<<<<      00000650
      WRITE (6,*) ' '                                                   00000660
      WRITE (6,*) ' '                                                   00000670
      WRITE (6,*) ' '                                                   00000680
      WRITE (6,*) '       EINGABE DER DATEN-DATEI'                      00000690
      WRITE (6,*) '=========================================='          00000700
      WRITE (6,*) ' '                                                   00000710
      WRITE (6,*) ' '                                                   00000720
  211 FORMAT (' YOU CAN CHOOSE: DEFAULT   ',A30)                        00000730
      WRITE (6,211) FN                                                  00000740
      WRITE (6,*) ' '                                                   00000750
      WRITE (6,*) '                    (1)   D14K.TES1.DATA'            00000760
      WRITE (6,*) '                    (2)   D14K.TES2.DATA'            00000770
      WRITE (6,*) '                    (3)   D14K.TES3.DATA'            00000780
      WRITE (6,*) '                    (4)   D14K.TES4.DATA'            00000790
      WRITE (6,*) '                    (5)   D14K.TES5.DATA'            00000800
      WRITE (6,*) '                    (6)   D14K.TES6.DATA'            00000810
      WRITE (6,*) '                    (0)   ****.****.****'            00000820
      WRITE (6,*) ' '                                                   00000830
      PROG =' '                                                         00000840
      READ (5,'(A1)') PROG                                              00000850
      IF (PROG .EQ. '1') FN= 'D14K.TES1.DATA'                           00000860
      IF (PROG .EQ. '2') FN= 'D14K.TES2.DATA'                           00000870
      IF (PROG .EQ. '3') FN= 'D14K.TES3.DATA'                           00000880
      IF (PROG .EQ. '4') FN= 'D14K.TES4.DATA'                           00000890
      IF (PROG .EQ. '5') FN= 'D14K.TES5.DATA'                           00000900
      IF (PROG .EQ. '6') FN= 'D14K.TES6.DATA'                           00000910
      IF (PROG .EQ. '0') GOTO 853                                       00000920
      GOTO 666                                                          00000930
  853 WRITE (6,*) ' '                                                   00000940
      WRITE (6,*) ' NAME DER DATEN-DATEI :'                             00000950
      READ (5,'(A20)') FN                                               00000960
  666 OPEN(21,FILE=FN)                                                  00000970
C                                                                       00000980
C                                                                       00000990
C                                                                       00001000
C                                                                       00001010
C     ***********  BILDSCHIRMPARAMETER *********                        00001020
C                                                                       00001030
      XMIN= -   5.10D0                                                  00001040
      XMAX=     5.10D0                                                  00001050
      YMIN= -   5.10D0                                                  00001060
      YMAX=     5.10D0                                                  00001070
      VL  =    75.0000D0                                                00001080
      PS  =      .0200D0                                                00001090
C                                                                       00001100
  988 CALL ERASE                                                        00001110
      WRITE (6,*) ' '                                                   00001120
      WRITE (6,*) ' '                                                   00001130
      WRITE (6,*) ' '                                                   00001140
      WRITE (6,*) '         MAX. AND MIN. VALUES      '                 00001150
      WRITE (6,*) '=========================================='          00001160
      WRITE (6,*) ' '                                                   00001170
  221 FORMAT ('  (B)  BOUNDARY        :  XMIN=',F10.5,'    XMAX='F10.5) 00001180
  222 FORMAT ('                       :  YMIN=',F10.5,'    YMAX='F10.5) 00001190
  223 FORMAT ('  (L)  LENGTH OF V-VEK.:    VL=',F10.5)                  00001200
  224 FORMAT ('  (P)  POINT SIZE      :    PS=',F10.5)                  00001210
      WRITE (6,221) XMIN,XMAX                                           00001220
      WRITE (6,222) YMIN,YMAX                                           00001230
      WRITE (6,223) VL                                                  00001240
      WRITE (6,224) PS                                                  00001250
      WRITE (6,*) ' '                                                   00001260
      WRITE (6,*) ' '                                                   00001270
      WRITE (6,*) 'CHANGE SCALE-PARAMETER ?  (DEFAULT = NO CHANGE)'     00001280
      WRITE (6,*) ' '                                                   00001290
      PROG=' '                                                          00001300
      READ (5,'(A1)') PROG                                              00001310
      IF (PROG .EQ. ' ') GOTO 999                                       00001320
      IF (PROG .EQ. 'B') THEN                                           00001330
            WRITE (6,*) 'VIER WERTE (DURCH KOMMATA GETRENNT)'           00001340
            READ (5,*) H1,H2,H3,H4                                      00001350
            XMIN=H1                                                     00001360
            XMAX=H2                                                     00001370
            YMIN=H3                                                     00001380
            YMAX=H4                                                     00001390
      ENDIF                                                             00001400
      IF (PROG .EQ. 'L') THEN                                           00001410
            WRITE (6,*) 'EIN WERT'                                      00001420
            READ (5,*) H1                                               00001430
            VL  =H1                                                     00001440
      ENDIF                                                             00001450
      IF (PROG .EQ. 'P') THEN                                           00001460
            WRITE (6,*) 'EIN WERT'                                      00001470
            READ (5,*) H1                                               00001480
            PS  =H1                                                     00001490
      ENDIF                                                             00001500
      GOTO 988                                                          00001510
C                                                                       00001520
C     >>>>>>>>>> INPUT DATA AND PLOT DATA <<<<<<<<<<                    00001530
  999 CONTINUE                                                          00001540
      REWIND 21                                                         00001550
   30 FORMAT (1X,A3)                                                    00001560
   31 FORMAT (1X,8X,D23.16,2(10X,D23.16))                               00001570
   32 FORMAT (1X,8X,I3,30X,I3,30X,D23.16)                               00001580
   35 FORMAT (1X,I3,4(2X,D23.16))                                       00001590
   40 FORMAT (1X,8X,I8,15X,2(10X,D23.16))                               00001600
   41 FORMAT (1X,I3,2X,I3,4(2X,D23.16))                                 00001610
   42 FORMAT (1X,I3,2(2X,D23.16))                                       00001620
      READ (21, 30) CNT                                                 00001630
      READ (21, 31) GAMMA,RHOA,C                                        00001640
      READ (21, 32) X9,Y9,LX                                            00001650
      X9=X9+1                                                           00001660
      Y9=Y9+2                                                           00001670
      DO 881 J=1,Y9                                                     00001680
         READ (21, 35) N,Y(J),H1                                        00001690
  881 CONTINUE                                                          00001700
      DO 882 I=1,X9                                                     00001710
         READ (21, 35) N,X(I),H1                                        00001720
  882 CONTINUE                                                          00001730
C                                                                       00001740
C                                                                       00001750
 1991 READ (21,40) COUNT,T,DELT                                         00001760
      DO 1855 I=1,X9*Y9                                                 00001770
         READ (21,41) N,N,RHON(I),GPOT(I),VX(I),VY(I)                   00001780
 1855 CONTINUE                                                          00001790
      DO 1856 I=1,X9                                                    00001800
         READ (21,42) N,BU(I),BUPKT(I)                                  00001810
 1856 CONTINUE                                                          00001820
      DO 1857 I=1,X9                                                    00001830
         READ (21,42) N,BO(I),BOPKT(I)                                  00001840
 1857 CONTINUE                                                          00001850
C                                                                       00001860
      CALL TWINDO (250*K1, 950*K1, 50*K1,750*K1)                        00001870
      CALL DWINDO (XMIN,XMAX,YMIN,YMAX)                                 00001880
      CALL ERASE                                                        00001890
      CALL MOVEA (XMIN,YMIN)                                            00001900
      CALL DRAWA (XMAX,YMIN)                                            00001910
      CALL DRAWA (XMAX,YMAX)                                            00001920
      CALL DRAWA (XMIN,YMAX)                                            00001930
      CALL DRAWA (XMIN,YMIN)                                            00001940
      CALL MOVABS (0, 730*K1)                                           00001950
      CALL ANMODE                                                       00001960
  231 FORMAT ('  XMIN =',F8.2)                                          00001970
  232 FORMAT ('  XMAX =',F8.2)                                          00001980
  233 FORMAT ('  YMIN =',F8.2)                                          00001990
  234 FORMAT ('  YMAX =',F8.2)                                          00002000
  251 FORMAT ('     C =',F8.2)                                          00002010
  252 FORMAT (' GAMMA =',F8.4)                                          00002020
  253 FORMAT ('  RHOA =',F8.2)                                          00002030
  261 FORMAT ('     T =',F8.2)                                          00002040
  262 FORMAT ('  DELT =',F8.4)                                          00002050
      WRITE (6,*) 'ABSCISSA:'                                           00002060
      WRITE (6,231) XMIN                                                00002070
      WRITE (6,232) XMAX                                                00002080
      WRITE (6,*) ' '                                                   00002090
      WRITE (6,*) 'ORDINATE:'                                           00002100
      WRITE (6,233) YMIN                                                00002110
      WRITE (6,234) YMAX                                                00002120
      WRITE (6,*) ' '                                                   00002130
      WRITE (6,251) C                                                   00002140
      WRITE (6,252) GAMMA                                               00002150
      WRITE (6,253) RHOA                                                00002160
      WRITE (6,*) ' '                                                   00002170
      WRITE (6,261) T                                                   00002180
      WRITE (6,262) DELT                                                00002190
C                                                                       00002200
      X0=0.D0                                                           00002210
      Y0=0.D0                                                           00002220
      H3=1.D0                                                           00002230
      CALL MOVEA (X0+X(1)*H3,Y0+ BU(1)*H3)                              00002240
      DO 1858 I=2,X9                                                    00002250
         CALL DRAWA (X0+X(I)*H3,Y0+BU(I)*H3)                            00002260
 1858 CONTINUE                                                          00002270
      CALL MOVEA (X0+X(1)*H3,Y0+ BO(1)*H3)                              00002280
      DO 1859 I=2,X9                                                    00002290
         CALL DRAWA (X0+X(I)*H3,Y0+BO(I)*H3)                            00002300
 1859 CONTINUE                                                          00002310
C                                                                       00002320
      DO 1921 I=1,X9                                                    00002330
      DO 1921 J=1,Y9                                                    00002340
         N=(I-1)*Y9+J                                                   00002350
         H1=X(I)                                                        00002360
         H2=( Y(J)*(BO(I)-BU(I)) +BO(I) +BU(I) ) *.5D0                  00002370
         CALL MOVEA (X0+H3*H1,Y0+H3*H2)                                 00002380
         CALL DRAWA (X0+H3*H1,Y0+H3*H2)                                 00002390
         CALL MOVEA (X0+H3*H1+PS,Y0+H3*H2)                              00002400
         DO 1923 L=0,40 ,10                                             00002410
            H4=L                                                        00002420
C           CALL DRAWA (X0+H3*H1,Y0+H3*H2)                              00002430
            CALL DRAWA (X0+H3*H1+DCOS(PI*H4/20.)*PS,                    00002440
     *                  Y0+H3*H2+DSIN(PI*H4/20.)*PS)                    00002450
 1923    CONTINUE                                                       00002460
         CALL MOVEA (X0+H3*H1,Y0+H3*H2)                                 00002470
         CALL DRAWA (X0+H3*H1+VX(N)*VL,Y0+H3*H2+VY(N)*VL)               00002480
 1921 CONTINUE                                                          00002490
C                                                                       00002500
         CALL VCURSR (ICH,XDUMMY,YDUMMY)                                00002510
C           A=65 B=66 C=67 .......                                      00002520
C           L=76 M=77 N=78 .......                                      00002530
C         N (78) UND DEFAULT: NAECHSTE KURVE                            00002540
C         P (80): BILDSCHIRMPARAMETER AENDERN                           00002550
C         D (68): ANDERER DATENSATZ                                     00002560
C         A (65): SELBER DATENSATZ NOCHMALS VON VORNE                   00002570
C         E (69): ENDE                                                  00002580
C                                                                       00002590
C                                                                       00002600
         IF (ICH .EQ. 65 .OR. ICH .EQ. 97) GOTO 999                     00002610
         IF (ICH .EQ. 69 .OR. ICH .EQ. 101) GOTO 9999                   00002620
         IF (ICH .EQ. 68 .OR. ICH .EQ. 100) GOTO 555                    00002630
         IF (ICH .EQ. 80 .OR. ICH .EQ. 112) GOTO 988                    00002640
         IF (ICH .EQ. 78 .OR. ICH .EQ. 110) GOTO 1991                   00002650
         GOTO 1991                                                      00002660
C                                                                       00002670
C                                                                       00002680
 9999 STOP                                                              00002690
      END                                                               00002700
