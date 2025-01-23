C                                                                       00010000
C                                                                       00020000
C     ******************************************************************00030000
C     ******************************************************************00040000
C     ****                                                          ****00050000
C     ****            P I C  2         (GRAPHIC-PROGRAMM)           ****00060001
C     ****                                                          ****00070000
C     ******************************************************************00080000
C     ******************************************************************00090000
C                                                                       00100000
C                                                                       00110000
      INTEGER I,J,K1,K,AMPN,   AB,M,APR                                 00120001
      INTEGER ND,L                                                      00130001
      DOUBLE PRECISION XMIN,XMAX,YMIN,YMAX                              00140000
      DOUBLE PRECISION KMIN,KMAX,AMIN,AMAX,AMPMAX,AMPMIN,RMAX,RMIN      00150000
      DOUBLE PRECISION MMIN,MMAX,FIMIN,FIMAX,RSMAX,RSMIN                00160002
      DOUBLE PRECISION H1,H2,H3,H4,PI    ,T8 , GAMMA,DT,K0MIN,B         00170006
      DOUBLE PRECISION XPMAX,YPMAX,XPMIN,YPMIN                          00180000
      DOUBLE PRECISION X(1:30,0:1 00),Y(1:30,0:1 00)                    00190000
      DOUBLE PRECISION                    KAPPA(1:30,0:1 00)            00200003
      DOUBLE PRECISION R(1:30,0:1 00),FI(1:30,0:1 00)                   00210001
      DOUBLE PRECISION AMP(1:30,0:100 )                                 00220000
      DOUBLE PRECISION A(1:30,0:1 00)                                   00230000
      DOUBLE PRECISION XP(1:30,0:1 00),YP(1:30,0:1 00)                  00240000
      CHARACTER PROG*1,FN*30,SCR*2                                      00250000
C                                                                       00260000
C                                                                       00270000
C                                                                       00280000
      AMPN=25                                                           00290000
      PI=4.*DATAN(DBLE(1.))                                             00300000
      FN= 'D14K.TEST.DATA'                                              00310007
C     ***********        INPUT OF THE TERMINAL-NUMBER        ***********00320000
      CALL INITT (960)                                                  00330000
      CALL WRITEK (1,' ')                                               00340000
      CALL NEWLIN                                                       00350000
      CALL WRITEK (34,'WHICH TEKTRONIX-TERMINAL IS USED ?')             00360000
      CALL NEWLIN                                                       00370000
      CALL WRITEK (34,'**********************************')             00380000
      CALL NEWLIN                                                       00390000
      CALL WRITEK (1,' ')                                               00400000
      CALL NEWLIN                                                       00410000
      CALL WRITEK (34,'ENTER: DEFAULT  IF TEKTRONIX 4012 ')             00420000
      CALL NEWLIN                                                       00430000
      CALL WRITEK (34,'            T2  IF TEKTRONIX 4014 ')             00440000
      CALL NEWLIN                                                       00450000
      SCR='T1'                                                          00460000
      READ (5,'(A2)') SCR                                               00470000
      IF (SCR .NE. 'T2') THEN                                           00480000
         K1=1                                                           00490000
         CALL SETBUF (3)                                                00500000
         CALL TERM (1,1024)                                             00510000
         CALL ERASE                                                     00520000
         CALL WRITEK (1,' ')                                            00530000
         CALL WRITEK (33,'*********************************')           00540000
         CALL WRITEK (33,'*                               *')           00550000
         CALL WRITEK (33,'* WELCOME TO THE TEKTRONIX 4012 *')           00560000
         CALL WRITEK (33,'*                               *')           00570000
         CALL WRITEK (33,'*********************************')           00580000
      ELSE                                                              00590000
         K1=4                                                           00600000
         CALL SETBUF (3)                                                00610000
         CALL TERM (3,4096)                                             00620000
         CALL ERASE                                                     00630000
         CALL WRITEK (1,' ')                                            00640000
         CALL WRITEK (33,'*********************************')           00650000
         CALL WRITEK (33,'*                               *')           00660000
         CALL WRITEK (33,'* WELCOME TO THE TEKTRONIX 4014 *')           00670000
         CALL WRITEK (33,'*                               *')           00680000
         CALL WRITEK (33,'*********************************')           00690000
      END IF                                                            00700000
  555 CALL ANMODE                                                       00710007
      WRITE (6,*) ' '                                                   00720007
      WRITE (6,*) ' '                                                   00730000
      WRITE (6,*) ' '                                                   00740000
      WRITE (6,*) '       EINGABE DER DATEN-DATEI'                      00750000
      WRITE (6,*) '=========================================='          00760000
      WRITE (6,*) ' '                                                   00770000
      WRITE (6,*) ' '                                                   00780000
  137 FORMAT (' YOU CAN CHOOSE: DEFAULT   ',A30)                        00790000
      WRITE (6,137) FN                                                  00800000
      WRITE (6,*) ' '                                                   00810000
      WRITE (6,*) '                    (1)   D14K.TES1.DATA'            00820007
      WRITE (6,*) '                    (2)   D14K.TES2.DATA'            00830007
      WRITE (6,*) '                    (3)   D14K.TES3.DATA'            00840007
      WRITE (6,*) '                    (4)   D14K.TES4.DATA'            00850007
      WRITE (6,*) '                    (5)   D14K.TES5.DATA'            00860007
      WRITE (6,*) '                    (0)       .    .    '            00870007
      WRITE (6,*) ' '                                                   00880000
      PROG =' '                                                         00890000
      READ (5,'(A1)') PROG                                              00900000
      IF (PROG .EQ. '1') FN= 'D14K.TES1.DATA'                           00910007
      IF (PROG .EQ. '2') FN= 'D14K.TES2.DATA'                           00920007
      IF (PROG .EQ. '3') FN= 'D14K.TES3.DATA'                           00930007
      IF (PROG .EQ. '4') FN= 'D14K.TES4.DATA'                           00940007
      IF (PROG .EQ. '5') FN= 'D14K.TES5.DATA'                           00950007
      IF (PROG .EQ. '0') GOTO 853                                       00960000
      GOTO 666                                                          00970000
  853 WRITE (6,*) ' '                                                   00980000
      WRITE (6,*) ' NAME DER DATEN-DATEI :'                             00990000
      READ (5,'(A20)') FN                                               01000000
C                                                                       01010000
C     ***********  INPUT OF THE FILENAME INCLUDING THE DATA  ***********01020000
  666 CONTINUE                                                          01030000
C     CALL ERASE                                                        01040000
C     WRITE (6,*) ' '                                                   01050000
C     WRITE (6,*) 'WHEN THE CURSOR APPEARS:'                            01060000
C     WRITE (6,*) '========================'                            01070000
C     WRITE (6,*) 'ENTER: BLANK     TO KEEP  THE SCREEN'                01080000
C     WRITE (6,*) '       C         TO CLEAR THE SCREEN'                01090000
C     WRITE (6,*) '       E         TO TERMINATE THE PROGRAM'           01100000
C     WRITE (6,*) '       S         TO START THE PROGRAM AGAIN'         01110000
C     WRITE (6,*) '       A         TO PLOT ANOTHER CURVE-PARAMETER'    01120000
C     WRITE (6,*) '       P         TO CHANGE THE SCALE-PARAMETER'      01130000
C     WRITE (6,*) '       1,...,9   TO ADD THE CORRESPONDING CURVE'     01140000
C     WRITE (6,*) '       *         TO ADD THE LAST CURVE'              01150000
C     WRITE (6,*) ' '                                                   01160000
C     WRITE (6,*) 'WHEN THE BELL SOUNDS: YOU CAN DECIDE TO GENERATE A HA01170000
C    *RDCOPY'                                                           01180000
C     WRITE (6,*) '====================='                               01190000
C                                                                       01200000
C                                                                       01210000
C                                                                       01220000
C     ***********  INPUT OF THE DATA ***********                        01230000
  777 OPEN(21,FILE=FN)                                                  01240000
      REWIND 21                                                         01250000
  200 FORMAT (1X,7X,E23.16)                                             01260001
  201 FORMAT (1X,7X,I6)                                                 01270001
  202 FORMAT (1X,3X,I3,1X,3X,E23.16)                                    01280001
  209 FORMAT (1X,I6,1X,E23.16,1X,E23.16)                                01290001
      READ (21,201) L                                                   01300001
      READ (21,200) GAMMA                                               01310001
      READ (21,200) B                                                   01320001
      READ (21,200) K0MIN                                               01330001
      READ (21,201) ND                                                  01340001
      READ (21,200) DT                                                  01350006
      AB=0                                                              01360000
      DO 1100 I=1,30,1                                                  01370000
         READ (21,202) APR,T8                                           01380001
         IF (APR .EQ. 0) GOTO 1101                                      01390000
         AB=AB+1                                                        01400000
         DO 1000 J=0,ND  ,1                                             01410001
            READ (21,209) K,R(I,J),FI(I,J)                              01420001
 1000    CONTINUE                                                       01430000
 1100 CONTINUE                                                          01440000
 1101 CLOSE(21,STATUS='KEEP')                                           01450000
      DO 1060 I=1,AB                                                    01460001
         DO 1060 J=0,ND                                                 01470002
            X(I,J)=R(I,J)*DCOS(FI(I,J))                                 01480001
            Y(I,J)=R(I,J)*DSIN(FI(I,J))                                 01490001
            KAPPA(I,J)=X(I,J)                                           01500004
            A    (I,J)=Y(I,J)                                           01510004
 1060 CONTINUE                                                          01520001
C                                                                       01530000
      XMIN=-2.                                                          01540001
      XMAX=2.                                                           01550001
      YMIN=XMIN                                                         01560001
      YMAX=XMAX                                                         01570001
      KMAX=2.                                                           01580001
      KMIN=-1.                                                          01590001
      RMIN=0.                                                           01600001
      RMAX=2.                                                           01610001
      AMAX=5.                                                           01620001
      AMIN=0.                                                           01630001
      DO 1030 I=1,AB,1                                                  01640000
         DO 1040 K=0,AMPN,1                                             01650000
           AMP(I,K)=0.                                                  01660000
           H1=K                                                         01670000
           DO 1050 J=0,ND  -1,1                                         01680005
              H2= R(I,J)*DCOS(H1*   FI(I,J))*.5                         01690001
              H3= R(I,J+1)*DCOS(H1*   FI(I,J+1))*.5                     01700001
              AMP(I,K)=AMP(I,K)+(H2+H3)*(   FI(I,J+1)-   FI(I,J))       01710001
 1050      CONTINUE                                                     01720000
           AMP(I,K)=AMP(I,K)/PI                                         01730000
 1040    CONTINUE                                                       01740000
         AMP(I,0)=AMP(I,0)*.5                                           01750000
 1030 CONTINUE                                                          01760000
      AMPMIN=-1.                                                        01770001
      AMPMAX=2.                                                         01780001
C                                                                       01790000
      RSMIN=0.                                                          01800000
      RSMAX=1.                                                          01810000
      FIMIN=0.                                                          01820001
      FIMAX=2.*PI                                                       01830001
      MMIN=-.5                                                          01840000
      MMAX=16.5                                                         01850000
C                                                                       01860000
  988 CALL ERASE                                                        01870000
      CALL ANMODE                                                       01880007
      WRITE (6,*) ' '                                                   01890000
      WRITE (6,*) ' '                                                   01900000
      WRITE (6,*) ' '                                                   01910000
      WRITE (6,*) '         MAX. AND MIN. VALUES      '                 01920000
      WRITE (6,*) '=========================================='          01930000
      WRITE (6,*) ' '                                                   01940000
  251 FORMAT ('  (B)  BOUNDARY      :   XMIN=',F10.5,'    XMAX='F10.5)  01950000
  252 FORMAT ('                     :   YMIN=',F10.5,'    YMAX='F10.5)  01960000
  253 FORMAT ('  (K)  CURVATURE     :  RSMIN=',F10.5,'   RSMAX='F10.5)  01970000
  254 FORMAT ('                     :   KMIN=',F10.5,'    KMAX='F10.5)  01980000
  255 FORMAT ('  (A)  CONCENTRATION :  RSMIN=',F10.5,'   RSMAX='F10.5)  01990000
  256 FORMAT ('                     :   AMIN=',F10.5,'    AMAX='F10.5)  02000000
  257 FORMAT ('  (R)  RADIUS(WINKEL):  FIMIN=',F10.5,'   FIMAX='F10.5)  02010006
  258 FORMAT ('                     :   RMIN=',F10.5,'    RMAX='F10.5)  02020000
  259 FORMAT ('  (M)  MODEN-AMPLIT. :   MMIN=',F10.5,'    MMAX='F10.5)  02030000
  261 FORMAT ('                     : AMPMIN=',F10.5,'  AMPMAX='F10.5)  02040000
      WRITE (6,251) XMIN,XMAX                                           02050000
      WRITE (6,252) YMIN,YMAX                                           02060000
      WRITE (6,253) RSMIN,RSMAX                                         02070000
      WRITE (6,254) KMIN,KMAX                                           02080000
      WRITE (6,255) RSMIN,RSMAX                                         02090000
      WRITE (6,256) AMIN,AMAX                                           02100000
      WRITE (6,257) FIMIN,FIMAX                                         02110002
      WRITE (6,258) RMIN,RMAX                                           02120000
      WRITE (6,259) MMIN,MMAX                                           02130000
      WRITE (6,261) AMPMIN,AMPMAX                                       02140000
      WRITE (6,*) ' '                                                   02150000
      WRITE (6,*) ' '                                                   02160000
      WRITE (6,*) 'CHANGE SCALE-PARAMETER ?  (DEFAULT = NO CHANGE)'     02170000
      WRITE (6,*) ' '                                                   02180000
      PROG=' '                                                          02190000
      READ (5,'(A1)') PROG                                              02200000
      IF (PROG .EQ. ' ') GOTO 999                                       02210000
      WRITE (6,*) 'VIER WERTE (DURCH KOMMATA GETRENNT)'                 02220000
      READ (5,*) H1,H2,H3,H4                                            02230000
      IF (PROG .EQ. 'B') THEN                                           02240000
            XMIN=H1                                                     02250000
            XMAX=H2                                                     02260000
            YMIN=H3                                                     02270000
            YMAX=H4                                                     02280000
      ENDIF                                                             02290000
      IF (PROG .EQ. 'K') THEN                                           02300000
            KMIN=H3                                                     02310000
            KMAX=H4                                                     02320000
           RSMIN=H1                                                     02330000
           RSMAX=H2                                                     02340000
      ENDIF                                                             02350000
      IF (PROG .EQ. 'A') THEN                                           02360000
            AMIN=H3                                                     02370000
            AMAX=H4                                                     02380000
           RSMIN=H1                                                     02390000
           RSMAX=H2                                                     02400000
      ENDIF                                                             02410000
      IF (PROG .EQ. 'R') THEN                                           02420000
            RMIN=H3                                                     02430000
            RMAX=H4                                                     02440000
           RSMIN=H1                                                     02450000
           RSMAX=H2                                                     02460000
      ENDIF                                                             02470000
      IF (PROG .EQ. 'M') THEN                                           02480000
          AMPMIN=H3                                                     02490000
          AMPMAX=H4                                                     02500000
            MMIN=H1                                                     02510000
            MMAX=H2                                                     02520000
      ENDIF                                                             02530000
      GOTO 988                                                          02540000
C                                                                       02550000
  999 CALL ERASE                                                        02560000
      CALL ANMODE                                                       02570007
      PROG=' '                                                          02580000
      WRITE (6,*) ' '                                                   02590000
      WRITE (6,*) ' '                                                   02600000
      WRITE (6,*) ' '                                                   02610000
      WRITE (6,*) ' '                                                   02620000
      WRITE (6,*) '           AUSWAHL DER KURVEN      '                 02630000
      WRITE (6,*) '=========================================='          02640000
      WRITE (6,*) ' '                                                   02650000
      WRITE (6,*) ' '                                                   02660000
      WRITE (6,*) ' '                                                   02670000
      WRITE (6,*) 'YOU CAN CHOOSE: DEFAULT   BOUNDARY     '             02680000
      WRITE (6,*) '                    (K)   CURVATURE    '             02690000
      WRITE (6,*) '                    (A)   CONCENTRATION'             02700000
      WRITE (6,*) '                    (R)   RADIUS(WINKEL)'            02710000
      WRITE (6,*) '                    (M)   MODEN-SELEKTION'           02720000
      WRITE (6,*) '                                       '             02730000
      WRITE (6,*) ' '                                                   02740000
      WRITE (6,*) ' '                                                   02750000
      READ (5,'(A1)') PROG                                              02760000
      DO 742 I=1,AB,1                                                   02770000
         DO 744 J=0, ND  , 1                                            02780005
            XP(I,J)=X(I,J)                                              02790000
            YP(I,J)=Y(I,J)                                              02800000
            IF (PROG .EQ. 'K')      XP(I,J)=   FI(I,J)                  02810003
            IF (PROG .EQ. 'K')      YP(I,J)=KAPPA(I,J)                  02820000
            IF (PROG .EQ. 'A')      XP(I,J)=   FI(I,J)                  02830003
            IF (PROG .EQ. 'A')      YP(I,J)=A    (I,J)                  02840000
            IF (PROG .EQ. 'R')      YP(I,J)=R    (I,J)                  02850000
            IF (PROG .EQ. 'R')      XP(I,J)=   FI(I,J)                  02860001
            IF (PROG .EQ. 'M')      XP(I,J)=J                           02870000
            IF (PROG .EQ. 'M')      YP(I,J)=AMP(I,J)                    02880000
  744    CONTINUE                                                       02890000
  742 CONTINUE                                                          02900000
      XPMAX=XMAX                                                        02910000
      YPMAX=YMAX                                                        02920000
      XPMIN=XMIN                                                        02930000
      YPMIN=YMIN                                                        02940000
      IF (PROG .EQ. 'K') THEN                                           02950000
         XPMAX=RSMAX                                                    02960000
         XPMIN=RSMIN                                                    02970000
         YPMAX=KMAX                                                     02980000
         YPMIN=KMIN                                                     02990000
      ENDIF                                                             03000000
      IF (PROG .EQ. 'A') THEN                                           03010000
         XPMAX=RSMAX                                                    03020000
         XPMIN=RSMIN                                                    03030000
         YPMAX=AMAX                                                     03040000
         YPMIN=AMIN                                                     03050000
      ENDIF                                                             03060000
      IF (PROG .EQ. 'R') THEN                                           03070000
         XPMAX=FIMAX                                                    03080001
         XPMIN=FIMIN                                                    03090001
         YPMAX=RMAX                                                     03100000
         YPMIN=RMIN                                                     03110000
      ENDIF                                                             03120000
      IF (PROG .EQ. 'M') THEN                                           03130000
         XPMAX=MMAX                                                     03140000
         XPMIN=MMIN                                                     03150000
         YPMAX=AMPMAX                                                   03160000
         YPMIN=AMPMIN                                                   03170000
      ENDIF                                                             03180000
C                                                                       03190000
      CALL TWINDO (250*K1, 950*K1, 50*K1,750*K1)                        03200000
      CALL DWINDO (XPMIN,XPMAX,YPMIN,YPMAX)                             03210000
      CALL ERASE                                                        03220000
      CALL MOVEA (XPMIN,YPMIN)                                          03230000
      CALL DRAWA (XPMAX,YPMIN)                                          03240000
      CALL DRAWA (XPMAX,YPMAX)                                          03250000
      CALL DRAWA (XPMIN,YPMAX)                                          03260000
      CALL DRAWA (XPMIN,YPMIN)                                          03270000
      CALL MOVABS (0, 730*K1)                                           03280000
      CALL ANMODE                                                       03290000
  210 FORMAT (' XMIN =',F8.2)                                           03300000
  220 FORMAT (' XMAX =',F8.2)                                           03310000
  230 FORMAT (' YMIN =',F8.2)                                           03320000
  240 FORMAT (' YMAX =',F8.2)                                           03330000
      WRITE (6,*) 'ABSCISSA:'                                           03340000
      WRITE (6,210) XPMIN                                               03350000
      WRITE (6,220) XPMAX                                               03360000
      WRITE (6,*) ' '                                                   03370000
      WRITE (6,*) 'ORDINATE:'                                           03380000
      WRITE (6,230) YPMIN                                               03390000
      WRITE (6,240) YPMAX                                               03400000
      DO 2100 M=1,AB,1                                                  03410000
         J=M                                                            03420000
 1900    CALL MOVEA (XP(J,0),YP(J,0))                                   03430000
         DO 2000 I=1,ND    ,1                                           03440005
            CALL DRAWA (XP(J,I),YP(J,I))                                03450000
 2000    CONTINUE                                                       03460000
         IF (M .EQ. AB) CALL BELL                                       03470000
         CALL VCURSR (ICH,XDUMMY,YDUMMY)                                03480000
         IF (ICH .GT. 48 .AND. ICH .LT. 58 .OR. ICH .EQ. 42) THEN       03490000
            IF (ICH .EQ. 42) THEN                                       03500000
               J=AB                                                     03510000
            ELSE                                                        03520000
               J=ICH-48                                                 03530000
            END IF                                                      03540000
            GOTO 1900                                                   03550000
         END IF                                                         03560000
         IF (ICH .EQ. 67 .OR. ICH .EQ. 99) THEN                         03570000
            CALL ERASE                                                  03580000
            CALL MOVEA (XPMIN,YPMIN)                                    03590000
            CALL DRAWA (XPMAX,YPMIN)                                    03600000
            CALL DRAWA (XPMAX,YPMAX)                                    03610000
            CALL DRAWA (XPMIN,YPMAX)                                    03620000
            CALL DRAWA (XPMIN,YPMIN)                                    03630000
            CALL MOVABS (0, 730*K1)                                     03640000
            CALL ANMODE                                                 03650000
            WRITE (6,*) 'ABSCISSA:'                                     03660000
            WRITE (6,210) XPMIN                                         03670000
            WRITE (6,220) XPMAX                                         03680000
            WRITE (6,*) ' '                                             03690000
            WRITE (6,*) 'ORDINATE:'                                     03700000
            WRITE (6,230) YPMIN                                         03710000
            WRITE (6,240) YPMAX                                         03720000
         END IF                                                         03730000
         IF (ICH .EQ. 69 .OR. ICH .EQ. 101) THEN                        03740000
            CALL ERASE                                                  03750000
            GOTO 9999                                                   03760000
         END IF                                                         03770000
         IF (ICH .EQ. 65 .OR. ICH .EQ. 97) GOTO 999                     03780000
         IF (ICH .EQ. 80 .OR. ICH .EQ. 112) GOTO 988                    03790000
         IF (ICH .EQ. 83 .OR. ICH .EQ. 115) THEN                        03800000
            CALL ERASE                                                  03810000
            GOTO 555                                                    03820000
         END IF                                                         03830000
 2100 CONTINUE                                                          03840000
C                                                                       03850000
C                                                                       03860000
 9999 STOP                                                              03870000
      END                                                               03880000
