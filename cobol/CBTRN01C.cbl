      ******************************************************************
      * Program     : CBTRN01C.CBL                                      
      * Application : CardDemo                                          
      * Type        : BATCH COBOL Program                                
      * Function    : Post the records from daily transaction file.     
      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.                   
      * All Rights Reserved.                                            
      *                                                                 
      * Licensed under the Apache License, Version 2.0 (the "License"). 
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at                         
      *                                                                 
      *    http://www.apache.org/licenses/LICENSE-2.0                   
      *                                                                 
      * Unless required by applicable law or agreed to in writing,      
      * software distributed under the License is distributed on an     
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    
      * either express or implied. See the License for the specific     
      * language governing permissions and limitations under the License
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBTRN01C.
       AUTHOR.        AWS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DALYTRAN-FILE ASSIGN TO DALYTRAN
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  FILE STATUS  IS DALYTRAN-STATUS.

           SELECT CUSTOMER-FILE ASSIGN TO   CUSTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS FD-CUST-ID
                  FILE STATUS  IS CUSTFILE-STATUS.

           SELECT XREF-FILE ASSIGN TO   XREFFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS FD-XREF-CARD-NUM
                  FILE STATUS  IS XREFFILE-STATUS.

           SELECT CARD-FILE ASSIGN TO   CARDFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS FD-CARD-NUM
                  FILE STATUS  IS CARDFILE-STATUS.

           SELECT ACCOUNT-FILE ASSIGN TO   ACCTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS FD-ACCT-ID
                  FILE STATUS  IS ACCTFILE-STATUS.

           SELECT TRANSACT-FILE ASSIGN TO   TRANFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS FD-TRANS-ID
                  FILE STATUS  IS TRANFILE-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  DALYTRAN-FILE.
       01  FD-TRAN-RECORD.
           05 FD-TRAN-ID                        PIC X(16).
           05 FD-CUST-DATA                      PIC X(334).

       FD  CUSTOMER-FILE.
       01  FD-CUSTFILE-REC.
           05 FD-CUST-ID                        PIC 9(09).
           05 FD-CUST-DATA                      PIC X(491).

       FD  XREF-FILE.
       01  FD-XREFFILE-REC.
           05 FD-XREF-CARD-NUM                  PIC X(16).
           05 FD-XREF-DATA                      PIC X(34).

       FD  CARD-FILE.
       01  FD-CARDFILE-REC.
           05 FD-CARD-NUM                       PIC X(16).
           05 FD-CARD-DATA                      PIC X(134).

       FD  ACCOUNT-FILE.
       01  FD-ACCTFILE-REC.
           05 FD-ACCT-ID                        PIC 9(11).
           05 FD-ACCT-DATA                      PIC X(289).

       FD  TRANSACT-FILE.
       01  FD-TRANFILE-REC.
           05 FD-TRANS-ID                       PIC X(16).
           05 FD-ACCT-DATA                      PIC X(334).

       WORKING-STORAGE SECTION.

      *****************************************************************

      *COPY CVTRA06Y.
      *** >>> Automated Expansion of CPY: [CVTRA06Y]
      *****************************************************************         
      *    Data-structure for DALYTRANsaction record (RECLN = 350)              
      *****************************************************************         
       01  DALYTRAN-RECORD.                                                     
           05  DALYTRAN-ID                             PIC X(16).               
           05  DALYTRAN-TYPE-CD                        PIC X(02).               
           05  DALYTRAN-CAT-CD                         PIC 9(04).               
           05  DALYTRAN-SOURCE                         PIC X(10).               
           05  DALYTRAN-DESC                           PIC X(100).              
           05  DALYTRAN-AMT                            PIC S9(09)V99.           
           05  DALYTRAN-MERCHANT-ID                    PIC 9(09).               
           05  DALYTRAN-MERCHANT-NAME                  PIC X(50).               
           05  DALYTRAN-MERCHANT-CITY                  PIC X(50).               
           05  DALYTRAN-MERCHANT-ZIP                   PIC X(10).               
           05  DALYTRAN-CARD-NUM                       PIC X(16).               
           05  DALYTRAN-ORIG-TS                        PIC X(26).               
           05  DALYTRAN-PROC-TS                        PIC X(26).               
           05  FILLER                                  PIC X(20).       
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:01 CDT
      *

       01  DALYTRAN-STATUS.
           05  DALYTRAN-STAT1      PIC X.
           05  DALYTRAN-STAT2      PIC X.


      *COPY CVCUS01Y.
      *** >>> Automated Expansion of CPY: [CVCUS01Y]
      *****************************************************************
      *    Data-structure for Customer entity (RECLN 500)
      *****************************************************************
       01  CUSTOMER-RECORD.
           05  CUST-ID                                 PIC 9(09).
           05  CUST-FIRST-NAME                         PIC X(25).
           05  CUST-MIDDLE-NAME                        PIC X(25).
           05  CUST-LAST-NAME                          PIC X(25).
           05  CUST-ADDR-LINE-1                        PIC X(50).
           05  CUST-ADDR-LINE-2                        PIC X(50).
           05  CUST-ADDR-LINE-3                        PIC X(50).         
           05  CUST-ADDR-STATE-CD                      PIC X(02).
           05  CUST-ADDR-COUNTRY-CD                    PIC X(03).
           05  CUST-ADDR-ZIP                           PIC X(10).
           05  CUST-PHONE-NUM-1                        PIC X(15).
           05  CUST-PHONE-NUM-2                        PIC X(15).
           05  CUST-SSN                                PIC 9(09).
           05  CUST-GOVT-ISSUED-ID                     PIC X(20).
           05  CUST-DOB-YYYY-MM-DD                     PIC X(10).
           05  CUST-EFT-ACCOUNT-ID                     PIC X(10).
           05  CUST-PRI-CARD-HOLDER-IND                PIC X(01).
           05  CUST-FICO-CREDIT-SCORE                  PIC 9(03).
           05  FILLER                                  PIC X(168).      
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:00 CDT
      *

       01  CUSTFILE-STATUS.
           05  CUSTFILE-STAT1      PIC X.
           05  CUSTFILE-STAT2      PIC X.


      *COPY CVACT03Y.
      *** >>> Automated Expansion of CPY: [CVACT03Y]
      *****************************************************************         
      *    Data-structure for card xref (RECLN 50)                              
      *****************************************************************         
       01 CARD-XREF-RECORD.                                                     
           05  XREF-CARD-NUM                     PIC X(16).                     
           05  XREF-CUST-ID                      PIC 9(09).                     
           05  XREF-ACCT-ID                      PIC 9(11).                     
           05  FILLER                            PIC X(14).                     
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:00 CDT
      *

       01  XREFFILE-STATUS.
           05  XREFFILE-STAT1      PIC X.
           05  XREFFILE-STAT2      PIC X.


      *COPY CVACT02Y.
      *** >>> Automated Expansion of CPY: [CVACT02Y]
      *****************************************************************
      *    Data-structure for card entity (RECLN 150)
      *****************************************************************
       01  CARD-RECORD.
           05  CARD-NUM                          PIC X(16).
           05  CARD-ACCT-ID                      PIC 9(11).
           05  CARD-CVV-CD                       PIC 9(03).
           05  CARD-EMBOSSED-NAME                PIC X(50).
           05  CARD-EXPIRAION-DATE               PIC X(10).
           05  CARD-ACTIVE-STATUS                PIC X(01).
           05  FILLER                            PIC X(59).
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:00 CDT
      *

       01  CARDFILE-STATUS.
           05  CARDFILE-STAT1      PIC X.
           05  CARDFILE-STAT2      PIC X.


      *COPY CVACT01Y.
      *** >>> Automated Expansion of CPY: [CVACT01Y]
      *****************************************************************
      *    Data-structure for  account entity (RECLN 300)
      *****************************************************************
       01  ACCOUNT-RECORD.
           05  ACCT-ID                           PIC 9(11).
           05  ACCT-ACTIVE-STATUS                PIC X(01).
           05  ACCT-CURR-BAL                     PIC S9(10)V99.
           05  ACCT-CREDIT-LIMIT                 PIC S9(10)V99.
           05  ACCT-CASH-CREDIT-LIMIT            PIC S9(10)V99.
           05  ACCT-OPEN-DATE                    PIC X(10).
           05  ACCT-EXPIRAION-DATE               PIC X(10). 
           05  ACCT-REISSUE-DATE                 PIC X(10).
           05  ACCT-CURR-CYC-CREDIT              PIC S9(10)V99.
           05  ACCT-CURR-CYC-DEBIT               PIC S9(10)V99.
           05  ACCT-ADDR-ZIP                     PIC X(10).
           05  ACCT-GROUP-ID                     PIC X(10).
           05  FILLER                            PIC X(178).      
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:15:59 CDT
      *

       01  ACCTFILE-STATUS.
           05  ACCTFILE-STAT1      PIC X.
           05  ACCTFILE-STAT2      PIC X.


      *COPY CVTRA05Y.
      *** >>> Automated Expansion of CPY: [CVTRA05Y]
      *****************************************************************         
      *    Data-structure for TRANsaction record (RECLN = 350)                  
      *****************************************************************         
       01  TRAN-RECORD.                                                         
           05  TRAN-ID                                 PIC X(16).               
           05  TRAN-TYPE-CD                            PIC X(02).               
           05  TRAN-CAT-CD                             PIC 9(04).               
           05  TRAN-SOURCE                             PIC X(10).               
           05  TRAN-DESC                               PIC X(100).              
           05  TRAN-AMT                                PIC S9(09)V99.           
           05  TRAN-MERCHANT-ID                        PIC 9(09).               
           05  TRAN-MERCHANT-NAME                      PIC X(50).               
           05  TRAN-MERCHANT-CITY                      PIC X(50).               
           05  TRAN-MERCHANT-ZIP                       PIC X(10).               
           05  TRAN-CARD-NUM                           PIC X(16).               
           05  TRAN-ORIG-TS                            PIC X(26).               
           05  TRAN-PROC-TS                            PIC X(26).               
           05  FILLER                                  PIC X(20).               
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:01 CDT
      *

       01  TRANFILE-STATUS.
           05  TRANFILE-STAT1      PIC X.
           05  TRANFILE-STAT2      PIC X.

       01  IO-STATUS.
           05  IO-STAT1            PIC X.
           05  IO-STAT2            PIC X.

       01  TWO-BYTES-BINARY        PIC 9(4) BINARY.
       01  TWO-BYTES-ALPHA         REDEFINES TWO-BYTES-BINARY.
           05  TWO-BYTES-LEFT      PIC X.
           05  TWO-BYTES-RIGHT     PIC X.

       01  IO-STATUS-04.
           05  IO-STATUS-0401      PIC 9   VALUE 0.
           05  IO-STATUS-0403      PIC 999 VALUE 0.

       01  APPL-RESULT             PIC S9(9)   COMP.
           88  APPL-AOK            VALUE 0.
           88  APPL-EOF            VALUE 16.

       01  END-OF-DAILY-TRANS-FILE             PIC X(01)    VALUE 'N'.
       01  ABCODE                  PIC S9(9) BINARY.
       01  TIMING                  PIC S9(9) BINARY.
       01  WS-MISC-VARIABLES.
           05 WS-XREF-READ-STATUS  PIC 9(04).
           05 WS-ACCT-READ-STATUS  PIC 9(04).

      *****************************************************************
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN01C'.
           PERFORM 0000-DALYTRAN-OPEN.
           PERFORM 0100-CUSTFILE-OPEN.
           PERFORM 0200-XREFFILE-OPEN.
           PERFORM 0300-CARDFILE-OPEN.
           PERFORM 0400-ACCTFILE-OPEN.
           PERFORM 0500-TRANFILE-OPEN.

           PERFORM UNTIL END-OF-DAILY-TRANS-FILE = 'Y'
               IF  END-OF-DAILY-TRANS-FILE = 'N'
                   PERFORM 1000-DALYTRAN-GET-NEXT
                   IF  END-OF-DAILY-TRANS-FILE = 'N'
                       DISPLAY DALYTRAN-RECORD
                   END-IF
                   MOVE 0                 TO WS-XREF-READ-STATUS
                   MOVE DALYTRAN-CARD-NUM TO XREF-CARD-NUM
                   PERFORM 2000-LOOKUP-XREF
                   IF WS-XREF-READ-STATUS = 0
                     MOVE 0            TO WS-ACCT-READ-STATUS
                     MOVE XREF-ACCT-ID TO ACCT-ID
                     PERFORM 3000-READ-ACCOUNT
                     IF WS-ACCT-READ-STATUS NOT = 0
                         DISPLAY 'ACCOUNT ' ACCT-ID ' NOT FOUND'
                     END-IF
                   ELSE
                     DISPLAY 'CARD NUMBER ' DALYTRAN-CARD-NUM
                     ' COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-'
                     DALYTRAN-ID
                   END-IF
               END-IF
           END-PERFORM.

           PERFORM 9000-DALYTRAN-CLOSE.
           PERFORM 9100-CUSTFILE-CLOSE.
           PERFORM 9200-XREFFILE-CLOSE.
           PERFORM 9300-CARDFILE-CLOSE.
           PERFORM 9400-ACCTFILE-CLOSE.
           PERFORM 9500-TRANFILE-CLOSE.

           DISPLAY 'END OF EXECUTION OF PROGRAM CBTRN01C'.

           GOBACK.

      *****************************************************************
      * READS FILE                                                    *
      *****************************************************************
       1000-DALYTRAN-GET-NEXT.
           READ DALYTRAN-FILE INTO DALYTRAN-RECORD.
           IF  DALYTRAN-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               IF  DALYTRAN-STATUS = '10'
                   MOVE 16 TO APPL-RESULT
               ELSE
                   MOVE 12 TO APPL-RESULT
               END-IF
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               IF  APPL-EOF
                   MOVE 'Y' TO END-OF-DAILY-TRANS-FILE
               ELSE
                   DISPLAY 'ERROR READING DAILY TRANSACTION FILE'
                   MOVE DALYTRAN-STATUS TO IO-STATUS
                   PERFORM Z-DISPLAY-IO-STATUS
                   PERFORM Z-ABEND-PROGRAM
               END-IF
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       2000-LOOKUP-XREF.
           MOVE XREF-CARD-NUM TO FD-XREF-CARD-NUM
           READ XREF-FILE  RECORD INTO CARD-XREF-RECORD
           KEY IS FD-XREF-CARD-NUM
                INVALID KEY
                  DISPLAY 'INVALID CARD NUMBER FOR XREF'
                  MOVE 4 TO WS-XREF-READ-STATUS
                NOT INVALID KEY
                  DISPLAY 'SUCCESSFUL READ OF XREF'
                  DISPLAY 'CARD NUMBER: ' XREF-CARD-NUM
                  DISPLAY 'ACCOUNT ID : ' XREF-ACCT-ID
                  DISPLAY 'CUSTOMER ID: ' XREF-CUST-ID
           END-READ.
      *---------------------------------------------------------------*
       3000-READ-ACCOUNT.
           MOVE ACCT-ID TO FD-ACCT-ID
           READ ACCOUNT-FILE RECORD INTO ACCOUNT-RECORD
           KEY IS FD-ACCT-ID
                INVALID KEY
                  DISPLAY 'INVALID ACCOUNT NUMBER FOUND'
                  MOVE 4 TO WS-ACCT-READ-STATUS
                NOT INVALID KEY
                  DISPLAY 'SUCCESSFUL READ OF ACCOUNT FILE'
           END-READ.
      *---------------------------------------------------------------*
       0000-DALYTRAN-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN INPUT DALYTRAN-FILE
           IF  DALYTRAN-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING DAILY TRANSACTION FILE'
               MOVE DALYTRAN-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.

      *---------------------------------------------------------------*
       0100-CUSTFILE-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN INPUT CUSTOMER-FILE
           IF  CUSTFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING CUSTOMER FILE'
               MOVE CUSTFILE-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       0200-XREFFILE-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN INPUT XREF-FILE
           IF  XREFFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING CROSS REF FILE'
               MOVE XREFFILE-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       0300-CARDFILE-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN INPUT CARD-FILE
           IF  CARDFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING CARD FILE'
               MOVE CARDFILE-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       0400-ACCTFILE-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN INPUT ACCOUNT-FILE
           IF  ACCTFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING ACCOUNT FILE'
               MOVE ACCTFILE-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       0500-TRANFILE-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN INPUT TRANSACT-FILE
           IF  TRANFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING TRANSACTION FILE'
               MOVE TRANFILE-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       9000-DALYTRAN-CLOSE.
           ADD 8 TO ZERO GIVING APPL-RESULT.
           CLOSE DALYTRAN-FILE
           IF  DALYTRAN-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR CLOSING CUSTOMER FILE'
               MOVE CUSTFILE-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       9100-CUSTFILE-CLOSE.
           ADD 8 TO ZERO GIVING APPL-RESULT.
           CLOSE CUSTOMER-FILE
           IF  CUSTFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR CLOSING CUSTOMER FILE'
               MOVE CUSTFILE-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       9200-XREFFILE-CLOSE.
           ADD 8 TO ZERO GIVING APPL-RESULT.
           CLOSE XREF-FILE
           IF  XREFFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR CLOSING CROSS REF FILE'
               MOVE XREFFILE-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       9300-CARDFILE-CLOSE.
           ADD 8 TO ZERO GIVING APPL-RESULT.
           CLOSE CARD-FILE
           IF  CARDFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR CLOSING CARD FILE'
               MOVE CARDFILE-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       9400-ACCTFILE-CLOSE.
           ADD 8 TO ZERO GIVING APPL-RESULT.
           CLOSE ACCOUNT-FILE
           IF  ACCTFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR CLOSING ACCOUNT FILE'
               MOVE ACCTFILE-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.
      *---------------------------------------------------------------*
       9500-TRANFILE-CLOSE.
           ADD 8 TO ZERO GIVING APPL-RESULT.
           CLOSE TRANSACT-FILE
           IF  TRANFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR CLOSING TRANSACTION FILE'
               MOVE TRANFILE-STATUS TO IO-STATUS
               PERFORM Z-DISPLAY-IO-STATUS
               PERFORM Z-ABEND-PROGRAM
           END-IF
           EXIT.

       Z-ABEND-PROGRAM.
           DISPLAY 'ABENDING PROGRAM'
           MOVE 0 TO TIMING
           MOVE 999 TO ABCODE
           CALL 'CEE3ABD'.

      *****************************************************************
       Z-DISPLAY-IO-STATUS.
           IF  IO-STATUS NOT NUMERIC
           OR  IO-STAT1 = '9'
               MOVE IO-STAT1 TO IO-STATUS-04(1:1)
               MOVE 0        TO TWO-BYTES-BINARY
               MOVE IO-STAT2 TO TWO-BYTES-RIGHT
               MOVE TWO-BYTES-BINARY TO IO-STATUS-0403
               DISPLAY 'FILE STATUS IS: NNNN' IO-STATUS-04
           ELSE
               MOVE '0000' TO IO-STATUS-04
               MOVE IO-STATUS TO IO-STATUS-04(3:2)
               DISPLAY 'FILE STATUS IS: NNNN' IO-STATUS-04
           END-IF
           EXIT.


