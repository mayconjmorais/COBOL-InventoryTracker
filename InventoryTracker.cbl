       PROGRAM-ID.     InventoryTracker.
       AUTHOR.         MAYCON MORAIS, 
                       DANIEL LENGLER.
       DATE-WRITTEN.   "March 9, 2020".

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENT-FILE-IN
               ASSIGN TO       
               "C:\INVENT.txt"              
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SUPPLIER-FILE-IN
               ASSIGN TO       
              "C:\SUPPLIER.txt"                
               ORGANIZATION IS LINE SEQUENTIAL.
	      
		  SELECT INVENT-REPORT-OUT       
            ASSIGN TO 
            "C:\INVREPRT.TXT"                  
	        ORGANIZATION IS LINE SEQUENTIAL.
            
          SELECT RE-ORDER-OUT       
            ASSIGN TO 
            "C:\REORDER.TXT"                   
	        ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
	   FD INVENT-FILE-IN.
	   01 IN-CBLFIL1.
	      05 IN-PART-NUMBER              PIC 9(05).
		  05 IN-PART-NAME                PIC X(20).
		  05 IN-QUANTITY-HAND            PIC 9(03).
		  05 IN-UNIT-PRICE               PIC 9(04).
		  05 IN-SUPP-CODE                PIC X(05).
          05 IN-ORDER-POINT              PIC 9(03).
         
	   FD SUPPLIER-FILE-IN.
       01 IN-SUPPLIER-RECORD.
          05 IN-SUPPLIER-CODE            PIC X(05).
          05 IN-SUPPLIER-NAME            PIC X(15).
       
	   FD INVENT-REPORT-OUT.
	   01 INVENT-REPORT                  PIC X(50).
       
       FD RE-ORDER-OUT.
	   01 OUT-RE-ORDER.
          05 OUT-RE-PART-NUMBER          PIC ZZZZZZZ.
          05 OUT-RE-PART-NAME            PIC X(20).
          05 OUT-RE-QUANTITY-HAND        PIC ZZZZ.
          05 OUT-RE-ORDER-POINT          PIC ZZZ.
          05 OUT-RE-SUPPLIER-NAME        PIC X(15).
           
       WORKING-STORAGE SECTION.
       01 WS-TABLE-HEADER.
           05 FILLER                     PIC X(10) VALUE "PARTNUMBER".
           05 FILLER                     PIC X(1) VALUE SPACE.
           05 FILLER                     PIC X(8) VALUE "PARTNAME".
           05 FILLER                     PIC X(12) VALUE SPACE.
           05 FILLER                     PIC X(8) VALUE "QUANTITY".
           05 FILLER                     PIC X(2) VALUE SPACE.
           05 FILLER                     PIC X(5) VALUE "VALUE".
           05 FILLER                     PIC X(4) VALUE SPACE.
           
       01 WS-INVENT-RECORD.
           05 PART-NUMBER                PIC 9(7).
           05 FILLER                     PIC X(4) VALUE SPACE.
           05 PART-NAME                  PIC X(20).
           05 FILLER                     PIC X(4) VALUE SPACE.
           05 QUANTITY-ON-HAND           PIC 9(4).
           05 FILLER                     PIC X(2) VALUE SPACE.
           05 STOCK-VALUE                PIC $,$$$,$$9.99.
           
       01 WS-AUDIT-TRAIL.
           05 FILLER                     PIC X(11) VALUE "TOTAL VALUE".
           05 FILLER                     PIC X(2) VALUE SPACE.
           05 TOTAL-VALUE                PIC $$,$$$,$99.99.
           05 FILLER                     PIC X(1) VALUE SPACE.
           05 FILLER                     PIC X(4) VALUE "READ".
           05 FILLER                     PIC X(1) VALUE SPACE.
           05 READ-CTR                   PIC 9(4) VALUE ZERO.
           05 FILLER                     PIC X(1) VALUE SPACE.
           05 FILLER                     PIC X(7) VALUE "WRITTEN".
           05 FILLER                     PIC X(2) VALUE SPACE.
           05 WRITE-CTR                  PIC 9(4) VALUE ZEROS.
           
       01 SUPPLIER-TABLE.
         05 SUPPLIER-RECORD OCCURS 20 TIMES INDEXED BY I.
           10 SUPPLIER-CODE              PIC X(05).
           10 SUPPLIER-NAME              PIC X(15).
           
       01 EOF-FLAGS.
           05 INVENT-EOF-FLAG            PIC X(3) VALUE "NO".
           05 SUPPLIER-EOF-FLAG          PIC X(3) VALUE "NO".
       
       01 AUX-TOTAL.
          05 WS-STOCK-TOTAL              PIC 9(6)V99 VALUE ZEROS.
          05 WS-TOTAL-VALUE              PIC 9(8)V99 VALUE ZEROS.
          
       01 WS-TODAYS-DATE.
		  05 WS-DATE                     PIC 9(08) VALUE ZERO.
          05 WS-DAY                      PIC 9(01).
          
       01 WEEK-LIST.
		  05 NAME-OF-WEEK-01 	         PIC X(09) VALUE 'SUNDAY   '.
		  05 NAME-OF-WEEK-02 			 PIC X(09) VALUE 'MONDAY   '.
		  05 NAME-OF-WEEK-03 			 PIC X(09) VALUE 'TUESDAY  '.
		  05 NAME-OF-WEEK-04 			 PIC X(09) VALUE 'WEDNESDAY'.
		  05 NAME-OF-WEEK-05 			 PIC X(09) VALUE 'THURSDAY '.
          05 NAME-OF-WEEK-06 			 PIC X(09) VALUE 'FRIDAY   '.
          05 NAME-OF-WEEK-07 			 PIC X(09) VALUE 'SATURDAY '.

	   01 WEEK-TABLE REDEFINES WEEK-LIST.
		  05 NAME-OF-WEEK                PIC X(09) OCCURS 7 TIMES.
     
       01 WS-INVENTORY-DESCR.   
          05 FILLER                      PIC X(19) VALUE
          "INVENTORY REPORT - ".
          05 WS-DAY-OF-WEEK              PIC X(09) VALUE SPACE.
          05 WS-INVENT-TODAYS.
             10 WS-YEAR-INVENT           PIC 9(02) VALUE ZEROS.
             10 FILLER                   PIC X(01) VALUE SPACE.
		     10 WS-MONTH-INVENT          PIC 9(02) VALUE ZEROS.
             10 FILLER                   PIC X(01) VALUE SPACE.
             10 WS-DAY-INVENT            PIC 9(02) VALUE ZEROS.
             
       01 WS-TODAYS.
          05 WS-TODAYS-YEAR              PIC 9(02) VALUE ZEROS.
		  05 WS-TODAYS-MONTH             PIC 9(02) VALUE ZEROS.
          05 WS-TODAYS-DAY               PIC 9(02) VALUE ZEROS.
             
       PROCEDURE DIVISION.
       100-PRODUCE-INVENT-REPORTS.
           PERFORM 200-INITIATE-CREATE-INVENT-FILE.
           PERFORM 201-PROCESS-INVENT-FILE 
             UNTIL INVENT-EOF-FLAG EQUALS "YES".
           PERFORM 202-TERMINATE-INVENT-FILE.                           
           
           STOP RUN.
           
       200-INITIATE-CREATE-INVENT-FILE.
           PERFORM 300-OPEN-FILES.
           
           SET I TO 1.
           PERFORM 400-READ-SUPP-FILE
               VARYING I FROM 1 BY 1
               UNTIL SUPPLIER-EOF-FLAG EQUALS "YES".
           PERFORM 303-WRITE-HEADER.
           PERFORM 301-READ-INVENT-FILE.
           
       300-OPEN-FILES.
	       OPEN INPUT  INVENT-FILE-IN
                       SUPPLIER-FILE-IN
                OUTPUT INVENT-REPORT-OUT
                       RE-ORDER-OUT.
                  
       400-READ-SUPP-FILE.
           READ SUPPLIER-FILE-IN 
		     AT END 
			   MOVE "YES"                TO SUPPLIER-EOF-FLAG
             NOT AT END
               MOVE IN-SUPPLIER-RECORD   TO SUPPLIER-RECORD (I).
                                                     
       303-WRITE-HEADER.
      * IF/ELSE: USED TO WRITE FIRST HEADER WITHOUT SKIP PAGE IN THE 
      * BEGINNING 
           IF WRITE-CTR EQUALS ZERO
               ACCEPT WS-TODAYS          FROM DATE
               MOVE WS-TODAYS-YEAR       TO WS-YEAR-INVENT
               MOVE WS-TODAYS-MONTH      TO WS-MONTH-INVENT
               MOVE WS-TODAYS-DAY        TO WS-DAY-INVENT
               
               ACCEPT WS-DAY             FROM DAY-OF-WEEK
               MOVE NAME-OF-WEEK(WS-DAY) TO WS-DAY-OF-WEEK
               
               WRITE INVENT-REPORT FROM WS-INVENTORY-DESCR
               WRITE INVENT-REPORT FROM WS-TABLE-HEADER
           ELSE
               WRITE INVENT-REPORT FROM WS-TABLE-HEADER
               AFTER ADVANCING PAGE
           END-IF.

       301-READ-INVENT-FILE.
           READ INVENT-FILE-IN 
		    AT END 
			   MOVE "YES"                TO INVENT-EOF-FLAG 
            NOT AT END 
			   ADD 1                     TO READ-CTR.
                  
       201-PROCESS-INVENT-FILE.
           PERFORM 306-CALC-INVENT-ITEM-VALUE.
           PERFORM 309-INCREASE-TOTAL-VALUE.
           PERFORM 307-WRITE-INVENT-RECORD.
           
           IF IN-QUANTITY-HAND LESS THAN IN-ORDER-POINT
               PERFORM 308-PROCESS-REORDER
		   END-IF.
           
           IF FUNCTION MOD (WRITE-CTR, 10) EQUALS ZERO
		      PERFORM 303-WRITE-HEADER
           END-IF.
           PERFORM 301-READ-INVENT-FILE.
       
       306-CALC-INVENT-ITEM-VALUE.
           COMPUTE WS-STOCK-TOTAL = IN-QUANTITY-HAND * IN-UNIT-PRICE.
                  
       309-INCREASE-TOTAL-VALUE.
           ADD WS-STOCK-TOTAL            TO WS-TOTAL-VALUE.
       
       307-WRITE-INVENT-RECORD.
           MOVE IN-PART-NAME             TO PART-NAME.
           MOVE IN-PART-NUMBER           TO PART-NUMBER.
           MOVE IN-QUANTITY-HAND         TO QUANTITY-ON-HAND.
           MOVE WS-STOCK-TOTAL           TO STOCK-VALUE.

           WRITE INVENT-REPORT FROM WS-INVENT-RECORD.
           ADD 1                         TO WRITE-CTR.
         
       308-PROCESS-REORDER.
           MOVE IN-PART-NUMBER           TO OUT-RE-PART-NUMBER.
           MOVE IN-PART-NAME             TO OUT-RE-PART-NAME. 
           MOVE IN-QUANTITY-HAND         TO OUT-RE-QUANTITY-HAND.
           MOVE IN-ORDER-POINT           TO OUT-RE-ORDER-POINT.
           MOVE WS-TOTAL-VALUE           TO TOTAL-VALUE.
           
           SET I TO 1.
           SEARCH SUPPLIER-RECORD
               WHEN SUPPLIER-CODE(I) EQUALS IN-SUPP-CODE
                   MOVE SUPPLIER-NAME(I) TO OUT-RE-SUPPLIER-NAME
           END-SEARCH.
           
           WRITE OUT-RE-ORDER.
  
       202-TERMINATE-INVENT-FILE.
           PERFORM 304-WRITE-AUDIT-TRAIL.
           PERFORM 305-CLOSE-FILES.

       304-WRITE-AUDIT-TRAIL.
           WRITE INVENT-REPORT FROM WS-AUDIT-TRAIL.
           
       305-CLOSE-FILES.
           CLOSE INVENT-FILE-IN
		         SUPPLIER-FILE-IN
                 INVENT-REPORT-OUT
                 RE-ORDER-OUT.