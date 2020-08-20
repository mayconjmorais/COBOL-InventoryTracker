# COBOL-InventoryTracker

Input Files
     Inventory records have already been established in an external file called INVENT.TXT. The record structure is described  below in INPUT RECORDS. The file containing these records must be referenced in the program as INVENT-FILE-IN. 
     There will be second  file  -- SUPPLIER.TXT – that contains the Supplier Information. Refer to SUPPLIER RECORD below in INPUT RECORDS.
The Supplier Records must be loaded into a table. The table will then be searched for the Supplier Name for a Re-order Report. Refer to Report Formats below.

Output Files
     Two output files will be generated by your program – the Inventory Report and a 
Re-order Report  

Inventory Report
     This program will essentially be the same as that for Project 2 but with some modifications and additional functions.
     The program will read each inventory record, calculate the value of that inventory item, then print out that inventory record with the calculated value, as part of an INVENTORY REPORT. This output record will be the same as the record described for Project 2. After processing the last record, the Audit Trail will be written as per Project 2 but with editing of the numeric fields. Refer to REPORT FORMATS for specific format details.
     The file name for the report in the program will be INVENT-REPORT-OUT. The name for the report file stored on the disk (external device) will be INVREPRT.TXT


Re-Order Report
     As each record is processed, determine whether the Quantity on Hand is less than the Reorder Point. If so, that record should be written to a Re-Order Report.
     The Re-order Record in the Re-order Report will contain the:
           Part Number; Part Name; Quantity on hand; Re-order Point; and Supplier Name.
 The Supplier Name will be extracted from the Supplier Table using the Supplier Code.
You must set up the actual layout for this report. Zero suppression of numeric fields is required.. 
     The Supplier Table must  be loaded with records from the Supplier File (SUPPLIER.TXT). The structure for the records on that file is given below in  INPUT RECORDS

     At the end of the INVENTORY REPORT, once all records have been printed, the TOTAL VALUE for all inventory items should be printed.
 
     Also, the program should keep a count of the inventory records read and the inventory records printed as an audit trail. 


 INPUT RECORDS
The record structures are as follows. Use the field name given below as the  source for the field names in your solution set and code. Abbreviations are allowed but must be meaningful.

      INVENTORY RECORD			
PART NUMBER		5 bytes		numeric
PART NAME			20 bytes	alphanumeric
QUANTITY ON HAND	3 bytes		numeric
UNIT PRICE			4 bytes		numeric (includes 2 bytes assumed decimal) 
SUPPLIER CODE		5 bytes		alphanumeric
RE-ORDER POINT		3 bytes		numeric

		
      SUPPLIER RECORD 
SUPPLIER CODE    PIC X (5)
SUPPLIER NAME   PIC X (15)

REQUIRED DATA FILE (input)

You must use the prescribed data files for input. The data file (test data) for this program will be in INVENT.TXT for the inventory file and SUPPLIER.TXT for the supplier records. 
Both of these will be in the PROJECTS Content area for this project (PROJECT 3) in Brightspace. 

 
PROCESSING REQUIREMENTS

Inventory Value calculation
     The INVENTORY VALUE is equal to the QUANTITY ON HAND times the
 UNIT PRICE.

Total Value calculation.
     The TOTAL VALUE for all inventory items is equal to the sum of all individual inventory items value.
     At the end of processing all records, the program should: 
1)	print out the number of records read in and the number of records written out to the output file (use zero suppression);
            2)  print out the total value of the inventory (TOTAL VALUE).
            3) edit the numeric fields by suppressing leading zeros and using a floating dollar
                sign, commas  and decimal point  for the TOTAL VALUE

     Use the same layout for the Inventory Report as for Project 2 with the following  adjustments:
    1)   all numeric fields must use suppressed zeros;
    2)   the Total Value field must have a floating dollar sign;
    3)   insert the decimal point where applicable and commas in all numeric fields ;
    4)   write only ten inventory records per page. After ten records have been written, go
          to  a new page, write the column  headings before writing more of the inventory detail records (ten per page);
    5)  the individual inventory value fields must use zero suppression.  
      

REPORT FORMATS
Inventory Report
     The Inventory Report will have the same layout as prescribed for Project 2 with the following modifications:
     1) the first page of the report will have a Report Title called INVENTORY REPORT. Include the date of the report and the day name of the report (eg. MONDAY 20 02 27). The spacing of this report line is up to you;
     2)  there will only be 10 Inventory Detail lines per page.  Once 10 lines have been printed, you will go to a new page, write the Column Headers and continue the writing of detail lines. This will continue for all records (only ten lines per page).     

Re-order Report
This report should contain the following fields in this order:
     Part Number, Part Name, Quantity on hand, Re-order Point, Supplier Name
You must set up the actual layout for this report. Zero suppression is required.