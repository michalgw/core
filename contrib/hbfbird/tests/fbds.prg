#require "hbfbird"
#include "hbfbird.ch"
#include "box.ch"
#include "inkey.ch"
#include "setcurs.ch"

REQUEST HB_LANG_PL
REQUEST HB_CODEPAGE_UTF8

FUNCTION Main()
   LOCAL oDatabase
   LOCAL oTransaction
   LOCAL oSQL
   
   LOCAL cServer := "localhost:"
   LOCAL cDatabase
   LOCAL cUser := "SYSDBA"
   LOCAL cPass := "masterkey"
   LOCAL nPageSize := 4096
   LOCAL cCharSet := "UTF8"
   LOCAL nDialect := 3
   LOCAL cName
   LOCAL tmp
   LOCAL cQuery
   LOCAL nI
   LOCAL nJ
   
   LOCAL oDS

   hb_langselect('PL')
   hb_cdpselect("UTF8")
   
   hb_FNameSplit( hb_argv( 0 ), NIL, @cName, NIL )
   cDatabase := hb_DirTemp() + cName + ".fdb"

   IF hb_FileExists( cDatabase )
      FErase( cDatabase )
   ENDIF
   
   ? "Create database..."
   ? tmp := FBCreateDB( cServer + cDatabase, cUser, cPass, nPageSize, cCharSet, nDialect ), FBError( tmp )

   ? "Connect database..."
   oDatabase := TFBServer():New( cServer + cDatabase, "", "", nDialect, ;
      { { ISC_DPB_USER_NAME, "sysdba" }, { ISC_DPB_PASSWORD, "masterkey" }, { ISC_DPB_LC_CTYPE, "UTF8" } } )
      
   IF oDatabase:NetErr()
      ? oDatabase:Error()
      QUIT
   ENDIF
   
   oTransaction := TFBTransaction():New( oDatabase, { ISC_TPB_VERSION3, ISC_TPB_READ_COMMITTED, ISC_TPB_REC_VERSION, ISC_TPB_NOWAIT }, .T. )

   ? "Create FBSQL..."
   oSQL := TFBSQL():New( oDatabase, oTransaction )
   
   cQuery := "create domain boolean_field as smallint default 0 not null check (value in (0,1))"
   ? "Prepare 1# SQL (CREATE DOMAIN....)"
   oSQL:SQL := cQuery
   ? "Prepare..."
   IF ! oSQL:Prepare()
      ? "Error: " + Str( oSQL:LastError )
      ? FBError( oSQL:LastError )
      QUIT
   ENDIF
   
   ? "Execute 1#"
   IF ! oSQL:Execute()
      ? "Error: " + Str( oSQL:LastError )
      QUIT
   ENDIF
   oSQL:Close()
   
   cQuery := "CREATE TABLE test("
   cQuery += "     Code SmallInt not null primary key, "
   cQuery += "     dept Integer, "
   cQuery += "     Name Varchar(40), "
   cQuery += "     Sales boolean_field, "
   cQuery += "     Tax Float, "
   cQuery += "     Salary Double Precision, "
   cQuery += "     Budget Numeric(12,2), "
   cQuery += "     Discount Decimal(5,2), "
   cQuery += "     Creation Date, "
   cQuery += "     Description blob sub_type 1 segment size 40 ) "

   ? "Prepare 2# SQL (CREATE TABLE....)"
   oSQL:SQL := cQuery
   ? "Prepare..."
   IF ! oSQL:Prepare()
      ? "Error: " + Str( oSQL:LastError )
      ? FBError( oSQL:LastError )
      QUIT
   ENDIF
   
   ? "Execute 2#"
   IF ! oSQL:Execute()
      ? "Error: " + Str( oSQL:LastError )
      QUIT
   ENDIF
   oSQL:Close()
   
   oTransaction:CommitRetaining()
   
   oSQL := NIL
   
   oDS := TFBDataSet():New( oDatabase, oTransaction, 'select * from test', ;
      { 'insert into test (Code, dept, Name, Sales, Tax, Salary, Budget, Discount, Creation, Description) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)', { 'Code', 'dept', 'Name', 'Sales', 'Tax', 'Salary', 'Budget', 'Discount', 'Creation', 'Description' } } )
   
   oDS:Open()
   ? "Execute inserts"
   FOR nI := 1 TO 10
      ? "RecordCount before: ", oDS:RecordCount
      ? "RecNo before: ", oDS:RecNo()

      ? "Setting params...", nI
      oDS:Append()
      ? "RecordCount append: ", oDS:RecordCount
      ? "RecNo append: ", oDS:RecNo()
      oDS:SetValue( 1, nI )
      oDS:SetValue( 2, - 2 * nI )

      oDS:SetValue( 3, "TESTŁÓĄ" )
      oDS[ 4 ] := 1
      oDS[ 5 ] := 1.37 * nI
      oDS:SetValue( 6, 10.45 * nI )
      oDS:SetValue( 7, 15.2 * nI )
      oDS:SetValue( 8, 7.57 * nI )
      oDS:SetValue( 9, Date() )

      /* BLOB */
      //oSQL:SetParam( 10, NIL )
      
      
      ? "Executing insert...", nI
      IF ! oDS:Post()
//         ? "Error: ", oSQL:LastError
//         ? FBError( oSQL:LastError )
         ? "Error insert"
         QUIT
      ENDIF
      ? "RecordCount after: ", oDS:RecordCount
      ? "RecNo after: ", oDS:RecNo()

   NEXT

   oDS:Close()
   
   oTransaction:Commit()
   
   oTransaction:Start()
   
 ? "Prepare 4# SQL (CREATE TABLE....)"
   oDS:SelectSQL := "select * from test"
   oDS:DeleteSQL := { "delete from test where code = ?", "CODE" }
   oDS:UpdateSQL := { "update test set dept = ?, Name = ?, Sales = ?, Tax = ?, Salary = ?, Budget = ?, Discount = ?, Creation = ? where code = ?", { "dept", "Name", "Sales", "Tax", "Salary", "Budget", "Discount", "Creation", "code" } }
   oDS:RefreshSQL := { "select * from test where code = ?", "CODE" }
   ? "Prepare..."
   oDS:Open()
   ? "Field count: ", oDS:FieldCount()
   
   nJ := 1
   DO WHILE ! oDS:Eof()
      ? " "
      ? " "
      ? " "
      ? "Record no: ", nJ
      nJ := nJ + 1
      FOR nI := 1 TO oDS:FieldCount()
         ? "-----------------------------------------------------------"
         ? oDS:FieldInfo( nI )[ 1 ], ":", oDS:GetValue( nI )
      NEXT
      
      ? oDS[ 'code' ]
      ? oDS[ 2 ]
      
      oDS:Skip(1)
   ENDDO
   oDS:GoTop()

   ? "Locate: ", oDS:Locate( { "CODE" }, { 5 } )
      ? "Record no: ", oDS:RecNo()
      FOR nI := 1 TO oDS:FieldCount()
         ? "-----------------------------------------------------------"
         ? oDS:FieldInfo( nI )[ 1 ], ":", oDS:GetValue( nI )
      NEXT
      
      ? oDS[ 'code' ]
      ? oDS[ 2 ]

//   FBBrowse( oDS )
      
   oDS:Close()
   oTransaction:Commit()
   
   ? "END!"

   RETURN 0

/*----------------------------------------------------------------------*/

