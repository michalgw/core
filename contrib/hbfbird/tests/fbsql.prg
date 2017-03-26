#require "hbfbird"
#include "hbfbird.ch"

REQUEST HB_LANG_PL
REQUEST HB_CODEPAGE_UTF8

FUNCTION Main()
   LOCAL oDatabase
   LOCAL oTransaction
   LOCAL oSQL
   LOCAL oBW
   LOCAL oBR
   
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
   LOCAL cStr
   
   //ErrorBlock( {| oError | FbDefError( oError ) } )
   
   //AltD()

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
   
   ? "Prepare 3# SQL (inserts)"
   oSQL:SQL := "insert into test (Code, dept, Name, Sales, Tax, Salary, Budget, Discount, Creation, Description) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) returning code, name"
   ? "Prepare..."
   IF ! oSQL:Prepare()
      ? "Error: " + Str( oSQL:LastError )
      ? FBError( oSQL:LastError )
      QUIT
   ENDIF
   ? "Param count: ", oSQL:ParamCount()
   
   
   cStr := ""
   ? "Execute inserts"
   FOR nI := 1 TO 10
      ? "Setting params...", nI
      oSQL:SetParam( 1, nI )
      oSQL:SetParam( 2, 2 )

      oSQL:SetParam( 3, "TESTŁÓĄ" )
      oSQL:SetParam( 4, 1 )
      oSQL:SetParam( 5, 1.37 * nI )
      oSQL:SetParam( 6, 10.45 * nI )
      oSQL:SetParam( 7, 15.2 * nI )
      oSQL:SetParam( 8, 7.57 * nI )
      oSQL:SetParam( 9, Date() )

      /* BLOB */
      oBW := TFbBlobWriter():New( oDatabase, oTransaction )
      cStr := cStr + "ABCD"
      oBW:Write( cStr, Len( cStr ) )
      oBW:Close()
      oSQL:SetParam( 10, oBW:Quad )
      
      
      ? "Executing insert...", nI
      IF ! oSQL:Execute()
         ? "Error: ", oSQL:LastError
         ? FBError( oSQL:LastError )
         QUIT
      ENDIF
      ? "Returning: ", oSQL:GetValue( 1 ), oSQL:GetValue( 2 )
      oBW := NIL
   NEXT

   oSQL:Close()
   
   oTransaction:CommitRetaining()
   
   ? "Prepare 4# SQL (CREATE TABLE....)"
   oSQL:SQL := "select * from test"
   ? "Prepare..."
   IF ! oSQL:Prepare()
      ? "Error: " + Str( oSQL:LastError )
      ? FBError( oSQL:LastError )
      QUIT
   ENDIF
   ? "Field count: ", oSQL:FieldCount()
   ? "Execute: ", oSQL:Execute()
   
   nJ := 1
   DO WHILE oSQL:Fetch() == 0
      ? " "
      ? " "
      ? " "
      ? "Record no: ", nJ
      nJ := nJ + 1
      FOR nI := 1 TO oSQL:FieldCount()
         ? "-----------------------------------------------------------"
         ? oSQL:FieldInfo( nI )[ 1 ], ":", oSQL:GetValue( nI )
         
         IF oSQL:FieldInfo( nI )[ 2 ] == SQL_BLOB
            ? "BLOB: "
            oBR := TFbBlobReader():New( oDatabase, oTransaction, oSQL:GetValue( nI ) )
            ? "Max seg: ", oBR:MaxSegment
            ? "Num seg: ", oBR:NumSegments
            ? "Len: ", oBR:TotalLength
            ? "Type: ", oBR:BlobType
            cStr = oBR:Read( oBR:TotalLength )
            oBR:Close()
            oBR := NIL
            ? "Data: ", cStr
         ENDIF
         
      NEXT
   ENDDO
   
   oSQL:Close()
   oTransaction:Commit()
   
   ? "END!"

   RETURN 0

/*----------------------------------------------------------------------*/
/*
STATIC FUNCTION FbDefError( )
   LOCAL n := 1
   DO WHILE ! Empty( ProcName( ++n ) )

      OutErr( hb_eol() )
      OutErr( hb_StrFormat( "Called from %1$s(%2$d)  ", ;
              ProcName( n ), ;
              ProcLine( n ) ) )

   ENDDO
   ErrorLevel( 1 )
   QUIT
   RETURN .F.
*/