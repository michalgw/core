#require "hbfbird"
#include "hbfbird.ch"
#include "fbutils.ch"

REQUEST HB_LANG_PL
REQUEST HB_CODEPAGE_UTF8

FUNCTION Main()
   LOCAL oDatabase
   LOCAL oTransaction
   LOCAL oDataSet
   
   LOCAL cServer := "localhost:"
   LOCAL cDatabase
   LOCAL cUser := "SYSDBA"
   LOCAL cPass := "masterkey"
   LOCAL nPageSize := 4096
   LOCAL cCharSet := "UTF8"
   LOCAL nDialect := 3
   LOCAL cName
   LOCAL tmp
   LOCAL aTables
   
   LOCAL aTest1Data := { { "Steave", 45 }, { "Bob", 25 }, { "Samantha", 34 } } 
   
   hb_langselect('PL')
   hb_cdpselect("UTF8")
   
*   AltD()
   
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
   
   ? "Create table TEST1"
   fb_CreateTable( oDatabase, oTransaction, "TEST1", { { "ID", "+", 0, 0 }, { "NAME", "C", 20, 0 }, { "AGE", "N", 4, 0 } } )
   oTransaction:CommitRetaining()

   ? "Create table TEST2"
   fb_CreateTable( oDatabase, oTransaction, "TEST2", { { "ID", "+", 0, 0 }, { "FLDC", "C", 20, 0 }, { "FLDN", "N", 4, 0 }, { "FLDN2", "N", 12, 4 }, { "FLDD", "D", 0, 0 }, { "FLDT", "T", 0, 0 }, { "FLDL", "L", 0, 0 } } )
   oTransaction:CommitRetaining()
   
   ? "List tables"
   aTables := fb_ListDBObjects( oDatabase, oTransaction, FBOT_TABLES )
   
   AEval( aTables, { | cTable | QOut( cTable ) } )
   
   ? "List fields"
   aTables := fb_ListDBObjects( oDatabase, oTransaction, FBOT_FIELDS, "TEST2" )

   AEval( aTables, { | cTable | QOut( cTable ) } )
   
   oTransaction:CommitRetaining()

   ? "Create dataset"
   oDataSet := fb_DataSetCreate( oDatabase, oTransaction, "TEST1", "ID" )
   oDataSet:Open()

   AEval( aTest1Data, { | aRow |
      oDataSet:Append()
      oDataSet[ 'name' ] := aRow[ 1 ]
      oDataSet[ 'age' ] := aRow[ 2 ]
      oDataSet:Post()
      QOut( oDataSet[ 'id' ] )
      QOut( oDataSet[ 'name' ] )
      QOut( oDataSet[ 'age' ] )
      RETURN NIL
   } )

   oTransaction:Commit()
   
   RETURN NIL