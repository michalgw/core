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
   
   LOCAL cServer := "localhost:"
   LOCAL cDatabase
   LOCAL cUser := "SYSDBA"
   LOCAL cPass := "masterkey"
   LOCAL nPageSize := 4096
   LOCAL cCharSet := "UTF8"
   LOCAL nDialect := 3
   LOCAL cName
   LOCAL tmp
   
   MEMVAR test_str, test_num, test_date, test_time, test_bool
   PUBLIC test_str, test_num, test_date, test_time, test_bool

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

   fb_mvCreateTable( oDatabase, oTransaction )
   oTransaction:CommitRetaining()
   
   test_str := "String Test"
   test_num := 1.2345
   test_date := Date()
   test_time := hb_DateTime()
   test_bool := .T.
   
   fb_mvSave( oDatabase, oTransaction, "test", "test_*" )
   oTransaction:CommitRetaining()
   
   test_str := NIL
   test_num := NIL
   test_date := NIL
   test_time := NIL
   test_bool := NIL
   
   fb_mvRestore( oDatabase, oTransaction, "test", "test_*" )
   oTransaction:CommitRetaining()

   ? ValType( test_str ), "test_str", test_str
   ? ValType( test_num ), "test_num", test_num
   ? ValType( test_date ), "test_date", test_date
   ? ValType( test_time ), "test_time", test_time
   ? ValType( test_bool ), "test_bool", test_bool

   test_str := NIL
   test_num := NIL
   test_date := NIL
   test_time := NIL
   test_bool := NIL
   
   fb_mvDelete( oDatabase, oTransaction, "test", "test_num" )
   fb_mvDelete( oDatabase, oTransaction, "test", "test_date" )
   oTransaction:CommitRetaining()

   fb_mvRestore( oDatabase, oTransaction, "test", "test_*" )
   oTransaction:CommitRetaining()

   ? "test_str", test_str
   ? "test_num", test_num
   ? "test_date", test_date
   ? "test_time", test_time
   ? "test_bool", test_bool

   fb_mvDelete( oDatabase, oTransaction, "test" )
   oTransaction:CommitRetaining()
   
   test_str := NIL
   test_num := NIL
   test_date := NIL
   test_time := NIL
   test_bool := NIL

   fb_mvRestore( oDatabase, oTransaction, "test", "test_*" )
   oTransaction:Commit()

   ? "test_str", test_str
   ? "test_num", test_num
   ? "test_date", test_date
   ? "test_time", test_time
   ? "test_bool", test_bool

   ? "END!"

   RETURN 0

/*----------------------------------------------------------------------*/

