#require "hbfbird"
#include "hbfbird.ch"
#include "inkey.ch"

FUNCTION Main()

   LOCAL cDatabase := PadR( "localhost:d:\employee.fdb", 250 )
   LOCAL cUserName := PadR( "SYSDBA", 32 )
   LOCAL cPassword := PadR( "masterkey", 32 )
   LOCAL oDatabase
   LOCAL n, i, GetList := {}, aRes, aDispRes
   
   LOCAL aInfoList := { ;
      "ISC_INFO_DB_ID", ;
      "ISC_INFO_READS", ;
      "ISC_INFO_WRITES", ;
      "ISC_INFO_FETCHES", ;
      "ISC_INFO_MARKS", ;
      "ISC_INFO_IMPLEMENTATION", ;
      "ISC_INFO_ISC_VERSION", ;
      "ISC_INFO_BASE_LEVEL", ;
      "ISC_INFO_PAGE_SIZE", ;
      "ISC_INFO_NUM_BUFFERS", ;
      "ISC_INFO_LIMBO", ;
      "ISC_INFO_CURRENT_MEMORY", ;
      "ISC_INFO_MAX_MEMORY", ;
      "ISC_INFO_WINDOW_TURNS", ;
      "ISC_INFO_LICENSE", ;
      "ISC_INFO_ALLOCATION", ;
      "ISC_INFO_ATTACHMENT_ID", ;
      "ISC_INFO_READ_SEQ_COUNT", ;
      "ISC_INFO_READ_IDX_COUNT", ;
      "ISC_INFO_INSERT_COUNT", ;
      "ISC_INFO_UPDATE_COUNT", ;
      "ISC_INFO_DELETE_COUNT", ;
      "ISC_INFO_BACKOUT_COUNT", ;
      "ISC_INFO_PURGE_COUNT", ;
      "ISC_INFO_EXPUNGE_COUNT", ;
      "ISC_INFO_SWEEP_INTERVAL", ;
      "ISC_INFO_ODS_VERSION", ;
      "ISC_INFO_ODS_MINOR_VERSION", ;
      "ISC_INFO_NO_RESERVE", ;
      "ISC_INFO_LOGFILE", ;
      "ISC_INFO_CUR_LOGFILE_NAME", ;
      "ISC_INFO_CUR_LOG_PART_OFFSET", ;
      "ISC_INFO_NUM_WAL_BUFFERS", ;
      "ISC_INFO_WAL_BUFFER_SIZE", ;
      "ISC_INFO_WAL_CKPT_LENGTH", ;
      "ISC_INFO_WAL_CUR_CKPT_INTERVAL", ;
      "ISC_INFO_WAL_PRV_CKPT_FNAME", ;
      "ISC_INFO_WAL_PRV_CKPT_POFFSET", ;
      "ISC_INFO_WAL_RECV_CKPT_FNAME", ;
      "ISC_INFO_WAL_RECV_CKPT_POFFSET", ;
      "ISC_INFO_WAL_GRPC_WAIT_USECS", ;
      "ISC_INFO_WAL_NUM_IO", ;
      "ISC_INFO_WAL_AVG_IO_SIZE", ;
      "ISC_INFO_WAL_NUM_COMMITS", ;
      "ISC_INFO_WAL_AVG_GRPC_SIZE", ;
      "ISC_INFO_FORCED_WRITES", ;
      "ISC_INFO_USER_NAMES", ;
      "ISC_INFO_PAGE_ERRORS", ;
      "ISC_INFO_RECORD_ERRORS", ;
      "ISC_INFO_BPAGE_ERRORS", ;
      "ISC_INFO_DPAGE_ERRORS", ;
      "ISC_INFO_IPAGE_ERRORS", ;
      "ISC_INFO_PPAGE_ERRORS", ;
      "ISC_INFO_TPAGE_ERRORS", ;
      "ISC_INFO_SET_PAGE_BUFFERS", ;
      "ISC_INFO_DB_SQL_DIALECT", ;
      "ISC_INFO_DB_READ_ONLY", ;
      "ISC_INFO_DB_SIZE_IN_PAGES", ;
      "FRB_INFO_ATT_CHARSET", ;
      "ISC_INFO_DB_CLASS", ;
      "ISC_INFO_FIREBIRD_VERSION", ;
      "ISC_INFO_OLDEST_TRANSACTION", ;
      "ISC_INFO_OLDEST_ACTIVE", ;
      "ISC_INFO_OLDEST_SNAPSHOT", ;
      "ISC_INFO_NEXT_TRANSACTION", ;
      "ISC_INFO_DB_PROVIDER", ;
      "ISC_INFO_ACTIVE_TRANSACTIONS", ;
      "ISC_INFO_ACTIVE_TRAN_COUNT", ;
      "ISC_INFO_CREATION_DATE", ;
      "ISC_INFO_DB_FILE_SIZE", ;
      "FB_INFO_PAGE_CONTENTS" }
      
   LOCAL aInfoVals := { ;
      ISC_INFO_DB_ID, ;
      ISC_INFO_READS, ;
      ISC_INFO_WRITES, ;
      ISC_INFO_FETCHES, ;
      ISC_INFO_MARKS, ;
      ISC_INFO_IMPLEMENTATION, ;
      ISC_INFO_ISC_VERSION, ;
      ISC_INFO_BASE_LEVEL, ;
      ISC_INFO_PAGE_SIZE, ;
      ISC_INFO_NUM_BUFFERS, ;
      ISC_INFO_LIMBO, ;
      ISC_INFO_CURRENT_MEMORY, ;
      ISC_INFO_MAX_MEMORY, ;
      ISC_INFO_WINDOW_TURNS, ;
      ISC_INFO_LICENSE, ;
      ISC_INFO_ALLOCATION, ;
      ISC_INFO_ATTACHMENT_ID, ;
      ISC_INFO_READ_SEQ_COUNT, ;
      ISC_INFO_READ_IDX_COUNT, ;
      ISC_INFO_INSERT_COUNT, ;
      ISC_INFO_UPDATE_COUNT, ;
      ISC_INFO_DELETE_COUNT, ;
      ISC_INFO_BACKOUT_COUNT, ;
      ISC_INFO_PURGE_COUNT, ;
      ISC_INFO_EXPUNGE_COUNT, ;
      ISC_INFO_SWEEP_INTERVAL, ;
      ISC_INFO_ODS_VERSION, ;
      ISC_INFO_ODS_MINOR_VERSION, ;
      ISC_INFO_NO_RESERVE, ;
      ISC_INFO_LOGFILE, ;
      ISC_INFO_CUR_LOGFILE_NAME, ;
      ISC_INFO_CUR_LOG_PART_OFFSET, ;
      ISC_INFO_NUM_WAL_BUFFERS, ;
      ISC_INFO_WAL_BUFFER_SIZE, ;
      ISC_INFO_WAL_CKPT_LENGTH, ;
      ISC_INFO_WAL_CUR_CKPT_INTERVAL, ;
      ISC_INFO_WAL_PRV_CKPT_FNAME, ;
      ISC_INFO_WAL_PRV_CKPT_POFFSET, ;
      ISC_INFO_WAL_RECV_CKPT_FNAME, ;
      ISC_INFO_WAL_RECV_CKPT_POFFSET, ;
      ISC_INFO_WAL_GRPC_WAIT_USECS, ;
      ISC_INFO_WAL_NUM_IO, ;
      ISC_INFO_WAL_AVG_IO_SIZE, ;
      ISC_INFO_WAL_NUM_COMMITS, ;
      ISC_INFO_WAL_AVG_GRPC_SIZE, ;
      ISC_INFO_FORCED_WRITES, ;
      ISC_INFO_USER_NAMES, ;
      ISC_INFO_PAGE_ERRORS, ;
      ISC_INFO_RECORD_ERRORS, ;
      ISC_INFO_BPAGE_ERRORS, ;
      ISC_INFO_DPAGE_ERRORS, ;
      ISC_INFO_IPAGE_ERRORS, ;
      ISC_INFO_PPAGE_ERRORS, ;
      ISC_INFO_TPAGE_ERRORS, ;
      ISC_INFO_SET_PAGE_BUFFERS, ;
      ISC_INFO_DB_SQL_DIALECT, ;
      ISC_INFO_DB_READ_ONLY, ;
      ISC_INFO_DB_SIZE_IN_PAGES, ;
      FRB_INFO_ATT_CHARSET, ;
      ISC_INFO_DB_CLASS, ;
      ISC_INFO_FIREBIRD_VERSION, ;
      ISC_INFO_OLDEST_TRANSACTION, ;
      ISC_INFO_OLDEST_ACTIVE, ;
      ISC_INFO_OLDEST_SNAPSHOT, ;
      ISC_INFO_NEXT_TRANSACTION, ;
      ISC_INFO_DB_PROVIDER, ;
      ISC_INFO_ACTIVE_TRANSACTIONS, ;
      ISC_INFO_ACTIVE_TRAN_COUNT, ;
      ISC_INFO_CREATION_DATE, ;
      ISC_INFO_DB_FILE_SIZE, ;
      FB_INFO_PAGE_CONTENTS }
   
   AltD()

   CLEAR SCREEN
   @ 0, 0 SAY "Database:  " GET cDatabase PICTURE "@S250 XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
   @ 1, 0 SAY "User name: " GET cUserName PICTURE "@S32 XXXXXXXXXXXXXXXX"
   @ 2, 0 SAY "Password:  " GET cPassword PICTURE "@S32 XXXXXXXXXXXXXXXX"
   READ
   
   IF LastKey() == K_ESC
      RETURN NIL
   ENDIF
   
   CLEAR SCREEN
   
   ? "Connect database..."
   oDatabase := TFBServer():New( AllTrim( cDatabase ), AllTrim( cUserName ), AllTrim( cPassword ), 3 )

   IF oDatabase:NetErr()
      ? oDatabase:Error()
      QUIT
   ENDIF


   n := 1
   
   DO WHILE n <> 0
      @ 0, 0 CLEAR TO MaxRow(), MaxCol()
      @ 0, 0 TO MaxRow(), MaxCol()
      n := AChoice( 1, 1, MaxRow() - 1, MaxCol() - 1, aInfoList )
      IF n > 0
         aRes := oDatabase:GetInfo( { aInfoVals[ n ] } )
         IF HB_ISARRAY( aRes ) .AND. Len( aRes ) > 0
            aDispRes := Array( Len( aRes[ 1 ] ) )
            FOR i := 1 TO Len( aRes[ 1 ] )
               IF HB_ISNUMERIC( aRes[ 1 ][ i ] )
                  aDispRes[ i ] := Str( aRes[ 1 ][ i ] )
               ELSE
                  aDispRes[ i ] := aRes[ 1 ][ i ]
               ENDIF
            NEXT
            @ 5, 5 CLEAR TO MaxRow() - 5, MaxCol() - 5
            @ 5, 5 TO MaxRow() - 5, MaxCol() - 5
            AChoice( 6, 6, MaxRow() - 6, MaxCol() - 6, aDispRes )
         ENDIF
      ENDIF
   ENDDO

   RETURN NIL