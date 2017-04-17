#require "hbfbird"
#include "hbfbird.ch"
#include "inkey.ch"

REQUEST HB_GT_WVT_DEFAULT

FUNCTION Main()

   LOCAL cDatabase := PadR( "d:\employee.fdb", 250 )
   LOCAL cUserName := PadR( "SYSDBA", 32 )
   LOCAL cPassword := PadR( "masterkey", 32 )
   LOCAL cService := PadR( "localhost:service_mgr", 250 )
   LOCAL cBkpFile := PadR( "d:\test.fbk", 250 )
   LOCAL oService
   LOCAL GetList := {}
   LOCAL l1 := .F., l2 := .F., l3 := .F., l4 := .F., l5 := .F., l6 := .F., l7 := .F.
   LOCAL nBF := 0
   AltD()

   CLEAR SCREEN
   @  0, 0 SAY "Service:        " GET cService  PICTURE "@S250 XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
   @  1, 0 SAY "User name:      " GET cUserName PICTURE "@S32 XXXXXXXXXXXXXXXX"
   @  2, 0 SAY "Password:       " GET cPassword PICTURE "@S32 XXXXXXXXXXXXXXXX"
   @  3, 0 SAY "Database:       " GET cDatabase PICTURE "@S250 XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
   @  4, 0 SAY "Backup file:    " GET cBkpFile  PICTURE "@S250 XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
   @  5, 0 SAY "Blocking factor:" GET nBF
   @  6, 0 SAY "Options:"
   @  7, 0 SAY "Ignore checksum   " GET l1 CHECKBOX
   @  8, 0 SAY "Ignore limbo      " GET l2 CHECKBOX
   @  9, 0 SAY "Metadata only     " GET l3 CHECKBOX
   @ 10, 0 SAY "No garbage collect" GET l4 CHECKBOX
   @ 11, 0 SAY "Old descriptions  " GET l5 CHECKBOX
   @ 12, 0 SAY "Non transportable " GET l6 CHECKBOX
   @ 13, 0 SAY "Convert           " GET l7 CHECKBOX
   READ
   
   IF LastKey() == K_ESC
      RETURN NIL
   ENDIF
   
   CLEAR SCREEN
   
   ? "Connect service..."
   oService := TFbBackupService():New( AllTrim( cService ), AllTrim( cUserName ), AllTrim( cPassword ) )

   oService:BackupFile := AllTrim( cBkpFile )
   oService:DatabaseName := AllTrim( cDatabase )
   oService:Verbose := .T.
   oService:BlockingFactor := nBF

   IF l1
      AAdd( oService:Options, ISC_SPB_BKP_IGNORE_CHECKSUMS )
   ENDIF
   IF l2
      AAdd( oService:Options, ISC_SPB_BKP_IGNORE_LIMBO )
   ENDIF
   IF l3
      AAdd( oService:Options, ISC_SPB_BKP_METADATA_ONLY )
   ENDIF
   IF l4
      AAdd( oService:Options, ISC_SPB_BKP_NO_GARBAGE_COLLECT )
   ENDIF
   IF l5
      AAdd( oService:Options, ISC_SPB_BKP_OLD_DESCRIPTIONS )
   ENDIF
   IF l6
      AAdd( oService:Options, ISC_SPB_BKP_NON_TRANSPORTABLE )
   ENDIF
   IF l7
      AAdd( oService:Options, ISC_SPB_BKP_CONVERT )
   ENDIF

   oService:Active := .T.

   ? "Start service"
   oService:StartService()

   DO WHILE ! oService:Eof
      ? oService:GetNextLine()
   ENDDO

   ? "END"

   RETURN NIL
