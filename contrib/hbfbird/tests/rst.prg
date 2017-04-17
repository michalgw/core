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
   LOCAL nPS := 0, nPB := 0
   AltD()

   CLEAR SCREEN
   @  0, 0 SAY "Service:     " GET cService  PICTURE "@S250 XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
   @  1, 0 SAY "User name:   " GET cUserName PICTURE "@S32 XXXXXXXXXXXXXXXX"
   @  2, 0 SAY "Password:    " GET cPassword PICTURE "@S32 XXXXXXXXXXXXXXXX"
   @  3, 0 SAY "Database:    " GET cDatabase PICTURE "@S250 XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
   @  4, 0 SAY "Backup file: " GET cBkpFile  PICTURE "@S250 XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
   @  5, 0 SAY "Page buffers:" GET nPB
   @  6, 0 SAY "Page size   :" GET nPS
   @  7, 0 SAY "Options:"
   @  8, 0 SAY "isc_spb_res_deactivate_idx" GET l1 CHECKBOX
   @  9, 0 SAY "isc_spb_res_no_shadow     " GET l2 CHECKBOX
   @ 10, 0 SAY "isc_spb_res_no_validity   " GET l3 CHECKBOX
   @ 11, 0 SAY "isc_spb_res_one_at_a_time " GET l4 CHECKBOX
   @ 12, 0 SAY "isc_spb_res_replace       " GET l5 CHECKBOX
   @ 13, 0 SAY "isc_spb_res_create        " GET l6 CHECKBOX
   @ 14, 0 SAY "isc_spb_res_use_all_space " GET l7 CHECKBOX
   READ
   
   IF LastKey() == K_ESC
      RETURN NIL
   ENDIF
   
   CLEAR SCREEN
   
   ? "Connect service..."
   oService := TFbRestoreService():New( AllTrim( cService ), AllTrim( cUserName ), AllTrim( cPassword ) )

   oService:BackupFile := AllTrim( cBkpFile )
   oService:DatabaseName := AllTrim( cDatabase )
   oService:Verbose := .T.
   oService:PageBuffers := nPB
   oService:PageSize := nPS

   IF l1
      AAdd( oService:Options, ISC_SPB_RES_DEACTIVATE_IDX )
   ENDIF
   IF l2
      AAdd( oService:Options, ISC_SPB_RES_NO_SHADOW )
   ENDIF
   IF l3
      AAdd( oService:Options, ISC_SPB_RES_NO_VALIDITY )
   ENDIF
   IF l4
      AAdd( oService:Options, ISC_SPB_RES_ONE_AT_A_TIME )
   ENDIF
   IF l5
      AAdd( oService:Options, ISC_SPB_RES_REPLACE )
   ENDIF
   IF l6
      AAdd( oService:Options, ISC_SPB_RES_CREATE )
   ENDIF
   IF l7
      AAdd( oService:Options, ISC_SPB_RES_USE_ALL_SPACE )
   ENDIF

   oService:Active := .T.

   ? "Start service"
   oService:StartService()

   DO WHILE ! oService:Eof
      ? oService:GetNextLine()
   ENDDO

   ? "END"

   RETURN NIL

