#include "hbfbird.ch"
#include "hbmemvar.ch"
#include "error.ch"
#include "dbstruct.ch"
#include "fbutils.ch"

FUNCTION fb_Query( oDatabase, oTransaction, cSQL, aParams, lRowAsHash )

   LOCAL oSQL
   LOCAL lOwnTransaction := Empty( oTransaction )
   LOCAL aResult := {}

   hb_default( @lRowAsHash, .F. )

   IF lOwnTransaction
      oTransaction := TfbTransaction():New( oDatabase )
      oTransaction:Start()
   ENDIF

   oSQL := TFbSQL():New( oDatabase, oTransaction, cSQL )

   oSQL:Prepare()

   IF HB_ISARRAY( aParams )
      oSQL:SetParams( aParams )
   ENDIF

   oSQL:Execute()

   DO WHILE oSQL:Fetch() == 0
      AAdd( aResult, oSQL:GetRow( lRowAsHash ) )
   ENDDO

   oSQL:Close()

   IF lOwnTransaction
      oTransaction:Commit()
   ENDIF

   RETURN aResult

/*----------------------------------------------------------------------*/

FUNCTION fb_QueryValue( oDatabase, oTransaction, cSQL, aParams, cnField )

   LOCAL oSQL
   LOCAL lOwnTransaction := Empty( oTransaction )
   LOCAL xResult := NIL

   hb_default( @cnField, 1 )

   IF lOwnTransaction
      oTransaction := TfbTransaction():New( oDatabase )
      oTransaction:Start()
   ENDIF

   oSQL := TFbSQL():New( oDatabase, oTransaction, cSQL )

   oSQL:Prepare()

   IF HB_ISARRAY( aParams )
      oSQL:SetParams( aParams )
   ENDIF

   oSQL:Execute()

   IF oSQL:Fetch() == 0
      xResult := oSQL[ cnField ]
   ENDIF

   oSQL:Close()

   IF lOwnTransaction
      oTransaction:Commit()
   ENDIF

   RETURN xResult

/*----------------------------------------------------------------------*/

FUNCTION fb_Execute( oDatabase, oTransaction, cSQL, aParams )

   LOCAL oSQL
   LOCAL lOwnTransaction := Empty( oTransaction )

   IF lOwnTransaction
      oTransaction := TfbTransaction():New( oDatabase )
      oTransaction:Start()
   ENDIF

   oSQL := TFbSQL():New( oDatabase, oTransaction, cSQL )

   oSQL:Prepare()

   IF HB_ISARRAY( aParams )
      oSQL:SetParams( aParams )
   ENDIF

   oSQL:Execute()

   oSQL:Close()

   IF lOwnTransaction
      oTransaction:Commit()
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION fb_mvCreateTable( oDatabase, oTransaction )

   oDatabase:Execute( ;
      "CREATE TABLE HB_MEM ( FILENAME VARCHAR(128), VARNAME VARCHAR(64), VALTYPE VARCHAR(1), VAL VARCHAR(1024), " ;
      + "PRIMARY KEY (FILENAME, VARNAME) )", oTransaction )

   RETURN .T.

/*----------------------------------------------------------------------*/

FUNCTION fb_mvSave( oDatabase, oTransaction, cFileName, cMask, lIncludeMask )

   LOCAL nCount
   LOCAL xValue
   LOCAL cName
   LOCAL nScope
   LOCAL lMatch

   LOCAL aVars
   LOCAL oSQL
   LOCAL tmp
   LOCAL lOwnTr := Empty( oTransaction )

   LOCAL oError

   IF HB_ISSTRING( cFileName )

      IF ! HB_ISSTRING( cMask ) .OR. ;
         Empty( cMask ) .OR. hb_LeftEq( cMask, "*" )
         cMask := "*"
      ENDIF

      hb_default( @lIncludeMask, .T. )

      aVars := {}

      FOR EACH nScope IN { HB_MV_PUBLIC, HB_MV_PRIVATE }
         nCount := __mvDbgInfo( nScope )
         FOR tmp := 1 TO nCount
            xValue := __mvDbgInfo( nScope, tmp, @cName )
            IF ValType( xValue ) $ "CNDTL"
               lMatch := hb_WildMatchI( cMask, cName )
               IF iif( lIncludeMask, lMatch, ! lMatch )
                  AAdd( aVars, { cName, xValue } )
               ENDIF
            ENDIF
         NEXT
      NEXT

      IF lOwnTr
         oTransaction := TFbTransaction():New( oDatabase )
         oTransaction:Start()
      ENDIF

      oSQL := TFbSQL():New( oDatabase, oTransaction, ;
         "UPDATE OR INSERT INTO HB_MEM (FILENAME, VARNAME, VALTYPE, VAL) " ;
         + "VALUES (?, ?, ?, ?) MATCHING (FILENAME, VARNAME)" )

      oSQL:Prepare()
      
      AEval( aVars, { | aVal | 
         oSQL:SetParam( 1, Upper( cFileName ) )
         oSQL:SetParam( 2, Upper( aVal[ 1 ] ) )
         oSQL:SetParam( 3, ValType( aVal[ 2 ] ) )
         SWITCH ValType( aVal[ 2 ] )
         CASE "C"
            oSQL:SetParam( 4, aVal[ 2 ] )
            EXIT
         CASE "N"
            oSQL:SetParam( 4, Str( aVal[ 2 ] ) )
            EXIT
         CASE "D"
            oSQL:SetParam( 4, DToS( aVal[ 2 ] ) )
            EXIT
         CASE "T"
            oSQL:SetParam( 4, hb_TToS( aVal[ 2 ] ) )
            EXIT
         CASE "L"
            oSQL:SetParam( 4, iif( aVal[ 2 ], "T", "F" ) )
            EXIT
         ENDSWITCH
         oSQL:Execute()
         RETURN NIL
      } )

      oSQL:Close()
      
      IF lOwnTr
         oTransaction:Commit()
      ENDIF

   ELSE
      oError := ErrorNew()

      oError:severity    := ES_ERROR
      oError:genCode     := EG_ARG
      oError:subSystem   := "BASE"
      oError:subCode     := 2008
      oError:canRetry    := .F.
      oError:canDefault  := .F.
      oError:Args        := hb_AParams()
      oError:operation   := ProcName()

      Eval( ErrorBlock(), oError )
   ENDIF

   RETURN NIL

FUNCTION fb_mvRestore( oDatabase, oTransaction, cFileName, lAdditive, cMask, lIncludeMask )

   LOCAL item
   LOCAL cName
   LOCAL lMatch

   LOCAL aVars
   LOCAL xValue

   LOCAL lOwnTr := Empty( oTransaction )

   LOCAL oError

   IF HB_ISSTRING( cFileName )

      IF ! hb_defaultValue( lAdditive, .T. )
         __mvClear()
      ENDIF

      IF ! HB_ISSTRING( cFileName ) .OR. ;
         Empty( cMask ) .OR. hb_LeftEq( cMask, "*" )
         cMask := "*"
      ENDIF

      hb_default( @lIncludeMask, .T. )

      IF lOwnTr
         oTransaction := TFbTransaction():New( oDatabase )
         oTransaction:Start()
      ENDIF

      aVars := fb_Query( oDatabase, oTransaction, ;
         "SELECT VARNAME, VAL, VALTYPE FROM HB_MEM WHERE FILENAME = ? AND VARNAME " ;
         + iif( At( cMask, "*" ) > 0, "LIKE", "=" ) + " ?", ;
         { Upper( cFileName ), StrTran( Upper( cMask ), "*", "%" ) } )

      IF lOwnTr
         oTransaction:Commit()
      ENDIF

      AEval( aVars, { | aVar |
         SWITCH aVar[ 3 ]
         CASE "N"
            aVar[ 2 ] := Val( aVar[ 2 ] )
            EXIT
         CASE "D"
            aVar[ 2 ] := SToD( aVar[ 2 ] )
            EXIT
         CASE "T"
            aVar[ 2 ] := hb_SToT( aVar[ 2 ] )
            EXIT
         CASE "L"
            aVar[ 2 ] := iif( aVar[ 2 ] == "T", .T., .F. )
            EXIT
         ENDSWITCH
         RETURN NIL
      } )

      xValue := NIL

      IF HB_ISARRAY( aVars )
         FOR EACH item IN aVars
            IF HB_ISARRAY( item ) .AND. Len( item ) == 3 .AND. ;
               HB_ISSTRING( item[ 1 ] ) .AND. ;
               ! Empty( item[ 1 ] )

               cName := item[ 1 ]
               lMatch := hb_WildMatchI( cMask, cName )
               IF iif( lIncludeMask, lMatch, ! lMatch )
                  IF xValue == NIL
                     xValue := item[ 2 ]
                  ENDIF
                  __mvPut( cName, item[ 2 ] )
               ENDIF
            ENDIF
         NEXT
         __mvSetBase()
      ENDIF

      RETURN xValue
   ELSE
      oError := ErrorNew()

      oError:severity    := ES_ERROR
      oError:genCode     := EG_ARG
      oError:subSystem   := "BASE"
      oError:subCode     := 2007
      oError:canRetry    := .F.
      oError:canDefault  := .F.
      oError:Args        := hb_AParams()
      oError:operation   := ProcName()

      Eval( ErrorBlock(), oError )
   ENDIF

   RETURN NIL

FUNCTION fb_mvDelete( oDatabase, oTransaction, cFileName, cMask )

   LOCAL oError
   LOCAL lOwnTr := Empty( oTransaction )
   LOCAL cSQL
   LOCAL aParams

   IF HB_ISSTRING( cFileName )

      IF lOwnTr
         oTransaction := TFbTransaction():New( oDatabase )
         oTransaction:Start()
      ENDIF

      aParams := {}
      AAdd( aParams, Upper( cFileName ) )

      cSQL := "DELETE FROM HB_MEM WHERE FILENAME = ?"
      IF HB_ISCHAR( cMask )
         cSQL := cSQL + " AND VARNAME " + iif( At( cMask, "*" ) > 0, "LIKE", "=" ) + " ?"
         AAdd( aParams, StrTran( Upper( cMask ), "*", "%" ) )
      ENDIF

      fb_Execute( oDatabase, oTransaction, cSQL, aParams )

      IF lOwnTr
         oTransaction:Commit()
      ENDIF

   ELSE
      oError := ErrorNew()

      oError:severity    := ES_ERROR
      oError:genCode     := EG_ARG
      oError:subSystem   := "BASE"
      oError:subCode     := 2007
      oError:canRetry    := .F.
      oError:canDefault  := .F.
      oError:Args        := hb_AParams()
      oError:operation   := ProcName()

      Eval( ErrorBlock(), oError )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION fb_CreateTable( oDatabase, oTransaction, cTableName, aStructure )

   LOCAL lOwnTransaction := Empty( oTransaction )
   LOCAL cFldSQL := ""
   LOCAL cAutoincField := NIL
   
   AEval( aStructure, { | aField |
      IF Len( cFldSQL ) > 0
         cFldSQL := cFldSQL + ", "
      ENDIF
      SWITCH aField[ DBS_TYPE ]
      CASE "C"
         cFldSQL := cFldSQL + aField[ DBS_NAME ] + " VARCHAR(" + AllTrim( Str( aField[ DBS_LEN ] ) ) + ")"
         EXIT
      CASE "N"
         cFldSQL := cFldSQL + aField[ DBS_NAME ]
         IF aField[ DBS_DEC ] == 0
            cFldSQL := cFldSQL + " INTEGER"
         ELSE
            cFldSQL := cFldSQL + " DECIMAL(" + AllTrim( Str( aField[ DBS_LEN ] ) ) ;
               + "," + AllTrim( Str( aField[ DBS_DEC ] ) ) + ")"
         ENDIF
         EXIT
      CASE "D"
         cFldSQL := cFldSQL + aField[ DBS_NAME ] + " DATE"
         EXIT
      CASE "T"
         cFldSQL := cFldSQL + aField[ DBS_NAME ] + " TIMESTAMP"         
         EXIT
      CASE "L"
         cFldSQL := cFldSQL + aField[ DBS_NAME ] + " SMALLINT"
         EXIT
      CASE "+"
         cFldSQL := cFldSQL + aField[ DBS_NAME ] + " INTEGER"
         cAutoincField := aField[ DBS_NAME ]
         EXIT
      ENDSWITCH
      RETURN NIL
   } )

   IF HB_ISCHAR( cAutoincField )
      cFldSQL := cFldSQL + ", PRIMARY KEY (" + cAutoincField + ")"
   ENDIF

   IF lOwnTransaction
      oTransaction := TfbTransaction():New( oDatabase )
      oTransaction:Start()
   ENDIF

   fb_Execute( oDatabase, oTransaction, "CREATE TABLE " + cTableName + " ( " + cFldSQL + " ) " )

   IF HB_ISCHAR( cAutoincField )
      fb_Execute( oDatabase, oTransaction, "CREATE GENERATOR GEN_" + cTableName )
      fb_Execute( oDatabase, oTransaction, ;
         "CREATE TRIGGER " + cTableName + "_BI FOR " + cTableName + " BEFORE INSERT AS BEGIN " ;
         + "NEW." + cAutoincField + " = GEN_ID( GEN_" + cTableName + ", 1 ); END;" )
   ENDIF

   IF lOwnTransaction
      oTransaction:Commit()
   ENDIF

   RETURN NIL

FUNCTION fb_ListDBObjects( oDatabase, oTransaction, nObjectType, cObjectName )

   LOCAL lOwnTransaction := Empty( oTransaction )
   LOCAL cSQL
   LOCAL aRes, aFixed
   LOCAL aParams := NIL

   hb_default( @nObjectType, FBOT_TABLES )

   SWITCH nObjectType
   CASE FBOT_TABLES
      cSQL := "SELECT RDB$RELATION_NAME FROM RDB$RELATIONS WHERE RDB$RELATION_TYPE = 0 AND ( RDB$SYSTEM_FLAG = 0 OR RDB$SYSTEM_FLAG IS NULL )"
      EXIT
   CASE FBOT_VIEWS
      cSQL := "SELECT RDB$RELATION_NAME FROM RDB$RELATIONS WHERE RDB$RELATION_TYPE = 1 AND ( RDB$SYSTEM_FLAG = 0 OR RDB$SYSTEM_FLAG IS NULL )"
      EXIT
   CASE FBOT_STOREDPROCS
      cSQL := "SELECT RDB$PROCEDURE_NAME FROM RDB$PROCEDURES WHERE RDB$SYSTEM_FLAG = 0 OR RDB$SYSTEM_FLAG IS NULL"
      EXIT
   CASE FBOT_GENERATORS
      cSQL := "SELECT RDB$GENERATOR_NAME FROM RDB$GENERATORS WHERE RDB$SYSTEM_FLAG = 0 OR RDB$SYSTEM_FLAG IS NULL"
      EXIT
   CASE FBOT_FIELDS
      cSQL := "SELECT RDB$FIELD_NAME FROM RDB$RELATION_FIELDS WHERE RDB$RELATION_NAME = ? AND ( RDB$SYSTEM_FLAG = 0 OR RDB$SYSTEM_FLAG IS NULL )"
      aParams := { Upper( cObjectName ) }
      EXIT
   OTHERWISE
      RETURN NIL
   ENDSWITCH

   IF lOwnTransaction
      oTransaction := TfbTransaction():New( oDatabase )
      oTransaction:Start()
   ENDIF

   aRes := fb_Query( oDatabase, oTransaction, cSQL, aParams )

   IF lOwnTransaction
      oTransaction:Commit()
   ENDIF

   aFixed := {}
   AEval( aRes, { | aRow | AAdd( aFixed, AllTrim( aRow[ 1 ] ) ) } )

   RETURN aFixed

/*----------------------------------------------------------------------*/

FUNCTION fb_DataSetCreate( oDatabase, oTransaction, cTableName, cKeyField )

   LOCAL oDataSet := TFbDataSet():New( oDatabase, oTransaction, "SELECT * FROM " + cTableName )
   LOCAL cSQL := ""
   LOCAL aSQLFlds := {}
   LOCAL aSQLFlds2 := {}
   LOCAL cParams := ""
   LOCAL aFields

   aFields := fb_ListDBObjects( oDatabase, oTransaction, FBOT_FIELDS, cTableName )

   oDataSet:DeleteSQL := { "DELETE FROM " + cTableName + " WHERE " + cKeyField + " = ?", { cKeyField } }

   AEval( aFields, { | cFld |
      IF Upper( cFld ) <> Upper( cKeyField )
         cSQL := cSQL + iif( Len( cSQL ) > 0, ",", "" )
         cSQL := cSQL + cFld
         AAdd( aSQLFlds, cFld )
         AAdd( aSQLFlds2, cFld )
         cParams := cParams + iif( Len( cParams ) > 0, ",", "" )
         cParams := cParams + "?"
      ENDIF
      RETURN NIL
   } )
   oDataSet:InsertSQL := { "INSERT INTO " + cTableName + " ( " + cSQL + " ) VALUES ( " ;
      + cParams + " ) RETURNING " + cKeyField, aSQLFlds }

   cSQL := ""
   AEval( aSQLFlds , { | cFld |
      cSQL := cSQL + iif( Len( cSQL ) > 0, ",", "" )
      cSQL := cSQL + cFld + " = ?"
      RETURN NIL
   } )
   AAdd( aSQLFlds2, cKeyField )
   oDataSet:UpdateSQL := { "UPDATE " + cTableName + " SET " + cSQL + " WHERE " + cKeyField + " = ?" , aSQLFlds2 }

   oDataSet:RefreshSQL := { "SELECT * FROM " + cTableName + " WHERE " + cKeyField + " = ?", { cKeyField } }

   RETURN oDataSet

/*----------------------------------------------------------------------*/

