/*
 * Firebird RDBMS low-level (client API) interface code.
 *
 * Copyright 2003 Rodrigo Moreno rodrigo_moreno@yahoo.com
 *           2017 Michal Gawrycki ( info / gmsystems / pl )
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbclass.ch"
#include "hbfbird.ch"

CREATE CLASS TFbServer

   VAR db
   VAR trans
   VAR StartedTrans
   VAR nError
   VAR lError
   VAR dialect

   METHOD   New( cServer, cUser, cPassword, nDialect, aParams )
   METHOD   Destroy()  INLINE FBClose( ::db )
   METHOD   Close()    INLINE FBClose( ::db )

   METHOD TableExists( cTable )
   METHOD ListTables()
   METHOD TableStruct( cTable )

   METHOD   StartTransaction( aParams )
   METHOD   Commit()
   METHOD   Rollback()

   METHOD   Execute( cQuery, oTransaction )
   METHOD   Query( cQuery, oTransaction )

   METHOD   Update( oRow, cWhere, oTransaction )
   METHOD   Delete( oRow, cWhere, oTransaction )
   METHOD   Append( oRow, oTransaction )

   METHOD NetErr()   INLINE ::lError
   METHOD Error()    INLINE FBError( ::nError )
   METHOD ErrorNo()  INLINE ::nError

   METHOD   GetInfo( aInfos )
   METHOD   BytesPerChar()

   HIDDEN:

   VAR nBytesPerChar INIT 0

ENDCLASS

METHOD New( cServer, cUser, cPassword, nDialect, aParams ) CLASS TFbServer

   hb_default( @nDialect, 1 )

   ::lError := .F.
   ::nError := 0
   ::StartedTrans := .F.
   ::Dialect := nDialect

   ::db := FBConnect( cServer, cUser, cPassword, aParams )

   IF HB_ISNUMERIC( ::db )
      ::lError := .T.
      ::nError := ::db
   ENDIF

   RETURN Self

METHOD StartTransaction( aParams ) CLASS TFbServer

   LOCAL result := .F.

   ::trans := FBStartTransaction( ::db, aParams )

   IF HB_ISNUMERIC( ::trans )
      ::lError := .T.
      ::nError := ::trans
   ELSE
      result := .T.
      ::lError := .F.
      ::lnError := 0
      ::StartedTrans := .T.
   ENDIF

   RETURN result

METHOD Rollback() CLASS TFbServer

   LOCAL result := .F.
   LOCAL n

   IF ::StartedTrans
      IF ( n := FBRollback( ::trans ) ) < 0
         ::lError := .T.
         ::nError := n
      ELSE
         ::lError := .F.
         ::nError := 0
         result := .T.
         ::StartedTrans := .F.
      ENDIF
   ENDIF

   RETURN result

METHOD Commit() CLASS TFbServer

   LOCAL result := .F.
   LOCAL n

   IF ::StartedTrans
      IF ( n := FBCommit( ::trans ) ) < 0
         ::lError := .T.
         ::nError := n
      ELSE
         ::lError := .F.
         ::nError := 0
         result := .T.
         ::StartedTrans := .F.
      ENDIF
   ENDIF

   RETURN result

METHOD Execute( cQuery, oTransaction ) CLASS TFbServer

   LOCAL result
   LOCAL n

   cQuery := RemoveSpaces( cQuery )

   IF HB_ISOBJECT( oTransaction )
      IF ! oTransaction:Active
         oTransaction:Start()
      ENDIF
      n := FBExecute( ::db, cQuery, ::dialect, oTransaction:Handle )
   ELSE
      IF ::StartedTrans
         n := FBExecute( ::db, cQuery, ::dialect, ::trans )
      ELSE
         n := FBExecute( ::db, cQuery, ::dialect )
      ENDIF
   ENDIF

   IF n < 0
      ::lError := .T.
      ::nError := n
      result := .F.
   ELSE
      ::lError := .F.
      ::nError := 0
      result := .T.
   ENDIF

   RETURN result

METHOD Query( cQuery, oTransaction ) CLASS TFbServer
   RETURN TFBQuery():New( ::db, cQuery, ::dialect, oTransaction )

METHOD TableExists( cTable ) CLASS TFbServer

   LOCAL cQuery
   LOCAL result := .F.
   LOCAL qry

   cQuery := 'select rdb$relation_name from rdb$relations where rdb$relation_name = "' + Upper( cTable ) + '"'

   qry := FBQuery( ::db, cQuery, ::dialect )

   IF HB_ISARRAY( qry )
      result := ( FBFetch( qry ) == 0 )

      FBFree( qry )
   ENDIF

   RETURN result

METHOD ListTables() CLASS TFbServer

   LOCAL result := {}
   LOCAL cQuery
   LOCAL qry

   cQuery := 'select rdb$relation_name '
   cQuery += '  from rdb$relations '
   cQuery += ' where rdb$relation_name not like "RDB$%" '
   cQuery += '   and rdb$view_blr is null '
   cQuery += ' order by 1 '

   qry := FBQuery( ::db, RemoveSpaces( cQuery ), ::dialect )

   IF HB_ISARRAY( qry )
      DO WHILE FBFetch( qry ) == 0
         AAdd( result, FBGetData( qry, 1 ) )
      ENDDO

      FBFree( qry )
   ENDIF

   RETURN result

METHOD TableStruct( cTable ) CLASS TFbServer

   LOCAL result := {}
   LOCAL cQuery, cType, nSize, cDomain, cField, nType, nDec
   LOCAL qry

   cQuery := 'select '
   cQuery += '  a.rdb$field_name,'
   cQuery += '  b.rdb$field_type,'
   cQuery += '  b.rdb$field_length,'
   cQuery += '  b.rdb$field_scale * -1,'
   cQuery += '  a.rdb$field_source '
   cQuery += 'from '
   cQuery += '  rdb$relation_fields a, rdb$fields b '
   cQuery += 'where '
   cQuery += '  a.rdb$field_source = b.rdb$field_name '
   cQuery += '  and a.rdb$relation_name = "' + Upper( ctable ) + '" '
   cQuery += 'order by '
   cQuery += '  a.rdb$field_position '

   qry := FBQuery( ::db, RemoveSpaces( cQuery ), ::dialect )

   IF HB_ISARRAY( qry )
      DO WHILE FBFetch( qry ) == 0
         cField  := FBGetData( qry, 1 )
         nType   := Val( FBGetData( qry, 2 ) )
         nSize   := Val( FBGetData( qry, 3 ) )
         nDec    := Val( FBGetData( qry, 4 ) )
         cDomain := FBGetData( qry, 5 )

         SWITCH nType
         CASE 7 // SMALLINT
            IF "BOOL" $ cDomain
               cType := "L"
               nSize := 1
               nDec := 0
            ELSE
               cType := "N"
               nSize := 5
            ENDIF

            EXIT

         CASE 8 // INTEGER
         CASE 9
            cType := "N"
            nSize := 9
            EXIT

         CASE 10 // FLOAT
         CASE 11
            cType := "N"
            nSize := 15
            EXIT

         CASE 12 // DATE
            cType := "D"
            nSize := 8
            EXIT

         CASE 13 // TIME
            cType := "C"
            nSize := 10
            EXIT

         CASE 14 // CHAR
            cType := "C"
            EXIT

         CASE 16 // INT64
            cType := "N"
            nSize := 9
            EXIT

         CASE 27 // DOUBLE
            cType := "N"
            nSize := 15
            EXIT

         CASE 35 // TIMESTAMP
            cType := "D"
            nSize := 8
            EXIT

         CASE 37 // VARCHAR
         CASE 40
            cType := "C"
            EXIT

         CASE 261 // BLOB
            cType := "M"
            nSize := 10
            EXIT

         OTHERWISE
            cType := "C"
            nDec := 0
         ENDSWITCH

         AAdd( result, { cField, cType, nSize, nDec } )

      ENDDO

      FBFree( qry )
   ENDIF

   RETURN result

METHOD Delete( oRow, cWhere, oTransaction ) CLASS TFbServer

   LOCAL result := .F.
   LOCAL aKeys, i, nField, xField, cQuery, aTables

   aTables := oRow:GetTables()

   IF ! HB_ISNUMERIC( ::db ) .AND. Len( aTables ) == 1
      // Cannot delete joined tables

      IF cWhere == NIL
         aKeys := oRow:GetKeyField()

         cWhere := ""
         FOR i := 1 TO Len( aKeys )
            nField := oRow:FieldPos( aKeys[ i ] )
            xField := oRow:FieldGet( nField )

            cWhere += aKeys[ i ] + "=" + DataToSql( xField )

            IF i != Len( aKeys )
               cWhere += ","
            ENDIF
         NEXT
      ENDIF

      IF !( cWhere == "" )
         cQuery := 'DELETE FROM ' + aTables[ 1 ] + ' WHERE ' + cWhere

         result := ::Execute( cQuery, oTransaction )
      ENDIF
   ENDIF

   RETURN result

METHOD Append( oRow, oTransaction ) CLASS TFbServer

   LOCAL result := .F.
   LOCAL cQuery, i, aTables

   aTables := oRow:GetTables()

   IF ! HB_ISNUMERIC( ::db ) .AND. Len( aTables ) == 1
      // Can insert only one table, not in joined tables

      cQuery := 'INSERT INTO ' + aTables[ 1 ] + '('
      FOR i := 1 TO oRow:FCount()
         IF oRow:Changed( i )
            // Send only changed field
            cQuery += oRow:FieldName( i ) + ","
         ENDIF
      NEXT

      cQuery := Left( cQuery, Len( cQuery ) - 1 ) +  ") VALUES ("

      FOR i := 1 TO oRow:FCount()
         IF oRow:Changed( i )
            cQuery += DataToSql( oRow:FieldGet( i ) ) + ","
         ENDIF
      NEXT

      cQuery := Left( cQuery, Len( cQuery ) - 1  ) + ")"

      result := ::Execute( cQuery, oTransaction )
   ENDIF

   RETURN result

METHOD Update( oRow, cWhere, oTransaction ) CLASS TFbServer

   LOCAL result := .F.
   LOCAL aKeys, cQuery, i, nField, xField, aTables

   aTables := oRow:GetTables()

   IF ! HB_ISNUMERIC( ::db ) .AND. Len( aTables ) == 1
      // Can't insert joined tables

      IF cWhere == NIL
         aKeys := oRow:GetKeyField()

         cWhere := ""
         FOR i := 1 TO Len( aKeys )
            nField := oRow:FieldPos( aKeys[ i ] )
            xField := oRow:FieldGet( nField )

            cWhere += aKeys[ i ] + "=" + DataToSql( xField )

            IF i != Len( aKeys )
               cWhere += ", "
            ENDIF
         NEXT
      ENDIF

      cQuery := "UPDATE " + aTables[ 1 ] + " SET "
      FOR i := 1 TO oRow:FCount()
         IF oRow:Changed( i )
            cQuery += oRow:FieldName( i ) + " = " + DataToSql( oRow:FieldGet( i ) ) + ","
         ENDIF
      NEXT

      IF !( cWhere == "" )
         cQuery := Left( cQuery, Len( cQuery ) - 1 ) + " WHERE " + cWhere

         result := ::Execute( cQuery, oTransaction )
      ENDIF
   ENDIF

   RETURN result

METHOD GetInfo( aInfos ) CLASS TFbServer

   LOCAL aRes

   aRes := FBDBGetInfo( ::db, aInfos )

   IF ! HB_ISARRAY( aRes )
      ::lError := .T.
      ::nError := aRes
   ENDIF

   RETURN aRes

METHOD BytesPerChar() CLASS TFbServer

   LOCAL nRes := 1
   LOCAL aInfo

   IF ::nBytesPerChar == 0
      aInfo := ::GetInfo( { FRB_INFO_ATT_CHARSET } )
      IF HB_ISARRAY( aInfo ) .AND. Len( aInfo ) >= 2 ;
         .AND. aInfo[ 1 ] == FRB_INFO_ATT_CHARSET .AND. HB_ISNUMERIC( aInfo[ 2 ] )

         AEval( { { 3, 3 }, { 4, 4 }, { 5, 2 }, { 6, 2 }, { 44, 2 }, ;
            { 56, 2 }, { 57, 2 }, { 67, 2 }, { 68, 2 }, { 69, 4 } }, ;
            { | aVal | nRes := iif( aVal[ 1 ] == aInfo[ 2 ], aVal[ 2 ], nRes ) } )

         ::nBytesPerChar := nRes
      ENDIF
   ELSE
      nRes := ::nBytesPerChar
   ENDIF

   RETURN nRes

CREATE CLASS TFbTransaction

   HIDDEN:
   VAR      oDatabase
   VAR      aParams
   VAR      transHandle
   VAR      lActive        INIT .F.
   VAR      nDefaultAction INIT FBTA_COMMIT
   VAR      bBeforeAction
   VAR      bAfterAction

   PROTECTED:

   METHOD DoBeforeAction( nAction )
   METHOD DoAfterAction( nAction )

   VISIBLE:

   METHOD   New( oDatabase, aParams, lStart ) CONSTRUCTOR
   DESTRUCTOR Destroy()
   METHOD   Start()
   METHOD   Commit()
   METHOD   CommitRetaining()
   METHOD   Rollback()
   METHOD   RollbackRetaining()

   ACCESS   Database   INLINE ( ::oDatabase )
   ASSIGN   Database   METHOD SetDatabase( oValue )
   ACCESS   Params     INLINE ( ::aParams )
   ASSIGN   Params     METHOD SetParams( aValue )
   ACCESS   Active     INLINE ( ::lActive )
   ASSIGN   Active     METHOD SetActive( lValue )

   ACCESS   Handle     INLINE ( ::transHandle )

   ACCESS   DefaultAction INLINE ( ::nDefaultAction )
   ASSIGN   DefaultAction METHOD SetDefaultAction( nValue )

   ACCESS   BeforeAction INLINE ( ::bBeforeAction )
   ASSIGN   BeforeAction( bValue ) INLINE ( ::bBeforeAction := bValue )
   ACCESS   AfterAction INLINE ( ::bAfterAction )
   ASSIGN   AfterAction( bValue ) INLINE ( ::bAfterAction := bValue )

ENDCLASS

METHOD DoBeforeAction( nAction ) CLASS TFbTransaction

   IF HB_ISBLOCK( ::bBeforeAction )
      Eval( ::bBeforeAction, Self, nAction )
   ENDIF

   RETURN NIL

METHOD DoAfterAction( nAction ) CLASS TFbTransaction

   IF HB_ISBLOCK( ::bAfterAction )
      Eval( ::bAfterAction, Self, nAction )
   ENDIF

   RETURN NIL

METHOD New( oDatabase, aParams, lStart ) CLASS TFbTransaction

   ::oDatabase := oDatabase
   ::aParams := aParams

   IF HB_ISLOGICAL( lStart ) .AND. lStart
      ::Start()
   ENDIF

   RETURN Self

DESTRUCTOR Destroy() CLASS TFbTransaction

   IF ::lActive
      IF ::nDefaultAction == FBTA_COMMIT .OR. ::nDefaultAction == FBTA_COMMITRETAINING
         ::Commit()
      ELSE
         ::Rollback()
      ENDIF
   ENDIF

   RETURN

METHOD Start() CLASS TFbTransaction

   LOCAL lResult := ::lActive

   IF ! ::lActive
      ::DoBeforeAction( FBTA_START )

      ::transHandle := FBStartTransaction( ::oDatabase:db, ::aParams )
      IF HB_ISNUMERIC( ::transHandle )
         lResult := .F.
         Eval( ErrorBlock(), FBErrorNew( ::transHandle ) )
      ELSE
         ::lActive := .T.
         lResult := .T.
         ::DoAfterAction( FBTA_START )
      ENDIF

   ELSE
      Eval( ErrorBlock(), FBErrorNew( 1000, "Transaction is active" ) )
   ENDIF

   RETURN lResult

METHOD Commit() CLASS TFbTransaction

   LOCAL lResult := .F.
   LOCAL n

   IF ::lActive
      ::DoBeforeAction( FBTA_COMMIT )

      IF ( n := FBCommit( ::transHandle ) ) < 0
         Eval( ErrorBlock(), FBErrorNew( n ) )
      ELSE
         lResult := .T.
         ::lActive := .F.
         ::DoAfterAction( FBTA_COMMIT )
      ENDIF
   ELSE
      Eval( ErrorBlock(), FBErrorNew( 1000, "Transaction is not active" ) )
   ENDIF

   RETURN lResult

METHOD CommitRetaining() CLASS TFbTransaction

   LOCAL lResult := .F.
   LOCAL n

   IF ::lActive
      ::DoBeforeAction( FBTA_COMMITRETAINING )

      IF ( n := FBCommitRetaining( ::transHandle ) ) < 0
         ::lActive := .F.
         Eval( ErrorBlock(), FBErrorNew( n ) )
      ELSE
         ::lActive := .T.
         lResult := .T.
         ::DoAfterAction( FBTA_COMMITRETAINING )
      ENDIF
   ELSE
      Eval( ErrorBlock(), FBErrorNew( 1000, "Transaction is not active" ) )
   ENDIF

   RETURN lResult

METHOD Rollback() CLASS TFbTransaction

   LOCAL lResult := .F.
   LOCAL n

   IF ::lActive
      ::DoBeforeAction( FBTA_ROLLBACK )

      IF ( n := FBRollback( ::transHandle ) ) < 0
         Eval( ErrorBlock(), FBErrorNew( n ) )
      ELSE
         lResult := .T.
         ::lActive := .F.
         ::DoAfterAction( FBTA_ROLLBACK )
      ENDIF
   ELSE
      Eval( ErrorBlock(), FBErrorNew( 1000, "Transaction is not active" ) )
   ENDIF

   RETURN lResult

METHOD RollbackRetaining() CLASS TFbTransaction

   LOCAL lResult := .F.
   LOCAL n

   IF ::lActive
      ::DoBeforeAction( FBTA_ROLLBACKRETAINING )

      IF ( n := FBRollbackRetaining( ::transHandle ) ) < 0
         ::lActive := .F.
         Eval( ErrorBlock(), FBErrorNew( n ) )
      ELSE
         ::lActive := .T.
         lResult := .T.
         ::DoAfterAction( FBTA_ROLLBACK )
      ENDIF
   ELSE
      Eval( ErrorBlock(), FBErrorNew( 1000, "Transaction is not active" ) )
   ENDIF

   RETURN lResult

METHOD SetDatabase( oValue ) CLASS TFbTransaction

   IF ! ::Active
      ::oDatabase := oValue
   ELSE
      Eval( ErrorBlock(), FBErrorNew( 1000, "Can not change database in active transaction" ) )
   ENDIF

   RETURN NIL

METHOD SetParams( aValue ) CLASS TFbTransaction

   IF ! ::Active
      ::aParams := aValue
   ELSE
      Eval( ErrorBlock(), FBErrorNew( 1000, "Can not change params on active transaction" ) )
   ENDIF

   RETURN NIL

METHOD SetActive( lValue ) CLASS TFbTransaction

   IF lValue .AND. ! ::lActive
      ::Start()
   ELSEIF ! lValue .AND. ::Active

      SWITCH ::DefaultAction
      CASE FBTA_COMMIT
         ::Commit()
         EXIT
      CASE FBTA_COMMITRETAINING
         ::CommitRetaining()
         EXIT
      CASE FBTA_ROLLBACK
         ::Rollback()
         EXIT
      CASE FBTA_ROLLBACKRETAINING
         ::RollbackRetaining()
      ENDSWITCH

   ENDIF

   RETURN NIL

METHOD SetDefaultAction( nValue ) CLASS TFbTransaction

   IF nValue >= FBTA_COMMIT .AND. nValue <= FBTA_ROLLBACKRETAINING
      ::nDefaultAction := nValue
   ELSE
      Eval( ErrorBlock(), FBErrorNew( 1000, "Invalid value" ) )
   ENDIF

   RETURN NIL

CREATE CLASS TFbQuery

   VAR      nError
   VAR      lError
   VAR      Dialect
   VAR      lBof
   VAR      lEof
   VAR      nRecno
   VAR      qry
   VAR      aStruct
   VAR      numcols
   VAR      closed
   VAR      db
   VAR      query
   VAR      aKeys
   VAR      aTables
   VAR      oTransaction

   METHOD   New( nDB, cQuery, nDialect, oTransaction )
   METHOD   Destroy()
   METHOD   Close()            INLINE ::Destroy()

   METHOD   Refresh()
   METHOD   Fetch()
   METHOD   Skip()             INLINE ::Fetch()

   METHOD   Bof()              INLINE ::lBof
   METHOD   Eof()              INLINE ::lEof
   METHOD   RecNo()            INLINE ::nRecno

   METHOD   NetErr()           INLINE ::lError
   METHOD   Error()            INLINE FBError( ::nError )
   METHOD   ErrorNo()          INLINE ::nError

   METHOD   FCount()           INLINE ::numcols
   METHOD   Struct()
   METHOD   FieldName( nField )
   METHOD   FieldPos( cField )
   METHOD   FieldLen( nField )
   METHOD   FieldDec( nField )
   METHOD   FieldType( nField )

   METHOD   FieldGet( nField )
   METHOD   GetRow()
   METHOD   GetBlankRow()
   METHOD   Blank()            INLINE ::GetBlankRow()
   METHOD   GetKeyField()

ENDCLASS

METHOD New( nDB, cQuery, nDialect, oTransaction ) CLASS TFbQuery

   ::db := nDb
   ::query := RemoveSpaces( cQuery )
   ::dialect := nDialect
   ::closed := .T.
   ::aKeys := NIL
   ::oTransaction := oTransaction

   ::Refresh()

   RETURN Self

METHOD Refresh() CLASS TFbQuery

   LOCAL qry, result, i, aTable := {}

   IF ! ::closed
      ::Destroy()
   ENDIF

   ::lBof := .T.
   ::lEof := .F.
   ::nRecno := 0
   ::closed := .F.
   ::numcols := 0
   ::aStruct := {}
   ::nError := 0
   ::lError := .F.

   result := .T.

   IF HB_ISOBJECT( ::oTransaction )
      IF ! ::oTransaction:Active
         ::oTransaction:Start()
      ENDIF
      qry := FBQuery( ::db, ::query, ::dialect, ::oTransaction:Handle )
   ELSE
      qry := FBQuery( ::db, ::query, ::dialect )
   ENDIF

   IF HB_ISARRAY( qry )
      ::numcols := qry[ 4 ]

      /* FIXME: This is faulty code. ::aStruct will become zero length, out of sync with ::numcols. */
      ::aStruct := StructConvert( qry[ 6 ], ::db, ::dialect )

      ::lError := .F.
      ::nError := 0
      ::qry := qry

      /* Tables in query */
      FOR i := 1 TO Len( ::aStruct )
         IF hb_AScan( aTable, ::aStruct[ i ][ 5 ], , , .T. ) == 0
            AAdd( aTable, ::aStruct[ i ][ 5 ] )
         ENDIF
      NEXT

      ::aTables := aTable

   ELSE
      ::lError := .T.
      ::nError := qry
   ENDIF

   RETURN result

METHOD Destroy() CLASS TFbQuery

   LOCAL result := .T.
   LOCAL n

   IF ! ::lError .AND. ( n := FBFree( ::qry ) ) < 0
      ::lError := .T.
      ::nError := n
   ENDIF

   ::closed := .T.

   RETURN result

METHOD Fetch() CLASS TFbQuery

   LOCAL result := .F.
   LOCAL fetch_stat

   IF ! ::lError .AND. ! ::lEof

      IF ! ::Closed
         fetch_stat := FBFetch( ::qry )

         ::nRecno++

         IF fetch_stat == 0
            ::lBof := .F.
            result := .T.
         ELSE
            ::lEof := .T.
         ENDIF
      ENDIF
   ENDIF

   RETURN result

METHOD Struct() CLASS TFbQuery

   LOCAL result := {}
   LOCAL i

   IF ! ::lError
      FOR i := 1 TO Len( ::aStruct )
         AAdd( result, { ::aStruct[ i ][ 1 ], ::aStruct[ i ][ 2 ], ::aStruct[ i ][ 3 ], ::aStruct[ i ][ 4 ] } )
      NEXT
   ENDIF

   RETURN result

METHOD FieldPos( cField ) CLASS TFbQuery

   LOCAL result := 0

   IF ! ::lError
      result := AScan( ::aStruct, {| x | x[ 1 ] == RTrim( Upper( cField ) ) } )
   ENDIF

   RETURN result

METHOD FieldName( nField ) CLASS TFbQuery

   LOCAL result

   IF ! ::lError .AND. nField >= 1 .AND. nField <= Len( ::aStruct )
      result := ::aStruct[ nField ][ 1 ]
   ENDIF

   RETURN result

METHOD FieldType( nField ) CLASS TFbQuery

   LOCAL result

   IF ! ::lError .AND. nField >= 1 .AND. nField <= Len( ::aStruct )
      result := ::aStruct[ nField ][ 2 ]
   ENDIF

   RETURN result

METHOD FieldLen( nField ) CLASS TFbQuery

   LOCAL result

   IF ! ::lError .AND. nField >= 1 .AND. nField <= Len( ::aStruct )
      result := ::aStruct[ nField ][ 3 ]
   ENDIF

   RETURN result

METHOD FieldDec( nField ) CLASS TFbQuery

   LOCAL result

   IF ! ::lError .AND. nField >= 1 .AND. nField <= Len( ::aStruct )
      result := ::aStruct[ nField ][ 4 ]
   ENDIF

   RETURN result

METHOD FieldGet( nField ) CLASS TFbQuery

   LOCAL result, aBlob, i, cType

   IF ! ::lError .AND. nField >= 1 .AND. nField <= Len( ::aStruct ) .AND. ! ::closed

      /* TODO: Convert to right data type */

      result := FBGetData( ::qry, nField )
      cType := ::aStruct[ nField ][ 2 ]

      IF cType == "M"
         /* Blob */

         IF result != NIL
            aBlob := FBGetBlob( ::db, result )

            result := ""
            FOR i := 1 TO Len( aBlob )
               result += aBlob[ i ]
            NEXT

            // result := FBGetBlob( ::db, result )
         ELSE
            result := ""
         ENDIF

      ELSEIF cType == "N"
         IF result != NIL
            result := Val( result )
         ELSE
            result := 0
         ENDIF

      ELSEIF cType == "D"
         IF result != NIL
            result := hb_SToD( Left( result, 4 ) + SubStr( result, 5, 2 ) + SubStr( result, 7, 2 ) )
         ELSE
            result := hb_SToD()
         ENDIF

      ELSEIF cType == "L"
         IF result != NIL
            result := ( Val( result ) == 1 )
         ELSE
            result := .F.
         ENDIF
      ENDIF
   ENDIF

   RETURN result

METHOD Getrow() CLASS TFbQuery

   LOCAL result
   LOCAL aRow
   LOCAL i

   IF ! ::lError .AND. ! ::closed

      aRow := Array( ::numcols )

      FOR i := 1 TO ::numcols
         aRow[ i ] := ::FieldGet( i )
      NEXT

      result := TFBRow():New( aRow, ::aStruct, ::db, ::dialect, ::aTables )
   ENDIF

   RETURN result

METHOD GetBlankRow() CLASS TFbQuery

   LOCAL result
   LOCAL aRow
   LOCAL i

   IF ! ::lError
      aRow := Array( ::numcols )

      FOR i := 1 TO ::numcols

         SWITCH ::aStruct[ i ][ 2 ]
         CASE "C"
         CASE "M"
            aRow[ i ] := ""
            EXIT
         CASE "N"
            aRow[ i ] := 0
            EXIT
         CASE "L"
            aRow[ i ] := .F.
            EXIT
         CASE "D"
            aRow[ i ] := hb_SToD()
            EXIT
         ENDSWITCH
      NEXT

      result := TFBRow():New( aRow, ::aStruct, ::db, ::dialect, ::aTables )
   ENDIF

   RETURN result

METHOD GetKeyField() CLASS TFbQuery

   IF ::aKeys == NIL
      ::aKeys := KeyField( ::aTables, ::db, ::dialect )
   ENDIF

   RETURN ::aKeys

CREATE CLASS TFbRow

   VAR      aRow
   VAR      aStruct
   VAR      aChanged
   VAR      aKeys
   VAR      db
   VAR      dialect
   VAR      aTables

   METHOD   New( row, struct, nDB, nDialect, aTable )
   METHOD   Changed( nField )
   METHOD   GetTables()        INLINE ::aTables
   METHOD   FCount()           INLINE Len( ::aRow )
   METHOD   FieldGet( nField )
   METHOD   FieldPut( nField, Value )
   METHOD   FieldName( nField )
   METHOD   FieldPos( cField )
   METHOD   FieldLen( nField )
   METHOD   FieldDec( nField )
   METHOD   FieldType( nField )
   METHOD   GetKeyField()

ENDCLASS

METHOD new( row, struct, nDb, nDialect, aTable ) CLASS TFbRow

   ::aRow := row
   ::aStruct := struct
   ::db := nDB
   ::dialect := nDialect
   ::aTables := aTable
   ::aChanged := Array( Len( row ) )

   RETURN Self

METHOD Changed( nField ) CLASS TFbRow

   LOCAL result

   IF nField >= 1 .AND. nField <= Len( ::aRow )
      result := ( ::aChanged[ nField ] != NIL )
   ENDIF

   RETURN result

METHOD FieldGet( nField ) CLASS TFbRow

   LOCAL result

   IF nField >= 1 .AND. nField <= Len( ::aRow )
      result := ::aRow[ nField ]
   ENDIF

   RETURN result

METHOD FieldPut( nField, Value ) CLASS TFbRow

   LOCAL result

   IF nField >= 1 .AND. nField <= Len( ::aRow )
      ::aChanged[ nField ] := .T.
      result := ::aRow[ nField ] := Value
   ENDIF

   RETURN result

METHOD FieldName( nField ) CLASS TFbRow

   LOCAL result

   IF nField >= 1 .AND. nField <= Len( ::aStruct )
      result := ::aStruct[ nField ][ 1 ]
   ENDIF

   RETURN result

METHOD FieldPos( cField ) CLASS TFbRow
   RETURN AScan( ::aStruct, {| x | x[ 1 ] == RTrim( Upper( cField ) ) } )

METHOD FieldType( nField ) CLASS TFbRow

   LOCAL result

   IF nField >= 1 .AND. nField <= Len( ::aStruct )
      result := ::aStruct[ nField ][ 2 ]
   ENDIF

   RETURN result

METHOD FieldLen( nField ) CLASS TFbRow

   LOCAL result

   IF nField >= 1 .AND. nField <= Len( ::aStruct )
      result := ::aStruct[ nField ][ 3 ]
   ENDIF

   RETURN result

METHOD FieldDec( nField ) CLASS TFbRow

   LOCAL result

   IF nField >= 1 .AND. nField <= Len( ::aStruct )
      result := ::aStruct[ nField ][ 4 ]
   ENDIF

   RETURN result

METHOD GetKeyField() CLASS TFbRow

   IF ::aKeys == NIL
      ::aKeys := KeyField( ::aTables, ::db, ::dialect )
   ENDIF

   RETURN ::aKeys

STATIC FUNCTION KeyField( aTables, db, dialect )

   LOCAL cTable, cQuery
   LOCAL qry
   LOCAL aKeys := {}

   /* Check row, many tables exists in current query, so we must have only one table */

   IF Len( aTables ) == 1
      cTable := aTables[ 1 ]

      cQuery := ' select                                      '
      cQuery += '   a.rdb$field_name                          '
      cQuery += ' from                                        '
      cQuery += '   rdb$index_segments a,                     '
      cQuery += '   rdb$relation_constraints b                '
      cQuery += ' where                                       '
      cQuery += '   a.rdb$index_name = b.rdb$index_name and   '
      cQuery += '   b.rdb$constraint_type = "PRIMARY KEY" and '
      cQuery += '   b.rdb$relation_name = ' + DataToSql( cTable )
      cQuery += ' order by                                    '
      cQuery += '   b.rdb$relation_name,                      '
      cQuery += '   a.rdb$field_position                      '

      qry := FBQuery( db, RemoveSpaces( cQuery ), dialect )

      IF HB_ISARRAY( qry )
         DO WHILE FBFetch( qry ) == 0
            AAdd( aKeys, RTrim( FBGetData( qry, 1 ) ) )
         ENDDO

         FBFree( qry )
      ENDIF
   ENDIF

   RETURN aKeys

STATIC FUNCTION DataToSql( xField )

   SWITCH ValType( xField )
   CASE "C"
      RETURN '"' + StrTran( xField, '"', ' ' ) + '"'
   CASE "D"
      RETURN '"' + StrZero( Month( xField ), 2 ) + "/" + StrZero( Day( xField ), 2 ) + "/" + StrZero( Year( xField ), 4 ) + '"'
   CASE "N"
      RETURN Str( xField )
   CASE "L"
      RETURN iif( xField, "1", "0" )
   ENDSWITCH

   RETURN NIL

STATIC FUNCTION StructConvert( aStru, db, dialect )

   LOCAL aNew := {}
   LOCAL cField
   LOCAL nType
   LOCAL cType
   LOCAL nSize
   LOCAL nDec
   LOCAL cTable
   LOCAL cDomain
   LOCAL i
   LOCAL qry
   LOCAL cQuery
   LOCAL aDomains := {}
   LOCAL nVal

   LOCAL xTables := ""
   LOCAL xFields := ""

   /* create table list and field list */

   FOR i := 1 TO Len( aStru )
      xtables += DataToSql( aStru[ i ][ 5 ] )
      xfields += DataToSql( aStru[ i ][ 1 ] )

      IF i != Len( aStru )
         xtables += ","
         xfields += ","
      ENDIF
   NEXT

   /* Look for domains */
   cQuery := 'select rdb$relation_name, rdb$field_name, rdb$field_source '
   cQuery += '  from rdb$relation_fields '
   cQuery += ' where rdb$field_name not like "RDB$%" '
   cQuery += '   and rdb$relation_name in (' + xtables + ')'
   cQuery += '   and rdb$field_name in (' + xfields + ')'

   qry := FBQuery( db, RemoveSpaces( cQuery ), dialect )

   IF HB_ISARRAY( qry )

      DO WHILE FBFetch( qry ) == 0
         AAdd( aDomains, { ;
            iif( FBGetData( qry, 1 ) == NIL, "", FBGetData( qry, 1 ) ), ;
            iif( FBGetData( qry, 2 ) == NIL, "", FBGetData( qry, 2 ) ), ;
            iif( FBGetData( qry, 3 ) == NIL, "", FBGetData( qry, 3 ) ) } )
      ENDDO

      FBFree( qry )

      FOR i := 1 TO Len( aStru )
         cField := RTrim( aStru[ i ][ 7 ] )
         nType := aStru[ i ][ 2 ]
         nSize := aStru[ i ][ 3 ]
         nDec := aStru[ i ][ 4 ] * -1
         cTable := RTrim( aStru[ i ][ 5 ] )

         nVal := AScan( aDomains, {| x | RTrim( x[ 1 ] ) == cTable .AND. RTrim( x[ 2 ] ) == cField } )

         IF nVal != 0
            cDomain := aDomains[ nVal, 3 ]
         ELSE
            cDomain := ""
         ENDIF

         SWITCH nType
         CASE SQL_TEXT
            cType := "C"
            EXIT
         CASE SQL_VARYING
            cType := "C"
            EXIT
         CASE SQL_SHORT
            /* Firebird doesn't have boolean field, so if you define domain with BOOL then i will consider logical, ex:
               create domain boolean_field as smallint default 0 not null check (value in (0,1)) */

            IF "BOOL" $ cDomain
               cType := "L"
               nSize := 1
               nDec := 0
            ELSE
               cType := "N"
               nSize := 5
            ENDIF
            EXIT
         CASE SQL_LONG
            cType := "N"
            nSize := 9
            EXIT
         CASE SQL_INT64
            cType := "N"
            nSize := 9
            EXIT
         CASE SQL_FLOAT
            cType := "N"
            nSize := 15
            EXIT
         CASE SQL_DOUBLE
            cType := "N"
            nSize := 15
            EXIT
         CASE SQL_TIMESTAMP
            cType := "T"
            nSize := 8
            EXIT
         CASE SQL_TYPE_DATE
            cType := "D"
            nSize := 8
            EXIT
         CASE SQL_TYPE_TIME
            cType := "C"
            nSize := 8
            EXIT
         CASE SQL_BLOB
            cType := "M"
            nSize := 10
            EXIT
         OTHERWISE
            cType := "C"
            nDec := 0
         ENDSWITCH

         AAdd( aNew, { cField, cType, nSize, nDec, cTable, cDomain } )
      NEXT
   ENDIF

   RETURN aNew

STATIC FUNCTION RemoveSpaces( cQuery )

   DO WHILE At( "  ", cQuery ) != 0
      cQuery := StrTran( cQuery, "  ", " " )
   ENDDO

   RETURN cQuery

/* query array items - for internal use only */
#define FBQA_STATEMENT      1
#define FBQA_SQLDA          2
#define FBQA_TRANSACTION    3
#define FBQA_COLUMNS_COUNT  4
#define FBQA_DIALECT        5
#define FBQA_COLUMNS        6
#define FBQA_STATEMENT_TYPE 7
#define FBQA_PARAMS_SQLDA   8
#define FBQA_PARAMS         9

CREATE CLASS TFbSQL

   HIDDEN:
   VAR lActive      INIT .F.
   VAR aQuery

   VAR cSQL
   VAR oDatabase
   VAR oTransaction

   VAR cCursorName

   VISIBLE:

   METHOD New( oDatabase, oTransaction, cSQL ) CONSTRUCTOR
   DESTRUCTOR Free()

   METHOD Prepare()
   METHOD Execute()
   METHOD Fetch()
   METHOD Close()

   METHOD ParamCount()
   METHOD ParamInfo( nIndex )
   METHOD SetParam( nIndex, xValue )
   METHOD SetParams( aParams )

   METHOD FieldCount()
   METHOD FieldInfo( nIndex )
   METHOD FieldIndex( cFieldName )
   METHOD FieldType( nIndex )
   METHOD FieldLen( nIndex )
   METHOD FieldDec( ncIndex )
   METHOD GetValue( xIndexOrName )
   METHOD GetRow( lAsHash )

   ACCESS Active      INLINE ( ::lActive )
   ASSIGN Active      METHOD SetActive( lValue )
   ACCESS Prepared    INLINE ( ! Empty( ::aQuery ) )
   ACCESS StatementType INLINE ( iif( HB_ISARRAY( ::aQuery ), ::aQuery[ FBQA_STATEMENT_TYPE ], 0 ) )

   ACCESS SQL         INLINE ( ::cSQL )
   ASSIGN SQL         METHOD SetSQL( cValue )
   ACCESS Database    INLINE ( ::oDatabase )
   ASSIGN Database    METHOD SetDatabase( oValue )
   ACCESS Transaction INLINE ( ::oTransaction )
   ASSIGN Transaction METHOD SetTransaction( oValue )

   ACCESS CursorName  INLINE ( ::cCursorName )
   ASSIGN CursorName  METHOD SetCursorName( cValue )

   OPERATOR "[]" ARG xIndex INLINE iif( PCount() > 2, ::SetParam( xIndex, hb_PValue( 3 ) ), ::GetValue( xIndex ) )

ENDCLASS

METHOD New( oDatabase, oTransaction, cSQL ) CLASS TFbSQL

   ::oDatabase := oDatabase
   ::oTransaction := oTransaction
   ::cSQL := cSQL
   ::cCursorName := 'HBSQL' + NetName() + AllTrim( Str( hb_RandomInt( 0xFFFFFFFF ) ) ) ;
      + AllTrim( Str( hb_RandomInt( 0xFFFFFFFF ) ) )

   RETURN Self

DESTRUCTOR Free() CLASS TFbSQL

   ::Close()

   RETURN

METHOD Prepare() CLASS TFbSQL

   LOCAL lRes := .F.

   IF Empty( ::aQuery ) .AND. ! Empty( ::oDatabase ) .AND. ! Empty( ::oTransaction ) .AND. HB_ISCHAR( ::cSQL )

      ::aQuery := FBSQLCreate( ::oDatabase:db, ::cSQL, 3, ::oTransaction:Handle )

      IF HB_ISARRAY( ::aQuery )
         lRes := .T.
      ELSE
         Eval( ErrorBlock(), FBErrorNew( ::aQuery ) )
      ENDIF

   ELSE
      Eval( ErrorBlock(), FBErrorNew( 1000, "Invalid params" ) )
   ENDIF

   RETURN lRes

METHOD Execute() CLASS TFbSQL

   LOCAL nRes
   LOCAL cCurName

   IF Empty( ::aQuery )
      ::Prepare()
   ENDIF

   IF ( ( ::aQuery[ FBQA_STATEMENT_TYPE ] == ISC_INFO_SQL_STMT_SELECT ) ;
      .OR. ( ::aQuery[ FBQA_STATEMENT_TYPE ] == ISC_INFO_SQL_STMT_SELECT_FOR_UPD ) ) ;
      .AND. HB_ISCHAR( ::cCursorName ) .AND. Len( ::cCursorName ) > 0

      cCurName := ::cCursorName

   ELSE
      cCurName := NIL
   ENDIF

   nRes := FBSQLExec( ::aQuery, cCurName )

   IF nRes <> 0
      Eval( ErrorBlock(), FBErrorNew( nRes ) )
   ELSE
      IF ( ::aQuery[ FBQA_STATEMENT_TYPE ] == ISC_INFO_SQL_STMT_SELECT ) ;
         .OR. ( ::aQuery[ FBQA_STATEMENT_TYPE ] == ISC_INFO_SQL_STMT_SELECT_FOR_UPD )

         ::lActive := .T.
      ENDIF
   ENDIF

   RETURN nRes == 0

METHOD Fetch() CLASS TFbSQL

   LOCAL nRes

   IF Empty( ::aQuery )
      Eval( ErrorBlock(), FBErrorNew( 1000, "Invalid query handle" ) )
   ENDIF

   nRes := FBFetch( ::aQuery )

   IF nRes <> 0 .AND. nRes <> -1
      Eval( ErrorBlock(), FBErrorNew( nRes ) )
   ENDIF

   RETURN nRes

METHOD Close() CLASS TFbSQL

   LOCAL lRes := .F.

   IF HB_ISARRAY( ::aQuery )
      FBFree( ::aQuery )
      ::aQuery := NIL
      ::lActive := .F.
      lRes := .T.
   ENDIF

   RETURN lRes

METHOD ParamCount() CLASS TFbSQL

   LOCAL nRes := 0

   IF HB_ISARRAY( ::aQuery ) .AND. HB_ISARRAY( ::aQuery[ FBQA_PARAMS ] )
      nRes := Len( ::aQuery[ FBQA_PARAMS ] )
   ENDIF

   RETURN nRes

METHOD ParamInfo( nIndex ) CLASS TFbSQL

   LOCAL aRes := {}

   IF HB_ISARRAY( ::aQuery ) .AND. HB_ISARRAY( ::aQuery[ FBQA_PARAMS ] )
      aRes := Len( ::aQuery[ FBQA_PARAMS ][ nIndex ] )
   ENDIF

   RETURN aRes

METHOD SetParam( nIndex, xValue ) CLASS TFbSQL

   LOCAL lRes := .F.

   IF ::ParamCount() >= nIndex
      lRes := FBSQLSetParam( ::aQuery, nIndex, xValue ) == 0
   ENDIF

   RETURN lRes

METHOD SetParams( aParams ) CLASS TFbSQL

   LOCAL lRes := .F., n

   IF HB_ISARRAY( aParams ) .AND. ( Len( aParams ) == ::ParamCount() )
      FOR n := 1 TO Len( aParams )
         ::SetParam( n, aParams[ n ] )
      NEXT
      lRes := .T.
   ENDIF

   RETURN lRes

METHOD FieldCount() CLASS TFbSQL

   LOCAL nRes := 0

   IF HB_ISARRAY( ::aQuery ) .AND. HB_ISARRAY( ::aQuery[ FBQA_COLUMNS ] )
      nRes := Len( ::aQuery[ FBQA_COLUMNS ] )
   ENDIF

   RETURN nRes

METHOD FieldInfo( nIndex ) CLASS TFbSQL

   LOCAL aRes := {}

   IF HB_ISARRAY( ::aQuery ) .AND. HB_ISARRAY( ::aQuery[ FBQA_COLUMNS ] )
      aRes := ::aQuery[ FBQA_COLUMNS ][ nIndex ]
   ENDIF

   RETURN aRes

METHOD FieldIndex( cFieldName ) CLASS TFbSQL

   LOCAL nRes := 0
   LOCAL nI

   FOR nI := 1 TO ::FieldCount()
      IF Upper( ::aQuery[ FBQA_COLUMNS ][ nI ][ FBFS_NAME ] ) == Upper( cFieldName )
         nRes := nI
         EXIT
      ENDIF
   NEXT

   RETURN nRes

METHOD FieldType( nIndex ) CLASS TFbSQL

   LOCAL cRes := "U"

   SWITCH ::FieldInfo( nIndex )[ FBFS_TYPE ]
   CASE SQL_TEXT
   CASE SQL_VARYING
      cRes := "C"
      EXIT
   CASE SQL_TIMESTAMP
   CASE SQL_TYPE_TIME
      cRes := "T"
      EXIT
   CASE SQL_TYPE_DATE
      cRes := "D"
      EXIT
   CASE SQL_SHORT
   CASE SQL_LONG
   CASE SQL_FLOAT
   CASE SQL_DOUBLE
   CASE SQL_D_FLOAT
   CASE SQL_INT64
      cRes := "N"
      EXIT
   ENDSWITCH

   RETURN cRes

METHOD FieldLen( nIndex ) CLASS TFbSQL

   LOCAL nRes := 0

   SWITCH ::FieldInfo( nIndex )[ FBFS_TYPE ]
   CASE SQL_TEXT
   CASE SQL_VARYING
      nRes := ::FieldInfo( nIndex )[ FBFS_LENGTH ] / ::oDatabase:BytesPerChar()
      EXIT
   CASE SQL_TIMESTAMP
   CASE SQL_TYPE_TIME
      nRes := Len( hb_TToC( hb_DateTime() ) )
      EXIT
   CASE SQL_TYPE_DATE
      nRes := Len( hb_DToC( hb_Date() ) )
      EXIT
   CASE SQL_SHORT
      nRes := 5
      EXIT
   CASE SQL_LONG
      nRes := 10
      EXIT
   CASE SQL_FLOAT
   CASE SQL_DOUBLE
   CASE SQL_D_FLOAT
      nRes := 15
      EXIT
   CASE SQL_INT64
      nRes := 19
      EXIT
   OTHERWISE
      nRes := ::FieldInfo( nIndex )[ FBFS_LENGTH ]
   ENDSWITCH

   RETURN nRes

METHOD FieldDec( ncIndex ) CLASS TFbSQL

   RETURN ::FieldInfo( ncIndex )[ FBFS_SCALE ]

METHOD GetValue( xIndexOrName ) CLASS TFbSQL

   LOCAL xRes := NIL

   IF ! ::Prepared
      Eval( ErrorBlock(), FBErrorNew( 1000, "Query is not active" ) )
   ENDIF
   IF HB_ISCHAR( xIndexOrName )
      xIndexOrName := ::FieldIndex( xIndexOrName )
   ENDIF
   IF xIndexOrName > 0 .AND. xIndexOrName <= ::FieldCount()
      xRes := FBSQLGetData( ::aQuery, xIndexOrName )
   ENDIF
   RETURN xRes

METHOD GetRow( lAsHash ) CLASS TFbSQL

   LOCAL aRow
   LOCAL n

   hb_default( @lAsHash, .F. )

   IF lAsHash
      aRow := hb_Hash()
      FOR n := 1 TO ::FieldCount()
         aRow[ ::FieldInfo( n )[ FBFS_NAME ] ] := ::GetValue( n )
      NEXT
   ELSE
      aRow := Array( ::FieldCount() )
      FOR n := 1 TO ::FieldCount()
         aRow[ n ] := ::GetValue( n )
      NEXT
   ENDIF

   RETURN aRow

METHOD SetActive( lValue ) CLASS TFbSQL

   IF lValue .AND. ! ::lActive

      IF ! ::Prepared
         ::Prepare()
      ENDIF

      ::Execute()

   ELSEIF ! lValue .AND. ::Prepared
      ::Close()
   ENDIF

   RETURN NIL

METHOD SetSQL( cValue ) CLASS TFbSQL

   IF ! ::Active
      ::cSQL := cValue
   ELSE
      Eval( ErrorBlock(), FBErrorNew( 1000, "Can not permit this operation on active query" ) )
   ENDIF

   RETURN NIL

METHOD SetDatabase( oValue ) CLASS TFbSQL

   IF ! ::Active
      ::oDatabase := oValue
   ELSE
      Eval( ErrorBlock(), FBErrorNew( 1000, "Can not permit this operation on active query" ) )
   ENDIF

   RETURN NIL

METHOD SetTransaction( oValue ) CLASS TFbSQL

   IF ! ::Active
      ::oTransaction := oValue
   ELSE
      Eval( ErrorBlock(), FBErrorNew( 1000, "Can not permit this operation on active query" ) )
   ENDIF

   RETURN NIL

METHOD SetCursorName( cValue ) CLASS TFbSQL

   IF ! ::Active
      ::cCursorName := cValue
   ELSE
      Eval( ErrorBlock(), FBErrorNew( 1000, "Can not permit this operation on active query" ) )
   ENDIF

   RETURN NIL

/* blob array items - for internal use only */
#define FBBA_QUAD           1
#define FBBA_HANDLE         2
#define FBBA_NUM_SEG        3
#define FBBA_MAX_SEG        4
#define FBBA_TOTAL_LEN      5
#define FBBA_BLOB_TYPE      6

CREATE CLASS TFbBlobReader

   HIDDEN:

   VAR   aBlob
   VAR   lActive   INIT .F.

   VISIBLE:

   METHOD New( oDatabase, oTransaction, pQuad ) CONSTRUCTOR
   DESTRUCTOR Free()

   METHOD Read( nLength )
   METHOD Close()

   ACCESS Active      INLINE ( ::lActive )
   ACCESS NumSegments INLINE ( ::aBlob[ FBBA_NUM_SEG ] )
   ACCESS MaxSegment  INLINE ( ::aBlob[ FBBA_MAX_SEG ] )
   ACCESS TotalLength INLINE ( ::aBlob[ FBBA_TOTAL_LEN ] )
   ACCESS BlobType    INLINE ( ::aBlob[ FBBA_BLOB_TYPE ] )

ENDCLASS

METHOD New( oDatabase, oTransaction, pQuad ) CLASS TFbBlobReader

   ::aBlob := FbBlobReaderCreate( oDatabase:db, oTransaction:Handle, pQuad )

   IF ! HB_ISARRAY( ::aBlob )
      Eval( ErrorBlock(), FBErrorNew( ::aBlob ) )
   ENDIF

   ::lActive := .T.

   RETURN Self

DESTRUCTOR Free() CLASS TFbBlobReader

   IF ::Active
      ::Close()
   ENDIF

   RETURN

METHOD Read( nLength ) CLASS TFbBlobReader

   LOCAL cRes

   cRes := FbBlobReaderRead( ::aBlob, nLength )

   IF HB_ISNUMERIC( cRes )
      Eval( ErrorBlock(), FBErrorNew( cRes ) )
   ENDIF

   RETURN cRes

METHOD Close() CLASS TFbBlobReader

   LOCAL xRes

   xRes := FbBlobClose( ::aBlob )
   IF ! HB_ISLOGICAL( xRes )
      Eval( ErrorBlock(), FBErrorNew( xRes ) )
   ENDIF

   ::lActive := .F.

   RETURN .T.

CREATE CLASS TFbBlobWriter

   HIDDEN:

   VAR   aBlob
   VAR   nWriten   INIT 0
   VAR   lActive

   VISIBLE:

   METHOD New( oDatabase, oTransaction ) CONSTRUCTOR
   DESTRUCTOR Free()

   METHOD Write( cData, nLength )
   METHOD Close()

   ACCESS Active      INLINE ( ::lActive )
   ACCESS WriteSize   INLINE ( ::nWriten )
   ACCESS Quad        INLINE ( ::aBlob[ FBBA_QUAD ] )

ENDCLASS

METHOD New( oDatabase, oTransaction ) CLASS TFbBlobWriter

   ::aBlob := FbBlobWriterCreate( oDatabase:db, oTransaction:Handle )

   IF ! HB_ISARRAY( ::aBlob )
      Eval( ErrorBlock(), FBErrorNew( ::aBlob ) )
   ENDIF

   ::lActive := .T.

   RETURN Self

DESTRUCTOR Free() CLASS TFbBlobWriter

   IF ::Active
      ::Close()
   ENDIF

   RETURN

METHOD Write( cData, nLength ) CLASS TFbBlobWriter

   LOCAL cRes

   cRes := FbBlobWriterWrite( ::aBlob, cData, nLength )

   IF HB_ISNUMERIC( cRes )
      Eval( ErrorBlock(), FBErrorNew( cRes ) )
   ENDIF

   ::nWriten := ::nWriten + nLength

   RETURN cRes

METHOD Close() CLASS TFbBlobWriter

   LOCAL xRes

   xRes := FbBlobClose( ::aBlob )
   IF ! HB_ISLOGICAL( xRes )
      Eval( ErrorBlock(), FBErrorNew( xRes ) )
   ENDIF

   ::lActive := .F.

   RETURN .T.


CREATE CLASS TFbGeneratorLink

   VISIBLE:

   VAR cGeneratorName
   VAR xField
   VAR nIncrement      INIT 1
   VAR nWhereApply     INIT FBGW_ON_NEW_RECORD
   VAR oDataSet

   METHOD New( oDataSet ) CONSTRUCTOR
   METHOD ValidLink()
   METHOD Generate()

ENDCLASS

METHOD New( oDataSet ) CLASS TFbGeneratorLink

   ::oDataSet := oDataSet

   RETURN Self

METHOD ValidLink() CLASS TFbGeneratorLink

   RETURN HB_ISCHAR( ::cGeneratorName ) .AND. Len( ::cGeneratorName ) > 0 ;
      .AND. ( ( HB_ISNUMERIC( ::xField ) .AND. ::xField > 0 ) ;
      .OR. ( HB_ISCHAR( ::xField ) .AND. Len( ::xField ) > 0 ) ) ;
      .AND. ( ::nWhereApply == FBGW_ON_NEW_RECORD .OR. ::nWhereApply == FBGW_ON_POST ) ;
      .AND. HB_ISOBJECT( ::oDataSet )

METHOD Generate() CLASS TFbGeneratorLink

   LOCAL oSQL

   oSQL := TFbSQL():New( ::oDataSet:Transaction, "select GEN_ID(" + AllTrim( ::cGeneratorName ) ;
      + ", " + AllTrim( Str( ::nIncrement ) ) + ") from RDB$DATABASE" )

   oSQL:Execute()
   oSQL:Fetch()

   RETURN oSQL:GetValue( 1 )

CREATE CLASS TFbDataSet

   HIDDEN:

   VAR   oSelectSQL
   VAR   cSelectSQL
   VAR   oInsertSQL
   VAR   aInsertSQL
   VAR   lCanInsert   INIT .F.
   VAR   oUpdateSQL
   VAR   aUpdateSQL
   VAR   lCanUpdate   INIT .F.
   VAR   oDeleteSQL
   VAR   aDeleteSQL
   VAR   lCanDelete   INIT .F.
   VAR   oRefreshSQL
   VAR   aRefreshSQL
   VAR   lCanRefresh  INIT .F.

   VAR   oDatabase
   VAR   oTransaction

   VAR   aRows        INIT {}
   VAR   nCurRow      INIT 0
   VAR   nState       INIT FBDS_INACTIVE
   VAR   aEditRow     INIT {}
   VAR   lEditRowChanged INIT .F.

   VAR   lSQLEof      INIT .T.

   VAR   oGeneratorLink

   METHOD InitSQL()
   METHOD FetchRecord()

   PROTECTED:

   METHOD UpdateParams( oSQL, aParams )

   VISIBLE:

   METHOD New( oDatabase, oTransaction, cSelectSQL, aInsertSQL, aUpdateSQL, aDeleteSQL, aRefreshSQL ) CONSTRUCTOR
   DESTRUCTOR Free()

   METHOD Open()
   METHOD Close()
   METHOD Eof()
   METHOD Bof()
   METHOD Skip( nRecs )
   METHOD RecNo()
   METHOD GoTop()
   METHOD GoBottom()
   METHOD GoTo( nRecNo )
   METHOD Edit()
   METHOD Append()
   METHOD Post()
   METHOD Cancel()
   METHOD Delete()
   METHOD Refresh()

   METHOD ParamCount()
   METHOD ParamInfo( nIndex )
   METHOD SetParam( nIndex, xValue )

   METHOD FieldCount()
   METHOD FieldInfo( ncIndex )
   METHOD GetValue( ncIndex )
   METHOD SetValue( ncIndex, xValue, lIsRefresh )
   METHOD DefaulValue( ncIndex )
   METHOD FieldType( ncIndex )
   METHOD FieldLen( ncIndex )
   METHOD FieldDec( ncIndex )

   METHOD Locate( aFlds, aValues, lContinue, lCaseSensitive, lPartial )

   ACCESS Database INLINE ( ::oDatabase )
   ASSIGN Database METHOD SetDatabase( oValue )
   ACCESS Transaction INLINE ( ::oTransaction )
   ASSIGN Transaction METHOD SetTransaction( oValue )

   ACCESS Active METHOD GetActive()
   ASSIGN Active METHOD SetActive( lValue )

   ACCESS State INLINE ( ::nState )

   ACCESS SelectSQL INLINE ( ::cSelectSQL )
   ASSIGN SelectSQL METHOD SetSelectSQL( cValue )
   ACCESS InsertSQL INLINE ( ::aInsertSQL )
   ASSIGN InsertSQL METHOD SetInsertSQL( aValue )
   ACCESS UpdateSQL INLINE ( ::aUpdateSQL )
   ASSIGN UpdateSQL METHOD SetUpdateSQL( aValue )
   ACCESS DeleteSQL INLINE ( ::aDeleteSQL )
   ASSIGN DeleteSQL METHOD SetDeleteSQL( aValue )
   ACCESS RefreshSQL INLINE ( ::aRefreshSQL )
   ASSIGN RefreshSQL METHOD SetRefreshSQL( aValue )

   ACCESS SelectObj INLINE ( ::oSelectSQL )
   ACCESS InsertObj INLINE ( ::oInsertSQL )
   ACCESS UpdateObj INLINE ( ::oUpdateSQL )
   ACCESS DeleteObj INLINE ( ::oDeleteSQL )
   ACCESS RefreshObj INLINE ( ::oRefreshSQL )

   ACCESS CanInsert INLINE ( ::lCanInsert )
   ACCESS CanUpdate INLINE ( ::lCanUpdate )
   ACCESS CanDelete INLINE ( ::lCanDelete )
   ACCESS CanRefresh INLINE ( ::lCanRefresh )

   ACCESS RecordCount INLINE ( ( Len( ::aRows ) + iif( ::nState == FBDS_APPEND, 1, 0 ) ) )

   ACCESS GeneratorLink INLINE ( ::oGeneratorLink )

   VAR    UseDefaultValue   INIT .F.

   OPERATOR "[]" ARG xIndex INLINE iif( PCount() > 2, ::SetValue( xIndex, hb_PValue( 3 ) ), ::GetValue( xIndex ) )

ENDCLASS

METHOD InitSQL() CLASS TFbDataSet

   IF HB_ISCHAR( ::cSelectSQL )
      ::oSelectSQL:SQL := ::cSelectSQL

      ::oSelectSQL:Prepare()

      IF ::oSelectSQL:StatementType <> ISC_INFO_SQL_STMT_SELECT
         Eval( ErrorBlock(), FBErrorNew( 1000, "Invalid SELECT query" ) )
      ENDIF
   ELSE
      RETURN .F.
   ENDIF

   IF HB_ISARRAY( ::aInsertSQL ) .AND. HB_ISCHAR( ::aInsertSQL[ 1 ] ) .AND. Len( ::aInsertSQL[ 1 ] ) > 0
      ::oInsertSQL:SQL := ::aInsertSQL[ 1 ]
      ::lCanInsert := ::oInsertSQL:Prepare() .AND. ;
         ( ::oInsertSQL:StatementType == ISC_INFO_SQL_STMT_INSERT .OR. ::oInsertSQL:StatementType == ISC_INFO_SQL_STMT_EXEC_PROCEDURE )
   ELSE
      ::lCanInsert := .F.
   ENDIF

   IF HB_ISARRAY( ::aUpdateSQL ) .AND. HB_ISCHAR( ::aUpdateSQL[ 1 ] ) .AND. Len( ::aUpdateSQL[ 1 ] ) > 0
      ::oUpdateSQL:SQL := ::aUpdateSQL[ 1 ]
      ::lCanUpdate := ::oUpdateSQL:Prepare() .AND. ::oUpdateSQL:StatementType == ISC_INFO_SQL_STMT_UPDATE
   ELSE
      ::lCanUpdate := .F.
   ENDIF

   IF HB_ISARRAY( ::aDeleteSQL ) .AND. HB_ISCHAR( ::aDeleteSQL[ 1 ] ) .AND. Len( ::aDeleteSQL[ 1 ] ) > 0
      ::oDeleteSQL:SQL := ::aDeleteSQL[ 1 ]
      ::lCanDelete := ::oDeleteSQL:Prepare() .AND. ::oDeleteSQL:StatementType == ISC_INFO_SQL_STMT_DELETE
   ELSE
      ::lCanDelete := .F.
   ENDIF

   IF HB_ISARRAY( ::aRefreshSQL ) .AND. HB_ISCHAR( ::aRefreshSQL[ 1 ] ) .AND. Len( ::aRefreshSQL[ 1 ] ) > 0
      ::oRefreshSQL:SQL := ::aRefreshSQL[ 1 ]
      ::lCanRefresh := ::oRefreshSQL:Prepare() .AND. ::oRefreshSQL:StatementType == ISC_INFO_SQL_STMT_SELECT
   ELSE
      ::lCanRefresh := .F.
   ENDIF

   RETURN .T.

METHOD FetchRecord() CLASS TFbDataSet

   LOCAL lRes
   LOCAL nI
   LOCAL aRow

   IF ( lRes := ( ::oSelectSQL:Fetch() == 0 ) )
      aRow := Array( ::oSelectSQL:FieldCount() )

      FOR nI := 1 TO ::oSelectSQL:FieldCount()
         aRow[ nI ] := ::oSelectSQL:GetValue( nI )
      NEXT

      AAdd( ::aRows, aRow )

   ENDIF

   RETURN lRes

METHOD UpdateParams( oSQL, aParams ) CLASS TFbDataSet

   LOCAL nI
   LOCAL lRes := .F.

   IF HB_ISOBJECT( oSQL )
      IF HB_ISARRAY( aParams ) .AND. oSQL:ParamCount() == Len( aParams )
         FOR nI := 1 TO Len( aParams )
            oSQL:SetParam( nI, ::GetValue( aParams[ nI ] ) )
         NEXT
         lRes := .T.
      ELSEIF HB_ISCHAR( aParams ) .AND. oSQL:ParamCount() == 1
         oSQL:SetParam( 1, ::GetValue( aParams ) )
         lRes := .T.
      ENDIF
   ENDIF

   RETURN lRes

METHOD New( oDatabase, oTransaction, cSelectSQL, aInsertSQL, aUpdateSQL, aDeleteSQL, aRefreshSQL ) CLASS TFbDataSet

   ::oDatabase := oDatabase
   ::oTransaction := oTransaction

   ::cSelectSQL := cSelectSQL
   ::oSelectSQL := TFBSQL():New( oDatabase, oTransaction, cSelectSQL )

   ::aInsertSQL := aInsertSQL
   ::oInsertSQL := TFBSQL():New( oDatabase, oTransaction )

   ::aUpdateSQL := aUpdateSQL
   ::oUpdateSQL := TFBSQL():New( oDatabase, oTransaction )

   ::aDeleteSQL := aDeleteSQL
   ::oDeleteSQL := TFBSQL():New( oDatabase, oTransaction )

   ::aRefreshSQL := aRefreshSQL
   ::oRefreshSQL := TFBSQL():New( oDatabase, oTransaction )

   ::oGeneratorLink := TFbGeneratorLink():New( Self )

   RETURN Self

DESTRUCTOR Free() CLASS TFbDataSet

   ::Active := .F.
   ::oRefreshSQL := NIL
   ::oDeleteSQL := NIL
   ::oInsertSQL := NIL
   ::oUpdateSQL := NIL
   ::oSelectSQL := NIL
   ::oGeneratorLink := NIL

   RETURN

METHOD Open() CLASS TFbDataSet

   ::Active := .T.

   RETURN ::Active

METHOD Close() CLASS TFbDataSet

   ::Active := .F.

   RETURN ::Active

METHOD Eof() CLASS TFbDataSet

   LOCAL lRes := .F.

   IF ::Active
      lRes := ( ::lSQLEof .AND. ::nCurRow == ::RecordCount ) .OR. ( ::nCurRow == 0 )
   ENDIF

   RETURN lRes

METHOD Bof() CLASS TFbDataSet

   LOCAL lRes := .F.

   IF ::Active
      lRes := ( ::nCurRow == 1 ) .OR. ( ::nCurRow == 0 )
   ENDIF

   RETURN lRes

METHOD Skip( nRecs ) CLASS TFbDataSet

   LOCAL nI
   LOCAL nSkipped := 0

   IF ::Active

      IF ::State <> FBDS_BROWSE
         ::Post()
      ENDIF

      hb_default( @nRecs, 1 )

      IF nRecs > 0

         IF nRecs + ::nCurRow <= ::RecordCount
            ::nCurRow := ::nCurRow + nRecs
            nSkipped := nRecs
         ELSE
            IF ::lSQLEof
               nSkipped := ::RecordCount - ::nCurRow
               ::nCurRow := ::RecordCount
            ELSE
               FOR nI := 1 TO nRecs
                  IF ::FetchRecord()
                     ::nCurRow := ::RecordCount
                     nSkipped++
                  ELSE
                     ::lSQLEof := .T.
                     EXIT
                  ENDIF
               NEXT
            ENDIF
         ENDIF
      ELSEIF ( nRecs < 0 ) .AND. ( ::RecordCount > 0 )
         IF -nRecs > ::nCurRow
            nSkipped := ::nCurRow + ( nRecs + ::nCurRow)
            ::nCurRow := 1
         ELSE
            ::nCurRow := ::nCurRow + nRecs
            nSkipped := nRecs
         ENDIF
      ENDIF
   ENDIF

   RETURN nSkipped

METHOD RecNo() CLASS TFbDataSet

   RETURN ::nCurRow

METHOD GoTop() CLASS TFbDataSet

   LOCAL lRes := .F.

   IF ::Active

      IF ::nState <> FBDS_BROWSE
         ::Post()
      ENDIF

      IF ::RecordCount > 0
         ::nCurRow := 1
         lRes := .T.
      ENDIF

   ENDIF

   RETURN lRes

METHOD GoBottom() CLASS TFbDataSet

   LOCAL lRes := .F.

   IF ::Active

      IF ::nState <> FBDS_BROWSE
         ::Post()
      ENDIF

      IF ! ::lSQLEof
         DO WHILE ( ::FetchRecord() )
         ENDDO
         ::lSQLEof := .T.
      ENDIF

      ::nCurRow := ::RecordCount

      lRes := .T.

   ENDIF

   RETURN lRes

METHOD GoTo( nRecNo ) CLASS TFbDataSet

   LOCAL lRes := .F.

   IF ::Active
      IF ::nState <> FBDS_BROWSE
         ::Post()
      ENDIF

      IF nRecNo > 0 .AND. nRecNo <= ::RecordCount
         ::nCurRow := nRecNo
         lRes := .T.
      ENDIF
   ENDIF

   RETURN lRes

METHOD Edit() CLASS TFbDataSet

   LOCAL lRes := .F.

   IF ::State == FBDS_BROWSE

      IF ::RecordCount > 0 .AND. ::nCurRow > 0
         ::aEditRow := ::aRows[ ::nCurRow ]
         ::nState := FBDS_EDIT
         ::lEditRowChanged := .F.
         lRes := .T.
      ELSE
         lRes := ::Append()
      ENDIF

   ENDIF

   RETURN lRes

METHOD Append() CLASS TFbDataSet

   LOCAL lRes := .F.
   LOCAL n

   IF ::State == FBDS_BROWSE

      ::aEditRow := Array( ::FieldCount() )

      IF ::UseDefaultValue
         FOR n := 1 TO ::FieldCount()
           ::aEditRow[ n ] := ::DefaulValue( n )
         NEXT
      ENDIF

      ::nState := FBDS_APPEND
      ::nCurRow := ::RecordCount
      ::lEditRowChanged := .F.

      IF ::oGeneratorLink:ValidLink() .AND. ::oGeneratorLink:nWhereApply == FBGW_ON_NEW_RECORD
         ::SetValue( ::oGeneratorLink:xField, ::oGeneratorLink:Generate() )
      ENDIF

      lRes := .T.
   ENDIF

   RETURN lRes

METHOD Post() CLASS TFbDataSet

   LOCAL lRes := .F.
   LOCAL n

   IF ::Active .AND. ( ::State == FBDS_EDIT .OR. ::State == FBDS_APPEND ) .AND. ::lEditRowChanged

      SWITCH ::State
      CASE FBDS_EDIT
         IF ::lCanUpdate
            lRes := ::UpdateParams( ::oUpdateSQL, ::aUpdateSQL[ 2 ] ) .AND. ::oUpdateSQL:Execute()
         ENDIF
         ::aRows[ ::nCurRow ] := ::aEditRow
         EXIT
      CASE FBDS_APPEND
         IF ::lCanInsert

            IF ::oGeneratorLink:ValidLink() .AND. ::oGeneratorLink:nWhereApply == FBGW_ON_POST
               ::SetValue( ::oGeneratorLink:xField, ::oGeneratorLink:Generate() )
            ENDIF

            lRes := ::UpdateParams( ::oInsertSQL, ::aInsertSQL[ 2 ] ) .AND. ::oInsertSQL:Execute()

            // RETURNING values
            IF lRes .AND. ( ::oInsertSQL:FieldCount() > 0 )
               FOR n := 1 TO ::oInsertSQL:FieldCount()
                  ::SetValue( ::oInsertSQL:FieldInfo( n )[ FBFS_NAME ], ::oInsertSQL:GetValue( n ) )
               NEXT
            ENDIF

         ENDIF

         AAdd( ::aRows, ::aEditRow )

         EXIT
      ENDSWITCH

      ::lEditRowChanged := .F.
      ::nState := FBDS_BROWSE

   ENDIF

   RETURN lRes

METHOD Cancel() CLASS TFbDataSet

   IF ::Active .AND. ( ::State == FBDS_EDIT .OR. ::State == FBDS_APPEND )
      ::nState := FBDS_BROWSE
      ::aEditRow := {}
      IF ::nCurRow > ::RecordCount
         ::nCurRow := ::RecordCount
      ENDIF
      ::lEditRowChanged := .F.
   ENDIF

   RETURN NIL

METHOD Delete() CLASS TFbDataSet

   LOCAL lRes := .F.

   IF ::Active .AND. ::State == FBDS_BROWSE .AND. ::RecordCount > 0 .AND. ::nCurRow > 0

      IF ::lCanDelete
         lRes := ::UpdateParams( ::oDeleteSQL, ::aDeleteSQL[ 2 ] ) .AND. ::oDeleteSQL:Execute()
      ENDIF

      hb_ADel( ::aRows, ::nCurRow, .T. )

      IF ::nCurRow > ::RecordCount
         ::nCurRow := ::RecordCount
      ENDIF

   ENDIF

   RETURN lRes

METHOD Refresh() CLASS TFbDataSet

   LOCAL lRes := .F.
   LOCAL nI

   IF ::Active .AND. ::State == FBDS_BROWSE .AND. ::RecordCount > 0 .AND. ::nCurRow > 0 .AND. ::lCanRefresh

      ::UpdateParams( ::oRefresSQL, ::aRefreshSQL[ 2 ] )

      IF ( lRes := ::oRefreshSQL:Execute() )
         IF ( lRes := ::oRefreshSQL:Fetch() == 0 )
            FOR nI := 1 TO ::oRefreshSQL:FieldCount()
               ::SetValue( ::oRefreshSQL:FieldInfo[ nI ][ 1 ], ::oRefreshSQL:GetData[ nI ], .T. )
            NEXT
         ENDIF
      ENDIF
   ENDIF

   RETURN lRes

METHOD ParamCount() CLASS TFbDataSet

   RETURN ::oSelectSQL:ParamCount()

METHOD ParamInfo( nIndex ) CLASS TFbDataSet

   RETURN ::oSelectSQL:ParamInfo( nIndex )

METHOD SetParam( nIndex, xValue ) CLASS TFbDataSet

   RETURN ::oSelectSQL:SetParam( nIndex, xValue )

METHOD FieldCount() CLASS TFbDataSet

   RETURN ::oSelectSQL:FieldCount()

METHOD FieldInfo( ncIndex ) CLASS TFbDataSet

   RETURN ::oSelectSQL:FieldInfo( ncIndex )

METHOD FieldType( ncIndex ) CLASS TFbDataSet

   RETURN ::oSelectSQL:FieldType( ncIndex )

METHOD FieldLen( ncIndex ) CLASS TFbDataSet

   RETURN ::oSelectSQL:FieldLen( ncIndex )

METHOD FieldDec( ncIndex ) CLASS TFbDataSet

   RETURN ::oSelectSQL:FieldDec( ncIndex )

METHOD GetValue( ncIndex ) CLASS TFbDataSet

   LOCAL xRes := NIL

   IF ::Active
      IF ::State == FBDS_BROWSE
         IF ::RecordCount > 0 .AND. ::nCurRow > 0
            IF HB_ISCHAR( ncIndex )
               ncIndex := ::oSelectSQL:FieldIndex( Upper( ncIndex ) )
            ENDIF

            xRes := ::aRows[ ::nCurRow ][ ncIndex ]
         ENDIF
      ELSE
         IF HB_ISCHAR( ncIndex )
            ncIndex := ::oSelectSQL:FieldIndex( Upper( ncIndex ) )
         ENDIF

         xRes := ::aEditRow[ ncIndex ]
      ENDIF
   ENDIF

   RETURN xRes

METHOD SetValue( ncIndex, xValue, lIsRefresh ) CLASS TFbDataSet

   hb_default( @lIsRefresh, .F. )

   IF ::Active

      IF ::State == FBDS_BROWSE
         IF ::RecordCount > 0 .AND. ::nCurRow > 0 .AND. lIsRefresh
            IF HB_ISCHAR( ncIndex )
               ncIndex := ::oSelectSQL:FieldIndex( Upper( ncIndex ) )
            ENDIF

            ::aRows[ ::nCurRec ][ ncIndex ] := xValue
         ENDIF
      ELSE
         IF HB_ISCHAR( ncIndex )
            ncIndex := ::oSelectSQL:FieldIndex( Upper( ncIndex ) )
         ENDIF

         ::aEditRow[ ncIndex ] := xValue
         ::lEditRowChanged := .T.
      ENDIF

   ENDIF

   RETURN NIL

METHOD DefaulValue( ncIndex ) CLASS TFbDataSet

   LOCAL xVal, aField

   IF HB_ISCHAR( ncIndex )
      ncIndex := ::oSelectSQL:FieldIndex( Upper( ncIndex ) )
   ENDIF

   aField := ::FieldInfo( ncIndex )

   SWITCH aField[ 2 ]

   CASE SQL_TEXT
   CASE SQL_VARYING
      xVal := Space( aField[ 3 ] )
      EXIT

   CASE SQL_SHORT
   CASE SQL_LONG
   CASE SQL_FLOAT
   CASE SQL_DOUBLE
   CASE SQL_D_FLOAT
   CASE SQL_INT64
      xVal := Val( Str( 0, aField[ 3 ], aField[ 4 ] ) )
      EXIT

   CASE SQL_TIMESTAMP
   CASE SQL_TYPE_TIME
   CASE SQL_TYPE_DATE
      xVal := hb_SToD()
      EXIT

   ENDSWITCH

   RETURN xVal

METHOD Locate( aFlds, aValues, lContinue, lCaseSensitive, lPartial ) CLASS TFbDataSet

   LOCAL lRes := .F.
   LOCAL n
   LOCAL xDbVal, xFindVal

   hb_default( @lContinue, .F. )
   hb_default( @lCaseSensitive, .T. )
   hb_default( @lPartial, .F. )

   IF ::Active .AND. ( ::RecordCount > 0 )
      IF ( Len( aFlds ) > 0 ) .AND. ( Len( aFlds ) == Len( aFlds ) )

         IF ! lContinue
            ::GoTop()
         ENDIF

         DO WHILE .NOT. ::Eof()

            FOR n := 1 TO Len( aFlds )

               xDbVal := ::GetValue( aFlds[ n ] )
               xFindVal := aValues[ n ]

               IF ValType( xDbVal ) == ValType( xFindVal )

                  IF ValType( xFindVal ) == "C"

                     IF ! lCaseSensitive
                        xDbVal := Upper( xDbVal )
                        xFindVal := Upper( xFindVal )
                     ENDIF

                     IF lPartial
                        lRes := At( xFindVal, xDbVal ) > 0
                     ELSE
                        lRes := At( xFindVal, xDbVal ) == 1
                     ENDIF

                  ELSE

                     lRes := xDbVal == xFindVal

                  ENDIF

               ENDIF

               IF ! lRes
                  EXIT // forloop
               ENDIF

            NEXT

            IF lRes
               EXIT // dowhile
            ENDIF

            ::Skip( 1 )

         ENDDO

      ENDIF
   ENDIF

   RETURN lRes

METHOD SetDatabase( oValue ) CLASS TFbDataSet

   IF ! ::Active
      ::oDatabase := oValue
      ::oTransaction := NIL

      ::oSelectSQL:Database := oValue
      ::oInsertSQL:Database := oValue
      ::oUpdateSQL:Database := oValue
      ::oDeleteSQL:Database := oValue
      ::oRefreshSQL:Database := oValue
   ENDIF

   RETURN NIL

METHOD SetTransaction( oValue ) CLASS TFbDataSet

   IF ! ::Active
      ::oTransaction := oValue

      IF ::oDatabase == NIL
         ::oDatabase := ::oTransaction:Database
      ENDIF

      ::oSelectSQL:Transaction := oValue
      ::oInsertSQL:Transaction := oValue
      ::oUpdateSQL:Transaction := oValue
      ::oDeleteSQL:Transaction := oValue
      ::oRefreshSQL:Transaction := oValue
   ENDIF

   RETURN NIL

METHOD GetActive() CLASS TFBDataSet

   RETURN ::nState > FBDS_INACTIVE

METHOD SetActive( lValue ) CLASS TFbDataSet

   DO CASE
      CASE ! lValue .AND. ::Active
         ::oSelectSQL:Close()
         ::oInsertSQL:Close()
         ::oUpdateSQL:Close()
         ::oDeleteSQL:Close()
         ::oRefreshSQL:Close()
         ::aRows := {}
         ::lSQLEof := .T.
         ::nCurRow := 0
         ::nState := FBDS_INACTIVE
      CASE lValue .AND. ! ::Active
         IF ::InitSQL()
            ::oSelectSQL:Execute()
            IF ::FetchRecord()
               ::nCurRow := 1
               ::lSQLEof := .F.
            ELSE
               ::lSQLEof := .T.
            ENDIF
            ::nState := FBDS_BROWSE
         ENDIF
   ENDCASE

   RETURN NIL

METHOD SetSelectSQL( cValue ) CLASS TFbDataSet

   IF ! ::Active
      ::cSelectSQL := cValue
   ENDIF

   RETURN NIL

METHOD SetInsertSQL( aValue ) CLASS TFbDataSet

   IF ! ::Active
      ::aInsertSQL := aValue
   ENDIF

   RETURN NIL

METHOD SetUpdateSQL( aValue ) CLASS TFbDataSet

   IF ! ::Active
      ::aUpdateSQL := aValue
   ENDIF

   RETURN NIL

METHOD SetDeleteSQL( aValue ) CLASS TFbDataSet

   IF ! ::Active
      ::aDeleteSQL := aValue
   ENDIF

   RETURN NIL

METHOD SetRefreshSQL( aValue ) CLASS TFbDataSet

   IF ! ::Active
      ::aRefreshSQL := aValue
   ENDIF

   RETURN NIL


CREATE CLASS TFbCustomService

   HIDDEN:

   VAR   pHandle
   VAR   cSvcName
   VAR   aParams
   VAR   cUserName
   VAR   cPassword
   VAR   aSPB

   PROTECTED:

   METHOD SetSPB()
   METHOD CheckActive()
   METHOD InternalQuery( cRequest, cResponse, aParams )

   VISIBLE:

   METHOD New( cServiceName, cUserName, cPassword, aParams )
   DESTRUCTOR Free()
   METHOD Attach()
   METHOD Detach()

   ACCESS Handle INLINE ( ::pHandle )

   ACCESS Active INLINE ( ! Empty( ::pHandle ) )
   ASSIGN Active METHOD SetActive( lValue )

   ACCESS ServiceName INLINE ( ::cSvcName )
   ASSIGN ServiceName( cValue ) INLINE ( ::cSvcName := cValue )

   ACCESS Params INLINE ( ::aParams )
   ASSIGN Params( aValue ) INLINE ( ::aParams := aValue )

   ACCESS UserName INLINE ( ::cUserName )
   ASSIGN UserName( cValue ) INLINE ( ::cUserName := cValue )

   ACCESS Password INLINE ( ::cPasword )
   ASSIGN Password( cValue ) INLINE ( ::cPasword := cValue )
ENDCLASS

METHOD SetSPB() CLASS TFbCustomService

   IF HB_ISARRAY( ::aParams )
      ::aSPB := AClone( ::aParams )
   ELSE
      ::aSPB := {}
   ENDIF

   AAdd( ::aSPB, { ISC_SPB_USER_NAME, ::cUserName } )
   AAdd( ::aSPB, { ISC_SPB_PASSWORD, ::cPassword } )

   RETURN NIL

METHOD CheckActive() CLASS TFbCustomService

   IF ! ::Active
      Eval( ErrorBlock(), FBErrorNew( 1000, "Service is not active" ) )
   ENDIF

   RETURN NIL

METHOD InternalQuery( cRequest, cResponse, aParams ) CLASS TFbCustomService

   LOCAL nRes

   ::CheckActive()

   nRes := FbSvcQuery( ::pHandle, aParams, cRequest, Len( cRequest ), @cResponse )

   IF nRes <> 0
      Eval( ErrorBlock(), FBErrorNew( nRes ) )
   ENDIF

   RETURN NIL

METHOD New( cServiceName, cUserName, cPassword, aParams ) CLASS TFbCustomService

   ::cSvcName := cServiceName
   ::cUserName := cUserName
   ::cPassword := cPassword
   ::aParams := aParams

   RETURN Self

DESTRUCTOR Free() CLASS TFbCustomService

   ::Active := .F.

   RETURN

METHOD Attach() CLASS TFbCustomService

   IF ! ::Active

      ::SetSPB()

      ::pHandle := FbSvcAttach( ::cSvcName, ::aSPB )

      IF HB_ISNUMERIC( ::pHandle )
         Eval( ErrorBlock(), FBErrorNew( ::pHandle ) )
      ENDIF

   ENDIF

   RETURN NIL

METHOD Detach() CLASS TFbCustomService

   IF ::Active

      FbSvcDetach( ::pHandle )

   ENDIF

   RETURN NIL

METHOD SetActive( lValue ) CLASS TFbCustomService

   IF lValue .AND. ! ::Active
      ::Attach()
   ELSEIF ! lValue .AND. ::Active
      ::Detach()
   ENDIF

   RETURN NIL


CREATE CLASS TFbControllService INHERIT TFbCustomService

   PROTECTED:

   VAR   cStartOptions   INIT ""

   METHOD InternalStart()
   METHOD SetStartOptions() VIRTUAL

   METHOD StartOptionAddC( nValue )
   METHOD StartOptionAddS( cValue )
   METHOD StartOptionAddU( nValue )
   METHOD StartOptionAddW( nValue )

   VISIBLE:

   METHOD StartService()

ENDCLASS

METHOD InternalStart() CLASS TFbControllService

   LOCAL nRes

   ::CheckActive()

   ::SetStartOptions()
   IF Len( ::cStartOptions ) == 0
      RETURN NIL
   ENDIF

   nRes := FbSvcStart( ::Handle, ::cStartOptions )

   IF nRes <> 0
      Eval( ErrorBlock(), FBErrorNew( nRes ) )
   ENDIF

   RETURN NIL

METHOD StartOptionAddC( nValue ) CLASS TFbControllService

   ::cStartOptions += Chr( nValue )

   RETURN NIL

METHOD StartOptionAddS( cValue ) CLASS TFbControllService

   ::StartOptionAddW( Len( cValue ) )
   ::cStartOptions += cValue

   RETURN NIL

METHOD StartOptionAddU( nValue ) CLASS TFbControllService

   ::cStartOptions += L2Bin( nValue )

   RETURN NIL

METHOD StartOptionAddW( nValue ) CLASS TFbControllService

   ::cStartOptions += I2Bin( nValue )

   RETURN NIL

METHOD StartService() CLASS TFbControllService

   ::InternalStart()

   RETURN NIL


CREATE CLASS TFbControllAndQueryService INHERIT TFbControllService

   HIDDEN:

   VAR   lEof      INIT .F.

   PROTECTED:

   VAR   nAction   INIT 0

   VISIBLE:

   METHOD GetNextChunk()
   METHOD GetNextLine()

   ACCESS Eof INLINE ( ::lEof )

ENDCLASS

METHOD GetNextChunk() CLASS TFbControllAndQueryService

   LOCAL cRes := ""
   LOCAL cBuf
   LOCAL nLen

   IF ! ::lEof
      cBuf := Space( 1024 * 32 )
      ::InternalQuery( Chr( ISC_INFO_SVC_TO_EOF ), @cBuf )

      IF SubStr( cBuf, 1, 1 ) <> Chr( ISC_INFO_SVC_TO_EOF )
         Eval( ErrorBlock(), FBErrorNew( 1000, "Invalid service response" ) )
      ENDIF

      nLen := FbVAXInteger( SubStr( cBuf, 2, 2 ), 2 )

      IF SubStr( cBuf, 4 + nLen, 1 ) == Chr( ISC_INFO_TRUNCATED )
         ::lEof := .F.
      ELSEIF SubStr( cBuf, 4 + nLen, 1 ) == Chr( ISC_INFO_END )
         ::lEof := .T.
      ELSE
         Eval( ErrorBlock(), FBErrorNew( 1000, "Invalid service response" ) )
      ENDIF

      cRes := SubStr( cBuf, 4, nLen )

   ENDIF

   RETURN cRes


METHOD GetNextLine() CLASS TFbControllAndQueryService

   LOCAL cRes := ""
   LOCAL cBuf
   LOCAL nLen

   IF ! ::lEof
      cBuf := Space( 1024 * 32 )
      ::InternalQuery( Chr( ISC_INFO_SVC_LINE ), @cBuf )

      IF SubStr( cBuf, 1, 1 ) <> Chr( ISC_INFO_SVC_LINE )
         Eval( ErrorBlock(), FBErrorNew( 1000, "Invalid service response" ) )
      ENDIF

      nLen := FbVAXInteger( SubStr( cBuf, 2, 2 ), 2 )

      IF SubStr( cBuf, 4 + nLen, 1 ) != Chr( ISC_INFO_END )
         Eval( ErrorBlock(), FBErrorNew( 1000, "Invalid service response" ) )
      ENDIF

      IF nLen == 0
         ::lEof := .T.
      ELSE
         cRes := SubStr( cBuf, 4, nLen )
      ENDIF

   ENDIF

   RETURN cRes


CREATE CLASS TFbBackupRestoreService INHERIT TFbControllAndQueryService

   HIDDEN:

   VAR   lVerbose   INIT .F.

   VISIBLE:

   ACCESS Verbose INLINE ( ::lVerbose )
   ASSIGN Verbose( lValue ) INLINE ( ::lVerbose := lValue )

ENDCLASS

CREATE CLASS TFbBackupService INHERIT TFbBackupRestoreService

   HIDDEN:

   VAR   cDatabaseName
   VAR   acBackupFile
   VAR   nBlockingFactor   INIT 0
   VAR   aOptions          INIT {}

   PROTECTED:

   METHOD SetStartOptions()

   VISIBLE:

   ACCESS DatabaseName INLINE ( ::cDatabaseName )
   ASSIGN DatabaseName( cValue ) INLINE ( ::cDatabaseName := cValue )

   ACCESS BackupFile INLINE ( ::acBackupFile )
   ASSIGN BackupFile( acValue ) INLINE ( ::acBackupFile := acValue )

   ACCESS BlockingFactor INLINE ( ::nBlockingFactor )
   ASSIGN BlockingFactor( nValue ) INLINE ( ::nBlockingFactor := nValue )

   ACCESS Options INLINE ( ::aOptions )
   ASSIGN Options( aValue ) INLINE ( ::aOptions := aValue )

 ENDCLASS

 METHOD SetStartOptions() CLASS TFbBackupService

   LOCAL nOptions := 0

   ::cStartOptions := ""
   ::StartOptionAddC( ISC_ACTION_SVC_BACKUP )

   ::StartOptionAddC( ISC_SPB_DBNAME )
   ::StartOptionAddS( ::DatabaseName )

   AEval( ::aOptions, { | nOption | nOptions := hb_bitOr( nOptions, nOption ) } )
   ::StartOptionAddC( ISC_SPB_OPTIONS )
   ::StartOptionAddU( nOptions )

   IF ::Verbose
      ::StartOptionAddC( ISC_SPB_VERBOSE )
   ENDIF

   IF ::BlockingFactor > 0
      ::StartOptionAddC( ISC_SPB_BKP_FACTOR )
      ::StartOptionAddU( ::BlockingFactor )
   ENDIF

   IF HB_ISARRAY( ::acBackupFile )
      // TODO: multiple file and size

   ELSE
      ::StartOptionAddC( ISC_SPB_BKP_FILE )
      ::StartOptionAddS( ::acBackupFile )
   ENDIF

   RETURN NIL


CREATE CLASS TFbRestoreService INHERIT TFbBackupRestoreService

   HIDDEN:

   VAR   acDatabaseName
   VAR   acBackupFile
   VAR   nPageBuffers    INIT 0
   VAR   nPageSize       INIT 0
   VAR   aOptions        INIT {}

   PROTECTED:

   METHOD SetStartOptions()

   VISIBLE:

   ACCESS DatabaseName INLINE ( ::acDatabaseName )
   ASSIGN DatabaseName( cValue ) INLINE ( ::acDatabaseName := cValue )

   ACCESS BackupFile INLINE ( ::acBackupFile )
   ASSIGN BackupFile( acValue ) INLINE ( ::acBackupFile := acValue )

   ACCESS PageBuffers INLINE ( ::nPageBuffers )
   ASSIGN PageBuffers( nValue ) INLINE ( ::nPageBuffers := nValue )

   ACCESS PageSize INLINE ( ::nPageSize )
   ASSIGN PageSize( nValue ) INLINE ( ::nPageSize := nValue )

   ACCESS Options INLINE ( ::aOptions )
   ASSIGN Options( aValue ) INLINE ( ::aOptions := aValue )

 ENDCLASS

 METHOD SetStartOptions() CLASS TFbRestoreService

   LOCAL nOptions := 0

   ::cStartOptions := ""
   ::StartOptionAddC( ISC_ACTION_SVC_RESTORE )

   AEval( ::aOptions, { | nOption | nOptions := hb_bitOr( nOptions, nOption ) } )
   ::StartOptionAddC( ISC_SPB_OPTIONS )
   ::StartOptionAddU( nOptions )

   IF ::Verbose
      ::StartOptionAddC( ISC_SPB_VERBOSE )
   ENDIF

   IF ::PageBuffers > 0
      ::StartOptionAddC( ISC_SPB_RES_BUFFERS )
      ::StartOptionAddU( ::PageBuffers )
   ENDIF

   IF ::PageSize > 0
      ::StartOptionAddC( ISC_SPB_RES_PAGE_SIZE )
      ::StartOptionAddU( ::PageSize )
   ENDIF

   IF HB_ISARRAY( ::DatabaseName )
      // TODO: multiple file and size

   ELSE
      ::StartOptionAddC( ISC_SPB_DBNAME )
      ::StartOptionAddS( ::DatabaseName )
   ENDIF

   IF HB_ISARRAY( ::BackupFile )
      // TODO: multiple file and size

   ELSE
      ::StartOptionAddC( ISC_SPB_BKP_FILE )
      ::StartOptionAddS( ::BackupFile )
   ENDIF

   RETURN NIL

/* ------------------------------------ */













STATIC FUNCTION FBErrorNew( nErrorCode, cDescription, cOperation )

   LOCAL oErr

   oErr := ErrorNew()

   oErr:subSystem := "HBFBIRD"
   oErr:genCode := 0
   oErr:subCode := nErrorCode
   IF HB_ISCHAR( cDescription )
      oErr:description := cDescription
   ELSE
      oErr:description := FBError( nErrorCode )
   ENDIF
   IF HB_ISCHAR( cOperation )
      oErr:operation := cOperation
   ENDIF

   RETURN oErr
