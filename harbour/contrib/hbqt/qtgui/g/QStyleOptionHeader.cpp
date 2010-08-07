/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum SectionPosition { Beginning, Middle, End, OnlyOneSection }
 *  enum SelectedPosition { NotAdjacent, NextIsSelected, PreviousIsSelected, NextAndPreviousAreSelected }
 *  enum SortIndicator { None, SortUp, SortDown }
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionHeader>


/*
 * QStyleOptionHeader ()
 * QStyleOptionHeader ( const QStyleOptionHeader & other )
 */

typedef struct
{
   QStyleOptionHeader * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionHeader;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionHeader )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QStyleOptionHeader   /.\\", p->ph ) );
         delete ( ( QStyleOptionHeader * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QStyleOptionHeader   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QStyleOptionHeader    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QStyleOptionHeader    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStyleOptionHeader( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionHeader * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionHeader;
   p->type = HBQT_TYPE_QStyleOptionHeader;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QStyleOptionHeader", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QStyleOptionHeader", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSTYLEOPTIONHEADER )
{
   QStyleOptionHeader * pObj = NULL;

   pObj =  new QStyleOptionHeader() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionHeader( ( void * ) pObj, true ) );
}

/*
 * QIcon icon
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_ICON )
{
   hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( hbqt_par_QStyleOptionHeader( 1 )->icon ), true ) );
}

/*
 * Qt::Alignment iconAlignment
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_ICONALIGNMENT )
{
   hb_retni( ( Qt::Alignment ) hbqt_par_QStyleOptionHeader( 1 )->iconAlignment );
}

/*
 * Qt::Orientation orientation
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_ORIENTATION )
{
   hb_retni( ( Qt::Orientation ) hbqt_par_QStyleOptionHeader( 1 )->orientation );
}

/*
 * SectionPosition position
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_POSITION )
{
   hb_retni( ( QStyleOptionHeader::SectionPosition ) hbqt_par_QStyleOptionHeader( 1 )->position );
}

/*
 * int section
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_SECTION )
{
   hb_retni( hbqt_par_QStyleOptionHeader( 1 )->section );
}

/*
 * SelectedPosition selectedPosition
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_SELECTEDPOSITION )
{
   hb_retni( ( QStyleOptionHeader::SelectedPosition ) hbqt_par_QStyleOptionHeader( 1 )->selectedPosition );
}

/*
 * SortIndicator sortIndicator
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_SORTINDICATOR )
{
   hb_retni( ( QStyleOptionHeader::SortIndicator ) hbqt_par_QStyleOptionHeader( 1 )->sortIndicator );
}

/*
 * QString text
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_TEXT )
{
   hb_retc( hbqt_par_QStyleOptionHeader( 1 )->text.toLatin1().data() );
}

/*
 * Qt::Alignment textAlignment
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_TEXTALIGNMENT )
{
   hb_retni( ( Qt::Alignment ) hbqt_par_QStyleOptionHeader( 1 )->textAlignment );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/