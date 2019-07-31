{$DEFINE USE_SERIALIZE}
{$UNDEF TRemotableTypeInitializer_Initialize}
{$UNDEF WST_TKPROCVAR}
{$UNDEF WST_UNICODESTRING}
{$UNDEF WST_SEMAPHORE_TIMEOUT}
{$UNDEF WST_TKHELPER}
{$DEFINE WST_HAS_TDURATIONREMOTABLE}
{$DEFINE WST_HAS_TTIMEREMOTABLE}
{$DEFINE WST_HAS_STRICT_DELIMITER}

{$WARNINGS OFF}

  {$IFDEF FPC}
    {$mode objfpc}{$H+}
    {$DEFINE HAS_QWORD}
    {$DEFINE HAS_BUILT_IN_64UINT}
    {$DEFINE HAS_TKBOOL}
    {$UNDEF WST_INTF_DOM}
    {$DEFINE WST_RECORD_RTTI}
    //{$DEFINE USE_INLINE}
    {$IF Defined(FPC_VERSION) and 
         ( (FPC_VERSION > 2) or
           ( (FPC_VERSION = 2) and 
             (FPC_RELEASE > 4)
           )
         )
    }
      { $DEFINE HAS_JSON_CLONE}
    {$IFEND}
    {$IF Defined(FPC_VERSION) and 
         ( (FPC_VERSION > 2) or
           ( (FPC_VERSION = 2) and 
             (FPC_RELEASE > 4)
           )
         )
    }              
      {$DEFINE WST_HAS_JSON_INT64}
      {$DEFINE WST_TKHELPER}
    {$IFEND}   

    {$DEFINE WST_TKPROCVAR}
    {$DEFINE WST_UNICODESTRING}
    {$DEFINE TDOMNodeList_RELEASE_NOT_FREE}
    {$DEFINE HAS_FORMAT_SETTINGS}
    {$IFDEF WINDOWS}
      {$DEFINE WST_SEMAPHORE_TIMEOUT}
    {$ENDIF WINDOWS}
    {$IF Defined(FPC_FULLVERSION) and (FPC_FULLVERSION >= 20600) }
      {$DEFINE HAS_DEFAULT_FORMAT_SETTINGS}
    {$IFEND}
    {$IF Defined(FPC_FULLVERSION) and (FPC_FULLVERSION > 20602) }
      {$DEFINE HAS_EXP_TREE}
    {$IFEND}
    {$IF Defined(FPC_FULLVERSION) and (FPC_FULLVERSION >= 20701) }
      {$DEFINE WST_TKFILE}
      {$DEFINE WST_TKCLASSREF}
      {$DEFINE WST_TKPOINTER}
    {$IFEND}
  {$ENDIF}

  {$IFNDEF FPC}
    {$DEFINE WST_DELPHI}
    {$DEFINE DELPHI}
    {$UNDEF HAS_QWORD}
    {$UNDEF HAS_TKBOOL}
    {$UNDEF USE_INLINE}
    {$DEFINE WST_RECORD_RTTI}
    {$DEFINE WST_INTF_DOM}
    {$DEFINE HAS_FORMAT_SETTINGS}
    {$IFDEF VER140} //Delphi 6
      {$UNDEF HAS_FORMAT_SETTINGS}
      {$UNDEF WST_HAS_STRICT_DELIMITER}
    {$ENDIF}
    {$IFDEF VER150} //Delphi 7
      {$UNDEF HAS_FORMAT_SETTINGS}   
      {$UNDEF WST_HAS_STRICT_DELIMITER}
    {$ENDIF}
    {$IFDEF VER170} //Delphi 2005
      {$UNDEF HAS_FORMAT_SETTINGS}
    {$ENDIF}
    {$IFDEF VER180} //Delphi 2006
      {$UNDEF HAS_FORMAT_SETTINGS}
    {$ENDIF}
    {$IFDEF VER180} //Delphi 2007
      {$UNDEF HAS_FORMAT_SETTINGS}
    {$ENDIF}
    {$IFDEF VER185} //Delphi 2007
      {$UNDEF HAS_FORMAT_SETTINGS}
    {$ENDIF}      
    {$IFDEF VER200} // Delphi 2009
      {$DEFINE HAS_BUILT_IN_64UINT}
      {$DEFINE WST_UNICODESTRING}
      {$DEFINE USE_INLINE}
    {$ENDIF}
    {$IFDEF VER210} // Delphi 2010
      {$DEFINE HAS_BUILT_IN_64UINT}
      {$DEFINE WST_UNICODESTRING}
      {$DEFINE USE_INLINE}
      {$DEFINE WST_TKCLASSREF}
      {$DEFINE WST_TKPOINTER}
      {$DEFINE WST_TKPROCEDURE}
    {$ENDIF}
    {$IFDEF VER220} // Delphi XE
      {$DEFINE HAS_BUILT_IN_64UINT}
      {$DEFINE WST_UNICODESTRING}
      {$DEFINE USE_INLINE}
      {$DEFINE WST_TKCLASSREF}
      {$DEFINE WST_TKPOINTER}
      {$DEFINE WST_TKPROCEDURE}
    {$ENDIF}
    {$IFDEF VER230} // Delphi XE2
      {$DEFINE HAS_BUILT_IN_64UINT}
      {$DEFINE WST_UNICODESTRING}
      {$DEFINE USE_INLINE}
      {$DEFINE WST_TKCLASSREF}
      {$DEFINE WST_TKPOINTER}
      {$DEFINE WST_TKPROCEDURE}
    {$ENDIF}
    {$IFDEF VER240} // Delphi XE3
      {$DEFINE HAS_BUILT_IN_64UINT}
      {$DEFINE WST_UNICODESTRING}
      {$DEFINE USE_INLINE}
      {$DEFINE WST_TKCLASSREF}
      {$DEFINE WST_TKPOINTER}
      {$DEFINE WST_TKPROCEDURE}
    {$ENDIF}
    {$IFDEF VER250} // Delphi XE4
      {$DEFINE HAS_BUILT_IN_64UINT}
      {$DEFINE WST_UNICODESTRING}
      {$DEFINE USE_INLINE}
      {$DEFINE WST_TKCLASSREF}
      {$DEFINE WST_TKPOINTER}
      {$DEFINE WST_TKPROCEDURE}
    {$ENDIF}
    {$IFDEF VER260} // Delphi XE5
      {$DEFINE HAS_BUILT_IN_64UINT}
      {$DEFINE WST_UNICODESTRING}
      {$DEFINE USE_INLINE}
      {$DEFINE WST_TKCLASSREF}
      {$DEFINE WST_TKPOINTER}
      {$DEFINE WST_TKPROCEDURE}
    {$ENDIF}
    {$IFDEF VER270} // Delphi XE6
      {$DEFINE HAS_BUILT_IN_64UINT}
      {$DEFINE WST_UNICODESTRING}
      {$DEFINE USE_INLINE}
      {$DEFINE WST_TKCLASSREF}
      {$DEFINE WST_TKPOINTER}
      {$DEFINE WST_TKPROCEDURE}
    {$ENDIF}
    {$IFDEF VER280} // Delphi XE7
      {$DEFINE HAS_BUILT_IN_64UINT}
      {$DEFINE WST_UNICODESTRING}
      {$DEFINE USE_INLINE}
      {$DEFINE WST_TKCLASSREF}
      {$DEFINE WST_TKPOINTER}
      {$DEFINE WST_TKPROCEDURE}
    {$ENDIF}
    {$IFDEF VER290} // Delphi XE8
      {$DEFINE HAS_BUILT_IN_64UINT}
      {$DEFINE WST_UNICODESTRING}
      {$DEFINE USE_INLINE}
      {$DEFINE WST_TKCLASSREF}
      {$DEFINE WST_TKPOINTER}
      {$DEFINE WST_TKPROCEDURE}
    {$ENDIF}
    {$IFDEF VER300} // Delphi XE 10
      {$DEFINE HAS_BUILT_IN_64UINT}
      {$DEFINE WST_UNICODESTRING}
      {$DEFINE USE_INLINE}
      {$DEFINE WST_TKCLASSREF}
      {$DEFINE WST_TKPOINTER}
      {$DEFINE WST_TKPROCEDURE}
    {$ENDIF}
    {$DEFINE WST_SEMAPHORE_TIMEOUT}
    {$IFDEF MSWINDOWS}
      {$IFNDEF WINDOWS}
        {$DEFINE WINDOWS}
      {$ENDIF WINDOWS}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF CPU86}
    {$DEFINE HAS_COMP}
  {$ENDIF}

{$WARNINGS ON}
