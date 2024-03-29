unit ulazautoupdate;

{
  Original DownloadHTTP code: wiki.freepascal.org
  Thread source: http://freepascalanswers.wordpress.com/2012/06/15/synapas-http-thread/
  VersionSupport:  Mike Thompson - mike.cornflake@gmail.com
  Added to and modified by minesadorada@charcodelvalle.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

interface


uses
  Forms, Classes, SysUtils, strutils, LazUTF8,LazFileUtils,FileUtil, Dialogs, StdCtrls,
  Buttons, httpsend, DateUtils, asyncprocess, zipper, LResources,
  VersionSupport, inifiles, aboutlazautoupdateunit, uappisrunning, LCLProc;

const
//  C_OnlineAppPath =
//    'https://downloads.sourceforge.net/project/%s/files/%s/%s/download';
  C_OnlineAppPath =
    'http://downloads.sourceforge.net/project/%s/%s/%s';
  // [updatepath,projectname,filename]
  C_TLazAutoUpdateComponentVersion = '0.1.28';
  C_LAUTRayINI = 'lauimport.ini';

{
 V0.0.1: Initial alpha
 V0.0.2: Added auOther properties
 V0.0.3: Limit to download time
         auOther code working
 V0.0.4: Added Events
 V0.0.5: Improved error handling in DownloadNewVersion
         Added LastError property
         Added VersionCountLimit rpoerty
         Added DownloadCountLimit property
 V0.0.6: Linux implementation
         Property CopyTree added
 V0.0.7: Added Sleep(1) to download code
 V0.0.8: UpdateToNewVersion now shells AppFileWithPath
 V0.0.9: AutoUpdate method added
 V0.1.0: Added WaitFor routine to simulate Sleep in Linux
 V0.1.1: Tidied up Debugmode
         Improved version comparison
 V0.1.3: Fixed bug whereby Setting AppVersion property gave incorrect value
 V0.1.4: Added public AppVersionNumber property
 V0.1.5: Added Scrollbars to the WhatsNew memo
 V0.1.6: Added Public ResetAppVersion method
         Added Private VersionStringToNumber function
 V0.1.7: Added Public SilentUpdate method
         Added public properties:
         LCLVersion,WidgetSet, FPCVersion,LastCompiled,TargetOS
 V0.1.8: Bugfix: Removed Parent Form code if fSilentMode=TRUE
 V0.1.9: Added public AppRunningSilentUpdate method
 V0.1.10:Added uAppIsRunning unit
         Added IsAppActive public function
 V0.1.11:SilentInstall method
 V0.1.12:Moved ProgramDir references to fAppFileName references
 V0.1.13:Bugfix Update
 V0.1.14:Added debug messages in DoSilentUpdate method
 V0.1.15:DoSilentUpdate: Added code for CopyTree=TRUE/FALSE
         Changed some Ansi functions to UTF8
 V0.1.16:$IFDEF WINDOWS UpdateToNewVersion kills app if running
 V0.1.17:Added public methods CreateLocalLauImportFile and RelocateLauImportFile
 V0.1.18:Added public method RemoteUpdateToNewVersion
 V0.1.19:Improved VersionStringToNumber
 V0.1.20:Bugfix: 'No Build Information Available' -> '0.0.0.0'
 V0.1.21:Bugfix: RemoteUpdate killed app too soon
 V0.1.22:lauimport file is not re-created if it already exists
 V0.1.23:Bugfix: PrettyName in lauimport sometimes contained duplicated OS info
 V0.1.24:Bugfix to CreateLocalLauImportFile
         More checks on PrettyName
 V0.1.25:Changed default: CopyTree = TRUE
 V0.1.26:Updated uses clause for FileUtils.
 V0.1.27: ??
}
  C_TThreadedDownloadComponentVersion = '0.0.2';
{
 V0.0.1: Initial alpha
 V0.0.2: Added fDebugmode to all classes and functions
 V0.0.3: ??
}
  C_OnlineVersionsININame = 'versions.ini'; // User can change
  C_UpdatesFolder = 'updates'; // User can change
  C_WhatsNewFilename = 'whatsnew.txt';
  C_INISection = 'versions';
  C_GUIEntry = 'GUI';
  C_ModuleEntry = 'Module';
  {$IFDEF WINDOWS}
  C_Updater = 'updatehm.exe';
  C_LOCALUPDATER = 'lauupdate.exe';
  {$ELSE}
  C_Updater = 'updatehm';
  C_LOCALUPDATER = 'lauupdate';
  {$ENDIF}

resourcestring
  C_ComponentPrettyName = 'Lazarus Auto-Update Component';
  C_TempVersionsININame = 'new%s'; // [C_OnlineVersionsININame]
  C_Checking = 'Checking for updates...';
  C_NoSFTypes = 'Sorry only ProjectType = auSourceForge is supported in this version';
  C_Downloading = 'Please wait.  Downloading new version... ';
  C_ConsoleTitle = 'Updating %s'; //[fAppFileName]
  C_TakingTooLong =
    'Check is taking too long (bad/slow internet connection?). Try again later?';
  C_Error500 = 'There is a problem with the Internet connection (error %d)  Try again later?';
  C_Error404 = 'Cannot find the file at this time. (error %d)  Try again later?';
  C_UnableToDelete = 'Sorry, unable to delete %s%sPlease delete it manually';
  C_OK = 'OK';
  C_NotProperFileName = 'This is not a proper file name';
  C_WhatsNewInVersion = 'What''s new in version %s';
  C_PropIsEmpty = 'Property SFProjectName is empty!';
  C_ThreadDownloadCrash = 'ThreadDownloadHTTP Crashed! (NewVersionAvailable)';
  C_DownloadedBytes = 'Downloaded %s: %d bytes';
  C_UnableToDeleteOld = 'Unable to delete old files in %s';
  C_DirectoryProblems = 'Problems with the % directory';
  C_ThreadDownloadHTTPCrash = 'ThreadDownloadHTTP Crashed! (NewVersionAvailable)';
  C_DownloadSuccess = 'Downloaded new version %s sucessfully.';
  C_UnableToDownload = 'Unable to download new version%sReturn code was %d';
  C_PleaseWaitProcessing = 'Please wait. Processing....';
  C_UpdaterMissing = 'Missing %s';
  C_FolderMissing = 'Missing %s folder';
  C_NotApplicable = '<not applicable>';
  C_ThreadStarted = 'Thread Started';
  C_SourceForgeDownload = 'SourceForge download';
  C_CannotLoadFromRemote = 'Cannot load document from remote server';
  C_DownloadIsEmpty = 'Downloaded document is empty.';
  C_DownloadFailedErrorCode = 'Download failed with error code ';
  rsANewVersionS = 'A new version %s is available.  Would you like to download it?';
  rsVewVersionSH = 'Vew version %s has downloaded. Click OK to update now.';
  rsCancelledYou = 'Cancelled.  You can download and update to the new version' +
    ' later.';
  rsDownloadFail = 'Download failed. (HTTP Errorcode %d) Try again later';
  rsCancelledYou2 = 'Cancelled.  You can download the new version later.';
  rsThisApplicat = 'This application is up-to-date';

type
  tc = class(tthread)
    procedure Execute; override;
  end;


type

  TProjectType = (auSourceForge, auOther);
  // Array of these records used for multiple updates
  UpdateListRecord = record
    PrettyName: string;
    Path: string;
    VersionString: string;
    VersionNumber: cardinal;
  end;
  TThreadedDownload = class; // Forward declaration
  {TLAZAUTOUPDATE}
  TOnNewVersionAvailable = procedure(Sender: TObject; Newer: boolean;
    OnlineVersion: string) of object;
  TOnDownloaded = procedure(Sender: TObject; ResultCode, BytesDownloaded: integer) of
    object;
  TOnDebugEvent = procedure(Sender: TObject; lauMethodName, lauMessage: string) of object;

  TLazAutoUpdate = class(TAboutLazAutoUpdate)
  private
    fSourceForgeProjectName: string;
    fApplicationVersionString: string;
    fApplicationVersionNumber: integer;
    fProjectType: TProjectType;
    fThreadDownload: TThreadedDownload;
    fAppFileName: string;
    fComponentVersion: string;
    fShowUpdateInCaption: boolean;
    fUpdateList: array of UpdateListRecord;
    fUpdateListCount: integer;
    fUpdatesFolder: string;
    fDownloadZipName: string;
    fVersionsININame: string;
    fParentApplication: TApplication;
    fParentForm: TForm;
    fGUIOnlineVersion: string;
    fShowDialogs: boolean;
    fDownloadInprogress: boolean;
    FUpdateHMProcess: TAsyncProcess;
    fauOtherSourceURL: string;
    fauOtherSourceFilename: string;
    WhatsNewForm: TForm;
    WhatsNewMemo: TMemo;
    cmdClose: TBitBtn;
    FOnNewVersionAvailable: TOnNewVersionAvailable;
    FOnDownloaded: TOnDownloaded;
    fOnDebugEvent: TOnDebugEvent;
    fLastError: string;
    fVersionCountLimit, fDownloadCountLimit: cardinal;
    fZipfileName: string;
    fCopyTree: boolean;
    fDebugMode, fFireDebugEvent: boolean;
    fSilentMode: boolean;
    fLCLVersion, fWidgetSet, fFPCVersion, fLastCompiled, fTargetOS: string;
    procedure SetProjectType(AValue: TProjectType);
    // projectype=auOther property Sets
    procedure SetauOtherSourceFilename(AValue: string);
    procedure SetauOtherSourceURL(AValue: string);

    procedure SetSourceForgeProjectName(Avalue: string);
    procedure SetAppFilename(Avalue: string);
    procedure SetApplicationVersionString(Avalue: string);
    procedure SetShowDialogs(AValue: boolean);
    procedure SetDebugMode(AValue: boolean);
    function GetThreadDownloadReturnCode: integer;
    function IsSourceForgeVersionNewer(const sznewINIPath: string): boolean;
    function VersionStringToNumber(AVersionString: string): integer;
    function DoSilentUpdate: boolean;
  protected

  public
    constructor Create(AOwner: TComponent); override;

    {Main functions}
    // If  NewVersionAvailable then DownloadNewVersion then UpdateToNewVersion
    // Returns TRUE if GUIVersion > AppVersion
    function NewVersionAvailable: boolean;
    // Returns TRUE if successful
    function DownloadNewVersion: boolean;
    // Returns TRUE if successful
    function UpdateToNewVersion: boolean;
    // Put in form.activate. Shows <whatsnew.txt> only if in ProgramDirectory then deletes it. Exits otherwise
    procedure ShowWhatsNewIfAvailable;
    // Checks for new version then shows dialogs to update
    procedure AutoUpdate;
    // No dialogs - what it says on the tin.
    function SilentUpdate: boolean;
    // Used in SilentUpdate. Shells to local lauupdate(.exe)
    function RemoteUpdateToNewVersion: boolean;
    // Returns TRUE if EXEName is running under Windows or Linux
    function AppIsActive(const ExeName: string): boolean;

    // Resets AppVersion property to the ownling application version
    procedure ResetAppVersion;
    // Create a new lauimport.ini in the App folder
    function CreateLocalLauImportFile: boolean;
    // If lauimport.ini is found in the app folder, move it to the AppData folder
    procedure RelocateLauImportFile;
    // Download lists (now superceded by CopyTree)
    // TODO: Use Indexed properties to handle list access
    function AddToUpdateList(APrettyName, APath, AVersionString: string;
      AVersionNumber: cardinal): integer;
    procedure ClearUpdateList;
    property UpdateListCount: integer read fUpdateListCount;


    // GUI can use these properties during and after downloads
    // NewVersionAvailable sets this.  It is the online version
    property GUIOnlineVersion: string read fGUIOnlineVersion;
    // Set by NewVersionAvailable and DownLoadNewVersion
    property ReturnCode: integer read GetThreadDownloadReturnCode;
    // Set by NewVersionAvailable and DownLoadNewVersion when running
    property DownloadInprogress: boolean read fDownloadInprogress;

    // The name of the zipfile in the remote <updates> directory
    property DownloadZipName: string read fDownloadZipName;
    // The Path + Filename of the app to overwite and then run
    property AppFileWithPath: string read fAppFilename write SetAppFilename;
    // The version string of the app to be updated.  You can set this to '0.0.0.0' for a definite update.
    property AppVersion: string read fApplicationVersionString
      write SetApplicationVersionString;
    // Can be queried
    property LastError: string read fLastError;
    // Debugging use only
    property DebugMode: boolean read fDebugMode write SetDebugMode;
    property AppVersionNumber: integer read fApplicationVersionNumber;

    // Info useful for About dialogs
    property LCLVersion: string read fLCLVersion;
    property WidgetSet: string read fWidgetSet;
    property FPCVersion: string read fFPCVersion;
    property LastCompiled: string read fLastCompiled;
    property TargetOS: string read fTargetOS;
  published
    // Events
    property OnNewVersionAvailable: TOnNewVersionAvailable
      read FOnNewVersionAvailable write FOnNewVersionAvailable;
    property OnDownloaded: TOnDownloaded read fOnDownloaded write fOnDownloaded;
    property OnDebugEvent: TOnDebugEvent read fOnDebugEvent write fOnDebugEvent;

    // Embedded class
    property ThreadDownload: TThreadedDownload read fThreadDownload write fThreadDownload;
    // Set this property before using methods
    property SFProjectName: string read fSourceForgeProjectName
      write SetSourceForgeProjectName;
    // Only auSourceForge at V0.0.1
    // For when fProjectType = auOther
    // Fully qualified URL (not including the filename).
    property auOtherSourceURL: string read fauOtherSourceURL write SetauOtherSourceURL;
    // Just the filename of the file to be downloaded (can be zipped)
    property auOtherSourceFilename: string read fauOtherSourceFilename
      write SetauOtherSourceFilename;

    property ProjectType: TProjectType
      read fProjectType write SetProjectType default auSourceForge;
    // Version of this component
    property AutoUpdateVersion: string read fComponentVersion;
    // Zipfile contains a whole directory tree (relative to App Directory)
    property CopyTree: boolean read fCopyTree write fCopyTree default TRUE;
    // Default is 'updates' *must be the same in SourceForge file section*
    property UpdatesFolder: string read fUpdatesFolder write fUpdatesFolder;
    // Default=versions.ini  File in SourceForge /updates folder
    property VersionsININame: string read fVersionsININame write fVersionsININame;
    // Default is to modify parent form's caption during downloads
    property ShowUpdateInCaption: boolean read fShowUpdateInCaption
      write fShowUpdateInCaption default False;
    // Set to FALSE if you want to handle them in form code
    property ShowDialogs: boolean read fShowDialogs write SetShowDialogs default False;
    // How many counts to wait until 'Too long' meesage quits out
    property VersionCountLimit: cardinal read fVersionCountLimit write fVersionCountLimit;
    // How many counts to wait until 'Too long' meesage quits out
    property DownloadCountLimit: cardinal read fDownloadCountLimit
      write fDownloadCountLimit;
    // Default is application filename.zip
    property ZipfileName: string read fZipfileName write fZipfileName;

  end;

  {TThreadedDownload }
  TThreadedDownload = class(TPersistent)
  private
    fURL: string;
    fFileName: string;
    fReturnCode: integer;
    fThreadFinished: boolean;
    fDownloadSize: integer;
    fUnzipAfter: boolean;
    fComponentVersion: string;
    fApplicationVersionString: string;
    fIsSourceForge: boolean;
  public
    fDebugMode: boolean;
    fShowDialogs: boolean;
    fLastError: string;  // Propagated to TLazAutoUpdate
    constructor Create;
    // Starts the thread
    function ThreadDownloadHTTP: boolean;
    // Called when the thread is done
    procedure DownloadTerminiated(Sender: TObject);
    // Passed to the thread
    property URL: string read fURL write fURL;
    // Passed to the thread
    property Filename: string read fFileName write fFileName;
    // From TLazAutoUpdate
    property AppVersion: string read fApplicationVersionString
      write fApplicationVersionString;
    // From the thread
    property ReturnCode: integer read fReturnCode write fReturnCode;
    // From DownloadTerminated
    property ThreadFinished: boolean read fThreadFinished write fThreadFinished;
    // From the thread
    property DownloadSize: integer read fDownloadSize write fDownloadSize;
    // From TLazAutoUpdate
    property UnzipAfter: boolean read fUnzipAfter;
    // From TLazAutoUpdate
    property IsSourceForge: boolean read fIsSourceForge;
  published
    // Version of the underlying thread class
    property ThreadDownloadVersion: string read fComponentVersion;
  end;

  {TDownloadThreadClass }
  TDownloadThreadClass = class(TThread)
  private
    fURL: string;
    fFileName: string;
  public
    fIsSourceForge: boolean; // Propagated from TLazAutoUpdate
    fDebugMode: boolean; // propagated from TLazAutoUpdate
    fShowDialogs: boolean; // propagated from TLazAutoUpdate
    fDownloadSize: integer; // propagated to TThreadedDownload
    fReturnCode: integer; // Propagated to TThreadedDownload
    fLastError: string;  // Propagated to TThreadedDownload
    constructor Create(URL, FileName: string);
    procedure Execute; override; // Starts thread
  end;



// Non-threaded version (redundant v0.0.1)
function DownloadHTTP(URL, TargetFile: string;
  var ReturnCode, DownloadSize: integer; bIsSourceForge, fDebugMode: boolean): boolean;

procedure Register;

implementation

procedure Register;
begin
  {$I lazautoupdate_icon.lrs}
  RegisterComponents('System', [TLazAutoUpdate]);
end;

procedure tc.Execute;
begin

end;

procedure WaitFor(const MillisecondDelay: longword);
// Linux - this proc is intentionally thread-blocking
var
  ThisSecond: longword;
begin
  ThisSecond := MilliSecondOfTheDay(Now);
  while MilliSecondOfTheDay(Now) < (ThisSecond + MillisecondDelay) do ;
end;


constructor TLazAutoUpdate.Create(AOwner: TComponent);
var
  sz: string;
begin
  inherited Create(AOwner); // TComponent method;

  { initialise threading system }
  with tc.Create(False) do
  begin
    waitfor;
    Free;
  end;


  fThreadDownload := TThreadedDownload.Create();
  // Leave URL and Filename to be set via properties
  fComponentVersion := C_TLazAutoUpdateComponentVersion;
  ClearUpdateList;
  fUpdateListCount := 0;
  fApplicationVersionString := GetFileVersion;
  if (fApplicationVersionString = 'No build information available') then
    fApplicationVersionString := '0.0.0.0';

  fCopyTree := TRUE; // User can change
  // UpdateList: Redundant?
  AddToUpdateList('', ParamStrUTF8(0), GetFileVersion, 0);

  fProjectType := auSourceForge; // User can change
  fUpdatesFolder := C_UpdatesFolder; // User can change
  fVersionsININame := C_OnlineVersionsININame; // User can change
  fShowUpdateInCaption := False; // User can change
  fShowDialogs := False; // User can change
  fDebugMode := False;
  fFireDebugEvent := False;
  fSilentMode := False;

  // Propagate down
  fThreadDownload.fDebugmode := fDebugMode;
  if fProjectType = auSourceForge then
    fThreadDownload.fIsSourceForge := True
  else
    fThreadDownload.fIsSourceForge := False;

  fApplicationVersionNumber := VersionStringToNumber(fApplicationVersionString);

  fLastError := C_OK;

  fVersionCountLimit := 1000000; // default
  fDownloadCountLimit := 10000000; // default

  // Grab the application and form objects from the application
  fParentApplication := Tapplication(AOwner.Owner);
  fParentForm := TForm(AOwner);

  fZipfileName := ''; // assign later

  // Assorted versioninfo properties
  fLCLVersion := GetLCLVersion;
  fWidgetSet := GetWidgetSet;
  fFPCVersion := GetCompilerInfo;
  fLastCompiled := GetCompiledDate;
  fTargetOS := GetOS;


  // AboutBox properties
  AboutBoxComponentName := Format('Laz Auto-update v%s',
    [C_TLazAutoUpdateComponentVersion]);
  ;
  AboutBoxWidth := 400;
  AboutBoxHeight := 450;
  sz := 'A component for updating your application' + LineEnding;
  sz += 'Designed for projects hosted by SourceForge' + LineEnding + LineEnding;
  sz += 'Main methods:' + LineEnding;
  sz += 'Procedure AutoUpdate' + LineEnding;
  sz += 'Function NewVersionAvailable: Boolean' + LineEnding;
  sz += 'Function DownloadNewVersion: Boolean' + LineEnding;
  sz += 'Function UpdateToNewVersion: Boolean' + LineEnding;
  sz += 'Procedure ShowWhatsNewIfAvailable' + LineEnding;
  sz += 'For troubleshooting, set DebugMode=TRUE';
  AboutBoxTitle := 'LazAutoUpdate';
  AboutBoxDescription := sz;
  // AboutBoxBackgroundColor:=clWindow;
  //AboutBoxFontName (string)
  //AboutBoxFontSize (integer)
  AboutBoxVersion := C_TLazAutoUpdateComponentVersion;
  AboutBoxAuthorname := 'Gordon Bamber';
  //AboutBoxOrganisation (string)
  AboutBoxAuthorEmail := 'minesadorada@gmail.com';
  AboutBoxLicenseType := 'MODIFIEDGPL';
end;

function TLazAutoUpdate.AppIsActive(const ExeName: string): boolean;
begin
  Result := AppIsRunning(ExeName);
end;

function TLazAutoUpdate.VersionStringToNumber(AVersionString: string): integer;
  // Converts 'n.n.n.n' into an integer
var
  s: string;
  i: integer;
begin
  Result := 0;
  // Fetch the 4 (or less) version elements and make into an Integer
  s := ExtractDelimited(1, AVersionString, ['.']);
  if TryStrToInt(s, i) then
    Result := Result + (i * 10000);
  s := ExtractDelimited(2, AVersionString, ['.']);
  if TryStrToInt(s, i) then
    Result := Result + (i * 1000);
  s := ExtractDelimited(3, AVersionString, ['.']);
  if TryStrToInt(s, i) then
    Result := Result + (i * 100);
  s := ExtractDelimited(4, AVersionString, ['.']);
  if TryStrToInt(s, i) then
    Result := Result + i;
end;

procedure TLazAutoUpdate.ResetAppVersion;
begin
  fApplicationVersionString := GetFileVersion;
  if (fApplicationVersionString = 'No build information available') then
    fApplicationVersionString := '0.0.0.0';
  fApplicationVersionNumber := VersionStringToNumber(fApplicationVersionString);
end;

procedure TLazAutoUpdate.SetShowDialogs(AValue: boolean);
begin
  fShowDialogs := AValue;
  if fThreadDownload <> nil then
    fThreadDownload.fShowDialogs := AValue;
end;

procedure TLazAutoUpdate.SetDebugMode(AValue: boolean);
begin
  fDebugMode := AValue;
  // Fire the OnDebugEvent event handler?
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := fDebugMode;
  if fThreadDownload <> nil then
    fThreadDownload.fDebugMode := AValue;
end;

procedure TLazAutoUpdate.SetauOtherSourceURL(AValue: string);
// Must end in '/'
begin
  if ((AValue <> fauOtherSourceURL) and (AValue <> '')) then
  begin
    if not AnsiEndsStr('/', AValue) then
      AValue += '/';
    fauOtherSourceURL := AValue;
  end;
end;

procedure TLazAutoUpdate.SetauOtherSourceFilename(AValue: string);
begin
  if ((AValue <> fauOtherSourceFilename) and (AValue <> '')) then
  begin
    try
      fauOtherSourceFilename := ExtractFileName(AValue);
    except
      ShowMessage(C_NotProperFileName);
    end;
  end;
end;


procedure TLazAutoUpdate.ShowWhatsNewIfAvailable;
begin
  // Should be called on form.activate
  // Afer an update, the 'whatsnew.txt' is copied into the application's folder
  // This routine shows it, then deletes it
  // If it isn't there, then it exits early

  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;

  if not FileExistsUTF8(ProgramDirectory + C_WhatsNewFilename) then
  begin
    if fFireDebugEvent then
      fOndebugEvent(Self, 'ShowWhatsNewIfAvailable', 'Unable to locate ' +
        C_WhatsNewFilename);
    Exit;
  end;
  // Create the form, memo and close button
  if fParentForm <> nil then
    WhatsNewForm := TForm.CreateNew(fParentForm)
  else
    WhatsNewForm := TForm.CreateNew(fParentApplication);

  WhatsNewMemo := TMemo.Create(WhatsNewForm);
  cmdClose := TBitBtn.Create(WhatsNewForm);

  try // ..finally destroy all
    with WhatsNewForm do
    begin
      Height := 480;
      Width := 640;
      // BorderStyle:=bsToolWindow;
      Caption := Format(C_WhatsNewInVersion, [GetFileVersion]);
      DefaultMonitor := dmActiveForm;
      // FormStyle:=fsStayOnTop;
      Position := poScreenCenter;
      ShowInTaskBar := stNever;
    end;
    with WhatsNewMemo do
    begin
      Height := WhatsNewForm.Height - 80;
      Width := WhatsNewForm.ClientWidth;
      ReadOnly := True;
      ScrollBars := ssAutoBoth;
      WordWrap := True;
      Parent := WhatsNewForm;
      Lines.LoadFromFile(ProgramDirectory + C_WhatsNewFilename);
    end;
    with cmdClose do
    begin
      Top := WhatsNewForm.Height - Height - 20;
      Left := (WhatsNewForm.Width div 2) - (Width div 2);
      Kind := bkClose;
      Parent := WhatsNewForm;
    end;
    // Show the window modally (cmdClose will close it)
    WhatsNewForm.ShowModal;
    try
      // Delete the whatsnew.txt now the user has seen it
      if not SysUtils.DeleteFile(ProgramDirectory + C_WhatsNewFilename) then
        if fShowDialogs then
          ShowMessageFmt(C_UnableToDelete,
            [C_WhatsNewFilename, LineEnding]);
    except
      // Ignore Exceptions
    end;
  finally
  {
  cmdClose.Free; // Not needed
  WhatsNewMemo.Free; // Not needed
  }
    FreeAndNil(WhatsNewForm); // Free the form and its minions
  end;
end;

function TLazAutoUpdate.SilentUpdate: boolean;
begin
  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;

  Result := False;
  fSilentMode := True;
  fShowUpdateInCaption := False;
  fShowDialogs := False;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'SilentUpdate', 'Calling UpdateToNewVersion');

  // Use the local lauupdate if available
  if FileExistsUTF8(ProgramDirectory + C_LOCALUPDATER) then
  begin
    if RemoteUpdateToNewVersion then
      // If IsAppRunning=FALSE, then calls DoSilentUpdate
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'SilentUpdate', 'UpdateToNewVersion succeeded');
      fSilentMode := False;
      Result := True;
    end;

  end
  else
  begin
    if UpdateToNewVersion then
      // If IsAppRunning=FALSE, then calls DoSilentUpdate
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'SilentUpdate', 'UpdateToNewVersion succeeded');
      fSilentMode := False;
      Result := True;
    end;
  end;
end;

procedure TLazAutoUpdate.AutoUpdate;
// Do-all proc that user can drop into a menu
begin
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;

  if fFireDebugEvent then
    fOndebugEvent(Self, 'AutoUpdate', 'Calling NewVersionAvailable');
  if NewVersionAvailable then
  begin
    if fFireDebugEvent then
      fOndebugEvent(Self, 'AutoUpdate', 'NewVersionAvailable succeeded');

    if MessageDlg(fParentApplication.Title, Format(rsANewVersionS,
      [fGUIOnlineVersion]), mtConfirmation, [mbYes, mbNo], 0, mbYes) = 6 then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'AutoUpdate', 'Calling DownloadNewVersion');

      if DownloadNewVersion then
      begin
        if fFireDebugEvent then
          fOndebugEvent(Self, 'AutoUpdate', 'DownloadNewVersion suceeded');
        if MessageDlg(fParentApplication.Title,
          Format(rsVewVersionSH, [fGUIOnlineVersion]), mtConfirmation,
          [mbOK, mbCancel], 0, mbOK) = 1 then
        begin
          if fFireDebugEvent then
            fOndebugEvent(Self, 'AutoUpdate', 'Calling UpdateToNewVersion');
          UpdateToNewVersion;
        end
        else
          MessageDlg(fParentApplication.Title,
            rsCancelledYou,
            mtInformation, [mbOK], 0);
      end
      else
        MessageDlg(fParentApplication.Title,
          Format(rsDownloadFail, [GetThreadDownloadReturnCode]),
          mtInformation, [mbOK], 0);
    end
    else
      MessageDlg(fParentApplication.Title,
        rsCancelledYou2,
        mtInformation, [mbOK], 0);
  end
  else
    MessageDlg(fParentApplication.Title,
      rsThisApplicat,
      mtInformation, [mbOK], 0);
end;

function TLazAutoUpdate.IsSourceForgeVersionNewer(const sznewINIPath: string): boolean;
  // Compares version contained in szTempXMLPath INI file
  // to fApplicationVersionNumber
var
  VersionINI: TIniFile;
  iGUIVersion: integer;
{
C_INISection = 'versions';
C_GUIEntry ='GUI';
C_ModuleEntry = 'Module';
}
begin
  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;

  Result := False;
  VersionINI := TIniFile.Create(sznewINIPath);
  try
    fGUIOnlineVersion := VersionINI.ReadString(C_INISection, C_GUIEntry, '0.0.0.0');
  finally
    VersionINI.Free;
  end;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'IsSourceForgeVersionNewer',
      Format('fGUIOnlineVersion=%s, fApplicationVersionString=%s, szTempXMLPath=%s',
      [fGUIOnlineVersion, fApplicationVersionString, sznewINIPath]));

  // Fetch the 4 (or less) version elements and make into an Integer
  // so 1.10 > 1.9.9.9
  iGUIVersion := VersionStringToNumber(fGUIOnlineVersion);

  if fFireDebugEvent then
    fOndebugEvent(Self, 'IsSourceForgeVersionNewer',
      Format('iGUIVersion=%d, fApplicationVersionNumber=%d',
      [iGUIVersion, fApplicationVersionNumber]));

  // Test: Is the online version newer?
  if (iGUIVersion > fApplicationVersionNumber) then
    Result := True;
end;


function TLazAutoUpdate.NewVersionAvailable: boolean;
  // Returns TRUE is a new version is available
var
  szURL, szTargetPath: string;
  cCount: cardinal;
  szOldCaption: string;
begin
  Result := False;

  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;

  if fSourceForgeProjectName = '' then
  begin
    if fShowDialogs then
      ShowMessage(C_PropIsEmpty);
    if fFireDebugEvent then
      fOndebugEvent(Self, 'NewVersionAvailable', C_PropIsEmpty);
    Exit;
  end;
  if fZipFileName = '' then
  begin
    fZipfileName := ChangeFileExt(ExtractFilename(fAppFilename), '.zip');
    if fFireDebugEvent then
      fOndebugEvent(Self, 'NewVersionAvailable',
        Format('Assigning ZipFile name %s', [fZipfileName]));
  end;

  if fProjectType = auSourceForge then
  begin
    szURL := Format(C_OnlineAppPath, [fSourceForgeProjectName,
      fUpdatesFolder, fVersionsININame]);

    if fFireDebugEvent then
      fOndebugEvent(Self, 'NewVersionAvailable',
        Format('SourceForgeURL is %s', [szURL]));

    szTargetPath := AppendPathDelim(ExtractFilePath(fAppFilename)) +
      Format(C_TempVersionsININame, [fVersionsININame]);

    if fFireDebugEvent then
      fOndebugEvent(Self, 'NewVersionAvailable',
        Format('Target Path %s', [szTargetPath]));

    if fProjectType = auOther then
      // fauOtherSourceURL ends with '/'
    begin
      szURL := fauOtherSourceURL + fVersionsININame;
    end;

    // Delete any old versions
    try
      if FileExistsUTF8(szTargetPath) then
      begin
        SysUtils.DeleteFile(szTargetPath);
        if fFireDebugEvent then
          fOndebugEvent(Self, 'NewVersionAvailable',
            Format('Deleted old file %s', [szTargetPath]));
      end;
    except
      if fFireDebugEvent then
        fOndebugEvent(Self, 'NewVersionAvailable',
          Format('Failed to delete old file %s', [szTargetPath]));
      // No error if the delete can't be done
    end;
    with fThreadDownload do
    begin
      URL := szURL;
      Filename := szTargetPath;
      if not fSilentMode then
        szOldCaption := fParentForm.Caption;
      // Initialise fields
      ThreadFinished := False;
      ReturnCode := 0;
      DownloadSize := 0;
      fDownloadInprogress := True;
      if not fSilentMode then
        fParentForm.Caption := C_Checking;
      // Start the thread
      ThreadDownloadHTTP;
      if fFireDebugEvent then
        fOndebugEvent(Self, 'NewVersionAvailable',
          Format('ThreadDownloadHTTP return Code was %d', [fReturnCode]));
      if fFireDebugEvent then
        fOndebugEvent(Self, 'NewVersionAvailable',
          Format('ThreadDownloadHTTP Last Error was %s', [fLastError]));

      cCount := 0;
      // Update the GUI during the thread
      try
        while (ThreadFinished = False) do
        begin
          Inc(cCount);
          Sleep(1);
          fParentApplication.ProcessMessages;
          ThreadSwitch();
        {$IFDEF WINDOWS}
          if fShowUpdateInCaption then
            fParentForm.Caption := Format(C_Checking + ' %d', [cCount])
          else
            Sleep(10);
        {$ENDIF}
          fParentApplication.ProcessMessages;
          if (cCount > fVersionCountLimit) then
          begin
            if fShowDialogs then
              ShowMessage(C_TakingTooLong);
            ThreadFinished := True;
            fDownloadSize := 0;
            fDownloadInprogress := False;
            if not fSilentMode then
              fParentForm.Caption := szOldCaption;
            Exit;
          end;
        end;
      except
        ThreadFinished := True;
        fDownloadSize := 0;
        fDownloadInprogress := False;
        if not fSilentMode then
          fParentForm.Caption := szOldCaption;
        if fFireDebugEvent then
          fOndebugEvent(Self, 'NewVersionAvailable',
            C_ThreadDownloadCrash);
        Exit;
      end;
      if fFireDebugEvent then
        fOndebugEvent(Self, 'NewVersionAvailable',
          Format('After Threadfinished: Return Code was %d', [fReturnCode]));
      Sleep(1);
      fDownloadInprogress := False;
      if fDownloadSize > 0 then
      begin
        if fFireDebugEvent then
          fOndebugEvent(Self, 'NewVersionAvailable',
            Format('Downloaded %s OK', [szTargetPath]));
        fParentApplication.ProcessMessages;
        Result := IsSourceForgeVersionNewer(szTargetPath);
        if fFireDebugEvent then
          fOndebugEvent(Self, 'NewVersionAvailable',
            Format(C_DownloadedBytes, [szTargetPath, fDownloadSize]));
      end
      else
      if fFireDebugEvent then
      fOndebugEvent(Self, 'NewVersionAvailable',
        Format('DownloadSize was %d', [fDownloadSize]));

    end;
  end;
  if not fSilentMode then
    fParentForm.Caption := szOldCaption;
  if Assigned(fOnNewVersionAvailable) then
    fOnNewVersionAvailable(Self, Result, fGUIOnlineVersion);
end;

function TLazAutoUpdate.DownloadNewVersion: boolean;
  // Returns TRUE is download succeeded
  // If FALSE. then examine ReturnCode property
var
  szURL, szTargetPath, szUpdatesFolder: string;
  cCount: cardinal;
  szOldCaption: string;
  iDownloadedSize: integer;
  FileStringList: TStringList;
  iCount: integer;
begin
  Result := False;
  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;
  iDownloadedSize := 0;
  if fZipFileName = '' then
  begin
    fZipfileName := ChangeFileExt(ExtractFilename(fAppFilename), '.zip');
    if fFireDebugEvent then
      fOndebugEvent(Self, 'DownloadNewVersion',
        Format('ZipFilename was empty.  Assigned %s', [fZipfileName]));
  end;
  szTargetPath := fZipfileName;
  szURL := Format(C_OnlineAppPath, [fSourceForgeProjectName, fUpdatesFolder,
    ExtractFileName(szTargetPath)]);
  szUpdatesFolder := AppendPathDelim(ExtractFilePath(fAppFilename)) + fUpdatesFolder;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'DownloadNewVersion',
      Format('Download parameters: TargetPath=%s%sURL=%s%sUpdatesFolder=%s',
      [szTargetPath, LineEnding, szURL, LineEnding, szUpdatesFolder]));


  // If updates folder exists, delete previous contents
  // If not, then create updates folder
  try
    if DirPathExists(szUpdatesFolder) then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DownloadNewVersion',
          Format('Files already exist in %s.  They will be deleted.',
          [szUpdatesFolder]));

      FileStringList := TStringList.Create;
      try
        FileStringList := FindAllFiles(szUpdatesFolder, '*.*', True);
        try
          for iCount := 0 to FileStringList.Count - 1 do
            SysUtils.DeleteFile(FileStringList[iCount]);
        except
          fLastError := Format(C_UnableToDeleteOld, [szUpdatesFolder]);
          if fDebugMode then
            ShowMessage(fLastError);
        end;
      finally
        FileStringList.Free;
      end;
    end
    else
    begin
      if ForceDirectory(szUpdatesFolder) then
        if fFireDebugEvent then
          fOndebugEvent(Self, 'DownloadNewVersion',
            Format('New directory %s was created', [szUpdatesFolder]));
    end;
  except
    if fFireDebugEvent then
      fOndebugEvent(Self, 'DownloadNewVersion',
        Format(C_DirectoryProblems, [szUpdatesFolder]));
    raise Exception.CreateFmt(C_DirectoryProblems, [szUpdatesFolder]);
  end;

  // Set target to the updates folder
  szTargetPath := AppendPathDelim(szUpdatesFolder) + ExtractFileName(szTargetPath);

  if fProjectType = auOther then
    // fauOtherSourceURL ends with PathDelimiter
  begin
    szURL := fauOtherSourceURL + fauOtherSourceFilename;
    szTargetPath := szUpdatesFolder + PathDelim + fauOtherSourceFilename;
  end;

  if fFireDebugEvent then
    fOndebugEvent(Self, 'DownloadNewVersion',
      Format('szURL=%s, szTargetPath=%s', [szURL, szTargetPath]));

  // Double-check: Delete any earlier updates?
  try
    if FileExistsUTF8(szTargetPath) then
    begin
      SysUtils.DeleteFile(szTargetPath);
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DownloadNewVersion',
          Format('Deleting old file %s', [szTargetPath]));
    end;
  except
    // Ignore exceptions
  end;

  fDownloadInprogress := True;
  // Do the download
  with fThreadDownload do
  begin
    // Initialise fields
    URL := szURL;
    Filename := szTargetPath;
    ThreadFinished := False;
    ReturnCode := 0;
    DownloadSize := 0;
    fUnzipAfter := True;
    if not fSilentMode then
      szOldCaption := fParentForm.Caption;
    if not fSilentMode then
      fParentForm.Caption := C_Downloading;
    // Start the thread
    ThreadDownloadHTTP;
    cCount := 0;
    begin
      // The thread is running....
      cCount := 0;
      try
        while (ThreadFinished = False) do
        begin
          Inc(cCount);
          Sleep(1);
          fParentApplication.ProcessMessages;
        {$IFDEF WINDOWS}
          if fShowUpdateInCaption then
            fParentForm.Caption := Format(C_Downloading + ' %d', [cCount])
          else
            sleep(10);
          fParentApplication.ProcessMessages; // Keep GUI responsive

          if (cCount > fDownloadCountLimit) then // Download taking too long?
          begin
            fDownloadInprogress := False;
            if not fSilentMode then
              fParentForm.Caption := szOldCaption;
            if fFireDebugEvent then
              fOndebugEvent(Self, 'DownloadNewVersion', C_TakingTooLong);
            ThreadFinished := True;
            fDownloadSize := 0;
            Exit;
          end;
        {$ENDIF}
        end;
        iDownloadedSize := fDownloadSize;
      except
        if fFireDebugEvent then
          fOndebugEvent(Self, 'DownloadNewVersion', C_ThreadDownloadHTTPCrash);
      end;
      fDownloadInprogress := False;
      Sleep(1);
      // We have the HTTP return code in MyThreadDownload.ReturnCode.  Normal=200
      if (ReturnCode <> 0) and (ReturnCode < 400) then
      begin
        // Success! New version is unzipped and ready in the /updates folder
        // Shell into the updater app here
        // 1) Closes this app
        // 2) Copies the szUpdateFolder/downloadedexe to the .exe
        // 3) Restarts this (updated) app (showing C_WhatsNewFilename?)
        Result := (fDownloadSize > 0);
        if fFireDebugEvent then
        begin
          fOndebugEvent(Self, 'DownloadNewVersion',
            Format(C_DownloadedBytes, [ExtractFilename(szTargetPath),
            fDownloadSize]));
          fOndebugEvent(Self, 'DownloadNewVersion',
            Format(C_DownloadSuccess, [fGUIOnlineVersion]));
        end;
      end
      else
      begin
        Result := False;
        if fFireDebugEvent then
          fOndebugEvent(Self, 'DownloadNewVersion',
            Format(C_UnableToDownload, [LineEnding, ReturnCode]));
        Free;
      end;
    end;
  end;
  // Fire the event?
  if not fSilentMode then
    fParentForm.Caption := C_PleaseWaitProcessing;
  if Assigned(fOnDownloaded) then
    fOnDownloaded(Self, ReturnCode, iDownloadedSize);
  if not fSilentMode then
    fParentForm.Caption := szOldCaption;
end;

function UTF8StringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
var
  uS, uOld: string;
  // Warning! Always changes string to UPPERCASE
begin
  if rfIgnoreCase in Flags then
  begin
    uS := UTF8UpperCase(S);
    uOld := UTF8UpperCase(OldPattern);
    Flags := Flags - [rfIgnoreCase]; //no point uppercasing again
    Result := StringReplace(uS, uOld, NewPattern, Flags);
  end
  else
    Result := StringReplace(S, OldPattern, NewPattern, Flags);
end;

function TLazAutoUpdate.CreateLocalLauImportFile: boolean;
var
  LAUTRayINI: TIniFile;
  szSection: string;
  szSuffix:String;
begin
  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'CreateLocalLauImportFile', 'CreateLocalLauImportFile called');
  if FileExistsUTF8(ProgramDirectory + C_LAUTRayINI) then
  begin
    Result := True;
    Exit;
  end;
  // Make up OS-Bitness suffix
  {$IFDEF WINDOWS}
  szSuffix:='win';
  {$ELSE}
  szSuffix:='linux';
  {$ENDIF}
  {$IFDEF CPU64}
  szSuffix+='64';
  {$ELSE}
  szSuffix+='32';
  {$ENDIF}
  Result := False;
  LAUTRayINI := TIniFile.Create(ProgramDirectory + C_LAUTRayINI);
  try
    with LAUTRayINI do
    begin
      if fParentApplication <> nil then
        szSection := fParentApplication.Title
      else
      if fParentForm <> nil then
        szSection := fParentForm.Caption
      else
        szSection := 'My Application';
      If ((AnsiContainsText(szSection,{$I %FPCTARGETOS%}) = FALSE)
      AND (AnsiContainsText(szSection,szSuffix) = FALSE)) then
         szSection += szSuffix;
      WriteString(szSection, 'AppPrettyName', szSection);
      WriteString(szSection, 'AppPath', ExtractFilename(fAppFilename));
      WriteString(szSection, 'INIPath', fVersionsININame);
      WriteString(szSection, 'ZipPath', fZipfileName);
      WriteString(szSection, 'AppVersion', fApplicationVersionString);
      WriteString(szSection, 'SFProjectName', fSourceForgeProjectName);
      WriteString(szSection, 'SFUpdatesDirectory', fUpdatesFolder);
      WriteString(szSection, 'Location', ExtractFilePath(fAppFilename));
      //Suggest a schedule
      WriteInteger(szSection, 'IntervalType', 0);
      WriteInteger(szSection, 'IntervalDay', 0);
      WriteInteger(szSection, 'IntervalDate', 1);
      WriteInteger(szSection, 'IntervalHour', 9);
      WriteInteger(szSection, 'Update', 0);
      WriteString(szSection, 'LastCheckDateTime', '2000-01-01 00-00');
      UpdateFile;
      Result := True;
    end;
  finally
    FreeAndNil(LAUTRayINI);
  end;
end;

procedure TLazAutoUpdate.RelocateLauImportFile;
// If C_LAUTRayINI is found in the App Folder, it is moved to the <LocalAppData>/updatehm folder
var
  szSourceLAUTrayPath, szDestLAUTrayPath, szDestLAUTrayDirectory: string;
begin
  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;
  szSourceLAUTrayPath := ExtractFilePath(fAppFilename) + C_LAUTRayINI;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'RelocateLauImportFile',
      Format('Looking for %s.', [szSourceLAUTrayPath]));

  if FileExistsUTF8(szSourceLAUTrayPath) then
  begin
    if fFireDebugEvent then
      fOndebugEvent(Self, 'RelocateLauImportFile',
        Format('Found %s.', [szSourceLAUTrayPath]));
    // Make up local <AppData>/updatehm/C_LAUTRayINI path
    szDestLAUTrayPath := GetAppConfigDirUTF8(False, False); // Don't create it yet
    {$IFDEF WINDOWS}
    szDestLAUTrayPath := StringReplace(szDestLAUTrayPath, Application.Title,
      'updatehm', [rfReplaceAll]);
    {$ELSE}
    szDestLAUTrayPath := UTF8StringReplace(szDestLAUTrayPath,
      Application.Title, 'updatehm', [rfReplaceAll]);
    {$ENDIF}
  end
  else
    Exit; // Nothing to do
  // szDestLAUTrayPath := LowerCase(szDestLAUTrayPath);
  szDestLAUTrayDirectory := ExtractFilePath(szDestLAUTrayPath);

  if DirectoryExistsUTF8(szDestLAUTrayDirectory) then
  begin
    if fFireDebugEvent then
      fOndebugEvent(Self, 'RelocateLauImportFile',
        Format('%s already exists.', [szDestLAUTrayDirectory]));
  end
  else
  begin
    if fFireDebugEvent then
      fOndebugEvent(Self, 'RelocateLauImportFile',
        Format('%s does not previously exist.', [szDestLAUTrayDirectory]));
    if ForceDirectory(szDestLAUTrayDirectory) then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'RelocateLauImportFile',
          Format('Created folder %s.', [szDestLAUTrayDirectory]));
    end
    else
    if fFireDebugEvent then
      fOndebugEvent(Self, 'RelocateLauImportFile',
        Format('Unable to create folder %s.', [szDestLAUTrayDirectory]));
  end;

  // Don't copy over an existing file
  if not FileExistsUTF8(szDestLAUTrayPath + C_LAUTRayINI) then
  begin
    // Move C_LAUTRayINI from app folder to local <AppData> folder
    if CopyFile(szSourceLAUTrayPath, szDestLAUTrayPath + C_LAUTRayINI,
      [cffOverwriteFile]) then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'RelocateLauImportFile',
          Format('Relocated %s from %s to %s', [C_LAUTRayINI,
          szSourceLAUTrayPath, szDestLAUTrayPath]));
      DeleteFile(szSourceLAUTrayPath);
    end
    else
    if fFireDebugEvent then
      fOndebugEvent(Self, 'RelocateLauImportFile',
        Format('Failed to relocate %s from %s to %s',
        [C_LAUTRayINI, szSourceLAUTrayPath, szDestLAUTrayPath]));
  end;
end;

function TLazAutoUpdate.DoSilentUpdate: boolean;
  // Called from UpdateToNewVersion when the app is not running
  // Updates the app, and also copies over and updates C_LAUTRayINI
var
  szAppFolder: string;
  szLAUTrayAppPath: string;
  INI: TINIFile;
  SectionStringList: TStrings;
  szTempUpdatesFolder: string;
begin
  Result := False;
  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'DoSilentUpdate', 'Starting DoSilentUpdate');


  if not FileExistsUTF8(fAppFilename) then
  begin
    if fFireDebugEvent then
      fOndebugEvent(Self, 'DoSilentUpdate',
        Format('AppFilename %s is missing.  Exiting routine', [fAppFilename]));
    Exit;

  end;
  // uses fUpdatesFolder
  szTempUpdatesFolder := AppendPathDelim(ExtractFilePath(fAppFilename) + fUpdatesFolder);

  if not DirectoryExistsUTF8(szTempUpdatesFolder) then
  begin
    if fFireDebugEvent then
      fOndebugEvent(Self, 'DoSilentUpdate',
        Format('Updates folder %s was missing.', [szTempUpdatesFolder]));
    if ForceDirectory(szTempUpdatesFolder) then
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DoSilentUpdate',
          Format('Created folder %s.', [szTempUpdatesFolder]));
  end;

  szAppFolder := AppendPathDelim(ExtractFilePath(fAppFilename));
  // Copy over everything from the updates folder
  if fFireDebugEvent then
    fOndebugEvent(Self, 'DoSilentUpdate',
      Format('About to copy from %s to %s', [szTempUpdatesFolder, szAppFolder]));

  if fCopyTree then
  begin
    if CopyDirTree(szTempUpdatesFolder, szAppFolder,
      [cffOverwriteFile, cffCreateDestDirectory]) then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DoSilentUpdate',
          Format('CopyTree successful from %s to %s',
          [szTempUpdatesFolder, szAppFolder]));
    end
    else
    if fFireDebugEvent then
      fOndebugEvent(Self, 'DoSilentUpdate',
        Format('Error: CopyTree unsuccessful from %s to %s',
        [szTempUpdatesFolder, szAppFolder]));
  end
  else
  begin
    // Copy over app
    if FileUtil.CopyFile(szTempUpdatesFolder + ExtractFileName(fAppFilename),
      szAppFolder + ExtractFileName(fAppFilename), [cffOverwriteFile]) then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DoSilentUpdate',
          Format('Copied app from %s to %s', [szTempUpdatesFolder +
          ExtractFileName(fAppFilename), szAppFolder + ExtractFileName(fAppFilename)]));
    end
    else
    if fFireDebugEvent then
      fOndebugEvent(Self, 'DoSilentUpdate',
        Format('Failed to copy app from %s to %s',
        [szTempUpdatesFolder + ExtractFileName(fAppFilename),
        szAppFolder + ExtractFileName(fAppFilename)]));

    // Copy over WhatsNew
    if FileUtil.CopyFile(szTempUpdatesFolder + 'whatsnew.txt',
      szAppFolder + 'whatsnew.txt', [cffOverwriteFile]) then
    begin
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DoSilentUpdate',
          Format('Copied whatsnew.txt from %s to %s',
          [szTempUpdatesFolder + 'whatsnew.txt', szAppFolder + 'whatsnew.txt']));
    end
    else
    if fFireDebugEvent then
      fOndebugEvent(Self, 'DoSilentUpdate',
        Format('Failed to copy whatsnew.txt from %s to %s',
        [szTempUpdatesFolder + 'whatsnew.txt', szAppFolder + 'whatsnew.txt']));

  end;
  // Deal with C_LAUTRayINI
  // Copied to the global application data folder
  // Add entry 'Location'
  if fFireDebugEvent then
    fOndebugEvent(Self, 'DoSilentUpdate', 'About to process ' +
      szTempUpdatesFolder + C_LAUTRayINI);

  if FileExistsUTF8(szTempUpdatesFolder + C_LAUTRayINI) then
  begin
    szLAUTrayAppPath := GetAppConfigDirUTF8(False, True); // Create it if necessary
    if fFireDebugEvent then
      fOndebugEvent(Self, 'DoSilentUpdate',
        Format('App data directory is %s', [szLAUTrayAppPath]));
    // AppDataDirectory/LazAutoUpdater/
    szLAUTrayAppPath := UTF8StringReplace(szLAUTrayAppPath, Application.Title,
      'updatehm', [rfIgnoreCase, rfReplaceAll]);
    szLAUTrayAppPath := LowerCase(szLAUTrayAppPath);

    if fFireDebugEvent then
      fOndebugEvent(Self, 'DoSilentUpdate',
        Format('App data directory changed to %s', [szLAUTrayAppPath]));
    // Now AppDataDirectory/updatehm/
    try
      if ForceDirectory(szLAUTrayAppPath) then
        if fFireDebugEvent then
          fOndebugEvent(Self, 'DoSilentUpdate',
            Format('Made directory %s', [szLAUTrayAppPath]));

      if fFireDebugEvent then
        fOndebugEvent(Self, 'DoSilentUpdate',
          Format('About to copy %s to %s', [szTempUpdatesFolder +
          C_LAUTRayINI, szLAUTrayAppPath]));

      Fileutil.CopyFile(szTempUpdatesFolder + C_LAUTRayINI, szLAUTrayAppPath +
        C_LAUTRayINI, [cffOverWriteFile]);
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DoSilentUpdate',
          Format('Successfully copied %s to %s ',
          [C_LAUTRayINI, szLAUTrayAppPath]));

      if FileExistsUTF8(szLAUTrayAppPath + C_LAUTRayINI) then
      begin
        INI := TINIFile.Create(szLAUTrayAppPath + C_LAUTRayINI);
        SectionStringList := TStringList.Create;
        try
          INI.ReadSections(SectionStringList);
          if SectionStringList.Count > 0 then
          begin
            INI.WriteString(SectionStringList[0], 'Location',
              ExtractFilePath(fAppFilename));
            if fFireDebugEvent then
              fOndebugEvent(Self, 'DoSilentUpdate',
                Format('Wrote new entry in section %s.  Location=%s',
                [SectionStringList[0], ExtractFilePath(fAppFilename)]));
          end
          else
          if fFireDebugEvent then
            fOndebugEvent(Self, 'DoSilentUpdate',
              'Failed to find a valid section in ' + C_LAUTRayINI);
        finally
          FreeAndNil(SectionStringList);
          FreeAndNil(INI);
        end;
        Result := True;
      end
      else
      if fFireDebugEvent then
        fOndebugEvent(Self, 'DoSilentUpdate',
          'Failed to copy ' + C_LAUTRayINI + ' to ' + szLAUTrayAppPath);
    except
      On E: Exception do
        if fFireDebugEvent then
          fOndebugEvent(Self, 'DoSilentUpdate',
            Format('Could not update %s.  Error: %s ', [C_LAUTRayINI, E.Message]));
    end;
  end;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'DoSilentUpdate', 'Leaving DoSilentUpdate');

end;

function TLazAutoUpdate.RemoteUpdateToNewVersion: boolean;
  // Shells to 'lauupdate' console app in ProgramDirectory to remotely update an app
var
  cCount: cardinal;
  szAppDir: string;
begin
  Result := False;
  szAppDir := AppendPathDelim(ExtractFilePath(fAppFilename));

  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'RemoteUpdateToNewVersion',
      'Entering RemoteUpdateToNewVersion');

  // Running update using updatehm?
  if ((AppIsRunning(ExtractFileName(fAppFilename)) = False) and
    (ExtractFileName(fAppFilename) <> ExtractFileName(fparentApplication.EXEname))) then
    Result := DoSilentUpdate
  else
  begin
    cCount := 0;
    if not FileExistsUTF8(ProgramDirectory + C_LOCALUPDATER) then
    begin
      if fShowDialogs then
        ShowMessageFmt(C_UpdaterMissing, [ProgramDirectory + C_LOCALUPDATER]);
      if fFireDebugEvent then
        fOndebugEvent(Self, 'RemoteUpdateToNewVersion',
          Format(C_UpdaterMissing, [ProgramDirectory + C_LOCALUPDATER]));
      Exit;
    end;

    if not DirectoryExistsUTF8(szAppDir + fUpdatesFolder) then
    begin
      if fShowDialogs then
        ShowMessageFmt(C_FolderMissing, [szAppDir + fUpdatesFolder]);
      if fFireDebugEvent then
        fOndebugEvent(Self, 'RemoteUpdateToNewVersion',
          Format(C_FolderMissing, [szAppDir + fUpdatesFolder]));
      Exit;
    end;



    // Update and re-start the app
    FUpdateHMProcess := TAsyncProcess.Create(nil);
    try
      FUpdateHMProcess.Executable := ProgramDirectory + C_LOCALUPDATER;
      FUpdateHMProcess.CurrentDirectory := ProgramDirectory;
      if not fSilentMode then
        FUpdateHMProcess.ConsoleTitle :=
          Format(C_ConsoleTitle, [fParentApplication.Title]);
      FUpdateHMProcess.Parameters.Clear;
      FUpdateHMProcess.Parameters.Add(fAppFilename); //Param 1 = EXEname
      FUpdateHMProcess.Parameters.Add(fUpdatesFolder); // Param 2 = updates
      FUpdateHMProcess.Parameters.Add(C_WhatsNewFilename); // Param 3 = whatsnew.txt
      FUpdateHMProcess.Parameters.Add(fParentApplication.Title); // Param 4 = Prettyname
      if (fCopyTree = True) then
        FUpdateHMProcess.Parameters.Add('copytree');
      // Param 5 = Copy the whole of /updates to the App Folder
      if fFireDebugEvent then
        fOndebugEvent(Self, 'RemoteUpdateToNewVersion',
          Format('Executing %s', [ProgramDirectory + C_LOCALUPDATER]));
      FUpdateHMProcess.Execute;

      // Check for C_WhatsNewFilename in the app directory in a LOOP
      if fFireDebugEvent then
        fOndebugEvent(Self, 'RemoteUpdateToNewVersion',
          Format('Waiting for %s', [szAppDir + C_WhatsNewFilename]));
      while not FileExistsUTF8(szAppDir + C_WhatsNewFilename) do
      begin
        fParentApplication.ProcessMessages;
        Inc(CCount);
        if cCount > 10000000 then
          Break; // Get out of jail in case updatehm.exe fails to copy file
      end;

      // remotely shut down the app?
      if fSilentMode then
      begin
        If AppIsRunning(ExtractFileName(fAppFilename)) then KillApp(ExtractFileName(fAppFilename));
        if fFireDebugEvent then
          fOndebugEvent(Self, 'RemoteUpdateToNewVersion',
            Format('Killing %s ready for update', [fAppFilename]));
      end;

    finally
      FUpdateHMProcess.Free;
      if not fSilentMode then
        fParentForm.Close;
    end;
    if fFireDebugEvent then
      fOndebugEvent(Self, 'RemoteUpdateToNewVersion',
        'Success');
    Result := True;
  end;
end;

function TLazAutoUpdate.UpdateToNewVersion: boolean;
var
  cCount: cardinal;
  szAppDir: string;
begin
  Result := False;
  szAppDir := AppendPathDelim(ExtractFilePath(fAppFilename));

  // read the VMT once
  if Assigned(fOndebugEvent) then
    fFireDebugEvent := True;
  if fFireDebugEvent then
    fOndebugEvent(Self, 'UpdateToNewVersion',
      'Entering UpdateToNewVersion');


  // Running update using updatehm?
  if not AppIsRunning(ExtractFileName(fAppFilename)) then
    Result := DoSilentUpdate
  else
  begin
    cCount := 0;
    if not FileExistsUTF8(szAppDir + C_Updater) then
    begin
      if fShowDialogs then
        ShowMessageFmt(C_UpdaterMissing, [szAppDir + C_Updater]);
      if fFireDebugEvent then
        fOndebugEvent(Self, 'UpdateToNewVersion',
          Format(C_UpdaterMissing, [szAppDir + C_Updater]));
      Exit;
    end;
    if not DirectoryExistsUTF8(szAppDir + fUpdatesFolder) then
    begin
      if fShowDialogs then
        ShowMessageFmt(C_FolderMissing, [szAppDir + fUpdatesFolder]);
      if fFireDebugEvent then
        fOndebugEvent(Self, 'UpdateToNewVersion',
          Format(C_FolderMissing, [szAppDir + fUpdatesFolder]));
      Exit;
    end;


    // remotely shut down the app?
    if fSilentMode then
    begin
      If AppIsRunning(ExtractFileName(fAppFilename)) then KillApp(ExtractFileName(fAppFilename));
      if fFireDebugEvent then
        fOndebugEvent(Self, 'UpdateToNewVersion',
          Format('Killing %s ready for update', [fAppFilename]));
    end;

    // Update and re-start the app
    FUpdateHMProcess := TAsyncProcess.Create(nil);
    try
      FUpdateHMProcess.Executable := szAppDir + C_UPDATER;
      FUpdateHMProcess.CurrentDirectory := szAppDir;
      if not fSilentMode then
        FUpdateHMProcess.ConsoleTitle :=
          Format(C_ConsoleTitle, [fParentApplication.Title]);
      FUpdateHMProcess.Parameters.Clear;
      FUpdateHMProcess.Parameters.Add(ExtractFileName(fAppFilename)); //Param 1 = EXEname
      FUpdateHMProcess.Parameters.Add(fUpdatesFolder); // Param 2 = updates
      FUpdateHMProcess.Parameters.Add(C_WhatsNewFilename); // Param 3 = whatsnew.txt
      FUpdateHMProcess.Parameters.Add(fParentApplication.Title); // Param 4 = Prettyname
      if (fCopyTree = True) then
        FUpdateHMProcess.Parameters.Add('copytree');
      // Param 5 = Copy the whole of /updates to the App Folder
      if fFireDebugEvent then
        fOndebugEvent(Self, 'UpdateToNewVersion',
          Format('Executing %s', [szAppDir + C_UPDATER]));
      FUpdateHMProcess.Execute;

      // Check for C_WhatsNewFilename in the app directory in a LOOP
      if fFireDebugEvent then
        fOndebugEvent(Self, 'UpdateToNewVersion',
          Format('Waiting for %s', [szAppDir + C_WhatsNewFilename]));
      while not FileExistsUTF8(szAppDir + C_WhatsNewFilename) do
      begin
        fParentApplication.ProcessMessages;
        Inc(CCount);
        if cCount > 10000000 then
          Break; // Get out of jail in case updatehm.exe fails to copy file
      end;

    finally
      FUpdateHMProcess.Free;
      if not fSilentMode then
        fParentForm.Close;
    end;
    if fFireDebugEvent then
      fOndebugEvent(Self, 'UpdateToNewVersion',
        'Success');
    Result := True;
  end;
end;


procedure TLazAutoUpdate.ClearUpdateList;
begin
  Setlength(fUpdateList, 0);
end;

function TLazAutoUpdate.AddToUpdateList(APrettyName, APath, AVersionString: string;
  AVersionNumber: cardinal): integer;
var
  iLast: integer;
  TheRec: UpdateListRecord;
begin
  Setlength(fUpdateList, 0);
  iLast := High(fUpdateList);
  if (iLast = -1) then
    iLast := 0; // For when array is empty
  if (iLast = 1) then
    Exit; // TEMP: Only one entry allowed


  try
    Inc(iLast);
    Setlength(fUpdateList, iLast);
    with TheRec do
    begin
      PrettyName := APrettyName;
      Path := APath;
      VersionString := AVersionString;
      VersionNumber := AVersionNumber;
    end;
    fUpdateList[iLast - 1] := TheRec; // Remember array is zero-based
  finally
    Result := High(fUpdateList); // 0 = one element
    fUpdateListCount := Result + 1; // 1 = one element
  end;
  if (Result = 0) then
  begin
    fAppFilename := fUpdateList[iLast - 1].Path;
    fDownloadZipName := ChangeFileExt(fAppFilename, '.zip');
    fZipfileName := fDownloadZipName;
    fApplicationVersionString := fUpdateList[iLast - 1].VersionString;
  end;

end;

function TLazAutoUpdate.GetThreadDownloadReturnCode: integer;
begin
  Result := 0;
  if ThreadDownload.ThreadFinished then
    Result := fThreadDownload.fReturnCode;
end;

procedure TLazAutoUpdate.SetProjectType(AValue: TProjectType);
begin
  if (AValue <> fProjectType) then
    fProjectType := AValue;
  if fProjectType = auOther then
  begin
    fSourceForgeProjectName := C_NotApplicable;
  end
  else
  begin
    fUpdatesFolder := C_UpdatesFolder;
    fSourceForgeProjectName := '';
    fauOtherSourceFilename := C_NotApplicable;
    fauOtherSourceURL := C_NotApplicable;
  end;
end;

procedure TLazAutoUpdate.SetSourceForgeProjectName(Avalue: string);
begin
  fSourceForgeProjectName := LowerCase(AValue);
end;

procedure TLazAutoUpdate.SetAppFilename(Avalue: string);
begin
  fAppFilename := AValue;
  // Set a default value?
  if (fDownloadZipName = '') then
    fDownloadZipName := ChangeFileExt(ExtractFilename(fAppFilename), '.zip');
  fThreadDownload.Filename := fUpdatesFolder + PathDelim + fDownloadZipName;
end;

procedure TLazAutoUpdate.SetApplicationVersionString(Avalue: string);
begin
  if AValue = '' then
    Exit;
  fApplicationVersionString := AValue;
  fApplicationVersionNumber := VersionStringToNumber(fApplicationVersionString);
end;

// Threaded version
// ================
// Var bDownloadIsPresent:Boolean;
//     MyTheadDownload:TThreadedDownload;
// Begin
// MyTheadDownload:=TThreadedDownload.Create(sourceforgedownloadURL,Localfilepath);
// {
//  Note the Localfilepath MUST be specified, and can be a different filename and path
//  than the filename specified in the sourceforgedownloadURL
// }
// bDownloadIsPresent:=MyTheadDownload.ThreadDownloadHTTP;
// MyTheadDownload.UnzipAfter:=FALSE; // True *by default* if targetfile is a zip file
// If NOT bDownloadIsPresent then Exit; {BailOut ->}
// WHILE NOT MyThreadDownload.ThreadFinished do
// begin
//  {.. show the user it is downloading in the background}
//  Application.ProcessMessages; // <- Very Important; else the app will freeze!
// end;
// {File has now downloaded OK to Localfilepath (and is optionally unzipped)}
// MyTheadDownload.Free;
// End;


{ TDownloadThreadClass }

constructor TDownloadThreadClass.Create(URL, FileName: string);
begin
  inherited Create(True);
  fURL := URL;
  fFileName := FileName;
  fReturnCode := 0; // Failure code
  fDownloadSize := 0;
  FreeOnTerminate := True;
  fLastError := C_OK;
end;

procedure TDownloadThreadClass.Execute;
begin
  // Start the download procedure
  DownloadHTTP(fURL, fFileName, fReturnCode, fDownloadSize, fIsSourceForge, fDebugMode);
end;

//constructor TThreadedDownload.Create(URL, FileName: string);
constructor TThreadedDownload.Create();
begin
  inherited Create;
  fThreadFinished := False;
  fAppLicationVersionString := '0.0.1';
  fComponentVersion := C_TThreadedDownloadComponentVersion;
  fLastError := C_OK;
end;

{ TThreadedDownload }

function TThreadedDownload.ThreadDownloadHTTP: boolean;
var
  download: TDownloadThreadClass;
begin
  if (CompareFileExt(ExtractFilename(fFileName), 'zip', False) = 0) then
    fUnzipAfter := True
  else
    fUnzipAfter := False;

  download := TDownloadThreadClass.Create(fURL, fFileName);
  download.OnTerminate := @DownloadTerminiated;
  download.fIsSourceForge := fIsSourceForge;
  download.fDebugMode := fDebugMode;
  download.fLastError := fLastError;
  download.FreeOnTerminate := True;
  download.start;
  Result := True;
end;

procedure TThreadedDownload.DownloadTerminiated(Sender: TObject);
// Unzips all files ready for updatehm to copy them over
var
  UnZipper: TUnZipper;
begin
  fReturnCode := (Sender as TDownloadThreadClass).fReturnCode;
  fDownloadSize := (Sender as TDownloadThreadClass).fDownloadSize;
  fLastError := (Sender as TDownloadThreadClass).fLastError;
  fThreadFinished := True;
  if (FileExistsUTF8(fFileName) = True) and
    (CompareFileExt(fFileName, '.zip', False) = 0) then
    if fUnzipAfter then
    begin
      UnZipper := TUnZipper.Create;
      try
        UnZipper.FileName := fFileName;
        UnZipper.OutputPath := ExtractFileDir(fFileName);
        UnZipper.Examine;
        UnZipper.UnZipAllFiles;
        SysUtils.DeleteFile(fFileName);
      finally
        UnZipper.Free;
      end;
    end;
end;

{ End of class members}

function DownloadHTTPStream(URL: string; Buffer: TStream; fDebugMode: boolean): boolean;
  // Download file; retry if necessary.
const
  MaxRetries = 3;
var
  RetryAttempt: integer;
  HTTPGetResult: boolean;
begin
  Result := False;
  RetryAttempt := 1;
  HTTPGetResult := False;
  while ((HTTPGetResult = False) and (RetryAttempt < MaxRetries)) do
  begin
    HTTPGetResult := HttpGetBinary(URL, Buffer);
    //Application.ProcessMessages;
    WaitFor(100 * RetryAttempt);
    // Sleep(100 * RetryAttempt);
    RetryAttempt := RetryAttempt + 1;
  end;
  if HTTPGetResult = False then
    if fDebugmode then
      raise Exception.Create(C_CannotLoadFromRemote);
  Buffer.Position := 0;
  if Buffer.Size = 0 then
    if fDebugmode then
      raise Exception.Create(C_DownloadIsEmpty)
    else
      Result := True;
end;

function SFDirectLinkURL(URL: string; Document: TMemoryStream): string;
{
Transform this part of the body:
<noscript>
<meta http-equiv="refresh" content="5; url=http://downloads.sourceforge.net/project/base64decoder/base64decoder/version%202.0/b64util.zip?r=&amp;ts=1329648745&amp;use_mirror=kent">
</noscript>
into a valid URL:
http://downloads.sourceforge.net/project/base64decoder/base64decoder/version%202.0/b64util.zip?r=&amp;ts=1329648745&amp;use_mirror=kent
}
const
  Refresh = '<meta http-equiv="refresh"';
  URLMarker = 'url=';
var
  Counter  : integer;
  HTMLBody  : TStringList;
  RefreshStart  : integer;
  URLStart  : integer;
begin
  HTMLBody := TStringList.Create;
   try
    HTMLBody.LoadFromStream(Document);
    for Counter := 0 to HTMLBody.Count - 1 do
     begin
      // This line should be between noscript tags and give the direct download locations:
      RefreshStart := Ansipos(Refresh, HTMLBody[Counter]);
      if RefreshStart > 0 then
       begin
        URLStart := AnsiPos(URLMarker, HTMLBody[Counter]) + Length(URLMarker);
        if URLStart > RefreshStart then
         begin
          // Look for closing "
          URL := Copy(HTMLBody[Counter], URLStart,
            PosEx('"', HTMLBody[Counter], URLStart + 1) - URLStart);
          //infoln('debug: new url after sf noscript:');
          //infoln(URL);
          break;
         end;
       end;
     end;
   finally
    HTMLBody.Free;
   end;
  Result := URL;
end;

function SourceForgeURL(URL: string; fDebugmode: boolean;
var AReturnCode: integer): string;
  // Detects sourceforge download and tries to deal with
  // redirection, and extracting direct download link.
  // Thanks to
  // Ocye: http://lazarus.freepascal.org/index.php/topic,13425.msg70575.html#msg70575
const
  SFProjectPart = '//downloads.sourceforge.net/project/';
  SFFilesPart = '/files/';
  SFDownloadPart = '/download';
var
  HTTPSender  : THTTPSend;
  i, j  : integer;
  FoundCorrectURL  : boolean;
  SFDirectory  : string; //Sourceforge directory
  SFDirectoryBegin  : integer;
  SFFileBegin  : integer;
  SFFilename  : string; //Sourceforge name of file
  SFProject  : string;
  SFProjectBegin  : integer;
begin
  // Detect SourceForge download; e.g. from URL
  //          1         2         3         4         5         6         7         8         9
  // 1234557890123456789012345578901234567890123455789012345678901234557890123456789012345578901234567890
  // http://sourceforge.net/projects/base64decoder/files/base64decoder/version%202.0/b64util.zip/download
  //                                 ^^^project^^^       ^^^directory............^^^ ^^^file^^^
  FoundCorrectURL := False; //Assume not a SF download
  i := Pos(SFProjectPart, URL);
  if i > 0 then
   begin
    // Possibly found project; now extract project, directory and filename parts.
    SFProjectBegin := i + Length(SFProjectPart);
    j := PosEx(SFFilesPart, URL, SFProjectBegin);
    if (j > 0) then
     begin
      SFProject := Copy(URL, SFProjectBegin, j - SFProjectBegin);
      SFDirectoryBegin := PosEx(SFFilesPart, URL, SFProjectBegin) + Length(SFFilesPart);
      if SFDirectoryBegin > 0 then
       begin
        // Find file
        // URL might have trailing arguments... so: search for first
        // /download coming up from the right, but it should be after
        // /files/
        i := RPos(SFDownloadPart, URL);
        // Now look for previous / so we can make out the file
        // This might perhaps be the trailing / in /files/
        SFFileBegin := RPosEx('/', URL, i - 1) + 1;

        if SFFileBegin > 0 then
         begin
          SFFilename      := Copy(URL, SFFileBegin, i - SFFileBegin);
          //Include trailing /
          SFDirectory     := Copy(URL, SFDirectoryBegin, SFFileBegin - SFDirectoryBegin);
          FoundCorrectURL := False;
         end;
       end;
     end;
   end;

  if not FoundCorrectURL then
   begin
     try
      // Rewrite URL if needed for Sourceforge download redirection
      // Detect direct link in HTML body and get URL from that
      HTTPSender := THTTPSend.Create;
      //Who knows, this might help:
      HTTPSender.UserAgent :=
        'curl/7.21.0 (i686-pc-linux-gnu) libcurl/7.21.0 OpenSSL/0.9.8o zlib/1.2.3.4 libidn/1.18';
      while not FoundCorrectURL do
       begin
        HTTPSender.HTTPMethod('GET', URL);
        // SEE: http_ReturnCodes.txt
        case HTTPSender.Resultcode of
          301, 302, 307: // Redirect
           begin
            for i := 0 to HTTPSender.Headers.Count - 1 do
              if (Pos('Location: ', HTTPSender.Headers.Strings[i]) > 0) or
                (Pos('location: ', HTTPSender.Headers.Strings[i]) > 0) then
               begin
                j := Pos('use_mirror=', HTTPSender.Headers.Strings[i]);
                if j > 0 then
                  URL :=
                    'http://' + RightStr(HTTPSender.Headers.Strings[i],
                    length(HTTPSender.Headers.Strings[i]) - j - 10) +
                    '.downloads.sourceforge.net/project/' + SFProject +
                    '/' + SFDirectory + SFFilename
                else
                  URL := StringReplace(HTTPSender.Headers.Strings[i],
                    'Location: ', '', []);
                HTTPSender.Clear;//httpsend
                FoundCorrectURL := True;
                AReturnCode     := HTTPSender.Resultcode;
                break; //out of rewriting loop
               end;
           end;
          100..200:
           begin
            //Could be a sourceforge timer/direct link page, but...
            if AnsiPos('Content-Type: text/html', HTTPSender.Headers.Text) > 0 then
             begin
              // find out... it's at least not a binary
              URL := SFDirectLinkURL(URL, HTTPSender.Document);
             end;
            FoundCorrectURL := True; //We're done by now
            AReturnCode     := HTTPSender.Resultcode;
           end;
          500:
           begin
            // if fDebugMode then ShowMessageFmt(C_Error500, [HTTPSender.ResultCode]);
            AReturnCode := HTTPSender.Resultcode;
            Break;
           end;
          //Raise Exception.Create('No internet connection available');
          //Internal Server Error ('+aURL+')');
          404:
           begin
            // if fDebugMode then ShowMessageFmt(C_Error404, [HTTPSender.ResultCode]);
            AReturnCode := HTTPSender.Resultcode;
            Break;
           end;
          else
            raise Exception.Create(C_DownloadFailedErrorCode +
              IntToStr(HTTPSender.ResultCode) + ' (' + HTTPSender.ResultString + ')');
         end;//case
       end;//while
     finally
      AReturnCode := HTTPSender.Resultcode;
      HTTPSender.Free;
     end;
   end;
  Result := URL;
end;

function DownloadHTTP(URL, TargetFile: string; var ReturnCode, DownloadSize: integer;
bIsSourceForge, fDebugmode: boolean): boolean;
  // Download file; retry if necessary.
  // Deals with SourceForge download links
  // Could use Synapse HttpGetBinary, but that doesn't deal
  // with result codes (i.e. it happily downloads a 404 error document)
const
  MaxRetries = 3;
var
  HTTPGetResult  : boolean;
  HTTPSender  : THTTPSend;
  RetryAttempt  : integer;
begin
  Result := False;
  RetryAttempt := 1;
  //Optional: mangling of Sourceforge file download URLs; see below.
  if bIsSourceForge then
    URL      := SourceForgeURL(URL, fDebugMode, ReturnCode); //Deal with sourceforge URLs
  // ReturnCode may not be useful, but it's provided here
  HTTPSender := THTTPSend.Create;
   try
     try
      // Try to get the file
      HTTPGetResult := HTTPSender.HTTPMethod('GET', URL);
      while (HTTPGetResult = False) and (RetryAttempt < MaxRetries) do
       begin
        WaitFor(500 * RetryAttempt);
        // sleep(500 * RetryAttempt);
        HTTPGetResult := HTTPSender.HTTPMethod('GET', URL);
        RetryAttempt  := RetryAttempt + 1;
       end;
      // If we have an answer from the server, check if the file
      // was sent to us
      ReturnCode   := HTTPSender.Resultcode;
      DownloadSize := HTTPSender.DownloadSize;
      case HTTPSender.Resultcode of
        100..299:
         begin
          with TFileStream.Create(TargetFile, fmCreate or fmOpenWrite) do
             try
              Seek(0, soFromBeginning);
              CopyFrom(HTTPSender.Document, 0);
             finally
              Free;
             end;
          Result := True;
         end; //informational, success
        300..399: Result := False; //redirection. Not implemented, but could be.
        400..499: Result := False; //client error; 404 not found etc
        500..599: Result := False; //internal server error
        else
          Result := False; //unknown code
       end;
     except
      // We don't care for the reason for this error; the download failed.
      Result := False;
     end;
   finally
    HTTPSender.Free;
   end;

end;


end.
