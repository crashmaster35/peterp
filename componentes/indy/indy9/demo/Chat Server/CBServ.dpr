{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  110574: CBServ.dpr 
{
{   Rev 1.0    25/10/2004 23:04:20  ANeillans    Version: 9.0.17
{ Verified
}
{
{   Rev 1.0    25/10/2004 23:03:32  ANeillans    Version: 9.0.17
{ Verified
}
// NOTE: This demo ONLY runs under Windows.

program CBServ;

uses

  Forms,
  MainForm in 'MainForm.pas' {frmMain};

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
