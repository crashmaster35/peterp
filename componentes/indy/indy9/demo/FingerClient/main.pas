{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  110696: main.pas 
{
{   Rev 1.0    25/10/2004 23:31:18  ANeillans    Version: 9.0.17
{ Verified
}
{-----------------------------------------------------------------------------
 Demo Name: main
 Author:    <unknown - please contact me to take credit! - Allen O'Neill>
 Copyright: Indy Pit Crew
 Purpose:
 History:
-----------------------------------------------------------------------------
 Notes:

 Demonstrates basic finger client functionality

Verified:
  Indy 9:
    D7: 25th Oct 2004 Andy Neillans
}


unit main;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons,
  SysUtils, Classes, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdFinger, LResources,Interfaces;

type

  { TfrmFingerDemo }

  TfrmFingerDemo = class(TForm)

    edtQuerry: TEdit;
    lblQuerry: TLabel;
    mmoQuerryResults: TMemo;
    lblInstructions: TLabel;
    chkVerboseQuerry: TCheckBox;
    bbtnQuerry: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure bbtnQuerryClick(Sender: TObject);
  private
     IdFngFinger: TIdFinger;
  public
  end;

var
  frmFingerDemo: TfrmFingerDemo;

implementation

procedure TfrmFingerDemo.bbtnQuerryClick(Sender: TObject);
begin
  {Set the Query string for the Finger from the text entered}
  IdFngFinger.CompleteQuery := edtQuerry.Text;
  {Do we want verbose query - not supported on many systems}
  IdFngFinger.VerboseOutput := chkVerboseQuerry.Checked;
  {Do our query with the Finger function}
  mmoQuerryResults.Lines.Text := IdFngFinger.Finger;
end;

procedure TfrmFingerDemo.FormCreate(Sender: TObject);
begin
  IdFngFinger:=TIdFinger.create(self);
end;

initialization
  {$i main.lrs}
end.
