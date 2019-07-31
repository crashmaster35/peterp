{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  110733: Main.pas 
{
{   Rev 1.0    25/10/2004 23:43:40  ANeillans    Version: 9.0.17
{ Verified
}
{-----------------------------------------------------------------------------
 Demo Name: Main
 Author:    <unknown - please contact me to claim credit! - Allen O'Neill>
 Copyright: Indy Pit Crew
 Purpose:
 History:
-----------------------------------------------------------------------------
 Notes:


 Demonstrates a DateTime client getting current date and time from remote DateTimeServer

// A list of time servers is available at:
// http://www.eecis.udel.edu/~mills/ntp/servers.html

Verified:
  Indy 9:
    D7: 25th Oct 2004 Andy Neillans
}


unit Main;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls,
  SysUtils, Classes, IdComponent, IdTCPConnection, IdTCPClient, IdTime,
  IdBaseComponent, LResources,Buttons,Interfaces;

type

  { TfrmTimeDemo }

  TfrmTimeDemo = class(TForm)
    lblTimeServer: TLabel;
    edtTimeResult: TEdit;
    Label1: TLabel;
    btnGetTime: TButton;
    cmboTimeServer: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure btnGetTimeClick(Sender: TObject);
  private
    IdDemoTime: TIdTime;
  public
  end;

var
  frmTimeDemo: TfrmTimeDemo;

implementation

// No real code required - all functionality built into component !
procedure TfrmTimeDemo.btnGetTimeClick(Sender: TObject);
begin
  IdDemoTime.Host := cmboTimeServer.Text;
  { After setting Host, this is all you have to get the time from a time
  server.  We do the rest. }
  edtTimeResult.Text := DateTimeToStr ( IdDemoTime.DateTime );
end;

procedure TfrmTimeDemo.FormCreate(Sender: TObject);
begin
   IdDemoTime:=TIdTime.create(self);
end;

initialization
  {$i Main.lrs}
end.
