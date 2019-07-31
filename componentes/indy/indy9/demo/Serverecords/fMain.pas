{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  110882: fMain.pas 
{
{   Rev 1.0    26/10/2004 13:05:10  ANeillans    Version: 9.0.17
{ Verified
}
unit fMain;

{$MODE Delphi}

{-----------------------------------------------------------------------------
 Demo Name: Records - Server
 Author:    Allen O'Neill
 Copyright: Indy Pit Crew
 Purpose:
 History:
-----------------------------------------------------------------------------
 Notes:

 Demonstrates sending / receiving record data and use of buffers
 Note - sending record data etc can also be done using Streams - this is just another method.

Verified:
  Indy 9:
    D7: 26th Oct 2004 Andy Neillans
}


interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, LResources, Buttons,Interfaces,

  IdBaseComponent, IdComponent, IdTCPServer,IdSocketHandle;

type
  Direction = (dirLeft,dirRight);

type
  MyRecord = Packed Record
  MyInteger : Integer;
  MyString : String[250];
  MyBool : Boolean;
  MyDirection : Direction;
  end;


type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnStart: TButton;
    btnExit: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
  private
    IdTCPServer: TIdTCPServer;
    procedure IdTCPServerExecute(AThread: TIdPeerThread);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation


procedure TfrmMain.btnExitClick(Sender: TObject);
begin
if IdTCPServer.active then IdTCPServer.active := false;
application.terminate;
end;

procedure TfrmMain.FormCreate(Sender: TObject);

var b : TidSockethandle;

begin
    IdTCPServer:=TIdTCPServer.create(Self);
    b:=IdTCPServer.Bindings.add;
    b.IP:=' 127.0.0.1';
    b.port:=9099;
    IdTCPServer.OnExecute:= IdTCPServerExecute;
    IdTCPServer.Active:=true;
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
IdTCPServer.active := not IdTCPServer.active;

if btnStart.caption = 'Start' then btnStart.caption := 'Stop'
else if btnStart.caption = 'Stop' then btnStart.caption := 'Start';

end;

procedure TfrmMain.IdTCPServerExecute(AThread: TIdPeerThread);
var
  MyRec : MyRecord;
begin
AThread.connection.ReadBuffer(MyRec,SizeOf(MyRec));
AThread.connection.WriteBuffer(MyRec,SizeOf(MyRec),true);
end;

initialization
  {$i fmain.lrs}

end.
