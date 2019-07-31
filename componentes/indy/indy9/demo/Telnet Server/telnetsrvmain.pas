{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  23301: telnetsrvmain.pas 
{
{   Rev 1.1    25/10/2004 22:50:28  ANeillans    Version: 9.0.17
{ Verified
}
{
{   Rev 1.0    12/09/2003 22:34:46  ANeillans
{ Initial Checking
{ Verified with Indy 9 and D7
}
{
  Demo Name:  Telnet Server
  Created By: Siamak Sarmady
          On: 27/10/2002

  Notes:
    Telnet Server Demo.
    A simple Indy Telnet Server Demo
    Test using telnet on port 23


  Version History:
    12th Sept 03: Andy Neillans
                  Added the instruction memo

  Tested:
   Indy 9:
     D5:     Untested
     D6:     Untested
     D7:     25th Oct 2004 by Andy Neillans
}

unit telnetsrvmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, IdBaseComponent, IdComponent, IdTCPServer, IdTelnetServer,
  IdAntiFreezeBase, IdAntiFreeze,lresources,buttons,interfaces;

type

  { TMainForm }

  TMainForm = class(TForm)

    buttonExit: TButton;
    Memo1: TMemo;
    moComments: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure buttonExitClick(Sender: TObject);
  private
      IdTelnetServer1: TIdTelnetServer;
    procedure IdTelnetServer1Authentication(AThread: TIdPeerThread;
      const AUsername, APassword: String; var AAuthenticated: Boolean);
    procedure IdTelnetServer1Execute(AThread: TIdPeerThread);

    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation


procedure TMainForm.IdTelnetServer1Authentication(AThread: TIdPeerThread;
  const AUsername, APassword: String; var AAuthenticated: Boolean);
begin
memo1.Lines.add(' connection! '+ausername+' ' +apassword);
AAuthenticated:=True;
AThread.Connection.Write(AUsername);
AThread.Connection.WriteLn(', Welcome to Indy Telnet Server.');
AThread.Connection.WriteLn('You have new mail.');
AThread.Connection.WriteLn('');
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
      IdTelnetServer1:=TIdTelnetServer.create(self);
      IdTelnetServer1.OnAuthentication:=IdTelnetServer1Authentication;
      IdTelnetServer1.OnExecute:=IdTelnetServer1Execute;
      idtelnetserver1.active:=true;
end;

procedure TMainForm.IdTelnetServer1Execute(AThread: TIdPeerThread);
var
  str : string;
begin
 memo1.lines.add('x');
 with AThread.Connection do
  begin
   Write('shell>');
   str:=InputLn('');
   WriteLn(str);
   if (str='exit') or (str='logout') then
      Disconnect;   
  end;
end;

procedure TMainForm.buttonExitClick(Sender: TObject);
begin
 if IdTelnetServer1.Active=true then
 begin
   IdTelnetServer1.Active:=false
 end;
 Application.Terminate;
end;

initialization
  {$i telnetsrvmain.lrs}
end.
