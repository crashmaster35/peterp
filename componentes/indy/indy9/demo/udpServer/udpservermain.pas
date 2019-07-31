{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  110720: UDPServerMain.pas 
{
{   Rev 1.0    25/10/2004 23:31:26  ANeillans    Version: 9.0.17
{ Verified
}
{-----------------------------------------------------------------------------
 Demo Name: UDP Server
 Author:    <unknown - please contact me to take credit! - Allen O'Neill>
 Copyright: Indy Pit Crew
 Purpose:
 History:
-----------------------------------------------------------------------------
 Notes:

 Simple UDP server demo

Verified:
  Indy 9:
    D7: 25th Oct 2004 Andy Neillans

}

unit UDPServerMain;

interface

uses
  Windows, Messages, Graphics, Controls, Forms, Dialogs, IdWinsock2, stdctrls,
  SysUtils, Classes, IdBaseComponent, IdAntiFreezeBase, IdAntiFreeze,
  IdComponent, IdUDPBase, IdUDPClient, IdStack, IdUDPServer, IdSocketHandle,interfaces,
  lresources,buttons;


type
  TUDPMainForm = class(TForm)
    SourceGroupBox: TGroupBox;
    HostNameLabel: TLabel;
    HostAddressLabel: TLabel;
    HostName: TLabel;
    HostAddress: TLabel;
    PortLabel: TLabel;
    Port: TLabel;
    BufferSizeLabel: TLabel;
    BufferSize: TLabel;
    UDPMemo: TMemo;
    procedure FormCreate(Sender: TObject);

  private
    { Private declarations }
    UDPServer: TIdUDPServer;
    UDPAntiFreeze: TIdAntiFreeze;
    procedure UDPServerUDPRead(Sender: TObject; AData: TStream; ABinding: TIdSocketHandle);
  public
    { Public declarations }
  end;

var
  UDPMainForm: TUDPMainForm;

implementation

const
  HOSTNAMELENGTH = 80;


procedure TUDPMainForm.FormCreate(Sender: TObject);
begin
  udpserver:=tidudpserver.create(self);
  udpserver.OnUDPRead:=udpserverudpread;
  udpserver.DefaultPort:=8090;
  udpantifreeze:=tidantifreeze.create(self);
  HostName.Caption := UDPServer.LocalName;
  HostAddress.Caption := GStack.LocalAddress;
  Port.Caption := IntToStr(UDPServer.DefaultPort);
  BufferSize.Caption := IntToStr(UDPServer.BufferSize);
  UDPServer.Active := True;
end;

procedure TUDPMainForm.UDPServerUDPRead(Sender: TObject; AData: TStream; ABinding: TIdSocketHandle);
var
  DataStringStream: TStringStream;
  s: String;
begin
  DataStringStream := TStringStream.Create('');
  try
    DataStringStream.CopyFrom(AData, AData.Size);
    UDPMemo.Lines.Add('Received "' + DataStringStream.DataString + '" from ' + ABinding.PeerIP + ' on port ' + IntToStr(ABinding.PeerPort));
    s := 'Replied from ' + UDPServer.LocalName + ' to "' + DataStringStream.DataString + '"';
    ABinding.SendTo(ABinding.PeerIP, ABinding.PeerPort, s[1], Length(s));
  finally
    DataStringStream.Free;
  end;
end;

initialization
  {$i udpservermain.lrs}
end.
