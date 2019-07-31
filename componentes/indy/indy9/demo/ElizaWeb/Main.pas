{$mode delphi}
{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  21156: Main.pas 
{
{   Rev 1.0    2003.06.30 1:28:38 PM  czhower
{ Initial Checkin
}
{
{   Rev 1.0    2003.05.19 2:59:42 PM  czhower
}
unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IdBaseComponent, IdComponent, IdTCPServer, IdCustomHTTPServer,
  IdHTTPServer, ExtCtrls, StdCtrls,Buttons,ezengine,LResources,Interfaces;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Button1: TButton;
    procedure IdHTTPServer1CommandGet(AThread: TIdPeerThread;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure FormCreate(Sender: TObject);
    procedure IdHTTPServer1SessionStart(Sender: TIdHTTPSession);
    procedure IdHTTPServer1SessionEnd(Sender: TIdHTTPSession);
    procedure Button1Click(Sender: TObject);
  private
  protected
    FHTMLDir: string;
    FTemplate: string;
    IdHTTPServer1: TIdHTTPServer;
    //
    procedure Ask(ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  public
  end;

var
  Form1: TForm1;

implementation

uses
  ShellAPI;

procedure TForm1.FormCreate(Sender: TObject);
begin
  IdHTTPServer1:=TIdHTTPServer.create(form1);
  IdHTTPServer1.oncommandget:=idhttpserver1commandget;
  IdHTTPServer1.OnSessionStart := IdHTTPServer1SessionStart;
  IdHTTPServer1.OnSessionEnd := IdHTTPServer1SessionEnd;
  IdHTTPServer1.autostartsession:=true;
  IdHTTPServer1.sessionstate:=true;
  IdHTTPServer1.sessionTimeOut:=600000;

  idhttpserver1.active:=true;
  FHTMLDir := ExtractFilePath(Application.ExeName) + 'HTML';
  with TFileStream.Create(FHTMLDir + '\eliza.html', fmOpenRead) do try
    SetLength(FTemplate, Size);
    ReadBuffer(FTemplate[1], Size);
  finally Free; end;
end;

procedure TForm1.IdHTTPServer1CommandGet(AThread: TIdPeerThread;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LFilename: string;
  LPathname: string;
begin
  LFilename := ARequestInfo.Document;
  if AnsiSameText(LFilename, '/eliza.html') then begin
    Ask(ARequestInfo, AResponseInfo);
  end else begin
    if LFilename = '/' then begin
      LFilename := '/index.html';
    end;
    LPathname := FHTMLDir + LFilename;
    if FileExists(LPathname) then begin
      AResponseInfo.ContentStream := TFileStream.Create(LPathname, fmOpenRead + fmShareDenyWrite);
    end else begin
      AResponseInfo.ResponseNo := 404;
      AResponseInfo.ContentText := 'The requested URL ' + ARequestInfo.Document
       + ' was not found on this server.';
    end;
  end;
end;

procedure TForm1.IdHTTPServer1SessionStart(Sender: TIdHTTPSession);
begin
  Sender.Content.AddObject('Eliza', TEzEngine.Create(nil));
  
end;

procedure TForm1.Ask(ARequestInfo: TIdHTTPRequestInfo;
 AResponseInfo: TIdHTTPResponseInfo);
var
  s: string;
  LEliza: TEZEngine;
  LPersonality: string;
  LResponse: string;
  LSound: string;
  LQuestion: string;
begin
  LResponse := '';
  LEliza := TEzEngine(ARequestInfo.Session.Content.Objects[0]);
  LPersonality := Trim(ARequestInfo.Params.Values['Personality']);
  if LPersonality <> '' then begin
    LEliza.SetPersonality(LPersonality);
  end else begin
    LQuestion := Trim(ARequestInfo.Params.Values['Thought']);
    if LQuestion <> '' then begin
      LResponse := LEliza.TalkTo(LQuestion, LSound);
    end;
  end;
  if LEliza.Done then begin
    AResponseInfo.ContentText := LResponse;
  end else begin
    s := FTemplate;
    s := StringReplace(s, '{%RESPONSE%}', LResponse, []);
    if LSound <> '' then begin
      LSound := '<BGSOUND SRC=' + LSound + '.wav>';
    end;
    s := StringReplace(s, '{%SOUND%}', LSound, []);
    AResponseInfo.ContentText := s
  end;
end;

procedure TForm1.IdHTTPServer1SessionEnd(Sender: TIdHTTPSession);
begin
  Tezengine(Sender.Content.Objects[0]).Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShellAPI.ShellExecute(Handle, PChar('open'), 'http://127.0.0.1/', nil, nil, 0);
end;

initialization
{$i main.lrs}

end.
