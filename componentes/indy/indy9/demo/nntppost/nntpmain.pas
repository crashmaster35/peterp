unit nntpmain;
// attempt at making a NNTP posting client.
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,idnntp,
  StdCtrls,idmessage,IdEMailAddress;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    nn : tidnntp;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);


begin
   nn:=tidnntp.create(self);
   nn.host:='news.atozedsoftware.com';
   nn.Connect();

end;

procedure TForm1.Button1Click(Sender: TObject);
var newslist:TStringList;
    i,j :integer;
    msg : tidmessage;
    fr  :TIdEMailAddressItem;

begin
    NewsList := TStringList.Create;
  { nn.GetNewsgroupList(NewsList);
   j:=newslist.count;

   memo1.lines.add(inttostr(j));
   if j>30 then j:=30;
   if j>0 then
   for i:=0 to j-1 do
      memo1.lines.add(newslist[i]); }
//   nn.SelectGroup('stack.test' );
   if crcanpost = nn.permission then
      memo1.lines.add('posting ok');
   msg:=tidmessage.create(nil);
   msg.body.add('Hello,');
   msg.body.add(' ');
   msg.body.add('This post is made using the current Indy 9 snapshot and a current');
   msg.body.add('Free Pascal snapshot. Lazarus was also used, but only as basic msg pump.');
   msg.body.add(' ');
   msg.body.add('All necessary mods were migrated back into FPC, except for FPC''s');
   msg.body.add('entry in idcompilerdefines.inc. Some additional changes to indy would ');
   msg.body.add('be nice though.');
   msg.body.add(' ');
   msg.body.add('Other working demoes till now are Elizaweb and the basic client/server ');
   msg.body.add(' combo.');
   msg.body.add(' ');
   msg.body.add('Designtime support not yet ready, but expected soon.');
   msg.body.add(' ');
   msg.body.add('Greetings');
   msg.body.add('Marco van de Voort (on behalf of the FPC Core Team and all contributors.');

   msg.subject:=('Indy9 + FPC 2.1.x = working');
   msg.newsgroups.add('atozedsoftware.indy.announcements');
   msg.from.name:='Marco van de Voort';
   msg.from.address:='Marco@freepascal.org';
   memo1.lines.add('voor posting');
   // commented to avoid mass posting to the example NG
   // nn.post(msg);
   memo1.lines.add('na posting' );
end;

initialization
  {$I nntpmain.lrs}

end.

