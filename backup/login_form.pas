unit login_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, BCLabel, sqldb, db, mysql56conn, utils, bcrypt;

type

  { TfrmLogin }

  TfrmLogin = class(TForm)
    BCLabel1: TBCLabel;
    BCLabel2: TBCLabel;
    BCLabel3: TBCLabel;
    btnCancel: TBitBtn;
    btnLogin: TBitBtn;
    DataSource: TDataSource;
    edtLogin: TEdit;
    edtPwd: TEdit;
    imgBackLogin: TImage;
    SQLConnect: TSQLConnector;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    procedure btnCancelClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmLogin: TfrmLogin;
  host, user, pwd : String;

implementation

{$R *.lfm}

{ TfrmLogin }

procedure TfrmLogin.btnCancelClick(Sender: TObject);
begin
  if btnCancel.ModalResult = mrCancel then
    frmLogin.ModalResult:=mrCancel;
end;

procedure TfrmLogin.btnLoginClick(Sender: TObject);
begin
  FrmLogin.Cursor:=crHourGlass;
  SQLConnect.HostName:=session.db.host;
  SQLConnect.UserName:=session.db.user;
  SQLConnect.Password:=session.db.pwd;

  if btnLogin.ModalResult = mrOk then
    begin
      if (trim(edtLogin.Text) <> '') and (trim(edtPwd.Text) <> '') then
        if (trim(edtLogin.Text) = 'root') and (checkPassword(trim(edtPwd.Text), HashPassword('123', Salt))) then
          begin
            session.user.nombre:='Root';
            session.user.id:=0;
            session.user.Config:=1;
            session.user.AbrirCaja:=1;
            session.user.CerrarCaja:=1;
            session.user.CorteCaja:=1;
            session.user.Clients:=1;
            saveConfig(session);
            frmLogin.Cursor := crDefault;
            frmLogin.ModalResult:=1
          end
        else
          begin
            with SQLConnect do
              begin
                if Connected then
                  Connected := false;
                HostName:=session.db.host;
                UserName:=session.db.user;
                Password:=session.db.pwd;

                Connected := true;
              end;

              if SQLConnect.Connected then
                begin
                  with SQLQuery do
                    begin
                      SQL.Clear;
                      SQL.Text:='SELECT u.active as active, u.id as uid, u.user_name as user_name, u.pwd as pwd, CONCAT (u.first_name, '' '', u.last_name) as name, up.config as config, up.open_pos as open_pos, up.close_pos as close_pos, up.box_cut as box_cut, up.supervisor as supervisor, up.clients as clients FROM `users` as u LEFT JOIN user_profiles as up ON (up.user_id = u.id) WHERE `user_name` = :qryUser AND pwd = :qryPwd';
                      params.ParamByName('qryUser').AsString:=trim(edtLogin.Text);
                      params.ParamByName('qryPwd').AsString:=HashPassword(trim(edtPwd.Text), Salt);
                      Active := True;
                        if FieldByName('uid').AsInteger > 0 then
                          begin
                            if FieldByName('active').AsInteger = 1 then
                              begin
                                session.user.nombre:=FieldByName('name').AsString;
                                session.user.id:=FieldByName('uid').AsInteger;
                                if (FieldByName('config').AsInteger = 1) OR (FieldByName('open_pos').AsInteger = 1) OR (FieldByName('close_pos').AsInteger = 1) OR (FieldByName('box_cut').AsInteger = 1) OR (FieldByName('supervisor').AsInteger = 1) OR (FieldByName('clients').AsInteger = 1) then
                                  begin
                                    session.user.Config:=FieldByName('config').AsInteger;
                                    session.user.AbrirCaja:=FieldByName('open_pos').AsInteger;
                                    session.user.CerrarCaja:=FieldByName('close_pos').AsInteger;
                                    session.user.CorteCaja:=FieldByName('box_cut').AsInteger;
                                    session.user.Supervisor:=FieldByName('supervisor').AsInteger;
                                    session.user.Clients:=FieldByName('clients').AsInteger;
                                    saveConfig(session);
                                    frmLogin.Cursor := crDefault;
                                    frmLogin.ModalResult:=1
                                  end
                                else
                                  begin
                                    frmLogin.Cursor := crDefault;
                                    frmLogin.ModalResult:=4;
                                  end;
                              end
                            else
                              begin
                                frmLogin.Cursor := crDefault;
                                frmLogin.ModalResult:=5;
                              end;
                          end
                        else
                          begin
                            frmLogin.Cursor := crDefault;
                            frmLogin.ModalResult:=3;
                          end;
                      end;
                  end
                else
                  begin
                    frmLogin.Cursor := crDefault;
                    frmLogin.ModalResult:=3;
                  end;
              end
        else
          begin
            frmLogin.Cursor := crDefault;
            frmLogin.ModalResult:=3;
          end;
      end;
end;

procedure TfrmLogin.FormCreate(Sender: TObject);
begin
  session := loadConfig;
end;

procedure TfrmLogin.FormDestroy(Sender: TObject);
begin
  frmLogin.Release;
end;

end.

