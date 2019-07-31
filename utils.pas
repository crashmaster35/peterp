unit utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, crt, dialogs;

type
  TSession = record
    user : record
      id : integer;
      nombre : string[100];
      Config : byte;
      AbrirCaja : byte;
      CerrarCaja : byte;
      CorteCaja : byte;
      Supervisor : byte;
      Clients : byte;
    end;
    db :record
      host : string[50];
      user : string[50];
      pwd : string[50];
    end;
  end;


const
  Salt : AnsiString = 'a2c901c8c6dea98958c219f6f2d038c44dc5d362';
var
  session : TSession;
  FSession : file of TSession;

// Lee el archivo de configuración para conectarse a la db.
function loadConfig : TSession;
procedure saveConfig(session : TSession);
procedure OpenDrawer();
procedure CutPaper();


implementation

function loadConfig : TSession;
begin
  try
    AssignFile(FSession, 'config.tpv');

    {$I-}
    Reset(FSession);
    Read(FSession,session);
    {$I+}

    CloseFile(FSession);
  except
    on E:EInOutError do
      begin
           Rewrite(FSession);
      end;
  end;

  result := session;

end;

procedure saveConfig(session : TSession);
begin
  try
    AssignFile(FSession, 'config.tpv');

    Rewrite(FSession);
    {$I-}
    Write(FSession,session);
    {$I+}

    CloseFile(FSession);
    except
    on E:EInOutError do
      begin
        ShowMessage('Exsite un problema en la escritura en el disco, por favor revise que su disco duro no se encuentre lleno.');
        exit;
      end;
  end;

end;

// Procedimiento que abre el cajon de dinero (drawer)
procedure OpenDrawer();
var
  F : TextFile;
begin
  try
  AssignFile(F, 'LPT1'); // Agregar al configurador el que pueda colocar un puerto
  {$I-}
  Rewrite(F);
  {$I+}

  Writeln(F, #27#112#0#105#105); // Abre el cajor del drawer

  CloseFile(F);
  except
    on E:EInOutError do
      ShowMessage('La caja de dinero no puede inicializarse. Por favor revise que su impresora de tickets esté encendida y la caja de dinero conectada a ella. También verifique que su impresora se encuentre en el puerto LPT1');
  end;
end;

// Procedimiento que corta el papel de la impresora.
procedure CutPaper();
var
  F : TextFile;
begin
  try
     AssignFile(F, 'LPT1'); // Agregar al configurador el que pueda colocar un puerto
     {$I-}
     Rewrite(F);
     {$I+}

     Writeln(F, #27#105); // Corta el papel de la impresora

     CloseFile(F);
  except
    on E:EInOutError do
      ShowMessage('La impresora no puede inicializarse. Por favor revise que su impresora de tickets esté encendida. También verifique que su impresora se encuentre en el puerto LPT1');
  end;
end;

end.

