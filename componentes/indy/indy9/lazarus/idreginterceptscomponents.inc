{*********************************
 *  Indy Intercepts components   *
 *********************************}

procedure RegisterUnit_IdBlockCipherIntercept;
begin//
  RegisterComponents(RSRegIndyIntercepts, [TIdBlockCipherIntercept]);
end;

procedure RegisterUnit_IdConnectionIntercept;
begin//
  RegisterComponents(RSRegIndyIntercepts, [TIdConnectionIntercept]);
end;

procedure RegisterUnit_IdCompressionIntercept;
begin//
  RegisterComponents(RSRegIndyIntercepts, [TIdCompressionIntercept]);
end;

procedure RegisterUnit_IdLogDebug;
begin//
  RegisterComponents(RSRegIndyIntercepts, [TIdLogDebug]);
end;

procedure RegisterUnit_IdLogEvent;
begin//
  RegisterComponents(RSRegIndyIntercepts, [TIdLogEvent]);
end;

procedure RegisterUnit_IdLogFile;
begin//
  RegisterComponents(RSRegIndyIntercepts, [TIdLogFile]);
end;

procedure RegisterUnit_IdLogStream;
begin
  RegisterComponents(RSRegIndyIntercepts, [TIdLogStream]);
end;

