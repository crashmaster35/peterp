{*********************************
 *   Indy server  components     *
 *********************************}
 
Procedure RegisterUnit_IdTCPServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdTCPServer]);
end;

Procedure RegisterUnit_IdUDPServer;
begin
  RegisterComponents(RSRegIndyServers,[TidUDPServer]);
end;

Procedure RegisterUnit_IdChargenServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdChargenServer]);
end;

Procedure RegisterUnit_IdChargenUDPServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdChargenUDPServer]);
end;

Procedure RegisterUnit_IdDayTimeServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdDayTimeServer]);
end;

Procedure RegisterUnit_IdDayTimeUDPServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdDayTimeUDPServer]);
end;

Procedure RegisterUnit_IdDICTServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdDICTServer]);
end;

Procedure RegisterUnit_IdDiscardServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdDiscardServer]);
end;

Procedure RegisterUnit_IdDiscardUDPServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdDiscardUDPServer]);
end;

Procedure RegisterUnit_IdEchoServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdEchoServer]);
end;

Procedure RegisterUnit_IdEchoUDPServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdEchoUDPServer]);
end;

Procedure RegisterUnit_IdFingerServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdFingerServer]);
end;

Procedure RegisterUnit_IdGopherServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdGopherServer]);
end;

Procedure RegisterUnit_IdHTTPServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdHTTPServer]);
end;

Procedure RegisterUnit_IdIdentServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdIdentServer]);
end;

Procedure RegisterUnit_IdIMAP4Server;
begin
  RegisterComponents(RSRegIndyServers,[TIdIMAP4Server]);
end;

Procedure RegisterUnit_IdIPMCastServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdIPMCastServer]);
end;

Procedure RegisterUnit_IdIrcServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdIrcServer]);
end;

Procedure RegisterUnit_IdMappedFTP;
begin
  RegisterComponents(RSRegIndyServers,[TIdMappedFTP]);
end;

Procedure RegisterUnit_IdMappedPortTCP;
begin
  RegisterComponents(RSRegIndyServers,[TIdMappedPortTCP,TIdMappedPop3,TidMappedTelnet]);
end;

Procedure RegisterUnit_IdMappedPortUDP;
begin
  RegisterComponents(RSRegIndyServers,[TIdMappedPortUDP]);
end;

Procedure RegisterUnit_IdNNTPServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdNNTPServer]);
end;

Procedure RegisterUnit_IdPOP3Server;
begin
  RegisterComponents(RSRegIndyServers,[TIdPOP3Server]);
end;

Procedure RegisterUnit_IdQotdServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdQotdServer]);
end;

Procedure RegisterUnit_IdQOTDUDPServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdQOTDUDPServer]);
end;

Procedure RegisterUnit_IdRexecServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdRexecServer]);
end;

Procedure RegisterUnit_IdRSHServer;
begin
  RegisterComponents(RSRegIndyServers,[TIdRSHServer]);
end;

Procedure RegisterUnit_IdSimpleServer;
begin
  RegisterComponents(RSRegIndyServers2,[TIdSimpleServer]);
end;

Procedure RegisterUnit_IdSMTPServer;
begin
  RegisterComponents(RSRegIndyServers2,[TIdSMTPServer]);
end;

Procedure RegisterUnit_IdSysLogServer;
begin
  RegisterComponents(RSRegIndyServers2,[TIdSysLogServer]);
end;

Procedure RegisterUnit_IdTelnetServer;
begin
  RegisterComponents(RSRegIndyServers2,[TIdTelnetServer]);
end;

Procedure RegisterUnit_IdTimeServer;
begin
  RegisterComponents(RSRegIndyServers2,[TIdTimeServer]);
end;

Procedure RegisterUnit_IdTimeUDPServer;
begin
  RegisterComponents(RSRegIndyServers2,[TIdTimeUDPServer]);
end;

Procedure RegisterUnit_IdTrivialFTPServer;
begin
  RegisterComponents(RSRegIndyServers2,[TIdTrivialFTPServer]);
end;

Procedure RegisterUnit_IdTunnelMaster;
begin
  RegisterComponents(RSRegIndyServers2,[TIdTunnelMaster]);
end;

Procedure RegisterUnit_IdTunnelSlave;
begin
  RegisterComponents(RSRegIndyServers2,[TIdTunnelSlave]);
end;

Procedure RegisterUnit_IdWhoIsServer;
begin
  RegisterComponents(RSRegIndyServers2,[TIdWhoIsServer]);
end;


