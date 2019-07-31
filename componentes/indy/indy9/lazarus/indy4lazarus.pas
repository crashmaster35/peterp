{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit indy4lazarus;

interface

uses
  RegisterIndyComponents, IdAntiFreeze, IdTCPClient, IdIntercept, 
  IdBlockCipherIntercept, IdCompressionIntercept, IdLogDebug, IdLogEvent, 
  IdLogFile, IdLogStream, IdUDPClient, IdDayTime, IdDayTimeUDP, IdDNSResolver, 
  IdEcho, IdEchoUDP, IdFinger, IdFTP, IdGopher, IdHTTP, IdIcmpClient, IdIdent, 
  IdIMAP4, IdIPMCastClient, IdIRC, IdLPR, IdNNTP, IdPOP3, IdQotd, IdQOTDUDP, 
  IdRexec, IdRSH, IdSMTP, IdSNMP, IdSNPP, IdSNTP, IdSysLog, IdTelnet, IdTime, 
  IdTimeUDP, IdTrivialFTP, IdWhois, IdTCPServer, IdUDPServer, IdChargenServer, 
  IdChargenUDPServer, IdDayTimeServer, IdDayTimeUDPServer, IdDICTServer, 
  IdDiscardServer, IdDiscardUDPServer, IdEchoServer, IdEchoUDPServer, 
  IdFingerServer, IdFTPServer, IdGopherServer, IdHTTPServer, IdIdentServer, 
  IdIMAP4Server, IdIPMCastServer, IdIrcServer, IdMappedFTP, IdMappedPortTCP, 
  IdMappedPortUDP, IdNNTPServer, IdPOP3Server, IdQotdServer, IdQOTDUDPServer, 
  IdRexecServer, IdRSHServer, IdSimpleServer, IdSMTPServer, IdSysLogServer, 
  IdTelnetServer, IdTimeServer, IdTimeUDPServer, IdTrivialFTPServer, 
  IdTunnelMaster, IdTunnelSlave, IdWhoIsServer, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegisterIndyComponents', @RegisterIndyComponents.Register);
end;

initialization
  RegisterPackage('indy4lazarus', @Register);
end.
