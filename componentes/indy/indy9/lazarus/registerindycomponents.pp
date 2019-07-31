(******************************************************************************
                        INDY COMPONENTS FOR FPC/LAZARUS

 SEE File : license.txt
            http://www.nevrona.com/Indy/

 Unit owner:    Olivier Guilbaud

 Last modified: JULY 08, 2003

******************************************************************************)


unit RegisterIndyComponents;

interface
{$I idcompilerdefines.inc}
uses
  Classes, SysUtils, LResources,

  IdTCPClient,IdUDPClient,IdDayTime,IdDayTimeUDP,IdDNSResolver,
  IdEcho,IdEchoUDP,IdFinger,IdFTP,IdGopher,IdHTTP,IdIcmpClient,
  IdIdent,IdIMAP4,IdIPMCastClient,IdIRC,IdLPR,IdNNTP,IdPOP3,
  IdQotd,IdQOTDUDP,IdRexec,IdRSH,IdSMTP,IdSNMP,IdSNPP,IdSNTP,
  IdSysLog,IdTelnet,IdTime,IdTimeUDP,IdTrivialFTP,IdWhois,

  IdTCPServer,IdUDPServer,IdChargenServer,IdChargenUDPServer,
  IdDayTimeServer,IdDayTimeUDPServer,IdDICTServer,IdDiscardServer,
  IdDiscardUDPServer,IdEchoServer,IdEchoUDPServer,IdFingerServer,
  IdFTPServer,IdGopherServer,IdHTTPServer,IdIdentServer,IdIMAP4Server,
  IdIPMCastServer,IdIrcServer,IdMappedFTP,IdMappedPortTCP,
  IdMappedPortUDP,IdNNTPServer,IdPOP3Server,IdQotdServer,IdQOTDUDPServer,
  IdRexecServer,IdRSHServer,IdSimpleServer,IdSMTPServer,IdSysLogServer,
  IdTelnetServer,IdTimeServer,IdTimeUDPServer,IdTrivialFTPServer,
  IdTunnelMaster,IdTunnelSlave,IdWhoIsServer,

  IdBlockCipherIntercept,IdCompressionIntercept,IdLogDebug,
  IdLogEvent,IdLogFile,IdLogStream,IdIntercept,

  IdAntiFreeze,

  LazarusPackageIntf;

procedure Register;

implementation
uses IdResourceStrings;


procedure RegisterUnit_IdAntiFreeze;
begin
  RegisterComponents(RSRegIndyMisc,[TIdAntiFreeze]);
end;


{$I idRegInterceptsComponents.inc}
{$I idregclientscomponents.inc}
{$I idregservercomponents.inc}

procedure Register;
begin
  //Indy Clients (33 components)
  RegisterUnit('IdTCPClient', @RegisterUnit_IdTCPClient);
  RegisterUnit('IdUDPClient', @RegisterUnit_IdUDPClient);
  RegisterUnit('IdDayTime', @RegisterUnit_IdDayTime);
  RegisterUnit('IdDayTimeUDP', @RegisterUnit_IdDayTimeUDP);
  RegisterUnit('IdDNSResolver', @RegisterUnit_IdDNSResolver);
  RegisterUnit('IdEcho', @RegisterUnit_IdEcho);
  RegisterUnit('IdEchoUDP', @RegisterUnit_IdEchoUDP);
  RegisterUnit('IdFinger', @RegisterUnit_IdFinger);
  RegisterUnit('IdFTP', @RegisterUnit_IdFTP);
  RegisterUnit('IdGopher', @RegisterUnit_IdGopher);
  RegisterUnit('IdHTTP', @RegisterUnit_IdHTTP);
  RegisterUnit('IdIcmpClient', @RegisterUnit_IdIcmpClient);
  RegisterUnit('IdIdent', @RegisterUnit_IdIdent);
  RegisterUnit('IdIMAP4', @RegisterUnit_IdIMAP4);
  RegisterUnit('IdIPMCastClient', @RegisterUnit_IdIPMCastClient);
  RegisterUnit('IdIRC', @RegisterUnit_IdIRC);
  RegisterUnit('IdLPR', @RegisterUnit_IdLPR);
  RegisterUnit('IdNNTP', @RegisterUnit_IdNNTP);
  RegisterUnit('IdPOP3', @RegisterUnit_IdPOP3);
  RegisterUnit('IdQotd', @RegisterUnit_IdQotd);
  RegisterUnit('IdQOTDUDP', @RegisterUnit_IdQOTDUDP);
  RegisterUnit('IdRexec', @RegisterUnit_IdRexec);
  RegisterUnit('IdRSH', @RegisterUnit_IdRSH);
  RegisterUnit('IdSMTP', @RegisterUnit_IdSMTP);
  RegisterUnit('IdSNMP', @RegisterUnit_IdSNMP);
  RegisterUnit('IdSNPP', @RegisterUnit_IdSNPP);
  RegisterUnit('IdSNTP', @RegisterUnit_IdSNTP);
  RegisterUnit('IdSysLog', @RegisterUnit_IdSysLog);
  RegisterUnit('IdTelnet', @RegisterUnit_IdTelnet);
  RegisterUnit('IdTime', @RegisterUnit_IdTime);
  RegisterUnit('IdTimeUDP', @RegisterUnit_IdTimeUDP);
  RegisterUnit('IdTrivialFTP', @RegisterUnit_IdTrivialFTP);
  RegisterUnit('IdWhois', @RegisterUnit_IdWhois);

  //Indy server (41 components)
  RegisterUnit('IdTCPServer', @RegisterUnit_IdTCPServer);
  RegisterUnit('IdUDPServer', @RegisterUnit_IdUDPServer);
  RegisterUnit('IdChargenServer', @RegisterUnit_IdChargenServer);
  RegisterUnit('IdChargenUDPServer', @RegisterUnit_IdChargenUDPServer);
  RegisterUnit('IdDayTimeServer', @RegisterUnit_IdDayTimeServer);
  RegisterUnit('IdDayTimeUDPServer', @RegisterUnit_IdDayTimeUDPServer);
  RegisterUnit('IdDICTServer', @RegisterUnit_IdDICTServer);
  RegisterUnit('IdDiscardServer', @RegisterUnit_IdDiscardServer);
  RegisterUnit('IdDiscardUDPServer', @RegisterUnit_IdDiscardUDPServer);
  RegisterUnit('IdEchoServer', @RegisterUnit_IdEchoServer);
  RegisterUnit('IdEchoUDPServer', @RegisterUnit_IdEchoUDPServer);
  RegisterUnit('IdFingerServer', @RegisterUnit_IdFingerServer);
  RegisterUnit('IdGopherServer', @RegisterUnit_IdGopherServer);
  RegisterUnit('IdHTTPServer', @RegisterUnit_IdHTTPServer);
  RegisterUnit('IdIdentServer', @RegisterUnit_IdIdentServer);
  RegisterUnit('IdIMAP4Server', @RegisterUnit_IdIMAP4Server);
  RegisterUnit('IdIPMCastServer', @RegisterUnit_IdIPMCastServer);
  RegisterUnit('IdIrcServer', @RegisterUnit_IdIrcServer);
  RegisterUnit('IdMappedFTP', @RegisterUnit_IdMappedFTP);
  RegisterUnit('IdMappedPortTCP', @RegisterUnit_IdMappedPortTCP);
  RegisterUnit('IdMappedPortUDP', @RegisterUnit_IdMappedPortUDP);
  RegisterUnit('IdNNTPServer', @RegisterUnit_IdNNTPServer);
  RegisterUnit('IdPOP3Server', @RegisterUnit_IdPOP3Server);
  RegisterUnit('IdQotdServer', @RegisterUnit_IdQotdServer);
  RegisterUnit('IdQOTDUDPServer', @RegisterUnit_IdQOTDUDPServer);
  RegisterUnit('IdRexecServer', @RegisterUnit_IdRexecServer);
  RegisterUnit('IdRSHServer', @RegisterUnit_IdRSHServer);
  RegisterUnit('IdSimpleServer', @RegisterUnit_IdSimpleServer);
  RegisterUnit('IdSMTPServer', @RegisterUnit_IdSMTPServer);
  RegisterUnit('IdSysLogServer', @RegisterUnit_IdSysLogServer);
  RegisterUnit('IdTelnetServer', @RegisterUnit_IdTelnetServer);
  RegisterUnit('IdTimeServer', @RegisterUnit_IdTimeServer);
  RegisterUnit('IdTimeUDPServer', @RegisterUnit_IdTimeUDPServer);
  RegisterUnit('IdTrivialFTPServer', @RegisterUnit_IdTrivialFTPServer);
  RegisterUnit('IdTunnelMaster', @RegisterUnit_IdTunnelMaster);
  RegisterUnit('IdTunnelSlave', @RegisterUnit_IdTunnelSlave);
  RegisterUnit('IdWhoIsServer', @RegisterUnit_IdWhoIsServer);

  //Indy Intercepts (7 components)
  RegisterUnit('IdIntercept'              ,@RegisterUnit_IdConnectionIntercept);
  RegisterUnit('IdBlockCipherIntercept'   ,@RegisterUnit_IdBlockCipherIntercept);
  RegisterUnit('IdCompressionIntercept'   ,@RegisterUnit_IdCompressionIntercept);
  RegisterUnit('IdLogDebug'               ,@RegisterUnit_IdLogDebug);
  RegisterUnit('IdLogEvent'               ,@RegisterUnit_IdLogEvent);
  RegisterUnit('IdLogFile'                ,@RegisterUnit_IdLogFile);
  RegisterUnit('IdLogStream'              ,@RegisterUnit_IdLogStream);


  RegisterUnit('IdAntiFreeze',@RegisterUnit_IdAntiFreeze);
end;

initialization
  {$i RegisterIndyComponents.lrs}

end.
