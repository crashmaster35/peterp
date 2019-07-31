
This doc lists the status of the demoes in this dir.

All should be working more or less except the telnet demoes, though
not much work has been put into nicing them up.

- the telnet server has a problem when accepting a password,
   suspect the #13#10 of the username isn't handled well,
   but have trouble debugging.
- Can't test the telnet client properly due to bug in telnet
	server. windows Telnet localhost seems to have same
	behaviour as indy telnetclient.


How to convert a demo:
-----------------------

I converted demoes using this procedure:

- open the .dpr with lazarus, when it asks, create a new GUI application project
- go to compiler options, turn on -Sd, turn off -S2, and set unit and include 
  path to <indybasedir>\indy9libsrc
- remove any {$ *.res} lines
- convert the dfm with tools-> dfm to lfm.  The conversion hardly changes anything,
   but a lot of indy9 demoes are in the old binary format, and this fixes that.
- back up the lfm.
- Only now, open the main form unit. Ignore all errors
- remove any {$ *.res} lines in the main form unit
- add "initialization" section near the bottom of the unit, with {$i mainformunitname.lrs}
- add the following units to the uses line:  LResources,Buttons,Interfaces
- Move all indy components (tid*) from the published to the private section
- In the formcreate (create if necessary), initialise the indy components using
	indyname:=TIdIndyType.create(self);
- Check the backup of the lfm. The backup has the properties "ignored" components still in it.
- assign all relevant properties, move events from published to private section. Note that also
   properties with their default value are listed in the dfm/lfm, you can skip them.
  Usually only the following three categories need to be assigned:
   - event handlers
   - port numbers/bindings for some servers (the protocols that don't have a std port like the udp demoes)
   - For a server the _active_ property which actually enables the server (accept)
- compile and test.

