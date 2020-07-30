{ -*- text -*- }

Program ClusterInvoke(PROG_CLUSTERINVOKE, 1, ClusterInvoke, Service);
Begin

Const EntrySize := 8;
Const MaxEntry := 8;
Const ServicePeriod := 1000;

var scheduled : opaque[256]; { max of 8 entries } 
var count : integer;
var seqno : integer; { next index is seqno % 8 }

{ Insert an entry into the log }
Procedure WriteEntry(progid : integer, procid : integer)
Begin
	var i : integer;
	var p : opaque;
	
	i := (seqno % MaxEntry) * EntrySize;
	p := scheduled + i;
	^p := progid;
	p := p + 4;
	^p := procid;
	seqno := seqno + 1;
	If count < MaxEntry Then count := count + 1;
End;

{
Register for a method to be invoked.
}
Procedure ClusterInvoke(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	var p : opaque;
	var progid, procid : integer;

	If argcount <> 8 Then
	   Return;

	p := argbuf;
	progid := ^p;
	p := p + 4;
	procid := ^p;
	Call WriteEntry(procid, procid);
End;

{ Callback methods are invoked with no arguments and receive no results }
Procedure InvokeMethod(progid : integer, procid : integer)
Begin
	  var msgbuf : string[64];
	  var rescount : integer;

	  Syscall Sprintf(msgbuf, 64, "Invoking %u:%u", AddressOf progid);
	  Syscall LogStr(msgbuf);
	  
	  Syscall Invoke(progid, procid, 0, 0, 0, rescount);
End;

{
The service routine does NOT get called in the standard way (being registered as a service routine).
Instead, it is registered as an initialization routine. It then periodically yields with a timeout.
This has the same effect, but it allows the service routine to keep local variables.
}
Procedure Service()
Begin
	var seq, progid, procid, i : integer;
	var p : opaque;

again:
	While seq < seqno Do
	Begin
		i := (seq % MaxEntry) * EntrySize;
		p := scheduled + i;
		progid := ^p;
		p := p + 4;
		procid := ^p;
		Call InvokeMethod(progid, procid);

		seq := seq + 1;
	End;

	Syscall Yield(ServicePeriod, 0, i);
	goto again;
End;

End.
