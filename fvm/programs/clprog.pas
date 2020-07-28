{ -*- text -*- }

Program ClusterProc(2333335,1,ProgNull,ProgInvoke);
Begin

Procedure ProcNull(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
   
End;

var scheduled : opaque[64];
var count : integer;
var seqno : integer;

Procedure ProcInvoke(argcount : integer, argbuf : opaque, var rescount : integer, var resbuf : opaque )
Begin
	var p1 : opaque;
	var p2 : opaque;
	var i : integer;

	If argcount <> 8 Then
	   Return;
	
	p1 := argbuf;
	p2 := scheduled;
	p2 := p2 + 8*count;
	^p2 := ^p1;
	p2 := p2 + 4;
	p1 := p1 + 4;
	^p2 := ^p1;
	
	
End;

Procedure InvokeMethod(progid : integer, procid : integer)
	  var rescount : integer;
	  Syscall Invoke(progid, procid, 0, 0, 0, rescount);
Begin
End;

Procedure Service()
Begin
	var progid : integer;
	var procid : integer;
	var p : opaque;

again:
	p := scheduled;

	While count > 0 Do
	Begin
		progid := ^p;
		p := p + 4;
		procid := ^p;
		p := p + 4;
		count := count - 1;
		Call InvokeMethod(progid, procid);
	End;

	goto again;
End;

End.
