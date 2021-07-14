
{ -*- mode: fvm -*- }

{
 * This implements a simple watchdog routine which monitors 
 * expected time intervals, looking for abnormal lag.
}

Program Watchdog(0,0,Init,Service,PrevLag);
Begin
   { Includes }
   Include "syscall.pas";
   Include "string.pas";
   Include "log.pas";
   
   { constants }
   Const servicePeriod = 1000;
   Const warnLag = servicePeriod + 50;

   { declarations }

   { globals }
   var prevH, prevL : int;
   var prevlag : int;
   
   { procedures }
   Procedure Init()
   Begin
	Syscall RpcNow(prevH, prevL);
   End;
	
   Procedure Service()
   Begin
	var nowh, nowl : int;
	var diff : int;
	
	Syscall RpcNow(nowh, nowl);

	diff = nowL - prevL;
	If diff < 0 Then diff = -diff;
	diff = diff - servicePeriod;
	
	If (nowh > prevH) || (diff > warnLag) Then
	Begin
		Call LogWritef(LogLvlWarn,"Watchdog: detected lag %dms", diff,0,0,0);
	End;

	prevLag = diff;
	prevL = nowL;
	prevH = nowH;
   End;

   Procedure PrevLag(var lag : int)
   Begin
	lag = prevLag;
   End;
   
   { constant values }

End.
