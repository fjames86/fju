
{ -*- mode: fvm -*- }

{
 * Header file for dlm fvm interface
}

Const DlmEx = 1;
Const DlmSh = 2;
Const DlmBlockEx = 3;
Const DlmBlockSh = 4;

Record DlmLock =
       lockidH : int;
       lockidL : int;
       hostidH : int;
       hostidL : int;
       residH : int;
       residL : int;
       state : int; { state : DlmEx etc } 
       spare : int;
End;

Declare Procedure DLM/Get(idH : int, idL : int, var len : int, var lock : opaque);
Declare Procedure DLM/Acquire(residH : int, residL : int, shared : int, var lockidH : int, var lockidL : int);
Declare Procedure DLM/Release(idH : int, idL : int);


   
