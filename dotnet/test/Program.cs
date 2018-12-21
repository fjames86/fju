using System;
using fju;

namespace test
{
    class Program
    {
        static void Main(string[] args)
        {
	   Rpc rpc = new Rpc("127.0.0.1", 8000);
	   byte[] ret = rpc.Call(100000,2,4,new byte[0]);
	   if( ret == null ) {
             Console.WriteLine("Timeout");
	   } else {
             Console.WriteLine("Program Version Protocol Port");
             XDR xdr = new XDR(ret);
	     bool b = xdr.decodeBoolean();
	     while( b ) {
	       BindMapping mapping = new BindMapping(xdr);
	       Console.WriteLine("{0} {1} {2} {3}", mapping.prog, mapping.vers, mapping.prot, mapping.port);
	       b = xdr.decodeBoolean();
	     }
	   }
        }
    }

    class BindMapping {
      public UInt32 prog;
      public UInt32 vers;
      public UInt32 prot;
      public UInt32 port;
      public BindMapping(XDR xdr) {
        prog = xdr.decodeUInt32();
	vers = xdr.decodeUInt32();
	prot = xdr.decodeUInt32();
	port = xdr.decodeUInt32();
      }
    }
}
