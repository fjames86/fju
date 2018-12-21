using System;
using System.Net.Sockets;

namespace fju
{
    public class Rpc 
    {
      public string host;
      public int port;

      public Rpc(string host, int port) {
        this.host = host;
	this.port = port;
      }
      
      public byte[] Call(UInt32 prog, UInt32 vers, UInt32 proc, byte[] args) {
        /* encode msg */
	RpcMsg msg = new RpcMsg();
	msg.tag = 0;
	msg.call = new RpcMsgCall();
	msg.call.prog = prog;
	msg.call.vers = vers;
	msg.call.proc = proc;
	msg.call.auth = new OpaqueAuth();
	msg.call.verf = new OpaqueAuth();

        XDR xdr = new XDR(32*1024);
	msg.encode(xdr);
	for(int i = 0; i < args.Length; i++ ) {
	  xdr.buf[xdr.offset++] = args[i];
	}


        Socket s = new Socket(AddressFamily.InterNetwork, SocketType.Dgram, ProtocolType.Udp);
	System.Net.IPEndPoint addr = new System.Net.IPEndPoint(System.Net.IPAddress.Parse(host), port);
	s.SendTo( xdr.buf, xdr.offset, 0, addr);

        byte[] ret = null;
	
        bool b = s.Poll(1000,SelectMode.SelectRead);
	if( b ) {
	  System.Net.EndPoint ep = new System.Net.IPEndPoint(0,0);
	  int count = s.ReceiveFrom(xdr.buf, ref ep);
	  xdr.offset = 0;
	  xdr.count = count;
	  msg.decode(xdr);

          /* check msg */
	  if( msg.tag != 1 ) throw new RpcException("Not a reply");
	  if( msg.reply.tag != 0 ) throw new RpcException("Messsage rejected");
	  if( msg.reply.accept.stat != 0 ) throw new RpcException("Message not accepted");

 	  /* get result data */
	  ret = new byte[xdr.count - xdr.offset];
	  Array.Copy(xdr.buf, xdr.offset, ret, 0, xdr.count - xdr.offset);	  
	}
	s.Close();

        return ret;
      }

  }
}
