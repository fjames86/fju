
using System;

namespace fju {
  public class OpaqueAuth {
    public UInt32 flavour;
    public byte[] data;

    public OpaqueAuth() {
      flavour = 0;
      data = new byte[0];
    }

    public OpaqueAuth(UInt32 flavour, byte[] data) {
      this.flavour = flavour;
      this.data = data;
    }
    
    public void encode(XDR xdr) {
      xdr.encodeUInt32(flavour);
      xdr.encodeOpaque(data);
    }

    public void decode(XDR xdr) {
      flavour = xdr.decodeUInt32();
      data = xdr.decodeOpaque();
    }
  }

  public class RpcMsgCall {
    public UInt32 rpcvers;
    public UInt32 prog;
    public UInt32 vers;
    public UInt32 proc;
    public OpaqueAuth auth;
    public OpaqueAuth verf;

    public void encode(XDR xdr) {
      xdr.encodeUInt32(2);
      xdr.encodeUInt32(prog);
      xdr.encodeUInt32(vers);
      xdr.encodeUInt32(proc);
      auth.encode(xdr);
      verf.encode(xdr);
    }

    public void decode(XDR xdr) {
      rpcvers = xdr.decodeUInt32();
      prog = xdr.decodeUInt32();
      vers = xdr.decodeUInt32();
      proc = xdr.decodeUInt32();
      auth = new OpaqueAuth();
      auth.decode(xdr);
      verf = new OpaqueAuth();
      verf.decode(xdr);
    }

  }

  public class RpcMismatch {
    public UInt32 low;
    public UInt32 high;

    public void encode(XDR xdr) {
      xdr.encodeUInt32(low);
      xdr.encodeUInt32(high);
    }

    public void decode(XDR xdr) {
      low = xdr.decodeUInt32();
      high = xdr.decodeUInt32();
    }

  }

  public enum RpcAcceptStat {
    SUCCESS = 0,
    PROG_UNAVAIL = 1,
    PROG_MISMATCH = 2,
    PROC_UNAVAIL = 3,
    GARBAGE_ARGS = 4,
    SYSTEM_ERROR = 5
  }

  public enum RpcAuthError {
    BADCRED = 1,
    REJECTED = 2,
    BADVERF = 3,
    REJECTEDVERF = 4,
    TOOWEAK = 5
  }
  
  public class RpcMsgReplyAccept {
    public OpaqueAuth verf;
    public UInt32 stat;
    public RpcMismatch mismatch;

    public void encode(XDR xdr) {
      verf.encode(xdr);
      if( stat == (UInt32)RpcAcceptStat.PROG_MISMATCH ) {
        mismatch.encode(xdr);
      }         
    }

    public void decode(XDR xdr) {
      verf = new OpaqueAuth();
      verf.decode(xdr);
      stat = xdr.decodeUInt32();
      if( stat == (UInt32)RpcAcceptStat.PROG_MISMATCH ) {
        mismatch = new RpcMismatch();
	mismatch.decode(xdr);
      }
    }

  }

  public class RpcMsgReplyReject {
    public UInt32 tag;
    public RpcMismatch mismatch;
    public UInt32 auth_error;

    public void encode(XDR xdr) {
      xdr.encodeUInt32(tag);
      if( tag == 0 ) {
        mismatch.encode(xdr);
      } else {
        xdr.encodeUInt32(auth_error);
      }        
    }

    public void decode(XDR xdr) {
      tag = xdr.decodeUInt32();
      if( tag == 0 ) {
        mismatch = new RpcMismatch();
	mismatch.decode(xdr);
      } else {
        auth_error = xdr.decodeUInt32();
      }      
    }

  }
  
  public class RpcMsgReply {
    public UInt32 tag;
    public RpcMsgReplyAccept accept;
    public RpcMsgReplyReject reject;

    public void encode(XDR xdr) {
      xdr.encodeUInt32(tag);
      if( tag == 0 ) {
        accept.encode(xdr);
      } else {
        reject.encode(xdr);
      }
    }

    public void decode(XDR xdr) {
      tag = xdr.decodeUInt32();
      if( tag == 0 ) {
        accept = new RpcMsgReplyAccept();
	accept.decode(xdr);
      } else {
        reject = new RpcMsgReplyReject();
	reject.decode(xdr);
      }
    }

  }
  
  public class RpcMsg {
    public UInt32 xid;
    public UInt32 tag;
    public RpcMsgCall call;
    public RpcMsgReply reply;

    public void encode(XDR xdr) {
      xdr.encodeUInt32(xid);
      xdr.encodeUInt32(tag);
      if( tag == 0 ) call.encode(xdr);
      else reply.encode(xdr);
    }

    public void decode(XDR xdr) {
      xid = xdr.decodeUInt32();
      tag = xdr.decodeUInt32();
      if( tag == 0 ) {
        call = new RpcMsgCall();
	call.decode(xdr);
      } else {
        reply = new RpcMsgReply();
        reply.decode(xdr);
      }
    }

  }
}