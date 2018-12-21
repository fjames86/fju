
using System;

namespace fju {
  public class XDR {
    public byte[] buf;
    public int count;
    public int offset;

    public XDR(byte[] buf) {
      this.buf = buf;
      this.count = buf.Length;
      this.offset = 0;
    }

    public XDR(int count) {
      this.buf = new byte[count];
      this.count = count;
      this.offset = 0;
    }

    public void encodeUInt32(UInt32 x) {
      buf[offset++] = (byte)((x >> 24) & 0xff);
      buf[offset++] = (byte)((x >> 16) & 0xff);
      buf[offset++] = (byte)((x >> 8) & 0xff);
      buf[offset++] = (byte)((x) & 0xff);
    }

    public UInt32 decodeUInt32() {
       UInt32 x = 0;
       x = buf[offset++];
       x = (x << 8) | buf[offset++];
       x = (x << 8) | buf[offset++];
       x = (x << 8) | buf[offset++];
       return x;
    }

    public void encodeString(String x) {
      byte[] octets = System.Text.Encoding.UTF8.GetBytes(x);
      encodeOpaque(octets);
    }

    public String decodeString() {
      return System.Text.Encoding.UTF8.GetString(decodeOpaque());
    }
 
    public void encodeOpaque(byte[] opaque) {
      encodeUInt32((UInt32)opaque.Length);
      for(int i = 0; i < opaque.Length; i++ ) {
        buf[offset++] = opaque[i];
      }
      if( opaque.Length % 4 != 0 ) {
        offset += 4 - (int)(opaque.Length % 4);
      }
    }

    public byte[] decodeOpaque() {
      UInt32 len = decodeUInt32();
      byte[] octets = new byte[len];
      for( int i = 0; i < len; i++ ) {
        octets[i] = buf[offset++];
      }
      if( len % 4 != 0 ) {
        offset += (4 - (int)(len % 4));
      }
       return octets;
    }

    public void encodeBoolean(bool x) {
      encodeUInt32((UInt32)(x ? 1 : 0));
    }
    public bool decodeBoolean() {
      UInt32 x = decodeUInt32();
      return x == 0 ? false : true;
    }
    
  }
}

