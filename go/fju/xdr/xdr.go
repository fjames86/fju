package xdr

type Xdr struct {
	offset int
	buf []byte
}

func New(buf []byte) *Xdr {
	return &Xdr {
		buf: buf,
	}
}

func (xdr *Xdr) Reset() {
	xdr.offset = 0
}

func (xdr *Xdr) Offset() int {
	return xdr.offset
}

func (xdr *Xdr) Buffer() []byte {
	return xdr.buf[0:xdr.offset]
}

func (xdr *Xdr) EncodeUInt32(x uint32) {
	xdr.buf[xdr.offset] = byte(x >> 24)
	xdr.buf[xdr.offset + 1] = byte(x >> 16)
	xdr.buf[xdr.offset + 2] = byte(x >> 8)
	xdr.buf[xdr.offset + 3] = byte(x)
	xdr.offset += 4
}

func (xdr *Xdr) DecodeUInt32() uint32 {
	var x uint32
	x = 0
	xdr.offset += 4
	return x
}

func (xdr *Xdr) EncodeInt(x int) {
	xdr.EncodeUInt32(uint32(x))
}

func (xdr *Xdr) DecodeInt() int {
	return int(xdr.DecodeUInt32())
}

func (xdr *Xdr) EncodeUInt64(x uint64) {
	xdr.EncodeUInt32(uint32(x >> 32))
	xdr.EncodeUInt32(uint32(x))
}

func (xdr *Xdr) DecodeUInt64() uint64 {
	h := xdr.DecodeUInt32()
	l := xdr.DecodeUInt32()
	return (uint64(h) << 32) | uint64(l)
}

func (xdr *Xdr) EncodeString(x string) {
	len := len(x)
	xdr.EncodeInt(len)
	xdr.EncodeFixed([]byte(x))
}

func (xdr *Xdr) DecodeString() string {
	len := xdr.DecodeInt()
	buf := xdr.buf[xdr.offset:xdr.offset + len]
	xdr.offset += len
	if len % 4 != 0 {
		xdr.offset += 4 - (len % 4)
	}
	return string(buf)
}

func (xdr *Xdr) EncodeOpaque(x []byte) {
	xdr.EncodeInt(len(x))
	copy(xdr.buf[xdr.offset:], x)
	xdr.offset += len(x)
	if len(x) % 4 != 0 {
		xdr.offset += 4 - (len(x))
	}
}

func (xdr *Xdr) DecodeOpaque() []byte {
	len := xdr.DecodeInt()
	buf := make([]byte, len)
	copy(buf, xdr.buf[xdr.offset:xdr.offset + len])
	xdr.offset += len
	if len % 4 != 0 {
		xdr.offset += 4 - (len % 4)
	}
	return buf
}

func (xdr *Xdr) EncodeFixed(x []byte) {
	copy(xdr.buf[xdr.offset:], x)
	xdr.offset += len(x)
	if len(x) % 4 != 0 {
		xdr.offset += 4 - (len(x) % 4)
	}
}

func (xdr *Xdr) DecodeFixed(len int) []byte {
	buf := make([]byte, len)
	copy(buf, xdr.buf[xdr.offset:xdr.offset + len])
	xdr.offset += len
	if len % 4 != 0 {
		xdr.offset += 4 - (len % 4)
	}
	return buf
}



	
	


