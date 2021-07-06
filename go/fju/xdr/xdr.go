package xdr

type Xdr struct {
	offset int
	buf []byte
}

func New(buf []byte) *Xdr {
	return &Xdr {
		buf : buf
	}
}

func (xdr *Xdr) Reset() {
	xdr.offset = 0
}

func (xdr *Xdr) Offset() {
	return xdr.offset
}

func (xdr *Xdr) EncodeUInt32(x uint32) {
	xdr.buf[xdr.offset] = x >> 24
	xdr.buf[xdr.offset + 1] = x >> 16
	xdr.buf[xdr.offset + 2] = x >> 8
	xdr.buf[xdr.offset + 3] = x
	xdr.offset += 4
}

func (xdr *Xdr) DecodeUInt32() uint32 {
	x := 0
	xdr.offset += 4
	return x
}

func (xdr *Xdr) EncodeString(x string) {
}

func (xdr *Xdr) DecodeString() string {
	return nil
}

func (xdr *Xdr) EncodeOpaque(x []byte) {
}

func (xdr *Xdr) DecodeOpaque() []byte {
	return nil
}

func (xdr *Xdr) EncodeFixed(x []byte) {
}

func (xdr *Xdr) DecodeFixed(len int) []byte {
}



	
	


