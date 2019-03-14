
(defpackage #:cfgen
  (:use #:cl))

(in-package #:cfgen)

(defparameter *prefix* "cfgen")

(defun type-name (type-sym)
  (ecase type-sym
    ((:char :string) "char")
    (:uint32 "uint32_t")
    (:uint64 "uint64_t")
    (:int "int")
    (:int32 "int32_t")
    (:int64 "int64_t")))

(defun emit-header-struct (entries extras)
  (format t "struct ~A_header {~%" *prefix*)
  (format t "    uint32_t magic;~%")
  (format t "#define ~A_MAGIC 0x~8,'0X~%" (string-upcase *prefix*) (random #xffffffff))
  (format t "    uint32_t version;~%")
  (format t "    uint64_t seq;~%")
  (dolist (entry entries)
    (format t "    uint32_t ~A_max;~%" entry)
    (format t "    uint32_t ~A_count;~%" entry))
  (dolist (extra extras)
    (destructuring-bind (name type &optional array-length) extra 
      (cond
	(array-length (format t "    ~A ~A[~A];~%"
			      (type-name type) name array-length))
	(t (format t "    ~A ~A;~%" (type-name type) name)))))
  (format t "};~%"))

(defun emit-prop-struct (entries extras)
  (format t "struct ~A_prop {~%" *prefix*)
  (format t "    uint32_t version;~%")
  (format t "#define ~A_VERSION 1~%" (string-upcase *prefix*))
  (format t "    uint64_t seq;~%")
  (dolist (entry entries)
    (format t "    uint32_t ~A_max;~%" entry)
    (format t "    uint32_t ~A_count;~%" entry))
  (dolist (extra extras)
    (destructuring-bind (name type &optional array-length) extra 
      (cond
	(array-length (format t "    ~A ~A[~A];~%"
			      (type-name type) name array-length))
	(t (format t "    ~A ~A;~%" (type-name type) name)))))
  (format t "};~%"))

(defun emit-prop (entries extras)
  (format t "int ~A_prop( struct ~A_prop *prop ) {~%" *prefix* *prefix*)
  (format t "    if( glob.ocount <= 0 ) return -1;~%")
  (format t "    ~A_lock();~%" *prefix*)
  (format t "    prop->version = glob.file->header.version;~%")
  (format t "    prop->seq = glob.file->header.seq;~%")
  (dolist (entry entries)
    (format t "    prop->~A_max = glob.file->header.~A_max;~%" entry entry)
    (format t "    prop->~A_count = glob.file->header.~A_count;~%" entry entry))
  (dolist (extra extras)
    (destructuring-bind (name type &optional array-length) extra
      (declare (ignore type))
      (cond
	(array-length (format t "    memcpy( prop->~A, glob.file->header.~A, sizeof(glob.file->header.~A[0]) * ~A );~%"
			      name name name array-length))
	(t (format t "    prop->~A = glob.file->header.~A;~%" name name)))))
  (format t "    ~A_unlock();~%" *prefix*)
  (format t "    return 0;~%")
  (format t "}~%"))
  
(defun emit-file-struct (entries)
  (format t "~%")
  (format t "struct ~A_file {~%" *prefix*)
  (format t "    struct ~A_header header;~%" *prefix*)
  (dolist (entry entries)
    (format t "    struct ~A_~A ~A[~A_MAX_~A]~%"
	    *prefix*
	    entry
	    entry
	    (string-upcase *prefix*)
	    (string-upcase entry)))
  (format t "};~%"))

(defun emit-open (entries)
  (format t "int ~A_open( void ) {~%" *prefix*)
  (format t "    int sts;~%")
  (format t "    ~%")
  (format t "    if( glob.ocount < 0 ) return -1;~%")
  (format t "    if( glob.ocount > 0 ) {~%")
  (format t "        glob.ocount++;~%")
  (format t "        return 0;~%")
  (format t "    }~%")
  (format t "    ~%")
  (format t "    sts = mmf_open( \"~A.dat\", &glob.mmf );~%" *prefix*)
  (format t "    if( sts ) return sts;~%")
  (format t "    ~%")
  (format t "    sts = mmf_remap( &glob.mmf, sizeof(*glob.file) );~%")
  (format t "    if( sts ) goto bad;~%")
  (format t "    glob.file = (struct ~A_file *)glob.mmf.file;~%" *prefix*)
  (format t "    ~%")
  (format t "    ~A_lock();~%" *prefix*)
  (format t "    if( glob.file->header.magic != ~A_MAGIC ) {~%" (string-upcase *prefix*))
  (format t "        glob.file->header.magic = ~A_MAGIC;~%" (string-upcase *prefix*))
  (format t "        glob.file->header.version = ~A_VERSION;~%" (string-upcase *prefix*))
  (format t "        glob.file->header.seq = 1;~%")
  (dolist (entry entries)
    (format t "        glob.file->header.~A_max = ~A_MAX_~A;~%" entry (string-upcase *prefix*) (string-upcase entry))
    (format t "        glob.file->header.~A_count = 0;~%" entry))
  (format t "    } else if( glob.file->header.version != ~A_VERSION ) {~%" (string-upcase *prefix*))
  (format t "        ~A_unlock();~%" *prefix*)
  (format t "        goto bad;~%")
  (format t "    }~%")
  (format t "    ~A_unlock();~%" *prefix*)
  (format t "    ~%")
  (format t "    glob.ocount = 1;~%")
  (format t "    return 0;~%")
  (format t " bad:~%")
  (format t "    mmf_close( &glob.mmf );~%")
  (format t "    return -1;~%")
  (format t "}~%"))

(defun emit-close ()
  (format t "int ~A_close( void ) {~%" *prefix*)
  (format t "    if( glob.ocount <= 0 ) return -1;~%")
  (format t "    glob.ocount--;~%")
  (format t "    if( glob.ocount > 0 ) return 0;~%")
  (format t "    mmf_close( &glob.mmf );~%")
  (format t "    return 0;~%")
  (format t "}~%"))

(defun emit-reset (entries)
  (format t "int ~A_reset( void ) {~%" *prefix*)
  (format t "    if( glob.ocount <= 0 ) return -1;~%")
  (format t "    ~A_lock();~%" *prefix*)
  (format t "    glob.file->header.magic = ~A_MAGIC;~%" (string-upcase *prefix*))
  (format t "    glob.file->header.version = ~A_VERSION;~%" (string-upcase *prefix*))
  (format t "    glob.file->header.seq = 1;~%")
  (dolist (entry entries)
    (format t "    glob.file->header.~A_max = ~A_MAX_~A;~%" entry (string-upcase *prefix*) (string-upcase entry))
    (format t "    glob.file->header.~A_count = 0;~%" entry))
  (format t "    ~A_unlock();~%" *prefix*)
  (format t "    return 0;~%")
  (format t "}~%"))

(defun emit-entry-struct (name fields)
  (format t "struct ~A_~A {~%" *prefix* name)
  (format t "    uint64_t tag;~%")
  (dolist (field fields)
    (destructuring-bind (name type &optional array-length) field
      (cond
	(array-length (format t "    ~A ~A[~A];~%"
			      (type-name type) name array-length))
	(t (format t "    ~A ~A;~%" (type-name type) name)))))
  (format t "};~%"))


(defun emit-entry-list (name)
  (format t "int ~A_~A_list( struct ~A_~A *~Alist, int n ) {~%"
	  *prefix* name *prefix* name name)
  (format t "    int sts, i;~%")
  (format t "    if( glob.ocount <= 0 ) return -1;~%")
  (format t "    ~A_lock();~%" *prefix*)
  (format t "    for( i = 0; i < glob.file->header.~A_count; i++ ) {~%"
	  name)
  (format t "         if( i < n ) {~%")
  (format t "             ~Alist[i] = glob.file->~A[i];~%" name name)
  (format t "         }~%")
  (format t "    }~%")
  (format t "    sts = glob.file->header.~A_count;~%" name)
  (format t "    ~A_unlock();~%" *prefix*)
  (format t "    return sts;~%")
  (format t "}~%"))

(defun key-comp (name key keytype keysym)
  (case keytype 
    (:string
     (format nil "strcmp( glob.file->~A[i].~A, ~A ) == 0" name key keysym))
    (t      
     (format nil "glob.file->~A[i].~A == ~A" name key keysym))))

(defun emit-entry-by-key (name key keytype)
  (format t "int ~A_~A_by_~A( ~A ~A~A, struct ~A_~A *~A ) {~%"
	  *prefix* name key
	  (type-name keytype) (if (eq keytype :string) "*" "")
	  key
	  *prefix* name name)
  (format t "    int sts;~%")
  (format t "    if( glob.ocount <= 0 ) return -1;~%")
  (format t "    ~A_lock();~%" *prefix*)
  (format t "    sts = -1;~%")
  (format t "    for( i = 0; i < glob.file->header.~A_count; i++ ) {~%"
	  name)
  (format t "        if( ~A ) {~%" (key-comp name key keytype key))
  (format t "            if( ~A ) *~A = glob.file->~A[i];~%"
	  name name name)
  (format t "            sts = 0;~%")
  (format t "            break;~%")
  (format t "        }~%")
  (format t "    }~%")
  (format t "    ~A_unlock();~%" *prefix*)
  (format t "    return sts;~%")
  (format t "}~%"))

(defun emit-entry-by-tag (name)
  (emit-entry-by-key name "tag" :uint64))

(defun emit-entry-add (name)
  (format t "int ~A_~A_add( struct ~A_~A *~A ) {~%"
	  *prefix* name *prefix* name name)
  (format t "    int sts, i;~%")
  (format t "    if( glob.ocount <= 0 ) return -1;~%")
  (format t "    ~A_lock();~%" *prefix*)
  (format t "    sts = -1;~%")
  (format t "    if( glob.file->header.~A_count < glob.file->header.~A_max ) {~%"
	  name name)
  (format t "        memset( ~A, 0, sizeof(*~A) );~%" name name)
  (format t "        ~A->tag = glob.file->header.seq;~%" name)
  (format t "        glob.file->header.seq++;~%")
  (format t "        glob.file->~A_count++;~%" name)
  (format t "        sts = 0;~%")
  (format t "    }~%")
  (format t "    ~A_unlock();~%" *prefix*)
  (format t "    return sts;~%")
  (format t "}~%"))

(defun emit-entry-rem (name key keytype)
  (format t "int ~A_~A_rem( ~A ~A ) {~%" *prefix* name (type-name keytype) key)
  (format t "    int sts, i;~%")
  (format t "    if( glob.ocount <= 0 ) return -1;~%")
  (format t "    ~A_lock();~%" *prefix*)
  (format t "    sts = -1;~%")
  (format t "    for( i = 0; i < glob.file->header.~A_count; i++ ) {~%"
	  name)
  (format t "        if( glob.file->~A[i].~A == ~A ) {~%" name key key)
  (format t "            if( i != (glob.file->header.~A_count - 1) ) glob.file->~A[i] = glob.file->~A[glob.file->header.~A_count - 1];~%"
	  name name name name)
  (format t "            glob.file->header.~A_count--;~%" name)
  (format t "            glob.file->header.seq++;~%")
  (format t "            sts = 0;~%")
  (format t "            break;~%")
  (format t "        }~%")
  (format t "    }~%")
  (format t "    ~A_unlock();~%" *prefix*)
  (format t "    return sts;~%")
  (format t "}~%"))

(defun emit-entry-set (name key keytype)
  (format t "int ~A_~A_set( struct ~A_~A *~A ) {~%"
	  *prefix* name *prefix* name name)
  (format t "    int sts, i;~%")
  (format t "    if( glob.ocount <= 0 ) return -1;~%")
  (format t "    ~A_lock();~%" *prefix*)
  (format t "    sts = -1;~%")
  (format t "    for( i = 0; i < glob.file->header.~A_count; i++ ) {~%" name)
  (format t "        if( ~A ) {~%" (key-comp name key keytype
					     (format nil "~A->~A~%" name key)))
  (format t "            glob.file->~A[i] = *~A;~%" name name)
  (format t "            glob.file->header.seq++;~%")
  (format t "            sts = 0;~%")
  (format t "        }~%")
  (format t "    }~%")
  (format t "    ~A_unlock();~%" *prefix*)
  (format t "    return sts;~%")
  (format t "}~%"))

(defun emit-header-file (entries prop-fields)
  (format t "~%")
  (format t "#ifndef ~A_H~%" (string-upcase *prefix*))
  (format t "#define ~A_H~%" (string-upcase *prefix*))
  (format t "~%")
  (format t "#include <stdint.h>~%")
  (format t "~%")
  (dolist (entry entries)
    (destructuring-bind (name fields) entry 
      (emit-entry-struct name fields))
    (format t "~%"))
  (format t "~%")
  (emit-prop-struct (mapcar #'car entries) prop-fields)
  (format t "~%")
  (format t "int ~A_open( void );~%" *prefix*)
  (format t "int ~A_close( void );~%" *prefix*)
  (format t "int ~A_prop( struct ~A_prop *prop );~%" *prefix* *prefix*)
  (format t "int ~A_reset( void );~%" *prefix*)
  (format t "~%")
  (dolist (entry entries)
    (let ((name (first entry)))
      (format t "int ~A_~A_list( struct ~A_~A *list, int n );~%"
	      *prefix* name *prefix* name)
      (format t "int ~A_~A_by_tag( uint64_t tag, struct ~A_~A *entry );~%"
	      *prefix* name *prefix* name)
      (format t "int ~A_~A_add( struct ~A_~A *entry );~%"
	      *prefix* name *prefix* name)
      (format t "int ~A_~A_rem( uint64_t tag );~%" *prefix* name)
      (format t "int ~A_~A_set( struct ~A_~A *entry );~%"
	      *prefix* name *prefix* name))
    (format t "~%"))
  (format t "#endif~%")
  (format t "~%"))

(defun emit-code-file (entries extras)
  (format t "~%")
  (format t "include \"~A.h\"~%" *prefix*)
  (format t "~%")
  (dolist (entry entries)
    (format t "#define ~A_MAX_~A 32~%" (string-upcase *prefix*) (string-upcase entry)))
  (format t "~%")
  (emit-header-struct entries extras)
  (format t "~%")
  (emit-file-struct entries)
  (format t "~%")
  (format t "static struct {~%")
  (format t "    struct mmf_s mmf;~%")
  (format t "    int ocount;~%")
  (format t "    struct ~A_file *file;~%" *prefix*)
  (format t "} glob;~%")
  (format t "~%")
  (format t "static void ~A_lock( void ) {~%" *prefix*)
  (format t "    mmf_lock( &glob.mmf );~%")
  (format t "}~%")
  (format t "static void ~A_unlock( void ) {~%" *prefix*)
  (format t "    mmf_unlock( &glob.mmf );~%")
  (format t "}~%")
  (format t "~%")
  (emit-open entries)
  (format t "~%")
  (emit-close)
  (format t "~%")
  (emit-reset entries)
  (format t "~%")
  (emit-prop entries extras)
  (format t "~%")
  (dolist (entry entries)
    (format t "/* ------------ ~A commands ----------- */~%" entry)
    (format t "~%")
    (emit-entry-list entry)
    (format t "~%")
    (emit-entry-by-tag entry)
    (format t "~%")
    (emit-entry-add entry)
    (format t "~%")
    (emit-entry-rem entry "tag" :uint64)
    (format t "~%")))

    
  
;; ------------------------------------


(defun emit-files (&key prefix header-extras entries)
  (let ((*prefix* prefix))
    (with-output-to-file (*standard-output* (format nil "~A.h" prefix)
					    :direction :output
					    :if-exists :supersede)
      (emit-header-file entries header-extras))
    (with-output-to-file (*standard-output* (format nil "~A.c" prefix)
					    :direction :output
					    :if-exists :supersede)
      (emit-code-file entries header-extras))))


					    
					    
