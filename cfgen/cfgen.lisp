;;;; Copyright Frank James 2019

;;; This file can be used to generate skeleton and utility CLI codes.
;;; They are indended to be used as base files, to be later touched up by
;;; the programmer. It is not indended to form a part of the build system.

;; (gen ("fred") 
;;   (("a" :uint32)
;;    ("b" :uint32 16))
;;   ("entry1" ("a" :uint32) ("b" :uint64))
;;   ("entry2" ("tagggg" :string 12) ("xxx" :string 12)))


(defpackage #:cfgen
  (:use #:cl)
  (:export #:gen
	   #:emit-entry-list
	   #:emit-entry-by-key
	   #:emit-entry-add
	   #:emit-entry-rem
	   #:emit-entry-set
	   #:emit-entry-update
	   #:emit-entry-update-decl))

(in-package #:cfgen)

(defparameter *prefix* "cfgen")

(defun make-field (name type &optional array-length)
  (list (string-downcase (string name)) type array-length))
(defun field-name (field)
  (first field))
(defun field-name-upcase (field)
  (string-upcase (field-name field)))
(defun field-type (field)
  (second field))
(defun field-array-length (field)
  (third field))

(defun type-name (type-sym)
  (ecase type-sym
    ((:char :string) "char")
    (:uint8 "uint8_t")
    (:uint16 "uint16_t")
    (:uint32 "uint32_t")
    (:uint64 "uint64_t")
    (:int "int")
    (:int8 "int8_t")
    (:int16 "int16_t")
    (:int32 "int32_t")
    (:int64 "int64_t")))
(defun field-type-name (field)
  (type-name (field-type field)))

(defun make-entry (name fields)
  (list name fields))
(defun entry-name (entry)
  (first entry))
(defun entry-name-upcase (entry)
  (string-upcase (entry-name entry)))			  
(defun entry-fields (entry)
  (second entry))


(defun emit-header-struct (entries extras)
  (format t "struct ~A_header {~%" *prefix*)
  (format t "    uint32_t magic;~%")
  (format t "#define ~A_MAGIC 0x~8,'0X~%" (string-upcase *prefix*) (random #xffffffff))
  (format t "    uint32_t version;~%")
  (format t "    uint64_t seq;~%")
  (format t "~%")
  (format t "    /* entry counts */~%")
  (dolist (entry entries)
    (format t "    uint32_t ~A_max;~%" (entry-name entry))
    (format t "    uint32_t ~A_count;~%" (entry-name entry)))
  (format t "~%")
  (format t "    /* header fields */~%")
  (dolist (extra extras)
    (cond
      ((field-array-length extra)
       (format t "    ~A ~A[~A];~%"
	       (field-type-name extra)
	       (field-name extra)
	       (field-array-length extra)))
      (t (format t "    ~A ~A;~%" (field-type-name extra) (field-name extra)))))
  (format t "};~%"))

(defun emit-prop-struct (entries extras)
  (format t "struct ~A_prop {~%" *prefix*)
  (format t "    uint32_t version;~%")
  (format t "#define ~A_VERSION 1~%" (string-upcase *prefix*))
  (format t "    uint64_t seq;~%")
  (dolist (entry entries)
    (format t "    uint32_t ~A_max;~%" (entry-name entry))
    (format t "    uint32_t ~A_count;~%" (entry-name entry)))
  (dolist (extra extras)
    (cond
      ((entry-array-length extra)
       (format t "    ~A ~A[~A];~%"
	       (field-type-name extra)
	       (field-name extra)
	       (field-array-length extra)))
      (t (format t "    ~A ~A;~%" (field-type-name extra) (field-name extra)))))
  (format t "};~%"))

(defun emit-prop (entries extras)
  (format t "int ~A_prop( struct ~A_prop *prop ) {~%" *prefix* *prefix*)
  (format t "    if( glob.ocount <= 0 ) return -1;~%")
  (format t "    ~A_lock();~%" *prefix*)
  (format t "    prop->version = glob.file->header.version;~%")
  (format t "    prop->seq = glob.file->header.seq;~%")
  (dolist (entry entries)
    (format t "    prop->~A_max = glob.file->header.~A_max;~%" (entry-name entry) (entry-name entry))
    (format t "    prop->~A_count = glob.file->header.~A_count;~%" (entry-name entry) (entry-name entry)))
  (dolist (extra extras)
    (cond
      ((entry-array-length extra)
       (format t "    memcpy( prop->~A, glob.file->header.~A, sizeof(glob.file->header.~A[0]) * ~A );~%"
	       (field-name extra)
	       (field-name extra)
	       (field-name extra)
	       (field-array-length extra)))
      (t (format t "    prop->~A = glob.file->header.~A;~%" (field-name extra) (field-name extra)))))
  (format t "    ~A_unlock();~%" *prefix*)
  (format t "    return 0;~%")
  (format t "}~%"))
  
(defun emit-file-struct (entries)
  (format t "~%")
  (format t "struct ~A_file {~%" *prefix*)
  (format t "    struct ~A_header header;~%" *prefix*)
  (dolist (entry entries)
    (format t "    struct ~A_~A ~A[~A_MAX_~A];~%"
	    *prefix*
	    (entry-name entry)
	    (entry-name entry)
	    (string-upcase *prefix*)
	    (entry-name-upcase entry)))
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
    (format t "        glob.file->header.~A_max = ~A_MAX_~A;~%" (entry-name entry) (string-upcase *prefix*) (entry-name-upcase entry))
    (format t "        glob.file->header.~A_count = 0;~%" (entry-name entry)))
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
    (format t "    glob.file->header.~A_max = ~A_MAX_~A;~%" (entry-name entry) (string-upcase *prefix*) (entry-name-upcase entry))
    (format t "    glob.file->header.~A_count = 0;~%" (entry-name entry)))
  (format t "    ~A_unlock();~%" *prefix*)
  (format t "    return 0;~%")
  (format t "}~%"))

(defun emit-entry-struct (entry)
  (format t "struct ~A_~A {~%" *prefix* (entry-name entry))
  (format t "    uint64_t tag;~%")
  (dolist (field (entry-fields entry))
    (cond
      ((field-array-length field)
       (format t "    ~A ~A[~A];~%"
	       (field-type-name field)
	       (field-name field)
	       (field-array-length field)))
      (t (format t "    ~A ~A;~%" (field-type-name field) (field-name field)))))
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
  (format t "    int sts, i;~%")
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
  (format t "        ~A->tag = glob.file->header.seq;~%" name)
  (format t "        i = glob.file->header.~A_count;~%" name)
  (format t "        glob.file->~A[i] = *~A;~%" name name)
  (format t "        glob.file->header.~A_count++;~%" name)
  (format t "        glob.file->header.seq++;~%")
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
					     (format nil "~A->~A" name key)))
  (format t "            glob.file->~A[i] = *~A;~%" name name)
  (format t "            glob.file->header.seq++;~%")
  (format t "            sts = 0;~%")
  (format t "            break;~%")
  (format t "        }~%")
  (format t "    }~%")
  (format t "    ~A_unlock();~%" *prefix*)
  (format t "    return sts;~%")
  (format t "}~%"))

(defun emit-entry-update (name key keytype fields)
  (format t "int ~A_~A_update( struct ~A_~A *~A, int *fields, int nfields ) {~%"
	  *prefix* name *prefix* name name)
  (format t "    int sts, i, j;~%")
  (format t "    if( glob.ocount <= 0 ) return -1;~%")
  (format t "    ~A_lock();~%" *prefix*)
  (format t "    sts = -1;~%")
  (format t "    for( i = 0; i < glob.file->header.~A_count; i++ ) {~%" name)
  (format t "        if( ~A ) {~%" (key-comp name key keytype
					     (format nil "~A->~A" name key)))
  (format t "            for( j = 0; j < nfields; j++ ) {~%")
  (format t "                switch( fields[j] ) {~%")
  (dolist (field-name fields)
    (format t "                    case ~A_~A_UPDATE_~A:~%"
	    (string-upcase *prefix*) (string-upcase name) (string-upcase field-name))
    (format t "                        glob.file->~A[i].~A = ~A->~A~%"
	    name field-name name field-name)
    (format t "                        break;~%"))
  (format t "                }~%")
  (format t "            }~%")
  (format t "            glob.file->header.seq++;~%")
  (format t "            sts = 0;~%")
  (format t "            break;~%")
  (format t "        }~%")
  (format t "    }~%")
  (format t "    ~A_unlock();~%" *prefix*)
  (format t "    return sts;~%")
  (format t "}~%"))

(defun emit-entry-update-decl (name fields)
  (do ((i 1 (1+ i))
       (field-names fields (cdr field-names)))
      ((null field-names))
    (format t "#define ~A_~A_UPDATE_~A ~A~%" 
	    (string-upcase *prefix*)
	    (string-upcase name)
	    (string-upcase (string (car field-names)))
	    i))
  (format t "int ~A_~A_update( struct ~A_~A *~A, int *fields, int nfields );~%"
	  *prefix* name *prefix* name name))

(defun emit-header-file (entries prop-fields)
  (format t "~%")
  (format t "/*~%")
  (format t " * This file was generated by cfgen.lisp~%")
  (pprint `(gen (,*prefix*) ,prop-fields ,entries))
  (format t "~%~%")
  (format t " *~%")
  (format t " */~%")
  (format t "~%")
  (format t "#ifndef ~A_H~%" (string-upcase *prefix*))
  (format t "#define ~A_H~%" (string-upcase *prefix*))
  (format t "~%")
  (format t "#include <stdint.h>~%")
  (format t "~%")
  (dolist (entry entries)
    (emit-entry-struct entry)
    (format t "~%"))
  (format t "~%")
  (emit-prop-struct entries prop-fields)
  (format t "~%")
  (format t "int ~A_open( void );~%" *prefix*)
  (format t "int ~A_close( void );~%" *prefix*)
  (format t "int ~A_prop( struct ~A_prop *prop );~%" *prefix* *prefix*)
  (format t "int ~A_reset( void );~%" *prefix*)
  (format t "~%")
  (dolist (entry entries)
    (let ((name (entry-name entry)))
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
  (format t "#include \"~A.h\"~%" *prefix*)
  (format t "#include <stdint.h>~%")
  (format t "#include <stdlib.h>~%")
  (format t "#include <string.h>~%")
  (format t "#include <mmf.h>~%")
  (format t "~%")
  (dolist (entry entries)
    (format t "#define ~A_MAX_~A 32~%" (string-upcase *prefix*) (entry-name-upcase entry)))
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
    (format t "/* ------------ ~A commands ----------- */~%" (entry-name entry))
    (format t "~%")
    (emit-entry-list (entry-name entry))
    (format t "~%")
    (emit-entry-by-tag (entry-name entry))
    (format t "~%")
    (emit-entry-add (entry-name entry))
    (format t "~%")
    (emit-entry-rem (entry-name entry) "tag" :uint64)
    (format t "~%")
    (emit-entry-set (entry-name entry) "tag" :uint64)))

;; -------------------------------------------------------


(defun emit-usage (entries)
  (format t "static void usage( char *fmt, ... ) {~%")
  (format t "    printf( \"Usage:    prop\\n\" ~%")
  (dolist (entry entries)
    (format t "            \"          add ~A ~{[~A=~A] ~}]\\n\"~%"
	    (entry-name entry)
	    (mapcan (lambda (field)
		      (list (field-name field)
			    (field-name-upcase field)))
		    (entry-fields entry)))
    (format t "            \"          set ~A TAG ~{[~A=~A] ~}]\\n\"~%"
	    (entry-name entry)
	    (mapcan (lambda (field)
		      (list (field-name field)
			    (field-name-upcase field)))
		    (entry-fields entry)))
    (format t "            \"          rem ~A TAG\\n\"~%" (entry-name entry)))
  (format t "    );~%")
  (format t "~%")
  (format t "    if( fmt ) {~%")
  (format t "        va_list args;~%")
  (format t "        printf( \"Error: \" );~%")
  (format t "        va_start( args, fmt );~%")
  (format t "        vprintf( fmt, args );~%")
  (format t "        va_end( args );~%")
  (format t "        printf( \"\\n\" );~%")
  (format t "    }~%")
  (format t "    exit( 0 );~%")
  (format t "}~%"))

(defun emit-cmd-decl (name)
  (format t "static void cmd_~A( void );~%" name))

(defun emit-cmd-definition (name)
  (format t "static void cmd_~A( void ) {~%" name)
  (format t "    printf( \"DO thing for ~A\\n\"~%" name)
  (format t "}~%"))

(defun emit-entry-set-parser (field)
  (format t "if( strcmp( argname, \"~A\" ) == 0 ) {~%" (field-name field))
  (cond
    ((eq (field-type field) :string)
     (format t "    strncpy( entry.~A, argval, sizeof(entry.~A) - 1 );~%"
	     (field-name field) (field-name field)))
    ((eq (field-type field) :uint64)
     (format t "    entry.~A = strtoull( argval, NULL, 10 );~%"
	     (field-name field)))
    (t 
     (format t "    entry.~A = strtoul( argval, NULL, 10 );~%"
	       (field-name field))))
  (format t "}~%"))

(defun emit-cmd-list (entries)
  (format t "static void cmd_list( void ) {~%")
  (format t "    int sts, i, n, m;~%")
  (dolist (entry entries)
    (format t "    {~%")
    (format t "        struct ~A_~A *lst;~%" *prefix* (entry-name entry))
    (format t "        n = ~A_~A_list( NULL, 0 );~%" *prefix* (entry-name entry))
    (format t "        lst = (struct ~A_~A *)malloc( sizeof(*lst) * n );~%"
	    *prefix* (entry-name entry))
    (format t "        m = ~A_~A_list( lst, n );~%" *prefix* (entry-name entry))
    (format t "        if( m < n ) n = m;~%")
    (format t "        for( i = 0; i < n; i++ ) {~%")
    (format t "            printf( \"%-16s %-4d: TAG=%\"PRIx64\" ~{~A=%~A ~}\\n\", \"~A\", i, lst[i].tag~{, lst[i].~A~} );~%" 
	    (mapcan (lambda (field)
		      (list (field-name field)
			    (case (field-type field)
			      (:string "s")
			      (:uint64 "\"PRIu64\"")
			      (otherwise "d"))))
		    (entry-fields entry))
	    (entry-name entry)
	    (mapcar #'field-name (entry-fields entry)))
    (format t "        }~%")
    (format t "        free( lst );~%")
    (format t "        printf( \"\\n\" );~%")
    (format t "    }~%"))
  (format t "}~%"))

(defun emit-cmd-prop (entries extras)
  (format t "static void cmd_prop( void ) {~%")
  (format t "     struct ~A_prop prop;~%" *prefix*)
  (format t "     ~A_prop( &prop );~%" *prefix*)
  (format t "     printf( \"seq=%\"PRIu64\"\\n\", prop.seq );~%")
  (dolist (entry entries)
    (format t "     printf( \"~A=%d / %d\\n\", prop.~A_count, prop.~A_max );~%"
	    (entry-name entry) (entry-name entry) (entry-name entry)))
  (dolist (extra extras)
    (format t "     printf( \"~A=%~A\\n\", prop.~A );~%"
	    (field-name extra)
	    (if (eq (field-type extra) :string) "s" "d")
	    (field-name extra)))
  (format t "}~%"))

(defun emit-argval-split ()
  (format t "static void argval_split( char *instr, char *argname, char **argval ) {~%")
  (format t "    char *p;~%")
  (format t "~%")
  (format t "    p = strchr( instr, '=' );~%")
  (format t "    if( p ) *argval = p + 1;~%")
  (format t "    else *argval = NULL;~%")
  (format t "~%")
  (format t "    p = instr;~%")
  (format t "    while( *p != '0' && *p != '=' ) {~%")
  (format t "        *argname = *p;~%")
  (format t "        p++;~%")
  (format t "        argname++;~%")
  (format t "    }~%")
  (format t "    *argname = '\\0';~%")
  (format t "}~%"))
  
(defun emit-main (entries extras)
  (labels ((emit-set-parser (entry)
	   (format t "            while( i < argc ) {~%")
	   (format t "                 argval_split( argv[i], argname, &argval );~%")
	   (let ((field (car (entry-fields entry))))
	     (format t "                 if( strcmp( argname, \"~A\" ) == 0 ) {~%" (field-name field))
	     (case (field-type field)
	       (:string (format t "                      if( argval ) strncpy( entry.~A, argval, sizeof(entry.~A) );~%"
				(field-name field)
				(field-name field)))
	       (:uint64 (format t "                      if( argval ) entry.~A = strtoull( argval, NULL, 10 );~%" (field-name field)))
	       (otherwise (format t "                      if( argval ) entry.~A = strtoul( argval, NULL, 10 );~%" (field-name field)))))
	   (dolist (field (cdr (entry-fields entry)))
	     (format t "                } else if( strcmp( argname, \"~A\" ) == 0 ) {~%" (field-name field))
	     (case (field-type field)
	       (:string (format t "                      if( argval ) strncpy( entry.~A, argval, sizeof(entry.~A) );~%" (field-name field) (field-name field)))
	       (:uint64 (format t "                      if( argval ) entry.~A = strtoull( argval, NULL, 10 );~%" (field-name field)))
	       (otherwise (format t "                      if( argval ) entry.~A = strtoul( argval, NULL, 10 );~%" (field-name field)))))
	   (format t "                 } else { printf( \"Unknown field name %s\\n\", argname ); usage( NULL ); }~%")
	   (format t "                 i++;~%")
	   (format t "            }~%"))	   
	 (emit-add-body (entry)
	   (format t "            struct ~A_~A entry;~%" *prefix* (entry-name entry))
	   (format t "            char argname[64], *argval;~%")
	   (format t "            memset( &entry, 0, sizeof(entry) );~%")
	   (format t "            i++;~%")
	   (emit-set-parser entry)
	   (format t "            sts = ~A_~A_add( &entry );~%" *prefix* (entry-name entry))
	   (format t "            if( sts ) usage( \"Failed to add ~A\" );~%" (entry-name entry))
	   (format t "            printf( \"Added ~A TAG=%\"PRIx64\"\\n\", entry.tag );~%" (entry-name entry)))
	 (emit-set-body (entry)
	   (format t "            struct ~A_~A entry;~%" *prefix* (entry-name entry))
	   (format t "            char argname[64], *argval;~%")
	   (format t "            memset( &entry, 0, sizeof(entry) );~%")
	   (format t "            i++;~%")
	   (format t "            if( i >= argc ) usage( NULL );~%")
	   (format t "            tag = strtoull( argv[i], NULL, 16 );~%")
	   (format t "            sts = ~A_~A_by_tag( tag, &entry );~%" *prefix* (entry-name entry))
	   (format t "            if( sts ) usage( \"Failed to lookup\" );~%")
	   (format t "            i++;~%")
	   (emit-set-parser entry)
	   (format t "            sts = ~A_~A_set( &entry );~%" *prefix* (entry-name entry))
	   (format t "            if( sts ) usage( \"Failed to set ~A\" );~%" (entry-name entry)))
	 (emit-rem-body (entry)
	   (format t "            uint64_t tag;~%")
	   (format t "            i++;~%")
	   (format t "            if( i >= argc ) usage( NULL );~%")
	   (format t "            tag = strtoull( argv[i], NULL, 16 );~%")
	   (format t "            sts = ~A_~A_rem( tag );~%" *prefix* (entry-name entry))
	   (format t "            if( sts ) usage( \"Failed to rem ~A\" );~%" (entry-name entry))))

    (format t "~%")
    (format t "#include <stdlib.h>~%")
    (format t "#include <stdio.h>~%")
    (format t "#include <string.h>~%")
    (format t "#include <stdint.h>~%")
    (format t "#include <stdarg.h>~%")
    (format t "#include <inttypes.h>~%")
    (format t "#include <mmf.h>~%")
    (format t "#include \"~A.h\"~%" *prefix*)
    (format t "~%")
    (emit-usage entries)
    (format t "~%")
    (emit-argval-split)
    (format t "~%")
    (format t "static void cmd_list( void );~%")
    (format t "static void cmd_prop( void );~%")
    (format t "~%")
    (format t "int main( int argc, char **argv ) {~%")
    (format t "    int sts, i;~%")
    (format t "~%")
    (format t "    sts = ~A_open();~%" *prefix*)
    (format t "    if( sts ) usage( \"Failed to open\" );~%")
    (format t "~%")
    (format t "    i = 1;~%")
    (format t "    if( i >= argc ) {~%")
    (format t "        cmd_list();~%")
    (format t "    } else if( strcmp( argv[i], \"list\" ) == 0 ) {~%")
    (format t "        cmd_list();~%")
    (format t "    } else if( strcmp( argv[i], \"prop\" ) == 0 ) {~%")
    (format t "        cmd_prop();~%")
    (format t "    } else if( strcmp( argv[i], \"add\" ) == 0 ) {~%")
    (format t "        i++;~%")
    (format t "        if( i >= argc ) usage( NULL );~%")
    (format t "        if( strcmp( argv[i], \"~A\" ) == 0 ) {~%" (entry-name (first entries)))
    (emit-add-body (first entries))
    (dolist (entry (cdr entries))
      (format t "        } else if( strcmp( argv[i], \"~A\" ) == 0 ) {~%" (entry-name entry))
      (emit-add-body entry))
    (format t "        } else usage( NULL );~%")
    (format t "    } else if( strcmp( argv[i], \"rem\" ) == 0 ) {~%")
    (format t "        i++;~%")
    (format t "        if( i >= argc ) usage( NULL );~%")
    (format t "        if( strcmp( argv[i], \"~A\" ) == 0 ) {~%" (entry-name (first entries)))
    (emit-rem-body (first entries))
    (dolist (entry (cdr entries))
      (format t "        } else if( strcmp( argv[i], \"~A\" ) == 0 ) {~%" (entry-name entry))
      (emit-rem-body entry))
    (format t "        } else usage( NULL );~%")
    (format t "    } else if( strcmp( argv[i], \"set\" ) == 0 ) {~%")
    (format t "        uint64_t tag;~%")
    (format t "        i++;~%")
    (format t "        if( i >= argc ) usage( NULL );~%")
    (format t "        if( strcmp( argv[i], \"~A\" ) == 0 ) {~%" (entry-name (first entries)))
    (emit-set-body (first entries))
    (dolist (entry (cdr entries))
      (format t "        } else if( strcmp( argv[i], \"~A\" ) == 0 ) {~%" (entry-name entry))
      (emit-set-body entry))
    (format t "        } else usage( NULL );~%")
    (format t "    } else usage( NULL );~%")
    (format t "~%")
    (format t "    ~A_close();~%" *prefix*)
    (format t "    return 0;~%")
    (format t "}~%")
    (format t "~%")
    (emit-cmd-list entries)
    (format t "~%")
    (emit-cmd-prop entries extras)
    (format t "~%")))

  
;; ------------------------------------


(defun emit-files (&key prefix header-extras entries)
  (let ((*prefix* prefix))
    (with-open-file (*standard-output* (format nil "~A.h" prefix)
					    :direction :output
					    :if-exists :supersede)
      (emit-header-file entries header-extras))
    (with-open-file (*standard-output* (format nil "~A.c" prefix)
					    :direction :output
					    :if-exists :supersede)
      (emit-code-file entries header-extras))
    (with-open-file (*standard-output* (format nil "~A-main.c" prefix)
				       :direction :output
				       :if-exists :supersede)
      (emit-main entries header-extras))))

(defun emit-files-stdout (&key prefix header-extras entries)
  (let ((*prefix* prefix))
    (emit-header-file entries header-extras)
    (emit-code-file entries header-extras)
    (emit-main entries header-extras)))

(defmacro gen ((prefix &optional stdoutp) header-extras &rest entries)
  `(,(if stdoutp 'emit-files-stdout 'emit-files)
		      :prefix ,(string-downcase (string prefix))
		      :header-extras (list ,@(mapcar (lambda (extra)
						       (destructuring-bind (name type &optional array-length) extra
							 `(make-field ,(string-downcase (string name)) ,type ,array-length)))
						     header-extras))
		      :entries (list ,@(mapcar (lambda (entry)
						 (destructuring-bind (entry-name &rest fields) entry
						   `(make-entry ,(string-downcase (string entry-name))
								(list
								 ,@(mapcar (lambda (field)
									     (destructuring-bind (field-name field-type &optional field-array-length) field
									       `(make-field ,(string-downcase (string field-name)) ,field-type ,field-array-length)))
									   fields)))))
					       entries))))


;; (cfgen::gen ("fred" t) 
;; 	    (("a" :uint32)
;; 	     ("b" :uint32 16))
;; 	    ("entry1" ("a" :uint32) ("b" :uint64))
;; 	    ("entry2" ("tag" :string 12) ("xxx" :string 12)))

					    
;; (gen (fcl t) 
;; 	    ((localid :uint64))
;; 	    (node 
;; 	     (nodeid :uint64)
;; 	     (name :string 64)
;; 	     (naddr :uint32)
;; 	     (addr :uint32 8))
;; 	    (cluster 
;; 	     (clid :uint64)
;; 	     (state :uint32)
;; 	     (seq :uint64)
;; 	     (leader :uint64))
;; 	    (member 
;; 	     (nodeid :uint64)
;; 	     (clid :uint64)))
					    
;; -----------------------------------------------
