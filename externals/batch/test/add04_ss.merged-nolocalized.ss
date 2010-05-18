(module add04_ss.merged '#%kernel
  (define stx11470
    (#%decode-syntax
     #(struct:wrapped
       context
       (#(struct:phase-shift
          0
          (module-path-index-join '#f #f)
          (module-path-index-join '#f #f))
        #(struct:module-rename
          2
          normal
          3760270
          (#(struct:all-from-module
             (module-path-index-join 'scheme #f)
             2
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'scheme #f)
             2
             0
             ()
             #f))
          ()
          ()
          #f)
        #(struct:module-rename #f normal 3760270 () () () #f)
        #(struct:module-rename
          1
          normal
          3760270
          (#(struct:all-from-module
             (module-path-index-join 'scheme #f)
             1
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'scheme #f)
             1
             0
             ()
             #f))
          ()
          ()
          #f)
        #(struct:module-rename
          0
          normal
          3760270
          (#(struct:all-from-module
             (module-path-index-join 'mzlib/runtime-path #f)
             0
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'mzlib/port #f)
             0
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'mzlib/foreign #f)
             0
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'scheme #f)
             0
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'scheme #f)
             0
             0
             ()
             #f))
          ((wrap-ports
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              wrap-ports
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv2_server_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv2_server_method
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_accept
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_accept
              (module-path-index-join '#f #f)
              *
              #t))
           (raise-not-available
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              raise-not-available
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-...
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-...
              (module-path-index-join '#f #f)
              *
              #t))
           (_SSL*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _SSL*
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_connect
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_connect
              (module-path-index-join '#f #f)
              *
              #t))
           (_BIO_METHOD*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _BIO_METHOD*
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_set_bio
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_set_bio
              (module-path-index-join '#f #f)
              *
              #t))
           (ports->ssl-ports
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ports->ssl-ports
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_new
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_new
              (module-path-index-join '#f #f)
              *
              #t))
           (create-ssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              create-ssl
              (module-path-index-join '#f #f)
              *
              #t))
           (atomically
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              atomically
              (module-path-index-join '#f #f)
              *
              #t))
           (define-mzscheme
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-mzscheme
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_load_client_CA_file
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_load_client_CA_file
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_use_PrivateKey_file
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_use_PrivateKey_file
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-output-port
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-output-port
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_use_RSAPrivateKey_file
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_use_RSAPrivateKey_file
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-available?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-available?
              (module-path-index-join '#f #f)
              *
              #t))
           (exn:atomic?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              exn:atomic?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-exn:atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-exn:atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_set_client_CA_list
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_set_client_CA_list
              (module-path-index-join '#f #f)
              *
              #t))
           (exn:atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              exn:atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (exn:atomic-thunk
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              exn:atomic-thunk
              (module-path-index-join '#f #f)
              *
              #t))
           (_X509_NAME*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _X509_NAME*
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_load_verify_locations
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_load_verify_locations
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-input-port
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-input-port
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-connect
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-connect
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_use_certificate_chain_file
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_use_certificate_chain_file
              (module-path-index-join '#f #f)
              *
              #t))
           (pump-output
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              pump-output
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-close
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-close
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_set_verify
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_set_verify
              (module-path-index-join '#f #f)
              *
              #t))
           (pump-output-once
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              pump-output-once
              (module-path-index-join '#f #f)
              *
              #t))
           (do-ssl-connect
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              do-ssl-connect
              (module-path-index-join '#f #f)
              *
              #t))
           (encrypt->method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              encrypt->method
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_set_mode
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_set_mode
              (module-path-index-join '#f #f)
              *
              #t))
           (pump-input-once
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              pump-input-once
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listen
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listen
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_ctrl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_ctrl
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-release
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-release
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-accept/enable-break
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-accept/enable-break
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_free
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_free
              (module-path-index-join '#f #f)
              *
              #t))
           (default-encrypt
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              default-encrypt
              (module-path-index-join '#f #f)
              *
              #t))
           (in-atomic?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              in-atomic?
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-set-verify!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-set-verify!
              (module-path-index-join '#f #f)
              *
              #t))
           (error/network
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              error/network
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-private-key!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-private-key!
              (module-path-index-join '#f #f)
              *
              #t))
           (_SSL_CTX*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _SSL_CTX*
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_set_mem_eof_return
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_set_mem_eof_return
              (module-path-index-join '#f #f)
              *
              #t))
           (flush-ssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              flush-ssl
              (module-path-index-join '#f #f)
              *
              #t))
           (check-valid
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              check-valid
              (module-path-index-join '#f #f)
              *
              #t))
           (do-ssl-accept
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              do-ssl-accept
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:exn:atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:exn:atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_ctrl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_ctrl
              (module-path-index-join '#f #f)
              *
              #t))
           (get-error-message
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              get-error-message
              (module-path-index-join '#f #f)
              *
              #t))
           (define-define-X
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-define-X
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_write
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_write
              (module-path-index-join '#f #f)
              *
              #t))
           (libmz
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libmz
              (module-path-index-join '#f #f)
              *
              #t))
           (BUFFER-SIZE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BUFFER-SIZE
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_read
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_read
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listener?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listener?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-listener
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-listener
              (module-path-index-join '#f #f)
              *
              #t))
           (libcrypto
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libcrypto
              (module-path-index-join '#f #f)
              *
              #t))
           (3m?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              3m?
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_free
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_free
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listener-l
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listener-l
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:ssl-listener
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:ssl-listener
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-fail-reason
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-fail-reason
              (module-path-index-join '#f #f)
              *
              #t))
           (cpointer-push-tag!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cpointer-push-tag!
              (module-path-index-join '#f #f)
              *
              #t))
           (cpointer-has-tag?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cpointer-has-tag?
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_new
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_new
              (module-path-index-join '#f #f)
              *
              #t))
           (define-c
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-c
              (module-path-index-join '#f #f)
              *
              #t))
           (get-ffi-obj
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              get-ffi-obj
              (module-path-index-join '#f #f)
              *
              #t))
           (get-context/listener
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              get-context/listener
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_s_mem
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_s_mem
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_new
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_new
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-abandon-port
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-abandon-port
              (module-path-index-join '#f #f)
              *
              #t))
           (get-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              get-context
              (module-path-index-join '#f #f)
              *
              #t))
           (TLSv1_server_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              TLSv1_server_method
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-addresses
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-addresses
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-make-server-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-make-server-context
              (module-path-index-join '#f #f)
              *
              #t))
           (TLSv1_client_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              TLSv1_client_method
              (module-path-index-join '#f #f)
              *
              #t))
           (typedef
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              typedef
              (module-path-index-join '#f #f)
              *
              #t))
           (libssl-so
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libssl-so
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-make-client-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-make-client-context
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv23_server_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv23_server_method
              (module-path-index-join '#f #f)
              *
              #t))
           (make-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-context
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv23_client_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv23_client_method
              (module-path-index-join '#f #f)
              *
              #t))
           (define-crypto
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-crypto
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv3_server_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv3_server_method
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv3_client_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv3_client_method
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-server-context?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-server-context?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-server-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-server-context
              (module-path-index-join '#f #f)
              *
              #t))
           (set-ffi-obj!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-ffi-obj!
              (module-path-index-join '#f #f)
              *
              #t))
           (make-immobile-bytes
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-immobile-bytes
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-server-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-server-context
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:ssl-server-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:ssl-server-context
              (module-path-index-join '#f #f)
              *
              #t))
           (register
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              register
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listener
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listener
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv2_client_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv2_client_method
              (module-path-index-join '#f #f)
              *
              #t))
           (_SSL_METHOD*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _SSL_METHOD*
              (module-path-index-join '#f #f)
              *
              #t))
           (make-c-parameter
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-c-parameter
              (module-path-index-join '#f #f)
              *
              #t))
           (define-ssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-ssl
              (module-path-index-join '#f #f)
              *
              #t))
           (ffi-obj-ref
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ffi-obj-ref
              (module-path-index-join '#f #f)
              *
              #t))
           (libcrypto-so
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libcrypto-so
              (module-path-index-join '#f #f)
              *
              #t))
           (cblock->vector
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cblock->vector
              (module-path-index-join '#f #f)
              *
              #t))
           (cblock->list
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cblock->list
              (module-path-index-join '#f #f)
              *
              #t))
           (make-mzssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-mzssl
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-error
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-error
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-client-context?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-client-context?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-client-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-client-context
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-client-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-client-context
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:ssl-client-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:ssl-client-context
              (module-path-index-join '#f #f)
              *
              #t))
           (with-failure
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              with-failure
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-close-original?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-close-original?!
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-r-bio
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-r-bio
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listener-mzctx
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listener-mzctx
              (module-path-index-join '#f #f)
              *
              #t))
           (enforce-retry?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              enforce-retry?
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-flushing?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-flushing?!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_VERIFY_NONE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_VERIFY_NONE
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-w-closed?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-w-closed?!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-buffer!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-buffer!
              (module-path-index-join '#f #f)
              *
              #t))
           (scheme_make_custodian
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              scheme_make_custodian
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_FILETYPE_ASN1
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_FILETYPE_ASN1
              (module-path-index-join '#f #f)
              *
              #t))
           (kernel-thread
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              kernel-thread
              (module-path-index-join '#f #f)
              *
              #t))
           (scheme_end_atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              scheme_end_atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-must-read
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-must-read
              (module-path-index-join '#f #f)
              *
              #t))
           (scheme_start_atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              scheme_start_atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-context?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-context?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-context
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_ERROR_ZERO_RETURN
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_ERROR_ZERO_RETURN
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTRL_MODE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTRL_MODE
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-context
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-context-ctx
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-context-ctx
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:ssl-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:ssl-context
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-o
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-o
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_MODE_ENABLE_PARTIAL_WRITE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_MODE_ENABLE_PARTIAL_WRITE
              (module-path-index-join '#f #f)
              *
              #t))
           (make-cvector*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-cvector*
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_VERIFY_FAIL_IF_NO_PEER_CERT
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_VERIFY_FAIL_IF_NO_PEER_CERT
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-connect/enable-break
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-connect/enable-break
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-error!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-error!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-finalizer-cancel!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-finalizer-cancel!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-shutdown-on-close?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-shutdown-on-close?!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_VERIFY_PEER
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_VERIFY_PEER
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-refcount!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-refcount!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-must-read!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-must-read!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-must-write!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-must-write!
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:mzssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:mzssl
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-r-closed?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-r-closed?!
              (module-path-index-join '#f #f)
              *
              #t))
           (_BIO*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _BIO*
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-lock!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-lock!
              (module-path-index-join '#f #f)
              *
              #t))
           (ptr-set!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ptr-set!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-pipe-w!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-pipe-w!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-pipe-r!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-pipe-r!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-w-bio!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-w-bio!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-r-bio!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-r-bio!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-o!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-o!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-i!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-i!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_FILETYPE_PEM
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_FILETYPE_PEM
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-finalizer-cancel
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-finalizer-cancel
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-shutdown-on-close?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-shutdown-on-close?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-close-original?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-close-original?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-refcount
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-refcount
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_C_SET_BUF_MEM_EOF_RETURN
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_C_SET_BUF_MEM_EOF_RETURN
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-must-write
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-must-write
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-flushing?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-flushing?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-r-closed?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-r-closed?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-w-closed?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-w-closed?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-lock
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-lock
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-buffer
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-buffer
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-pipe-w
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-pipe-w
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-pipe-r
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-pipe-r
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-w-bio
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-w-bio
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_ERROR_SYSCALL
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_ERROR_SYSCALL
              (module-path-index-join '#f #f)
              *
              #t))
           (escape-atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              escape-atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-i
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-i
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl?
              (module-path-index-join '#f #f)
              *
              #t))
           (set-cpointer-tag!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-cpointer-tag!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_ERROR_WANT_WRITE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_ERROR_WANT_WRITE
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-accept
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-accept
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_ERROR_WANT_READ
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_ERROR_WANT_READ
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-ssl!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-ssl!
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-ssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-ssl
              (module-path-index-join '#f #f)
              *
              #t))
           (make-sized-byte-string
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-sized-byte-string
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_load_error_strings
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_load_error_strings
              (module-path-index-join '#f #f)
              *
              #t))
           (ptr-ref
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ptr-ref
              (module-path-index-join '#f #f)
              *
              #t))
           (memcpy
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              memcpy
              (module-path-index-join '#f #f)
              *
              #t))
           (memmove
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              memmove
              (module-path-index-join '#f #f)
              *
              #t))
           (memset
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              memset
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_library_init
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_library_init
              (module-path-index-join '#f #f)
              *
              #t))
           (free-immobile-cell
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              free-immobile-cell
              (module-path-index-join '#f #f)
              *
              #t))
           (malloc-immobile-cell
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              malloc-immobile-cell
              (module-path-index-join '#f #f)
              *
              #t))
           (free
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              free
              (module-path-index-join '#f #f)
              *
              #t))
           (end-stubborn-change
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              end-stubborn-change
              (module-path-index-join '#f #f)
              *
              #t))
           (malloc
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              malloc
              (module-path-index-join '#f #f)
              *
              #t))
           (ERR_error_string_n
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ERR_error_string_n
              (module-path-index-join '#f #f)
              *
              #t))
           (cpointer-tag
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cpointer-tag
              (module-path-index-join '#f #f)
              *
              #t))
           (ERR_get_error
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ERR_get_error
              (module-path-index-join '#f #f)
              *
              #t))
           (ffi-lib
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ffi-lib
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_get_error
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_get_error
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-suggested-certificate-authorities!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-suggested-certificate-authorities!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_shutdown
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_shutdown
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_write
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_write
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-verify-root-certificates!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-verify-root-certificates!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_read
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_read
              (module-path-index-join '#f #f)
              *
              #t))
           (libssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libssl
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-certificate-chain!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-certificate-chain!
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-ports
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-ports
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_free
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_free
              (module-path-index-join '#f #f)
              *
              #t))
           (lookup
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              lookup
              (module-path-index-join '#f #f)
              *
              #t)))
          (#(free
             #<module-path-index>
             end-stubborn-change
             #<module-path-index>
             malloc
             #<module-path-index>
             set-cpointer-tag!
             #<module-path-index>
             cpointer-tag
             #<module-path-index>
             make-c-parameter
             #<module-path-index>
             cpointer-push-tag!
             #<module-path-index>
             cpointer-has-tag?
             #<module-path-index>
             ffi-lib
             (#<module-path-index> get-ffi-lib #<module-path-index> . ffi-lib)
             define-c
             #<module-path-index>
             ffi-obj-ref
             #<module-path-index>
             cblock->vector
             #<module-path-index>
             cblock->list
             #<module-path-index>
             make-sized-byte-string
             #<module-path-index>
             ptr-set!
             #<module-path-index>
             ptr-ref
             #<module-path-index>
             memcpy
             #<module-path-index>
             memmove
             #<module-path-index>
             memset
             #<module-path-index>
             set-ffi-obj!
             #<module-path-index>
             get-ffi-obj
             #<module-path-index>
             make-cvector*
             (#<module-path-index>
              make-cvector
              #<module-path-index>
              .
              make-cvector*)
             free-immobile-cell
             #<module-path-index>
             malloc-immobile-cell
             #<module-path-index>))
          #f)
        #(struct:lexical-rename ((libcrypto-so . env928679)))
        mark-3761984)
       (() -3761984 #<module-path-index>))))
  (define stx11471
    (#%decode-syntax
     #(struct:wrapped
       tag
       (#(struct:phase-shift
          0
          (module-path-index-join '#f #f)
          (module-path-index-join '#f #f))
        #(struct:module-rename
          2
          normal
          3760270
          (#(struct:all-from-module
             (module-path-index-join 'scheme #f)
             2
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'scheme #f)
             2
             0
             ()
             #f))
          ()
          ()
          #f)
        #(struct:module-rename #f normal 3760270 () () () #f)
        #(struct:module-rename
          1
          normal
          3760270
          (#(struct:all-from-module
             (module-path-index-join 'scheme #f)
             1
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'scheme #f)
             1
             0
             ()
             #f))
          ()
          ()
          #f)
        #(struct:module-rename
          0
          normal
          3760270
          (#(struct:all-from-module
             (module-path-index-join 'mzlib/runtime-path #f)
             0
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'mzlib/port #f)
             0
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'mzlib/foreign #f)
             0
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'scheme #f)
             0
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'scheme #f)
             0
             0
             ()
             #f))
          ((wrap-ports
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              wrap-ports
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv2_server_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv2_server_method
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_accept
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_accept
              (module-path-index-join '#f #f)
              *
              #t))
           (raise-not-available
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              raise-not-available
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-...
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-...
              (module-path-index-join '#f #f)
              *
              #t))
           (_SSL*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _SSL*
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_connect
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_connect
              (module-path-index-join '#f #f)
              *
              #t))
           (_BIO_METHOD*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _BIO_METHOD*
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_set_bio
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_set_bio
              (module-path-index-join '#f #f)
              *
              #t))
           (ports->ssl-ports
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ports->ssl-ports
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_new
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_new
              (module-path-index-join '#f #f)
              *
              #t))
           (create-ssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              create-ssl
              (module-path-index-join '#f #f)
              *
              #t))
           (atomically
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              atomically
              (module-path-index-join '#f #f)
              *
              #t))
           (define-mzscheme
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-mzscheme
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_load_client_CA_file
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_load_client_CA_file
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_use_PrivateKey_file
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_use_PrivateKey_file
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-output-port
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-output-port
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_use_RSAPrivateKey_file
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_use_RSAPrivateKey_file
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-available?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-available?
              (module-path-index-join '#f #f)
              *
              #t))
           (exn:atomic?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              exn:atomic?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-exn:atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-exn:atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_set_client_CA_list
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_set_client_CA_list
              (module-path-index-join '#f #f)
              *
              #t))
           (exn:atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              exn:atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (exn:atomic-thunk
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              exn:atomic-thunk
              (module-path-index-join '#f #f)
              *
              #t))
           (_X509_NAME*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _X509_NAME*
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_load_verify_locations
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_load_verify_locations
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-input-port
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-input-port
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-connect
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-connect
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_use_certificate_chain_file
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_use_certificate_chain_file
              (module-path-index-join '#f #f)
              *
              #t))
           (pump-output
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              pump-output
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-close
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-close
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_set_verify
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_set_verify
              (module-path-index-join '#f #f)
              *
              #t))
           (pump-output-once
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              pump-output-once
              (module-path-index-join '#f #f)
              *
              #t))
           (do-ssl-connect
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              do-ssl-connect
              (module-path-index-join '#f #f)
              *
              #t))
           (encrypt->method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              encrypt->method
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_set_mode
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_set_mode
              (module-path-index-join '#f #f)
              *
              #t))
           (pump-input-once
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              pump-input-once
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listen
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listen
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_ctrl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_ctrl
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-release
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-release
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-accept/enable-break
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-accept/enable-break
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_free
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_free
              (module-path-index-join '#f #f)
              *
              #t))
           (default-encrypt
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              default-encrypt
              (module-path-index-join '#f #f)
              *
              #t))
           (in-atomic?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              in-atomic?
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-set-verify!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-set-verify!
              (module-path-index-join '#f #f)
              *
              #t))
           (error/network
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              error/network
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-private-key!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-private-key!
              (module-path-index-join '#f #f)
              *
              #t))
           (_SSL_CTX*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _SSL_CTX*
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_set_mem_eof_return
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_set_mem_eof_return
              (module-path-index-join '#f #f)
              *
              #t))
           (flush-ssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              flush-ssl
              (module-path-index-join '#f #f)
              *
              #t))
           (check-valid
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              check-valid
              (module-path-index-join '#f #f)
              *
              #t))
           (do-ssl-accept
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              do-ssl-accept
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:exn:atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:exn:atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_ctrl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_ctrl
              (module-path-index-join '#f #f)
              *
              #t))
           (get-error-message
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              get-error-message
              (module-path-index-join '#f #f)
              *
              #t))
           (define-define-X
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-define-X
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_write
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_write
              (module-path-index-join '#f #f)
              *
              #t))
           (libmz
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libmz
              (module-path-index-join '#f #f)
              *
              #t))
           (BUFFER-SIZE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BUFFER-SIZE
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_read
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_read
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listener?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listener?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-listener
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-listener
              (module-path-index-join '#f #f)
              *
              #t))
           (libcrypto
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libcrypto
              (module-path-index-join '#f #f)
              *
              #t))
           (3m?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              3m?
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_free
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_free
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listener-l
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listener-l
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:ssl-listener
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:ssl-listener
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-fail-reason
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-fail-reason
              (module-path-index-join '#f #f)
              *
              #t))
           (cpointer-push-tag!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cpointer-push-tag!
              (module-path-index-join '#f #f)
              *
              #t))
           (cpointer-has-tag?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cpointer-has-tag?
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_new
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_new
              (module-path-index-join '#f #f)
              *
              #t))
           (define-c
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-c
              (module-path-index-join '#f #f)
              *
              #t))
           (get-ffi-obj
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              get-ffi-obj
              (module-path-index-join '#f #f)
              *
              #t))
           (get-context/listener
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              get-context/listener
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_s_mem
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_s_mem
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_new
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_new
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-abandon-port
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-abandon-port
              (module-path-index-join '#f #f)
              *
              #t))
           (get-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              get-context
              (module-path-index-join '#f #f)
              *
              #t))
           (TLSv1_server_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              TLSv1_server_method
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-addresses
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-addresses
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-make-server-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-make-server-context
              (module-path-index-join '#f #f)
              *
              #t))
           (TLSv1_client_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              TLSv1_client_method
              (module-path-index-join '#f #f)
              *
              #t))
           (typedef
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              typedef
              (module-path-index-join '#f #f)
              *
              #t))
           (libssl-so
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libssl-so
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-make-client-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-make-client-context
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv23_server_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv23_server_method
              (module-path-index-join '#f #f)
              *
              #t))
           (make-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-context
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv23_client_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv23_client_method
              (module-path-index-join '#f #f)
              *
              #t))
           (define-crypto
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-crypto
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv3_server_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv3_server_method
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv3_client_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv3_client_method
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-server-context?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-server-context?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-server-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-server-context
              (module-path-index-join '#f #f)
              *
              #t))
           (set-ffi-obj!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-ffi-obj!
              (module-path-index-join '#f #f)
              *
              #t))
           (make-immobile-bytes
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-immobile-bytes
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-server-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-server-context
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:ssl-server-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:ssl-server-context
              (module-path-index-join '#f #f)
              *
              #t))
           (register
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              register
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listener
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listener
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv2_client_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv2_client_method
              (module-path-index-join '#f #f)
              *
              #t))
           (_SSL_METHOD*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _SSL_METHOD*
              (module-path-index-join '#f #f)
              *
              #t))
           (make-c-parameter
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-c-parameter
              (module-path-index-join '#f #f)
              *
              #t))
           (define-ssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-ssl
              (module-path-index-join '#f #f)
              *
              #t))
           (ffi-obj-ref
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ffi-obj-ref
              (module-path-index-join '#f #f)
              *
              #t))
           (libcrypto-so
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libcrypto-so
              (module-path-index-join '#f #f)
              *
              #t))
           (cblock->vector
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cblock->vector
              (module-path-index-join '#f #f)
              *
              #t))
           (cblock->list
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cblock->list
              (module-path-index-join '#f #f)
              *
              #t))
           (make-mzssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-mzssl
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-error
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-error
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-client-context?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-client-context?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-client-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-client-context
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-client-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-client-context
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:ssl-client-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:ssl-client-context
              (module-path-index-join '#f #f)
              *
              #t))
           (with-failure
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              with-failure
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-close-original?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-close-original?!
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-r-bio
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-r-bio
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listener-mzctx
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listener-mzctx
              (module-path-index-join '#f #f)
              *
              #t))
           (enforce-retry?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              enforce-retry?
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-flushing?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-flushing?!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_VERIFY_NONE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_VERIFY_NONE
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-w-closed?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-w-closed?!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-buffer!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-buffer!
              (module-path-index-join '#f #f)
              *
              #t))
           (scheme_make_custodian
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              scheme_make_custodian
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_FILETYPE_ASN1
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_FILETYPE_ASN1
              (module-path-index-join '#f #f)
              *
              #t))
           (kernel-thread
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              kernel-thread
              (module-path-index-join '#f #f)
              *
              #t))
           (scheme_end_atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              scheme_end_atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-must-read
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-must-read
              (module-path-index-join '#f #f)
              *
              #t))
           (scheme_start_atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              scheme_start_atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-context?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-context?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-context
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_ERROR_ZERO_RETURN
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_ERROR_ZERO_RETURN
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTRL_MODE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTRL_MODE
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-context
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-context-ctx
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-context-ctx
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:ssl-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:ssl-context
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-o
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-o
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_MODE_ENABLE_PARTIAL_WRITE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_MODE_ENABLE_PARTIAL_WRITE
              (module-path-index-join '#f #f)
              *
              #t))
           (make-cvector*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-cvector*
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_VERIFY_FAIL_IF_NO_PEER_CERT
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_VERIFY_FAIL_IF_NO_PEER_CERT
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-connect/enable-break
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-connect/enable-break
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-error!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-error!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-finalizer-cancel!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-finalizer-cancel!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-shutdown-on-close?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-shutdown-on-close?!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_VERIFY_PEER
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_VERIFY_PEER
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-refcount!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-refcount!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-must-read!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-must-read!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-must-write!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-must-write!
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:mzssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:mzssl
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-r-closed?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-r-closed?!
              (module-path-index-join '#f #f)
              *
              #t))
           (_BIO*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _BIO*
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-lock!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-lock!
              (module-path-index-join '#f #f)
              *
              #t))
           (ptr-set!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ptr-set!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-pipe-w!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-pipe-w!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-pipe-r!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-pipe-r!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-w-bio!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-w-bio!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-r-bio!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-r-bio!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-o!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-o!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-i!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-i!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_FILETYPE_PEM
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_FILETYPE_PEM
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-finalizer-cancel
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-finalizer-cancel
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-shutdown-on-close?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-shutdown-on-close?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-close-original?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-close-original?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-refcount
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-refcount
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_C_SET_BUF_MEM_EOF_RETURN
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_C_SET_BUF_MEM_EOF_RETURN
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-must-write
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-must-write
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-flushing?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-flushing?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-r-closed?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-r-closed?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-w-closed?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-w-closed?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-lock
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-lock
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-buffer
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-buffer
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-pipe-w
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-pipe-w
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-pipe-r
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-pipe-r
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-w-bio
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-w-bio
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_ERROR_SYSCALL
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_ERROR_SYSCALL
              (module-path-index-join '#f #f)
              *
              #t))
           (escape-atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              escape-atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-i
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-i
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl?
              (module-path-index-join '#f #f)
              *
              #t))
           (set-cpointer-tag!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-cpointer-tag!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_ERROR_WANT_WRITE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_ERROR_WANT_WRITE
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-accept
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-accept
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_ERROR_WANT_READ
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_ERROR_WANT_READ
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-ssl!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-ssl!
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-ssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-ssl
              (module-path-index-join '#f #f)
              *
              #t))
           (make-sized-byte-string
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-sized-byte-string
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_load_error_strings
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_load_error_strings
              (module-path-index-join '#f #f)
              *
              #t))
           (ptr-ref
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ptr-ref
              (module-path-index-join '#f #f)
              *
              #t))
           (memcpy
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              memcpy
              (module-path-index-join '#f #f)
              *
              #t))
           (memmove
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              memmove
              (module-path-index-join '#f #f)
              *
              #t))
           (memset
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              memset
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_library_init
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_library_init
              (module-path-index-join '#f #f)
              *
              #t))
           (free-immobile-cell
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              free-immobile-cell
              (module-path-index-join '#f #f)
              *
              #t))
           (malloc-immobile-cell
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              malloc-immobile-cell
              (module-path-index-join '#f #f)
              *
              #t))
           (free
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              free
              (module-path-index-join '#f #f)
              *
              #t))
           (end-stubborn-change
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              end-stubborn-change
              (module-path-index-join '#f #f)
              *
              #t))
           (malloc
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              malloc
              (module-path-index-join '#f #f)
              *
              #t))
           (ERR_error_string_n
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ERR_error_string_n
              (module-path-index-join '#f #f)
              *
              #t))
           (cpointer-tag
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cpointer-tag
              (module-path-index-join '#f #f)
              *
              #t))
           (ERR_get_error
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ERR_get_error
              (module-path-index-join '#f #f)
              *
              #t))
           (ffi-lib
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ffi-lib
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_get_error
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_get_error
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-suggested-certificate-authorities!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-suggested-certificate-authorities!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_shutdown
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_shutdown
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_write
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_write
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-verify-root-certificates!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-verify-root-certificates!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_read
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_read
              (module-path-index-join '#f #f)
              *
              #t))
           (libssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libssl
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-certificate-chain!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-certificate-chain!
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-ports
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-ports
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_free
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_free
              (module-path-index-join '#f #f)
              *
              #t))
           (lookup
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              lookup
              (module-path-index-join '#f #f)
              *
              #t)))
          (#(free
             #<module-path-index>
             end-stubborn-change
             #<module-path-index>
             malloc
             #<module-path-index>
             set-cpointer-tag!
             #<module-path-index>
             cpointer-tag
             #<module-path-index>
             make-c-parameter
             #<module-path-index>
             cpointer-push-tag!
             #<module-path-index>
             cpointer-has-tag?
             #<module-path-index>
             ffi-lib
             (#<module-path-index> get-ffi-lib #<module-path-index> . ffi-lib)
             define-c
             #<module-path-index>
             ffi-obj-ref
             #<module-path-index>
             cblock->vector
             #<module-path-index>
             cblock->list
             #<module-path-index>
             make-sized-byte-string
             #<module-path-index>
             ptr-set!
             #<module-path-index>
             ptr-ref
             #<module-path-index>
             memcpy
             #<module-path-index>
             memmove
             #<module-path-index>
             memset
             #<module-path-index>
             set-ffi-obj!
             #<module-path-index>
             get-ffi-obj
             #<module-path-index>
             make-cvector*
             (#<module-path-index>
              make-cvector
              #<module-path-index>
              .
              make-cvector*)
             free-immobile-cell
             #<module-path-index>
             malloc-immobile-cell
             #<module-path-index>))
          #f)
        #(struct:lexical-rename ((libcrypto-so . env928679)))
        mark-3761990)
       (() -3761990 #<module-path-index>))))
  (define stx11472
    (#%decode-syntax
     #(struct:wrapped
       context
       (#(struct:phase-shift
          0
          (module-path-index-join '#f #f)
          (module-path-index-join '#f #f))
        #(struct:module-rename
          2
          normal
          3760270
          (#(struct:all-from-module
             (module-path-index-join 'scheme #f)
             2
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'scheme #f)
             2
             0
             ()
             #f))
          ()
          ()
          #f)
        #(struct:module-rename #f normal 3760270 () () () #f)
        #(struct:module-rename
          1
          normal
          3760270
          (#(struct:all-from-module
             (module-path-index-join 'scheme #f)
             1
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'scheme #f)
             1
             0
             ()
             #f))
          ()
          ()
          #f)
        #(struct:module-rename
          0
          normal
          3760270
          (#(struct:all-from-module
             (module-path-index-join 'mzlib/runtime-path #f)
             0
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'mzlib/port #f)
             0
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'mzlib/foreign #f)
             0
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'scheme #f)
             0
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'scheme #f)
             0
             0
             ()
             #f))
          ((wrap-ports
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              wrap-ports
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv2_server_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv2_server_method
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_accept
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_accept
              (module-path-index-join '#f #f)
              *
              #t))
           (raise-not-available
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              raise-not-available
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-...
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-...
              (module-path-index-join '#f #f)
              *
              #t))
           (_SSL*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _SSL*
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_connect
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_connect
              (module-path-index-join '#f #f)
              *
              #t))
           (_BIO_METHOD*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _BIO_METHOD*
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_set_bio
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_set_bio
              (module-path-index-join '#f #f)
              *
              #t))
           (ports->ssl-ports
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ports->ssl-ports
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_new
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_new
              (module-path-index-join '#f #f)
              *
              #t))
           (create-ssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              create-ssl
              (module-path-index-join '#f #f)
              *
              #t))
           (atomically
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              atomically
              (module-path-index-join '#f #f)
              *
              #t))
           (define-mzscheme
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-mzscheme
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_load_client_CA_file
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_load_client_CA_file
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_use_PrivateKey_file
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_use_PrivateKey_file
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-output-port
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-output-port
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_use_RSAPrivateKey_file
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_use_RSAPrivateKey_file
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-available?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-available?
              (module-path-index-join '#f #f)
              *
              #t))
           (exn:atomic?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              exn:atomic?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-exn:atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-exn:atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_set_client_CA_list
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_set_client_CA_list
              (module-path-index-join '#f #f)
              *
              #t))
           (exn:atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              exn:atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (exn:atomic-thunk
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              exn:atomic-thunk
              (module-path-index-join '#f #f)
              *
              #t))
           (_X509_NAME*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _X509_NAME*
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_load_verify_locations
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_load_verify_locations
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-input-port
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-input-port
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-connect
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-connect
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_use_certificate_chain_file
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_use_certificate_chain_file
              (module-path-index-join '#f #f)
              *
              #t))
           (pump-output
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              pump-output
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-close
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-close
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_set_verify
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_set_verify
              (module-path-index-join '#f #f)
              *
              #t))
           (pump-output-once
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              pump-output-once
              (module-path-index-join '#f #f)
              *
              #t))
           (do-ssl-connect
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              do-ssl-connect
              (module-path-index-join '#f #f)
              *
              #t))
           (encrypt->method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              encrypt->method
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_set_mode
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_set_mode
              (module-path-index-join '#f #f)
              *
              #t))
           (pump-input-once
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              pump-input-once
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listen
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listen
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_ctrl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_ctrl
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-release
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-release
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-accept/enable-break
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-accept/enable-break
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_free
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_free
              (module-path-index-join '#f #f)
              *
              #t))
           (default-encrypt
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              default-encrypt
              (module-path-index-join '#f #f)
              *
              #t))
           (in-atomic?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              in-atomic?
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-set-verify!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-set-verify!
              (module-path-index-join '#f #f)
              *
              #t))
           (error/network
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              error/network
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-private-key!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-private-key!
              (module-path-index-join '#f #f)
              *
              #t))
           (_SSL_CTX*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _SSL_CTX*
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_set_mem_eof_return
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_set_mem_eof_return
              (module-path-index-join '#f #f)
              *
              #t))
           (flush-ssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              flush-ssl
              (module-path-index-join '#f #f)
              *
              #t))
           (check-valid
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              check-valid
              (module-path-index-join '#f #f)
              *
              #t))
           (do-ssl-accept
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              do-ssl-accept
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:exn:atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:exn:atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_ctrl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_ctrl
              (module-path-index-join '#f #f)
              *
              #t))
           (get-error-message
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              get-error-message
              (module-path-index-join '#f #f)
              *
              #t))
           (define-define-X
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-define-X
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_write
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_write
              (module-path-index-join '#f #f)
              *
              #t))
           (libmz
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libmz
              (module-path-index-join '#f #f)
              *
              #t))
           (BUFFER-SIZE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BUFFER-SIZE
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_read
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_read
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listener?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listener?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-listener
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-listener
              (module-path-index-join '#f #f)
              *
              #t))
           (libcrypto
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libcrypto
              (module-path-index-join '#f #f)
              *
              #t))
           (3m?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              3m?
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_free
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_free
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listener-l
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listener-l
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:ssl-listener
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:ssl-listener
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-fail-reason
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-fail-reason
              (module-path-index-join '#f #f)
              *
              #t))
           (cpointer-push-tag!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cpointer-push-tag!
              (module-path-index-join '#f #f)
              *
              #t))
           (cpointer-has-tag?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cpointer-has-tag?
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_new
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_new
              (module-path-index-join '#f #f)
              *
              #t))
           (define-c
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-c
              (module-path-index-join '#f #f)
              *
              #t))
           (get-ffi-obj
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              get-ffi-obj
              (module-path-index-join '#f #f)
              *
              #t))
           (get-context/listener
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              get-context/listener
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_s_mem
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_s_mem
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_new
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_new
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-abandon-port
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-abandon-port
              (module-path-index-join '#f #f)
              *
              #t))
           (get-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              get-context
              (module-path-index-join '#f #f)
              *
              #t))
           (TLSv1_server_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              TLSv1_server_method
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-addresses
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-addresses
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-make-server-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-make-server-context
              (module-path-index-join '#f #f)
              *
              #t))
           (TLSv1_client_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              TLSv1_client_method
              (module-path-index-join '#f #f)
              *
              #t))
           (typedef
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              typedef
              (module-path-index-join '#f #f)
              *
              #t))
           (libssl-so
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libssl-so
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-make-client-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-make-client-context
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv23_server_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv23_server_method
              (module-path-index-join '#f #f)
              *
              #t))
           (make-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-context
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv23_client_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv23_client_method
              (module-path-index-join '#f #f)
              *
              #t))
           (define-crypto
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-crypto
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv3_server_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv3_server_method
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv3_client_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv3_client_method
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-server-context?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-server-context?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-server-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-server-context
              (module-path-index-join '#f #f)
              *
              #t))
           (set-ffi-obj!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-ffi-obj!
              (module-path-index-join '#f #f)
              *
              #t))
           (make-immobile-bytes
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-immobile-bytes
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-server-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-server-context
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:ssl-server-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:ssl-server-context
              (module-path-index-join '#f #f)
              *
              #t))
           (register
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              register
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listener
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listener
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv2_client_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv2_client_method
              (module-path-index-join '#f #f)
              *
              #t))
           (_SSL_METHOD*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _SSL_METHOD*
              (module-path-index-join '#f #f)
              *
              #t))
           (make-c-parameter
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-c-parameter
              (module-path-index-join '#f #f)
              *
              #t))
           (define-ssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-ssl
              (module-path-index-join '#f #f)
              *
              #t))
           (ffi-obj-ref
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ffi-obj-ref
              (module-path-index-join '#f #f)
              *
              #t))
           (libcrypto-so
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libcrypto-so
              (module-path-index-join '#f #f)
              *
              #t))
           (cblock->vector
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cblock->vector
              (module-path-index-join '#f #f)
              *
              #t))
           (cblock->list
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cblock->list
              (module-path-index-join '#f #f)
              *
              #t))
           (make-mzssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-mzssl
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-error
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-error
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-client-context?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-client-context?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-client-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-client-context
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-client-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-client-context
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:ssl-client-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:ssl-client-context
              (module-path-index-join '#f #f)
              *
              #t))
           (with-failure
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              with-failure
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-close-original?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-close-original?!
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-r-bio
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-r-bio
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listener-mzctx
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listener-mzctx
              (module-path-index-join '#f #f)
              *
              #t))
           (enforce-retry?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              enforce-retry?
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-flushing?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-flushing?!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_VERIFY_NONE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_VERIFY_NONE
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-w-closed?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-w-closed?!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-buffer!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-buffer!
              (module-path-index-join '#f #f)
              *
              #t))
           (scheme_make_custodian
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              scheme_make_custodian
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_FILETYPE_ASN1
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_FILETYPE_ASN1
              (module-path-index-join '#f #f)
              *
              #t))
           (kernel-thread
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              kernel-thread
              (module-path-index-join '#f #f)
              *
              #t))
           (scheme_end_atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              scheme_end_atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-must-read
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-must-read
              (module-path-index-join '#f #f)
              *
              #t))
           (scheme_start_atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              scheme_start_atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-context?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-context?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-context
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_ERROR_ZERO_RETURN
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_ERROR_ZERO_RETURN
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTRL_MODE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTRL_MODE
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-context
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-context-ctx
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-context-ctx
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:ssl-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:ssl-context
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-o
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-o
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_MODE_ENABLE_PARTIAL_WRITE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_MODE_ENABLE_PARTIAL_WRITE
              (module-path-index-join '#f #f)
              *
              #t))
           (make-cvector*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-cvector*
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_VERIFY_FAIL_IF_NO_PEER_CERT
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_VERIFY_FAIL_IF_NO_PEER_CERT
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-connect/enable-break
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-connect/enable-break
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-error!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-error!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-finalizer-cancel!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-finalizer-cancel!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-shutdown-on-close?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-shutdown-on-close?!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_VERIFY_PEER
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_VERIFY_PEER
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-refcount!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-refcount!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-must-read!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-must-read!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-must-write!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-must-write!
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:mzssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:mzssl
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-r-closed?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-r-closed?!
              (module-path-index-join '#f #f)
              *
              #t))
           (_BIO*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _BIO*
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-lock!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-lock!
              (module-path-index-join '#f #f)
              *
              #t))
           (ptr-set!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ptr-set!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-pipe-w!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-pipe-w!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-pipe-r!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-pipe-r!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-w-bio!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-w-bio!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-r-bio!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-r-bio!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-o!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-o!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-i!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-i!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_FILETYPE_PEM
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_FILETYPE_PEM
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-finalizer-cancel
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-finalizer-cancel
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-shutdown-on-close?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-shutdown-on-close?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-close-original?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-close-original?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-refcount
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-refcount
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_C_SET_BUF_MEM_EOF_RETURN
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_C_SET_BUF_MEM_EOF_RETURN
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-must-write
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-must-write
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-flushing?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-flushing?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-r-closed?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-r-closed?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-w-closed?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-w-closed?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-lock
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-lock
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-buffer
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-buffer
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-pipe-w
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-pipe-w
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-pipe-r
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-pipe-r
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-w-bio
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-w-bio
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_ERROR_SYSCALL
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_ERROR_SYSCALL
              (module-path-index-join '#f #f)
              *
              #t))
           (escape-atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              escape-atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-i
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-i
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl?
              (module-path-index-join '#f #f)
              *
              #t))
           (set-cpointer-tag!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-cpointer-tag!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_ERROR_WANT_WRITE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_ERROR_WANT_WRITE
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-accept
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-accept
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_ERROR_WANT_READ
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_ERROR_WANT_READ
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-ssl!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-ssl!
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-ssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-ssl
              (module-path-index-join '#f #f)
              *
              #t))
           (make-sized-byte-string
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-sized-byte-string
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_load_error_strings
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_load_error_strings
              (module-path-index-join '#f #f)
              *
              #t))
           (ptr-ref
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ptr-ref
              (module-path-index-join '#f #f)
              *
              #t))
           (memcpy
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              memcpy
              (module-path-index-join '#f #f)
              *
              #t))
           (memmove
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              memmove
              (module-path-index-join '#f #f)
              *
              #t))
           (memset
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              memset
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_library_init
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_library_init
              (module-path-index-join '#f #f)
              *
              #t))
           (free-immobile-cell
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              free-immobile-cell
              (module-path-index-join '#f #f)
              *
              #t))
           (malloc-immobile-cell
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              malloc-immobile-cell
              (module-path-index-join '#f #f)
              *
              #t))
           (free
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              free
              (module-path-index-join '#f #f)
              *
              #t))
           (end-stubborn-change
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              end-stubborn-change
              (module-path-index-join '#f #f)
              *
              #t))
           (malloc
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              malloc
              (module-path-index-join '#f #f)
              *
              #t))
           (ERR_error_string_n
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ERR_error_string_n
              (module-path-index-join '#f #f)
              *
              #t))
           (cpointer-tag
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cpointer-tag
              (module-path-index-join '#f #f)
              *
              #t))
           (ERR_get_error
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ERR_get_error
              (module-path-index-join '#f #f)
              *
              #t))
           (ffi-lib
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ffi-lib
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_get_error
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_get_error
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-suggested-certificate-authorities!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-suggested-certificate-authorities!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_shutdown
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_shutdown
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_write
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_write
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-verify-root-certificates!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-verify-root-certificates!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_read
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_read
              (module-path-index-join '#f #f)
              *
              #t))
           (libssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libssl
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-certificate-chain!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-certificate-chain!
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-ports
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-ports
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_free
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_free
              (module-path-index-join '#f #f)
              *
              #t))
           (lookup
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              lookup
              (module-path-index-join '#f #f)
              *
              #t)))
          (#(free
             #<module-path-index>
             end-stubborn-change
             #<module-path-index>
             malloc
             #<module-path-index>
             set-cpointer-tag!
             #<module-path-index>
             cpointer-tag
             #<module-path-index>
             make-c-parameter
             #<module-path-index>
             cpointer-push-tag!
             #<module-path-index>
             cpointer-has-tag?
             #<module-path-index>
             ffi-lib
             (#<module-path-index> get-ffi-lib #<module-path-index> . ffi-lib)
             define-c
             #<module-path-index>
             ffi-obj-ref
             #<module-path-index>
             cblock->vector
             #<module-path-index>
             cblock->list
             #<module-path-index>
             make-sized-byte-string
             #<module-path-index>
             ptr-set!
             #<module-path-index>
             ptr-ref
             #<module-path-index>
             memcpy
             #<module-path-index>
             memmove
             #<module-path-index>
             memset
             #<module-path-index>
             set-ffi-obj!
             #<module-path-index>
             get-ffi-obj
             #<module-path-index>
             make-cvector*
             (#<module-path-index>
              make-cvector
              #<module-path-index>
              .
              make-cvector*)
             free-immobile-cell
             #<module-path-index>
             malloc-immobile-cell
             #<module-path-index>))
          #f)
        #(struct:lexical-rename ((libssl-so . env928687)))
        mark-3762003)
       (() -3762003 #<module-path-index>))))
  (define stx11473
    (#%decode-syntax
     #(struct:wrapped
       tag
       (#(struct:phase-shift
          0
          (module-path-index-join '#f #f)
          (module-path-index-join '#f #f))
        #(struct:module-rename
          2
          normal
          3760270
          (#(struct:all-from-module
             (module-path-index-join 'scheme #f)
             2
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'scheme #f)
             2
             0
             ()
             #f))
          ()
          ()
          #f)
        #(struct:module-rename #f normal 3760270 () () () #f)
        #(struct:module-rename
          1
          normal
          3760270
          (#(struct:all-from-module
             (module-path-index-join 'scheme #f)
             1
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'scheme #f)
             1
             0
             ()
             #f))
          ()
          ()
          #f)
        #(struct:module-rename
          0
          normal
          3760270
          (#(struct:all-from-module
             (module-path-index-join 'mzlib/runtime-path #f)
             0
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'mzlib/port #f)
             0
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'mzlib/foreign #f)
             0
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'scheme #f)
             0
             0
             ()
             #f)
           #(struct:all-from-module
             (module-path-index-join 'scheme #f)
             0
             0
             ()
             #f))
          ((wrap-ports
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              wrap-ports
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv2_server_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv2_server_method
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_accept
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_accept
              (module-path-index-join '#f #f)
              *
              #t))
           (raise-not-available
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              raise-not-available
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-...
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-...
              (module-path-index-join '#f #f)
              *
              #t))
           (_SSL*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _SSL*
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_connect
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_connect
              (module-path-index-join '#f #f)
              *
              #t))
           (_BIO_METHOD*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _BIO_METHOD*
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_set_bio
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_set_bio
              (module-path-index-join '#f #f)
              *
              #t))
           (ports->ssl-ports
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ports->ssl-ports
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_new
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_new
              (module-path-index-join '#f #f)
              *
              #t))
           (create-ssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              create-ssl
              (module-path-index-join '#f #f)
              *
              #t))
           (atomically
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              atomically
              (module-path-index-join '#f #f)
              *
              #t))
           (define-mzscheme
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-mzscheme
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_load_client_CA_file
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_load_client_CA_file
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_use_PrivateKey_file
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_use_PrivateKey_file
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-output-port
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-output-port
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_use_RSAPrivateKey_file
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_use_RSAPrivateKey_file
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-available?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-available?
              (module-path-index-join '#f #f)
              *
              #t))
           (exn:atomic?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              exn:atomic?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-exn:atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-exn:atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_set_client_CA_list
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_set_client_CA_list
              (module-path-index-join '#f #f)
              *
              #t))
           (exn:atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              exn:atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (exn:atomic-thunk
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              exn:atomic-thunk
              (module-path-index-join '#f #f)
              *
              #t))
           (_X509_NAME*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _X509_NAME*
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_load_verify_locations
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_load_verify_locations
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-input-port
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-input-port
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-connect
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-connect
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_use_certificate_chain_file
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_use_certificate_chain_file
              (module-path-index-join '#f #f)
              *
              #t))
           (pump-output
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              pump-output
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-close
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-close
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_set_verify
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_set_verify
              (module-path-index-join '#f #f)
              *
              #t))
           (pump-output-once
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              pump-output-once
              (module-path-index-join '#f #f)
              *
              #t))
           (do-ssl-connect
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              do-ssl-connect
              (module-path-index-join '#f #f)
              *
              #t))
           (encrypt->method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              encrypt->method
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_set_mode
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_set_mode
              (module-path-index-join '#f #f)
              *
              #t))
           (pump-input-once
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              pump-input-once
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listen
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listen
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_ctrl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_ctrl
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-release
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-release
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-accept/enable-break
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-accept/enable-break
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_free
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_free
              (module-path-index-join '#f #f)
              *
              #t))
           (default-encrypt
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              default-encrypt
              (module-path-index-join '#f #f)
              *
              #t))
           (in-atomic?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              in-atomic?
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-set-verify!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-set-verify!
              (module-path-index-join '#f #f)
              *
              #t))
           (error/network
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              error/network
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-private-key!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-private-key!
              (module-path-index-join '#f #f)
              *
              #t))
           (_SSL_CTX*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _SSL_CTX*
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_set_mem_eof_return
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_set_mem_eof_return
              (module-path-index-join '#f #f)
              *
              #t))
           (flush-ssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              flush-ssl
              (module-path-index-join '#f #f)
              *
              #t))
           (check-valid
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              check-valid
              (module-path-index-join '#f #f)
              *
              #t))
           (do-ssl-accept
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              do-ssl-accept
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:exn:atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:exn:atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_ctrl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_ctrl
              (module-path-index-join '#f #f)
              *
              #t))
           (get-error-message
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              get-error-message
              (module-path-index-join '#f #f)
              *
              #t))
           (define-define-X
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-define-X
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_write
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_write
              (module-path-index-join '#f #f)
              *
              #t))
           (libmz
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libmz
              (module-path-index-join '#f #f)
              *
              #t))
           (BUFFER-SIZE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BUFFER-SIZE
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_read
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_read
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listener?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listener?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-listener
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-listener
              (module-path-index-join '#f #f)
              *
              #t))
           (libcrypto
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libcrypto
              (module-path-index-join '#f #f)
              *
              #t))
           (3m?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              3m?
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_free
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_free
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listener-l
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listener-l
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:ssl-listener
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:ssl-listener
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-fail-reason
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-fail-reason
              (module-path-index-join '#f #f)
              *
              #t))
           (cpointer-push-tag!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cpointer-push-tag!
              (module-path-index-join '#f #f)
              *
              #t))
           (cpointer-has-tag?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cpointer-has-tag?
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_new
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_new
              (module-path-index-join '#f #f)
              *
              #t))
           (define-c
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-c
              (module-path-index-join '#f #f)
              *
              #t))
           (get-ffi-obj
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              get-ffi-obj
              (module-path-index-join '#f #f)
              *
              #t))
           (get-context/listener
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              get-context/listener
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_s_mem
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_s_mem
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTX_new
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTX_new
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-abandon-port
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-abandon-port
              (module-path-index-join '#f #f)
              *
              #t))
           (get-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              get-context
              (module-path-index-join '#f #f)
              *
              #t))
           (TLSv1_server_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              TLSv1_server_method
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-addresses
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-addresses
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-make-server-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-make-server-context
              (module-path-index-join '#f #f)
              *
              #t))
           (TLSv1_client_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              TLSv1_client_method
              (module-path-index-join '#f #f)
              *
              #t))
           (typedef
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              typedef
              (module-path-index-join '#f #f)
              *
              #t))
           (libssl-so
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libssl-so
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-make-client-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-make-client-context
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv23_server_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv23_server_method
              (module-path-index-join '#f #f)
              *
              #t))
           (make-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-context
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv23_client_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv23_client_method
              (module-path-index-join '#f #f)
              *
              #t))
           (define-crypto
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-crypto
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv3_server_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv3_server_method
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv3_client_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv3_client_method
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-server-context?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-server-context?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-server-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-server-context
              (module-path-index-join '#f #f)
              *
              #t))
           (set-ffi-obj!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-ffi-obj!
              (module-path-index-join '#f #f)
              *
              #t))
           (make-immobile-bytes
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-immobile-bytes
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-server-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-server-context
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:ssl-server-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:ssl-server-context
              (module-path-index-join '#f #f)
              *
              #t))
           (register
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              register
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listener
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listener
              (module-path-index-join '#f #f)
              *
              #t))
           (SSLv2_client_method
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSLv2_client_method
              (module-path-index-join '#f #f)
              *
              #t))
           (_SSL_METHOD*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _SSL_METHOD*
              (module-path-index-join '#f #f)
              *
              #t))
           (make-c-parameter
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-c-parameter
              (module-path-index-join '#f #f)
              *
              #t))
           (define-ssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              define-ssl
              (module-path-index-join '#f #f)
              *
              #t))
           (ffi-obj-ref
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ffi-obj-ref
              (module-path-index-join '#f #f)
              *
              #t))
           (libcrypto-so
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libcrypto-so
              (module-path-index-join '#f #f)
              *
              #t))
           (cblock->vector
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cblock->vector
              (module-path-index-join '#f #f)
              *
              #t))
           (cblock->list
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cblock->list
              (module-path-index-join '#f #f)
              *
              #t))
           (make-mzssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-mzssl
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-error
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-error
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-client-context?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-client-context?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-client-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-client-context
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-client-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-client-context
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:ssl-client-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:ssl-client-context
              (module-path-index-join '#f #f)
              *
              #t))
           (with-failure
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              with-failure
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-close-original?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-close-original?!
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-r-bio
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-r-bio
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-listener-mzctx
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-listener-mzctx
              (module-path-index-join '#f #f)
              *
              #t))
           (enforce-retry?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              enforce-retry?
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-flushing?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-flushing?!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_VERIFY_NONE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_VERIFY_NONE
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-w-closed?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-w-closed?!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-buffer!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-buffer!
              (module-path-index-join '#f #f)
              *
              #t))
           (scheme_make_custodian
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              scheme_make_custodian
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_FILETYPE_ASN1
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_FILETYPE_ASN1
              (module-path-index-join '#f #f)
              *
              #t))
           (kernel-thread
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              kernel-thread
              (module-path-index-join '#f #f)
              *
              #t))
           (scheme_end_atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              scheme_end_atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-must-read
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-must-read
              (module-path-index-join '#f #f)
              *
              #t))
           (scheme_start_atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              scheme_start_atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-context?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-context?
              (module-path-index-join '#f #f)
              *
              #t))
           (make-ssl-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-ssl-context
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_ERROR_ZERO_RETURN
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_ERROR_ZERO_RETURN
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_CTRL_MODE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_CTRL_MODE
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-context
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-context-ctx
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-context-ctx
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:ssl-context
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:ssl-context
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-o
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-o
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_MODE_ENABLE_PARTIAL_WRITE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_MODE_ENABLE_PARTIAL_WRITE
              (module-path-index-join '#f #f)
              *
              #t))
           (make-cvector*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-cvector*
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_VERIFY_FAIL_IF_NO_PEER_CERT
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_VERIFY_FAIL_IF_NO_PEER_CERT
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-connect/enable-break
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-connect/enable-break
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-error!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-error!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-finalizer-cancel!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-finalizer-cancel!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-shutdown-on-close?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-shutdown-on-close?!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_VERIFY_PEER
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_VERIFY_PEER
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-refcount!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-refcount!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-must-read!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-must-read!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-must-write!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-must-write!
              (module-path-index-join '#f #f)
              *
              #t))
           (struct:mzssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              struct:mzssl
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-r-closed?!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-r-closed?!
              (module-path-index-join '#f #f)
              *
              #t))
           (_BIO*
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              _BIO*
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-lock!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-lock!
              (module-path-index-join '#f #f)
              *
              #t))
           (ptr-set!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ptr-set!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-pipe-w!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-pipe-w!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-pipe-r!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-pipe-r!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-w-bio!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-w-bio!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-r-bio!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-r-bio!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-o!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-o!
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-i!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-i!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_FILETYPE_PEM
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_FILETYPE_PEM
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-finalizer-cancel
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-finalizer-cancel
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-shutdown-on-close?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-shutdown-on-close?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-close-original?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-close-original?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-refcount
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-refcount
              (module-path-index-join '#f #f)
              *
              #t))
           (BIO_C_SET_BUF_MEM_EOF_RETURN
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              BIO_C_SET_BUF_MEM_EOF_RETURN
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-must-write
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-must-write
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-flushing?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-flushing?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-r-closed?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-r-closed?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-w-closed?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-w-closed?
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-lock
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-lock
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-buffer
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-buffer
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-pipe-w
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-pipe-w
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-pipe-r
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-pipe-r
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-w-bio
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-w-bio
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_ERROR_SYSCALL
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_ERROR_SYSCALL
              (module-path-index-join '#f #f)
              *
              #t))
           (escape-atomic
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              escape-atomic
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-i
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-i
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl?
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl?
              (module-path-index-join '#f #f)
              *
              #t))
           (set-cpointer-tag!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-cpointer-tag!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_ERROR_WANT_WRITE
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_ERROR_WANT_WRITE
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-accept
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-accept
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_ERROR_WANT_READ
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_ERROR_WANT_READ
              (module-path-index-join '#f #f)
              *
              #t))
           (set-mzssl-ssl!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              set-mzssl-ssl!
              (module-path-index-join '#f #f)
              *
              #t))
           (mzssl-ssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              mzssl-ssl
              (module-path-index-join '#f #f)
              *
              #t))
           (make-sized-byte-string
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              make-sized-byte-string
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_load_error_strings
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_load_error_strings
              (module-path-index-join '#f #f)
              *
              #t))
           (ptr-ref
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ptr-ref
              (module-path-index-join '#f #f)
              *
              #t))
           (memcpy
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              memcpy
              (module-path-index-join '#f #f)
              *
              #t))
           (memmove
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              memmove
              (module-path-index-join '#f #f)
              *
              #t))
           (memset
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              memset
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_library_init
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_library_init
              (module-path-index-join '#f #f)
              *
              #t))
           (free-immobile-cell
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              free-immobile-cell
              (module-path-index-join '#f #f)
              *
              #t))
           (malloc-immobile-cell
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              malloc-immobile-cell
              (module-path-index-join '#f #f)
              *
              #t))
           (free
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              free
              (module-path-index-join '#f #f)
              *
              #t))
           (end-stubborn-change
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              end-stubborn-change
              (module-path-index-join '#f #f)
              *
              #t))
           (malloc
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              malloc
              (module-path-index-join '#f #f)
              *
              #t))
           (ERR_error_string_n
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ERR_error_string_n
              (module-path-index-join '#f #f)
              *
              #t))
           (cpointer-tag
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              cpointer-tag
              (module-path-index-join '#f #f)
              *
              #t))
           (ERR_get_error
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ERR_get_error
              (module-path-index-join '#f #f)
              *
              #t))
           (ffi-lib
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ffi-lib
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_get_error
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_get_error
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-suggested-certificate-authorities!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-suggested-certificate-authorities!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_shutdown
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_shutdown
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_write
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_write
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-verify-root-certificates!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-verify-root-certificates!
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_read
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_read
              (module-path-index-join '#f #f)
              *
              #t))
           (libssl
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              libssl
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-load-certificate-chain!
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-load-certificate-chain!
              (module-path-index-join '#f #f)
              *
              #t))
           (ssl-ports
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              ssl-ports
              (module-path-index-join '#f #f)
              *
              #t))
           (SSL_free
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              SSL_free
              (module-path-index-join '#f #f)
              *
              #t))
           (lookup
            .
            #(struct:module-binding
              (module-path-index-join '#f #f)
              *
              *
              lookup
              (module-path-index-join '#f #f)
              *
              #t)))
          (#(free
             #<module-path-index>
             end-stubborn-change
             #<module-path-index>
             malloc
             #<module-path-index>
             set-cpointer-tag!
             #<module-path-index>
             cpointer-tag
             #<module-path-index>
             make-c-parameter
             #<module-path-index>
             cpointer-push-tag!
             #<module-path-index>
             cpointer-has-tag?
             #<module-path-index>
             ffi-lib
             (#<module-path-index> get-ffi-lib #<module-path-index> . ffi-lib)
             define-c
             #<module-path-index>
             ffi-obj-ref
             #<module-path-index>
             cblock->vector
             #<module-path-index>
             cblock->list
             #<module-path-index>
             make-sized-byte-string
             #<module-path-index>
             ptr-set!
             #<module-path-index>
             ptr-ref
             #<module-path-index>
             memcpy
             #<module-path-index>
             memmove
             #<module-path-index>
             memset
             #<module-path-index>
             set-ffi-obj!
             #<module-path-index>
             get-ffi-obj
             #<module-path-index>
             make-cvector*
             (#<module-path-index>
              make-cvector
              #<module-path-index>
              .
              make-cvector*)
             free-immobile-cell
             #<module-path-index>
             malloc-immobile-cell
             #<module-path-index>))
          #f)
        #(struct:lexical-rename ((libssl-so . env928687)))
        mark-3762007)
       (() -3762007 #<module-path-index>))))
  (define-values
   (lift11462)
   (lambda (arg0-11474 arg1-11475)
     '#(...e/more-scheme.ss:220:7
        "/Users/jay/Dev/svn/plt/collects/scheme/private/more-scheme.ss"
        220
        7
        6847
        727
        #t)
     '(captures: #%modvars)
     (with-continuation-mark
      |_break-enabled-key@(quote #%paramz)|
      arg0-11474
      (with-continuation-mark
       |_exception-handler-key@(quote #%paramz)|
       lift11461
       (arg1-11475)))))
  (define-values
   (lift11461)
   (lambda (arg0-11476)
     '#(...e/more-scheme.ss:232:15
        "/Users/jay/Dev/svn/plt/collects/scheme/private/more-scheme.ss"
        232
        15
        7365
        180
        #t)
     '(captures: #%modvars)
     (abort-current-continuation _handler-prompt-key11321 arg0-11476)))
  (define-values
   (lift11460)
   (lambda (arg0-11479 arg1-11480 arg2-11481)
     '#(loop
        "/Users/jay/Dev/svn/plt/collects/scheme/private/more-scheme.ss"
        175
        6
        5620
        302
        #f)
     '(captures: #%modvars)
     (if (null? arg2-11481)
       (raise arg0-11479)
       (if ((caar arg2-11481) arg0-11479)
         (begin0
           ((cdar arg2-11481) arg0-11479)
           (with-continuation-mark
            |_break-enabled-key@(quote #%paramz)|
            arg1-11480
            (|_check-for-break@(quote #%paramz)|)))
         (lift11460 arg0-11479 arg1-11480 (cdr arg2-11481))))))
  (define-values
   (_select-handler/no-breaks11319)
   (lambda (arg0-11492 arg1-11493 arg2-11494)
     '#(select-handler/no-breaks
        "/Users/jay/Dev/svn/plt/collects/scheme/private/more-scheme.ss"
        170
        2
        5413
        511
        #f)
     '(captures: #%modvars)
     (with-continuation-mark
      |_break-enabled-key@(quote #%paramz)|
      (make-thread-cell '#f)
      (lift11460 arg0-11492 arg1-11493 arg2-11494))))
  (define-values (_false-thread-cell11320) (make-thread-cell '#f))
  (define-values (_handler-prompt-key11321) (make-continuation-prompt-tag))
  (define-values
   (_call-handled-body11322)
   (lambda (arg0-11500 arg1-11501 arg2-11502)
     '#(call-handled-body
        "/Users/jay/Dev/svn/plt/collects/scheme/private/more-scheme.ss"
        213
        2
        6537
        1107
        #f)
     '(captures: #%modvars)
     (with-continuation-mark
      |_break-enabled-key@(quote #%paramz)|
      _false-thread-cell11320
      (call-with-continuation-prompt
       lift11462
       _handler-prompt-key11321
       arg1-11501
       arg0-11500
       arg2-11502))))
  (define-values
   (_call-with-exception-handler11323)
   (lambda (arg0-11508 arg1-11509)
     '#(call-with-exception-handler
        "/Users/jay/Dev/svn/plt/collects/scheme/private/more-scheme.ss"
        271
        2
        9078
        256
        #f)
     '(captures: #%modvars)
     (begin0
       (with-continuation-mark
        |_exception-handler-key@(quote #%paramz)|
        arg0-11508
        (arg1-11509))
       '0)))
  (define-values
   (_struct:keyword-procedure11324
    _make-keyword-procedure11325
    _keyword-procedure?11326
    _keyword-procedure-proc11327
    _set-keyword-procedure-proc!11328
    _keyword-procedure-required11329
    _set-keyword-procedure-required!11330
    _keyword-procedure-allowed11331
    _set-keyword-procedure-allowed!11332)
   (let ((local11510 (current-inspector)))
     (begin
       (if local11510
         (if (not (inspector? local11510))
           (raise-type-error 'define-struct '"inspector or #f" local11510)
           (void))
         (void))
       (let ((localv11516 ?)
             (localv11517 ?)
             (localv11518 ?)
             (localv11519 ?)
             (localv11520 ?))
         (begin
           (set!-values (localv11516
                         localv11517
                         localv11518
                         localv11519
                         localv11520)
             (make-struct-type
              'keyword-procedure
              '#f
              '3
              '0
              '#f
              '()
              local11510))
           (values
            localv11516
            localv11517
            localv11518
            (make-struct-field-accessor localv11519 '0 'proc)
            (make-struct-field-mutator localv11520 '0 'proc)
            (make-struct-field-accessor localv11519 '1 'required)
            (make-struct-field-mutator localv11520 '1 'required)
            (make-struct-field-accessor localv11519 '2 'allowed)
            (make-struct-field-mutator localv11520 '2 'allowed)))))))
  (define-values
   (_struct:keyword-method11333
    _make-km11334
    _keyword-method?11335
    _km-ref11336
    _km-set!11337)
   (make-struct-type 'procedure _struct:keyword-procedure11324 '0 '0 '#f))
  (define-values
   (_generate-arity-string11338)
   (lambda (arg0-11560)
     '#(generate-arity-string
        "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
        31
        2
        1001
        2388
        #f)
     '(captures: #%modvars)
     (let ((localv11561 ?) (localv11562 ?) (localv11563 ?) (localv11564 ?))
       (begin
         (set!-values (localv11561 localv11562)
           (_procedure-keywords11339 arg0-11560))
         (begin
           (set!-values (localv11563) (procedure-arity arg0-11560))
           (begin
             (set!-values (localv11564)
               (lambda (arg0-11567 arg1-11568)
                 '#(keywords-desc
                    "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
                    35
                    18
                    1195
                    890
                    #f)
                 (format
                  '"~a with keyword~a~a"
                  (if (null? (cdr arg1-11568))
                    (format '"an ~aargument" arg0-11567)
                    (format '"~aarguments" arg0-11567))
                  (if (null? (cdr arg1-11568)) '"" '"s")
                  (let ((local11581 (length arg1-11568)))
                    (if (eq? local11581 '1)
                      (begin local11581 (format '" ~a" (car arg1-11568)))
                      (if (eq? local11581 '2)
                        (format
                         '" ~a and ~a"
                         (car arg1-11568)
                         (cadr arg1-11568))
                        ((lambda (arg0-11596)
                           '#(loop
                              "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
                              47
                              31
                              1810
                              271
                              #f)
                           (if (null? (cdr arg0-11596))
                             (format '" and ~a" (car arg0-11596))
                             (format
                              '" ~a,~a"
                              (car arg0-11596)
                              (let ((local11606 (cdr arg0-11596)))
                                (if (null? (cdr local11606))
                                  (format '" and ~a" (car local11606))
                                  (format
                                   '" ~a,~a"
                                   (car local11606)
                                   (let ((local11617 (cdr local11606)))
                                     (if (null? (cdr local11617))
                                       (format '" and ~a" (car local11617))
                                       (format
                                        '" ~a,~a"
                                        (car local11617)
                                        (loop922 (cdr local11617)))))))))))
                         arg1-11568)))))))
             (string-append
              (if (number? localv11563)
                (let ((local11634
                       (if (if (_okm?11340 arg0-11560)
                             (begin arg0-11560 '#t)
                             (_keyword-method?11335 arg0-11560))
                         (if (zero? localv11563)
                           (begin localv11563 '0)
                           (sub1 localv11563))
                         localv11563)))
                  (format
                   '"~a argument~a"
                   local11634
                   (if (= local11634 '1) '"" '"s")))
                (if (arity-at-least? localv11563)
                  (let ((local11645
                         (let ((local11646 (arity-at-least-value localv11563)))
                           (if (if (_okm?11340 arg0-11560)
                                 '#t
                                 (_keyword-method?11335 arg0-11560))
                             (if (zero? local11646) '0 (sub1 local11646))
                             local11646))))
                    (format
                     '"at least ~a argument~a"
                     local11645
                     (if (= local11645 '1) '"" '"s")))
                  (begin localv11563 '"a different number of arguments")))
              (if (null? localv11561)
                '""
                (format '" plus ~a" (localv11564 '"" localv11561)))
              (if localv11562
                (let ((local11662
                       ((lambda (arg0-11665 arg1-11666)
                          '#(loop
                             "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
                             73
                             25
                             2832
                             353
                             #f)
                          (if (null? arg0-11665)
                            arg1-11666
                            (if (eq? (car arg0-11665) (car arg1-11666))
                              (let ((local11672 (cdr arg0-11665)))
                                (let ((local11674 (cdr arg1-11666)))
                                  (if (null? local11672)
                                    local11674
                                    (if (eq? (car local11672) (car local11674))
                                      (let ((local11681 (cdr local11672)))
                                        (let ((local11683 (cdr local11674)))
                                          (if (null? local11681)
                                            local11683
                                            (if (eq?
                                                 (car local11681)
                                                 (car local11683))
                                              (loop923
                                               (cdr local11681)
                                               (cdr local11683))
                                              (cons
                                               (car local11683)
                                               (loop923
                                                local11681
                                                (cdr local11683)))))))
                                      (cons
                                       (car local11674)
                                       (let ((local11703 (cdr local11674)))
                                         (if (null? local11672)
                                           local11703
                                           (if (eq?
                                                (car local11672)
                                                (car local11703))
                                             (loop923
                                              (cdr local11672)
                                              (cdr local11703))
                                             (cons
                                              (car local11703)
                                              (loop923
                                               local11672
                                               (cdr local11703)))))))))))
                              (cons
                               (car arg1-11666)
                               (let ((local11723 (cdr arg1-11666)))
                                 (if (null? arg0-11665)
                                   local11723
                                   (if (eq? (car arg0-11665) (car local11723))
                                     (let ((local11730 (cdr arg0-11665)))
                                       (let ((local11732 (cdr local11723)))
                                         (if (null? local11730)
                                           local11732
                                           (if (eq?
                                                (car local11730)
                                                (car local11732))
                                             (loop923
                                              (cdr local11730)
                                              (cdr local11732))
                                             (cons
                                              (car local11732)
                                              (loop923
                                               local11730
                                               (cdr local11732)))))))
                                     (cons
                                      (car local11723)
                                      (let ((local11752 (cdr local11723)))
                                        (if (null? arg0-11665)
                                          local11752
                                          (if (eq?
                                               (car arg0-11665)
                                               (car local11752))
                                            (loop923
                                             (cdr arg0-11665)
                                             (cdr local11752))
                                            (cons
                                             (car local11752)
                                             (loop923
                                              arg0-11665
                                              (cdr local11752))))))))))))))
                        localv11561
                        localv11562)))
                  (if (null? local11662)
                    '""
                    (format
                     '" plus ~a"
                     (localv11564 '"optional " local11662))))
                '" plus arbitrary keyword arguments"))))))))
  (define-values
   (_struct:okp11341
    _make-optional-keyword-procedure11342
    _okp?11343
    _okp-ref11344
    _okp-set!11345)
   (make-struct-type
    'procedure
    _struct:keyword-procedure11324
    '1
    '0
    '#f
    (list (cons prop:arity-string _generate-arity-string11338))
    (current-inspector)
    '0))
  (define-values
   (_struct:okm11346
    _make-optional-keyword-method11347
    _okm?11340
    _okm-ref11348
    _okm-set!11349)
   (make-struct-type 'procedure _struct:okp11341 '0 '0 '#f))
  (define-values
   (_new-prop:procedure11350 _new-procedure?11351 _new-procedure-ref11352)
   (make-struct-type-property
    'procedure
    '#f
    (list (cons prop:procedure values))))
  (define-values
   (_*make-keyword-procedure11353)
   (let ((localv11796 (box ?)))
     (begin
       (set-boxes!
        (localv11796)
        (case-lambda
         ((arg0-11797)
          '#(...me/private/kw.ss:128:16
             "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
             128
             16
             5426
             158
             #t)
          '(captures: localv11796)
          ((unbox localv11796)
           arg0-11797
           (lambda rest11800
             '#(...me/private/kw.ss:130:25
                "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
                130
                25
                5514
                68
                #t)
             '(captures: arg0-11797)
             (apply arg0-11797 '() '() rest11800))))
         ((arg0-11805 arg1-11806)
          '#(...me/private/kw.ss:132:16
             "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
             132
             16
             5601
             166
             #t)
          '(captures: #%modvars)
          (_make-optional-keyword-procedure11342
           arg0-11805
           '()
           '#f
           arg1-11806))))
       (unbox localv11796))))
  (define-values
   (_keyword-apply11354)
   (lambda (arg0-11811 arg1-11812 arg2-11813 . rest11814)
     '#(keyword-apply
        "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
        140
        2
        5830
        1799
        #f)
     '(captures: #%modvars)
     (begin
       (if (procedure? arg0-11811)
         (void)
         (apply
          raise-type-error
          'keyword-apply
          '"procedure"
          '0
          arg0-11811
          arg1-11812
          arg2-11813
          rest11814))
       ((lambda (arg0-11829 arg1-11830 arg2-11831 arg3-11832 arg4-11833)
          '#(loop
             "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
             153
             6
             6251
             459
             #f)
          (if (null? arg4-11833)
            (void)
            (if (if (pair? arg4-11833) (not (keyword? (car arg4-11833))) '#t)
              (apply
               raise-type-error
               'keyword-apply
               '"list of keywords"
               '1
               arg0-11829
               arg1-11830
               arg2-11831
               arg3-11832)
              (if (null? (cdr arg4-11833))
                (void)
                (if (if (pair? (cdr arg4-11833))
                      (not (keyword? (cadr arg4-11833)))
                      '#t)
                  (loop925
                   arg0-11829
                   arg1-11830
                   arg2-11831
                   arg3-11832
                   (cdr arg4-11833))
                  (if (keyword<? (car arg4-11833) (cadr arg4-11833))
                    (loop925
                     arg0-11829
                     arg1-11830
                     arg2-11831
                     arg3-11832
                     (cdr arg4-11833))
                    (apply
                     raise-type-error
                     'keyword-apply
                     '"sorted list of keywords"
                     '1
                     arg0-11829
                     arg1-11830
                     arg2-11831
                     arg3-11832)))))))
        arg0-11811
        arg1-11812
        arg2-11813
        rest11814
        arg1-11812)
       (if (list? arg2-11813)
         (void)
         (apply
          raise-type-error
          'keyword-apply
          '"list"
          '2
          arg0-11811
          arg1-11812
          arg2-11813
          rest11814))
       (if (= (length arg1-11812) (length arg2-11813))
         (void)
         (raise-mismatch-error
          'keyword-apply
          (format
           '"keyword list: ~e; does not match the length of the value list: "
           arg1-11812)
          arg2-11813))
       (let ((local11896
              ((lambda (arg0-11903
                        arg1-11904
                        arg2-11905
                        arg3-11906
                        arg4-11907
                        arg5-11908)
                 '#(loop
                    "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
                    176
                    13
                    7039
                    363
                    #f)
                 (if (null? (cdr arg4-11907))
                   (let ((local11911 (car arg4-11907)))
                     (if (list? local11911)
                       local11911
                       (apply
                        raise-type-error
                        'keyword-apply
                        '"list"
                        arg5-11908
                        arg0-11903
                        arg1-11904
                        arg2-11905
                        arg3-11906)))
                   (cons
                    (car arg4-11907)
                    (let ((local11925 (cdr arg4-11907)))
                      (let ((local11927 (add1 arg5-11908)))
                        (if (null? (cdr local11925))
                          (let ((local11931 (car local11925)))
                            (if (list? local11931)
                              local11931
                              (begin
                                local11931
                                (apply
                                 raise-type-error
                                 'keyword-apply
                                 '"list"
                                 local11927
                                 arg0-11903
                                 arg1-11904
                                 arg2-11905
                                 arg3-11906))))
                          (cons
                           (car local11925)
                           (loop926
                            arg0-11903
                            arg1-11904
                            arg2-11905
                            arg3-11906
                            (cdr local11925)
                            (add1 local11927)))))))))
               arg0-11811
               arg1-11812
               arg2-11813
               rest11814
               rest11814
               '3)))
         (if (null? arg1-11812)
           (apply arg0-11811 local11896)
           (apply
            (let ((local11960 (+ '2 (length local11896))))
              (_keyword-procedure-extract/method11356
               arg1-11812
               local11960
               arg0-11811
               '0))
            arg1-11812
            arg2-11813
            local11896))))))
  (define-values
   (_procedure-keywords11339)
   (lambda (arg0-11968)
     '#(procedure-keywords
        "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
        192
        2
        7633
        652
        #f)
     '(captures: #%modvars)
     (if (_keyword-procedure?11326 arg0-11968)
       (values
        (_keyword-procedure-required11329 arg0-11968)
        (_keyword-procedure-allowed11331 arg0-11968))
       (if (procedure? arg0-11968)
         (let ((local11975 (procedure-extract-target arg0-11968)))
           (if local11975
             (begin
               arg0-11968
               (if (_keyword-procedure?11326 local11975)
                 (values
                  (_keyword-procedure-required11329 local11975)
                  (_keyword-procedure-allowed11331 local11975))
                 (if (procedure? local11975)
                   (let ((local11983 (procedure-extract-target local11975)))
                     (if local11983
                       (_procedure-keywords11339 local11983)
                       (begin
                         local11983
                         (if (_new-procedure?11351 local11975)
                           (let ((local11987
                                  (_new-procedure-ref11352 local11975)))
                             (if (procedure? local11987)
                               (_procedure-keywords11339 local11987)
                               (values '() '())))
                           (values '() '())))))
                   (raise-type-error
                    'procedure-keywords
                    '"procedure"
                    local11975))))
             (if (_new-procedure?11351 arg0-11968)
               (let ((local11999 (_new-procedure-ref11352 arg0-11968)))
                 (if (procedure? local11999)
                   (if (_keyword-procedure?11326 local11999)
                     (values
                      (_keyword-procedure-required11329 local11999)
                      (_keyword-procedure-allowed11331 local11999))
                     (if (procedure? local11999)
                       (let ((local12008
                              (procedure-extract-target local11999)))
                         (if local12008
                           (_procedure-keywords11339 local12008)
                           (begin
                             local12008
                             (if (_new-procedure?11351 local11999)
                               (let ((local12012
                                      (_new-procedure-ref11352 local11999)))
                                 (if (procedure? local12012)
                                   (_procedure-keywords11339 local12012)
                                   (values '() '())))
                               (values '() '())))))
                       (raise-type-error
                        'procedure-keywords
                        '"procedure"
                        local11999)))
                   (values '() '())))
               (values '() '()))))
         (raise-type-error 'procedure-keywords '"procedure" arg0-11968)))))
  (define-values
   (_check-kw-args11355)
   (lambda (arg0-12030 arg1-12031)
     '#(check-kw-args
        "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
        723
        2
        35661
        749
        #f)
     '(captures: #%modvars)
     ((lambda (arg0-12035 arg1-12036 arg2-12037)
        '#(loop
           "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
           724
           4
           35695
           714
           #f)
        (if (null? arg0-12035)
          (if (null? arg1-12036)
            (values '#f '#f)
            (values (car arg1-12036) '#f))
          (if (if (pair? arg1-12036)
                (eq? (car arg1-12036) (car arg0-12035))
                '#f)
            (let ((local12050 (cdr arg0-12035)))
              (let ((local12052 (cdr arg1-12036)))
                (let ((local12054 (if arg2-12037 (cdr arg2-12037) '#f)))
                  (if (null? local12050)
                    (if (null? local12052)
                      (values '#f '#f)
                      (values (car local12052) '#f))
                    (if (if (pair? local12052)
                          (eq? (car local12052) (car local12050))
                          '#f)
                      (loop927
                       (cdr local12050)
                       (cdr local12052)
                       (if local12054 (cdr local12054) '#f))
                      (if local12054
                        (if (pair? local12054)
                          (if (eq? (car local12054) (car local12050))
                            (loop927
                             (cdr local12050)
                             local12052
                             (cdr local12054))
                            (loop927 local12050 local12052 (cdr local12054)))
                          (values '#f (car local12050)))
                        (loop927 (cdr local12050) local12052 '#f)))))))
            (if arg2-12037
              (if (pair? arg2-12037)
                (if (eq? (car arg2-12037) (car arg0-12035))
                  (let ((local12100 (cdr arg0-12035)))
                    (let ((local12102 (cdr arg2-12037)))
                      (if (null? local12100)
                        (if (null? arg1-12036)
                          (values '#f '#f)
                          (values (car arg1-12036) '#f))
                        (if (if (pair? arg1-12036)
                              (eq? (car arg1-12036) (car local12100))
                              '#f)
                          (loop927
                           (cdr local12100)
                           (cdr arg1-12036)
                           (if local12102 (cdr local12102) '#f))
                          (if local12102
                            (if (pair? local12102)
                              (if (eq? (car local12102) (car local12100))
                                (loop927
                                 (cdr local12100)
                                 arg1-12036
                                 (cdr local12102))
                                (loop927
                                 local12100
                                 arg1-12036
                                 (cdr local12102)))
                              (values '#f (car local12100)))
                            (loop927 (cdr local12100) arg1-12036 '#f))))))
                  (let ((local12143 (cdr arg2-12037)))
                    (if (null? arg0-12035)
                      (if (null? arg1-12036)
                        (values '#f '#f)
                        (values (car arg1-12036) '#f))
                      (if (if (pair? arg1-12036)
                            (eq? (car arg1-12036) (car arg0-12035))
                            '#f)
                        (loop927
                         (cdr arg0-12035)
                         (cdr arg1-12036)
                         (if local12143 (cdr local12143) '#f))
                        (if local12143
                          (if (pair? local12143)
                            (if (eq? (car local12143) (car arg0-12035))
                              (loop927
                               (cdr arg0-12035)
                               arg1-12036
                               (cdr local12143))
                              (loop927 arg0-12035 arg1-12036 (cdr local12143)))
                            (values '#f (car arg0-12035)))
                          (loop927 (cdr arg0-12035) arg1-12036 '#f))))))
                (values '#f (car arg0-12035)))
              (let ((local12187 (cdr arg0-12035)))
                (if (null? local12187)
                  (if (null? arg1-12036)
                    (values '#f '#f)
                    (values (car arg1-12036) '#f))
                  (if (if (pair? arg1-12036)
                        (eq? (car arg1-12036) (car local12187))
                        '#f)
                    (loop927 (cdr local12187) (cdr arg1-12036) '#f)
                    (loop927 (cdr local12187) arg1-12036 '#f))))))))
      arg1-12031
      (_keyword-procedure-required11329 arg0-12030)
      (_keyword-procedure-allowed11331 arg0-12030))))
  (define-values
   (_keyword-procedure-extract/method11356)
   (lambda (arg0-12212 arg1-12213 arg2-12214 arg3-12215)
     '#(keyword-procedure-extract/method
        "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
        746
        2
        36564
        3524
        #f)
     '(captures: #%modvars)
     (if (if (_keyword-procedure?11326 arg2-12214)
           (if (procedure-arity-includes?
                (_keyword-procedure-proc11327 arg2-12214)
                arg1-12213)
             (let ((localv12220 ?) (localv12221 ?))
               (begin
                 (set!-values (localv12220 localv12221)
                   (_check-kw-args11355 arg2-12214 arg0-12212))
                 (if localv12220 '#f (not localv12221))))
             '#f)
           '#f)
       (_keyword-procedure-proc11327 arg2-12214)
       (let ((local12226
              (if (_keyword-procedure?11326 arg2-12214)
                '#f
                (if (procedure? arg2-12214)
                  (let ((local12229 (procedure-extract-target arg2-12214)))
                    (if local12229
                      local12229
                      (begin
                        local12229
                        (if (_new-procedure?11351 arg2-12214) 'method '#f))))
                  '#f))))
         (if local12226
           (if (eq? local12226 'method)
             (begin
               local12226
               (let ((local12234
                      (_keyword-procedure-extract/method11356
                       arg0-12212
                       (add1 arg1-12213)
                       (_new-procedure-ref11352 arg2-12214)
                       (add1 arg3-12215))))
                 (lambda (arg0-12242 arg1-12243 . rest12244)
                   '#(...me/private/kw.ss:764:14
                      "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
                      764
                      14
                      37398
                      82
                      #t)
                   '(captures: local12234 arg2-12214)
                   (apply
                    local12234
                    arg0-12242
                    arg1-12243
                    (cons arg2-12214 rest12244)))))
             (_keyword-procedure-extract/method11356
              arg0-12212
              arg1-12213
              local12226
              arg3-12215))
           (lambda (arg0-12255 arg1-12256 . rest12257)
             '#(...me/private/kw.ss:769:10
                "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
                769
                10
                37609
                2475
                #t)
             '(captures: arg1-12213 arg2-12214 arg3-12215 #%modvars)
             (let ((localv12258 (box ?)) (localv12259 (box ?)))
               (begin
                 (set-boxes!
                  (localv12258 localv12259)
                  (if (_keyword-procedure?11326 arg2-12214)
                    (_check-kw-args11355 arg2-12214 arg0-12255)
                    (values '#f (car arg0-12255))))
                 (let ((local12266
                        (let ((local12267
                               (+
                                arg3-12215
                                (if (if (_keyword-method?11335 arg2-12214)
                                      '#t
                                      (_okm?11340 arg2-12214))
                                  '1
                                  '0))))
                          (if (>= arg1-12213 local12267)
                            (- arg1-12213 local12267)
                            arg1-12213))))
                   (let ((local12276
                          (if (if (null? rest12257) (null? arg0-12255) '#f)
                            (begin
                              rest12257
                              arg1-12256
                              arg0-12255
                              '"no arguments supplied")
                            (let ((with-handlers-handler212279
                                   (lambda (arg0-12280)
                                     '#(with-handlers-handler2
                                        "/Users/jay/Dev/svn/plt/collects/scheme/private/kw.ss"
                                        784
                                        27
                                        38304
                                        407
                                        #f)
                                     (regexp-replace
                                      '#rx"^.*? given: x; (other )?"
                                      (exn-message arg0-12280)
                                      '""))))
                              (let ((local12285
                                     (continuation-mark-set-first
                                      '#f
                                      |_break-enabled-key@(quote #%paramz)|)))
                                (_call-handled-body11322
                                 local12285
                                 (begin0
                                   (lambda (arg0-12291)
                                     '(captures:
                                       local12285
                                       with-handlers-handler212279
                                       #%modvars)
                                     (_select-handler/no-breaks11319
                                      arg0-12291
                                      local12285
                                      (list
                                       (cons
                                        exn:fail?
                                        with-handlers-handler212279))))
                                   local12285
                                   with-handlers-handler212279)
                                 (begin0
                                   (lambda ()
                                     '(captures:
                                       arg0-12255
                                       arg1-12256
                                       rest12257)
                                     (apply
                                      raise-type-error
                                      'x
                                      '"x"
                                      '0
                                      'x
                                      (append
                                       rest12257
                                       (apply
                                        append
                                        (map list arg0-12255 arg1-12256)))))
                                   arg0-12255
                                   arg1-12256
                                   rest12257)))))))
                     (raise
                      (make-exn:fail:contract
                       (if (unbox localv12259)
                         (begin
                           localv12258
                           local12266
                           (if (_keyword-procedure?11326 arg2-12214)
                             (format
                              (string-append
                               '"procedure application: procedure: ~e;"
                               '" does not expect an argument with keyword ~a; ~a")
                              arg2-12214
                              (unbox localv12259)
                              local12276)
                             (begin
                               localv12259
                               (format
                                (string-append
                                 '"procedure application: expected a procedure that"
                                 '" accepts keyword arguments, given ~e; ~a")
                                arg2-12214
                                local12276))))
                         (if (unbox localv12258)
                           (begin
                             local12266
                             (format
                              (string-append
                               '"procedure application: procedure: ~e; requires"
                               '" an argument with keyword ~a, not supplied; ~a")
                              arg2-12214
                              (unbox localv12258)
                              local12276))
                           (begin
                             localv12258
                             (format
                              (string-append
                               '"procedure application: no case matching ~a non-keyword"
                               '" argument~a for: ~e; ~a")
                              (- local12266 '2)
                              (if (= '1 (- local12266 '2)) '"" '"s")
                              arg2-12214
                              local12276))))
                       (current-continuation-marks)))))))))))))
  (define-values
   (_check-inspector11357)
   (lambda (arg0-12345 arg1-12346)
     '#(check-inspector
        "/Users/jay/Dev/svn/plt/collects/scheme/private/define-struct.ss"
        52
        2
        1892
        149
        #f)
     (begin
       (if arg1-12346
         (if (inspector? arg1-12346)
           (void)
           (raise-type-error arg0-12345 '"inspector or #f" arg1-12346))
         (void))
       arg1-12346)))
  (define-values
   (_print-values11358)
   (lambda rest12351
     '#(print-values
        "/Users/jay/Dev/svn/plt/collects/scheme/private/modbeg.ss"
        10
        4
        214
        41
        #f)
     (for-each (current-print) rest12351)))
  (define-values
   (_map211359)
   (let ((map12354
          (case-lambda
           ((arg0-12355 arg1-12356)
            '#(...e/private/map.ss:18:12
               "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
               18
               12
               527
               319
               #t)
            (if (if (procedure? arg0-12355)
                  (if (procedure-arity-includes? arg0-12355 '1)
                    (list? arg1-12356)
                    '#f)
                  '#f)
              ((lambda (arg0-12363 arg1-12364)
                 '#(loop
                    "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
                    22
                    17
                    675
                    142
                    #f)
                 (if (null? arg1-12364)
                   '()
                   (cons
                    (arg0-12363 (car arg1-12364))
                    (let ((local12370 (cdr arg1-12364)))
                      (if (null? local12370)
                        '()
                        (cons
                         (arg0-12363 (car local12370))
                         (let ((local12377 (cdr local12370)))
                           (if (null? local12377)
                             '()
                             (cons
                              (arg0-12363 (car local12377))
                              (let ((local12384 (cdr local12377)))
                                (if (null? local12384)
                                  '()
                                  (cons
                                   (arg0-12363 (car local12384))
                                   (loop1283
                                    arg0-12363
                                    (cdr local12384))))))))))))))
               arg0-12355
               arg1-12356)
              (map arg0-12355 arg1-12356)))
           ((arg0-12396 arg1-12397 arg2-12398)
            '#(...e/private/map.ss:27:12
               "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
               27
               12
               859
               474
               #t)
            (if (if (procedure? arg0-12396)
                  (if (procedure-arity-includes? arg0-12396 '2)
                    (if (list? arg1-12397)
                      (if (list? arg2-12398)
                        (= (length arg1-12397) (length arg2-12398))
                        '#f)
                      '#f)
                    '#f)
                  '#f)
              (if (null? arg1-12397)
                '()
                (cons
                 (arg0-12396 (car arg1-12397) (car arg2-12398))
                 (let ((local12415 (cdr arg1-12397)))
                   (let ((local12417 (cdr arg2-12398)))
                     (if (null? local12415)
                       '()
                       (cons
                        (arg0-12396 (car local12415) (car local12417))
                        (let ((local12426 (cdr local12415)))
                          (let ((local12428 (cdr local12417)))
                            (if (null? local12426)
                              '()
                              (cons
                               (arg0-12396 (car local12426) (car local12428))
                               ((lambda (arg0-12440 arg1-12441 arg2-12442)
                                  '#(loop
                                     "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
                                     33
                                     17
                                     1095
                                     205
                                     #f)
                                  (if (null? arg1-12441)
                                    '()
                                    (cons
                                     (arg0-12440
                                      (car arg1-12441)
                                      (car arg2-12442))
                                     (let ((local12450 (cdr arg1-12441)))
                                       (let ((local12452 (cdr arg2-12442)))
                                         (if (null? local12450)
                                           '()
                                           (cons
                                            (arg0-12440
                                             (car local12450)
                                             (car local12452))
                                            (let ((local12461
                                                   (cdr local12450)))
                                              (let ((local12463
                                                     (cdr local12452)))
                                                (if (null? local12461)
                                                  '()
                                                  (cons
                                                   (arg0-12440
                                                    (car local12461)
                                                    (car local12463))
                                                   (loop1284
                                                    arg0-12440
                                                    (cdr local12461)
                                                    (cdr
                                                     local12463)))))))))))))
                                arg0-12396
                                (cdr local12426)
                                (cdr local12428))))))))))))
              (map arg0-12396 arg1-12397 arg2-12398)))
           ((arg0-12482 . rest12483)
            '#(...e/private/map.ss:39:12
               "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
               39
               12
               1346
               31
               #t)
            (apply map arg0-12482 rest12483)))))
     map12354))
  (define-values
   (_for-each211360)
   (let ((for-each12487
          (case-lambda
           ((arg0-12488 arg1-12489)
            '#(...e/private/map.ss:45:12
               "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
               45
               12
               1472
               327
               #t)
            (if (if (procedure? arg0-12488)
                  (if (procedure-arity-includes? arg0-12488 '1)
                    (list? arg1-12489)
                    '#f)
                  '#f)
              (if (null? arg1-12489)
                (void)
                (begin
                  (arg0-12488 (car arg1-12489))
                  (let ((local12497 (cdr arg1-12489)))
                    (if (null? local12497)
                      (void)
                      (begin
                        (arg0-12488 (car local12497))
                        (let ((local12502 (cdr local12497)))
                          (if (null? local12502)
                            (void)
                            (begin
                              (arg0-12488 (car local12502))
                              (let ((local12507 (cdr local12502)))
                                (if (null? local12507)
                                  (void)
                                  (begin
                                    (arg0-12488 (car local12507))
                                    ((lambda (arg0-12514 arg1-12515)
                                       '#(loop
                                          "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
                                          49
                                          17
                                          1620
                                          145
                                          #f)
                                       (if (null? arg1-12515)
                                         (void)
                                         (begin
                                           (arg0-12514 (car arg1-12515))
                                           (let ((local12519 (cdr arg1-12515)))
                                             (if (null? local12519)
                                               (void)
                                               (begin
                                                 (arg0-12514 (car local12519))
                                                 (let ((local12524
                                                        (cdr local12519)))
                                                   (if (null? local12524)
                                                     (void)
                                                     (begin
                                                       (arg0-12514
                                                        (car local12524))
                                                       (let ((local12529
                                                              (cdr
                                                               local12524)))
                                                         (if (null? local12529)
                                                           (void)
                                                           (begin
                                                             (arg0-12514
                                                              (car local12529))
                                                             (let ((local12534
                                                                    (cdr
                                                                     local12529)))
                                                               (if (null?
                                                                    local12534)
                                                                 (void)
                                                                 (begin
                                                                   (arg0-12514
                                                                    (car
                                                                     local12534))
                                                                   (let ((local12539
                                                                          (cdr
                                                                           local12534)))
                                                                     (if (null?
                                                                          local12539)
                                                                       (void)
                                                                       (begin
                                                                         (arg0-12514
                                                                          (car
                                                                           local12539))
                                                                         (let ((local12544
                                                                                (cdr
                                                                                 local12539)))
                                                                           (if (null?
                                                                                local12544)
                                                                             (void)
                                                                             (begin
                                                                               (arg0-12514
                                                                                (car
                                                                                 local12544))
                                                                               (let ((local12549
                                                                                      (cdr
                                                                                       local12544)))
                                                                                 (if (null?
                                                                                      local12549)
                                                                                   (void)
                                                                                   (begin
                                                                                     (arg0-12514
                                                                                      (car
                                                                                       local12549))
                                                                                     (loop1285
                                                                                      arg0-12514
                                                                                      (cdr
                                                                                       local12549))))))))))))))))))))))))))
                                     arg0-12488
                                     (cdr local12507)))))))))))))
              (for-each arg0-12488 arg1-12489)))
           ((arg0-12560 arg1-12561 arg2-12562)
            '#(...e/private/map.ss:54:12
               "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
               54
               12
               1812
               483
               #t)
            (if (if (procedure? arg0-12560)
                  (if (procedure-arity-includes? arg0-12560 '2)
                    (if (list? arg1-12561)
                      (if (list? arg2-12562)
                        (= (length arg1-12561) (length arg2-12562))
                        '#f)
                      '#f)
                    '#f)
                  '#f)
              (if (null? arg1-12561)
                (void)
                (begin
                  (arg0-12560 (car arg1-12561) (car arg2-12562))
                  (let ((local12577 (cdr arg1-12561)))
                    (let ((local12579 (cdr arg2-12562)))
                      (if (null? local12577)
                        (void)
                        (begin
                          (arg0-12560 (car local12577) (car local12579))
                          (let ((local12586 (cdr local12577)))
                            (let ((local12588 (cdr local12579)))
                              (if (null? local12586)
                                (void)
                                (begin
                                  (arg0-12560
                                   (car local12586)
                                   (car local12588))
                                  ((lambda (arg0-12598 arg1-12599 arg2-12600)
                                     '#(loop
                                        "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
                                        60
                                        17
                                        2048
                                        209
                                        #f)
                                     (if (null? arg1-12599)
                                       (void)
                                       (begin
                                         (arg0-12598
                                          (car arg1-12599)
                                          (car arg2-12600))
                                         (let ((local12606 (cdr arg1-12599)))
                                           (let ((local12608 (cdr arg2-12600)))
                                             (if (null? local12606)
                                               (void)
                                               (begin
                                                 (arg0-12598
                                                  (car local12606)
                                                  (car local12608))
                                                 (let ((local12615
                                                        (cdr local12606)))
                                                   (let ((local12617
                                                          (cdr local12608)))
                                                     (if (null? local12615)
                                                       (void)
                                                       (begin
                                                         (arg0-12598
                                                          (car local12615)
                                                          (car local12617))
                                                         (let ((local12624
                                                                (cdr
                                                                 local12615)))
                                                           (let ((local12626
                                                                  (cdr
                                                                   local12617)))
                                                             (if (null?
                                                                  local12624)
                                                               (void)
                                                               (begin
                                                                 (arg0-12598
                                                                  (car
                                                                   local12624)
                                                                  (car
                                                                   local12626))
                                                                 (let ((local12633
                                                                        (cdr
                                                                         local12624)))
                                                                   (let ((local12635
                                                                          (cdr
                                                                           local12626)))
                                                                     (if (null?
                                                                          local12633)
                                                                       (void)
                                                                       (begin
                                                                         (arg0-12598
                                                                          (car
                                                                           local12633)
                                                                          (car
                                                                           local12635))
                                                                         (let ((local12642
                                                                                (cdr
                                                                                 local12633)))
                                                                           (let ((local12644
                                                                                  (cdr
                                                                                   local12635)))
                                                                             (if (null?
                                                                                  local12642)
                                                                               (void)
                                                                               (begin
                                                                                 (arg0-12598
                                                                                  (car
                                                                                   local12642)
                                                                                  (car
                                                                                   local12644))
                                                                                 (loop1286
                                                                                  arg0-12598
                                                                                  (cdr
                                                                                   local12642)
                                                                                  (cdr
                                                                                   local12644)))))))))))))))))))))))))
                                   arg0-12560
                                   (cdr local12586)
                                   (cdr local12588))))))))))))
              (for-each arg0-12560 arg1-12561 arg2-12562)))
           ((arg0-12661 . rest12662)
            '#(...e/private/map.ss:66:12
               "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
               66
               12
               2308
               36
               #t)
            (apply for-each arg0-12661 rest12662)))))
     for-each12487))
  (define-values
   (_andmap211361)
   (let ((andmap12666
          (case-lambda
           ((arg0-12667 arg1-12668)
            '#(...e/private/map.ss:72:12
               "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
               72
               12
               2438
               406
               #t)
            (if (if (procedure? arg0-12667)
                  (if (procedure-arity-includes? arg0-12667 '1)
                    (list? arg1-12668)
                    '#f)
                  '#f)
              (if (null? arg1-12668)
                '#t
                (if (null? (cdr arg1-12668))
                  (arg0-12667 (car arg1-12668))
                  (if (arg0-12667 (car arg1-12668))
                    (let ((local12680 (cdr arg1-12668)))
                      (if (null? (cdr local12680))
                        (arg0-12667 (car local12680))
                        (if (arg0-12667 (car local12680))
                          (let ((local12688 (cdr local12680)))
                            (if (null? (cdr local12688))
                              (arg0-12667 (car local12688))
                              (if (arg0-12667 (car local12688))
                                ((lambda (arg0-12698 arg1-12699)
                                   '#(loop
                                      "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
                                      78
                                      21
                                      2645
                                      166
                                      #f)
                                   (if (null? (cdr arg1-12699))
                                     (arg0-12698 (car arg1-12699))
                                     (if (arg0-12698 (car arg1-12699))
                                       (let ((local12706 (cdr arg1-12699)))
                                         (if (null? (cdr local12706))
                                           (arg0-12698 (car local12706))
                                           (if (arg0-12698 (car local12706))
                                             (let ((local12714
                                                    (cdr local12706)))
                                               (if (null? (cdr local12714))
                                                 (arg0-12698 (car local12714))
                                                 (if (arg0-12698
                                                      (car local12714))
                                                   (loop1287
                                                    arg0-12698
                                                    (cdr local12714))
                                                   '#f)))
                                             '#f)))
                                       '#f)))
                                 arg0-12667
                                 (cdr local12688))
                                '#f)))
                          '#f)))
                    '#f)))
              (andmap arg0-12667 arg1-12668)))
           ((arg0-12728 arg1-12729 arg2-12730)
            '#(...e/private/map.ss:83:12
               "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
               83
               12
               2857
               575
               #t)
            (if (if (procedure? arg0-12728)
                  (if (procedure-arity-includes? arg0-12728 '2)
                    (if (list? arg1-12729)
                      (if (list? arg2-12730)
                        (= (length arg1-12729) (length arg2-12730))
                        '#f)
                      '#f)
                    '#f)
                  '#f)
              (if (null? arg1-12729)
                '#t
                ((lambda (arg0-12744 arg1-12745 arg2-12746)
                   '#(loop
                      "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
                      91
                      21
                      3153
                      242
                      #f)
                   (if (null? (cdr arg1-12745))
                     (arg0-12744 (car arg1-12745) (car arg2-12746))
                     (if (arg0-12744 (car arg1-12745) (car arg2-12746))
                       (let ((local12757 (cdr arg1-12745)))
                         (let ((local12759 (cdr arg2-12746)))
                           (if (null? (cdr local12757))
                             (arg0-12744 (car local12757) (car local12759))
                             (if (arg0-12744 (car local12757) (car local12759))
                               (let ((local12771 (cdr local12757)))
                                 (let ((local12773 (cdr local12759)))
                                   (if (null? (cdr local12771))
                                     (arg0-12744
                                      (car local12771)
                                      (car local12773))
                                     (if (arg0-12744
                                          (car local12771)
                                          (car local12773))
                                       (loop1288
                                        arg0-12744
                                        (cdr local12771)
                                        (cdr local12773))
                                       '#f))))
                               '#f))))
                       '#f)))
                 arg0-12728
                 arg1-12729
                 arg2-12730))
              (andmap arg0-12728 arg1-12729 arg2-12730)))
           ((arg0-12793 . rest12794)
            '#(...e/private/map.ss:97:12
               "/Users/jay/Dev/svn/plt/collects/scheme/private/map.ss"
               97
               12
               3445
               34
               #t)
            (apply andmap arg0-12793 rest12794)))))
     andmap12666))
  (define-values
   (_new-apply11362)
   (_*make-keyword-procedure11353
    (lambda (arg0-12800 arg1-12801 arg2-12802 arg3-12803 . rest12804)
      '#(...vate/pre-base.ss:31:5
         "/Users/jay/Dev/svn/plt/collects/scheme/private/pre-base.ss"
         31
         5
         811
         103
         #t)
      '(captures: #%modvars)
      (_keyword-apply11354
       arg2-12802
       arg0-12800
       arg1-12801
       (apply list* arg3-12803 rest12804)))
    apply))
  (define-values
   (lift11466)
   (lambda (arg0-12812 arg1-12813)
     '#(loop
        "/Users/jay/Dev/svn/plt/collects/scheme/promise.ss"
        120
        4
        5549
        827
        #f)
     '(captures: #%modvars)
     (if (procedure? arg1-12813)
       (begin
         (_set-promise-val!11372
          arg0-12812
          (_make-running11376 (object-name arg1-12813)))
         (_call-with-exception-handler11323
          (lambda (arg0-12821)
            '#(...cheme/promise.ss:130:15
               "/Users/jay/Dev/svn/plt/collects/scheme/promise.ss"
               130
               15
               6107
               58
               #t)
            '(captures: arg0-12812 #%modvars)
            (begin
              (_set-promise-val!11372
               arg0-12812
               (_make-reraise11374 arg0-12821))
              arg0-12821))
          (lambda ()
            '#(...cheme/promise.ss:131:15
               "/Users/jay/Dev/svn/plt/collects/scheme/promise.ss"
               131
               15
               6181
               34
               #t)
            '(captures: arg1-12813 arg0-12812 #%modvars)
            (_force-proc11377 arg1-12813 arg0-12812))))
       (if (_promise?11368 arg1-12813)
         (let ((local12828 (_promise-val11369 arg1-12813)))
           (if (procedure? local12828)
             (begin
               (_set-promise-val!11372
                arg0-12812
                (_make-running11376 (object-name local12828)))
               (_call-with-exception-handler11323
                (lambda (arg0-12837)
                  '#(...cheme/promise.ss:130:15
                     "/Users/jay/Dev/svn/plt/collects/scheme/promise.ss"
                     130
                     15
                     6107
                     58
                     #t)
                  '(captures: arg0-12812 #%modvars)
                  (begin
                    (_set-promise-val!11372
                     arg0-12812
                     (_make-reraise11374 arg0-12837))
                    arg0-12837))
                (lambda ()
                  '#(...cheme/promise.ss:131:15
                     "/Users/jay/Dev/svn/plt/collects/scheme/promise.ss"
                     131
                     15
                     6181
                     34
                     #t)
                  '(captures: local12828 arg0-12812 #%modvars)
                  (_force-proc11377 local12828 arg0-12812))))
             (if (_promise?11368 local12828)
               (lift11466 arg0-12812 (_promise-val11369 local12828))
               (if (null? local12828)
                 (values)
                 (if (null? (cdr local12828))
                   (car local12828)
                   (apply values local12828))))))
         (if (null? arg1-12813)
           (values)
           (if (null? (cdr arg1-12813))
             (car arg1-12813)
             (apply values arg1-12813)))))))
  (define-values
   (lift11464)
   (lambda (arg0-12859 arg1-12860)
     '#(loop1
        "/Users/jay/Dev/svn/plt/collects/scheme/promise.ss"
        103
        2
        4859
        634
        #f)
     '(captures: #%modvars)
     (if (_promise?11368 arg1-12860)
       (lift11465 arg0-12859 arg1-12860)
       (begin
         (_set-promise-val!11372 arg0-12859 (list arg1-12860))
         arg1-12860))))
  (define-values
   (lift11465)
   (lambda (arg0-12867 arg1-12868)
     '#(loop2
        "/Users/jay/Dev/svn/plt/collects/scheme/promise.ss"
        105
        6
        4942
        432
        #f)
     '(captures: #%modvars)
     (let ((local12869 (_promise-val11369 arg1-12868)))
       (begin
         (_set-promise-val!11372 arg1-12868 arg0-12867)
         (if (procedure? local12869)
           (lift11464 arg0-12867 (local12869))
           (if (_promise?11368 local12869)
             (let ((local12877 (_promise-val11369 local12869)))
               (begin
                 (_set-promise-val!11372 local12869 arg0-12867)
                 (if (procedure? local12877)
                   (lift11464 arg0-12867 (local12877))
                   (if (_promise?11368 local12877)
                     (lift11465 arg0-12867 local12877)
                     (begin
                       (_set-promise-val!11372 arg0-12867 local12877)
                       (if (null? local12877)
                         (values)
                         (if (null? (cdr local12877))
                           (car local12877)
                           (apply values local12877))))))))
             (begin
               (_set-promise-val!11372 arg0-12867 local12869)
               (if (null? local12869)
                 (values)
                 (if (null? (cdr local12869))
                   (car local12869)
                   (apply values local12869))))))))))
  (define-values
   (lift11463)
   (lambda (arg0-12903 arg1-12904 arg2-12905)
     '#(loop
        "/Users/jay/Dev/svn/plt/collects/scheme/promise.ss"
        19
        2
        948
        1189
        #f)
     '(captures: #%modvars)
     (if (_reraise?11364 arg2-12905)
       (let ((local12907 (_reraise-val11365 arg2-12905)))
         (if (exn? local12907)
           (fprintf
            arg0-12903
            (if arg1-12904 '"#<promise!exn!~s>" '"#<promise!exn!~a>")
            (exn-message local12907))
           (fprintf
            arg0-12903
            (if arg1-12904 '"#<promise!~s>" '"#<promise!~a>")
            (list 'raise local12907))))
       (if (_running?11366 arg2-12905)
         (begin
           arg1-12904
           (let ((local12920 (_running-name11367 arg2-12905)))
             (if local12920
               (fprintf arg0-12903 '"#<promise:!running!~a>" local12920)
               (fprintf arg0-12903 '"#<promise:!running>"))))
         (if (procedure? arg2-12905)
           (let ((local12928 (object-name arg2-12905)))
             (if local12928
               (fprintf arg0-12903 '"#<promise:~a>" local12928)
               (display '"#<promise>" arg0-12903)))
           (if (_promise?11368 arg2-12905)
             (lift11463 arg0-12903 arg1-12904 (_promise-val11369 arg2-12905))
             (if (null? arg2-12905)
               (fprintf arg0-12903 '"#<promise!(values)>")
               (if (null? (cdr arg2-12905))
                 (fprintf
                  arg0-12903
                  (if arg1-12904 '"#<promise!~s>" '"#<promise!~a>")
                  (car arg2-12905))
                 (begin
                   (display '"#<promise!(values" arg0-12903)
                   (let ((local12951 (if arg1-12904 '" ~s" '" ~a")))
                     (for-each
                      (begin0
                        (lambda (arg0-12954)
                          '#(...cheme/promise.ss:44:23
                             "/Users/jay/Dev/svn/plt/collects/scheme/promise.ss"
                             44
                             23
                             2066
                             33
                             #t)
                          '(captures: local12951 arg0-12903)
                          (fprintf arg0-12903 local12951 arg0-12954))
                        local12951)
                      arg2-12905))
                   (display '")>" arg0-12903))))))))))
  (define-values
   (_promise-printer11363)
   (lambda (arg0-12960 arg1-12961 arg2-12962)
     '#(promise-printer
        "/Users/jay/Dev/svn/plt/collects/scheme/promise.ss"
        18
        0
        900
        1238
        #f)
     '(captures: #%modvars)
     (lift11463 arg1-12961 arg2-12962 (_promise-val11369 arg0-12960))))
  (define-values
   (_struct:promise11370
    _make-promise11371
    _promise?11368
    _promise-val11369
    _set-promise-val!11372)
   (let ((localv12967 ?)
         (localv12968 ?)
         (localv12969 ?)
         (localv12970 ?)
         (localv12971 ?))
     (begin
       (set!-values (localv12967
                     localv12968
                     localv12969
                     localv12970
                     localv12971)
         (make-struct-type
          'promise
          '#f
          '1
          '0
          '#f
          (list (cons prop:custom-write _promise-printer11363))
          (current-inspector)
          '#f
          '()
          '#f))
       (values
        localv12967
        localv12968
        localv12969
        (make-struct-field-accessor localv12970 '0 'val)
        (make-struct-field-mutator localv12971 '0 'val)))))
  (define-values
   (_struct:reraise11373 _make-reraise11374 _reraise?11364 _reraise-val11365)
   (let ((localv12996 ?)
         (localv12997 ?)
         (localv12998 ?)
         (localv12999 ?)
         (localv13000 ?))
     (begin
       (set!-values (localv12996
                     localv12997
                     localv12998
                     localv12999
                     localv13000)
         (make-struct-type
          'reraise
          '#f
          '1
          '0
          '#f
          (list
           (cons
            prop:procedure
            (lambda (arg0-13014)
              '#(...cheme/promise.ss:89:28
                 "/Users/jay/Dev/svn/plt/collects/scheme/promise.ss"
                 89
                 28
                 4268
                 42
                 #t)
              '(captures: #%modvars)
              (raise (_reraise-val11365 arg0-13014)))))
          (current-inspector)
          '#f
          '(0)
          '#f))
       (begin
         localv13000
         (values
          localv12996
          localv12997
          localv12998
          (make-struct-field-accessor localv12999 '0 'val))))))
  (define-values
   (_struct:running11375 _make-running11376 _running?11366 _running-name11367)
   (let ((localv13024 ?)
         (localv13025 ?)
         (localv13026 ?)
         (localv13027 ?)
         (localv13028 ?))
     (begin
       (set!-values (localv13024
                     localv13025
                     localv13026
                     localv13027
                     localv13028)
         (make-struct-type
          'running
          '#f
          '1
          '0
          '#f
          (list
           (cons
            prop:procedure
            (lambda (arg0-13042)
              '#(...cheme/promise.ss:91:28
                 "/Users/jay/Dev/svn/plt/collects/scheme/promise.ss"
                 91
                 28
                 4370
                 268
                 #t)
              '(captures: #%modvars)
              (let ((local13043 (_running-name11367 arg0-13042)))
                (if local13043
                  (error 'force '"reentrant promise ~v" local13043)
                  (error 'force '"reentrant promise"))))))
          (current-inspector)
          '#f
          '(0)
          '#f))
       (begin
         localv13028
         (values
          localv13024
          localv13025
          localv13026
          (make-struct-field-accessor localv13027 '0 'name))))))
  (define-values
   (_force-proc11377)
   (lambda (arg0-13057 arg1-13058)
     '#(force-proc
        "/Users/jay/Dev/svn/plt/collects/scheme/promise.ss"
        102
        0
        4829
        665
        #f)
     '(captures: #%modvars)
     (lift11464 arg1-13058 (arg0-13057))))
  (define-values
   (_force11378)
   (lambda (arg0-13061)
     '#(force
        "/Users/jay/Dev/svn/plt/collects/scheme/promise.ss"
        118
        0
        5496
        951
        #f)
     '(captures: #%modvars)
     (if (_promise?11368 arg0-13061)
       (lift11466 arg0-13061 (_promise-val11369 arg0-13061))
       arg0-13061)))
  (define-values
   (_user-collects-dir11379)
   (_make-promise11371
    (lambda ()
      '#(user-collects-dir
         "/Users/jay/Dev/svn/plt/collects/setup/dirs.ss"
         49
         2
         1813
         67
         #f)
      '(captures: #%modvars)
      (_make-promise11371
       (call-with-values
        (lambda ()
          (build-path
           (simplify-path (cleanse-path (find-system-path 'addon-dir)) '#f)
           (version)
           '"collects"))
        list)))))
  (define-values
   (_user-dir.1011380)
   (_make-promise11371
    (lambda ()
      '#(user-dir
         "/Users/jay/Dev/svn/plt/collects/setup/dirs.ss"
         101
         9
         3638
         64
         #f)
      '(captures: #%modvars)
      (_make-promise11371
       (call-with-values
        (lambda ()
          (build-path
           (simplify-path (cleanse-path (find-system-path 'addon-dir)) '#f)
           (version)
           (let ((local13084 (system-type)))
             (if (memv local13084 '(windows macosx))
               'same
               (if (eq? local13084 'unix) '"bin" (void))))))
        list)))))
  (define-values
   (lift11467)
   (lambda (arg0-13089 arg1-13090 arg2-13091 arg3-13092)
     '#(loop
        "/Users/jay/Dev/svn/plt/collects/setup/path-relativize.ss"
        38
        4
        1494
        662
        #f)
     '(captures: #%modvars)
     (if (null? arg3-13092)
       (begin
         arg3-13092
         arg0-13089
         (cons arg1-13090 (_map211359 path-element->bytes arg2-13091)))
       (if (null? arg2-13091)
         arg0-13089
         (if (equal?
              (|_normal-case-path@(quote #%utils)| (car arg2-13091))
              (|_normal-case-path@(quote #%utils)| (car arg3-13092)))
           (let ((local13105 (cdr arg2-13091)))
             (let ((local13107 (cdr arg3-13092)))
               (if (null? local13107)
                 (begin
                   arg0-13089
                   local13107
                   (cons
                    arg1-13090
                    (_map211359 path-element->bytes local13105)))
                 (if (null? local13105)
                   arg0-13089
                   (if (equal?
                        (|_normal-case-path@(quote #%utils)| (car local13105))
                        (|_normal-case-path@(quote #%utils)| (car local13107)))
                     (let ((local13121 (cdr local13105)))
                       (let ((local13123 (cdr local13107)))
                         (if (null? local13123)
                           (begin
                             arg0-13089
                             local13123
                             (cons
                              arg1-13090
                              (_map211359 path-element->bytes local13121)))
                           (if (null? local13121)
                             arg0-13089
                             (if (equal?
                                  (|_normal-case-path@(quote #%utils)|
                                   (car local13121))
                                  (|_normal-case-path@(quote #%utils)|
                                   (car local13123)))
                               (lift11467
                                arg0-13089
                                arg1-13090
                                (cdr local13121)
                                (cdr local13123))
                               arg0-13089)))))
                     arg0-13089)))))
           arg0-13089)))))
  (define-values
   (_make-relativize11381)
   (lambda (arg0-13143 arg1-13144 arg2-13145 arg3-13146)
     '#(make-relativize
        "/Users/jay/Dev/svn/plt/collects/setup/path-relativize.ss"
        6
        0
        72
        3317
        #f)
     '(captures: #%modvars)
     (let ((localv13147 (box ?))
           (localv13148 (box ?))
           (localv13149 (box ?))
           (localv13150 (box ?)))
       (begin
         (set-boxes!
          (localv13147)
          (lambda (arg0-13151)
            '#(explode-path
               "/Users/jay/Dev/svn/plt/collects/setup/path-relativize.ss"
               25
               2
               1039
               228
               #f)
            (let ((localv13152 ?) (localv13153 ?) (localv13154 ?))
              (begin
                (set!-values (localv13152 localv13153 localv13154)
                  (split-path arg0-13151))
                (begin
                  localv13154
                  (if (path? localv13152)
                    (let ((local13157 (cons localv13153 '())))
                      (let ((localv13160 ?) (localv13161 ?) (localv13162 ?))
                        (begin
                          (set!-values (localv13160 localv13161 localv13162)
                            (split-path localv13152))
                          (begin
                            localv13162
                            (if (path? localv13160)
                              (let ((local13165 (cons localv13161 local13157)))
                                (let ((localv13168 ?)
                                      (localv13169 ?)
                                      (localv13170 ?))
                                  (begin
                                    (set!-values (localv13168
                                                  localv13169
                                                  localv13170)
                                      (split-path localv13160))
                                    (begin
                                      localv13170
                                      (if (path? localv13168)
                                        (let ((local13173
                                               (cons localv13169 local13165)))
                                          (let ((localv13176 ?)
                                                (localv13177 ?)
                                                (localv13178 ?))
                                            (begin
                                              (set!-values (localv13176
                                                            localv13177
                                                            localv13178)
                                                (split-path localv13168))
                                              (begin
                                                localv13178
                                                (if (path? localv13176)
                                                  (let ((local13181
                                                         (cons
                                                          localv13177
                                                          local13173)))
                                                    (let ((localv13184 ?)
                                                          (localv13185 ?)
                                                          (localv13186 ?))
                                                      (begin
                                                        (set!-values (localv13184
                                                                      localv13185
                                                                      localv13186)
                                                          (split-path
                                                           localv13176))
                                                        (begin
                                                          localv13186
                                                          (if (path?
                                                               localv13184)
                                                            (let ((local13189
                                                                   (cons
                                                                    localv13185
                                                                    local13181)))
                                                              (let ((localv13192
                                                                     ?)
                                                                    (localv13193
                                                                     ?)
                                                                    (localv13194
                                                                     ?))
                                                                (begin
                                                                  (set!-values (localv13192
                                                                                localv13193
                                                                                localv13194)
                                                                    (split-path
                                                                     localv13184))
                                                                  (begin
                                                                    localv13194
                                                                    (if (path?
                                                                         localv13192)
                                                                      ((lambda (arg0-13199
                                                                                arg1-13200)
                                                                         '#(loop
                                                                            "/Users/jay/Dev/svn/plt/collects/setup/path-relativize.ss"
                                                                            26
                                                                            4
                                                                            1076
                                                                            190
                                                                            #f)
                                                                         (let ((localv13201
                                                                                ?)
                                                                               (localv13202
                                                                                ?)
                                                                               (localv13203
                                                                                ?))
                                                                           (begin
                                                                             (set!-values (localv13201
                                                                                           localv13202
                                                                                           localv13203)
                                                                               (split-path
                                                                                arg0-13199))
                                                                             (begin
                                                                               localv13203
                                                                               (if (path?
                                                                                    localv13201)
                                                                                 (let ((local13206
                                                                                        (cons
                                                                                         localv13202
                                                                                         arg1-13200)))
                                                                                   (let ((localv13209
                                                                                          ?)
                                                                                         (localv13210
                                                                                          ?)
                                                                                         (localv13211
                                                                                          ?))
                                                                                     (begin
                                                                                       (set!-values (localv13209
                                                                                                     localv13210
                                                                                                     localv13211)
                                                                                         (split-path
                                                                                          localv13201))
                                                                                       (begin
                                                                                         localv13211
                                                                                         (if (path?
                                                                                              localv13209)
                                                                                           (let ((local13214
                                                                                                  (cons
                                                                                                   localv13210
                                                                                                   local13206)))
                                                                                             (let ((localv13217
                                                                                                    ?)
                                                                                                   (localv13218
                                                                                                    ?)
                                                                                                   (localv13219
                                                                                                    ?))
                                                                                               (begin
                                                                                                 (set!-values (localv13217
                                                                                                               localv13218
                                                                                                               localv13219)
                                                                                                   (split-path
                                                                                                    localv13209))
                                                                                                 (begin
                                                                                                   localv13219
                                                                                                   (if (path?
                                                                                                        localv13217)
                                                                                                     (loop2074
                                                                                                      localv13217
                                                                                                      (cons
                                                                                                       localv13218
                                                                                                       local13214))
                                                                                                     (cons
                                                                                                      localv13218
                                                                                                      local13214))))))
                                                                                           (cons
                                                                                            localv13210
                                                                                            local13206))))))
                                                                                 (cons
                                                                                  localv13202
                                                                                  arg1-13200))))))
                                                                       localv13192
                                                                       (cons
                                                                        localv13193
                                                                        local13189))
                                                                      (cons
                                                                       localv13193
                                                                       local13189))))))
                                                            (cons
                                                             localv13185
                                                             local13181))))))
                                                  (cons
                                                   localv13177
                                                   local13173))))))
                                        (cons localv13169 local13165))))))
                              (cons localv13161 local13157))))))
                    (cons localv13153 '())))))))
         (begin
           (set-boxes!
            (localv13148)
            (_make-promise11371
             (lambda ()
               '#(main-dir/
                  "/Users/jay/Dev/svn/plt/collects/setup/path-relativize.ss"
                  33
                  4
                  1293
                  80
                  #f)
               '(captures: localv13147 arg0-13143 #%modvars)
               (_make-promise11371
                (call-with-values
                 (lambda ()
                   (let ((local13248 (arg0-13143)))
                     (if local13248
                       ((unbox localv13147)
                        (simplify-path (path->complete-path local13248)))
                       '#f)))
                 list)))))
           (begin
             (set-boxes!
              (localv13149)
              (lambda (arg0-13252)
                '#(path->main-relative*
                   "/Users/jay/Dev/svn/plt/collects/setup/path-relativize.ss"
                   37
                   2
                   1454
                   703
                   #f)
                '(captures:
                  localv13147
                  localv13148
                  arg1-13144
                  arg2-13145
                  #%modvars)
                (lift11467
                 arg0-13252
                 arg1-13144
                 (let ((local13257
                        (if (bytes? arg0-13252)
                          (begin arg2-13145 (bytes->path arg0-13252))
                          (if (|_path-string?@(quote #%utils)| arg0-13252)
                            (begin arg2-13145 arg0-13252)
                            (raise-type-error
                             arg2-13145
                             '"path, string, or bytes"
                             arg0-13252)))))
                   ((unbox localv13147)
                    (simplify-path (path->complete-path local13257))))
                 (_force11378 (unbox localv13148)))))
             (begin
               (set-boxes!
                (localv13150)
                (lambda (arg0-13268)
                  '#(main-relative->path*
                     "/Users/jay/Dev/svn/plt/collects/setup/path-relativize.ss"
                     54
                     2
                     2228
                     1096
                     #f)
                  '(captures: arg0-13143 arg1-13144 arg3-13146 #%modvars)
                  (if (if (pair? arg0-13268)
                        (if (eq? arg1-13144 (car arg0-13268))
                          (let ((local13273 (bytes? (cdr arg0-13268))))
                            (if local13273
                              local13273
                              (begin
                                local13273
                                (if (list? (cdr arg0-13268))
                                  (_andmap211361 bytes? (cdr arg0-13268))
                                  '#f))))
                          '#f)
                        '#f)
                    (begin
                      arg3-13146
                      arg1-13144
                      (let ((local13281
                             (let ((local13282 (arg0-13143)))
                               (if local13282
                                 local13282
                                 (begin
                                   local13282
                                   (find-system-path 'orig-dir))))))
                        (if (bytes? (cdr arg0-13268))
                          (if (equal? (cdr arg0-13268) '#"")
                            local13281
                            (build-path
                             local13281
                             (bytes->path (cdr arg0-13268))))
                          (_new-apply11362
                           build-path
                           local13281
                           (_map211359
                            bytes->path-element
                            (cdr arg0-13268))))))
                    (begin
                      arg0-13143
                      (if (path? arg0-13268)
                        arg0-13268
                        (if (bytes? arg0-13268)
                          (bytes->path arg0-13268)
                          (if (string? arg0-13268)
                            (string->path arg0-13268)
                            (raise-type-error
                             arg3-13146
                             (format
                              '"path, string, bytes, or a list beginning with ~a"
                              arg1-13144)
                             arg0-13268))))))))
               (values (unbox localv13149) (unbox localv13150)))))))))
  (define-values
   (_path->main-collects-relative11382 _main-collects-relative->path11383)
   (_make-relativize11381
    _user-collects-dir11379
    'collects
    'path->main-collects-relative
    'main-collects-relative->path))
  (define-values
   (_extract-module-directory11384)
   (lambda (arg0-13315)
     '#(extract-module-directory
        "/Users/jay/Dev/svn/plt/collects/mzlib/etc.ss"
        254
        0
        9385
        420
        #f)
     (let ((local13316
            (let ((local13317 (syntax-source-module arg0-13315)))
              (if (module-path-index? local13317)
                (module-path-index-resolve local13317)
                local13317))))
       (let ((local13321 (resolved-module-path-name local13316)))
         (if (path? local13321)
           (let ((localv13324 ?) (localv13325 ?) (localv13326 ?))
             (begin
               (set!-values (localv13324 localv13325 localv13326)
                 (split-path local13321))
               (begin
                 localv13326
                 localv13325
                 (if (path? localv13324) localv13324 '#f))))
           '#f)))))
  (define-values (_insp11385) (current-inspector))
  (define-values
   (_struct:class11386
    _make-class11387
    _class?11388
    _class-name11389
    _class-pos11390
    _class-supers11391
    _class-self-interface11392
    _class-insp-mk11393
    _class-method-width11394
    _class-method-ht11395
    _class-method-ids11396
    _class-methods11397
    _class-beta-methods11398
    _class-meth-flags11399
    _class-field-width11400
    _class-field-ht11401
    _class-field-ids11402
    _class-struct:object11403
    _class-object?11404
    _class-make-object11405
    _class-field-ref11406
    _class-field-set!11407
    _class-init-args11408
    _class-init-mode11409
    _class-init11410
    _class-serializer11411
    _class-fixup11412
    _class-no-super-init?11413
    _set-class-struct:object!11414
    _set-class-object?!11415
    _set-class-make-object!11416
    _set-class-field-ref!11417
    _set-class-field-set!!11418
    _set-class-init!11419
    _set-class-serializer!11420
    _set-class-fixup!11421)
   (let ((localv13329 ?)
         (localv13330 ?)
         (localv13331 ?)
         (localv13332 ?)
         (localv13333 ?))
     (begin
       (set!-values (localv13329
                     localv13330
                     localv13331
                     localv13332
                     localv13333)
         (make-struct-type
          'class
          '#f
          '25
          '0
          '#f
          '()
          (_check-inspector11357 'define-struct _insp11385)
          '#f
          '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 19 20 24)
          '#f))
       (values
        localv13329
        localv13330
        localv13331
        (make-struct-field-accessor localv13332 '0 'name)
        (make-struct-field-accessor localv13332 '1 'pos)
        (make-struct-field-accessor localv13332 '2 'supers)
        (make-struct-field-accessor localv13332 '3 'self-interface)
        (make-struct-field-accessor localv13332 '4 'insp-mk)
        (make-struct-field-accessor localv13332 '5 'method-width)
        (make-struct-field-accessor localv13332 '6 'method-ht)
        (make-struct-field-accessor localv13332 '7 'method-ids)
        (make-struct-field-accessor localv13332 '8 'methods)
        (make-struct-field-accessor localv13332 '9 'beta-methods)
        (make-struct-field-accessor localv13332 '10 'meth-flags)
        (make-struct-field-accessor localv13332 '11 'field-width)
        (make-struct-field-accessor localv13332 '12 'field-ht)
        (make-struct-field-accessor localv13332 '13 'field-ids)
        (make-struct-field-accessor localv13332 '14 'struct:object)
        (make-struct-field-accessor localv13332 '15 'object?)
        (make-struct-field-accessor localv13332 '16 'make-object)
        (make-struct-field-accessor localv13332 '17 'field-ref)
        (make-struct-field-accessor localv13332 '18 'field-set!)
        (make-struct-field-accessor localv13332 '19 'init-args)
        (make-struct-field-accessor localv13332 '20 'init-mode)
        (make-struct-field-accessor localv13332 '21 'init)
        (make-struct-field-accessor localv13332 '22 'serializer)
        (make-struct-field-accessor localv13332 '23 'fixup)
        (make-struct-field-accessor localv13332 '24 'no-super-init?)
        (make-struct-field-mutator localv13333 '14 'struct:object)
        (make-struct-field-mutator localv13333 '15 'object?)
        (make-struct-field-mutator localv13333 '16 'make-object)
        (make-struct-field-mutator localv13333 '17 'field-ref)
        (make-struct-field-mutator localv13333 '18 'field-set!)
        (make-struct-field-mutator localv13333 '21 'init)
        (make-struct-field-mutator localv13333 '22 'serializer)
        (make-struct-field-mutator localv13333 '23 'fixup)))))
  (define-values
   (_prop:object11430 _object?11432 _object-ref11429)
   (make-struct-type-property 'object))
  (define-values
   (_struct:interface11427
    _make-interface11428
    _interface?11424
    _interface-name11425
    _interface-supers11433
    _interface-all-implemented11434
    _interface-public-ids11426
    _interface-class11435
    _interface-properties11431
    _set-interface-all-implemented!11436
    _set-interface-class!11437)
   (let ((localv13482 ?)
         (localv13483 ?)
         (localv13484 ?)
         (localv13485 ?)
         (localv13486 ?))
     (begin
       (set!-values (localv13482
                     localv13483
                     localv13484
                     localv13485
                     localv13486)
         (make-struct-type
          'interface
          '#f
          '6
          '0
          '#f
          '()
          (_check-inspector11357 'define-struct _insp11385)
          '#f
          '(0 1 3 5)
          '#f))
       (values
        localv13482
        localv13483
        localv13484
        (make-struct-field-accessor localv13485 '0 'name)
        (make-struct-field-accessor localv13485 '1 'supers)
        (make-struct-field-accessor localv13485 '2 'all-implemented)
        (make-struct-field-accessor localv13485 '3 'public-ids)
        (make-struct-field-accessor localv13485 '4 'class)
        (make-struct-field-accessor localv13485 '5 'properties)
        (make-struct-field-mutator localv13486 '2 'all-implemented)
        (make-struct-field-mutator localv13486 '4 'class)))))
  (define-values
   (_object<%>11438)
   ((let ((local13540 _struct:interface11427))
      (let ((localv13541 ?)
            (localv13542 ?)
            (localv13543 ?)
            (localv13544 ?)
            (localv13545 ?))
        (begin
          (set!-values (localv13541
                        localv13542
                        localv13543
                        localv13544
                        localv13545)
            (make-struct-type
             'interface:object%
             local13540
             '0
             '0
             '#f
             '()
             _insp11385))
          localv13542)))
    'object%
    '()
    '#f
    '()
    '#f
    '()))
  (call-with-values
   (lambda ()
     (let ((local13553 _object<%>11438))
       (let ((local13554 (make-hasheq)))
         (begin
           (hash-set! local13554 local13553 '#t)
           (_for-each211360
            (lambda (arg0-13560)
              '#(...lass-internal.ss:2510:16
                 "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
                 2510
                 16
                 102257
                 116
                 #t)
              '(captures: local13554 #%modvars)
              (hash-for-each
               (_interface-all-implemented11434 arg0-13560)
               (lambda (arg0-13564 arg1-13565)
                 '#(...lass-internal.ss:2513:19
                    "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
                    2513
                    19
                    102330
                    41
                    #t)
                 '(captures: local13554)
                 (hash-set! local13554 arg0-13564 '#t))))
            (_interface-supers11433 local13553))
           (_set-interface-all-implemented!11436 local13553 local13554)))))
   _print-values11358)
  (define-values
   (_object%11423)
   ((let ((local13597 _struct:class11386))
      (let ((localv13598 ?)
            (localv13599 ?)
            (localv13600 ?)
            (localv13601 ?)
            (localv13602 ?))
        (begin
          (set!-values (localv13598
                        localv13599
                        localv13600
                        localv13601
                        localv13602)
            (make-struct-type
             'class:object%
             local13597
             '0
             '0
             '#f
             '()
             _insp11385))
          localv13599)))
    'object%
    '0
    (vector '#f)
    _object<%>11438
    void
    '0
    (make-hasheq)
    '()
    (vector)
    (vector)
    (vector)
    '0
    (make-hasheq)
    '()
    'struct:object
    _object?11432
    'make-object
    'field-ref-not-needed
    'field-set!-not-needed
    '()
    'normal
    (lambda (arg0-13611 arg1-13612 arg2-13613 arg3-13614 arg4-13615 arg5-13616)
      '#(...lass-internal.ss:2566:19
         "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
         2566
         19
         103783
         143
         #t)
      '(captures: #%modvars)
      (begin
        arg4-13615
        arg3-13614
        arg2-13613
        arg1-13612
        (begin
          (if (null? arg5-13616)
            (void)
            (_unused-args-error11439 arg0-13611 arg5-13616))
          (void))))
    (lambda (arg0-13620)
      '#(...lass-internal.ss:2571:19
         "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
         2571
         19
         103933
         20
         #t)
      '#(()))
    (lambda (arg0-13621 arg1-13622)
      '#(...lass-internal.ss:2572:19
         "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
         2572
         19
         103978
         26
         #t)
      (void))
    '#t))
  (call-with-values
   (lambda ()
     (vector-set! (_class-supers11391 _object%11423) '0 _object%11423))
   _print-values11358)
  (call-with-values
   (lambda ()
     (let ((localv13627 ?)
           (localv13628 ?)
           (localv13629 ?)
           (localv13630 ?)
           (localv13631 ?))
       (begin
         (set!-values (localv13627
                       localv13628
                       localv13629
                       localv13630
                       localv13631)
           (make-struct-type
            'object
            '#f
            '0
            '0
            '#f
            (list (cons _prop:object11430 _object%11423))
            '#f))
         (begin
           localv13631
           localv13630
           localv13629
           (begin
             (_set-class-struct:object!11414 _object%11423 localv13627)
             (_set-class-make-object!11416 _object%11423 localv13628))))))
   _print-values11358)
  (call-with-values
   (lambda () (_set-class-object?!11415 _object%11423 _object?11432))
   _print-values11358)
  (call-with-values
   (lambda () (_set-interface-class!11437 _object<%>11438 _object%11423))
   _print-values11358)
  (define-values
   (_make-named-arg-string11440)
   (lambda (arg0-13650)
     '#(make-named-arg-string
        "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
        2827
        2
        113751
        287
        #f)
     ((lambda (arg0-13653 arg1-13654)
        '#(loop
           "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
           2828
           4
           113792
           245
           #f)
        (if (null? arg0-13653)
          '""
          (if (= arg1-13654 '3)
            '" ..."
            (let ((local13658
                   (let ((local13659 (cdr arg0-13653)))
                     (let ((local13661 (add1 arg1-13654)))
                       (if (null? local13659)
                         '""
                         (if (= local13661 '3)
                           '" ..."
                           (let ((local13666
                                  (let ((local13667 (cdr local13659)))
                                    (let ((local13669 (add1 local13661)))
                                      (if (null? local13667)
                                        '""
                                        (if (= local13669 '3)
                                          '" ..."
                                          (let ((local13674
                                                 (loop3414
                                                  (cdr local13667)
                                                  (add1 local13669))))
                                            (format
                                             '" (~a ~e)~a"
                                             (caar local13667)
                                             (cdar local13667)
                                             local13674))))))))
                             (format
                              '" (~a ~e)~a"
                              (caar local13659)
                              (cdar local13659)
                              local13666))))))))
              (format
               '" (~a ~e)~a"
               (caar arg0-13653)
               (cdar arg0-13653)
               local13658)))))
      arg0-13650
      '0)))
  (define-values
   (_unused-args-error11439)
   (lambda (arg0-13697 arg1-13698)
     '#(unused-args-error
        "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
        2838
        2
        114042
        244
        #f)
     '(captures: #%modvars)
     (let ((local13699 (_make-named-arg-string11440 arg1-13698)))
       (_obj-error11422
        'instantiate
        '"unused initialization arguments:~a~a"
        local13699
        (_for-class/which11441
         '"instantiated"
         (_class-name11389 (_object-ref11429 arg0-13697)))))))
  (define-values
   (_struct:exn:fail:object11442
    _make-exn:fail:object11443
    _exn:fail:object?11444)
   (let ((localv13709 ?)
         (localv13710 ?)
         (localv13711 ?)
         (localv13712 ?)
         (localv13713 ?))
     (begin
       (set!-values (localv13709
                     localv13710
                     localv13711
                     localv13712
                     localv13713)
         (make-struct-type
          'exn:fail:object
          struct:exn:fail
          '0
          '0
          '#f
          '()
          (_check-inspector11357 'define-struct _insp11385)
          '#f
          '()
          '#f))
       (values localv13709 localv13710 localv13711))))
  (define-values
   (_obj-error11422)
   (lambda (arg0-13729 . rest13730)
     '#(obj-error
        "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
        3681
        2
        147456
        177
        #f)
     '(captures: #%modvars)
     (raise
      (_make-exn:fail:object11443
       (string-append
        (format '"~a: " arg0-13729)
        (_new-apply11362 format rest13730))
       (current-continuation-marks)))))
  (define-values
   (_for-class/which11441)
   (lambda (arg0-13740 arg1-13741)
     '#(for-class/which
        "/Users/jay/Dev/svn/plt/collects/scheme/private/class-internal.ss"
        3688
        2
        147713
        94
        #f)
     (if arg1-13741 (format '" for ~a class: ~a" arg0-13740 arg1-13741) '"")))
  (define-values
   (lift11469)
   (lambda (arg0-13745)
     '#(...cheme/foreign.ss:194:20
        "/Users/jay/Dev/svn/plt/collects/scheme/foreign.ss"
        194
        20
        8891
        115
        #t)
     '(captures: #%modvars)
     (if (file-exists? arg0-13745)
       (let ((local13747 (path->complete-path (cleanse-path arg0-13745))))
         (|_ffi-lib@(quote #%foreign)| local13747 '#t))
       '#f)))
  (define-values
   (lift11468)
   (lambda (arg0-13752)
     '#(ffi-lib*
        "/Users/jay/Dev/svn/plt/collects/scheme/foreign.ss"
        181
        25
        8201
        33
        #f)
     '(captures: #%modvars)
     (|_ffi-lib@(quote #%foreign)| arg0-13752 '#t)))
  (define-values
   (_lib-suffix11445)
   (bytes->string/latin-1 (subbytes (system-type 'so-suffix) '1)))
  (define-values
   (_lib-suffix-re11446)
   (regexp (string-append '"\\." _lib-suffix11445 '"$")))
  (define-values
   (_suffix-before-version?11447)
   (not (equal? _lib-suffix11445 '"dylib")))
  (define-values
   (_get-ffi-lib11448)
   (case-lambda
    ((arg0-13766)
     '#(...cheme/foreign.ss:150:3
        "/Users/jay/Dev/svn/plt/collects/scheme/foreign.ss"
        150
        3
        6375
        30
        #t)
     '(captures: #%modvars)
     (_get-ffi-lib11448 arg0-13766 '""))
    ((arg0-13769 arg1-13770)
     '#(...cheme/foreign.ss:151:3
        "/Users/jay/Dev/svn/plt/collects/scheme/foreign.ss"
        151
        3
        6409
        2839
        #t)
     '(captures: #%modvars)
     (if arg0-13769
       (if (if (string? arg0-13769) '#t (path? arg0-13769))
         (let ((local13773
                (if (list? arg1-13770) arg1-13770 (list arg1-13770))))
           (let ((local13776
                  (_map211359
                   (lambda (arg0-13779)
                     '#(...cheme/foreign.ss:167:29
                        "/Users/jay/Dev/svn/plt/collects/scheme/foreign.ss"
                        167
                        29
                        7364
                        145
                        #t)
                     (if (if arg0-13779 (zero? (string-length arg0-13779)) '#t)
                       '""
                       (string-append '"." arg0-13779)))
                   local13773)))
             (let ((local13784 (absolute-path? arg0-13769)))
               (let ((local13786 (path->string (cleanse-path arg0-13769))))
                 (let ((local13789
                        (_map211359
                         (if (regexp-match _lib-suffix-re11446 local13786)
                           (lambda (arg0-13794)
                             '#(...cheme/foreign.ss:175:28
                                "/Users/jay/Dev/svn/plt/collects/scheme/foreign.ss"
                                175
                                28
                                7855
                                36
                                #t)
                             '(captures: local13786)
                             (string-append local13786 arg0-13794))
                           (lambda (arg0-13797)
                             '#(...cheme/foreign.ss:176:28
                                "/Users/jay/Dev/svn/plt/collects/scheme/foreign.ss"
                                176
                                28
                                7920
                                217
                                #t)
                             '(captures: local13786 #%modvars)
                             (if _suffix-before-version?11447
                               (string-append
                                local13786
                                '"."
                                _lib-suffix11445
                                arg0-13797)
                               (string-append
                                local13786
                                arg0-13797
                                '"."
                                _lib-suffix11445))))
                         local13776)))
                   (let ((local13806
                          (if local13784
                            '#f
                            (ormap
                             (lambda (arg0-13809)
                               '#(...cheme/foreign.ss:184:25
                                  "/Users/jay/Dev/svn/plt/collects/scheme/foreign.ss"
                                  184
                                  25
                                  8351
                                  318
                                  #t)
                               '(captures: local13786 local13789 #%modvars)
                               (let ((local13810
                                      (ormap
                                       (lambda (arg0-13813)
                                         '#(...cheme/foreign.ss:186:38
                                            "/Users/jay/Dev/svn/plt/collects/scheme/foreign.ss"
                                            186
                                            38
                                            8469
                                            88
                                            #t)
                                         '(captures: arg0-13809 #%modvars)
                                         (let ((local13814
                                                (build-path
                                                 arg0-13809
                                                 arg0-13813)))
                                           (|_ffi-lib@(quote #%foreign)|
                                            local13814
                                            '#t)))
                                       local13789)))
                                 (if local13810
                                   local13810
                                   (begin
                                     local13810
                                     (let ((local13819
                                            (build-path
                                             arg0-13809
                                             local13786)))
                                       (|_ffi-lib@(quote #%foreign)|
                                        local13819
                                        '#t))))))
                             (_user-dir.1011380)))))
                     (if local13806
                       local13806
                       (begin
                         local13806
                         (let ((local13824 (ormap lift11468 local13789)))
                           (if local13824
                             local13824
                             (begin
                               local13824
                               (let ((local13827
                                      (|_ffi-lib@(quote #%foreign)|
                                       local13786
                                       '#t)))
                                 (if local13827
                                   local13827
                                   (begin
                                     local13827
                                     (let ((local13830
                                            (ormap lift11469 local13789)))
                                       (if local13830
                                         local13830
                                         (begin
                                           local13830
                                           (let ((local13833
                                                  (if (file-exists? local13786)
                                                    (let ((local13835
                                                           (path->complete-path
                                                            (cleanse-path
                                                             local13786))))
                                                      (|_ffi-lib@(quote #%foreign)|
                                                       local13835
                                                       '#t))
                                                    '#f)))
                                             (if local13833
                                               local13833
                                               (|_ffi-lib@(quote #%foreign)|
                                                (car
                                                 local13789)))))))))))))))))))))
         (raise-type-error 'ffi-lib '"library-name" arg0-13769))
       (|_ffi-lib@(quote #%foreign)| arg0-13769)))))
  (define-values (_held-callbacks11450) (make-weak-hasheq))
  (define-values
   (__cprocedure*11449)
   (lambda (arg0-13846 arg1-13847 arg2-13848 arg3-13849 arg4-13850 arg5-13851)
     '#(_cprocedure*
        "/Users/jay/Dev/svn/plt/collects/scheme/foreign.ss"
        489
        0
        22167
        689
        #f)
     '(captures: #%modvars)
     (if arg3-13849
       (|_make-ctype@(quote #%foreign)|
        |__fpointer@(quote #%foreign)|
        (lambda (arg0-13855)
          '#(...cheme/foreign.ss:492:6
             "/Users/jay/Dev/svn/plt/collects/scheme/foreign.ss"
             492
             6
             22297
             443
             #t)
          '(captures:
            arg0-13846
            arg1-13847
            arg2-13848
            arg3-13849
            arg4-13850
            arg5-13851
            #%modvars)
          (if arg0-13855
            (let ((local13856
                   (|_ffi-callback@(quote #%foreign)|
                    (arg3-13849 arg0-13855)
                    arg0-13846
                    arg1-13847
                    arg2-13848
                    arg5-13851)))
              (begin
                (if (eq? arg4-13850 '#t)
                  (begin
                    arg4-13850
                    (hash-set! _held-callbacks11450 arg0-13855 local13856))
                  (begin
                    arg0-13855
                    (if (box? arg4-13850)
                      (let ((local13869 (unbox arg4-13850)))
                        (set-box!
                         arg4-13850
                         (if (if (null? local13869) '#t (pair? local13869))
                           (cons local13856 local13869)
                           local13856)))
                      (if (procedure? arg4-13850)
                        (arg4-13850 local13856)
                        (void)))))
                local13856))
            '#f))
        (lambda (arg0-13879)
          '#(...cheme/foreign.ss:502:6
             "/Users/jay/Dev/svn/plt/collects/scheme/foreign.ss"
             502
             6
             22747
             57
             #t)
          '(captures: arg0-13846 arg1-13847 arg2-13848 arg3-13849 #%modvars)
          (if arg0-13879
            (arg3-13849
             (|_ffi-call@(quote #%foreign)|
              arg0-13879
              arg0-13846
              arg1-13847
              arg2-13848))
            '#f)))
       (|_make-ctype@(quote #%foreign)|
        |__fpointer@(quote #%foreign)|
        (lambda (arg0-13888)
          '#(...cheme/foreign.ss:492:6
             "/Users/jay/Dev/svn/plt/collects/scheme/foreign.ss"
             492
             6
             22297
             443
             #t)
          '(captures:
            arg0-13846
            arg1-13847
            arg2-13848
            arg4-13850
            arg5-13851
            #%modvars)
          (if arg0-13888
            (let ((local13889
                   (|_ffi-callback@(quote #%foreign)|
                    arg0-13888
                    arg0-13846
                    arg1-13847
                    arg2-13848
                    arg5-13851)))
              (begin
                (if (eq? arg4-13850 '#t)
                  (begin
                    arg4-13850
                    (hash-set! _held-callbacks11450 arg0-13888 local13889))
                  (begin
                    arg0-13888
                    (if (box? arg4-13850)
                      (let ((local13901 (unbox arg4-13850)))
                        (set-box!
                         arg4-13850
                         (if (if (null? local13901) '#t (pair? local13901))
                           (cons local13889 local13901)
                           local13889)))
                      (if (procedure? arg4-13850)
                        (arg4-13850 local13889)
                        (void)))))
                local13889))
            '#f))
        (lambda (arg0-13911)
          '#(...cheme/foreign.ss:502:6
             "/Users/jay/Dev/svn/plt/collects/scheme/foreign.ss"
             502
             6
             22747
             57
             #t)
          '(captures: arg0-13846 arg1-13847 arg2-13848 #%modvars)
          (if arg0-13911
            (|_ffi-call@(quote #%foreign)|
             arg0-13911
             arg0-13846
             arg1-13847
             arg2-13848)
            '#f))))))
  (call-with-values
   (lambda ()
     (if (= '4 (|_ctype-sizeof@(quote #%foreign)| |__float@(quote #%foreign)|))
       (void)
       (error
        'foreign
        '"internal error: float has a bad size (~s)"
        (|_ctype-sizeof@(quote #%foreign)| |__float@(quote #%foreign)|))))
   _print-values11358)
  (call-with-values
   (lambda ()
     (if (=
          '8
          (|_ctype-sizeof@(quote #%foreign)| |__double*@(quote #%foreign)|))
       (void)
       (error
        'foreign
        '"internal error: double has a bad size (~s)"
        (|_ctype-sizeof@(quote #%foreign)| |__double*@(quote #%foreign)|))))
   _print-values11358)
  (define-values
   (_libcrypto-so11451)
   (let ((local13930
          (let ((local13931 (system-type)))
            (if (eq? local13931 'windows)
              '(so "libeay32")
              '(so "libcrypto")))))
     (let ((get-dir13934
            (lambda ()
              'get-dir
              '(captures: #%modvars)
              (let ((local13935 (_extract-module-directory11384 stx11470)))
                (if local13935
                  local13935
                  (_main-collects-relative->path11383
                   '(collects #"openssl")))))))
       (_new-apply11362
        values
        (|_path-replace-suffix@(quote #%utils)|
         stx11471
         get-dir13934
         (list local13930))))))
  (define-values
   (_libssl-so11452)
   (let ((local13944
          (let ((local13945 (system-type)))
            (if (eq? local13945 'windows) '(so "ssleay32") '(so "libssl")))))
     (let ((get-dir13948
            (lambda ()
              'get-dir
              '(captures: #%modvars)
              (let ((local13949 (_extract-module-directory11384 stx11472)))
                (if local13949
                  local13949
                  (_main-collects-relative->path11383
                   '(collects #"openssl")))))))
       (_new-apply11362
        values
        (|_path-replace-suffix@(quote #%utils)|
         stx11473
         get-dir13948
         (list local13944))))))
  (define-values (_ssl-load-fail-reason11453) '#f)
  (define-values
   (_libcrypto11454)
   (let ((with-handlers-handler213958
          (lambda (arg0-13959)
            '#(with-handlers-handler2
               "/Users/jay/Dev/svn/plt/collects/openssl/mzssl.ss"
               68
               31
               1901
               125
               #f)
            '(captures: #%modvars)
            (begin
              (set! _ssl-load-fail-reason11453 (exn-message arg0-13959))))))
     (let ((local13961
            (continuation-mark-set-first
             '#f
             |_break-enabled-key@(quote #%paramz)|)))
       (_call-handled-body11322
        local13961
        (begin0
          (lambda (arg0-13967)
            '(captures: local13961 with-handlers-handler213958 #%modvars)
            (_select-handler/no-breaks11319
             arg0-13967
             local13961
             (list (cons exn:fail? with-handlers-handler213958))))
          local13961
          with-handlers-handler213958)
        (lambda ()
          '(captures: #%modvars)
          (_get-ffi-lib11448
           _libcrypto-so11451
           '("" "0.9.8b" "0.9.8" "0.9.7")))))))
  (define-values
   (_libssl11455)
   (if _libcrypto11454
     (let ((with-handlers-handler413976
            (lambda (arg0-13977)
              '#(with-handlers-handler4
                 "/Users/jay/Dev/svn/plt/collects/openssl/mzssl.ss"
                 76
                 26
                 2183
                 115
                 #f)
              '(captures: #%modvars)
              (begin
                (set! _ssl-load-fail-reason11453 (exn-message arg0-13977))))))
       (let ((local13979
              (continuation-mark-set-first
               '#f
               |_break-enabled-key@(quote #%paramz)|)))
         (_call-handled-body11322
          local13979
          (begin0
            (lambda (arg0-13985)
              '(captures: local13979 with-handlers-handler413976 #%modvars)
              (_select-handler/no-breaks11319
               arg0-13985
               local13979
               (list (cons exn:fail? with-handlers-handler413976))))
            local13979
            with-handlers-handler413976)
          (lambda ()
            '(captures: #%modvars)
            (_get-ffi-lib11448
             _libssl-so11452
             '("" "0.9.8b" "0.9.8" "0.9.7"))))))
     '#f))
  (define-values
   (_SSL_library_init11457)
   (if _libssl11455
     (__cprocedure*11449
      '"SSL_library_init"
      _libssl11455
      (|_ffi-callback@(quote #%foreign)|
       '()
       |__void@(quote #%foreign)|
       '#f
       '#f
       '#t
       '#f))
     (lambda rest14003
       '#(SSL_library_init
          "/Users/jay/Dev/svn/plt/collects/openssl/mzssl.ss"
          96
          22
          2766
          35
          #f)
       '(captures: #%modvars)
       (_raise-not-available11456))))
  (define-values
   (_SSL_load_error_strings11458)
   (if _libssl11455
     (__cprocedure*11449
      '"SSL_load_error_strings"
      _libssl11455
      (|_ffi-callback@(quote #%foreign)|
       '()
       |__void@(quote #%foreign)|
       '#f
       '#f
       '#t
       '#f))
     (lambda rest14013
       '#(SSL_load_error_strings
          "/Users/jay/Dev/svn/plt/collects/openssl/mzssl.ss"
          96
          22
          2766
          35
          #f)
       '(captures: #%modvars)
       (_raise-not-available11456))))
  (define-values
   (_raise-not-available11456)
   (lambda ()
     '#(raise-not-available
        "/Users/jay/Dev/svn/plt/collects/openssl/mzssl.ss"
        202
        2
        6875
        86
        #f)
     (error 'openssl '"OpenSSL shared library not found")))
  (define-values (_ssl-available?11459) (if _libssl11455 '#t '#f))
  (call-with-values
   (lambda ()
     (if _ssl-available?11459
       (begin (_SSL_library_init11457) (_SSL_load_error_strings11458))
       (void)))
   _print-values11358))
