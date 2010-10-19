#lang s-exp "../lang/base.rkt"

(require "../permissions/require-permission.rkt")
(require "../ffi/ffi.rkt")
(require "../jsworld/jsworld.rkt")

(provide on-location-change!
	 on-location-change)


(require-permission "PERMISSION:LOCATION")


(define (on-location-change! world-updater effect-updater)
  (let ([geolocation (js-get-field (js-get-global-value "navigator")
				   "phonegap_geo")])
    (make-world-config (lambda (success error)
                         (js-call (js-get-field geolocation "watchPosition")
                                  geolocation
                                  success
                                  error))
                       (lambda (id) (js-call (js-get-field geolocation "clearWatch")
                                             geolocation
                                             id))
                       (lambda (w lat lng)
                         (world-with-effects (effect-updater w (prim-js->scheme lat) (prim-js->scheme lng))
                                             (world-updater w (prim-js->scheme lat) (prim-js->scheme lng))))
                       (lambda (w e)
                         (error 'on-location-change! "an error occurred with accessing GPS locations")))))




(define (on-location-change world-updater)
  (let ([geolocation (js-get-field (js-get-global-value "navigator")
				   "phonegap_geo")])
    (make-world-config (lambda (success error)
                         (js-call (js-get-field geolocation "watchPosition")
                                  geolocation
                                  success
                                  error))
                       (lambda (id) (js-call (js-get-field geolocation "clearWatch")
                                             geolocation
                                             id))
                       (lambda (w lat lng)
                         (world-updater w (prim-js->scheme lat) (prim-js->scheme lng)))
                       (lambda (w e)
                         (error 'on-location-change "an error occurred with accessing GPS locations")))))
