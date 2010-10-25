#lang scribble/manual

@(require (for-label "../jsworld/jsworld.rkt")
          (for-label "../phone/location.rkt")
          (for-label "../phone/tilt.rkt")
          (for-label "../phone/sms.rkt")
          )




location.rkt
    on-location-change: world-updater  -> handler
    on-location-change!: world-updater effect-f -> handler 


sms.rkt

    on-sms-receive: world-updater -> handler
    on-sms-receive!: world-updater effect-f -> handler


tilt.rkt

    on-acceleration: (world number number number -> world)
    on-acceleration!: ...
    on-shake
    on-shake!
    on-tilt
    on-tilt!