#lang parendown racket/base

; cene
;
; A Racket library with entrypoints to the Cene programming language.

;   Copyright 2018 The Era Authors
;
;   Licensed under the Apache License, Version 2.0 (the "License");
;   you may not use this file except in compliance with the License.
;   You may obtain a copy of the License at
;
;       http://www.apache.org/licenses/LICENSE-2.0
;
;   Unless required by applicable law or agreed to in writing,
;   software distributed under the License is distributed on an
;   "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;   either express or implied. See the License for the specific
;   language governing permissions and limitations under the License.


(require cene/private)
(require #/only-in cene/private/essentials
  sink-effects-init-essentials)


; TODO: Document these exports.
(provide extfx-with-gets-from)
(provide make-sink-effects)
(provide sink-authorized-name)
(provide sink-authorized-name-subname)
(provide sink-effects-claim-and-split)
(provide sink-effects-fuse)
(provide sink-effects-init-essentials)
(provide sink-effects-run!)
(provide sink-effects-run-string)

; TODO: See if we really want to provide these exports at all. If we
; do, document them.
(provide exn:fail:cene)
