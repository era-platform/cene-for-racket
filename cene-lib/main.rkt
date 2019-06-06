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
  sink-extfx-init-essentials sink-extfx-init-package)


; TODO: Document these exports.
(provide extfx-with-gets-from)
(provide make-fault-internal)
(provide make-sink-extfx)
(provide sink-authorized-name)
(provide sink-authorized-name-subname)
(provide sink-extfx-claim-and-split)
(provide sink-extfx-fuse)
(provide sink-extfx-init-essentials)
(provide sink-extfx-init-package)
(provide sink-extfx-run!)
(provide sink-extfx-run-string)
