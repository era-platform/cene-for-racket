#lang parendown/slash racket/base

; cene
;
; A Racket library with entrypoints to the Cene programming language.

;   Copyright 2018-2020, 2022 The Era Authors
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


(require cene/private/shim)
(init-shim)

(require /only-in cene/private
  cenegetfx-done
  extfx-run-sink-extfx
  make-cene-root-info
  make-fault-internal
  make-sink-extfx
  sink-authorized-name
  sink-authorized-name-subname
  sink-extfx-claim-and-split
  sink-extfx-fuse
  sink-extfx-run-directive-cexprs-in-string
  sink-name-of-racket-string
  sink-qualify)
(require /only-in cene/private/essentials
  minimal-and-essential-tags
  sink-extfx-init-essentials
  sink-extfx-init-package)


; TODO: Document these exports.
(provide /recontract-out
  cenegetfx-done
  extfx-run-sink-extfx
  make-cene-root-info)
(provide
  make-fault-internal)
(provide /recontract-out
  make-sink-extfx
  minimal-and-essential-tags)
(provide
  sink-authorized-name)
(provide /recontract-out
  sink-authorized-name-subname
  sink-extfx-claim-and-split
  sink-extfx-fuse
  sink-extfx-init-essentials
  sink-extfx-init-package
  sink-extfx-run-directive-cexprs-in-string
  sink-name-of-racket-string)
(provide
  sink-qualify)
