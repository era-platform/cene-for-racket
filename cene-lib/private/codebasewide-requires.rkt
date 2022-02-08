#lang parendown/slash reprovide

; codebasewide-requires.rkt
;
; An import list that's useful primarily for this codebase.

;   Copyright 2022 The Era Authors
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


(for-syntax /combine-in/fallback
  (combine-in
    (only-in syntax/parse expr expr/c id nat syntax-parse)
    
    (only-in lathe-comforts w-))
  racket/base)

(only-in racket/contract/base
  -> ->* ->i =/c and/c any any/c contract? cons/c contract-name list/c listof none/c or/c recontract-out rename-contract)
(only-in racket/contract/combinator coerce-contract)
(only-in racket/control reset-at shift-at)
(only-in racket/generic define/generic define-generics)
(only-in racket/match match-define)
(only-in racket/math natural?)
(only-in racket/port filter-read-input-port peeking-input-port)
(only-in racket/runtime-path define-runtime-path)
(only-in racket/string string-contains?)
(only-in syntax/parse/define define-syntax-parse-rule)

(only-in lathe-comforts
  dissect dissectfn expect expectfn fn mat w- w-loop)
(only-in lathe-comforts/list
  list-all list-any list-foldl list-foldr list-kv-map list-map list-zip-map nat->maybe)
(only-in lathe-comforts/match
  define-match-expander-attenuated define-match-expander-from-match-and-make match/c)
(only-in lathe-comforts/maybe
  just just-value maybe? maybe-bind maybe/c maybe-map nothing nothing?)
(only-in lathe-comforts/own-contract
  ascribe-own-contract define/own-contract own-contract-out)
(only-in lathe-comforts/string immutable-string?)
(only-in lathe-comforts/struct
  auto-equal auto-write define-imitation-simple-struct define-syntax-and-value-imitation-simple-struct struct-easy tupler/c tupler-make-fn)
(only-in lathe-comforts/trivial trivial trivial?)

(only-in interconfection/extensibility/base
  authorized-name? authorized-name-get-name authorized-name-subname dex-authorized-name dex-dspace dspace? error-definer? error-definer-from-exn error-definer-from-message error-definer-uninformative extfx? extfx-claim-unique extfx-ct-continue extfx-freshen extfx-noop extfx-pub-write extfx-run-getfx extfx-put extfx-split-list extfx-split-table extfx-sub-write fuse-extfx getfx? getfx-bind getfx/c getfx-done getfx-err getfx-get make-pub make-sub optionally-dexed-dexed optionally-dexed-once pure-run-getfx success-or-error-definer)
(only-in interconfection/order
  assocs->table-if-mutually-unique cline-exact-rational dex-exact-rational dex-immutable-string dex-trivial fuse-exact-rational-by-plus fuse-exact-rational-by-times getfx-is-eq-by-dex ordering-eq)
(only-in interconfection/order/base
  cline-by-dex cline-default cline-fix cline-give-up cline-opaque cline-result? cline-tuple dex? dex-by-own-method dex-cline dex-default dex-dex dexed? dexed-first-order/c dexed-get-dex dexed-get-name dexed-get-value dex-fix dex-fuse dex-give-up dex-merge dex-name dex-opaque dex-table dex-tuple fusable-function? fuse-by-merge fuse-fix fuse-fusable-function fuse-opaque fuse-table fuse-tuple get-dex-from-cline getfx-call-fuse getfx-call-merge getfx-compare-by-cline getfx-compare-by-dex getfx-dexed-of getfx-is-in-cline getfx-is-in-dex getfx-name-of getfx-table-map-fuse getfx-table-sort make-fusable-function merge-by-dex merge-fix merge-opaque merge-table merge-tuple name? ordering-eq ordering-eq? ordering-gt ordering-lt ordering-private table? table-empty table-empty? table-get table-shadow table-v-of)
(prefix-in unsafe: /only-in interconfection/order/unsafe
  autoname-cline autoname-dex autoname-fuse autoname-merge cline cline-by-own-method-thorough cline-by-own-method::getfx-err-different-methods
  cline-by-own-method::getfx-get-method dex dexed fuse fuse-by-own-method-thorough fuse-by-own-method::getfx-err-cannot-get-output-method fuse-by-own-method::getfx-err-different-output-method fuse-by-own-method::getfx-get-method fuse-fusable-function-thorough fuse-fusable-function::getfx-err-cannot-combine-results fuse-fusable-function::getfx-arg-to-method gen:cline-internals gen:dex-internals gen:furge-internals merge merge-by-own-method-thorough merge-by-own-method::getfx-err-cannot-get-output-method merge-by-own-method::getfx-err-different-output-method merge-by-own-method::getfx-get-method name table->sorted-list)
