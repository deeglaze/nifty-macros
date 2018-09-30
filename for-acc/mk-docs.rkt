#lang racket/base
;;
;; Copyright 2018 Dionna Glaze
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
(require racket/system)
(define file "for-acc")
;; Added this file because I can never fully remember the proper incantation.
(system (format "scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ ~a.scrbl" file))
(system (format "scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --markdown ~a.scrbl; mv ~a.md README.md" file file))