#lang racket

;; As a large system with generic operations evolves, new types of
;; data objects or new operations may be needed. For each of the three
;; strategies -- generic operations with explicit dispatch,
;; data-directed style, and message-passing-style -- describe the
;; changes that must be made to a system in order to add new types or
;; new operations. Which organization would be most appropriate for a
;; system in which new types must often be added? Which would be most
;; appropriate for a system in which new operations must often be
;; added?

;; Generic operations with explicit dispatch:
;; To add new type, every generic interface procedure should add new
;; clause for this specific type. Besides, there should be no name
;; conflict between the new type and the existing types. To add new
;; operation, additional clause should be added to the dispatch
;; function of every type.

;; Data-directed style:
;; To add either a new type or a new operation, it needs to update the
;; table of operations.

;; Message-passing style:
;; To add a new type, it just needs to implement this type.
;; To add a new operation to the interface, it needs to add a clause
;; to every existing type, which is not favorable compared with
;; data-directed style.
