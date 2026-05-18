;;; l-typeclasses.el --- Typeclass system for l-el -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; since: NEXT
;; updated-at: ()

;; This file is part of l-el.

;;; Commentary:

;; Main entry point for the typeclass system in l-el.
;; This file loads all available typeclasses.

;;; Code:

;;
;; Instances
;;

(l-require 'algebra)

(lmonoid :string
         :id ""
         :op #'concat)

(lmonoid :list
         :id nil
         :op #'append)

(lmonoid :vector
         :id []
         :op #'vconcat)



(provide 'l-typeclasses)
;;; l-typeclasses.el ends here
