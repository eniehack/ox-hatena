;;; ox-hatenamd.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Nakaya
;;
;; Author: Nakaya <eniehack@outlook.jp>
;; Maintainer: Nakaya <eniehack@outlook.jp>
;; Created: 3月 13, 2023
;; Modified: 3月 13, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/eniehack/ox-hatenamd
;; Package-Requires: ((emacs "26.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'org-macs)
(org-assert-version)

(require 'ox-md)
(require 'ox-publish)

(defgroup org-export-hatenamd nil
  "Hatena Blog Markdown Backend for Org Export Engine."
  :group 'org-export
  :tag "Org Hatena blog"
  :version "26.1")

(org-export-define-derived-backend 'hatenamd 'md
  :menu-entry
  '(?8 "Export to Hatena-Flavored Markdown"
    ((?b "To temp. buffer"
         (lambda (a s v b) (org-hatenamd--export-as-markdown a s v)))))
  :translate-alist '((inner-template . org-hatenamd--inner-template)
                     (src-block . org-hatenamd--src-block)
                     (latex-fragment . org-hatenamd--latex-fragment)
                     (headline . org-hatenamd--headline)))

(defun org-hatenamd--inner-template (contents info)
  (concat
   "[:contents]\n"
   contents
   "\n"
   (org-md--footnote-section info)))

(defun org-hatenamd--src-block (src-block _contents info)
  (let* ((lang (org-element-property :language src-block))
         (code (org-export-format-code-default src-block info))
         (prefix (concat "```" lang "\n"))
         (suffix "```\n"))
    (concat prefix code suffix)))

(defun org-hatenamd--latex-fragment (latex-fragment _contents info)
  (when (plist-get info :with-latex)
    (let ((frag (org-element-property :value latex-fragment)))
      (cond
       ((string-match-p "^\(" frag)
        (concat "[tex:" (substring frag 2 -2) "]"))
       ((string-match-p "^\\[" frag)
        (concat "<div style=\"text-align:center;\">" "[tex: \displaystyle " (substring frag 2 -2) "]" "</div>"))
       (t frag)))))

(defun org-hatenamd--export-as-markdown (&optional async subtreep visible-only)
  (interactive)
  (org-export-to-buffer 'hatenamd "*Org HatenaMD Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

(defun org-hatenamd--headline (headline contents info)
  (let* ((level (+ 2 (org-export-get-relative-level headline info)))
         (title (org-export-data (org-element-property :title headline) info)))
    (cond
     ((or (org-export-low-level-p headline info)
          (< 5 level))
      (let ((bullet
             (if (not (org-export-numbered-headline-p headline info))
                 (concat (number-to-string
                          (car (last (org-export-get-headline-number
                                      headline info))))
                         "."))))
        (concat bullet (make-string (- 4 (length bullet)) ?\s) title "\n\n"
                (and contents (replace-regexp-in-string "^" "  " contents)))))
     (t (concat (org-md--headline-title 'atx level title))))))

(provide 'ox-hatenamd)
;;; ox-hatenamd.el ends here
