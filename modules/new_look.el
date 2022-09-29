;;; lang/org/autoload/org.el -*- lexical-binding: t; -*-

;; Modules need to provide the variable
;;   that will be (required) in the init
(provide 'new_look)

;;; Look and Feel
;; ===============

(setq window-divider-mode nil)

(defun mode-line-render (left-text mid-text right-text)
  "Function to render the modeline LEFT, CENTER and RIGHT."
  ;; mode-line-render requires 3 "format-mode-line" calls, each will
  ;;  be properly placed in the far left, middle, and right
  (concat (propertize " " 'display `(space
				     :align-to
				     (- left)))
	  left-text
	  (propertize " " 'display `(space
				     :align-to
				     (- center ,(/ (length mid-text) 2))))
	  mid-text
	  (propertize " " 'display `(space
				     :align-to (- right ,(length right-text))))
	  right-text))

(setq-default mode-line-format
	      '((:eval
       (mode-line-render
       (format-mode-line
       (if (and buffer-file-name (buffer-modified-p))
             (propertize "%*%+ " 'face '(:background "red" :inherit bold))
         (propertize "%*%+ " 'face 'bold)))
       (format-mode-line (propertize "  %b  " 'face '(:inherit variable-pitch)))
       (format-mode-line
        (propertize (concat "%l,%2c   " (substring (format-mode-line "%p") 0 3) "%2%%%") 'face 'bold)
)))))
;; working on getting the % section to shorten "Bottom to Bot" mode-line-position

(window-divider-mode)
(setq window-divider-default-right-width 5)

;;;### autoload
(defun clean-borders()
  "make a clean border the same color as the line number background"
  (set-frame-parameter (selected-frame) 'internal-border-width 3)
  (set-face-attribute 'mode-line nil :height .75 :inverse-video t :box nil :background (face-background 'default)) 
  (set-face-attribute 'mode-line-inactive nil :height .75 :inverse-video t :box nil :background (face-background 'default))
  (set-face-attribute 'line-number nil :background (face-background 'default))
  (set-face-attribute 'line-number-current-line nil :inverse-video t)
  (set-face-attribute 'fringe nil :background nil)
  ;; setup for window dividers
  (set-face-foreground 'window-divider (face-background 'default))
  (set-face-foreground 'window-divider-first-pixel (face-foreground 'default))
  (set-face-foreground 'window-divider-last-pixel (face-foreground 'default))
  ;;vertical border for terminal emacs
  ;;Reverse colors for the border to have nicer line  
  (set-face-inverse-video-p 'vertical-border nil)
  (set-face-background 'vertical-border (face-background 'default))) 
(add-hook 'emacs-startup-hook 'clean-borders)

;;; Color-Themes

;; modus-themes
;; a dark and light accessibility theme
;; vivendi:dark::operandi:light
(use-package modus-themes
;  :disabled
  :init  
	(load-theme 'modus-vivendi t)
;       (load-theme 'modus-operandi t)
)

;;; visual packages

;;; Olivetti
;; Olivetti cleans up fringes and centers text
;; like a "writeroom" or "zen" modee
(use-package olivetti
  :ensure t
  :init
;  (set-face-background 'olivetti-fringe (face-background 'line-number))
  :config
  (setq olivetti-recall-visual-line-mode-entry-state t)
  (setq olivetti-style nil)
  (setq olivetti-minimum-body-width 100)
  (setq olivetti-margin-width 1)
  :hook (text-mode . olivetti-mode))


;; Font Settings
;; -------------

(defface variable-pitch-serif
  '((t :family "serif" :foundry "outline"))
  "The basic variable pitch face with serifs."
  :group 'basic-faces
  )

;; set font for emoji
(set-fontset-font
 t
 '(#x1f300 . #x1fad0)
 (cond
  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
  ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
  ((member "Symbola" (font-family-list)) "Symbola")
  ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji"))
 ;; Apple Color Emoji should be before Symbola, but Richard Stallman skum disabled it.
 ;; GNU Emacs Removes Color Emoji Support on the Mac
 ;; http://ergoemacs.org/misc/emacs_macos_emoji.html
 )

;; Change Default Font Height
(set-face-attribute 'default nil :height 125)

;; ORG Mode Settings
;; =================

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '(" ")))

(use-package mixed-pitch
  :hook (text-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-fixed-pitch-faces (-union mixed-pitch-fixed-pitch-faces (list
            'org-date
            'org-footnote
            'org-special-keyword
	    ; 'org-drawer ;; commented because it won’t let me bold otherwise
            'org-property-value
            'org-ref-cite-face
            'org-tag
            'org-todo-keyword-todo
            'org-todo-keyword-habt
            'org-todo-keyword-done
            'org-todo-keyword-wait
            'org-todo-keyword-kill
            'org-todo-keyword-outd
            'org-todo
            'org-done
            'font-lock-comment-face))))


 (defun cgloz/org-font-setup ()
    ;; Set faces for heading levels
   (dolist (face '((org-document-title . 1.75)
 		   (org-level-1 . 1.5)
                   (org-level-2 . 1.4)
                   (org-level-3 . 1.35)
                   (org-level-4 . 1.3)
                   (org-level-5 . 1.25)

                   (org-level-6 . 1.25)
                   (org-level-7 . 1.25)
                   (org-level-8 . 1.1)
 		   (org-table . 0.75)))
     (set-face-attribute (car face) nil :height (cdr face) :width 'normal :inherit 'variable-pitch-serif))
  
   ;; Ensure that anything that should be fixed-pitch in Org files appears that way

    (set-face-attribute 'org-table     nil :inherit 'shadow)
    (set-face-attribute 'org-verbatim  nil :inherit 'shadow)
    (set-face-attribute 'org-meta-line nil :inherit 'font-lock-comment-face)
    (set-face-attribute 'org-block     nil :inherit 'menu)

    (set-face-attribute 'org-block-begin-line nil :inherit 'shadow)
    (set-face-attribute 'org-block-end-line   nil :inherit 'org-block-begin-line)

    (set-face-attribute 'org-property-value  nil :inherit 'menu)
    (set-face-attribute 'org-special-keyword nil :inherit 'shadow)
    (set-face-attribute 'org-drawer nil :weight 'bold :inherit 'font-lock-comment-face)
 )



(use-package org
  :commands (org-capture org-agenda)
  :config
  (setq org-ellipsis "...")
  (setq org-ellipsis '(:inherit (variable-pitch shadow) :underline nil))
  (setq org-num-face '(:inherit variable-pitch-serif :weight bold))
  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-done-headline t)
  (setq org-fontify-quote-and-verse-blocks t)
  (cgloz/org-font-setup)
)

;  TEST AREA
; ===========

;;; The package I used to get info displaying in the echo area
;; in the actual file I made a change,
;; I removed (setq-default mode-line-format nil)
;; so that I could keep my mode-line
(straight-use-package
 '(xfel-mode-line :type git :host github :repo "fernando-jascovich/xfel-mode-line")
 )
(require 'xfel-mode-line)
(xfel-mode-line-mode 1)
 (defun test-format (left-text right-text)
  (concat (propertize " " 'display `(space
				     :align-to
				     (- left)))
	  left-text
	  (propertize " " 'display `(space
				     :align-to (- right ,(length right-text))))
	  right-text)
  )


(setq battery-mode-line-format "[%p]")
(defun xfel-render ()
  	      (format-mode-line '((:eval
       (mode-line-render
       (format-mode-line
       mode-line-modes)
       " "
       (concat (format-time-string
       "%a %d %b %Y %H:%M " ) battery-mode-line-string " "))))))


(setq xfel-mode-line-format-function #'xfel-render)
