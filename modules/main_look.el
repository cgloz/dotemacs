;;; lang/org/autoload/org.el -*- lexical-binding: t; -*-

;; Modules need to provide the variable
;;   that will be (required) in the init
(provide 'main_look)

;;; Look and Feel
;; ===============

(setq window-divider-mode nil)

;;; Elegant Modeline

(defun mode-line-render (left right)
  "Function to render the modeline LEFT to RIGHT."
  (concat (propertize " " 'display `(space :align-to (- left))) left
          (propertize " " 'display `(space :align-to (- right ,(length right))))
          right))
(setq-default header-line-format
     '((:eval
       (mode-line-render
       (format-mode-line (list
         " %b "
         (if (and buffer-file-name (buffer-modified-p))
             (propertize "(modified)" 'face 'italic))))
       (format-mode-line
        (propertize "%4l:%2c " 'face 'bold))))))
;; I added a space after "%2c" because in the terminal
;;   the line gets cut off on the right vertical border
;;   between windows

;;; visual packages
(use-package nyan-mode)
(use-package badwolf-theme)
;; modus-themes
;; a dark and light accessibility theme
;; vivendi:dark::operandi:light
(use-package modus-themes
;  :disabled
	:init  
	(load-theme 'modus-vivendi t)
;        (load-theme 'modus-operandi t)
)

;;; Olivetti
;; Olivetti cleans up fringes and centers text
;; like a "writeroom" or "zen" modee
(use-package olivetti
  :ensure t
  :config
  (setq olivetti-recall-visual-line-mode-entry-state t)
  (setq olivetti-style t)
  (setq olivetti-minimum-body-width 100)
  (setq olivetti-margin-width 1)
  (set-face-attribute 'olivetti-fringe nil  :background (face-background 'line-number))
  :hook (text-mode . olivetti-mode))


;;;### autoload
(defun clean-borders()
  "make a clean border the same color as the line number background"
  (setq left-margin-width 8)
  (setq right-margin-width 8)
  (set-face-background 'internal-border (face-background 'line-number) (selected-frame))
  (set-frame-parameter (selected-frame) 'internal-border-width 10)
  (set-face-attribute 'fringe nil :background (face-background 'line-number)) 
  (set-face-attribute 'header-line nil :background (face-background 'line-number))  
;;; defined custom variable line-bg with let and set it to line-number bg
;;    then I can use that to set box color properly (it creates issues when
;;    using set-face-attribute... (:box '(:color)" for whatever reason
(let ((line-bg (face-attribute 'line-number :background)))
(custom-set-faces
 `(header-line ((t :box (:line-width
			 5 :color ,line-bg) :background ,line-bg)))
 `(vertical-border ((t :background ,line-bg :foreground ,line-bg)))))
)
;; function only seems to load right
;; if it loads after everything else
(add-hook 'emacs-startup-hook 'clean-borders)


;;; Mini-Modeline
;; puts modeline in mini-buffer creating a sort
;;   of global modeline
(use-package mini-modeline
  :after modus-themes
  :init
  (display-battery-mode t)
  (setq battery-mode-line-format "[%p]")
  (set-face-attribute 'mode-line nil :box "unspecified" :extend t  :background (face-background 'line-number))
  :config
  (mini-modeline-mode t)
  (set-face-attribute 'mini-modeline-mode-line nil :height 0.4 :foreground nil :background (face-background 'line-number))
  (set-face-attribute 'mini-modeline-mode-line-inactive nil :height 0.4 :foreground nil  :background (face-background 'line-number))
  (setq mini-modeline-r-format (list
		         '(:eval (propertize (format-time-string " %a") 'face 'bold)) " "
		         '(:eval (propertize (format-time-string "%d %b %Y"))) " "
		         '(:eval (propertize (format-time-string "%H") 'face 'bold))
		         '(:eval (propertize (format-time-string ":%M"))) " "
		         '(:eval (propertize battery-mode-line-string))
			 " "))
)

;; Font Settings
;; -------------

(set-frame-font
 (cond
  ((string-equal system-type "windows-nt")
   (if (member "Consolas" (font-family-list)) "Consolas" nil ))
  ((string-equal system-type "darwin")
   (if (member "Menlo" (font-family-list)) "Menlo-16" nil ))
  ((string-equal system-type "gnu/linux")
   (if (member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono" nil ))
  (t nil))
 t t)

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

(defun cgloz/org-mode-setup ()
  (org-num-mode) ; auto-numbered headings
  (org-indent-mode) ; indent based on heading level
  (setq org-startup-indented t
        org-hide-leading-stars t)
 ;  (variable-pitch-mode 1) ; non fixed-space
  (visual-line-mode 1)
; (set-window-buffer nil (current-buffer))
)

(defun cgloz/org-font-setup ()
   ;; Set faces for heading levels
  (dolist (face '((org-document-title . 1.35)
		  (org-level-1 . 1.5)
                  (org-level-2 . 1.4)
                  (org-level-3 . 1.35)
                  (org-level-4 . 1.3)
                  (org-level-5 . 1.25)

                  (org-level-6 . 1.25)
                  (org-level-7 . 1.25)
                  (org-level-8 . 1.1)
		  (org-table . 0.75)))
    (set-face-attribute (car face) nil :height (cdr face) :width 'expanded   :weight 'thin))
    
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(use-package org
  :commands (org-capture org-agenda)
  :hook (org-mode . cgloz/org-mode-setup)
  :config
  (setq org-ellipsis "…")
  (custom-set-faces '(org-ellipsis ((t (:foreground "gray40" :underline nil)))))
  (setq org-num-face '(:underline nil :width ultra-condensed :inherit 'variable-pitch))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-done-headline t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  (cgloz/org-font-setup))


