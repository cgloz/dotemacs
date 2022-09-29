

;; go straight to scratch buffer
(setq inhibit-startup-message t)

(scroll-bar-mode -1)            ;; disable visible scrollbar
(tool-bar-mode -1)              ;; disable the toolbar
(tooltip-mode -1)               ;; Disable tooltips
(menu-bar-mode -1)              ;; Disable Menubar
(set-fringe-mode nil)             ;; Minimal Space for Indicators

;; maximized on start-up
(add-hook 'emacs-startup-hook 'toggle-frame-fullscreen)
;;(toggle-frame-fullscreen)

;; tranparency when unfocused (75)
(add-to-list 'default-frame-alist '(alpha . (100 . 75)))

(setq word-wap t)


;; Set up visible bell
(setq visible-bell t)    ;; flash instead of ping

;; mouse settings
(setq mouse-wheel-scroll-amount '(0.02))
(setq mouse-wheel-progressive-speed nil)

;; cursor settings
(setq blink-cursor-delay 10)

(set-face-attribute 'default nil :height 160)

;; frame title (title on the OS Window)
; (setq frame-title-format (list "%b - " (getenv "USERNAME") "@" (getenv "USERDOMAIN")))

(defun start-up-tweaks()
  (set-face-attribute 'default nil :height 150)
  ;; Set-up Relative Line Numbers
  (setq display-line-numbers-type 'absolute)
  (set-face-attribute 'line-number nil :height 0.70 :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :height 0.70 :inherit 'fixed-pitch)
  (setq display-line-numbers-width 1))
;; function only seems to load right
;; if it loads after everything else
(add-hook 'emacs-startup-hook 'start-up-tweaks)

;; this code allows it so that only the active window has line numbers displayed
(defun line-numbers-selected-window ()
  "Highlight selected window with a different background color."
  (walk-windows (lambda (w)
                  (unless (eq w (selected-window))
                    (with-current-buffer (window-buffer w)
                     (setq display-line-numbers nil)))))
  (setq display-line-numbers 'relative))
(add-hook 'buffer-list-update-hook 'line-numbers-selected-window)

;; set up spacing and wrapping for text-mode, not prog
(defun prog-mode-settings()
  (setq truncate-lines t)
  (setq line-spacing nil))
(defun text-mode-settings()
;  (variable-pitch-mode 1) ; non fixed-space
  (setq truncate-lines nil)
  (setq line-spacing 0.25))

(add-hook 'prog-mode-hook 'prog-mode-settings)
(add-hook 'text-mode-hook 'text-mode-settings)

;; Sane Writing Helpers
;; ---------------------

;; automatic pairing of characters like "(" and quote
(electric-pair-mode)
(electric-quote-mode)
;;---------------------

;; Navigation
(winner-mode)
;; manages buffer layout history

;; Desktop Save
(setq desktop-path `("~" "~/DOCS/Desktop"))

;; Sane Keybindings
;;-------------------

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; User Function for finding init-file quicker

(global-set-key (kbd "<M-f4>") 'save-buffers-kill-emacs)

(global-set-key (kbd "C-c g") 'exit-minibuffer)

;; User Function for System Clipboard Copy
(global-set-key (kbd "C-c w") #'copy-to-clipboard)
;; User Function for System Clipboard Paste
(global-set-key (kbd "C-c y") #'paste-from-clipboard)

;; Quick-Find User Init
;; Define Function
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-tab user-init-file))
;; accompanying keybinding
(global-set-key (kbd "C-c I") #'find-user-init-file)

;; New Frame With Scratch
(defun new-frame-with-scratch ()
  "Open a new frame with scratch buffer selected"
  (interactive)
  (let ((frame (make-frame))
        (scratch-name "*temp*"))
    (select-frame-set-input-focus frame)
    (unless (get-buffer scratch-name)
      (with-current-buffer (get-buffer-create scratch-name)
        (org-mode)))
    (switch-to-buffer scratch-name 'norecord)))
(global-set-key (kbd "C-c S") #'new-frame-with-scratch)

;;; Start-Up
;; ==========

;; Performance Measurements
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun my/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'my/display-startup-time)


;;; Set-Up Straight + Use-Package
;; ================================

;; Instead of Package.el let's use Straight.el
;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
    (url-retrieve-synchronously
     "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
     'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom (straight-use-package-by-default t))


;;; very early update of org so that other packages work well
;; ----------------------------------------------------------

(use-package org
  :demand t)


;;; Modular Directories
;; =====================

;; set up 2 modular directories
(defvar dotemacs-dir  (file-name-directory user-init-file))
(defvar core-dir  (expand-file-name "core" dotemacs-dir)
  "User created scripts that are all automatically required")
(add-to-list 'load-path core-dir)
;; Require packages in core/
(mapc 'load (directory-files core-dir nil "^[^#].*el$"))
;; This regex magic seeks out .el files

(defvar modules-dir  (expand-file-name "modules" dotemacs-dir)
  "User created scripts that you can optionally loaded one by one")
(add-to-list 'load-path modules-dir)

;;; Require modules
;; Look at Feel
;; ------------
;; only uncomment one
;   (require 'main_look)
;   (require 'elegant_look)
   (require 'new_look)

;; Misc
;; ----
(require 'personal)

;;; Per Environment Settings
;; ==========================

(defvar android (not(null(getenv "ANDROID_ROOT"))))
(defvar windows (string-equal system-type "windows-nt"))

;; for magit to work on Windows the emacs environmental
;;   variable "HOME" needs to be set to "HOMEPATH" on
;;   Windows. This should be "C:\\Users\UserName\"
;;   One of the reasons it might not be that is that
;;   I use a custom site-start.el file to get my
;;   dotemacs directory where I want it instead
;;   of Appdata.
(add-hook 'magit-mode-hook (lambda () (if windows (setenv "HOME" (getenv "HOMEPATH")))))

;; do not display battery on android
(if android t (display-battery-mode t))

;; modules
;; -------
(if android nil (require 'android-settings))

;;; Test Area
;; ===========

(use-package foldout
  :after outline)
(use-package outshine
  :after outline)

(use-package zoom
  :config
  (custom-set-variables '(zoom-mode t))
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618)))
  )

(use-package avy
  :config
  (setq avy-all-windows t
      avy-background t
      avy-style 'de-bruijn
      ;; the unpredictability of this (when enabled) makes it a poor default
      avy-single-candidate-jump nil)
  :bind ("C-s" . avy-goto-char-timer))

(use-package link-hint
  :ensure t
  :bind
  ("C-M-s" . link-hint-open-link)
  ("M-s c" . link-hint-copy-link)
  :config
  (setq browse-url-browser-function 'browse-url-firefox)
  ;; fallback to embark
  (setq link-hint-action-fallback-commands
      (list :open (lambda ()
                    (condition-case _
                        (progn
                          (embark-dwim)
                          t)
                      (error
                       nil))))))

;;; ----FIN----
