;; TODO IF In Windows, setenv HOME to HOMEPATH

;;; Personal Sanity Settings
;; ==========================


;; go straight to scratch buffer
(setq inhibit-startup-message t)

(scroll-bar-mode -1)            ;; disable visible scrollbar
(tool-bar-mode -1)              ;; disable the toolbar
(tooltip-mode -1)               ;; Disable tooltips
(menu-bar-mode -1)              ;; Disable Menubar
(set-fringe-mode '(5 . 25))     ;; Create a light space on left and a lot of space on right

;; maximized on start-up
(toggle-frame-maximized)

;; minor transparency while focused (99) and notable
;;    tranparency when unfocused (85)
(add-to-list 'default-frame-alist '(alpha . (99 . 85)))

(setq word-wap t)


;; Set up visible bell
(setq visible-bell t)    ;; flash instead of ping

;; mouse settings
(setq mouse-wheel-scroll-amount '(0.02))
(setq mouse-wheel-progressive-speed nil)

;; cursor settings
(setq blink-cursor-delay 10)

;; Set-up Relative Line Numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(set-face-attribute 'line-number nil :height 0.70)

;; set up spacing and wrapping for text-mode, not prog
(defun prog-mode-settings()
  (setq truncate-lines t)
  (setq line-spacing nil))
(defun text-mode-settings()
  (setq truncate-lines nil)
  (setq line-spacing 0.25))

(add-hook 'prog-mode-hook 'prog-mode-settings)
(add-hook 'text-mode-hook 'text-mode-settings)

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
  (find-file-other-window user-init-file))
;; accompanying keybinding
(global-set-key (kbd "C-c I") #'find-user-init-file)

;;; Start-Up
;; ==========

;; Performance Measurements
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun cgloz/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'cgloz/display-startup-time)


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
   (require 'main_look)
;   (require 'elegant_look)

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

;; modules
;; -------
(if android nil (require 'android-settings))

;;; Test Area
;; =========

(use-package foldout
  :after outline)
(use-package outshine
  :after outline)

;;; ----FIN----


