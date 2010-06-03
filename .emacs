(eval-when-compile (require 'cl))
(setq load-path
      (append (list nil "~/")
              load-path))

(load "~/haskell-mode-2.7.0/haskell-site-file")

;(require 'w3m-e21)
;(provide 'w3m-e23)
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
        "Prevent annoying \"Active processes exist\" query when you quit Emacs."
        (flet ((process-list ())) ad-do-it))

(autoload 'magit-status "magit" nil t)
(global-set-key [(ctrl f12)]       'magit-status)
(delete-selection-mode 1)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'font-lock-mode)
;;(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
;;(require 'column-marker)
;;(autoload 'column-marker "/home/tomn/column-marker" "Cycle forward." t)
;;(add-hook 'find-file-hook (lambda () (interactive) (column-marker-1 80)))
(add-hook 'haskell-mode-hook (lambda () (interactive) (column-marker-1 81)))
(defun refresh-file ()
  (interactive)
  (revert-buffer t t t)
  )

(global-set-key [f5] 'refresh-file)

(autoload 'cycle-buffer "/home/tomn/cycle-buffer" "Cycle forward." t)
(autoload 'cycle-buffer-backward "/home/tomn/cycle-buffer.el" "Cycle backward." t)
(autoload 'cycle-buffer-permissive "/home/tomn/cycle-buffer.el" "Cycle forward allowing *buffers*." t)
(autoload 'cycle-buffer-backward-permissive "/home/tomn/cycle-buffer.el" "Cycle backward allowing *buffers*." t)
(autoload 'cycle-buffer-toggle-interesting "/home/tomn/cycle-buffer.el" "Toggle if this buffer will be considered." t)
(global-set-key [(f9)]        'cycle-buffer-backward)
(global-set-key [(f10)]       'cycle-buffer)
(global-set-key [(shift f9)]  'cycle-buffer-backward-permissive)
(global-set-key [(shift f10)] 'cycle-buffer-permissive)
(define-key global-map (kbd "<C-return>") 'dabbrev-expand)
(define-key global-map (kbd "<C-]>") 'dabbrev-expand)
(define-key global-map [?\C-o] 'save-buffer)
(define-key global-map [f8] 'eval-current-buffer)
(define-key global-map (kbd "ESC <up>") 'cycle-buffer)
(define-key global-map (kbd "ESC <down>") 'cycle-buffer-backward)
(define-key global-map (kbd "ESC <left>") 'select-next-window)
(define-key global-map (kbd "ESC <right>") 'select-previous-window)
(global-set-key (kbd "M-<right>") 'select-next-window)
(global-set-key (kbd "M-<left>")  'select-previous-window)
(global-set-key (kbd "M-<up>") 'cycle-buffer)
(global-set-key (kbd "M-<down>")  'cycle-buffer-backward)

;;(define-key global-map (kbd "M-+") 'bigger-window)

(define-key global-map (kbd "M-+") 'bigger-window)
(define-key global-map (kbd "M--") 'smaller-window)
(defun back-window ()
  (interactive)
  (other-window -1))
(defun bigger-window ()
  (interactive)
  (enlarge-window 1)
  (enlarge-window-horizontally 1))
(defun smaller-window ()
  (interactive)
  (shrink-window 1)
  (shrink-window-horizontally 1))
(defun select-next-window ()
  "Switch to the next window" 
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window" 
  (interactive)
  (select-window (previous-window)))

;;(require 'w3m-load)

(setq visible-bell t)


(require 'inf-haskell)

(require 'flymake)

(defun flymake-Haskell-init ()
          (flymake-simple-make-init-impl
            'flymake-create-temp-with-folder-structure nil nil
            (file-name-nondirectory buffer-file-name)
            'flymake-get-Haskell-cmdline))

(defun flymake-get-Haskell-cmdline (source base-dir)
       (list "flycheck_haskell.pl"
            (list source base-dir)))
    
(push '(".+\\.hs$" flymake-Haskell-init flymake-simple-java-cleanup)
          flymake-allowed-file-name-masks)
(push '(".+\\.lhs$" flymake-Haskell-init flymake-simple-java-cleanup)
          flymake-allowed-file-name-masks)
(push
      '("^\\(\.+\.hs\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
       1 2 3 4) flymake-err-line-patterns)

;;optional setting
    ;; if you want to use flymake always, then add the following hook.
(add-hook
  'haskell-mode-hook
  '(lambda ()
     (if (not (null buffer-file-name)) (flymake-mode))))

(when (fboundp 'resize-minibuffer-mode) ; for old emacs
      (resize-minibuffer-mode)
      (setq resize-minibuffer-window-exactly nil))

(defun credmp/flymake-display-err-minibuf () 
      "Displays the error/warning for the current line in the minibuffer"
      (interactive)
      (let* ((line-no             (flymake-current-line-no))
             (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
             (count               (length line-err-info-list))
             )
        (while (> count 0)
           (when line-err-info-list
           (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
                   (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
                   (text (flymake-ler-text (nth (1- count) line-err-info-list)))
                   (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
              (message "[%s] %s" line text)
              )
            )
          (setq count (1- count)))))

(add-hook
     'haskell-mode-hook
     '(lambda ()
        (define-key haskell-mode-map "\C-cd"
          'credmp/flymake-display-err-minibuf)))

(defun infhask-load-main ()
  (interactive)
  (inferior-haskell-load-and-run "main\n"))

(defun infhask-quit ()
  (interactive)
  (inferior-haskell-send-command (inferior-haskell-process) ":q\n"))

(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(global-set-key "\C-j" 'inferior-haskell-load-file)
(global-set-key [(f12)] 'compile)
(global-set-key [(f11)] 'infhask-load-main)
(global-set-key [(shift f11)] 'infhask-quit)
;;(global-set-key "\C-t" 'infhask-insert-type)


(setq inferior-haskell-find-project-root nil)

;;(defun infhask-insert-type ()
;;  (interactive)
;;  (inferior-haskell-type "near" 1))

(require 'ido)
(ido-mode t)

(setq completion-ignored-extensions
      (append completion-ignored-extensions
	      (list ".o" ".hi" ".lnk" ".gif" ".hs~" ".c~" ".prof")))

(define-key global-map [\C-kp-1] [?- ?>])
(define-key global-map [\C-kp-2] [?= ?>])
(define-key global-map [\C-kp-3] [?< ?-])
(define-key global-map [\C-kp-4] [?: ?:])


;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (and transient-mark-mode mark-active)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

(global-set-key (kbd "M-8") 'extend-selection)
(transient-mark-mode t)
(show-paren-mode t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

(server-start)
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
;; Yes, you can do this same trick with the cool "It's All Text" firefox add-on :-)
  (add-to-list 'auto-mode-alist '("/mutt-\\|itsalltext.*mail\\.google" . mail-mode))
  (add-hook 'mail-mode-hook 'turn-on-auto-fill)
  (add-hook
   'mail-mode-hook
   (lambda ()
     (define-key mail-mode-map [(control c) (control c)]
       (lambda ()
         (interactive)
         (save-buffer)
         (server-edit)))))

(setq inhibit-startup-message t) 
;;;(global-set-key "C-Z" nil)
(define-key global-map (kbd "<C-z>") nil)

(column-number-mode t)

(setq dabbrev-case-fold-search nil)

(global-set-key (kbd "<C-kp-subtract>") 'shrink-window)
(global-set-key (kbd "<C-kp-add>") 'enlarge-window)
(global-set-key (kbd "<C-S-kp-subtract>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-S-kp-add>") 'enlarge-window-horizontally)

(split-window-horizontally)
(split-window-vertically)
(other-window 2)
(split-window-vertically)
(other-window 1)
(ansi-term "/bin/bash")
(other-window 1)

;; GNUS

;; (add-to-list 'gnus-secondary-select-methods '(nnimap "gmail"
;;                                   (nnimap-address "imap.gmail.com")
;;                                   (nnimap-server-port 993)
;;                                   (nnimap-stream ssl)))

;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587 "tanielsen@gmail.com" nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587
;;       smtpmail-local-domain "le.ac.uk")
