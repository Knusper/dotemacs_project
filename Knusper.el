(server-start)

(setq custom-file "~/.gnu-emacs-custom")
(load custom-file)

(setq user-full-name "Edmund Christian Herenz")

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq LaTeX-math-menu-unicode t)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-cite-format 'natbib)

;; use mupdf as default PDF viewer
(with-eval-after-load "tex"
  (add-to-list 'TeX-view-program-list '("mupdf" "/usr/bin/mupdf %o"))
  (setcdr (assq 'output-pdf TeX-view-program-selection) '("mupdf")))

(setq auto-mode-alist
      (cons '("\\.org$" . org-mode) auto-mode-alist))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

(add-hook 'org-mode-hook
'(lambda ()
       (setq org-file-apps
             (append '(
                       ("\\.png\\'" . default)
                       ) org-file-apps ))))

(defun ensc/mailcap-mime-data-filter (filter)
  ""
  (mapcar (lambda(major)
        (append (list (car major))
            (remove nil
                (mapcar (lambda(minor)
      		(when (funcall filter (car major) (car minor) (cdr minor))
                    minor))
                    (cdr major)))))
mailcap-mime-data))

(defun ensc/no-pdf-doc-view-filter (major minor spec)
  (if (and (string= major "application")
 (string= minor "pdf")
 (member '(viewer . doc-view-mode) spec))
nil
    t))

(eval-after-load 'mailcap
  '(progn
     (setq mailcap-mime-data
 (ensc/mailcap-mime-data-filter 'ensc/no-pdf-doc-view-filter))))


(require 'org-drill)
(setq org-drill-add-random-noise-to-intervals-p t)
(setq org-drill-leech-method 'warn)
(setq org-drill-learn-fraction 0.3)

(define-skeleton swedish-phrase-skeleton
  "Insert swedish phrases in org-drill mode"
  ""
  "** sonst.                                                          :drill:\n"
  "   :PROPERTIES:\n"
  "   :DRILL_CARD_TYPE: hide1cloze\n"
  "   :END:\n"
  "   sv: [" (skeleton-read "svenska: ") "]\n"
  "   de: [" (skeleton-read "deutsch: ") "]\n")

(define-skeleton swedish-verb-skeleton
  "Insert swedish verbs in org-drill mode"
  ""
  "** verb                                                            :drill:\n"
  "   :PROPERTIES:\n"
  "   :DRILL_CARD_TYPE: hide1cloze\n"
  "   :END:\n"
  "   sv: [" (skeleton-read "svenska: ") "]\n"
  "   de: [" (skeleton-read "deutsch: ") "]\n"
  "*** konj.\n"
  "    | infinitiv | presens | preteritum | supinum | imperativ |\n"
  "    |-----------+---------+------------+---------+-----------|\n"
  "    |    " _ "       |         |            |         |           |\n")

(define-skeleton swedish-noun-skeleton
  "Insert swedish nouns in org-drill-mode"
  ""
  "** substantiv                                                          :drill:\n"
  "   :PROPERTIES:\n"
  "   :DRILL_CARD_TYPE: hide1cloze\n"
  "   :END:\n"
  "   sv: [" (skeleton-read "svenska: ") "]\n"
  "   de: [" (skeleton-read "deutsch: ") "]\n"
  "*** dekl.\n"
  "    | sing. obestämd | sing. bestämd | pl. obestämd | pl. bestämd |\n"
  "    |----------------+---------------+--------------+-------------|\n"
  "    |    "_"            |               |              |             |\n")

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

(use-package diminish
  :ensure t)

<<<<<<< HEAD
(use-package tea-time
  :ensure t
  :config
  (setq tea-time-sound "~/.sounds/tea.ogg")
  (setq tea-time-sound-command "ogg123 -q %s")
  )

(use-package zenburn-theme
  :config
  (load-theme 'zenburn))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  )

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-dont-blink-commands nil) ;; always blink
  (setq beacon-lighter '"Λ")
  (add-to-list 'beacon-dont-blink-major-modes 'Man-mode)
  )

(use-package buffer-move
  :ensure t
  :config
  (global-set-key (kbd "<S-s-up>")     'buf-move-up)
  (global-set-key (kbd "<S-s-down>")   'buf-move-down)
  (global-set-key (kbd "<S-s-left>")   'buf-move-left)
  (global-set-key (kbd "<S-s-right>")  'buf-move-right)
  )

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  )

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package writeroom-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :init (setq auto-mode-alist
              (cons '("\\.mdml$" . markdown-mode) auto-mode-alist)))

(use-package markdown-toc
  :ensure t)

(use-package jedi
  :ensure t
  )

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
        '("◉" "◎" "⚫" "○" "►" "◇"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )
(setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
(sequence "⚑ WAITING(w)" "|")
(sequence "|" "✘ CANCELED(c)")))

(use-package pager
  :ensure t)
(use-package pager-default-keybindings
  :ensure t)

(use-package uptimes
  :ensure t)

(use-package zenburn-theme
  :config
  (load-theme 'zenburn))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  )

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-dont-blink-commands nil) ;; always blink
  ;; (setq beacon-lighter '"Λ")
  :diminish beacon-mode
  )

(use-package muttrc-mode
  :ensure t
  :config
   (setq auto-mode-alist
            (append '((".muttrc\\'" . muttrc-mode))
                    auto-mode-alist)))

(use-package offlineimap
  :ensure t
  )

(use-package xkcd
  :ensure t)

(use-package fireplace
  :ensure t)

(use-package tea-time
  :ensure t
  :config
  (setq tea-time-sound "~/.sounds/tea.ogg")
  (setq tea-time-sound-command "ogg123 -q %s")
  )

(use-package post
  :config
  (setq post-signature-pattern "\\(--\\|\\)")
  )

(use-package simple-wiki)

(use-package wikidot-mode)

(use-package printing
  :config
  (pr-update-menus t))

(require 'uniquify)

(use-package ido
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-max-prospects 50)
  (setq ido-max-window-height 0.25)
  )

(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode 1))

(global-set-key (kbd "<C-S-up>")     'windmove-up)
(global-set-key (kbd "<C-S-down>")   'windmove-down)
(global-set-key (kbd "<C-S-left>")   'windmove-left)
(global-set-key (kbd "<C-S-right>")  'windmove-right)

(global-set-key (kbd "<menu>") 'nil)

(blink-cursor-mode 0)

(setq shift-select-mode nil)

(setq mouse-yank-at-point t)

(setq transient-mark-mode t)

(show-paren-mode t)

(recentf-mode 1)

(global-set-key "\M- " 'hippie-expand)

(setq truncate-lines t)
(add-hook 'minibuffer-setup-hook
      (lambda () (setq truncate-lines nil)))

(setq kill-emacs-query-functions
      (cons (lambda () (yes-or-no-p "Really Quit Emacs? "))
            kill-emacs-query-functions))

(put 'upcase-region 'disabled nil)

(desktop-save-mode 1)
(setq desktop-restore-eager 10)
(setq desktop-save t) ;; save without asking

(defalias 'list-buffers 'ibuffer)

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 30 30 :left :elide) " "
              (size 9 -1 :right) " "
              (mode 16 16 :left :elide) " " filename-and-process)
        (mark " " (name 16 -1) " " filename)))

(electric-pair-mode 1)
(defvar markdown-electric-pairs '((?* . ?*)) "Electric pairs for markdown-mode.")
(defun markdown-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs markdown-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))
(add-hook 'markdown-mode-hook 'markdown-add-electric-pairs)

(global-set-key (kbd "<C-S-up>")     'windmove-up)
(global-set-key (kbd "<C-S-down>")   'windmove-down)
(global-set-key (kbd "<C-S-left>")   'windmove-left)
(global-set-key (kbd "<C-S-right>")  'windmove-right)

(defun timestamp ()
  (interactive)
  (insert (format-time-string "%d.%m.%Y, %H:%M")))

(defun my-count-words-region (posBegin posEnd)
  "Print number of words and chars in region."
  (interactive "r")
  (message "Counting …")
  (save-excursion
    (let (wordCount charCount)
      (setq wordCount 0)
      (setq charCount (- posEnd posBegin))
      (goto-char posBegin)
      (while (and (< (point) posEnd)
                  (re-search-forward "\\w+\\W*" posEnd t))
        (setq wordCount (1+ wordCount)))

      (message "Words: %d. Chars: %d." wordCount charCount)
      )))

(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000)) ; 90002000 is just random. you can use `most-positive-fixnum'
    (fill-paragraph nil)))
(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))
