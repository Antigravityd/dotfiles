(setq gc-cons-threshold (* 50 1000 1000))

(defun dnw/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'dnw/display-startup-time)

;;(server-start)

(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)

(setq tab-always-indent 'complete)
(setq align-to-tab-stop nil)

;; default-frame-alist works with --daemon

(add-to-list 'default-frame-alist
             '(font . "Iosevka-10"))

(defun dnw/unicode-fonts ()
  (setf use-default-font-for-symbols nil)
  (set-fontset-font t 'unicode "Noto Emoji" nil 'append)
  (set-fontset-font t 'emoji "Noto Color Emoji"))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'dnw/unicode-fonts)
  (dnw/unicode-fonts))

(setq inhibit-startup-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable in e.g. shell
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                Info-mode-hook
                ement-room-mode-hook
                elfeed-show-mode-hook
                pdf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-frame-parameter (selected-frame) 'alpha '(100 100))

(add-to-list 'default-frame-alist '(alpha 100 100))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(add-to-list 'load-path "/home/dnw/Code/PyPHITS/phits-mode")
(require 'phits-mode)

(add-to-list 'auto-mode-alist '("\\.inp\\'" . phits-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . phits-mode))

(use-package exec-path-from-shell

  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GUIX_PROFILE" "PHITSPATH"))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package vertico
  :init (vertico-mode)
  :custom (vertico-cycle t))

(use-package corfu
  :custom (corfu-cycle t)
  :init (global-corfu-mode))

;; (use-package kind-icon
;;   :ensure t
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Doesn't work :(

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-r" . consult-history))
  :custom (completion-in-region-function #'consult-completion-in-region))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(defun dnw/prompt-date ()
  (let ((date (read-string "Due date: "))
                                          (now  (split-string (format-time-string "%e %B %Y" (current-time)))))
                                      (cond ((equal date "")
                                             (concat now))
                                            ((equal (substring date 0 1) "+")
                                             (concat (number-to-string (+ (string-to-number (car now))
                                                                          (string-to-number (substring date 1))))
                                                     " "
                                                     (cadr now)
                                                     " "
                                                     (caddr now)))
                                            ((= (length date) 2)
                                             (concat date
                                                     " "
                                                     (cadr now)
                                                     " "
                                                     (caddr now))))))

(setq dnw/autoinsert-latex-presets
      '(("Physics" . (nil "\\documentclass{article}\n\n"

                          "\\usepackage[letterpaper]{geometry}\n"
                          "\\usepackage{tgpagella}\n"
                          "\\usepackage{amsmath}\n"
                          "\\usepackage{amssymb}\n"
                          "\\usepackage{amsthm}\n"
                          "\\usepackage{tikz}\n"
                          "\\usepackage{minted}\n"
                          "\\usepackage{physics}\n"
                          "\\usepackage{siunitx}\n\n"

                          "\\sisetup{detect-all}\n"
                          "\\newtheorem{plm}{Problem}\n"
                          "\\renewcommand*{\\proofname}{Solution}\n\n"


                          "\\title{" (read-string "Title: ") "}\n"
                          "\\author{Duncan Wilkie}\n"
                          "\\date{" (dnw/prompt-date) "}\n\n"

                          "\\begin{document}\n\n"

                          "\\maketitle\n\n"

                          -

                          "\n\n\\end{document}"))
        ("Math" . (nil "\\documentclass{article}\n\n"

                          "\\usepackage[letterpaper]{geometry}\n"
                          "\\usepackage{tgpagella}\n"
                          "\\usepackage{amsmath}\n"
                          "\\usepackage{amssymb}\n"
                          "\\usepackage{amsthm}\n"
                          "\\usepackage{tikz}\n"
                          "\\usepackage{minted}\n"
                          "\\usepackage{physics}\n"
                          "\\usepackage{siunitx}\n\n"

                          "\\sisetup{detect-all}\n"
                          "\\newtheorem{plm}{Problem}\n\n"


                          "\\title{" (read-string "Title: ") "}\n"
                          "\\author{Duncan Wilkie}\n"
                          "\\date{" (dnw/prompt-date) "}\n\n"

                          "\\begin{document}\n\n"

                          "\\maketitle\n\n"

                          -

                          "\n\n\\end{document}"))
        ("Default" . ("options, RET: " "\\documentclass[" str & 93 | -1 123
                      (read-string "class: ")
                      "}\n"
                      ("package, %s: " "\\usepackage["
                       (read-string "options, RET: ")
                       & 93 | -1 123 str "}\n")
                      _ "\n\\begin{document}\n"
                      _ "\n\\end{document}"))))

(use-package autoinsert
  :hook (find-file . auto-insert)
  :init
  (setq auto-insert t)
  (setq auto-insert-query nil)
  (auto-insert-mode t)
  :config
  (assoc-delete-all 'latex-mode auto-insert-alist)
  (define-auto-insert 'latex-mode
    (lambda ()
      (let* ((presets (mapcar (lambda (pair) (car pair))
                              dnw/autoinsert-latex-presets))
             (choice (completing-read "Preset:" presets)))
        (skeleton-insert (assoc choice dnw/autoinsert-latex-presets))))))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package diminish)

(use-package all-the-icons) ;; requires M-x all-the-icons-install-fonts on first load

(use-package doom-themes
  :init (load-theme 'doom-tomorrow-night t))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (LaTeX-mode . rainbow-delimiters-mode)))

(use-package smartparens
  :hook ((prog-mode . smartparens-mode)
         (LaTeX-mode . smartparens-mode)
         (org-mode . smartparens-mode))
  :config
  (require 'smartparens-latex))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(setq require-final-newline t)

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(defun dnw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :commands (org-capture org-agenda)
  :hook (org-mode . dnw/org-mode-setup)
  :bind ("C-c C-x C-l" . org-latex-preview)
  :config
  (setq org-ellipsis " ▼")
  (setq org-latex-create-formula-image-program 'imagemagick))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(with-eval-after-load 'org-faces (dolist (face '((org-level-1 . 1.2)
                                                 (org-level-2 . 1.1)
                                                 (org-level-3 . 1.05)
                                                 (org-level-4 . 1.0)
                                                 (org-level-5 . 1.0)
                                                 (org-level-6 . 1.0)
                                                 (org-level-7 . 1.0)
                                                 (org-level-8 . 1.0)))
                                   (set-face-attribute (car face) nil :font "Liberation Sans" :weight 'regular :height (cdr face)))

                      (set-face-attribute 'fixed-pitch nil :font "Iosevka" :weight 'regular :height 1.0)
                      (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
                      (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
                      (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
                      (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
                      (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
                      (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
                      (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(setq org-hide-emphasis-markers t)

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(defun dnw/org-mode-visual-fill ()
  (setq visual-fill-column-width 170
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . dnw/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (fortran . t)
     (gnuplot t)
     (R . t)
     (sqlite . t)
     (haskell . t)
     (lua . t)
     (shell . t)
     (C . t)))

  (setq org-confirm-babel-evaluate nil)

  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python3"))
  (add-to-list 'org-structure-template-alist '("ft" . "src fortran"))
  (add-to-list 'org-structure-template-alist '("gp" . "src gnuplot"))
  (add-to-list 'org-structure-template-alist '("sql" . "src sqlite"))
  (add-to-list 'org-structure-template-alist '("r" . "src R"))
  (add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
  (add-to-list 'org-structure-template-alist '("lu" . "src lua"))
  (add-to-list 'org-structure-template-alist '("sys" . "src C")))

(defun dnw/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/config.org"))

  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dnw/org-babel-tangle-config)))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "/home/dnw/Roam")
  (org-roam-completion-everywhere t)
  (org-roam-db-node-include-function
   (defun dnw/org-roam-include ()
     (not (member "drill" (org-get-tags)))))
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarowed t)
     ("i" "idea" plain
      "* Motivation\n\n%?\n\n* Similar Work\n\n* Feasibility\n\n* Implementation"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Idea")
      :unnarrowed t)
     ("p" "project" plain
      "* Description\n\n%?\n\n** Collaborators\n\n** Stack\n\n* Tasks"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)
     ("a" "article" plain
      "* Summary\n\n%?\n\n* Context"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Article")
      :unnarrowed t)
     ("m" "musing" plain
      "* %?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Musing")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-db-autosync-mode))

(use-package org-drill)

(use-package org-present)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-ahgo --group-directories-first")))

(use-package dired-single
  :after dired)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  ((c-mode) . lsp)
  ((LaTeX-mode) . lsp)
  (lsp-completion-mode . dnw/lsp-completion)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (defun dnw/lsp-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :config
  (lsp-enable-which-key-integration t)
  :custom
  (lsp-completion-provider :none))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (setq lsp-ui-doc-position 'bottom))


(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

;; (use-package company
;;   :after lsp-mode
;;   :hook (prog-mode . company-mode)
;;   :bind
;;   (:map company-active-map
;;         ("<tab>" . company-complete-selection))
;;   (:map lsp-mode-map
;;         ("<tab>" . company-indent-or-complete-common))
;;   :custom
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0.0))

;; (eval-after-load 'company
;;    '(add-to-list
;;      'company-backends '(company-irony-c-headers
;;                          company-irony
;;                          company-rtags)))


;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; (use-package projectile
;;   :diminish projectile-mode
;;   :config (projectile-mode)
;;   :custom ((projectile-completion-system 'ivy))
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :init
;;   (when (file-directory-p "~")
;;     (setq projectile-project-search-path '("~")))
;;   (setq projectile-switch-project-action #'projectile-dired))

;; (use-package counsel-projectile
;;   :config (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package yasnippet
;;   :hook ((prog-mode LaTeX-mode) . yas-minor-mode)
;;   :config
;;   (yas-reload-all))

;; (use-package yasnippet-snippets)

(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master t)
  (setq LaTeX-command "latex -shell-escape")
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-view-program-selection '((output-pdf "Zathura")))
  (setq TeX-electric-sub-and-superscript t))

;; Done from Guix
;; (pdf-loader-install)
;; (use-package pdf-tools
;;   :init
;;   (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
;;   (pdf-loader-install))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "zsh"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(defun dnw/prompt-prefix ()
  (let ((guess (apply
                'concat
                (-map
                 (lambda (x)
                   (if (string= x "dnw")
                        "~/"
                     (concat x "/")))
                 (seq-subseq
                  ;; extra ""'s are to prevent slicing errors
                  (cons "" (cons "" (split-string (eshell/pwd) "/")))
                  -2)))))
    (if (string= guess "home/~/")
        "~"
      (string-remove-suffix "/" guess))))

(remove-hook 'eshell-output-filter-functions
             'eshell-postoutput-scroll-to-bottom)

(defun dnw/prompt ()
  (concat
   (propertize
    (dnw/prompt-prefix)
    'font-lock-face '(:foreground "#4068A3"))
   (propertize " ᛋ" 'font-lock-face '(:foreground "#CB77F9"))
   (propertize " " 'font-lock-face "default")))

(setq eshell-prompt-regexp "^[^ᛋ\n]* ᛋ ")

(setq eshell-highlight-prompt nil
      eshell-prompt-function #'dnw/prompt)

(setq eshell-banner-message "We will reinvent the wheel. They used triangles. 🗿\n\n")

;; (add-to-list eshell-visual-subcommands '("guix" "search"))
;; (add-to-list eshell-visual-subcommands '("guix" "install"))
;; (add-to-list eshell-visual-subcommands '("guix" "remove"))

(setq eshell-destroy-buffer-when-process-dies t)

(setq eshell-prefer-lisp-functions t)
(setq eshell-prefer-lisp-variables t)

;; (use-package notmuch
;;   :config
;;   (setq mail-user-agent 'message-user-agent)
;;   (setq user-mail-address "antigravityd@gmail.com"
;;         user-full-name "Duncan Wilkie")
;;   (setq smtpmail-smtp-server "smtp.gmail.com"
;;         message-mail-send-function 'message-smtpmail-send-it)
;;   (setq smtpmail-debug-info t)
;;   (setq message-default-mail-headers "Cc: \nBcc: \n")
;;   (setq message-auto-save-directory "~/.mail/drafts")
;;   (setq message-kill-buffer-on-exit t)
;;   (setq message-directory "~/.mail/sent")
;;   (setq message-signature "-Duncan Wilkie"))

;; (require 'mu4e)

;; (setq mail-user-agent 'mu4e-user-agent)
;; (setq mu4e-get-mail-command "mbsync -a")

;; (setq user-full-name  "Duncan Wilkie")
;; (setq mu4e-compose-signature  "-Duncan Wilkie")

;; (setq message-kill-buffer-on-exit t)

;; (require 'smtpmail)

;; (setq message-send-mail-function 'smtpmail-send-it)

;; ;;; Call the oauth2ms program to fetch the authentication token
;; (defun fetch-access-token ()
;;   (with-temp-buffer
;;     (call-process "oauth2ms" nil t nil "--encode-xoauth2")
;;     (buffer-string)))

;; ;;; Add new authentication method for xoauth2
;; (cl-defmethod smtpmail-try-auth-method
;;   (process (_mech (eql xoauth2)) user password)
;;   (let* ((access-token (fetch-access-token)))
;;     (smtpmail-command-or-throw
;;      process
;;      (concat "AUTH XOAUTH2 " access-token)
;;      235)))

;; ;;; Register the method
;; (with-eval-after-load 'smtpmail
;;   (add-to-list 'smtpmail-auth-supported 'xoauth2))

;; (setq message-send-mail-function   'smtpmail-send-it
;;       smtpmail-default-smtp-server "smtp.example.com"
;;       smtpmail-smtp-server         "smtp.example.com"
;;       smtpmail-stream-type  'starttls
;;       smtpmail-smtp-service 587)

;; (setq mu4e-contexts
;;       `(,(make-mu4e-context
;;         :name "Personal Gmail"
;;         :enter-func (lambda () (mu4e-message "Switching to Personal Gmail..."))
;;         :match-func  (lambda (msg)
;;                       (when msg
;;                         (string-match-p "/gmail-personal" (mu4e-message-field msg :maildir))))
;;         :vars '((user-mail-address . "antigravityd@gmail.com") ;; set up example Gmail config from manual
;;                 (mu4e-drafts-folder . "/gmail-personal/[Gmail].Drafts")
;;                 (mu4e-sent-folder . "/gmail-personal/[Gmail].Sent Mail")
;;                 (mu4e-trash-folder . "/gmail-personal/[Gmail].Trash")
;;                 (mu4e-sent-messages-behavior . delete)
;;                 (assoc 'mu4e-maildir-shortcuts '((:maildir "/gmail-personal/Inbox" :key ?i)
;;                                             (:maildir "/gmail-personal/[Gmail].Sent Mail" :key ?s)
;;                                             (:maildir "/gmail-personal/[Gmail].Trash" :key ?t)
;;                                             (:maildir "/gmail-personal/[Gmail].All Mail" :key ?a)))
;;                 (starttls-use-gnutls . t)
;;                 (assoc smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
;;                 (assoc smtpmail-auth-credentials  '(("smtp.gmail.com" 587 "antigravityd@gmail.com" nil)))
;;                 (smtpmail-smtp-server . "smtp.gmail.com")
;;                 (smtpmail-smtp-service . 587)))
;;       ,(make-mu4e-context
;;         :name "LSU"
;;         :enter-func (lambda () (mu4e-message "Switching to LSU email..."))
;;         :match-func (lambda (msg)
;;                       (when msg
;;                         (string-match-p "/lsu" (mu4e-message-field msg :maildir))))
;;         :vars '((user-mail-address . "dwilk14@lsu.edu")
;;                 (smtpmail-smtp-server . "smtp-mail.outlook.com")
;;                 (smtpmail-stream-type . starttls)
;;                 (smtpmail-smtp-service . 587)))))
      ;; `(make-mu4e-context
      ;; 	:name "Professional Gmail"
      ;; 	:enter-func (lambda () (mu4e-message "Switching to Professional Gmail..."))
      ;; 	:match-func  (lambda (msg)
      ;; 		       (when msg
      ;; 			 (string= (mu4e-message-field msg :maildir) "/gmail-professional")))
      ;; 	:vars '((user-mail-address . "duncannwilkie@gmail.com")
      ;; 		(user-full-name . "Duncan Wilkie")
      ;; 		(mu4e-compose-signature . "-Duncan Wilkie")))
      ;; `(make-mu4e-context
      ;;   :name "Lab"
      ;;   :enter-func (lambda () (mu4e-message "Switching to Lab email..."))
      ;;   :match-func  (lambda (msg)
      ;;                  (when msg
      ;;                    (string= (mu4e-message-field msg :maildir) "/lab")))
      ;;   :vars '((user-mail-address . "duncan@spartanphysics.com")
      ;;           (user-full-name . "Duncan Wilkie")
      ;;           (mu4e-compose-signature . "-Duncan Wilkie")))

(setq user-mail-address "antigravityd@gmail.com"
      user-full-name "Duncan Wilkie")

(setq gnus-select-method '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port "imaps")
                                  (nnimap-stream ssl)))
(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq
 erc-nick "FlaminWalrus"
 erc-user-full-name "Duncan W")

(global-set-key (kbd "C-c e")
                (lambda ()
                  (interactive)
                  (erc-tls :server "irc.libera.chat"
                           :port "6697")))

;; (package-install 'quelpa-use-package)
;; (require 'quelpa-use-package)

;; (use-package plz
;;   :quelpa (plz :fetcher github :repo "alphapapa/plz.el"))

;; (use-package plz
;;   :quelpa (plz :fetcher github :repo "alphapapa/plz.el"))

;; (use-package ement
;;   :quelpa (ement :fetcher github :repo "alphapapa/ement.el"))

(defun dnw/elfeed-show-mode-visual-fill ()
  (setq visual-fill-column-width 130
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(defun dnw/render-latex ()
  (let ((current-prefix-arg '(2)))
    (switch-to-buffer "*elfeed-entry*")
    (call-interactively 'org-latex-preview)))

(use-package elfeed
  :hook ((elfeed-show-mode . dnw/elfeed-show-mode-visual-fill))
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer)
  (setq elfeed-feeds
        '("http://feeds.aps.org/rss/allsuggestions.xml"  ;; Physics
          "http://feeds.aps.org/rss/recent/rmp.xml"

          "https://lexi-lambda.github.io/feeds/all.rss.xml" ;; CS
          "https://blog.functorial.com/feed.rss"

          "https://www.ams.org/rss/jams.rss"
          "https://jaireetschahal.substack.com/feed" ;; Math
          "https://golem.ph.utexas.edu/category/atom10.xml"
          "https://homotopytypetheory.org/feed/"

          "https://notrelated.xyz/rss" ;; Misc
          ))
  :bind
  ("C-x w" . elfeed ))


;; doesn't function. It'd be really nice to configure this from this orgfile
;; (use-package elfeed-org
;;   :config
;;   (setq elfeed-show-entry-switch 'display-buffer)
;;   (setq rmh-elfeed-org-files (list "/home/dnw/.emacs.d/feeds.org")))

;;(use-package bison-mode)
(add-to-list 'auto-mode-alist '("\\.g4\\'" . c-mode))

(use-package haskell-mode
  :bind ("C-c C-h" . hoogle)) ;; figure out how to defer loading until .hs is opened?

;; TRAMP can't find necessary binaries on Guix machines without this after Emacs 28
(add-to-list 'tramp-remote-path "/run/current-system/profile/bin")

(setq Info-use-header-line nil)

(setq markdown-command "pandoc")

(use-package emms
  :config
  (emms-all)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  :bind
  ("<XF86AudioPlay>" . emms-start)
  ("<XF86AudioPause>" . emms-pause)
  ("<XF86AudioNext>" . emms-next)
  ("<XF86AudioNext>" . emms-previous))

(add-hook 'xhtml-mode-hook (lambda () (call-interactively 'shr-render-buffer)))

(use-package gnuplot)

(require 'exwm-xim)
(require 'exwm-randr)
(require 'exwm-systemtray)

(defun dnw/exwm-config ()
  "My configuration of EXWM, adapted from the example."
  ;; Set the initial workspace number.
  (unless (get 'exwm-workspace-number 'saved-value)
    (setq exwm-workspace-number 4))
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  (setq exwm-randr-workspace-output-plist  '(1 "VGA1" 2 "VGA1" 3 "VGA1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))

  ;; Global keybindings.
  (unless (get 'exwm-input-global-keys 'saved-value)
    (setq exwm-input-global-keys
          `(
            ;; 's-r': Reset (to line-mode).
            ([?\s-r] . exwm-reset)
            ;; 's-w': Switch workspace.
            ([?\s-w] . exwm-workspace-switch)
            ;; 's-p': Launch application.
            ([?\s-p] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            ;; 's-P': retrieve a password from password store
            ([?\s-P] . password-store-copy)
            ;; 's-N': Switch to certain workspace.
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9)))))
  ;; Line-editing shortcuts
  (unless (get 'exwm-input-simulation-keys 'saved-value)
    (setq exwm-input-simulation-keys
          '(([?\C-b] . [left])
            ([?\C-f] . [right])
            ([?\C-p] . [up])
            ([?\C-n] . [down])
            ([?\C-a] . [home])
            ([?\C-e] . [end])
            ([?\M-v] . [prior])
            ([?\C-v] . [next])
            ([?\C-s] . [C-f])
            ([?\C-d] . [delete])
            ([?\C-g] . [ESC])
            ([?\M-b] . [C-left])
            ([?\M-f] . [C-right])
            ([?\C-k] . [S-end delete])
            ([?\C-w] . [C-x])
            ([?\M-w] . [C-c])
            ([?\C-y] . [C-v])
            ([?\C-/] . [C-z])
            ([?\C-x ?h] . [C-a]))))
  ;; Enable EXWM
  (exwm-enable)
  (exwm-xim-enable)
  (exwm-randr-enable)
  (exwm-systemtray-enable)
  (push ?\C-\\ exwm-input-prefix-keys))

(use-package exwm
  :config (dnw/exwm-config))

(use-package password-store
  :config (pinentry-start))

(use-package exwm-edit)

(use-package eww
  :config
  (setq browse-url-browser-function 'eww-browse-url)
  (setq eww-search-prefix "https://librex.devol.it/search.php?q=")
  (setq dnw/eww-auto-readable-blacklist '("https://librex.devol.it"))

  (defun dnw/eww-auto-readable ()
    (if (seq-some (lambda (bl) (string-prefix-p bl (eww-current-url)))
                  dnw/eww-auto-readable-blacklist)
        nil
      (eww-readable)))

  (defun dnw/eww-unreadable ()
    (interactive)
    (let ((hook eww-after-render-hook))
      (setq eww-after-render-hook nil)
      (eww-reload t)
      (setq eww-after-render-hook hook))))

  ;; (add-hook 'eww-after-render-hook #'dnw/eww-auto-readable)
  ;; :hook (eww-after-render-hook . dnw/eww-auto-readable) this, for some reason, doesn't work
  ;; :bind ("U" . dnw/eww-unreadable)

(use-package lean-mode)

(use-package company-lean)

(use-package racket-mode)

(use-package geiser
  :config (require 'geiser-guile))

(with-eval-after-load 'quail (defun quail-completion ()))

(setq gc-cons-threshold (* 2 1000 1000))
