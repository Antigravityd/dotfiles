(setq gc-cons-threshold (* 50 1000 1000))

(defun dnw/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'dnw/display-startup-time)

(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)

(setq tab-always-indent 'complete)

(setq inhibit-startup-message t) 

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(set-face-attribute 'default nil :font "Liberation Mono")
(set-fontset-font "fontset-default" nil
                  (font-spec :name "Noto"))
(set-fontset-font "fontset-default" nil
                  (font-spec :name "Font Awesome"))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable in e.g. shell
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-frame-parameter (selected-frame) 'alpha '(85 85))

(add-to-list 'default-frame-alist '(alpha 85 85))

(require 'package)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("elpa" . "https://elpa.gnu.org/packages/")))

  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))

  (eval-when-compile
    (require 'use-package))

  (setq use-package-always-ensure t)

  (add-to-list 'load-path "/home/dnw/Code/PHYTS/phits-mode")
  (require 'phits-mode)

  (add-to-list 'auto-mode-alist '("\\.inp\\'" . phits-mode))
;;  (add-to-list 'auto-mode-alist '("\\.out\\'" . phits-mode))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "12:00"))

(use-package exec-path-from-shell

  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PHITSPATH"))
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
  :init (corfu-global-mode))

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

(use-package autoinsert
  :hook (find-file . auto-insert)
  :config
  (setq auto-insert t)
  (setq auto-insert-query nil)
  (auto-insert-mode 1)
  (setq auto-insert-directory "~/emacs.d/insert/"))

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
  :init (load-theme 'doom-dark+ t))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (LaTeX-mode . rainbow-delimiters-mode)))

(use-package smartparens
  :hook ((prog-mode . smartparens-mode)
         (LaTeX-mode . smartparens-mode))
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
  :config
  (setq org-ellipsis " ▼"))

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
     (fortran . t)))

(setq org-confirm-babel-evaluate nil)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("ft" . "src fortran")))

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
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-db-autosync-mode))

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
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-view-program-selection '((output-pdf "Zathura"))))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "zsh"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(setq
 erc-nick "FlaminWalrus"
 erc-user-full-name "Duncan W")

(global-set-key (kbd "C-c e")
                (lambda ()
                  (interactive)
                  (erc-tls :server "irc.libera.chat"
                           :port "6697")))

(use-package elfeed
  :hook elfeed
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-x w" . elfeed ))

(setq elfeed-feeds
      '("http://feeds.aps.org/rss/prdsuggestions.xml"
        ;;"http://feeds.aps.org/rss/recent/physics.xml"
        ))

;; doesn't function. It'd be really nice to configure this from this orgfile
;; (use-package elfeed-org
;;   :config
;;   (setq elfeed-show-entry-switch 'display-buffer)
;;   (setq rmh-elfeed-org-files (list "/home/dnw/.emacs.d/feeds.org")))

;;(use-package bison-mode)
(add-to-list 'auto-mode-alist '("\\.g4\\'" . c-mode))

(use-package haskell-mode) ;; figure out how to defer loading until .hs is opened?

(setq gc-cons-threshold (* 2 1000 1000))
