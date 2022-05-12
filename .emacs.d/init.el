
;; use-package
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq use-package-compute-statistics t)


;; Statup Performance
(setq gc-cons-threshold (* 50 1000 1000))


;; No Littering
(setq user-emacs-directory "~/.cache/emacs")
(use-package no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))


;; Global Keybinds
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-q") 'kill-this-buffer)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Leader Keys
(use-package general
  :config
  (general-create-definer amnesia/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (amnesia/leader-keys
    "ff" '(consult-locate :which-key "locate file")
    "fe" '(consult-buffer :which-key "buffer search")
    "fb" '(consult-bookmark :which-key "bookmark search")
    "fn" '(bookmark-set :which-key "add bookmark")
    "fd" '(bookmark-delete :which-key "remove bookmark")
    "fv" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/init.org")))
    "cc" '(evilnc-comment-or-uncomment-lines :which-key "toggle line comments")
    "uu" '(undo-tree-visualize :which-key "show undo tree")
    "ww" '(lsp-treemacs-symbols :which-key "treemacs symbols")
    "ee" '(treemacs :which-key "toggle treemacs")))

;; Evil
(use-package move-dup
  :after (evil))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "F") 'consult-line)
  (define-key evil-normal-state-map (kbd "E") 'lsp-ui-doc-glance)
  (define-key evil-normal-state-map "Q" (kbd "@@")))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :after evil
  :config
  (define-key evil-visual-state-map (kbd "C-f") #'evil-visualstar/begin-search-forward)
  (global-evil-visualstar-mode))

(use-package evil-numbers
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map  (kbd "C-d") 'evil-numbers/dec-at-pt))

(defun meain/evil-yank-advice (orig-fn beg end &rest args)
  (pulse-momentary-highlight-region beg end)
  (apply orig-fn beg end args))

(advice-add 'evil-yank :around 'meain/evil-yank-advice)


;; Move Region
(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  (define-key evil-normal-state-map (kbd "K") 'drag-stuff-up)
  (define-key evil-normal-state-map (kbd "J")  'drag-stuff-down)
  (define-key evil-visual-state-map (kbd "K") 'drag-stuff-up)
  (define-key evil-visual-state-map (kbd "J")  'drag-stuff-down))


;; Sudo Save
(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))


;; Basic UI
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(set-default 'truncate-lines t)

(recentf-mode 1)
(setq recentf-max-menu-items 25)


;; Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-center-content t)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-footer nil)
(setq dashboard-items '((recents  . 10)
                        (projects . 5)))


;; Hex Colour
(use-package hex-colour-mode
  :load-path "~/.emacs.d/elisp/hex-colour-mode"
  :hook ((text-mode . hex-colour-mode)
	 (prog-mode . hex-colour-mode)))


;; Awesome Tab
(defun my-awesome-tab-buffer-groups ()
  "`awesome-tab-buffer-groups' control buffers' group rules.

Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `awesome-tab-get-group-name' with project name."
  (list
   (cond
    ((or (string-equal "*" (substring (buffer-name) 0 1))
         (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode
                            )))
     "Emacs")
    ((derived-mode-p 'eshell-mode)
     "EShell")
    ((derived-mode-p 'dired-mode)
     "Dired")
    (t
     (awesome-tab-get-group-name (current-buffer))))))


(use-package awesome-tab
  :load-path "~/.emacs.d/elisp/awesome-tab"
  :config
  (setq awesome-tab-buffer-groups-function #'my-awesome-tab-buffer-groups)
  (awesome-tab-mode t))


;; Indent Guides
(use-package highlight-indent-guides
  :hook ((text-mode . highlight-indent-guides-mode)
	 (prog-mode . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0.05))


;; Line Numbers
(global-hl-line-mode)

(column-number-mode)
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-widen t)
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Font
(defvar amnesia/default-font-size 150)
(set-face-attribute 'default nil :font "Source Code Pro" :height amnesia/default-font-size)


;; Theme
(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package all-the-icons)

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package diminish)

;; Modeline
(use-package doom-modeline
  :hook (after-init-hook)
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 40)
           (doom-modeline-lsp t)
           (doom-modeline-buffer-encoding nil)
           (doom-modeline-persp-name nil)
           (doom-modeline-buffer-file-name-style 'truncate-except-project)
           (doom-modeline-major-mode-icon nil)
           (doom-modeline-github nil)))


;; Scrolling
(setq
 scroll-conservatively 1000
 scroll-margin 4
 scroll-step 1
 mouse-wheel-scroll-amount '(4 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 redisplay-dont-pause t
 fast-but-imprecise-scrolling nil
 jit-lock-defer-time 0)


;; Rainbow Delimiters
(use-package rainbow-delimiters
  :defer t
  :hook ((org-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)))


;; Which Key
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))


;; Smart Parenthesies
(use-package smartparens
  :hook ((text-mode . smartparens-mode)
         (prog-mode . smartparens-mode)))


;; Evil Nerd Commenter
(use-package evil-nerd-commenter
  :defer t)


;; Consult
(use-package consult
  :bind (
         ;;("C-c h" . consult-history)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ;; ("C-x p b" . consult-project-buffer)
         ("M-p" . consult-yank-pop)
         ;; ("M-g f" . consult-flymake)
         ;; M-s bindings (search-map)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  ;; (setq register-preview-delay 0.5)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.

  ;; Use Consult to select xref locations with preview

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;;  consult-theme
  ;;  :preview-key '(:debounce 0.2 any)
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-bookmark consult--source-recent-file
  ;;  consult--source-project-recent-file
  ;;  :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  ;; (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
    ;;;; 1. project.el (the default)
    ;;;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
    ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
    ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

;; Vertico
;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
;; (defun crm-indicator (args)
;;   (cons (format "[CRM%s] %s"
;;                 (replace-regexp-in-string
;;                  "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
;;                  crm-separator)
;;                 (car args))
;;         (cdr args)))

;; Do not allow the cursor in the minibuffer prompt
;; (setq minibuffer-prompt-properties
;;       '(read-only t cursor-intangible t face minibuffer-prompt))

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
;; (setq read-extended-command-predicate

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)


;; Corfu
(use-package corfu
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :hook
  (text-mode . corfu-mode)
  (prog-mode . corfu-mode)
  (corfu-mode . corfu-history-mode)
  :custom
  (tab-always-indent 'complete)
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)
  (corfu-auto-prefix 1)
  (corfu-min-width 30)
  (corfu-max-width corfu-min-width)
  (corfu-auto-delay 0)
  (corfu-quit-no-match 'separator)
  :config
  (general-add-advice '(corfu--setup corfu--teardown) :after 'evil-normalize-keymaps)
  (evil-make-overriding-map corfu-map))





(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/"))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

(use-package corfu-doc
  :after corfu
  :hook (corfu-mode . corfu-doc-mode)
  :general (:keymaps 'corfu-map
                     [remap corfu-show-documentation] #'corfu-doc-toggle
                     "M-j" #'corfu-doc-scroll-up
                     "M-k" #'corfu-doc-scroll-down)
  :custom
  (corfu-doc-delay 0.0)
  (corfu-doc-max-width 70)
  (corfu-doc-max-height 20))


;; Orderless
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; WS Butler
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))


;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-p" . projectile-command-map)
  :init
  (when (file-directory-p "~/drives/Local/Projects/Godot")
    (setq projectile-project-search-path '("~/drives/Local/Projects/Godot"))))


;; Magit
(use-package magit)


;; LSP Configuration
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-completion-provider :none)
  (lsp-eldoc-enable-hover t)
  (read-process-output-max (* 1024 1024)) ;; Control how much data emacs can read in one pass from server. For efficiency.
  (lsp-eldoc-hook nil)
  :init
  (setq lsp-keymap-prefix "C-l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-folding nil)
  (setq lsp-signature-function 'lsp-signature-posframe)
  (setq lsp-signature-doc-lines 2))

(use-package flycheck
  :after lsp-mode
  :init
  (global-flycheck-mode))

(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ("C-c u" . lsp-ui-imenu)
        ("M-i" . lsp-ui-doc-focus-frame))
  (:map lsp-mode-map
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph))
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-cursor)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))  ;; Border color of the frame
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
)

(use-package lsp-treemacs
  :after lsp-mode)

(use-package consult-lsp
  :after lsp-mode)

(use-package posframe)


;; Treesitter
(use-package tree-sitter)
(use-package tree-sitter-langs)


;; C#
(use-package csharp-mode
  :mode "\\.cs\\'"
  :commands (lsp lsp-deferred)
  :hook (csharp-mode . lsp-deferred))


(use-package lsp-haskell
  :mode "\\.hs\\'"
  :commands (lsp lsp-deferred))

(use-package haskell-mode
  :mode "\\.hs\\'"
  :commands (lsp lsp-deferred)
  :hook ((haskell-mode . lsp-deferred)
	 (haskell-literate-mode . lsp-deferred))
  :config
  (setq haskell-process-type 'cabal-repl))
	 ;; (turn-on-haskell-doc-mode . haskell-mode)
	 ;; (turn-on-haskell-indentation . haskell-mode)))


;; Elisp
(use-package elisp-format)


;; Runtime Performance
(setq gc-cons-threshold (* 2 1000 1000))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" default))
 '(package-selected-packages
   '(flycheck haskell-mode awesome-tab evil-visualstar rainbow-mode hexcolour consult-lsp elisp-format corfu-doc ws-butler which-key vertico use-package undo-tree tree-sitter-langs tree-sitter-indent smartparens ranger rainbow-delimiters org-bullets orderless no-littering move-dup minions magit lsp-ui lsp-treemacs kind-icon highlight-numbers helpful general evil-surround evil-numbers evil-nerd-commenter evil-collection drag-stuff doom-themes doom-modeline diminish dashboard csharp-mode counsel-projectile corfu consult)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic))))))
