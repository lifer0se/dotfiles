;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Source Code Pro" :size 18 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Source Code Pro" :size 19))

(global-auto-revert-mode 1)

(setq global-auto-revert-non-file-buffers t)
(setq doom-theme 'doom-gruvbox)
(setq display-line-numbers-type t)
(setq org-directory "~/org/")
(setq display-line-numbers 'relative)
(require 'whitespace)

(setq centaur-tabs-set-bar 'over
      centaur-tabs-set-icons t
      centaur-tabs-gray-out-icons 'buffer
      centaur-tabs-height 28
      centaur-tabs-set-modified-marker t
      centaur-tabs-style "bar"
      centaur-tabs-modified-marker "•")
(map! :leader
      :desc "Toggle tabs globally" "t c" #'centaur-tabs-mode
      :desc "Toggle tabs local display" "t C" #'centaur-tabs-local-mode)
(evil-define-key 'normal centaur-tabs-mode-map (kbd "g <right>") 'centaur-tabs-forward
                                               (kbd "g <left>")  'centaur-tabs-backward
                                               (kbd "g <down>")  'centaur-tabs-forward-group
                                               (kbd "g <up>")    'centaur-tabs-backward-group)

(evil-define-key 'normal general-override-mode-map (kbd "C-a") 'evil-numbers/inc-at-pt
                                                   (kbd "C-d") 'evil-numbers/dec-at-pt)

(evil-define-key 'normal general-override-mode-map (kbd "C-s") 'save-buffer)

(map! :leader
      :desc "Toggle comments" "c c" #'comment-line)

(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))
(global-rainbow-mode 1 )
(beacon-mode 1)
