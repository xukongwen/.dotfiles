(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar
;; neotree
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(global-set-key [f8] 'neotree-toggle)
(global-set-key [f9] 'dashboard-refresh-buffer)


;; Set up the visible bell
;;(setq visible-bell t)
;; Setting English Font
;;(set-face-attribute 'default nil :font "Ubuntu Mono 29")

;; 測試中文
;;(dolist (charset '(kana han symbol cjk-misc bopomofo))
;;    (set-fontset-font (frame-parameter nil 'font)
;;		                          charset (font-spec :family "Songti"
;;							                                            :size 39)))
;;(if (display-graphic-p)
;;  (dolist (charset ‘(kana han symbol cjk-misc bopomofo))
;;    (set-fontset-font (frame-parameter nil ‘font)
;;		      charset
;;		      (font-spec :family "KKong3"
;;				 :size 39))))
;;   快捷鍵設置
;;   org 的captrue 模式的快捷鍵
(define-key global-map "\C-cc" 'org-capture)


;;   org 的某種輸出文字格式
(setq org-export-coding-system 'utf-8)
(setq graphic-only-plugins-setting nil)
(setq make-backup-files nil)
(set-default 'truncate-lines t)
(set-display-table-slot standard-display-table 'wrap ?\ )
;; 将字体配置加入容器
(push '(progn (set-face-attribute 'default nil :font "KKong3 19")) graphic-only-plugins-setting)

;; 当GUI Emacs打开时加载容器中的代码
(add-hook 'after-make-frame-functions #'(lambda (frame)
					                                            (dolist (code graphic-only-plugins-setting)
										      (eval code))))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;(load-theme 'gruvbox)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; projectile
(projectile-mode +1)
;;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile--find-file)
;;RIME設置
(require 'rime)

(setq rime-user-data-dir "~/data/rime")

(setq rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :font "KKong3-24"
            :internal-border-width 10))

(setq default-input-method "rime"
      rime-show-candidate 'posframe)
;; 自動且切換中英文


(setq rime-disable-predicates
      '(rime-predicate-evil-mode-p
        rime-predicate-after-alphabet-char-p
        rime-predicate-prog-in-code-p))

;; 自動存檔
(add-to-list 'load-path "~/git/auto-save/") ; add auto-save to your load-path
(require 'auto-save)
(auto-save-enable)

(setq auto-save-silent t)   ; quietly save
(setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
   (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
     (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package command-log-mode)

(use-package writeroom-mode)
(global-writeroom-mode)
(setq writeroom-mode 1)

(use-package awesome-tab
	       :load-path "/home/k/.emacs.d/elpa/tab/"
	       :config
		   (awesome-tab-mode t))
(use-package ivy
	       :diminish
	       :config
		     (ivy-mode 1))

(use-package doom-modeline
	       :ensure t
	       :init (doom-modeline-mode 1)
	       :custom ((doom-modeline-height 15)))


(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; Org Mode Configuration ------------------------------------------------------

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "KKong3" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup))
(require 'org-tempo)
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))
;; org 裏面打開鏈接直接用回車
(setq org-return-follows-link t)
;; org 打開鏈接直接是本頁
(add-to-list 'org-link-frame-setup '(file . find-file))
;;(setq org-hide-emphasis-markers t)
;;(use-package org-appear)
;;(add-hook 'org-mode-hook 'org-appear-mode)
;;(setq org-appear-autolinks t)


(require 'evil)
(evil-mode 1)

(setq latex-run-command "xelatex")
;; dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

; Set the title
;;(setq dashboard-init-info "This is an init message!")
(setq dashboard-banner-logo-title "None-Note")
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
;; Set the banner
(setq dashboard-startup-banner "/home/k/Pictures/logo1.png")
;; Content is not centered by default. To center, set
(setq dashboard-center-content t)
;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)))
(setq dashboard-set-init-info nil)

(setq dashboard-set-footer nil)
(setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                   :height 1.1
                                                   :v-adjust -0.05
                                                   :face 'font-lock-keyword-face))


(add-hook 'dashboard-mode-hook '(lambda () (setq mode-line-format nil)))
(add-hook 'dashboard-mode-hook '(lambda () (setq mode-line-format nil)
                                           (doom-modeline-mode -1)))
;===================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes '(gruvbox-dark-soft))
 '(custom-safe-themes
   '("4eb6fa2ee436e943b168a0cd8eab11afc0752aebb5d974bba2b2ddc8910fca8f" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default))
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(package-selected-packages
   '(neotree projectile dashboard org-appear rime pdf-tools evil gruvbox-theme zenburn-theme helm doom-modeline ivy command-log-mode use-package))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
