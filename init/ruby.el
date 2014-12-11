;; -*-emacs-lisp-*-

;;;
;;; Ruby Mode
;;;
(when (locate-library "ruby-mode")
  ;; For Emacs 24, install ruby-mode and inf-ruby-mode via package.el
  (require 'ruby-mode)

  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist
               '("[rR]akefile" . ruby-mode))

  (add-to-list 'interpreter-mode-alist  '("ruby" . ruby-mode))

  ;; Above configuration works on even ruby-mode 1.0, which is
  ;; distributed with Emacs 24 build-in.

  (when (locate-library "rvm")
    (require 'rvm)
    (rvm-use-default)
    (when (executable-find "pry")
      (setq inf-ruby-default-implementation "pry")))

  (when (locate-library "inf-ruby")
    (require 'inf-ruby))

  (when (fboundp 'inf-ruby-keys)
    ;; Unlike most major modes, key-bindings of ruby-mode (at
    ;; least 1.8.6) is done by `inf-ruby-keys', which is called
    ;; from `ruby-mode-hook' in the current implementation of
    ;; 'ruby-mode'.
    ;;
    ;; Thus, using `define-key' on `ruby-mode-map' is not working,
    ;; unless we call `define-key' in the `ruby-mode-hook' after
    ;; `inf-ruby-keys'.
    (add-hook 'ruby-mode-hook 'inf-ruby-keys)
    (add-hook 'ruby-mode-hook
              (lambda ()
                (define-key ruby-mode-map [(control ?c) ?\!] 'run-ruby)
                (define-key ruby-mode-map [(control ?c) (control ?c)]
                  'comment-region)
                (define-key ruby-mode-map [(control ?c) (control ?b)]
                  'ruby-send-buffer)
                (define-key ruby-mode-map [(control ?c) (control ?e)]
                  'ruby-send-block))
              ;; Make sure that this function is appended, not prepended
              'append))

  (when (boundp 'inf-ruby-minor-mode-map)
    ;; In inf-ruby version 2.2.7, `inf-ruby-keys' is not defined, so
    ;; above sexp is not evaulated.  Instead, the minor mode map,
    ;; `inf-ruby-minor-mode-map' is defined.  So I'll put all key
    ;; bindings in it.
    (define-key ruby-mode-map [(control ?c) ?\!] 'run-ruby)
    (define-key ruby-mode-map [(control ?c) (control ?c)]
      'comment-region)
    (define-key ruby-mode-map [(control ?c) (control ?b)]
      'ruby-send-buffer)
    (define-key ruby-mode-map [(control ?c) (control ?e)]
      'ruby-send-region))


  (if (fboundp 'ruby-send-buffer)
      (lwarn '(dot-emacs) :warning
             "`ruby-send-buffer' already defined.  need to update init.el")
    (defun ruby-send-buffer ()
      "Send the current buffer to the inferior Ruby process."
      (interactive)
      (save-excursion
        (save-restriction
          (widen)
          (ruby-send-region (point-min) (point-max))))))

  ;;
  ;; RVM irb uses its own prompt pattern which does not work properly
  ;; with ruby-mode.  There are two ways to resolve this.  One is to modify
  ;; ruby-mode so that it can recognize RVM prompt pattern.  Another one is
  ;; to modify $HOME/.irbrc to force RVM irb to use the original prompt pattern.
  ;;
  ;; [http://bugs.ruby-lang.org/issues/6950]
  ;;
  ;; Since it is not the defect of ruby-mode, I decided to use $HOME/.irbrc
  ;; way.  You may need to insert following sentence in your .irbrc:
  ;;
  ;;   IRB.conf[:PROMPT_MODE] = :DEFAULT
  ;;
  ;; I'll leave the first solution as comments here in case of needs:
  ;;
  ;; The first prompt of Ruby 1.8 irb looks like "irb(main):001:0> ".
  ;; Depending on the current string token, the subsequent prompts
  ;; look like one of:
  ;;
  ;;   irb(main):001:0> _
  ;;   irb(main):005:0* _
  ;;   irb(main):006:0' _
  ;;   irb(main):007:0" _
  ;;
  ;; The first prompt of Ruby 1.9 irb looks like "ruby-1.9.2-p180 :001 > ".
  ;; And subsequent prompts look like one of:
  ;;
  ;;   ruby-1.9.2-p180 :001 > _
  ;;   ruby-1.9.2-p180 :002 >    _
  ;;   ruby-1.9.2-p180 :003"> _
  ;;   ruby-1.9.2-p180 :004'> _
  ;;
  ;; ----
  ;;
  ;; (and (string-equal inferior-ruby-first-prompt-pattern
  ;;                    "^irb(.*)[0-9:]+0> *")
  ;;      (setq inferior-ruby-first-prompt-pattern
  ;;            "^\\(?:irb(.*)[0-9:]+0\\|ruby[-0-9.a-z]+ *:[0-9]+ *\\)> *"))
  ;; (and (string-equal inferior-ruby-prompt-pattern
  ;;                    "^\\(irb(.*)[0-9:]+[>*\"'] *\\)+")
  ;;      (setq inferior-ruby-prompt-pattern
  ;;            (concat "^\\(?:\\(irb(.*)[0-9:]+[>*\"'] *\\)+\\|"
  ;;                    "ruby[-0-9.a-z]+ *:[0-9]+[ \"']> *\\)")))
  )
