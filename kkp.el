;;; kkp.el --- Enable emacs support for the Kitty Keyboard Protocol (one flavor of the CSI u encoding)  -*- lexical-binding: t -*-

;; Copyright (C) 2022  Benjamin Orthen

;; Author: Benjamin Orthen <contact@orthen.net>
;; Maintainer: Benjamin Orthen <contact@orthen.net>
;; Keywords: terminal
;; Version: 0.1
;; Homepage: https://github.com/benjaminor/kkp
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; The Kitty Keyboard Protocol (KKP) is documented here: https://sw.kovidgoyal.net/kitty/keyboard-protocol

;; kitty modifier encoding
;; shift     0b1         (1)
;; alt       0b10        (2)
;; ctrl      0b100       (4)
;; super     0b1000      (8)
;; hyper     0b10000     (16)
;; meta      0b100000    (32)
;; caps_lock 0b1000000   (64)
;; num_lock  0b10000000  (128)

;; Possible format of escape sequences sent to Emacs.
;; - CSI keycode u
;; - CSI keycode; modifier u
;; - CSI number ; modifier ~
;; - CSI {ABCDEFHPQRS}
;; - CSI 1; modifier {ABCDEFHPQRS}

;;; Code:
(eval-when-compile (require 'cl-lib))

(defvar kkp--printable-ascii-letters
  (cl-loop for c from ?a to ?z collect c))

;; when not found in emacs source code, taken from http://xahlee.info/linux/linux_show_keycode_keysym.html
(defvar kkp--non-printable-keys-with-u-terminator
  '((27 . "<escape>")
    (13 . "<return>")
    (?\s . "SPC")
    (9 . "<tab>")
    (127 . "<backspace>")
    (57358 . "<Caps_Lock>")
    (57359 . "<Scroll_Lock>")
    (57360 . "<kp-numlock>")
    (57361 . "<print>")
    (57362 . "<pause>")
    (57363 . "<menu>")
    (57376 . "<f13>")
    (57377 . "<f14>")
    (57378 . "<f15>")
    (57379 . "<f16>")
    (57380 . "<f17>")
    (57381 . "<f18>")
    (57382 . "<f19>")
    (57383 . "<f20>")
    (57384 . "<f21>")
    (57385 . "<f22>")
    (57386 . "<f23>")
    (57387 . "<f24>")
    (57388 . "<f25>")
    (57389 . "<f26>")
    (57390 . "<f27>")
    (57391 . "<f28>")
    (57392 . "<f29>")
    (57393 . "<f30>")
    (57394 . "<f31>")
    (57395 . "<f32>")
    (57396 . "<f33>")
    (57397 . "<f34>")
    (57398 . "<f35>")
    (57399 . "<kp-0>")
    (57400 . "<kp-1>")
    (57401 . "<kp-2>")
    (57402 . "<kp-3>")
    (57403 . "<kp-4>")
    (57404 . "<kp-5>")
    (57405 . "<kp-6>")
    (57406 . "<kp-7>")
    (57407 . "<kp-8>")
    (57408 . "<kp-9>")
    (57409 . "<kp-decimal>")
    (57410 . "<kp-divide>")
    (57411 . "<kp-multiply>")
    (57412 . "<kp-subtract>")
    (57413 . "<kp-add>")
    (57414 . "<kp-enter>")
    (57415 . "<kp-equal>")
    (57416 . "<kp-separator>")
    (57417 . "<kp-left>")
    (57418 . "<kp-right>")
    (57419 . "<kp-up>")
    (57420 . "<kp-down>")
    (57421 . "<kp-prior>") ;; KP_PAGE_UP
    (57422 . "<kp-next>") ;; KP_PAGE_DOWN
    (57423 . "<kp-home>")
    (57424 . "<kp-end>")
    (57425 . "<kp-insert>")
    (57426 . "<kp-delete>")
    (57428 . "<media-play>")
    (57429 . "<media-pause>")
    (57430 . "<media-play-pause>")
    (57431 . "<media-reverse>")
    (57432 . "<media-stop>")
    (57433 . "<media-fast-forward>")
    (57434 . "<media-rewind>")
    (57435 . "<media-next>") ;; MEDIA_TRACK_NEXT
    (57436 . "<media-previous>") ;; MEDIA_TRACK_PREVIOUS
    (57437 . "<media-record>")
    (57438 . "<volume-down>")
    (57439 . "<volume-up>")
    (57440 . "<volume-mute>")

    ;; it is rather unlikely Emacs gets this keysequence directly from the terminal
    ;; but just for the case...
    (57441 . "<SHIFT_L>")
    (57442 . "<Control_L>")
    (57443 . "<Alt_L>")
    (57444 . "<Super_L>")
    (57445 . "<Hyper_L>")
    (57446 . "<Meta_L>")
    (57447 . "<Shift_R>")
    (57448 . "<Control_R>")
    (57449 . "<Alt_R>")
    (57450 . "<Super_R>")
    (57451 . "<Hyper_R>")
    (57452 . "<Meta_R>")
    (57453 . "<ISO_Level3_Shift>")
    (57454 . "<ISO_Level5_Shift>")))

(defvar kkp--non-printable-keys-with-tilde-terminator
  '((2 . "<insert>")
    (3 . "<delete>")
    (5 . "<prior>")
    (6 . "<next>")
    (7 . "<home>")
    (8 . "<end>")
    (11 . "<f1>")
    (12 . "<f2>")
    (13 . "<f3>")
    (14 . "<f4>")
    (15 . "<f5>")
    (17 . "<f6>")
    (18 . "<f7>")
    (19 . "<f8>")
    (20 . "<f9>")
    (21 . "<f10>")
    (23 . "<f11>")
    (24 . "<f12>")
    (57427 . "<kp-begin>")))

(defvar kkp--keycode-mapping
  (cl-concatenate 'list kkp--non-printable-keys-with-u-terminator kkp--non-printable-keys-with-tilde-terminator))

(defvar kkp--non-printable-keys-with-letter-terminator
  '((?D . "<left>")
    (?C . "<right>")
    (?A . "<up>")
    (?B . "<down>")
    (?H . "<home>")
    (?F . "<end>")
    (?P . "<f1>")
    (?Q . "<f2>")
    (?R . "<f3>")
    (?S . "<f4>")
    (?E . "<kp-begin>")))

(defun kkp--csi-escape (&rest args)
  "Prepend the CSI bytes before the ARGS."
  (concat "\e[" (apply 'concat args)))

(defun kkp--cl-split (separator seq)
  "Split SEQ along SEPARATOR and return all subsequences."
  (let ((pos 0)
        (last-pos 0)
        (len (length seq))
        (subseqs nil))
    (while (< pos len)
      (setq pos (cl-position separator seq :start last-pos))
      (unless pos
        (setq pos len))
      (let ((subseq (cl-subseq seq last-pos pos)))
        (push subseq subseqs))
      (setq last-pos (1+ pos)))
    (nreverse subseqs)))

(defun kkp--ascii-chars-to-number (seq)
  (string-to-number (apply #'string seq)))

(defun kkp--is-bit-set (num bit)
  (not (eql (logand num bit) 0)))

(defun kkp--create-modifiers-string (modifier-num)

  ;; add modifiers as defined in key-valid-p
  ;; Modifiers have to be specified in this order:
  ;;    A-C-H-M-S-s which is
  ;;    Alt-Control-Hyper-Meta-Shift-super

  (let ((key-str ""))
    (when (kkp--is-bit-set modifier-num 4) ;; Ctrl
      (setq key-str (concat key-str "C-")))
    (when (kkp--is-bit-set modifier-num 16) ;; Hyper
      (setq key-str (concat key-str "H-")))
    (when
        (or
         (kkp--is-bit-set modifier-num 2) ;; Alt = Meta
         (kkp--is-bit-set modifier-num 32)) ;; Meta
      (setq key-str (concat key-str "M-")))
    (when (kkp--is-bit-set modifier-num 1) ;; shift
      (setq key-str (concat key-str "S-")))
    (when (kkp--is-bit-set modifier-num 8) ;; super
      (setq key-str (concat key-str "s-")))
    key-str))


(defun kkp--get-keycode-representation (keycode)
  (let ((rep (alist-get keycode kkp--keycode-mapping)))
    (if rep
        rep
      (string keycode))))

(defvar kkp--acceptable-sequence-bytes
  (cl-concatenate 'list '(?\; ?:) (cl-loop for c from ?0 to ?9 collect c)))

(defvar kkp--acceptable-terminators
  '(?u ?~ ?A ?B ?C ?D ?E ?F ?H ?P ?Q ?R ?S))

(defun kkp--process-keys (first-num)

  ;; read input from terminal
  ;; extract keycode, shifted-key, modifiers, text-as-codepoints
  ;; generate and return keybinding string

  ;; check if keycode has shifted key ->
  ;; set keychar to shifted key & remove Shift from modifiers

  (let ((terminal-input `(,first-num)))

    ;; read in events from stdin until we have found a terminator
    (while (not (member (cl-first terminal-input) kkp--acceptable-terminators))
      (let ((evt (read-event)))
        (push evt terminal-input)))

    ;; reverse input has we pushed to events to the front of the list
    (setq terminal-input (nreverse terminal-input))

    ;; extract keycode, shifted-key, modifiers, text-as-codepoints
    ;; three different terminator types: u, ~, and letters
    ;; u und tilde have same structure:
    ;; keycode[:[shifted-key][:base-layout-key]];[modifiers[:event-type]][;text-as-codepoints]{u~}
    ;; letter terminated have a different structure
    ;; [1;modifier[:event-type]]{letter}
    (let
        ((terminator (car (last terminal-input))))
      (if
          (member terminator '(?u ?~))
          (progn
            (let* ((splitted-terminal-input (kkp--cl-split ?\; (remq terminator terminal-input)))
                   (splitted-keycodes (kkp--cl-split ?: (cl-first splitted-terminal-input))) ;; get keycodes from sequence
                   (splitted-modifiers-events (kkp--cl-split ?: (cl-second splitted-terminal-input))) ;; list of modifiers and event types
                   ;; (text-as-codepoints (cl-third splitted-terminal-input))
                   (primary-key-code (cl-first splitted-keycodes))
                   (shifted-key-code (cl-second splitted-keycodes))
                   (modifiers (cl-first splitted-modifiers-events)) ;; get modifiers
                   (key-code nil)
                   (modifier-num
                    (if modifiers
                        (1-
                         (kkp--ascii-chars-to-number modifiers))
                      0)))

              (if
                  (and
                   shifted-key-code
                   (not (member (kkp--ascii-chars-to-number primary-key-code) kkp--printable-ascii-letters)))
                  (progn
                    (setq key-code shifted-key-code)
                    (setq modifier-num (logand modifier-num (lognot 1))))
                (setq key-code primary-key-code))

              (let
                  ((modifier-str (kkp--create-modifiers-string modifier-num))
                   (key-name (kkp--get-keycode-representation (kkp--ascii-chars-to-number key-code))))
                (kbd (concat modifier-str key-name)))))

        ;; terminal input has this form [1;modifier[:event-type]]{letter}
        (progn
          (let* ((splitted-terminal-input (kkp--cl-split ?\; (remq terminator terminal-input)))
                 (splitted-modifiers-events (kkp--cl-split ?: (cl-second splitted-terminal-input))) ;; list of modifiers and event types
                 (modifiers (cl-first splitted-modifiers-events)) ;; get modifiers
                 (modifier-num
                  (if modifiers
                      (1-
                       (kkp--ascii-chars-to-number modifiers))
                    0)))
            (let
                ((modifier-str (kkp--create-modifiers-string modifier-num))
                 (key-name (alist-get terminator kkp--non-printable-keys-with-letter-terminator)))
              (kbd (concat modifier-str key-name)))))))))


(defvar kkp--key-prefixes
  '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?A ?B ?C ?D ?E ?F ?H ?P ?Q ?R ?S))

(defvar kkp-terminal-query-timeout 0.1
  "Seconds to wait for an answer from the terminal. Nil means no timeout.")

(defvar kkp--progressive-enhancement-flags
  '((disambiguate-escape-codes . (:bit 1))
    (report-alternate-keys . (:bit 4))))

(defvar kkp-active-enhancements
  '(disambiguate-escape-codes report-alternate-keys)
  "List of enhancements which should be enabled.
Possible values are the keys in `kkp--progressive-enhancement-flags`.")

(defun kkp--get-enhancement-bit (enhancement)
  "Get the bitflag which enables the ENHANCEMENT."
  (plist-get (cdr enhancement) :bit))


(defun kkp--query-terminal-sync (query &optional terminal)
  "Send QUERY to TERMINAL (to current if nil) and return response (if any)."
  (discard-input)
  (send-string-to-terminal (kkp--csi-escape query) terminal)
  (let ((cond t)
        (terminal-input nil))
    (while cond
      (let ((evt (read-event nil nil kkp-terminal-query-timeout)))
        (if (null evt)
            (setq cond nil)
          (push evt terminal-input))))
    (nreverse terminal-input)))

(defun kkp--query-terminal-async (query handlers)
  "Send QUERY string to the terminal and watch for a response.
HANDLERS is an alist with elements of the form (STRING . FUNCTION).
We run the first FUNCTION whose STRING matches the input events.
This function code is copied from `xterm--query`."
  (let ((register
         (lambda (handlers)
           (dolist (handler handlers)
             (define-key input-decode-map (car handler)
                         (lambda (&optional _prompt)
                           ;; Unregister the handler, since we don't expect
                           ;; further answers.
                           (dolist (handler handlers)
                             (define-key input-decode-map (car handler) nil))
                           (funcall (cdr handler))
                           []))))))

    (funcall register handlers)
    (send-string-to-terminal (kkp--csi-escape query))))


(defun kkp--enabled-terminal-enhancements ()
  "Query the terminal and return list of currently enabled enhancements."
  (let ((reply (kkp--query-terminal-sync "?u")))
    (when (not reply)
      (error "Terminal did not reply correctly to query"))

    (let ((intflag (- (nth 3 reply) ?0))
          (enabled-enhancements nil))

      (dolist (bind kkp--progressive-enhancement-flags)
        (when (> (logand intflag (kkp--get-enhancement-bit bind)) 0)
          (push (car bind) enabled-enhancements)))
      enabled-enhancements)))


(defun kkp--check-terminal-supports-kkp ()
  "Check if the terminal supports the Kitty Keyboard Protocol."
  (let ((reply (kkp--query-terminal-sync "?u")))
    (and
     (eql (length reply) 5) ;; TODO response can be 6 bytes long
     (equal '(27 91 63) (cl-subseq reply 0 3))
     (eql 117 (nth 4 reply)))))



(defun kkp--calculate-flags-integer ()
  "Calculate the flag integer to send to the terminal to activate the enhancemets."
  (cl-reduce #'(lambda (sum elt) (+ sum
                               (kkp--get-enhancement-bit (assoc elt kkp--progressive-enhancement-flags))))
             kkp-active-enhancements :initial-value 0))


(defvar kkp--terminals-with-active-kkp
  '())

(defun kkp--pop-terminal-flag (&optional terminal)
  "Send query to TERMINAL (if nil, the current one) to pop previously pushed flags."
  (unless (or
           (display-graphic-p)
           (member terminal kkp--terminals-with-active-kkp))
    (delete terminal kkp--terminals-with-active-kkp)
    (kkp--query-terminal-sync "<u") terminal))

(defun kkp-check-progressive-enhancement-flags ()
  "Message, if terminal supports kkp, currently enabled enhancements."
  (interactive)
  (if (kkp--check-terminal-supports-kkp)
      (message "%s" (kkp--enabled-terminal-enhancements))
    (message "KKP not supported in this terminal.")))

(defun kkp-terminal-disable ()
  "Disable in this terminal where command is executed, the activated enhancements."
  (interactive)
  (dolist (prefix kkp--key-prefixes)
    (define-key input-decode-map (kkp--csi-escape (string prefix)) nil t))
  (kkp--query-terminal-sync "=0;1u"))

(defun kkp-enable ()
  "Enable support for the Kitty Keyboard Protocol.
This activates, if a terminal supports it, the protocol enhancements
specified in `kkp-active-enhancements`."

  (push 'kkp--pop-terminal-flag delete-frame-functions)
  ;; call setup for future terminals to be opened
  (add-hook 'tty-setup-hook 'kkp-terminal-setup)

  ;; call setup for this terminal (if it is a terminal, else this does nothing)
  (kkp-terminal-setup))


(defun kkp--terminal-setup ()
  "Run setup to enable KKP support."
  (let ((a (read-event))
        (b (read-event)))
    (when (and (<= ?0 a ?9) ;; TODO: flag can be two bytes
               (eql b ?u))

      (message "Enabled KKP support in this terminal.")
      (let ((intflag (kkp--calculate-flags-integer)))
        (unless (eq intflag 0)
          (kkp--query-terminal-sync (format ">%su" intflag))

          ;; we register functions for each prefix to not interfere with e.g., M-[ I
          (dolist (prefix kkp--key-prefixes)
            (define-key input-decode-map (kkp--csi-escape (string prefix)) (lambda (_prompt) (kkp--process-keys prefix))))

          (let ((frame (selected-frame)))
            (add-hook 'kill-emacs-hook `(lambda () (kkp--pop-terminal-flag ',frame)))))))))

;; this function can be called more than one time, as each time we call it, we also add an pop-flag function to the exit hooks
(defun kkp-terminal-setup ()
  "Enable KKP support in Emacs running in the terminal."
  (unless
      (or
       (display-graphic-p)
       (member (selected-frame) kkp--terminals-with-active-kkp))
    (kkp--query-terminal-async "?u"
                               '(("\e[?" . kkp--terminal-setup)))))

(provide 'kkp)
;;; kkp.el ends here
