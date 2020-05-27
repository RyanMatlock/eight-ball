;; eight-ball.el
;;
;; Implements a Magic 8-Ball function.

(defcustom eight-ball-reponses
  '("It is certain."
    "It is decidedly so!"
    "Without a doubt!"
    "Yes, definitely!"
    "You may rely on it."
    "As I see it, yes."
    "Most likely."
    "Outlook good."
    "Yes."
    "Signs point to yes."
    "Reply hazy try again."
    "Ask again later."
    "Better not tell you now."
    "Cannot predict now."
    "Concentrate and ask again."
    "Don't count on it."
    "My reply is no."
    "My sources say no."
    "Outlook not so good."
    "Very doubtful.")
  "Response possibilities for EIGHT-BALL.
Default values are standard Magic 8-ball responses."
  :type 'list
  :options nil
  :group nil)

(defcustom eight-ball-kill-ring-include-timestamp t
  "Include timestamp when adding EIGHT-BALL response to KILL-RING?
Default: t."
  :type 'boolean
  :options nil
  :group nil)

(defcustom eight-ball-timestamp-format
  "[%F %H:%M:%S]"
  "Timestamp format for EIGHT-BALL response in KILL-RING.
Default: \"[%F %H:%M:%S]\" (\"[ISO-8601 date⎵hour:min:sec]\")
See documentation for FORMAT-TIME-STRING for valid values."
  :type 'string
  :options nil
  :group nil)

(message "%s" (format-time-string eight-ball-timestamp-format))  ;; works

;; hash question, mod 1000, wait for that many milliseconds (so that question
;; content affects time, which affects random seed)

(defcustom eight-ball-print-question-with-response t
  "Include question with response in message?
Default: t."
  :type 'boolean
  :options nil
  :group nil)

(defun eight-ball (&optional add-to-kill-ring question)
  "
Passing the \\[[universal-argument]]"
  (interactive "p\nsAsk the Magic 8-Ball a question: ")
  ;; (message "add-to-kill-ring: %s, question: %s" add-to-kill-ring question)
  (random t)
  ;; stolen from
  ;; https://www.rosettacode.org/wiki/Pick_random_element#Emacs_Lisp
  (let* ((size (length eight-ball-reponses))
         (index (random size))
         (response (nth index eight-ball-reponses)))
    (when add-to-kill-ring
      (let (formatted-question-response)
        ))
    (message "%s" response)))






