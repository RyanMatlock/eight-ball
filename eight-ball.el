;; eight-ball.el
;;
;; Implements a Magic 8-Ball function.

(defgroup eight-ball-custom
  nil
  "Customization group for EIGHT-BALL (Magic 8-Ball) function")

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
  :type '(repeat string)
  :group 'eight-ball-custom)

(defcustom eight-ball-kill-ring-include-timestamp t
  "Include timestamp when adding EIGHT-BALL response to KILL-RING?
Default: t."
  :type 'boolean
  :options '(choice (const :tag "Include timestamp" t)
                    (const :tag "Omit timestamp" nil))
  :group 'eight-ball-custom)

(defcustom eight-ball-timestamp-format
  "[%F %H:%M:%S]"
  "Timestamp format for EIGHT-BALL response in KILL-RING.
Default: \"[%F %H:%M:%S]\" i.e. \"[<ISO-8601 date>⎵<hour(24)>:<min>:<sec>]\"
See documentation for FORMAT-TIME-STRING for valid values."
  :type 'string
  :group 'eight-ball-custom)

;; (message "%s" (format-time-string eight-ball-timestamp-format))  ;; works

;; hash question, mod 1000, wait for that many milliseconds (so that question
;; content affects time, which affects random seed)

(defcustom eight-ball-print-question-with-response t
  "Include question with response in message?
Default: t."
  :type 'boolean
  :options '(choice (const :tag "Include question when printing response" t)
                    (const :tag "Print response only" nil))
  :group 'eight-ball-custom)

(defun eight-ball (&optional add-to-kill-ring question)
  "Prompts user to ask a question, responds like a Magic 8-Ball.
Passing the \\[[universal-argument]]..."
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






