;;; eight-ball.el --- Ask the Magic 8-Ball a queustion.

;; Copyright (C) 2020 Ryan Matlock

;; Author: Ryan Matlock
;; Created: 2020-05-28
;; Version: 0.1
;; Keywords: games
;; URL: ...


;;; Commentary:

;; If silly things like 「M-x butterfly」 can exist, why can't this?

(defgroup 8ball nil
  "Ask the Magic 8-Ball a question."
  :group 'games)

(defcustom eight-ball-reponses '("It is certain."
                                 "It is decidedly so!"
                                 "Without a doubt!"
                                 "Yes, definitely!"
                                 "You may rely on it."
                                 "As I see it, yes."
                                 "Most likely."
                                 "Outlook good."
                                 "Yes."
                                 "Signs point to yes."
                                 "Reply hazy; try again."
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
  :group '8ball)

(defcustom eight-ball-kill-ring-include-timestamp t
  "Include timestamp when adding EIGHT-BALL response to KILL-RING?
Default: t."
  :type 'boolean
  :options '(choice (const :tag "Yes" t)
                    (const :tag "Omit timestamp" nil))
  :group '8ball)

(defcustom eight-ball-timestamp-format "[%F %H:%M:%S]"
  "Timestamp format for EIGHT-BALL response in KILL-RING.
Default: \"[%F %H:%M:%S]\" i.e. \"[<ISO-8601 date>⎵<hour(24)>:<min>:<sec>]\"
See documentation for FORMAT-TIME-STRING for valid values."
  :type 'string
  :group '8ball)

;;;###autoload
(defcustom eight-ball-print-question-with-response t
  "Include question with response in message?
Default: t."
  :type 'boolean
  :options '(choice (const :tag "Yes" t)
                    (const :tag "Print response only" nil))
  :group '8ball)

(defun eight-ball (&optional add-to-kill-ring question)
  "Prompts user to ask a question, responds like a Magic 8-Ball.

Passing the \\[[universal-argument]] will make ADD-TO-KILL-RING non-nil, which
will add QUESTION and response to KILL-RING, and if
EIGHT-BALL-KILL-RING-INCLUDE-TIMESTAMP is not nil, the question/response is
preceeded by timestamp formatted per EIGHT-BALL-TIMESTAMP-FORMAT.

Why would you want your Magic 8-Ball queries on the KILL-RING? In case you're
weird like me and log them for the purpose of performing analytics on the timing
and content of said queries."
  (interactive "P\nsAsk the Magic 8-Ball a question: ")
  (random t)
  (let* ((hash (secure-hash 'sha256 question))
         ;; overflow if full hash is converted to decimal, so take 6 digits
         (small-hash (substring hash (- (length hash) 6) nil))
         (decimal-hash (string-to-number small-hash 16))
         (size (length eight-ball-reponses))
         ;; subtracting inner mod from decimal-hash ensures that values will be
         ;; evenly distributed across eight-ball-responses (i.e. that value
         ;; be a multiple of size)
         ;; outer mod ensures it's a valid index for eight-ball-responses
         ;;
         ;; this appears to generate all possibilities; may have to histogram
         ;; results later to feel more confident that this is a reasonable
         ;; approach
         ;; I mean, it's unreasonable in the sense that I could just do
         ;; (rand-index (random size)), but I want to "mix in" the string
         (rand-index (% (random (- decimal-hash (% decimal-hash
                                              size)))
                   size))
         (response (nth rand-index eight-ball-reponses)))
    (when add-to-kill-ring
      (let (formatted-question-response)
        (if eight-ball-kill-ring-include-timestamp
            (setq formatted-question-response
                  (format "%s %s %s"
                          (format-time-string eight-ball-timestamp-format)
                          question
                          response))
          (setq formatted-question-response
                (format "%s %s" question response)))
        (kill-new formatted-question-response)))
    (if eight-ball-print-question-with-response
        (message "%s %s" question response)
      (message "%s" response))))

(provide 'eight-ball)
;;; eight-ball.el ends here
