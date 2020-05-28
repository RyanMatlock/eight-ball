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
  :options '(choice (const :tag "Yes" t)
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
;; (string-to-number (secure-hash 'sha256 "foo") 16)  ;; hmm, overflow
;; (string-to-number "ff" 16)  ;; -> 255 (and stuff) (works)
;; (string-to-number "100" 16)  ;; -> 256 (and stuff) (works)
;; (message "%d" (string-to-number "100" 16))  ;; -> 256 (works)

;; better than sleeping: add the last few digits of the hash to the random
;; number, then take the modulo of the length of the responses
;; or
;; take the last several digits of the hash, round down to a number divisible by
;; the length of the available choices, take a random number in that range, and
;; then take the modulo of that

;; (let* ((hash (secure-hash 'sha256 "foo"))
;;        (size (length hash))
;;        (small-hash (substring hash (- size 5) nil))
;;        (decimal-hash (string-to-number small-hash 16))
;;        (mod-hash (% decimal-hash 20)))
;;   (message (concat "hash: %s\n"
;;                    "small-hash: %s\n"
;;                    "decimal-hash: %d\n"
;;                    "mod-hash: %d")
;;            hash
;;            small-hash
;;            decimal-hash
;;            mod-hash))

(defcustom eight-ball-print-question-with-response t
  "Include question with response in message?
Default: t."
  :type 'boolean
  :options '(choice (const :tag "Yes" t)
                    (const :tag "Print response only" nil))
  :group 'eight-ball-custom)

(defun eight-ball (&optional add-to-kill-ring question)
  "Prompts user to ask a question, responds like a Magic 8-Ball.
Passing the \\[[universal-argument]]..."
  (interactive "P\nsAsk the Magic 8-Ball a question: ")
  ;; (message "add-to-kill-ring: %s, question: %s" add-to-kill-ring question)
  (random t)
  (let* ((hash (secure-hash 'sha256 question))
         (small-hash (substring hash (- (length hash) 6) nil))
         (decimal-hash (string-to-number small-hash 16))
         (size (length eight-ball-reponses))
         ;; subtracting inner mod from decimal-hash ensures that values will be
         ;; evenly distributed across eight-ball-responses (i.e. that value
         ;; be a multiple of size)
         ;; outer mod ensures it's a valid index for eight-ball-responses
         ;;
         ;; this appears to generate all possibilities; will have to histogram
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


