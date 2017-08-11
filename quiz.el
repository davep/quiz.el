;;; quiz.el --- Multiple choice quiz game -*- lexical-binding: t -*-
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 0.01
;; Keywords: games, trivia, quiz
;; URL: https://github.com/davep/quiz.el
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; quiz.el implements a simple multiple-choice trivia quiz, using
;; https://opentdb.com/ as the back end.

;;; Code:

(require 'cl-lib)
(require 'url-vars)
(require 'json)
(require 'widget)
(require 'wid-edit)

(defgroup quiz nil
  "Trivia quiz game using Open Trivia DB as the back end."
  :group 'games)

(defface quiz-question-number-face
  '((t :height 1.2
       :background "black"
       :foreground "white"))
  "Face for the question number."
  :group 'quiz)

(defface quiz-question-face
  '((t :weight bold))
  "Face for the question."
  :group 'quiz)

(defconst quiz-source-url "https://opentdb.com/api.php?amount=%d&&encode=base64"
  "URL for loading up questions from the Open Trivia DB.")

(defconst quiz-user-agent "quiz.el"
  "User agent to send when requesting number information.")

(defconst quiz-buffer-name "*Quiz*"
  "Name of the quiz buffer.")

(defun quiz-lispify-questions (json-questions)
  "Turn JSON-QUESTIONS into a list."
  (alist-get 'results (json-read-from-string json-questions)))

(defun quiz-get-questions (&optional count)
  "Load COUNT questions from the trivia server.

Ten questions are loaded if COUNT isn't supplied."
  (let* ((url-request-extra-headers `(("User-Agent" . ,quiz-user-agent)))
         (buffer (url-retrieve-synchronously (format quiz-source-url (or count 10)))))
    (when buffer
      (with-current-buffer buffer
        (set-buffer-multibyte t)
        (setf (point) (point-min))
        (when (search-forward-regexp "^$" nil t)
          (quiz-lispify-questions (buffer-substring (1+ (point)) (point-max))))))))

(defun quiz-decode (s)
  "Decode S."
  (decode-coding-string (base64-decode-string s) 'utf-8))

(defun quiz-insert-question-text (questions i)
  "From QUESTIONS insert the text of question I."
  (insert
   (propertize
    (quiz-decode (alist-get 'question (aref questions i)))
    'font-lock-face 'quiz-question-face)))

(defun quiz-insert-answers (questions i)
  "From QUESTIONS insert the answers for question I."
  (let ((correct (quiz-decode (alist-get 'correct_answer (aref questions i)))))
    (apply #'widget-create
           'radio-button-choice
           :notify (lambda (widget &rest _)
                     (setf (alist-get 'given_answer (aref questions i))
                           (base64-encode-string (widget-value widget))))
           (cl-loop for answer in
                    (sort (append (list correct)
                                  (cl-loop for wrong across (alist-get 'incorrect_answers (aref questions i))
                                           collect (quiz-decode wrong))) #'string<)
                    collect (list 'item answer)))))

(defun quiz-insert-question (questions i)
  "From QUESTIONS insert QUESTION I."
  (insert
   (propertize (format "Question %s:\n" (1+ i)) 'font-lock-face 'quiz-question-number-face)
   "\n")
  (quiz-insert-question-text questions i)
  (insert "\n")
  (quiz-insert-answers questions i)
  (insert "\n"))

(defun quiz-insert-questions (count)
  "Get and insert COUNT questions into the current buffer."
  (let ((questions (quiz-get-questions count)))
    (if questions
        (cl-loop for i from 0 to (1- (length questions))
                 do (quiz-insert-question questions i))
      (insert "Sorry. Unable to load up any questions right now."))
    questions))

(defun quiz-insert-finish ()
  "Insert the finish button for the QUESTIONS."
  (insert-text-button
   "Check answers"
   'action (lambda (_) (quiz-check-answers))
   :follow-link t))


(defun quiz-check-answers ()
  "Show the results of the quiz."
  (interactive)
  (message "%d out of %d correct answers."
           (cl-loop for q across quiz-questions
                    sum (if (string=
                             (alist-get 'correct_answer q)
                             (alist-get 'given_answer q ""))
                            1
                          0))
           (length quiz-questions)))

(defun quiz-quit ()
  "Quit the current quiz."
  (interactive)
  (kill-buffer))

(defvar quiz-mode-map nil
  "Local keymap for `quiz'.")

(unless quiz-mode-map
  (let ((map widget-keymap))
    (suppress-keymap map t)
    (define-key map " "   #'quiz-check-answers)
    (define-key map "q"   #'quiz-quit)
    (define-key map "?"   #'describe-mode)
    (setq quiz-mode-map map)))

(put 'quiz-mode 'mode-class 'special)

(defun quiz-mode ()
  "Major mode for playing `quiz'.

The key bindings for `quiz-mode' are:

\\{quiz-mode-map}"
  (kill-all-local-variables)
  (use-local-map quiz-mode-map)
  (setq major-mode       'quiz-mode
        mode-name        "Quiz mode"
        buffer-read-only t
        truncate-lines   nil)
  (buffer-disable-undo))

(defvar-local quiz-questions nil
  "Holds the questions for the current quiz.")

;;;###autoload
(defun quiz (count)
  "Play a multiple choice trivia quiz with COUNT questions."
  (interactive (list (read-number "Questions: " 10)))
  (if (< 51 count 0)
      (error "Between 1 and 50 questions would seem sensible")
    (when (get-buffer quiz-buffer-name)
      (kill-buffer quiz-buffer-name))
    (let ((buffer (get-buffer-create quiz-buffer-name)))
      (with-current-buffer buffer
        (quiz-mode)
        (let ((buffer-read-only nil))
          (setf (buffer-string) "")
          (save-excursion
            (setq quiz-questions (quiz-insert-questions count))
            (quiz-insert-finish))
          (widget-forward 1))
        (switch-to-buffer buffer)))))

(provide 'quiz)

;;; quiz.el ends here
