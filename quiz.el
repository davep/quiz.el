;;; quiz.el --- Multiple choice quiz game -*- lexical-binding: t -*-
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 0.01
;; Keywords: games, trivia, quiz
;; URL: https://github.com/davep/quiz.el
;; Package-Requires: ((emacs "24"))

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

(require 'url-vars)
(require 'json)
(require 'xml)

(defgroup quiz nil
  "Trivia quiz game using Open Trivia DB as the back end."
  :group 'games)

(defface quiz-question-number-face
  '((t :weight bold))
  "Face for the question number."
  :group 'quiz)

(defface quiz-question-face
  '((t :weight bold))
  "Face for the question."
  :group 'quiz)

(defface quiz-answer-face
  '((t :weight bold))
  "Face for an answer."
  :group 'quiz)

(defconst quiz-source-url "https://opentdb.com/api.php?amount=%d"
  "URL for loading up questions from the Open Trivia DB.")

(defconst quiz-user-agent "quiz.el"
  "User agent to send when requesting number information.")

(defun quiz-lispify-questions (json-questions)
  "Turn JSON-QUESTIONS into a list."
  (cdr (assoc 'results (json-read-from-string json-questions))))

(defun quiz-unhtml (s)
  "Un-HTML S."
  (with-temp-buffer
    (insert s)
    (setf (point) (point-min))
    (xml-parse-string)))

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

(defun quiz--question (q)
  "Return the question text for question Q."
  (propertize (quiz-unhtml (cdr (assoc 'question q))) 'font-lock-face 'quiz-question-face))

(defun quiz--answers-multiple (q)
  "Return the answers for Q formatted as a multiple choice question."
  (cl-loop for answer in
           (sort (append (list (quiz-unhtml (cdr (assoc 'correct_answer q))))
                         (cl-loop for wrong across (cdr (assoc 'incorrect_answers q))
                                  collect (quiz-unhtml wrong))) #'string<)
           concat "\t"
           concat (propertize answer 'font-lock-face 'quiz-answer-face)
           concat "\n"))

(defun quiz--answers-boolean (q)
  "Return the answers for Q formatted as a true/false question."
  (concat
   "\t"
   (propertize "True" 'font-lock-face 'quiz-answer-face)
   "\n\t"
   (propertize "False" 'font-lock-face 'quiz-answer-face)
   "\n"))

(defun quiz--answers (q)
  "Return the formatted answers for question Q."
  (let ((type (cdr (assoc 'type q))))
    (when type
      (cl-case (intern (concat ":" type))
        (:multiple
         (quiz--answers-multiple q))
        (:boolean
         (quiz--answers-boolean q))))))

(defun quiz-insert-questions (count)
  "Get and insert COUNT questions into the current buffer."
  (let ((questions (quiz-get-questions count)))
    (if questions
        (cl-loop for i from 1 to (length questions)
                 and q across questions
                 do (insert
                     (propertize (format "Question %s:\n" i) 'font-lock-face 'quiz-question-number-face)
                     (quiz--question q)
                     "\n"
                     (quiz--answers q)
                     "\n"))
      (insert "Sorry. Unable to load up any questions right now."))))

;;;###autoload
(defun quiz (count)
  "Play a multiple choice trivia quiz with COUNT questions."
  (interactive (list (read-number "Questions: " 10)))
  (if (> 51 count 0)
      (let ((buffer (get-buffer-create "*Quiz*")))
        (with-current-buffer buffer
          (setf (buffer-string) "")
          (font-lock-mode)
          (quiz-insert-questions count))
        (switch-to-buffer buffer))
    (error "Between 1 and 50 questions would seem sensible")))

(provide 'quiz)

;;; quiz.el ends here
