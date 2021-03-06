[![MELPA Stable](https://stable.melpa.org/packages/quiz-badge.svg)](https://stable.melpa.org/#/quiz)
[![MELPA](https://melpa.org/packages/quiz-badge.svg)](https://melpa.org/#/quiz)

# quiz.el

A simple trivia quiz game that uses [The Open Trivia
Database](https://opentdb.com/) as the back end.

## Commentary:

`quiz.el` implements a simple multiple-choice trivia quiz, using [The Open
Trivia Database](https://opentdb.com/) as the back end.

For the moment it provides a single command: `quiz`. When run you will be
prompted for how many questions you'd like (between 1 and 50) and then a
buffer of questions with multiple-choice answers will be displayed. Answer
the questions by checking your choice of answer and hit <kbd>Space</kbd> at
any time to check your progress.

![M-x quiz RET](quiz.png)

## Todo:

- [X] Implement categories.
- [X] Implement difficulty levels.
- [ ] Implement session IDs to have fewer repeat questions.
- [ ] Implement question type selection.
