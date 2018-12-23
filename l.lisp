;(in-package "COM.INFORMATIMAGO.HANGMAN.COCOA")

(defstruct (hangman
            (:constructor %make-hangman))
  word ; the word to guess, should only contain letters in the alphabet
  found ; vector of boolean one for each letter in word
  tried-letters ; vector of letters tried so far
  (error-count 0)
  maximum-error-count
  (missing-letter ".")
  alphabet)

(defun make-hangman (word maximum-error-count &key (alphabet "abcdefghijklmnopqrstuvwxyz"))
  (let ((found (make-array (length word) :element-type 'boolean :initial-element nil)))
    (setf (aref found 0) t
          (aref found (1- (length found))) t)
    (%make-hangman :word word
                   :found found
                   :alphabet alphabet
                   :missing-letter "."
                   :tried-letters (make-array (length alphabet) :element-type 'character :fill-pointer 0)
                   :maximum-error-count maximum-error-count)))

(defun hangman-found-word (hangman)
  (loop :with found-word = (copy-seq (hangman-word hangman))
        :with found = (hangman-found hangman)
        :for i :below (length found-word)
        :unless (aref found i)
          :do (setf (aref found-word i) (character (hangman-missing-letter hangman)))
        :finally (return found-word)))

(defun hangman-try-letter (hangman letter)
  (let ((tried (position letter (hangman-tried-letters hangman) :test (function char-equal))))
    (when tried
      (return-from hangman-try-letter
        (if (< (hangman-error-count hangman) (hangman-maximum-error-count hangman))
            :already-tried
            :loses))))
  (vector-push letter (hangman-tried-letters hangman))
  (loop
    :with word  = (hangman-word hangman)
    :with found = (hangman-found hangman)
    :with error = t
    :for i :below (length word)
    :when (char-equal letter (aref word i))
      :do (setf (aref found i) t
                error nil)
    :finally (when (and error (< (hangman-error-count hangman) (hangman-maximum-error-count hangman)))
               (incf (hangman-error-count hangman)))
             (return (cond
                       ((not (< (hangman-error-count hangman) (hangman-maximum-error-count hangman)))
                        :loses)
                       ((not (position nil found))
                        :wins)
                       (error
                        :bad-guess)
                       (t
                        :good-guess)))))


(defun test/hangman ()
  (let ((game (make-hangman "Hello" 11)))
    (assert (string= "Hello" (hangman-word game)))
    (assert (string= "H...o" (hangman-found-word game)))
    (assert (eq :good-guess (hangman-try-letter game #\l)))
    (assert (= 0 (hangman-error-count game)))
    (assert (eq :already-tried (hangman-try-letter game #\l)))
    (assert (= 1 (hangman-error-count game)))
    (assert (eq :bad-guess (hangman-try-letter game #\z)))
    (assert (= 2 (hangman-error-count game)))
    (assert (eq :wins (hangman-try-letter game #\e)))
    (assert (= 2 (hangman-error-count game))))
  (let ((game (make-hangman "Hello" 3)))
    (assert (string= "Hello" (hangman-word game)))
    (assert (string= "H...o" (hangman-found-word game)))
    (assert (eq :good-guess (hangman-try-letter game #\l)))
    (assert (= 0 (hangman-error-count game)))
    (assert (eq :already-tried (hangman-try-letter game #\l)))
    (assert (= 1 (hangman-error-count game)))
    (assert (eq :bad-guess (hangman-try-letter game #\z)))
    (assert (= 2 (hangman-error-count game)))
    (assert (eq :loses (hangman-try-letter game #\z)))
    (assert (= 3 (hangman-error-count game)))))

;;;; THE END ;;;;
