(defparameter *num-cpu* (lisp-implementation-type))

(defun count-numbers (start end)
  (let ((count 0))
    (loop for i from start to end do
      (incf count))
    count))

(defun spawn-counter (start end)
  (let ((parent (current-process))
        (pid (make-process :function (lambda ()
                                      (send parent (list :result (count-numbers start end))))
                           :name "Counter")))
    (list pid start end)))

(defun collect-results (n acc)
  (if (= n 0)
      (reverse acc)
      (let ((result (receive (:result count) (values count))))
        (collect-results (- n 1) (cons result acc)))))

(defun main ()
  (let* ((num-cpu *num-cpu*)
         (per-chunk (floor (/ 1000000000 num-cpu)))
         (remainder (mod 1000000000 num-cpu))
         (start-time (get-internal-real-time)))
    (setq mp:*yield-time* 0) ; Reduce process yield time for better performance
    (setq mp:*process-run-function* #'mp:process-run-function-with-time-slice)
    (setq mp:*default-process-time-slice* 0.01) ; Set time slice for processes
    (setq mp:*default-thread-time-slice* 0.01) ; Set time slice for threads
    
    (let ((threads (loop for i from 0 below num-cpu collecting
                         (apply #'spawn-counter (list (+ (* i per-chunk) 1) (+ (* (1+ i) per-chunk)
                                                                                (if (= i (1- num-cpu))
                                                                                    remainder
                                                                                    0)))))))
      (let ((results (collect-results num-cpu nil)))
        (let ((total-count (loop for (pid start end) in results summing (nth 2 (receive (:result count) (values count))))))
          (let ((end-time (get-internal-real-time))
                (execution-time (- (get-internal-real-time) start-time)))
            (format t "Count: ~d~%" total-count)
            (format t "Execution Time: ~d microseconds~%" execution-time)))))))
