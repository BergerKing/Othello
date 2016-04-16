(defparameter *WCanMove* 0)
(defparameter *BCanMove* 0)
(defparameter *MovesMade* 4)
(loop while (and (< *MovesMade* 63) (or (equal *WCanMove* 0) (equal *BCanMove* 0) ) )  do
		(setf *WCanMove* 1)
		(setf *BCanMove* 1)
		(incf *MovesMade*)
		(format t "success!!")
)
