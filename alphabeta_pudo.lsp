 (defun  alphabeta (node, depth, alpha, beta, maximizingPlayer)
 	(let ()
		(cond
			((deepenough) return-from alphabeta (heuristicvalue));Weiss is not gonna like this return from and heuristic value is bad magic
			
		)
		(cond
			((equal maximizingPlayer 'MAX) 
				(setf v -9999999999)
				(dolist (child node)
					(setf v (max v (alphabeta child (1- depth) alpha beta 'False)))
					(setf alpha (max alpha v))
					(cond
						((<= beta alpha) );break.... not sure how (break-transparent) <-- according to Hoyle err zach
						;return beta cut-off...
					)
				)
				(return-from alphabeta v)
			)
			(t
				(setf v 999999999999)
				(dolist (child node)
					(setf v (min v (alphabeta child (1- depth alpha beta 'True))))
					(setf beta (min beta v))
					(cond
						((<= beta alpha) );break ... return alpha cut-off
					)
				)
				(return-from alphabeta v)
			)
		)
