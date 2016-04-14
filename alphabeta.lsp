#|
                  ***** MINIMAX.LSP *****

Generalized recursive minimax routine.

Author: Dr. John M. Weiss
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (minimax position depth)
          where position is the position to be evaluated,
          and depth is the search depth (number of plys).

Returns:  (value path)
          where value is the backed-up value from evaluation of leaf nodes,
          and path is the path to the desired leaf node.

Functions called:

          (deepenough depth) -
              predicate that returns T if the current position has reached
              the desired search depth, NIL otherwise.

          (move-generator position) -
              generates successors to the position.

          (static position) -
              applies the static evaluation function to the position.

          Note: these functions may need additional arguments.
|#

(defun minimax (position depth alpha beta player maxFlag)

    ; if we have searched deep enough, or there are no successors,
    ; return position evaluation and nil for the path
    (if (or (deepenough depth) (null (move-generator position player)))
	;identified that this returns (nil nil)
        (list (hueristics position player) nil)

        ; otherwise, generate successors and run minimax recursively
        (let
            (
			
				;generate list of possible move position
				(moves (move-generator position player))
				
				
                ; generate list of sucessor positions
                (successors '() )
				
				;passed in list
				(start (copy-list position) )

                ; initialize current best path to nil
                (best-path nil)

                ; initialize current best score to negative infinity
                (best-score -100000000)

                ; other local variables
                succ-value
                succ-score
            )
			(when (equal player 'W)
				(setf next 'B)	
			)
			(when (equal player 'B)
				(setf next 'W)
			)
			
			(dolist (index moves)
			
				(setf successors (append successors (list (flipTiles position player index) ) ) )
				(setf position (copy-list start) )
				(format t "successors: ~s index ~s ~%" successors index)
			)
			
			(when (equal maxFlag t)
					(setq succ-value alpha)
					(dolist (successor successors)

						; perform recursive DFS exploration of game tree
						(setq succ-value (max succ-value (first (minimax successor (1- depth) alpha beta next nil) ) ) )
						;(break)
						(setq alpha (max succ-value alpha) )
						(when (<= beta alpha)
							(format t "max pruned~%")
							(return) ;my question being what do we return from this?
						)
						(setq best-path (cons successor (list succ-value))) ;consing does things - bad things
						;(break)
						(setq best-score succ-value)
						(format t "max best score set: ~s ~%" best-score)

					)
			)
			
			(when (equal maxFlag nil)
				(setq succ-value beta)
				(dolist (successor successors)

						; perform recursive DFS exploration of game tree
						(setq succ-value (min succ-value (first (minimax successor (1- depth) alpha beta next t) ) ) )
						;(break)
						(setq beta (min succ-value beta) )
						(when (<= beta alpha)
							(format t "min pruned~%")
							(return) ;my question being what do we return from this?
						)
						(setq best-path (cons successor (list succ-value))) ;consing does things - bad things
						;(break)
						(setq best-score succ-value)
						(format t "min best score set: ~s ~%" best-score)	
				)
				
			)
            ; return (value path) list when done
			(format t "here3 ~s ~s ~%" best-score best-path)
            (list best-score best-path)
        )
    )
)

(defun deepenough (depth)
	(let ()
		(cond
			( (equal depth 0) 
				t
			)
			(t nil)
		)
	)
)
