(load 'minmax.lsp)
;this needs 
(defun make-move (position player ply)
	(let (x)
		(setf x (minimax position ply))
		(cadr x)
	)
)
