(let
	(($global (alloc (object)))
	(@OpAdd (lambda (@x @y)
		(let
		 (($opLhs @x)
		  ($opRhs @y))
		 (let
		  (($addLhs $opLhs)
		   ($addRhs $opRhs))
		  (if (let
			   (($or (=== (typeof $addLhs) "string")))
			   (if $or
				$or
				(=== (typeof $addRhs) "string")))
		   (string-+ (prim->string $addLhs) (prim->string $addRhs))
		   (+ (prim->number $addLhs) (prim->number $addRhs))))))))
		   (begin
			 (begin
			  (set!
			   $global
			   (update-field (deref $global)
				"z"
				(@OpAdd "pippo" "polok")))
			  undefined)
			 undefined)
)