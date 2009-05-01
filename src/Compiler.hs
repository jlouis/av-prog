-- The compiler from OCult to Twodee
module Compiler ()
where

import Twodee.Ast
import Ocult.Ast

compileInt 0 = Inl { e_exp = Unit }


