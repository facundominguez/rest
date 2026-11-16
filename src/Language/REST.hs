-- |

module Language.REST where

import Language.REST.OCAlgebra (OCAlgebra)
import Language.REST.OCToAbstract (lift)
import Language.REST.WQOConstraints.SMT (SMTConstraint, smtOC)
import Language.REST.RPO
import Language.REST.LPO (lpo)
import Language.REST.RuntimeTerm
import Language.REST.Op
import System.IO (Handle)


-- | 'adtRPO' is an ordering constraint algebra derived from the recursive
-- path ordering; constraints are represented directly as SMT expressions.
adtRPO :: (Handle, Handle) -> OCAlgebra (SMTConstraint Op) RuntimeTerm IO
adtRPO z3 = lift (smtOC z3) rpo

-- | 'adtLPO' is the lexicographic-path-ordering analogue of 'adtRPO'.
adtLPO :: (Handle, Handle) -> OCAlgebra (SMTConstraint Op) RuntimeTerm IO
adtLPO z3 = lift (smtOC z3) lpo
