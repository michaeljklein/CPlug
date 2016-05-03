module Text.PrettyPrint.HughesPJ.Ext where

import Text.PrettyPrint (Doc(..), TextDetails(..) )
import TextShow         (TextShow(..)     )
import TextShow.Generic (genericShowbPrec)

instance TextShow Doc where
  showbPrec = genericShowbPrec

instance TextShow TextDetails where
  showbPrec = genericShowbPrec

