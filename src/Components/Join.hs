module Components.Join where

import Components.Core
import Graphics.Vty

data Join i o
  = Join JT (Component i o) (Component i o)

data JT = LR | RL | UD | DU


instance Interface (Join i o) i o where
  handle (Join t f s) e =
    case handle f e of
      Right n -> pure $ Join t n s
      Left Pass -> case handle s e of
        Right n -> pure $ Join t f n
        Left l -> Left l
      Left l -> Left l

  render a (Join t f s) o =
    (case t of
      LR -> horizJoin
      RL -> flip horizJoin
      UD -> vertJoin
      DU -> flip vertJoin
    )
    (render a f o)
    (render a s o)

  upd (Join _ f s) = upd s . upd f

