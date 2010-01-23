
module Yadorigi.Common (amap) where

import Control.Applicative

amap :: a -> [a -> b] -> [b]
amap = (<**>).pure

