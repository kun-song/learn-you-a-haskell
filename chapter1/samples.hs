{-# LANGUAGE TemplateHaskell #-}
-- 函数定义
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

-- if-then-else 两个分支不可缺少
doubleSmallNumber x = if x > 100
                      then x
                      else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

song'kun = "it's me Song Kun"

