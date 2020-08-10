data Maybe' a = Just a | Nothing

x :: Maybe' Int
x = Main.Nothing

fromMaybe :: a -> Maybe' a -> a
fromMaybe defaultVal Main.Nothing = defaultVal
fromMaybe _ (Main.Just x) = x