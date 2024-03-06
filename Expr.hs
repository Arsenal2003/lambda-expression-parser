module Expr where

{-- A lambda expression is:
        - a Variable    - x
        - a Function    - \x.e
        - a Application - e1 e2
--}

data Expr = Variable String
          | Function String Expr
          | Application Expr Expr

data Code = Evaluate Expr
          | Assign String Expr
    deriving (Eq, Show)

-- shorthand functions
v = Variable
f = Function
a = Application
macro = undefined -- TODO 3. add shorthand for Macro

-- show instance 
instance Show Expr where
    show (Variable x) = x
    show (Function x (Application e1 e2)) = ('λ':x) ++ ".(" ++ (show e) ++ ")"
        where e = (Application e1 e2)
    show (Function x e) = ('λ':x) ++ ('.':(show e))
    show (Application e1 (Application u v)) = (show e1) ++ " (" ++ (show e2) ++ ")"
        where e2 = (Application u v)
    show (Application e1 e2) = (show e1) ++ (' ':(show e2))
    -- TODO 3. add show instance for Macro

-- equality instance
instance Eq Expr where
    (==) e1 e2 = equal e1 e2 []
      where
        equal :: Expr -> Expr -> [(String, String)] -> Bool
        equal (Variable x) (Variable y) env = case (lookup x env) of 
                                                (Just xv) -> xv == y
                                                Nothing -> x == y

        equal (Function x e1) (Function y e2) env = equal e1 e2 ((x,y):env)
        equal (Application e1 e2) (Application e3 e4) env = (equal e1 e3 env) && (equal e2 e4 env)
        -- TODO 3. add equal instance for Macro
        -- before default case !!!
        equal _ _ _ = False
