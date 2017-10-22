module Datatypes

data Nat = Z | S Nat

data List a = Ni | (::) a (List a)

infixr 10 ::


