module Main

import Control.ST
import Control.ST.ImplicitCall

data Access = LoggedOut | LoggedIn
data LoginResult = OK | BadPassword

interface DataStore (m : Type -> Type) where
  Store : Access -> Type
  connect : ST m Var [add (Store LoggedOut)]
  disconnect : (store : Var) -> ST m () [remove store (Store LoggedOut)]
  readSecret : (store : Var) -> ST m String [store ::: Store LoggedIn]
  login : (store : Var) ->
          ST m LoginResult [store ::: Store LoggedOut :->
                             (\res => Store (case res of
                                                  OK => LoggedIn
                                                  BadPassword => LoggedOut))]
  logout : (store : Var) -> ST m () [tore ::: Store LoggedIn :-> Store LoggedOut]


getData : (ConsoleIO m, DataStore m) =>
          (failcount : Var)-> ST m () [failcount ::: State Integer]
getData failcount
        = do st <- connect
             OK <- login st
                | BadPassword => do putStrLn "Failure"
                                    fc <- read failcount
                                    write failcount (fc + 1)
                                    putStrLn ("Number of failures:" ++ show (fc + 1))
                                    disconnect st
                                    getData failcount
             secret <- readSecret st
             putStrLn ("Secret is: " ++ show secret)
             logout st
             disconnect st
             getData failcount

implementation DataStore IO where
  Store x = State String
  connect = new "Seret Data"
  disconnect store = delete store
  readSecret store = read store
  login store = do putStr "Enter password: "
                   p <- getStr
                   if p == "p123"
                      then pure OK
                      else pure BadPassword
  logout store = pure ()

main : IO ()
main = run (do fc <- new 0
               getData fc
               delete fc)

