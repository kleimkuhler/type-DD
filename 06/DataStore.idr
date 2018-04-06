module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

size : DataStore -> Nat
size (MkData size items) = size

items : (store : DataStore) -> Vect (size store) String
items (MkData size items) = items

addToStore : DataStore -> String -> DataStore
addToStore (MkData _ items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (oldItem :: oldItems) = oldItem :: addToData oldItems

parseCommand : String -> String -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "search" str = Just (Search str)
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store
  = let store_items = items store in
        case integerToFin pos (size store) of
              Nothing => Just ("Out of range\n", store)
              Just id => Just (index id store_items ++ "\n", store)

searchEntry : (substr : String) -> (store : DataStore) -> Maybe (String, DataStore)
searchEntry substr store = Just (searchStore 0 (items store), store)
  where
    searchStore : Nat -> (items : Vect n String) -> String
    searchStore k [] = ""
    searchStore k (x :: xs)
      = let rest = searchStore (k + 1) xs in
        if isInfixOf substr x
          then show k ++ ": " ++ x ++ "\n" ++ rest
          else rest

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp
  = case parse inp of
         Nothing => Just ("Invalid command\n", store)
         Just (Add item) =>
           Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         Just (Get pos) => getEntry pos store
         Just (Search substr) => searchEntry substr store
         Just Size => Just (show (size store) ++ " entries\n", store)
         Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
