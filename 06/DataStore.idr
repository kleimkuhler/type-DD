module Main

import Data.Vect

infixr 5 .+.

-- Datastore
data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) newItem
           = MkData schema _ (addToItems items)
  where
    addToItems : Vect oldSize (SchemaType schema) ->
                 Vect (S oldSize) (SchemaType schema)
    addToItems [] = [newItem]
    addToItems (x :: xs) = x :: addToItems xs

getEntry : Integer -> DataStore -> Maybe (String, DataStore)
getEntry pos store
  = let storeItems = items store in
        case integerToFin pos (size store) of
              Nothing => Just ("Out of range\n", store)
              Just id => Just (?display (index id storeItems) ++ "\n", store)

-- I/O
data Command : Schema -> Type where
     Add : SchemaType schema -> Command schema
     Get : Integer -> Command schema
     Quit : Command schema

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" str = Just (Add (?parseBySchema str))
parseCommand schema "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand schema "quit" "" = Just Quit
parseCommand _  _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input
  = case parse (schema store) input of
         Nothing => Just ("Invalid command\n", store)
         Just (Add item) =>
           Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         Just (Get pos) => getEntry pos store
         Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
