module Util where
import Debug
import List
import Set
import Dict

log a = Debug.log "logging" a

for : List a -> (a -> b) -> List b
for xs f = List.map f xs

isJust : Maybe a -> Bool
isJust m = case m of
        Just a -> True
        Nothing -> False

fromJust : Maybe a -> a
fromJust x = case x of
        Just y -> y
        Nothing -> Debug.crash "error: fromJust Nothing"


updateKeys : (comparable -> b -> b) -> List comparable -> Dict.Dict comparable b -> Dict.Dict comparable b
updateKeys fn keys dict = List.foldr (\key d -> Dict.update key (Maybe.map (fn key)) d) dict keys

-- Equivalent to updateKeys using all keys of the dict
keyMap : (comparable -> b -> b) -> Dict.Dict comparable b -> Dict.Dict comparable b
keyMap fn dict = updateKeys fn (Dict.keys dict) dict
