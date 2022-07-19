module State.Changelist where

import Trello.Types (TrelloEntry(..), TrelloItem(..), cardIdList, listIdBoard)

data Operation
  = Create
  | Update
  | Delete
  deriving (Show, Eq)

data Change =
  Change Operation TrelloItem
  deriving (Show, Eq)

getItem :: Change -> TrelloItem
getItem (Change _ item) = item

addChange :: Change -> [Change] -> [Change]
addChange ch chs =
  let itemAlreadyInChanges =
        itemId (getItem ch) `elem` map (itemId . getItem) chs
   in if itemAlreadyInChanges
        then let filteredChs =
                   filter (\x -> itemId (getItem ch) /= itemId (getItem x)) chs
              in case ch of
                   Change Delete _ -> filteredChs
                   Change _ _ -> ch : filteredChs
        else ch : chs

updateChangelistChildren :: TrelloItem -> TrelloItem -> [Change] -> [Change]
updateChangelistChildren oldParent newParent changelist =
  let updateParentId (Change op oldItem) =
        Change op $
        case oldItem of
          TCard c ->
            if cardIdList c == itemId oldParent
              then TCard $ c {cardIdList = itemId newParent}
              else TCard c
          TList l ->
            if listIdBoard l == itemId oldParent
              then TList $ l {listIdBoard = itemId newParent}
              else TList l
          TBoard b -> TBoard b
   in map updateParentId changelist

addChanges :: [Change] -> [Change] -> [Change]
addChanges newChs chs = foldr addChange chs newChs
