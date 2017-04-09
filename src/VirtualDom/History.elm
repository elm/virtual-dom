module VirtualDom.History exposing
  ( History
  , Address
  , Msg(..)
  , empty
  , size
  , latestAddress
  , initialModel
  , add
  , get
  , visibleAddressBefore
  , visibleAddressAfter
  , openGroup
  , closeGroup
  , view
  , decoder
  , encode
  )


import Array exposing (Array)
import Char
import Time exposing (Time)
import Json.Decode as Decode
import Json.Encode as Encode
import Native.Debug
import VirtualDom.Helpers as VDom exposing (Node)
import VirtualDom.Metadata as Metadata



-- CONSTANTS


minSnapshotSize : Int
minSnapshotSize =
  64



-- HISTORY


type alias History model msg =
  { snapshots : Array (Snapshot model msg)
  , recent : Snapshot model msg
  , numMessages : Int
  , lastTimestamp : Maybe Time
  , lastGroupName : String
  }


type alias Snapshot model msg =
  { model : model
  , groups : Array (Group msg)
  , numMessages : Int
  }


empty : model -> History model msg
empty model =
  History Array.empty (Snapshot model Array.empty 0) 0 Nothing ""


size : History model msg -> Int
size history =
  history.numMessages


initialModel : History model msg -> model
initialModel  { snapshots, recent } =
  case Array.get 0 snapshots of
    Just { model } ->
      model

    Nothing ->
      recent.model



-- GROUP


type alias Group msg =
  { closed : Bool
  , messages : List msg
  , numMessages : Int
  }


initGroup : msg -> Group msg
initGroup msg =
  { closed = False
  , messages = [ msg ]
  , numMessages = 1
  }


lastMessage : Group msg -> msg
lastMessage group =
  case group.messages of
    [] ->
      Debug.crash "Group must contain at least one message."

    last :: _ ->
      last



-- JSON


decoder : model -> (msg -> model -> model) -> Decode.Decoder (model, History model msg)
decoder initialModel update =
  let
    addMessage rawMsg (model, history) =
      let
        msg =
          jsToElm rawMsg
      in
        (update msg model, add Nothing msg model history)

    updateModel rawMsgs =
      List.foldl addMessage (initialModel, empty initialModel) rawMsgs
  in
    Decode.map updateModel (Decode.list Decode.value)


jsToElm : Encode.Value -> a
jsToElm =
  Native.Debug.unsafeCoerce


encode : History model msg -> Encode.Value
encode { snapshots, recent } =
  let
    recentJson =
      consEncodedGroups recent.groups []
  in
    Encode.list <| Array.foldr consEncodedSnapshot recentJson snapshots


consEncodedSnapshot : Snapshot model msg -> List Encode.Value -> List Encode.Value
consEncodedSnapshot snapshot allMessages =
  consEncodedGroups snapshot.groups allMessages


consEncodedGroups : Array (Group msg) -> List Encode.Value -> List Encode.Value
consEncodedGroups groups allMessages =
  Array.foldl consEncodedGroup allMessages groups


consEncodedGroup : Group msg -> List Encode.Value -> List Encode.Value
consEncodedGroup group allMessages =
  List.foldl (\elm msgs -> elmToJs elm :: msgs) allMessages group.messages


elmToJs : a -> Encode.Value
elmToJs =
  Native.Debug.unsafeCoerce



-- ADD MESSAGES


add : Maybe Time -> msg -> model -> History model msg -> History model msg
add timestamp msg model { snapshots, recent, numMessages, lastTimestamp, lastGroupName } =
  let
    groupName =
      Native.Debug.messageToGroupName msg

    sameGroup =
      isSameGroup groupName lastGroupName

    fast =
      isImmediateAfter lastTimestamp timestamp

    imported =
      timestamp == Nothing
  in
    case addRecent sameGroup fast imported msg model recent of
      (Just snapshot, newRecent) ->
        History (Array.push snapshot snapshots) newRecent (numMessages + 1) timestamp groupName

      (Nothing, newRecent) ->
        History snapshots newRecent (numMessages + 1) timestamp groupName


addRecent  : Bool
          -> Bool
          -> Bool
          -> msg
          -> model
          -> Snapshot model msg
          -> (Maybe (Snapshot model msg), Snapshot model msg)
addRecent sameGroup fast imported msg model recent =
  if sameGroup then
    let
      headGroup =
        case Array.get (Array.length recent.groups - 1) recent.groups of
          Just group -> group

          Nothing ->
            Debug.crash "Bug in add"

      newGroups =
        Array.set
          (Array.length recent.groups - 1)
          (updateLatestGroup fast imported msg headGroup)
          recent.groups
    in
      ( Nothing
      , Snapshot recent.model newGroups (recent.numMessages + 1)
      )
  else if recent.numMessages < minSnapshotSize then
    ( Nothing
    , Snapshot recent.model (Array.push (initGroup msg) recent.groups) (recent.numMessages + 1)
    )
  else
    ( Just recent
    , Snapshot model (Array.fromList [ initGroup msg ]) 1
    )


updateLatestGroup : Bool -> Bool -> msg -> Group msg -> Group msg
updateLatestGroup fast imported msg group =
  let
    closed =
      if group.numMessages == 1 && fast then
        True
      else if group.numMessages == 5 && imported then
        True
      else
        group.closed
  in
    { group
        | closed = closed
        , messages = msg :: group.messages
        , numMessages = group.numMessages + 1
    }


isSameGroup : String -> String -> Bool
isSameGroup groupName1 groupName2 =
  groupName1 /= "" && groupName1 == groupName2


isImmediateAfter : Maybe Time -> Maybe Time -> Bool
isImmediateAfter timestamp1 timestamp2 =
  timestamp1
    |> Maybe.andThen (\t1 -> timestamp2
    |> Maybe.map (\t2 ->
      t2 - t1 < 120
    ))
    |> Maybe.withDefault False



-- GET SUMMARY


get : (msg -> model -> (model, a)) -> Address -> History model msg -> ( model, msg )
get update address history =
  case getSnapshot address.snapshot history of
    Just snapshot ->
      let
        updateHelp msg model =
          Tuple.first <| update msg model
      in
        undone <|
          Array.foldl
            (getHelp updateHelp address.message)
            (Stepping address.group snapshot.model)
            snapshot.groups

    Nothing ->
      Debug.crash ("UI should only let you ask for real indexes! " ++ toString address)


type GetResult model msg
  = Stepping Int model
  | Done msg model


getHelp : (msg -> model -> model) -> Int -> Group msg -> GetResult model msg -> GetResult model msg
getHelp update messageIndex group getResult =
  case getResult of
    Done _ _ ->
      getResult

    Stepping n model ->
      if n == 0 then
        List.foldr (getHelpHelp update) (Stepping messageIndex model) group.messages
      else
        Stepping (n - 1) (List.foldr update model group.messages)


getHelpHelp : (msg -> model -> model) -> msg -> GetResult model msg -> GetResult model msg
getHelpHelp update msg getResult =
  case getResult of
    Done _ _ ->
      getResult

    Stepping n model ->
      if n == 0 then
        Done msg (update msg model)

      else
        Stepping (n - 1) (update msg model)


undone : GetResult model msg -> ( model, msg )
undone getResult =
  case getResult of
    Done msg model ->
      ( model, msg )

    Stepping _ _ ->
      Debug.crash "Bug in History.get"



-- UP AND DOWN


latestAddress : History model msg -> Maybe Address
latestAddress history =
  validateAddress
    history
    ( Address (Array.length history.snapshots) lastIndex lastIndex )


visibleAddressBefore : Address -> History model msg -> Maybe Address
visibleAddressBefore =
  nextVisibleAddress False


visibleAddressAfter : Address -> History model msg -> Maybe Address
visibleAddressAfter =
  nextVisibleAddress True


nextVisibleAddress : Bool -> Address -> History model msg -> Maybe Address
nextVisibleAddress forward from history =
  case getGroup from.snapshot from.group history of
    Just group ->
      case validateAddress history (nextAddress forward group.closed from) of
        Nothing ->
          if forward then Nothing else Just firstAddress

        x -> x

    Nothing ->
      Debug.crash ("UI should only let you ask for real indexes! " ++ toString from)


nextAddress : Bool -> Bool -> Address -> Address
nextAddress forward groupClosed =
  if groupClosed then
    if forward then incrementGroup else decrementGroup
  else
    if forward then increment else decrement



-- OPEN AND CLOSE GROUP


openGroup : Address -> History model msg -> (History model msg, Address)
openGroup = toggleGroup False


closeGroup : Address -> History model msg -> (History model msg, Address)
closeGroup = toggleGroup True


toggleGroup : Bool -> Address -> History model msg -> (History model msg, Address)
toggleGroup closed address history =
  case getGroup address.snapshot address.group history of
    Just group ->
      if group.numMessages >= 2 && group.closed /= closed then
        ( updateGroup
            (\_ -> { group | closed = closed })
            address.snapshot
            address.group
            history
        , { address | message = 0 }
        )
      else
        (history, address)

    Nothing ->
      Debug.crash ("UI should only let you ask for real indexes! " ++ toString address)


updateGroup : (Group msg -> Group msg) -> Int -> Int -> History model msg -> History model msg
updateGroup fg snapshotIndex groupIndex history =
  let
    fs snapshot =
      Snapshot
        snapshot.model
        (arrayUpdate fg groupIndex snapshot.groups)
        snapshot.numMessages
  in
    updateSnapshot
      fs
      snapshotIndex
      history


updateSnapshot  : (Snapshot model msg -> Snapshot model msg)
               -> Int -> History model msg
               -> History model msg
updateSnapshot f snapshotIndex history =
  if snapshotIndex == Array.length history.snapshots then
    { history | recent = f history.recent }
  else
    { history | snapshots = arrayUpdate f snapshotIndex history.snapshots }


arrayUpdate : (a -> a) -> Int -> Array a -> Array a
arrayUpdate f index array =
  case Array.get index array of
    Just a ->
      Array.set index (f a) array

    Nothing ->
      array



-- ACCESS BY ADDRESS


validateAddress : History model msg -> Address -> Maybe Address
validateAddress history address =
  if address.message == -1 then
    validateAddress history (decrementGroup address)
  else if address.group == -1 then
    validateAddress history (decrementSnapshot address)
  else
    getSnapshot address.snapshot history
      |> Maybe.andThen (\snapshot ->
        if address.group >= Array.length snapshot.groups then
          validateAddress history (incrementSnapshot address)
        else if address.group == lastIndex then
          validateAddress history { address | group = Array.length snapshot.groups - 1 }
        else
          getGroup address.snapshot address.group history
            |> Maybe.andThen (\group ->
                if address.message >= group.numMessages then
                  validateAddress history (incrementGroup address)
                else if address.message == lastIndex then
                  Just { address | message = group.numMessages - 1 }
                else
                  Just address
              )
        )


getGroup : Int -> Int -> History model msg -> Maybe (Group msg)
getGroup snapshotIndex groupIndex history =
  getSnapshot snapshotIndex history
    |> Maybe.andThen (\snapshot ->
        Array.get groupIndex snapshot.groups
      )


getSnapshot : Int -> History model msg -> Maybe (Snapshot model msg)
getSnapshot snapshotIndex history =
  if snapshotIndex < 0 then
    Nothing
  else if snapshotIndex == Array.length history.snapshots then
    Just history.recent
  else
    Array.get snapshotIndex history.snapshots



-- VIEW


type Msg
  = OpenGroup Address
  | CloseGroup Address
  | Select Address


view : Maybe Address -> History model msg -> Node Msg
view currentAddress { snapshots, recent, numMessages } =
  let
    className =
      if currentAddress == Nothing then
        "debugger-sidebar-messages"
      else
        "debugger-sidebar-messages-paused"

    oldStuff =
      VDom.lazy3 viewSnapshots currentAddress (numMessages - recent.numMessages - 1) snapshots

    currentAddressHelp =
      currentAddress
        |> Maybe.andThen (filterAddressBySnapshot (Array.length snapshots))

    newStuff =
      VDom.lazy3 viewSnapshot currentAddressHelp (numMessages - 1) recent
        |> VDom.map ((|>) (Array.length snapshots))
  in
    VDom.div [ VDom.class className ] [ oldStuff, newStuff ]



-- VIEW SNAPSHOTS


viewSnapshots : Maybe Address -> Int -> Array (Snapshot model msg) -> Node Msg
viewSnapshots currentAddress index snapshots =
  VDom.div [] <| (\(index, _, nodes) -> nodes) <|
    Array.foldr
      (consSnapshot currentAddress)
      (index, (Array.length snapshots - 1), [])
      snapshots


consSnapshot  : Maybe Address
             -> Snapshot model msg
             -> ( Int, Int, List (Node Msg) )
             -> ( Int, Int, List (Node Msg) )
consSnapshot currentAddress snapshot (index, snapshotIndex, rest) =
  let
    currentAddressHelp =
      currentAddress
        |> Maybe.andThen (filterAddressBySnapshot snapshotIndex)
  in
    ( index - snapshot.numMessages
    , snapshotIndex - 1
    , VDom.map
        ((|>) snapshotIndex)
        (VDom.lazy3 viewSnapshot currentAddressHelp index snapshot)
      :: rest
    )


viewSnapshot : Maybe Address -> Int -> Snapshot model msg -> Node (Int -> Msg)
viewSnapshot currentAddress index { groups } =
  groups
    |> Array.foldr (consGroup currentAddress) (index, (Array.length groups - 1), [])
    |> (\(_, _, nodes) -> nodes)
    |> VDom.div []



-- VIEW GROUP


consGroup  : Maybe Address
          -> Group msg
          -> ( Int, Int, List (Node (Int -> Msg)) )
          -> ( Int, Int, List (Node (Int -> Msg)) )
consGroup currentAddress group (index, groupIndex, rest) =
  let
    currentAddressHelp =
      currentAddress
        |> Maybe.andThen (filterAddressByGroup groupIndex)
  in
    ( index - group.numMessages
    , groupIndex - 1
    , viewGroup currentAddressHelp index groupIndex group :: rest
    )


viewGroup : Maybe Address -> Int -> Int -> Group msg -> Node (Int -> Msg)
viewGroup currentAddress index groupIndex group =
  let
    children =
      if group.closed then
        viewClosedGroup
          currentAddress
          groupIndex
          (index - group.numMessages + 1)
          index
          (lastMessage group)
          :: []
      else
        group.messages
          |> List.foldl (consMsg currentAddress) (index, (group.numMessages - 1), [])
          |> (\(_, _, nodes) -> nodes)

    folder =
      VDom.lazy groupFolder group
  in
    VDom.div [ VDom.class "messages-group" ] (folder :: children)
      |> VDom.map ((|>) groupIndex)


groupFolder : Group msg -> Node (Int -> Int -> Msg)
groupFolder group =
  let
    msg = \groupIndex snapshotIndex ->
      (if group.closed then OpenGroup else CloseGroup)
        (Address snapshotIndex groupIndex 0)
  in
    VDom.div
      [ VDom.onClick msg
      , VDom.class "message-group-folder"
      ]
      (groupFolderHelp group)


groupFolderHelp : Group msg -> List (Node a)
groupFolderHelp group =
  if group.numMessages <= 1 then
    []
  else if group.closed then
    [ groupToggleButton "+" ]
  else
    let
      height =
        (toFloat group.numMessages - 1.5) * 2

      groupGuide =
        VDom.div
          [ VDom.class "messages-group-guide"
          , VDom.style [("height", toString height ++ "em")]
          ]
          []
    in
      [ groupToggleButton "-", groupGuide ]


groupToggleButton : String -> Node a
groupToggleButton s =
  VDom.div [ VDom.class "messages-group-button" ] [ VDom.text s ]


viewClosedGroup : Maybe Address -> Int -> Int -> Int -> msg -> Node (Int -> Int -> Msg)
viewClosedGroup currentAddress groupIndex firstIndex lastIndex msg =
  let
    selected =
      case currentAddress of
        Just address ->
          address.group == groupIndex

        Nothing ->
          False

    indexStr =
      ".." ++ toString lastIndex
  in
    VDom.lazy3 viewRow selected indexStr (Native.Debug.messageToString msg)
      |> VDom.map ((|>) (lastIndex - firstIndex - 1))



-- VIEW MESSAGE


consMsg  : Maybe Address
        -> msg
        -> ( Int, Int, List (Node (Int -> Int -> Msg)) )
        -> ( Int, Int, List (Node (Int -> Int -> Msg)) )
consMsg currentAddress msg (index, msgIndex, rest) =
  ( index - 1
  , msgIndex - 1
  , viewMessage currentAddress index msgIndex msg :: rest
  )


viewMessage : Maybe Address -> Int -> Int -> msg -> Node (Int -> Int -> Msg)
viewMessage currentAddress index msgIndex msg =
  let
    selected =
      case currentAddress of
        Just address ->
          address.message == msgIndex

        Nothing ->
          False
  in
    VDom.lazy3 viewRow selected (toString index) (Native.Debug.messageToString msg)
      |> VDom.map ((|>) msgIndex)


viewRow : Bool -> String -> String -> Node (Int -> Int -> Int -> Msg)
viewRow selected index content =
  let
    className =
      if selected then
        "messages-entry messages-entry-selected"

      else
        "messages-entry"

    msg = \msgIndex groupIndex snapshotIndex ->
      Select (Address snapshotIndex groupIndex msgIndex)
  in
    VDom.div
      [ VDom.class className
      , VDom.on "click" (Decode.succeed msg)
      ]
      [ VDom.span
          [ VDom.class "messages-entry-content"
          , VDom.attribute "title" content
          ]
          [ VDom.text content ]
      , VDom.span [ VDom.class "messages-entry-index"] [ VDom.text index ]
      ]



-- ADDRESS


type alias Address =
  { snapshot : Int
  , group : Int
  , message : Int
  }


lastIndex : Int
lastIndex = -2


increment : Address -> Address
increment address =
  { address | message = address.message + 1 }


decrement : Address -> Address
decrement address =
  { address | message = address.message - 1 }


incrementGroup : Address -> Address
incrementGroup address =
  { address | group = address.group + 1, message = 0 }


decrementGroup : Address -> Address
decrementGroup address =
  { address | group = address.group - 1, message = lastIndex }


incrementSnapshot : Address -> Address
incrementSnapshot address =
  { address
      | snapshot = address.snapshot + 1
      , group = 0
      , message = 0
  }


decrementSnapshot : Address -> Address
decrementSnapshot address =
  { address
      | snapshot = address.snapshot - 1
      , group = lastIndex
      , message = lastIndex
  }



-- ADDRESS HELPER


firstAddress : Address
firstAddress = Address 0 0 0


filterAddressBySnapshot : Int -> Address -> Maybe Address
filterAddressBySnapshot = filterAddressBy .snapshot


filterAddressByGroup : Int -> Address -> Maybe Address
filterAddressByGroup = filterAddressBy .group


filterAddressBy : (Address -> Int) -> Int -> Address -> Maybe Address
filterAddressBy f index address =
  if f address == index then
    Just address
  else
    Nothing
