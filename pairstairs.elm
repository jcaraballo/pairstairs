import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Char


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL

type alias Model =
  { players : List String
  , sessions : List (List Int)
  }

sessionsForPair : Model -> Int -> Int -> Int
sessionsForPair model p1 p2 =
  List.length (List.filter (\s -> s == (List.sort [p1, p2])) model.sessions)

model : Model
model =
  Model [] []



-- UPDATE


type Msg
    = AddPlayer
    | ChangeName Int String
    | AddSession Int Int


update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeName id name ->
        { model | players = (List.take id model.players) ++ [name] ++ List.drop (id + 1) model.players }
    AddPlayer ->
        { model | players = model.players ++ [""] }
    AddSession id1 id2 ->
        { model | sessions = (List.sort [id1, id2]) :: model.sessions }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick AddPlayer ] [ text "+" ]
    , table [] (List.map (buildRow model) (List.range 0 ((List.length model.players)-1)))
    , (ul [] (List.map (\s -> li [] [text (session model s)]) model.sessions))
    ]

buildRow : Model -> Int -> Html Msg
buildRow model position =
  let otherIds =
    List.range 0 (position - 1)
  in
    tr [] ((List.map (pairingCell model position) otherIds) ++ [playerCell position])

tick : String
tick = (String.fromChar (Char.fromCode 10004))
ticks : Int -> String
ticks n =
  case n of
      0 -> ""
      n -> tick ++ (ticks (n - 1))


pairingCell : Model -> Int -> Int -> Html Msg
pairingCell model id1 id2 =
  td [style [("border", "2px dotted orange")]]
    [ button [ onClick (AddSession id1 id2) ] [ text tick ]
    , text (ticks (sessionsForPair model id1 id2))
    ]

playerCell position = th [] [input [ placeholder "Name", onInput (\s -> ChangeName position s) ] []]

player : Model -> Int -> String
player model id =
  case List.head (List.drop id model.players) of
    Just name -> name
    Nothing   -> "???"

session : Model -> List Int -> String
session model ids =
  case ids of
      [] -> ""
      head :: [] -> player model head
      head :: tail -> (player model head) ++ " x " ++ (session model tail)
