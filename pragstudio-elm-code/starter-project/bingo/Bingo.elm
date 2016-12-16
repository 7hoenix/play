module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import String exposing (..)


-- MODEL

initialModel : Model
initialModel =
  { name = "7hoenix"
  , gameNumber = 0
  , entries = initialEntries
  }

type alias Model =
  { name : String
  , gameNumber : Int
  , entries : List Entry
  }

type alias Entry =
  { id : Int
  , phrase : String
  , points : Int
  , marked : Bool
  }

type alias PlayerName = String
type alias GameNumber = Int

initialEntries : List Entry
initialEntries =
  [ Entry 3 "In the cloud" 300 False
  , Entry 1 "Future-Proof" 100 False
  , Entry 4 "Rock star ninja samauri" 400 False
  , Entry 2 "do the agile" 200 False
  ]


-- UPDATE


type Msg
  = NewGame
  | Mark Int
  | SortEntries


update : Msg -> Model -> Model
update msg model =
  case msg of
    NewGame ->
      { model | gameNumber = model.gameNumber + 1,
                entries = initialEntries }
    Mark id ->
      let
          markEntry e =
            if e.id == id then
               { e | marked = (not e.marked) }
            else
               e

      in
         { model | entries = List.map markEntry model.entries }

    SortEntries ->
      { model | entries = List.sortBy .points model.entries }


-- VIEW


playerInfo : PlayerName -> GameNumber -> String
playerInfo name gameNumber =
  name ++ " - Game #" ++ (toString gameNumber)


viewPlayer : PlayerName -> GameNumber -> Html Msg
viewPlayer name gameNumber =
  let
      playerInfoText =
        playerInfo name gameNumber
        |> toUpper
        |> text

  in
     h2 [ id "info", class "classy" ]
        [ playerInfoText ]

viewHeader : String -> Html Msg
viewHeader title =
  header []
      [ h1 [] [ text title ] ]


viewFooter : Html Msg
viewFooter =
  footer []
      [ a [ href "http:example.com" ]
          [ text "Powered by elm" ]
      ]


viewEntryItem : Entry -> Html Msg
viewEntryItem entry =
  li [ classList [ ("marked", entry.marked) ], onClick (Mark entry.id) ]
      [ span [ class "phrase" ] [ text entry.phrase ]
      , span [ class "points" ] [ text (toString entry.points) ]
      ]

viewEntryList : List Entry -> Html Msg
viewEntryList entries =
  entries
    |> List.map viewEntryItem
    |> ul [ ]

view : Model -> Html Msg
view model =
  div [ class "content" ]
      [ viewHeader "Buzzowrd bingo"
      , viewPlayer model.name model.gameNumber
      , viewEntryList model.entries
      , div [ class "button-group" ]
          [ button [ onClick NewGame ] [ text "New Game" ]
          , button [ onClick SortEntries ] [ text "Sort those entries" ]
          ]
      , div [ class "debug" ] [ text (toString model) ]
      , viewFooter
      ]


main : Program Never Model Msg
main =
  Html.beginnerProgram
    { model = initialModel
    , view = view
    , update = update
    }
