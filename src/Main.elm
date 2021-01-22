module Main exposing (..)

import Browser
import Html exposing ( .. )
import Html as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Choice as Choice
import Platform.Sub as Sub

main : Program () Model Msg
main = Browser.element { init = init
                       , update = update
                       , view = view
                       , subscriptions = subscriptions }

stylesheet : Html msg
stylesheet =
  node "link"
  [ rel  "stylesheet"
  , href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.9.1/css/bulma.min.css"
  ]
  []

type alias Model = { overall : Choice.Model
                   , category1 : Choice.Model
                   , category2 : Choice.Model
                   , category3 : Choice.Model }

type Msg = UpdateOverall Choice.Msg
         | UpdateCategory1 Choice.Msg
         | UpdateCategory2 Choice.Msg
         | UpdateCategory3 Choice.Msg

init : () -> (Model, Cmd Msg)
init _ = ( { overall = Choice.init
           , category1 = Choice.init
           , category2 = Choice.init
           , category3 = Choice.init }
         , Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    UpdateOverall subMsg -> ({ model | overall = Choice.update subMsg model.overall }, Cmd.none)
    UpdateCategory1 subMsg -> ({ model | category1 = Choice.update subMsg model.category1 }, Cmd.none)
    UpdateCategory2 subMsg -> ({ model | category2 = Choice.update subMsg model.category2 }, Cmd.none)
    UpdateCategory3 subMsg -> ({ model | category3 = Choice.update subMsg model.category3 }, Cmd.none)

view : Model -> Html Msg
view { overall, category1, category2, category3 } = 
  div 
  [ class "container"
  , style "padding" "10px 0 30px" ] 
  [ stylesheet
  , cardView overall UpdateOverall "OVERALL" "400px"
  , div [ style "height" "10px" ] [] 
  , h1 [ class "is-size-4" ] [ text "BREAKDOWN" ]
  , p [ class "is-size-5" ] [ text "Select the optionsfrom dropdown menu" ]
  , div [ style "height" "10px" ] [] 
  , cardView category1 UpdateCategory1 "CATEGORY!" "300px"
  , div [ style "height" "10px" ] [] 
  , div 
    [ class "level" ]
    [ div [ style "width" "50%" ] [ cardView category2 UpdateCategory2 "CATEGORY2" "320px" ]
    , div [ style "width" "6px" ] [] 
    , div [ style "width" "50%" ] [ cardView category3 UpdateCategory3 "CATEGORY3" "320px" ] ] ]

subscriptions : Model -> Sub Msg
subscriptions { overall, category1, category2, category3 } = 
  let overallSubscriptions = Choice.subscriptions overall
      category1Subscriptions = Choice.subscriptions category1
      category2Subscriptions = Choice.subscriptions category2
      category3Subscriptions = Choice.subscriptions category3
  in  Sub.batch [ Sub.map UpdateOverall overallSubscriptions
                , Sub.map UpdateCategory1 category1Subscriptions
                , Sub.map UpdateCategory2 category2Subscriptions
                , Sub.map UpdateCategory3 category3Subscriptions ]

cardView : Choice.Model -> (Choice.Msg -> Msg) -> String -> String -> Html Msg
cardView cModel updateCModel title height = 
  div 
  [ class "card"
  , style "height" height ] 
  [ div 
    [ class "card-content" ]
    [ div 
      [ class "content" ]
      [ div 
        [ class "level"
        , style "align-items" "flex-start" ]
        [ div 
          [ class "level-left"
          , style "max-width" "50%" ] 
          [ div 
            [] 
            [ h2 
              [ class "is-size-3" ]
              [ text title ] 
            , p 
              [ class "is-level-6" ]
              (if List.length cModel.choices == 0 
                then [] 
                else [ text ("Selected: " ++ String.join "," cModel.choices) ] ) ] ] 
        , div 
          [ class "level-right" ]
          [ div
            [ class "level-item" ]
            [ Html.map updateCModel (Choice.view cModel) ] ] ] ] ] ]
