module Choice exposing ( .. )

import Browser.Events as Events
import Html exposing ( .. )
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import Json.Decode as Json

type alias Model = { choices: List String
                   , active: Bool }


init : Model
init = { choices = []
       , active = False }

type Msg = Selected String
         | DeSelected String
         | Active Bool
         | Noop
update : Msg -> Model -> Model
update msg model = 
  case msg of
     Selected s -> { model | choices = model.choices ++ [s]}
     DeSelected s -> { model | choices = List.filter (\s1 -> s /= s1) model.choices }
     Active b -> { model | active = b }
     Noop -> model

view : Model -> Html Msg
view model = div 
              [ class ("dropdown" ++ (if model.active then " is-active" else "") )
              , attribute "data-id" "dropdown"
              , stopPropagationOn "click" (Json.succeed (Noop, True)) ] 
              [ div 
                [ class "dropdown-trigger"
                , onClick (Active (not model.active)) ] 
                [ button 
                  [ class "button is-primary" ] 
                  [ span [] [text "COMPARE"]
                  , span [ class "icon is-small" ] [ i [ class "fas fa-angle-down" ] [] ] ] ]
              , div 
                [ class "dropdown-menu" ] 
                [ div 
                  [ class "dropdown-content"
                  , style "max-height" "200px"
                  , style "width" "130px"
                  , style "overflow" "auto" ] 
                  (options model.choices) ] ]

subscriptions : Model -> Sub Msg
subscriptions model = if model.active 
                        then Events.onClick (Json.succeed (Active False))
                        else Sub.none

options : List String -> List (Html Msg)
options list = 
  (List.map (\item -> let b = List.member item list
                      in  div [ class ("dropdown-item" ++ if b then " is-active" else ""), attribute "data-key" item ] [ label [ class "checkbox", style "width" "100%" ] [ input [ type_ "checkbox", style "margin-right" "6px" , property "checked" (Encode.bool b), onClick (if b then DeSelected item else Selected item) ] [], text item ] ]) <<
  List.map (\i -> "Option " ++ String.fromInt(i)))
  (List.range 1 10)
