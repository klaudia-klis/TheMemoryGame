module Main exposing (main)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Process exposing (sleep)
import Random exposing (Generator)
import Task
import Time exposing (..)


type alias Model =
    { currentSequence : List Int
    , userSequence : List Int
    , activeButton : Maybe Int
    , currentLevel : Int
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { currentSequence = []
      , userSequence = []
      , activeButton = Nothing
      , currentLevel = 0
      }
    , Cmd.none
    )


type Msg
    = Reset
    | StartGame
    | RandomResult Int
    | AddToUserSequence Int
    | SetActiveButton Int
    | ClearActiveButton


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


showSequence : List Int -> String
showSequence sequence =
    sequence
        |> List.map String.fromInt
        |> String.concat


sequenceLengthFromLevel : Int -> Int
sequenceLengthFromLevel level =
    level + 1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            let
                cmd =
                    Random.generate RandomResult (Random.int 0 5)

                newModel =
                    { model
                        | userSequence = []
                        , currentLevel = model.currentLevel + 1
                    }
            in
            ( newModel
            , if model.currentLevel == 0 then
                Cmd.batch [ cmd, cmd ]

              else
                cmd
            )

        RandomResult n ->
            let
                newSequence =
                    model.currentSequence ++ [ n ]

                cmds =
                    newSequence
                        |> List.map SetActiveButton
                        |> List.intersperse ClearActiveButton
                        |> (\xs -> xs ++ [ ClearActiveButton ])
                        |> List.indexedMap (\i m -> delay (toFloat i * 200) m)
            in
            ( { model | currentSequence = newSequence }
            , if List.length newSequence == sequenceLengthFromLevel model.currentLevel then
                Cmd.batch cmds

              else
                Cmd.none
            )

        AddToUserSequence n ->
            if gameIsInProgress model || (model.userSequence == [] && model.currentSequence /= []) then
                ( { model | userSequence = model.userSequence ++ [ n ] }, Cmd.none )

            else
                ( model, Cmd.none )

        Reset ->
            initialModel ()

        SetActiveButton n ->
            ( { model | activeButton = Just n }, Cmd.none )

        ClearActiveButton ->
            ( { model | activeButton = Nothing }, Cmd.none )


gameStyle =
    [ marginRight auto
    , marginLeft auto
    , height (px 540)
    , padding (px 100)
    , textAlign center
    , backgroundColor (hex "7FDBFF")
    ]


gameState model =
    div
        [ css [ fontSize (px 16), fontWeight bold, fontFamilies [ "Verdana" ] ] ]
        [ if gameHasNotStartedYet model then
            text "Game hasn't started yet."

          else if userWon model then
            text "User wins! Next level!"

          else if gameIsInProgress model then
            text ""

          else
            text "You lost..."
        ]


makeButton : Model -> Int -> Html Msg
makeButton model idx =
    if model.activeButton == Just idx then
        button ([ onClick (AddToUserSequence idx) ] ++ activeButtonStyle) []

    else
        button ([ onClick (AddToUserSequence idx) ] ++ buttonStyle (gameIsInProgress model)) []


view : Model -> Html Msg
view model =
    div [ css gameStyle ]
        ([ gameState model
         , br [] []
         ]
            ++ List.map (makeButton model) (List.range 0 5)
            ++ [ br [] []
               , if gameIsInProgress model then
                    button ([ onClick Reset ] ++ startButtonStyle) [ text "Restart" ]

                 else if userWon model then
                    button ([ onClick StartGame ] ++ startButtonStyle) [ text "Next Level" ]

                 else if gameHasNotStartedYet model then
                    button ([ onClick StartGame ] ++ startButtonStyle) [ text "Start game" ]

                 else
                    button ([ onClick Reset ] ++ startButtonStyle) [ text "Start game" ]
               ]
        )


userWon : Model -> Bool
userWon model =
    model.currentSequence == model.userSequence && model.currentSequence /= []


gameHasNotStartedYet : Model -> Bool
gameHasNotStartedYet model =
    model.currentSequence == [] && model.userSequence == []


gameIsInProgress : Model -> Bool
gameIsInProgress model =
    model.currentSequence /= [] && (model.userSequence == [] || (model.userSequence /= [] && String.startsWith (showSequence model.userSequence) (showSequence model.currentSequence) && (model.currentSequence /= model.userSequence)))


startButtonStyle : List (Attribute msg)
startButtonStyle =
    [ css
        [ width (px 100)
        , height (px 30)
        , margin (px 20)
        , cursor pointer
        , borderRadius (px 10)
        , borderColor (hex "DDDDDD")
        , backgroundColor (hex "7FDBFF")
        , fontFamilies [ "verdana" ]
        , fontWeight bold
        , borderWidth (px 2)
        ]
    ]


buttonStyle : Bool -> List (Attribute msg)
buttonStyle showActive =
    [ css
        ([ width (px 100)
         , height (px 100)
         , margin (px 10)
         , cursor pointer
         , borderRadius (px 20)
         , backgroundColor (hex "DDDDDD")
         ]
            ++ (if showActive then
                    [ active [ backgroundColor (hex "0074D9") ] ]

                else
                    []
               )
        )
    ]


activeButtonStyle : List (Attribute msg)
activeButtonStyle =
    List.singleton <|
        css
            [ width (px 100)
            , height (px 100)
            , margin (px 10)
            , cursor pointer
            , borderRadius (px 20)
            , backgroundColor (hex "0074D9")
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
